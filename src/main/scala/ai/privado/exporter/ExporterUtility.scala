/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.exporter

import ai.privado.cache
import ai.privado.cache.{AppCache, DataFlowCache, Environment, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.outputDirectoryName
import ai.privado.model.{CatLevelOne, Constants, Language, PolicyThreatType}
import ai.privado.model.exporter.{
  CollectionModel,
  DataFlowSubCategoryModel,
  DataFlowSubCategoryPathExcerptModel,
  RuleInfo,
  SinkModel,
  SinkProcessingModel,
  SourceModel,
  SourceProcessingModel,
  ViolationModel,
  ViolationPolicyDetailsModel
}
import ai.privado.model.exporter.SourceEncoderDecoder.*
import ai.privado.model.exporter.DataFlowEncoderDecoder.*
import ai.privado.model.exporter.ViolationEncoderDecoder.*
import ai.privado.model.exporter.CollectionEncoderDecoder.*
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.semantic.Language.finder
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities.dump
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory
import privado_core.BuildInfo

import java.util.Calendar
import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import scala.util.Try

object ExporterUtility {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Convert List of path element schema object
    */
  def convertPathElements(
    nodes: List[AstNode],
    sourceId: String = "",
    taggerCache: TaggerCache = new TaggerCache()
  ): List[DataFlowSubCategoryPathExcerptModel] = {
    val lang     = AppCache.repoLanguage
    val isPython = lang == Language.PYTHON

    val sizeOfList = nodes.size
    nodes.zipWithIndex.flatMap { case (node, index) =>
      val currentNodeModel = convertIndividualPathElement(node, index, sizeOfList)
      if (
        index == 0 && node.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.DERIVED_SOURCES.name).nonEmpty
      ) {
        var typeFullName = Iterator(node).isIdentifier.typeFullName.headOption.getOrElse("")

        // Temporary fix for python to match the typeFullName
        typeFullName = updateTypeFullNameForPython(typeFullName, isPython)

        // Going 1 level deep for derived sources to add extra nodes
        taggerCache.typeDeclMemberCache
          .getOrElse(typeFullName, TrieMap[String, mutable.Set[Member]]())
          .get(sourceId) match {
          case Some(members: mutable.HashSet[Member]) =>
            // Picking up only the head as any path to base is sufficient
            val member: Member     = members.head
            var typeFullNameLevel2 = member.typeFullName // java.lang.string

            // Temporary fix for python to match the typeFullName
            typeFullNameLevel2 = updateTypeFullNameForPython(typeFullNameLevel2, isPython)

            taggerCache.typeDeclMemberCache
              .getOrElse(typeFullNameLevel2, TrieMap[String, mutable.Set[Member]]())
              .get(sourceId) match {
              case Some(member2Set: mutable.HashSet[Member]) =>
                // Picking up only the head as any path to base is sufficient
                val member2 = member2Set.head
                // Going 2 level deep for derived sources to add extra nodes
                convertIndividualPathElement(
                  member2,
                  messageInExcerpt = generateDSMemberMsg(member2.name, typeFullNameLevel2)
                ) ++ convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName)
                ) ++ currentNodeModel
              case _ =>
                convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName)
                ) ++ currentNodeModel
            }

          case _ => // Checking if 2nd level is of Extends type
            taggerCache
              .getTypeDeclExtendingTypeDeclCacheItem(typeFullName)
              .get(sourceId) match {
              case Some(typeDecl: TypeDecl) => // Fetching information for the 2nd level member node
                taggerCache.typeDeclMemberCache
                  .getOrElse(typeDecl.fullName, TrieMap[String, mutable.Set[Member]]())
                  .get(sourceId) match {
                  case Some(members: mutable.HashSet[Member]) =>
                    // Picking up only the head as any path to base is sufficient
                    val member = members.head
                    val currentTypeDeclNode = // Fetching the current TypeDecl node
                      taggerCache.typeDeclDerivedByExtendsCache.get(typeFullName)
                    convertIndividualPathElement(
                      member,
                      messageInExcerpt = generateDSMemberMsg(member.name, typeDecl.fullName)
                    ) ++ convertIndividualPathElement(
                      currentTypeDeclNode.get,
                      messageInExcerpt = generateDSExtendsMsg(typeDecl.name, typeFullName)
                    ) ++ currentNodeModel
                  case _ =>
                    currentNodeModel
                }
              case _ =>
                currentNodeModel
            }
        }
      } else currentNodeModel
    }
  }

  /** Convert Individual path element
    * @param node
    *   \- cfg node
    * @param index
    *   \- index of the list item if any
    * @param sizeOfList
    *   \- size of the list if any
    * @return
    */
  def convertIndividualPathElement(
    node: AstNode,
    index: Int = -1,
    sizeOfList: Int = -1,
    messageInExcerpt: String = ""
  ): Option[DataFlowSubCategoryPathExcerptModel] = {
    val sample = node.code
    val lineNumber: Int = {
      node.lineNumber match {
        case Some(n) => n
        case None    => -1
      }
    }
    val columnNumber: Int = {
      node.columnNumber match {
        case Some(n) => n
        case None    => -1
      }
    }
    val fileName = Utilities.getFileNameForNode(node)
    val absoluteFileName = {
      val file = File(fileName)
      if (file.exists)
        fileName
      else {
        if (AppCache.scanPath.endsWith("/"))
          AppCache.scanPath + fileName
        else
          AppCache.scanPath + "/" + fileName
      }
    }

    if (fileName.equals(Constants.EMPTY) || sample.equals(Constants.EMPTY)) None
    else {
      val message = {
        if (Iterator(node).isCall.nonEmpty) {
          val methodFullName  = Iterator(node).isCall.methodFullName.headOption.getOrElse("")
          val methodInterface = methodFullName.split(":").headOption.getOrElse("")
          if (methodInterface.contains("unresolved") || methodInterface.contains("<operator>")) ""
          else methodInterface
        } else if (Iterator(node).isIdentifier.nonEmpty)
          Iterator(node).isIdentifier.typeFullName.headOption.getOrElse("")
        else
          messageInExcerpt
      }
      val excerpt = dump(absoluteFileName, node.lineNumber, message)
      // Get the actual filename
      val actualFileName = {
        if (AppCache.isLombokPresent)
          fileName.replace("/" + Constants.delombok, "")
        else
          fileName
      }
      Some(DataFlowSubCategoryPathExcerptModel(sample, lineNumber, columnNumber, actualFileName, excerpt))
    }
  }

  def getRuleInfoForExporting(ruleCache: RuleCache, ruleId: String): RuleInfo = {
    ruleCache.getRuleInfo(ruleId) match {
      case Some(rule) =>
        RuleInfo(rule.id, rule.name, rule.category, rule.domains, rule.sensitivity, rule.isSensitive, rule.tags)
      case None => RuleInfo("", "", "", Array[String](), "", isSensitive = false, Map[String, String]())
    }
  }

  def getPolicyInfoForExporting(ruleCache: RuleCache, policyOrThreatId: String): Option[ViolationPolicyDetailsModel] = {
    ruleCache.getPolicyOrThreat(policyOrThreatId) match {
      case Some(policyOrThreat) =>
        Some(
          ViolationPolicyDetailsModel(
            policyOrThreat.name,
            policyOrThreat.policyOrThreatType.toString,
            policyOrThreat.description,
            policyOrThreat.fix,
            { if (policyOrThreat.action != null) policyOrThreat.action.toString else "" },
            policyOrThreat.tags
          )
        )
      case None => None
    }
  }

  /** Helper function to generate message
    * @param memberName
    * @param typeDeclFullName
    * @return
    */
  private def generateDSMemberMsg(memberName: String, typeDeclFullName: String): String = {
    s"'$memberName' is a member of '$typeDeclFullName' class"
  }

  /** Helper function to generate message
    * @param typeDeclName
    * @param typeDeclFullName
    * @return
    */
  private def generateDSExtendsMsg(typeDeclName: String, typeDeclFullName: String): String = {
    s"'$typeDeclName' class is inherited by '$typeDeclFullName' class"
  }

  private def updateTypeFullNameForPython(typeFullName: String, isPython: Boolean): String = {
    var updatedTypeFullName = typeFullName
    val pattern1            = "(.+)\\.<init>".r
    val pattern2            = "(.+)\\.\\w+<body>.*".r
    val pattern3            = "(.+)<meta>.*".r

    if (isPython) {
      typeFullName match {
        case pattern1(str) => updatedTypeFullName = str
        case pattern2(str) => updatedTypeFullName = str
        case pattern3(str) => updatedTypeFullName = str
        case _             => updatedTypeFullName = typeFullName
      }
    }
    updatedTypeFullName
  }

  def writeJsonToFile(
    cpg: Cpg,
    outputFileName: String,
    repoPath: String,
    ruleCache: RuleCache,
    output: Map[String, Json],
    intermediateFolderName: Option[String] = None
  ): File = {
    val outputDirectory = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
    if (intermediateFolderName.isDefined)
      File(s"$repoPath/$outputDirectoryName/${intermediateFolderName.get}").createDirectoryIfNotExists()
    val f =
      if (intermediateFolderName.isDefined)
        File(s"$repoPath/$outputDirectoryName/${intermediateFolderName.get}/$outputFileName")
      else
        File(s"$repoPath/$outputDirectoryName/$outputFileName")
    f.write(output.asJson.toString())
    f
  }

  def generateIndividualComponent(
    cpg: Cpg,
    outputFileName: String,
    repoPath: String,
    dataflows: Map[String, Path],
    ruleCache: RuleCache,
    taggerCache: TaggerCache = new TaggerCache(),
    dataFlowCache: DataFlowCache,
    privadoInput: PrivadoInput,
    repoItemTagName: Option[String] = None
  ): (
    mutable.LinkedHashMap[String, Json],
    List[SourceModel],
    List[SinkModel],
    List[SourceProcessingModel],
    mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
    List[CollectionModel],
    Int
  ) = {
    logger.info("Initiated exporter engine")
    val sourceExporter       = new SourceExporter(cpg, ruleCache, privadoInput, repoItemTagName = repoItemTagName)
    val sinkExporter         = new SinkExporter(cpg, ruleCache, repoItemTagName = repoItemTagName)
    val dataflowExporter     = new DataflowExporter(cpg, dataflows, taggerCache, dataFlowCache) // TODO
    val collectionExporter   = new CollectionExporter(cpg, ruleCache, repoItemTagName = repoItemTagName)
    val probableSinkExporter = new ProbableSinkExporter(cpg, ruleCache, repoPath, repoItemTagName = repoItemTagName)
    val policyAndThreatExporter =
      new PolicyAndThreatExporter(cpg, ruleCache, dataflows, taggerCache, dataFlowCache, privadoInput) // TODO
    val output = mutable.LinkedHashMap[String, Json]()

    output.addOne(Constants.coreVersion -> Environment.privadoVersionCore.asJson)
    output.addOne(Constants.cliVersion  -> Environment.privadoVersionCli.getOrElse(Constants.notDetected).asJson)
    output.addOne(Constants.mainVersion -> AppCache.privadoVersionMain.asJson)
    output.addOne(Constants.privadoLanguageEngineVersion -> BuildInfo.joernVersion.asJson)
    output.addOne(Constants.createdAt                    -> Calendar.getInstance().getTimeInMillis.asJson)

    // To have the repoName as `pay` in nonMonolith case and in case of monolith as `pay/app/controller/payment_methods`
    output.addOne(
      Constants.repoName -> (if (repoItemTagName.isDefined)
                               s"${AppCache.repoName}/${repoItemTagName.get.replaceAll("--", "/")}"
                             else AppCache.repoName).asJson
    )
    output.addOne(Constants.language      -> AppCache.repoLanguage.toString.asJson)
    output.addOne(Constants.gitMetadata   -> GitMetaDataExporter.getMetaData(repoPath).asJson)
    output.addOne(Constants.localScanPath -> AppCache.localScanPath.asJson)
    output.addOne(Constants.probableSinks -> probableSinkExporter.getProbableSinks.asJson)

    // Future creates a thread and starts resolving the function call asynchronously
    val sources = Future {
      val _sources = Try(sourceExporter.getSources).getOrElse(List[SourceModel]())
      output.addOne(Constants.sources -> _sources.asJson)
      _sources
    }
    val processing = Future {
      val _processing = Try(sourceExporter.getProcessing).getOrElse(List[SourceProcessingModel]())
      output.addOne(Constants.processing -> _processing.asJson)
      _processing
    }
    val sinks = Future {
      val _sinks = Try(sinkExporter.getSinks).getOrElse(List[SinkModel]())
      output.addOne(Constants.sinks -> _sinks.asJson)
      _sinks
    }
    val processingSinks = Future {
      val _processingSinks = Try(sinkExporter.getProcessing).getOrElse(List[SinkProcessingModel]())
      output.addOne(Constants.sinkProcessing -> _processingSinks.asJson)
      _processingSinks
    }
    val collections = Future {
      val _collections = Try(collectionExporter.getCollections).getOrElse(List[CollectionModel]())
      output.addOne(Constants.collections -> _collections.asJson)
      _collections
    }

    val finalCollections = Await.result(collections, Duration.Inf)

    val violationResult =
      Try(policyAndThreatExporter.getViolations(repoPath, finalCollections)).getOrElse(List[ViolationModel]())
    output.addOne(Constants.violations -> violationResult.asJson)

    val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
    ruleCache.getRule.sinks.foreach(sinkRule => {
      if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
        sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
      sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
    })

    val dataflowsOutput = mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]()
    sinkSubCategories.foreach(sinkSubTypeEntry => {
      dataflowsOutput.addOne(
        sinkSubTypeEntry._1 -> dataflowExporter
          .getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet, ruleCache, dataFlowCache)
          .toList
      )
    })

    output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
    logger.info("Completed Sink Exporting")

    logger.info("Completed Collections Exporting")

    MetricHandler.metricsData("policyViolations") = violationResult.size.asJson
    violationResult.foreach(violation => {
      MetricHandler.internalPoliciesOrThreatsMatched.addOne(violation.policyId)
    })

    //  Compliance Violations
    val complianceViolations = violationResult.filter(violation =>
      violation.policyDetails match {
        case Some(policyDetail) => policyDetail.policyType.equals(PolicyThreatType.COMPLIANCE.toString)
        case None               => false
      }
    )

    // We need to wait till this get completed before moving ahead to export the result
    Await.result(processingSinks, Duration.Inf)

    (
      output,
      Await.result(sources, Duration.Inf),
      Await.result(sinks, Duration.Inf),
      Await.result(processing, Duration.Inf),
      dataflowsOutput,
      finalCollections,
      complianceViolations.size
    )
  }

  def filterNodeBasedOnRepoItemTagName(nodes: List[AstNode], repoItemTagName: Option[String]): List[AstNode] = {
    if (repoItemTagName.isDefined)
      nodes.where(_.tag.nameExact(Constants.monolithRepoItem).valueExact(repoItemTagName.get)).l
    else
      nodes
  }

}
