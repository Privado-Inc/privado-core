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
import ai.privado.cache.{
  AppCache,
  DataFlowCache,
  Environment,
  FileSkippedBySizeListModel,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.cache.PropertyFilterCacheEncoderDecoder.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.outputDirectoryName
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, InternalTag, Language, PolicyThreatType}
import ai.privado.model.exporter.{
  AndroidPermissionModel,
  CollectionModel,
  DataFlowSubCategoryModel,
  DataFlowSubCategoryPathExcerptModel,
  PropertyNodesModel,
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
import ai.privado.model.exporter.AndroidPermissionsEncoderDecoder.*
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.model.exporter.PropertyNodesEncoderDecoder.*
import ai.privado.semantic.Language.finder
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities.{dump, getTruncatedText}
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*
import ai.privado.tagger.AssetTagger
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import scala.util.Try
import privado_core.BuildInfo

object ExporterUtility {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Convert List of path element schema object
    */
  def convertPathElements(
    nodes: List[AstNode],
    sourceId: String = "",
    taggerCache: TaggerCache = new TaggerCache(),
    appCache: AppCache,
    ruleCache: RuleCache
  ): List[DataFlowSubCategoryPathExcerptModel] = {
    val lang     = appCache.repoLanguage
    val isPython = lang == Language.PYTHON

    val sizeOfList = nodes.size
    nodes.zipWithIndex.flatMap { case (node, index) =>
      val currentNodeModel =
        convertIndividualPathElement(node, index, sizeOfList, appCache = appCache, ruleCache = ruleCache)
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
                  messageInExcerpt = generateDSMemberMsg(member2.name, typeFullNameLevel2),
                  appCache = appCache,
                  ruleCache = ruleCache
                ) ++ convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName),
                  appCache = appCache,
                  ruleCache = ruleCache
                ) ++ currentNodeModel
              case _ =>
                convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName),
                  appCache = appCache,
                  ruleCache = ruleCache
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
                      messageInExcerpt = generateDSMemberMsg(member.name, typeDecl.fullName),
                      appCache = appCache,
                      ruleCache = ruleCache
                    ) ++ convertIndividualPathElement(
                      currentTypeDeclNode.get,
                      messageInExcerpt = generateDSExtendsMsg(typeDecl.name, typeFullName),
                      appCache = appCache,
                      ruleCache = ruleCache
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
    messageInExcerpt: String = "",
    appCache: AppCache,
    ruleCache: RuleCache
  ): Option[DataFlowSubCategoryPathExcerptModel] = {
    val allowedCharLimit: Option[Int] = ruleCache.getSystemConfigByKey(Constants.MaxCharLimit, true).toIntOption
    val sample                        = getTruncatedText(node.code, allowedCharLimit)
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
      else
        s"${appCache.scanPath}/$fileName"
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
      val excerpt = dump(absoluteFileName, node.lineNumber, message, allowedCharLimit = allowedCharLimit)
      // Get the actual filename
      val actualFileName = {
        if (appCache.isLombokPresent)
          fileName.replace(s"${Constants.delombok}/", "")
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
    dataFlowModel: List[DataFlowPathModel],
    privadoInput: PrivadoInput,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    repoItemTagName: Option[String] = None,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache = PropertyFilterCache()
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
    val sourceExporter = new SourceExporter(cpg, ruleCache, privadoInput, repoItemTagName = repoItemTagName, appCache)
    val sinkExporter =
      new SinkExporter(
        cpg,
        ruleCache,
        privadoInput,
        repoItemTagName = repoItemTagName,
        s3DatabaseDetailsCache,
        appCache
      )
    val dataflowExporter = new DataflowExporter(dataflows, taggerCache)
    val collectionExporter =
      new CollectionExporter(cpg, ruleCache, repoItemTagName = repoItemTagName, appCache = appCache)
    val httpConnectionMetadataExporter = new HttpConnectionMetadataExporter(cpg, ruleCache, appCache)
    val androidPermissionsExporter =
      new AndroidPermissionsExporter(cpg, ruleCache, repoItemTagName = repoItemTagName, appCache = appCache)
    val probableSinkExporter =
      new ProbableSinkExporter(cpg, ruleCache, repoPath, repoItemTagName = repoItemTagName, appCache)
    val policyAndThreatExporter =
      new PolicyAndThreatExporter(cpg, ruleCache, taggerCache, dataFlowModel, privadoInput, appCache)
    val output = mutable.LinkedHashMap[String, Json]()

    output.addOne(Constants.coreVersion -> Environment.privadoVersionCore.asJson)
    output.addOne(Constants.cliVersion  -> Environment.privadoVersionCli.getOrElse(Constants.notDetected).asJson)
    output.addOne(Constants.mainVersion -> appCache.privadoVersionMain.asJson)
    output.addOne(Constants.privadoLanguageEngineVersion -> BuildInfo.joernVersion.asJson)
    output.addOne(Constants.createdAt                    -> Calendar.getInstance().getTimeInMillis.asJson)

    if (privadoInput.enableIngressAndEgressUrls) {
      output.addOne(Constants.ingressUrls -> Utilities.ingressUrls.toArray.asJson)
      output.addOne(Constants.egressUrls  -> httpConnectionMetadataExporter.getEgressUrls.toArray.asJson)
      output.addOne(
        Constants.egressUrlsFromCode -> httpConnectionMetadataExporter.getEgressUrlsFromCodeFiles.toArray.asJson
      )
      output.addOne(
        Constants.httpEndPointBasePaths -> httpConnectionMetadataExporter.getEndPointBasePath.toArray.asJson
      )
    }
    // To have the repoName as `pay` in nonMonolith case and in case of monolith as `pay/app/controller/payment_methods`
    output.addOne(
      Constants.repoName -> (if (repoItemTagName.isDefined)
                               s"${appCache.repoName}/${repoItemTagName.get.replaceAll("--", "/")}"
                             else appCache.repoName).asJson
    )
    output.addOne(Constants.language                  -> appCache.repoLanguage.toString.asJson)
    output.addOne(Constants.gitMetadata               -> GitMetaDataExporter.getMetaData(repoPath).asJson)
    output.addOne(Constants.localScanPath             -> appCache.localScanPath.asJson)
    output.addOne(Constants.probableSinks             -> probableSinkExporter.getProbableSinks.asJson)
    output.addOne(Constants.repoConfigMetaData        -> RepoConfigMetaDataExporter.getMetaData(cpg, ruleCache).asJson)
    output.addOne(Constants.propertyFileSkippedBySize -> propertyFilterCache.getFileSkippedBySizeData(ruleCache).asJson)
    output.addOne(
      Constants.propertyFileSkippedByDirCount -> propertyFilterCache.getFileSkippedDirCountData(ruleCache).asJson
    )

    // Future creates a thread and starts resolving the function call asynchronously
    val sources = Future {
      Try(sourceExporter.getSources).getOrElse(List[SourceModel]())
    }
    val processing = Future {
      Try(sourceExporter.getProcessing).getOrElse(List[SourceProcessingModel]())
    }
    val sinks = Future {
      Try(sinkExporter.getSinks).getOrElse(List[SinkModel]())
    }
    val processingSinks = Future {
      Try(sinkExporter.getProcessing).getOrElse(List[SinkProcessingModel]())
    }
    val collections = Future {
      Try(collectionExporter.getCollections).getOrElse(List[CollectionModel]())
    }

    val finalCollections = Await.result(collections, Duration.Inf)
    logger.debug("Done with exporting Collections")
    val violationResult =
      Try(policyAndThreatExporter.getViolations(repoPath, finalCollections, appCache)).getOrElse(List[ViolationModel]())
    output.addOne(Constants.violations -> violationResult.asJson)
    logger.debug("Done with exporting Violations")

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
          .getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet, ruleCache, dataFlowModel, appCache)
          .toList
      )
    })

    output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)

    if (privadoInput.assetDiscovery) {
      val propertyNodesData =
        cpg.property.map(p => PropertyNodesModel(p.name, p.value, p.file.name.headOption.getOrElse(""))).dedup.l
      output.addOne("propertyNodesData" -> propertyNodesData.asJson)

      val probablePropertyNodes = propertyNodesData
        .filterNot(_.fileName.matches(".*package(-lock)?.json"))
        .or(
          _.filter(
            _.key.matches(
              "(?i).*(connection|host|database|uri$|mongo|sql|postgres|pgsql|s3|oracle|redis|bucket|dynamo|hbase|neo4j|cassandra|couchbase|memcached|couchdb|litedb|LiteDatabase|tinydb|maria|db(_)?name).*"
            )
          ),
          _.filter(
            _.value.matches(
              "(?i).*(mongo|sql|postgres|pgsql|aws|oracle|redis|dynamo|hbase|neo4j|cassandra|couchbase|memcached|couchdb|litedb|LiteDatabase|tinydb|maria).*"
            )
          )
        )
        .filterNot(_.value.matches(".*[.](png|jpg|jpeg|jar|zip|xml|json|yml)$"))
        .filterNot(_.value.matches("^(true|false)$"))
        .filterNot(_.value.matches("require[(]"))
        .l

      println("Printing probable assets")
      probablePropertyNodes.foreach(item => println(s"${item.key}, ${item.value}"))

      output.addOne("probableAssets" -> probablePropertyNodes.asJson)

      // Run AssetTagger
      // Turn off the AssetTagger on source code for now
      // new AssetTagger(cpg).createAndApply()

      val probableSourceFromCode = cpg.identifier.where(_.tag.nameExact(InternalTag.PROBABLE_ASSET.toString)).l ++
        cpg.literal.where(_.tag.nameExact(InternalTag.PROBABLE_ASSET.toString)).l ++
        cpg.fieldAccess.fieldIdentifier.where(_.tag.nameExact(InternalTag.PROBABLE_ASSET.toString)).l ++
        cpg.member.where(_.tag.nameExact(InternalTag.PROBABLE_ASSET.toString)).l

      import ai.privado.model.exporter.DataFlowEncoderDecoder._
      output.addOne(
        "probableAssetsFromCode" -> probableSourceFromCode
          .map(node => {
            val fileName = Utilities.getFileNameForNode(node)
            val absoluteFileName = {
              val file = File(fileName)
              if (file.exists)
                fileName
              else
                s"${appCache.scanPath}/$fileName"
            }

            DataFlowSubCategoryPathExcerptModel(
              node.code,
              node.lineNumber.get,
              node.columnNumber.get,
              fileName,
              Utilities.dump(absoluteFileName, node.lineNumber, excerptStartLine = -1, excerptEndLine = 1)
            )
          })
          .asJson
      )
    }

    val androidPermissions = Future {
      Try(androidPermissionsExporter.getPermissions).getOrElse(List[AndroidPermissionModel]())
    }

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

    val _sources = Await.result(sources, Duration.Inf)
    logger.debug("Done with exporting Sources")
    val _processing = Await.result(processing, Duration.Inf)
    logger.debug("Done with exporting Processing sources")
    val _sinks = Await.result(sinks, Duration.Inf)
    logger.debug("Done with exporting Sinks")
    val _processingSinks = Await.result(processingSinks, Duration.Inf)
    logger.debug("Done with exporting Processing Sinks")
    val _permissions = Await.result(androidPermissions, Duration.Inf)
    logger.debug("Done with exporting android permissions")

    output.addOne(Constants.sources            -> _sources.asJson)
    output.addOne(Constants.processing         -> _processing.asJson)
    output.addOne(Constants.sinks              -> _sinks.asJson)
    output.addOne(Constants.sinkProcessing     -> _processingSinks.asJson)
    output.addOne(Constants.collections        -> finalCollections.asJson)
    output.addOne(Constants.androidPermissions -> _permissions.asJson)
    (output, _sources, _sinks, _processing, dataflowsOutput, finalCollections, complianceViolations.size)
  }

  def filterNodeBasedOnRepoItemTagName(nodes: List[AstNode], repoItemTagName: Option[String]): List[AstNode] = {
    if (repoItemTagName.isDefined)
      nodes.where(_.tag.nameExact(Constants.monolithRepoItem).valueExact(repoItemTagName.get)).l
    else
      nodes
  }

}
