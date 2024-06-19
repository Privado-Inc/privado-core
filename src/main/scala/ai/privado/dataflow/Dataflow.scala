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

package ai.privado.dataflow

import ai.privado.audit.UnresolvedFlowReport
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow.getExpendedFlowInfo
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.default.NodeStarters
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.languageEngine.javascript.JavascriptSemanticGenerator
import ai.privado.languageEngine.python.semantic.PythonSemanticGenerator
import ai.privado.model.exporter.DataFlowPathIntermediateModel
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language}
import ai.privado.utility.{StatsRecorder, Utilities}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, CfgNode, HightouchSink}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal
import ai.privado.tagger.utility.SourceTaggerUtility.getFilteredSourcesByTaggingDisabled

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

class Dataflow(cpg: Cpg, statsRecorder: StatsRecorder) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Compute the flow of data from tagged Sources to Sinks
    *
    * @return
    *   \- Map of PathId -> Path corresponding to source to sink path
    */
  def dataflow(
    privadoScanConfig: PrivadoInput,
    ruleCache: RuleCache,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    appCache: AppCache
  ): Map[String, Path] = {

    if (privadoScanConfig.generateAuditReport && privadoScanConfig.enableAuditSemanticsFilter) {
      auditCache.addIntoBeforeSemantics(cpg, privadoScanConfig, ruleCache, appCache)
    }

    logger.info("Generating dataflow")
    implicit val engineContext: EngineContext =
      Utilities.getEngineContext(privadoScanConfig, appCache, 4)(semanticsP =
        getSemantics(cpg, privadoScanConfig, ruleCache, appCache)
      )

    val sources = Dataflow.getSources(cpg)
    var sinks   = Dataflow.getSinks(cpg)
    statsRecorder.justLogMessage(s"no of source nodes - ${sources.size}")
    statsRecorder.justLogMessage(s"no of sinks nodes - ${sinks.size}")

    if (privadoScanConfig.limitNoSinksForDataflows > -1) {
      sinks = sinks.take(privadoScanConfig.limitNoSinksForDataflows)
      statsRecorder.justLogMessage(s"no of sinks nodes post limit - ${sinks.size}")
    }

    if (sources.isEmpty || sinks.isEmpty) Map[String, Path]()
    else {
      statsRecorder.initiateNewStage("Finding flows")
      val dataflowPathsUnfiltered = {
        if (privadoScanConfig.disable2ndLevelClosure) sinks.reachableByFlows(sources).l
        else {
          val firstLevelSources =
            sources.or(
              _.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name),
              _.tag.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
            )
          sinks.reachableByFlows(firstLevelSources).l
        }
        // Commented the below piece of code as we still need to test out and fix few open Issues which are
        // resulting in FP in 2nd level derivation for Storages
        /*
        if (privadoScanConfig.disable2ndLevelClosure)
          sinks.reachableByFlows(sources).l
        else {
          // If 2nd level is turned off then dataflows for storages should consider Derived Sources also, but for rest only Sources
          val nonStorageSources =
            sources.or(
              _.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name),
              _.tag.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
            )
          val nonStorageSinks = sinks.whereNot(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
          val storageSinks    = sinks.where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))

          val nonStorageFlows = nonStorageSinks.reachableByFlows(nonStorageSources).toSet
          val storageFlows    = storageSinks.reachableByFlows(sources).toSet
          (nonStorageFlows ++ storageFlows).l
        }
         */
      }

      if (privadoScanConfig.generateAuditReport) {
        auditCache.addIntoBeforeFirstFiltering(dataflowPathsUnfiltered, privadoScanConfig, ruleCache)

        // For Unresolved flow sheet
        val unfilteredSinks = UnresolvedFlowReport.getUnresolvedSink(cpg)
        val unresolvedFlows = unfilteredSinks.reachableByFlows(sources).l
        auditCache.setUnfilteredFlow(getExpendedFlowInfo(unresolvedFlows, appCache, ruleCache))
      }

      // Storing the pathInfo into dataFlowCache
      if (privadoScanConfig.testOutput || privadoScanConfig.generateAuditReport) {
        dataFlowCache.intermediateDataFlow = getExpendedFlowInfo(dataflowPathsUnfiltered, appCache, ruleCache)
      }
      statsRecorder.endLastStage()
      statsRecorder.justLogMessage(s"Unique flows - ${dataflowPathsUnfiltered.size}")
      statsRecorder.initiateNewStage("Filtering flows 1")
      appCache.totalFlowFromReachableBy = dataflowPathsUnfiltered.size

      // Apply `this` filtering for JS, JAVA
      val dataflowPaths = {
        if (
          privadoScanConfig.disableThisFiltering || (!List(Language.JAVA, Language.JAVASCRIPT)
            .contains(appCache.repoLanguage))
        )
          dataflowPathsUnfiltered
        else
          dataflowPathsUnfiltered
            .filter(DuplicateFlowProcessor.filterFlowsByContext)
            .filter(DuplicateFlowProcessor.flowNotTaintedByThis)
      }
      statsRecorder.endLastStage()
      statsRecorder.justLogMessage(s"Unique flows - ${dataflowPaths.size}")
      appCache.totalFlowAfterThisFiltering = dataflowPaths.size
      // Stores key -> PathID, value -> Path
      val dataflowMapByPathId = dataflowPaths
        .flatMap(dataflow => {
          DuplicateFlowProcessor.calculatePathId(dataflow) match {
            case Success(pathId) => Some(pathId, dataflow)
            case Failure(e) =>
              logger.debug("Exception : ", e)
              None
          }
        })
        .toMap

      // Setting cache
      dataflowMapByPathId.foreach(item => {
        dataFlowCache.dataflowsMapByType.put(item._1, item._2)
      })

      statsRecorder.initiateNewStage("Filtering flows 2")
      DuplicateFlowProcessor.filterIrrelevantFlowsAndStoreInCache(
        dataflowMapByPathId,
        privadoScanConfig,
        ruleCache,
        dataFlowCache,
        auditCache,
        appCache
      )
      statsRecorder.endLastStage()
      statsRecorder.justLogMessage(s"Final flows - ${dataFlowCache.getDataflowBeforeDedup.size}")
    }
    // Need to return the filtered result
    statsRecorder.initiateNewStage("Deduplicating flows")
    val dataflowFromCache = dataFlowCache.getDataflowAfterDedup
    statsRecorder.endLastStage()
    statsRecorder.justLogMessage(s"Unique flows - ${dataflowFromCache.size}")
    auditCache.addIntoFinalPath(dataflowFromCache)
    dataflowFromCache
      .map(_.pathId)
      .toSet
      .map((pathId: String) => (pathId, dataFlowCache.dataflowsMapByType.get(pathId)))
      .toMap
  }

  def getSemantics(cpg: Cpg, privadoScanConfig: PrivadoInput, ruleCache: RuleCache, appCache: AppCache): Semantics = {
    val lang = appCache.repoLanguage
    lang match {
      case Language.JAVA =>
        JavaSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache, exportRuntimeSemantics = true)
      case Language.PYTHON =>
        PythonSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache, exportRuntimeSemantics = true)
      case Language.JAVASCRIPT =>
        JavascriptSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache, exportRuntimeSemantics = true)
      case _ => JavaSemanticGenerator.getDefaultSemantics
    }
  }

}

object Dataflow {

  def dataflowForSourceSinkPair(
    sources: List[AstNode],
    sinks: List[CfgNode],
    privadoInputConfig: PrivadoInput,
    appCache: AppCache
  ): List[Path] = {
    sinks.reachableByFlows(sources)(Utilities.getEngineContext(privadoInputConfig, appCache)).l
  }

  def getSources(cpg: Cpg): List[AstNode] = {
    def filterSources(traversal: Traversal[AstNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
    }

    val sources: List[AstNode] =
      cpg.literal
        .where(filterSources)
        .l ++ cpg.identifier
        .where(filterSources)
        .l ++ cpg.call
        .where(filterSources)
        .l ++ cpg.argument.isFieldIdentifier.where(filterSources).l ++ cpg.member.where(filterSources).l

    // Remove TAGGING_DISABLED_BY_DED sources from list
    getFilteredSourcesByTaggingDisabled(sources)
  }

  def getSinks(cpg: Cpg): List[CfgNode] = {
    // TODO:  This print statement is for debug purpose only, remove it after testing
    cpg.highTouchSink
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .foreach(sink =>
        println(
          s"Tagged: ${sink.name}, which has corresponding model as ```${sink.correspondingModel}``` which corresponds to the 3P: ```${sink.actualDestinationName}```. This was discovered in ```${sink.sourceFileOut.name.headOption
              .getOrElse("Unknown file")}```"
        )
      )
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l ++ cpg.highTouchSink
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .l
      .asInstanceOf[List[CfgNode]]
  }

  def getExpendedFlowInfo(
    dataflowPathsUnfiltered: List[Path],
    appCache: AppCache,
    ruleCache: RuleCache
  ): List[DataFlowPathIntermediateModel] = {
    // Fetching the sourceId, sinkId and path Info
    val expendedFlow = ListBuffer[DataFlowPathIntermediateModel]()
    dataflowPathsUnfiltered.map(path => {
      val paths = path.elements.map(node =>
        ExporterUtility.convertIndividualPathElement(node, appCache = appCache, ruleCache = ruleCache)
      )
      val pathId   = DuplicateFlowProcessor.calculatePathId(path)
      var sourceId = ""
      if (path.elements.head.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name).nonEmpty) {
        sourceId = path.elements.head.tag
          .nameExact(Constants.id)
          .valueNot(Constants.privadoDerived + ".*")
          .value
          .headOption
          .getOrElse("")
      } else {
        sourceId = Iterator(path.elements.head).isIdentifier.typeFullName.headOption.getOrElse("")
      }
      var sinkId = path.elements.last.tag.nameExact(Constants.id).value.headOption.getOrElse("")
      // fetch call node methodeFullName if tag not present
      if (sinkId.isEmpty) {
        sinkId = path.elements.last.asInstanceOf[Call].methodFullName
      }
      expendedFlow += DataFlowPathIntermediateModel(sourceId, sinkId, pathId.getOrElse(""), paths)
    })
    expendedFlow.toList
  }
}
