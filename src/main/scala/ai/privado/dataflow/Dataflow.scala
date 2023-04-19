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

import ai.privado.cache.{AppCache, DataFlowCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor, TimeMetric}
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.semantic.SemanticGenerator
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language}
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal
import ai.privado.model.exporter.DataFlowPathIntermediateModel

import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  implicit val engineContext: EngineContext =
    EngineContext(semantics = SemanticGenerator.getSemantics(cpg, ScanProcessor.config), config = EngineConfig(4))

  /** Compute the flow of data from tagged Sources to Sinks
    * @return
    *   \- Map of PathId -> Path corresponding to source to sink path
    */
  def dataflow(privadoScanConfig: PrivadoInput): Map[String, Path] = {

    logger.info("Generating dataflow")
    val sources = Dataflow.getSources(cpg)
    val sinks   = Dataflow.getSinks(cpg)

    println(s"${TimeMetric.getNewTimeAndSetItToStageLast()} - --no of source nodes - ${sources.size}")
    println(s"${TimeMetric.getNewTimeAndSetItToStageLast()} - --no of sinks nodes - ${sinks.size}")

    if (sources.isEmpty || sinks.isEmpty)
      Map[String, Path]()
    else {
      println(s"${TimeMetric.getNewTimeAndSetItToStageLast()} - --Finding flows invoked...")
      val dataflowPathsUnfiltered = {
        if (privadoScanConfig.disable2ndLevelClosure)
          sinks.reachableByFlows(sources).l
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

      if (ScanProcessor.config.testOutput) {
        val intermediateDataflow = ListBuffer[DataFlowPathIntermediateModel]()
        // Fetching the sourceId, sinkId and path Info
        dataflowPathsUnfiltered.map(path => {
          val paths    = path.elements.map(node => ExporterUtility.convertIndividualPathElement(node))
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
            sourceId = Traversal(path.elements.head).isIdentifier.typeFullName.headOption.getOrElse("")
          }
          val sinkId = path.elements.last.tag.nameExact(Constants.id).value.headOption.getOrElse("")
          intermediateDataflow += DataFlowPathIntermediateModel(sourceId, sinkId, pathId.getOrElse(""), paths)
        })

        // Storing the pathInfo into dataFlowCache
        DataFlowCache.intermediateDataFlow = intermediateDataflow.toList
      }

      println(s"${TimeMetric.getNewTime()} - --Finding flows is done in \t\t\t- ${TimeMetric
          .setNewTimeToStageLastAndGetTimeDiff()} - Unique flows - ${dataflowPathsUnfiltered.size}")
      println(s"${Calendar.getInstance().getTime} - --Filtering flows 1 invoked...")
      AppCache.totalFlowFromReachableBy = dataflowPathsUnfiltered.size
      val dataflowPaths = {
        if (ScanProcessor.config.disableThisFiltering || AppCache.repoLanguage != Language.JAVA)
          dataflowPathsUnfiltered
        else
          dataflowPathsUnfiltered
            .filter(DuplicateFlowProcessor.filterFlowsByContext)
            .filter(DuplicateFlowProcessor.flowNotTaintedByThis)
      }
      println(s"${TimeMetric.getNewTime()} - --Filtering flows 1 is done in \t\t\t- ${TimeMetric
          .setNewTimeToStageLastAndGetTimeDiff()} - Unique flows - ${dataflowPaths.size}")
      AppCache.totalFlowAfterThisFiltering = dataflowPaths.size
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
      DataFlowCache.dataflowsMapByType ++= dataflowMapByPathId

      println(s"${Calendar.getInstance().getTime} - --Filtering flows 2 is invoked...")
      DuplicateFlowProcessor.filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId, privadoScanConfig)
      println(
        s"${TimeMetric.getNewTime()} - --Filtering flows 2 is done in \t\t\t- ${TimeMetric
            .setNewTimeToStageLastAndGetTimeDiff()} - Final flows - ${DataFlowCache.dataflow.values.flatMap(_.values).flatten.size}"
      )
      // Need to return the filtered result
      println(s"${Calendar.getInstance().getTime} - --Deduplicating flows invoked...")
      val dataflowFromCache = DataFlowCache.getDataflow
      println(s"${TimeMetric.getNewTime()} - --Deduplicating flows is done in \t\t- ${TimeMetric
          .setNewTimeToStageLastAndGetTimeDiff()} - Unique flows - ${dataflowFromCache.size}")

      dataflowFromCache
        .map(_.pathId)
        .toSet
        .map((pathId: String) => (pathId, DataFlowCache.dataflowsMapByType(pathId)))
        .toMap
    }
  }

}

object Dataflow {
  def getSources(cpg: Cpg): List[AstNode] = {
    def filterSources(traversal: Traversal[AstNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
    }

    cpg.literal
      .where(filterSources)
      .l ++ cpg.identifier
      .where(filterSources)
      .l ++ cpg.call
      .where(filterSources)
      .l ++ cpg.argument.isFieldIdentifier.where(filterSources).l ++ cpg.member.where(filterSources).l
  }

  def getSinks(cpg: Cpg): List[CfgNode] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }
}
