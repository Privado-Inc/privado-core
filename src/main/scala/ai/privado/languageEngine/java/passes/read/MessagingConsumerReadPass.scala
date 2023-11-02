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

package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{DataFlowCache, TaggerCache}
import ai.privado.dataflow.{Dataflow, DuplicateFlowProcessor}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, NodeType}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode}
import io.shiftleft.semanticcpg.language.*

class MessagingConsumerReadPass(
  cpg: Cpg,
  taggerCache: TaggerCache,
  dataFlowCache: DataFlowCache,
  privadoInputConfig: PrivadoInput
) extends PrivadoParallelCpgPass[String](cpg) {

  override def generateParts(): Array[String] =
    List(Constants.jmsConsumerRuleId, Constants.kafkaConsumerRuleId).toArray

  override def runOnPart(builder: DiffGraphBuilder, consumerRuleId: String): Unit = {

    generateReadFlowAndAddToDataflowCache(consumerRuleId)
  }

  private def generateReadFlowAndAddToDataflowCache(consumerRuleId: String): Unit = {

    /*
    Case when the deserialized object is part of the methodParameter and is a Derived source
    Ex - @KafkaListener(topics = "orders", groupId = "inventory")
        public void consume(Order order) throws IOException {
            log.info("Order received to process: {}", order);
            }
     */
    val directlyConsumedParameters = cpg.method
      .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
      .where(_.tag.nameExact(Constants.id).valueExact(consumerRuleId))
      .parameter
      .filter(parameter => taggerCache.typeDeclMemberCache.contains(parameter.typeFullName))
      .l
    directlyConsumedParameters.foreach(consumedParameter => {
      Utility
        .appendExtraNodesAndRetunNewFlow(taggerCache, consumedParameter.typeFullName, consumedParameter)
        .foreach(entry => {
          synchronized {
            val (pathId, sourceRuleId, path) = entry
            addToDataflowCache(consumerRuleId, sourceRuleId, path, pathId, dataFlowCache)
          }
        })
    })

    // Case when the method parameter is not a derived source, but probably a String

    val dataflowSource = cpg.method
      .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
      .where(_.tag.nameExact(Constants.id).valueExact(consumerRuleId))
      .whereNot(_.parameter.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
      .parameter
      .filter(_.index != 0)
      .map(_.referencingIdentifiers.headOption)
      .filter(_.isDefined)
      .map(_.get)
      .l

    val dataflowSink =
      cpg.call.methodFullName("(?i)(.*ObjectMapper[.](readValue|convertValue):.*)|(.*gson[.](fromJson[\\w]*):.*)").l
    val readFlow    = Dataflow.dataflowForSourceSinkPair(dataflowSource, dataflowSink, privadoInputConfig)
    val uniqueFlows = DuplicateFlowProcessor.getUniquePathsAfterDedup(readFlow)
    uniqueFlows.foreach { flow =>
      val readNode = flow.elements.last.asInstanceOf[Call]
      val pathId   = DuplicateFlowProcessor.calculatePathId(flow).get
      readNode.argument.code(".*[.]class").isCall.argument.isIdentifier.typeFullName.headOption match {
        case Some(readTypeFullName) =>
          if (taggerCache.typeDeclMemberCache.contains(readTypeFullName)) {
            taggerCache.getTypeDeclMemberCacheItem(readTypeFullName).keys.foreach { sourceRuleId =>
              addToDataflowCache(consumerRuleId, sourceRuleId, flow, pathId, dataFlowCache)
            }
          }
        case None =>
      }
    }

    generateReadFlowAndAddToDataflowCachePOJO(consumerRuleId)
  }

  /** Helper function to solve cases like
    *
    * ConsumerRecords<String, Integer> records = consumer.poll(Duration.ofMillis(1000));
    * StreamSupport.stream(records.spliterator(), false) .map(record -> {CountryPopulation cp = new
    * CountryPopulation(record.key(), record.value()); cp;}) .forEach(countryPopulationConsumer);
    *
    * consumer.poll is the sink here, we are looking to identify the object cp of type CountryPopulation
    * @param consumerRuleId
    */
  private def generateReadFlowAndAddToDataflowCachePOJO(consumerRuleId: String): Unit = {
    val dataflowReadSource = cpg.call
      .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
      .where(_.tag.nameExact(Constants.id).valueExact(consumerRuleId))
      .l
    val dataflowReadSink = Dataflow
      .getSources(cpg)
      .filter(_.isInstanceOf[CfgNode])
      .map(_.asInstanceOf[CfgNode])
      .l

    val dataflowReadFlows = Dataflow.dataflowForSourceSinkPair(dataflowReadSource, dataflowReadSink, privadoInputConfig)
    val dataflowUniqueFlows = DuplicateFlowProcessor.getUniquePathsAfterDedup(dataflowReadFlows)
    dataflowUniqueFlows
      .foreach { flow =>
        val sinkNode     = flow.elements.last
        val sourceRuleId = sinkNode.tag.value("Data.Sensitive.*").value.headOption.getOrElse("")
        if (sourceRuleId.nonEmpty) {
          val path   = new Path(List(sinkNode))
          val pathId = DuplicateFlowProcessor.calculatePathId(path).get
          addToDataflowCache(consumerRuleId, sourceRuleId, path, pathId, dataFlowCache)
        }
      }
  }

  private def addToDataflowCache(
    consumerRuleId: String,
    sourceRuleId: String,
    flow: Path,
    pathId: String,
    dataFlowCache: DataFlowCache
  ): Unit =
    synchronized {
      // We need to update the dataflowsMap and set new dataflow using setDataflow function
      dataFlowCache.dataflowsMapByType.put(pathId, flow)
      dataFlowCache.setDataflow(
        DataFlowPathModel(
          sourceRuleId,
          consumerRuleId,
          Constants.storages,
          NodeType.REGULAR.toString,
          pathId,
          applyDedup = false
        )
      )
    }
}
