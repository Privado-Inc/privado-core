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
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, NodeType}
import ai.privado.tagger.{PrivadoParallelCpgPass, PrivadoSimpleCpgPass}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

class MessagingConsumerReadPass(cpg: Cpg, taggerCache: TaggerCache) extends PrivadoSimpleCpgPass(cpg) {

  /*override def generateParts(): Array[String] =
    List(Constants.jmsConsumerRuleId, Constants.kafkaConsumerRuleId).toArray


   */
  override def run(builder: DiffGraphBuilder): Unit = {

    val consumerRuleId = Constants.kafkaConsumerRuleId
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
            addToDataflowCache(consumerRuleId, sourceRuleId, path, pathId)
          }
        })
    })

    // Case when the method paraeter is not a derived source, but probably a String

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
    val readFlow    = Dataflow.dataflowForSourceSinkPair(dataflowSource, dataflowSink)
    val uniqueFlows = DuplicateFlowProcessor.getUniquePathsAfterDedup(readFlow)
    uniqueFlows.foreach { flow =>
      val readNode = flow.elements.last.asInstanceOf[Call]
      val pathId   = DuplicateFlowProcessor.calculatePathId(flow).get
      readNode.argument.code(".*[.]class").isCall.argument.isIdentifier.typeFullName.headOption match {
        case Some(readTypeFullName) =>
          if (taggerCache.typeDeclMemberCache.contains(readTypeFullName)) {
            taggerCache.typeDeclMemberCache(readTypeFullName).keys.foreach { sourceRuleId =>
              addToDataflowCache(consumerRuleId, sourceRuleId, flow, pathId)
            }
          }
        case None =>
      }
    }
  }

  private def addToDataflowCache(consumerRuleId: String, sourceRuleId: String, flow: Path, pathId: String): Unit =
    synchronized {
      // We need to update the dataflowsMap and set new dataflow using setDataflow function
      DataFlowCache.dataflowsMapByType.put(pathId, flow)
      DataFlowCache.setDataflow(
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
