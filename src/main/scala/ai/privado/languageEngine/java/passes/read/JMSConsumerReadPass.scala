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
import ai.privado.model.{Constants, DataFlowPathModel, NodeType}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

class JMSConsumerReadPass(cpg: Cpg, taggerCache: TaggerCache) extends CpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val dataflowSource = cpg.method
      .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
      .where(_.tag.nameExact(Constants.id).valueExact(Constants.jmsConsumerRuleId))
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
      readNode.argument.code(".*[.]class").isCall.argument.isIdentifier.typeFullName.headOption match {
        case Some(readTypeFullName) =>
          if (taggerCache.typeDeclMemberCache.contains(readTypeFullName)) {
            taggerCache.typeDeclMemberCache(readTypeFullName).keys.foreach { sourceRuleId =>
              val pathId = DuplicateFlowProcessor.calculatePathId(flow).get

              // We need to update the dataflowsMap and set new dataflow using setDataflow function
              DataFlowCache.dataflowsMapByType ++= List((pathId, flow)).toMap
              DataFlowCache.setDataflow(
                DataFlowPathModel(
                  sourceRuleId,
                  Constants.jmsConsumerRuleId,
                  Constants.storages,
                  NodeType.REGULAR.toString,
                  pathId,
                  applyDedup = false
                )
              )
            }
          }
        case None =>
      }
    }
  }
}
