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
import ai.privado.model.{Constants, DataFlowPathModel, NodeType}
import ai.privado.tagger.PrivadoSimpleCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class DatabaseRepositoryReadPass(cpg: Cpg, taggerCache: TaggerCache, dataFlowCache: DataFlowCache)
    extends PrivadoSimpleCpgPass(cpg) {

  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys.l
  implicit val resolver: ICallResolver = NoResolve

  val logger = LoggerFactory.getLogger(getClass)
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.call
      .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.storages))
      .where(_.tag.nameExact(Constants.id).value(".*Read.*"))
      .foreach(callNode => {
        try {
          val methodNode = callNode.callee.head
          // method signature is ai.privado.User(java.lang.String)
          methodNode.signature.split("\\(").headOption match {
            case Some(returnedFullName) =>
              sensitiveClasses.find(_.equals(returnedFullName)) match {
                case Some(matchedTypeDeclFullName) =>
                  addFlowToDataFlowCache(taggerCache, matchedTypeDeclFullName, callNode, dataFlowCache)
                case None =>
                  // This case is to get the Returned object from the code
                  // Ex - Optional<UserE> resp = userr.findByEmail(login.getEmail());
                  // Our intention is to get the UserE present inside <> and retrieve the corresponding TypeDecl information
                  methodNode.code.trim.split(" ").find(_.contains(">")) match {
                    case Some(returnValue) =>
                      returnValue.stripSuffix(">").split("<").lastOption match {
                        case Some(returnClassValue) =>
                          sensitiveClasses.find(_.endsWith("." + returnClassValue)) match {
                            case Some(matchedTypeDeclFullName) =>
                              addFlowToDataFlowCache(taggerCache, matchedTypeDeclFullName, callNode, dataFlowCache)
                            case None =>
                          }
                        case None =>
                      }
                    case None =>
                  }
              }
            case None =>
          }
        } catch {
          case e: Exception => logger.debug(s"Exception when identifying database read DataElement $e")
        }
      })
  }

  private def addFlowToDataFlowCache(
    taggerCache: TaggerCache,
    matchedTypeDeclFullName: String,
    callNode: AstNode,
    dataFlowCache: DataFlowCache
  ): Unit = {
    Utility
      .appendExtraNodesAndRetunNewFlow(taggerCache, matchedTypeDeclFullName, callNode)
      .foreach(entry => {
        synchronized {
          val (pathId, sourceRuleId, path) = entry
          // We need to update the dataflowsMap and set new dataflow using setDataflow function
          dataFlowCache.dataflowsMapByType.put(pathId, path)
          dataFlowCache.setDataflow(
            DataFlowPathModel(
              sourceRuleId,
              callNode.tag.nameExact(Constants.id).value(".*Read.*").value.head,
              Constants.storages,
              NodeType.REGULAR.toString,
              pathId,
              applyDedup = false
            )
          )
        }

      })

  }
}
