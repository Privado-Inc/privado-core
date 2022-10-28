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

package ai.privado.exporter.javascript

import ai.privado.exporter.ExporterUtility
import ai.privado.model.InternalTag
import ai.privado.model.exporter.{CollectionModel, CollectionOccurrenceDetailModel, CollectionOccurrenceModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CollectionExporter(cpg: Cpg) {
  def processByCollectionId(collectionId: String, collectionCalls: List[Call]) = {

    val collectionSourceMapById = mutable.HashMap[String, ListBuffer[Call]]()
    collectionCalls.foreach(collectionCall => {
      collectionCall.tag
        .where(_.name(InternalTag.COLLECTION_METHOD_SOURCE_RULE.toString + ".*"))
        .value
        .foreach(sourceRuleId => {
          if (!collectionSourceMapById.contains(sourceRuleId))
            collectionSourceMapById(sourceRuleId) = ListBuffer()
          collectionSourceMapById(sourceRuleId).append(collectionCall)
        })
    })

    val ruleInfo = ExporterUtility.getRuleInfoForExporting(collectionId)
    CollectionModel(
      collectionId,
      ruleInfo.name,
      ruleInfo.isSensitive,
      collectionSourceMapById
        .map(entrySet => processBySourceId(entrySet._1, entrySet._2.toList))
        .toList
    )
  }

  def processBySourceId(sourceRuleId: String, collectionPointCalls: List[Call]): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      sourceRuleId,
      collectionPointCalls
        .flatMap(collectionPointCall => {
          ExporterUtility.convertIndividualPathElement(collectionPointCall) match {
            case Some(pathElement) =>
              Some(
                CollectionOccurrenceModel(
                  collectionPointCall.tag
                    .where(_.name(InternalTag.COLLECTION_METHOD_ENDPOINT.toString))
                    .value
                    .headOption
                    .getOrElse(""),
                  collectionPointCall.name,
                  pathElement.lineNumber,
                  pathElement.columnNumber,
                  pathElement.fileName,
                  pathElement.excerpt
                )
              )
            case None => None
          }
        })
    )
  }

}
