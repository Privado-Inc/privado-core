/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.exporter

import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class CollectionExporter(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Processes collection points and return final output
    */
  def getCollections: immutable.Iterable[mutable.LinkedHashMap[String, Json]] = {
    val collectionMapByCollectionId = cpg.method
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
      .l
      .groupBy(collectionMethod => collectionMethod.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId.map(entrySet => processByCollectionId(entrySet._1, entrySet._2))
  }

  private def processByCollectionId(collectionId: String, collectionMethods: List[Method]) = {

    val collectionParameterMapById = mutable.HashMap[String, ListBuffer[MethodParameterIn]]()

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.parameter
        .or(_.tag.nameExact(Constants.id), _.tag.name(Constants.privadoDerived + ".*"))
        .foreach(parameter => {
          try {
            def addToMap(parameterId: String): Unit = {
              if (!collectionParameterMapById.contains(parameterId))
                collectionParameterMapById(parameterId) = ListBuffer()
              collectionParameterMapById(parameterId).append(parameter)
            }
            parameter.tag
              .nameExact(Constants.id)
              .value
              .filter(!_.startsWith(Constants.privadoDerived))
              .foreach(addToMap)
            parameter.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)

          } catch {
            case e: Exception => logger.debug("Exception : ", e)
          }
        })
    })

    val collectionOutput = mutable.LinkedHashMap[String, Json]()
    collectionOutput.addOne(Constants.collectionId -> collectionId.asJson)
    val ruleInfoForExporting = ExporterUtility.getRuleInfoForExporting(collectionId)
    ruleInfoForExporting.remove(Constants.id)
    collectionOutput.addAll(ruleInfoForExporting)
    collectionOutput.addOne(
      Constants.collections -> collectionParameterMapById
        .map(entrySet => processByParameterId(entrySet._1, entrySet._2.toList))
        .asJson
    )

    collectionOutput
  }

  def processByParameterId(parameterId: String, methodParameterOccurrences: List[MethodParameterIn]) = {

    val parameterCollectionOutput = mutable.LinkedHashMap[String, Json]()
    parameterCollectionOutput.addOne(Constants.sourceId -> parameterId.asJson)
    parameterCollectionOutput.addOne(
      Constants.occurrences -> methodParameterOccurrences
        .map(methodParameter => {
          val parameterOccurrenceMap = mutable.LinkedHashMap[String, Json]()
          parameterOccurrenceMap.addOne(Constants.endPoint -> getCollectionUrl(methodParameter).asJson)
          parameterOccurrenceMap.addAll(ExporterUtility.convertIndividualPathElement(methodParameter).get)
          parameterOccurrenceMap
        })
        .asJson
    )
    parameterCollectionOutput
  }

  /** Returns rest Url for this parameter's method
    * @param parameterIn
    * @return
    */
  private def getCollectionUrl(parameterIn: MethodParameterIn) = {
    Try(Traversal(parameterIn).method.tag.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).value.head) match {
      case Success(url) => url
      case Failure(e) =>
        logger.debug("Exception : ", e)
        ""
    }
  }
}
