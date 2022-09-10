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
 */

package ai.privado.exporter

import ai.privado.model.exporter.{CollectionOccurrenceDetailModel, CollectionOccurrenceModel, CollectionModel}
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class CollectionExporter(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Processes collection points and return final output
    */
  def getCollections: List[CollectionModel] = {
    val collectionMapByCollectionId = cpg.method
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
      .l
      .groupBy(collectionMethod => collectionMethod.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId.map(entrySet => processByCollectionId(entrySet._1, entrySet._2)).toList
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

    val ruleInfo = ExporterUtility.getRuleInfoForExporting(collectionId)
    CollectionModel(
      collectionId,
      ruleInfo.name,
      ruleInfo.isSensitive,
      collectionParameterMapById
        .map(entrySet => processByParameterId(entrySet._1, entrySet._2.toList))
        .toList
    )
  }

  def processByParameterId(
    parameterId: String,
    methodParameterOccurrences: List[MethodParameterIn]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      parameterId,
      methodParameterOccurrences
        .flatMap(methodParameter => {
          ExporterUtility.convertIndividualPathElement(methodParameter) match {
            case Some(pathElement) =>
              Some(
                CollectionOccurrenceModel(
                  getCollectionUrl(methodParameter),
                  pathElement.sample,
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

  /** Returns rest Url for this parameter's method
    * @param parameterIn
    *   \- methodParameter
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
