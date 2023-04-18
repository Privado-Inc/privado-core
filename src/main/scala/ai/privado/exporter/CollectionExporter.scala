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

import ai.privado.cache.RuleCache
import ai.privado.model.exporter.{
  CollectionModel,
  CollectionOccurrenceDetailModel,
  CollectionOccurrenceModel,
  DataFlowSubCategoryPathExcerptModel
}
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Literal, Local, Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class CollectionExporter(cpg: Cpg, ruleCache: RuleCache) {

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
    val collectionLocalMapById     = mutable.HashMap[String, ListBuffer[Local]]()
    val collectionLiteralMapById   = mutable.HashMap[String, ListBuffer[Literal]]()

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.parameter
        .or(_.tag.nameExact(Constants.id), _.tag.name(Constants.privadoDerived + ".*"))
        .foreach(parameter => {
          try {
            parameter.tag
              .nameExact(Constants.id)
              .value
              .filter(!_.startsWith(Constants.privadoDerived))
              .foreach(x => addToMap(x, collectionParameterMapById, parameter))
            parameter.tag
              .name(Constants.privadoDerived + ".*")
              .value
              .foreach(x => addToMap(x, collectionParameterMapById, parameter))

          } catch {
            case e: Exception => logger.debug("Exception : ", e)
          }
        })
    })

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.local
        .and(_.tag.nameExact(Constants.id))
        .foreach(localVar => {
          try {
            localVar.tag
              .nameExact(Constants.id)
              .value
              .filter(!_.startsWith(Constants.privadoDerived))
              .foreach(x => addToMap(x, collectionLocalMapById, localVar))

          } catch {
            case e: Exception => logger.debug("Exception : ", e)
          }
        })
    })

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.literal
        .and(_.tag.nameExact(Constants.id))
        .foreach(literal => {
          try {
            literal.tag
              .nameExact(Constants.id)
              .value
              .filter(!_.startsWith(Constants.privadoDerived))
              .foreach(x => addToMap(x, collectionLiteralMapById, literal))

          } catch {
            case e: Exception => logger.debug("Exception : ", e)
          }
        })
    })

    def addToMap[T](literalId: String, mapper: mutable.HashMap[String, ListBuffer[T]], node: T): Unit = {
      if (!mapper.contains(literalId))
        mapper(literalId) = ListBuffer()
      mapper(literalId).append(node)
    }

    val ruleInfo = ExporterUtility.getRuleInfoForExporting(ruleCache, collectionId)
    CollectionModel(
      collectionId,
      ruleInfo.name,
      ruleInfo.isSensitive,
      collectionParameterMapById
        .map(entrySet => processByParameterId(entrySet._1, entrySet._2.toList))
        .toList ::: collectionLocalMapById
        .map(entrySet => processByLocalVariableId(entrySet._1, entrySet._2.toList))
        .toList ::: collectionLiteralMapById
        .map(entrySet => processByLiteralId(entrySet._1, entrySet._2.toList))
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
              getCollectionOccurrenceModel(Traversal(methodParameter).method, pathElement)

            case None => None
          }
        })
    )
  }

  def getCollectionOccurrenceModel(
    methodNode: Traversal[Method],
    pathElement: DataFlowSubCategoryPathExcerptModel
  ): Some[CollectionOccurrenceModel] = {
    Some(
      CollectionOccurrenceModel(
        getCollectionUrl(methodNode),
        pathElement.sample,
        pathElement.lineNumber,
        pathElement.columnNumber,
        pathElement.fileName,
        pathElement.excerpt
      )
    )

  }

  def processByLocalVariableId(
    localVariableId: String,
    methodLocalOccurrences: List[Local]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      localVariableId,
      methodLocalOccurrences
        .flatMap(localVar => {
          ExporterUtility.convertIndividualPathElement(localVar) match {
            case Some(pathElement) => getCollectionOccurrenceModel(Traversal(localVar).method, pathElement)
            case None              => None
          }
        })
    )
  }

  def processByLiteralId(
    localVariableId: String,
    methodLiteralOccurrences: List[Literal]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      localVariableId,
      methodLiteralOccurrences
        .flatMap(literal => {
          ExporterUtility.convertIndividualPathElement(literal) match {
            case Some(pathElement) => getCollectionOccurrenceModel(Traversal(literal).method, pathElement)
            case None              => None
          }
        })
    )
  }

  /** Returns rest Url for this methodNode which is already tagged under COLLECTION_METHOD_ENDPOINT
    * @param methodNode
    *   \- Traversal[Method
    * @return
    *   String
    */
  private def getCollectionUrl(methodNode: Traversal[Method]) = {
    Try(methodNode.tag.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).value.head) match {
      case Success(url) => url
      case Failure(e) =>
        logger.debug("Exception : ", e)
        ""
    }
  }

}
