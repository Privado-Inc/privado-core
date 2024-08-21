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

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.exporter.{
  CollectionModel,
  CollectionOccurrenceDetailModel,
  CollectionOccurrenceModel,
  DataFlowSubCategoryPathExcerptModel
}
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*
import ai.privado.semantic.language.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class CollectionExporter(cpg: Cpg, ruleCache: RuleCache, repoItemTagName: Option[String] = None, appCache: AppCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Processes collection points and return final output
    */
  def getCollections: List[CollectionModel] = {
    getCollectionsByMethods ++ getCollectionsByTemplateDom ++ getCollectionsByAndroidXmlFieldIds ++ getCollectionsByCalls ++ getDummyCollection
  }

  /** Get dummy collection in case of monolith subProject/repoItem for showing PA
    * @return
    */
  private def getDummyCollection: List[CollectionModel] = {
    if (repoItemTagName.isDefined) {
      val fileName = cpg.file
        .where(_.tag.nameExact(Constants.monolithRepoItem).valueExact(repoItemTagName.get))
        .name
        .headOption
        .getOrElse(Constants.default)
      List(
        CollectionModel(
          Constants.default,
          Constants.default,
          false,
          List(
            CollectionOccurrenceDetailModel(
              Constants.default,
              List(
                CollectionOccurrenceModel(
                  Constants.default,
                  Constants.default,
                  Constants.defaultLineNumber,
                  Constants.defaultLineNumber,
                  fileName,
                  Constants.default
                )
              )
            )
          )
        )
      )
    } else List()
  }
  private def getCollectionsByTemplateDom: List[CollectionModel] = {
    val collectionMapByCollectionId = ExporterUtility
      .filterNodeBasedOnRepoItemTagName(
        cpg.templateDom
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .l,
        repoItemTagName
      )
      .groupBy(collectionTemplateDom => collectionTemplateDom.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId
      .map(entrySet => processByCollectionIdForTemplateDom(entrySet._1, entrySet._2.isTemplateDom.l))
      .toList
  }

  private def getCollectionsByAndroidXmlFieldIds: List[CollectionModel] = {
    val collectionMapByCollectionId = ExporterUtility
      .filterNodeBasedOnRepoItemTagName(
        cpg.fieldAccess.astChildren.isFieldIdentifier
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .l,
        repoItemTagName
      )
      .groupBy(collectionAndroidXml => collectionAndroidXml.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId
      .map(entrySet => processByCollectionIdForAndroidXmlFieldIds(entrySet._1, entrySet._2.isFieldIdentifier.l))
      .toList
  }

  private def processByCollectionIdForAndroidXmlFieldIds(
    collectionId: String,
    collectionAndroidXmlFieldIds: List[FieldIdentifier]
  ) = {

    val collectionAndroidXmlFieldIdMapById = mutable.HashMap[String, ListBuffer[FieldIdentifier]]()

    collectionAndroidXmlFieldIds.foreach(fieldId => {
      try {
        fieldId.tag
          .nameExact(Constants.collectionSource)
          .value
          .foreach(x => addToMap(x, collectionAndroidXmlFieldIdMapById, fieldId))
      } catch {
        case e: Exception => logger.debug("Exception : ", e)
      }
    })

    def addToMap[T](literalId: String, mapper: mutable.HashMap[String, ListBuffer[T]], node: T): Unit = {
      if (!mapper.contains(literalId))
        mapper(literalId) = ListBuffer()
      mapper(literalId).append(node)
    }

    val ruleInfo       = ExporterUtility.getRuleInfoForExporting(ruleCache, collectionId)
    val linkedSourceId = Option(ruleInfo.tags("sourceId"))
    CollectionModel(
      collectionId,
      ruleInfo.name,
      ruleInfo.isSensitive,
      collectionAndroidXmlFieldIdMapById
        .map(entrySet => processByAndroidXmlFieldIds(linkedSourceId.getOrElse(""), entrySet._2.toList))
        .toList
    )
  }

  private def processByCollectionIdForTemplateDom(collectionId: String, collectionTemplateDoms: List[TemplateDom]) = {

    val collectionTemplateDomMapById = mutable.HashMap[String, ListBuffer[TemplateDom]]()
    collectionTemplateDoms.foreach(templateDom => {
      try {
        templateDom.tag
          .nameExact(Constants.collectionSource)
          .value
          .foreach(x => addToMap(x, collectionTemplateDomMapById, templateDom))
      } catch {
        case e: Exception => logger.debug("Exception : ", e)
      }
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
      collectionTemplateDomMapById
        .map(entrySet => processByTemplatedDomId(entrySet._1, entrySet._2.toList))
        .toList
    )
  }

  private def getCollectionsByCalls: List[CollectionModel] = {
    val collectionMapByCollectionId = ExporterUtility
      .filterNodeBasedOnRepoItemTagName(
        cpg.call
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .l,
        repoItemTagName
      )
      .groupBy(collectionCall => collectionCall.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId.map(entrySet => processCallByCollectionId(entrySet._1, entrySet._2.isCall.l)).toList

  }

  private def processCallByCollectionId(collectionId: String, collectionCalls: List[Call]) = {
    val ruleInfo = ExporterUtility.getRuleInfoForExporting(ruleCache, collectionId)

    val collectionDetailsModel = collectionCalls
      .flatMap(callNode => {
        Try(callNode.argument(1)) match
          case Success(arg) if arg.isInstanceOf[AstNode] =>
            val argNode = arg.asInstanceOf[AstNode]
            argNode.tag
              .nameExact(Constants.id)
              .value
              .flatMap(sourceId => {
                Some(
                  CollectionOccurrenceDetailModel(
                    sourceId,
                    ExporterUtility
                      .convertIndividualPathElement(argNode, appCache = appCache, ruleCache = ruleCache) match
                      case Some(pathElement) =>
                        List(
                          CollectionOccurrenceModel(
                            getCollectionUrl(callNode),
                            pathElement.sample,
                            pathElement.lineNumber,
                            pathElement.columnNumber,
                            pathElement.fileName,
                            pathElement.excerpt
                          )
                        )
                      case None => List()
                  )
                )
              })
          case _ => None
      })

    val groupedModel = collectionDetailsModel.groupBy(_.sourceId)
    CollectionModel(collectionId, ruleInfo.name, ruleInfo.isSensitive, groupedModel.values.flatten.toList)
  }
  private def getCollectionsByMethods: List[CollectionModel] = {
    val collectionMapByCollectionId = ExporterUtility
      .filterNodeBasedOnRepoItemTagName(
        cpg.method
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .l,
        repoItemTagName
      )
      .groupBy(collectionMethod => collectionMethod.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId.map(entrySet => processByCollectionId(entrySet._1, entrySet._2.isMethod.l)).toList
  }

  private def processByCollectionId(collectionId: String, collectionMethods: List[Method]) = {

    val collectionParameterMapById = mutable.HashMap[String, ListBuffer[MethodParameterIn]]()
    val collectionLocalMapById     = mutable.HashMap[String, ListBuffer[Local]]()
    val collectionLiteralMapById   = mutable.HashMap[String, ListBuffer[Literal]]()
    val collectionAstMapById       = mutable.HashMap[String, ListBuffer[AstNode]]()

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

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.ast
        .and(_.tag.nameExact(Constants.id))
        .foreach(astNode => {
          try {
            astNode
              .filterNot(_.isParameter)
              .filterNot(_.isLocal)
              .filter(node =>
                node.tag
                  .valueExact(CatLevelOne.SOURCES.name)
                  .nonEmpty || node.tag.valueExact(CatLevelOne.DERIVED_SOURCES.name).nonEmpty
              )
              .tag
              .nameExact(Constants.id)
              .value
              .filter(!_.startsWith(Constants.privadoDerived))
              .foreach(x => addToMap(x, collectionAstMapById, astNode))

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
      (collectionParameterMapById
        .map(entrySet => processByParameterId(entrySet._1, entrySet._2.toSet.toList))
        .toList ::: collectionLocalMapById
        .map(entrySet => processByLocalVariableId(entrySet._1, entrySet._2.toSet.toList))
        .toList ::: collectionLiteralMapById
        .map(entrySet => processByLiteralId(entrySet._1, entrySet._2.toSet.toList))
        .toList ::: collectionAstMapById
        .map(entrySet => processNode(entrySet._1, entrySet._2.toSet.toList))
        .toList).dedupBy(_.sourceId).l
    )
  }

  private def processByParameterId(
    parameterId: String,
    methodParameterOccurrences: List[MethodParameterIn]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      parameterId,
      methodParameterOccurrences
        .flatMap(methodParameter => {
          ExporterUtility
            .convertIndividualPathElement(methodParameter, appCache = appCache, ruleCache = ruleCache) match {
            case Some(pathElement) =>
              getCollectionOccurrenceModel(Iterator(methodParameter).method.head, pathElement)

            case None => None
          }
        })
    )
  }

  private def getCollectionOccurrenceModel(
    methodNode: AstNode,
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

  private def processByTemplatedDomId(
    templatedDomId: String,
    methodLocalOccurrences: List[TemplateDom]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      templatedDomId,
      methodLocalOccurrences
        .flatMap(templateDom => {
          ExporterUtility.convertIndividualPathElement(templateDom, appCache = appCache, ruleCache = ruleCache) match {
            case Some(pathElement) =>
              Some(
                CollectionOccurrenceModel(
                  templateDom.file.name.headOption.getOrElse(""),
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

  def processByAndroidXmlFieldIds(
    sourceId: String,
    fieldIdentiferOccurances: List[FieldIdentifier]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      sourceId,
      fieldIdentiferOccurances
        .flatMap(fieldId => {
          val androidXmlFileName = cpg.androidXmlLayoutNode
            .name(fieldId.canonicalName)
            .file
            .name
            .headOption
            .getOrElse("")
            .stripPrefix(cpg.metaData.root.headOption.getOrElse("") + "/")

          ExporterUtility
            .convertIndividualPathElement(
              fieldId,
              messageInExcerpt = "Android Form: " + androidXmlFileName,
              appCache = appCache,
              ruleCache = ruleCache
            ) match {
            case Some(pathElement) =>
              Some(
                CollectionOccurrenceModel(
                  fieldId.file.name.headOption.getOrElse(""),
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

  def processByLocalVariableId(
    localVariableId: String,
    methodLocalOccurrences: List[Local]
  ): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      localVariableId,
      methodLocalOccurrences
        .flatMap(localVar => {
          ExporterUtility.convertIndividualPathElement(localVar, appCache = appCache, ruleCache = ruleCache) match {
            case Some(pathElement) => getCollectionOccurrenceModel(Iterator(localVar).method.head, pathElement)
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
          ExporterUtility.convertIndividualPathElement(literal, appCache = appCache, ruleCache = ruleCache) match {
            case Some(pathElement) => getCollectionOccurrenceModel(Iterator(literal).method.head, pathElement)
            case None              => None
          }
        })
    )
  }

  def processNode(variableId: String, nodeOccurrences: List[AstNode]): CollectionOccurrenceDetailModel = {

    CollectionOccurrenceDetailModel(
      variableId,
      nodeOccurrences
        .flatMap(node => {
          ExporterUtility.convertIndividualPathElement(node, appCache = appCache, ruleCache = ruleCache) match {
            case Some(pathElement) =>
              Some(
                CollectionOccurrenceModel(
                  getCollectionUrl(node),
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

  /** Returns rest Url for this methodNode which is already tagged under COLLECTION_METHOD_ENDPOINT
    * @param methodNode
    *   \- Iterator[Method]
    * @return
    *   String
    */
  private def getCollectionUrl(node: AstNode) = {
    Try(node.tag.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).value.head) match {
      case Success(url) => url
      case Failure(e) =>
        logger.debug("Exception : ", e)
        ""
    }
  }

}
