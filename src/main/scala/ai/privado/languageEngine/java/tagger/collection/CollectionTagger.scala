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

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.collections.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val methodUrlMap = mutable.HashMap[Long, String]()
    val classUrlMap  = mutable.HashMap[Long, String]()
    def getFinalEndPoint(collectionPoint: Method): String = {

      val methodUrl = methodUrlMap.getOrElse(collectionPoint.id(), "")
      Try(classUrlMap.getOrElse(collectionPoint.typeDecl.head.id(), "")) match {
        case Success(classUrl) => classUrl + methodUrl
        case Failure(e) =>
          logger.debug("Exception : ", e)
          methodUrl
      }
    }

    // A cached method so that we are not computing again
    val combinedRulePatterns = ruleInfo.combinedRulePattern
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .foreach(classAnnotation => {
        classUrlMap.addOne(classAnnotation.typeDecl.head.id() -> getCollectionUrl(classAnnotation))
      })
    val collectionMethodsCache = cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.method.nonEmpty)
      .map(matchedAnnotation => {
        methodUrlMap.addOne(matchedAnnotation.method.head.id() -> getCollectionUrl(matchedAnnotation))
        matchedAnnotation
      })
      .method
      .l

    val collectionPoints = Traversal(collectionMethodsCache).flatMap(collectionMethod => {
      sourceRuleInfos.flatMap(sourceRule => {
        val parameters =
          collectionMethod.parameter.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("this")).l
        if (parameters.isEmpty) {
          None
        } else {
          parameters.foreach(parameter => storeForTag(builder, parameter)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        }
      })
    })

    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, ruleInfo)
      storeForTag(builder, collectionPoint)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint)
      )
    })

    // Implementation to also mark the collection points which use derived type declaration as there parameters
    val derivedTypeDecl = (getAllDerivedTypeDecl(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString) ++
      getAllDerivedTypeDecl(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString) ++
      getAllDerivedTypeDecl(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)).distinct

    val collectionPointsFromDerivedTypeDecl = Traversal(collectionMethodsCache).flatMap(collectionMethod => {
      val parameters =
        collectionMethod.parameter.where(_.typeFullName.filter(fullName => derivedTypeDecl.contains(fullName)))
      if (parameters.isEmpty) {
        None
      } else {
        parameters.foreach(parameter => {
          val derivedReferencingIdentifier = parameter.referencingIdentifiers
            .whereNot(_.code("this"))
            .where(_.tag.name(Constants.privadoDerived + ".*"))
            .l
          if (derivedReferencingIdentifier.nonEmpty) {
            Try(derivedReferencingIdentifier.head.tag.name(Constants.privadoDerived + ".*")) match {
              case Success(refIdentifierTags) =>
                refIdentifierTags.foreach(refTag => storeForTag(builder, parameter)(refTag.name, refTag.value))
              case Failure(e) => logger.debug("Exception when reading referencing identifier information : ", e)
            }
          }
        })
        collectionMethod
      }
    })

    collectionPointsFromDerivedTypeDecl.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, ruleInfo)
      storeForTag(builder, collectionPoint)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint)
      )
    })
  }

  private def getAllDerivedTypeDecl(objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
  }

  /** Returns rest Url for this annotation
    * @param parameterIn
    * @return
    */
  private def getCollectionUrl(annotation: Annotation) = {
    Try(annotation.parameterAssign.order(1).astChildren.order(2).l.head) match {
      case Success(url) => url.code
      case Failure(_) =>
        Try(annotation.parameterAssign.order(1).head) match {
          case Success(url) => url.code
          case Failure(_) =>
            Try(annotation) match {
              case Success(url) => url.code
              case Failure(e) =>
                logger.debug("Exception : ", e)
                ""
            }
        }
    }
  }
}
