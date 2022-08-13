package ai.privado.tagger.collection

import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends PrivadoSimplePass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

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

    cpg.annotation
      .name(ruleInfo.patterns.head)
      .filter(_.typeDecl.nonEmpty)
      .foreach(classAnnotation => {
        classUrlMap.addOne(classAnnotation.typeDecl.head.id() -> getCollectionUrl(classAnnotation))
      })
    val collectionMethodsCache = cpg.annotation
      .name(ruleInfo.patterns.head)
      .filter(_.method.nonEmpty)
      .map(matchedAnnotation => {
        methodUrlMap.addOne(matchedAnnotation.method.head.id() -> getCollectionUrl(matchedAnnotation))
        matchedAnnotation
      })
      .method
      .l

    /*
    cpg.annotation.name(ruleInfo.patterns.head).filter(_.method.nonEmpty).foreach(matchedAnnotation => {
      //println(matchedAnnotation.code)
      methodUrlMap.addOne(matchedAnnotation.method.head.id() -> getCollectionUrl(matchedAnnotation))
    })

    val collectionMethodsCache = cpg.annotation.name(ruleInfo.patterns.head).method.l

     */

    val collectionPoints = Traversal(collectionMethodsCache).flatMap(collectionMethod => {
      sourceRuleInfos.flatMap(sourceRule => {
        val parameters = collectionMethod.parameter.where(_.name(sourceRule.patterns.head)).l
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
          val refIdentifierTags = Try(
            parameter.referencingIdentifiers
              .where(_.tag.name(Constants.privadoDerived + ".*"))
              .head
              .tag
              .name(Constants.privadoDerived + ".*")
          )
          refIdentifierTags match {
            case Success(refIdentifierTags) =>
              refIdentifierTags.foreach(refTag => storeForTag(builder, parameter)(refTag.name, refTag.value))
            case Failure(e) => logger.debug("Exception : ", e)
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
      case Failure(e) =>
        Try(annotation.parameterAssign.order(1).head) match {
          case Success(url) => url.code
          case Failure(e) =>
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
