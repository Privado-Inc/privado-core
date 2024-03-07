package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import scala.util.{Failure, Success, Try}
import scala.collection.mutable

object CollectionUtility {

  private val logger = LoggerFactory.getLogger(this.getClass)
  def tagDirectSources(
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    sourceRuleInfos: List[RuleInfo],
    collectionRuleInfo: RuleInfo,
    ruleCache: RuleCache,
    returnByName: Boolean = false,
    methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String](),
    classUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  ) = {
    val collectionPoints = collectionMethods.flatMap(collectionMethod => {
      sourceRuleInfos.flatMap(sourceRule => {
        val parameters =
          collectionMethod.parameter.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("this")).l
        if (parameters.isEmpty) {
          None
        } else {
          parameters.foreach(parameter => storeForTag(builder, parameter, ruleCache)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        }
      })
    })
    tagMethodEndpoints(
      builder,
      collectionPoints.l,
      collectionRuleInfo,
      ruleCache,
      returnByName,
      methodUrlMap,
      classUrlMap
    )
  }

  def tagDerivedSources(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    collectionRuleInfo: RuleInfo,
    ruleCache: RuleCache,
    returnByName: Boolean = false,
    methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String](),
    classUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  ) = {
    // Implementation to also mark the collection points which use derived type declaration as there parameters
    val derivedTypeDecl = (getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)).toSet

    val collectionPointsFromDerivedTypeDecl = collectionMethods.flatMap(collectionMethod => {
      val parameters =
        collectionMethod.parameter.where(_.typeFullName.filter(fullName => derivedTypeDecl.contains(fullName)))
      if (parameters.isEmpty) {
        None
      } else {
        // Have removed the earlier code, where we were fetching all the referencing identifiers of parameter and then tagging, because we were missing on cases where the parameter is not used in the code
        parameters
          .whereNot(_.code("this"))
          .foreach(parameter => {
            parameter.tag
              .name(Constants.privadoDerived + ".*")
              .foreach(refTag => storeForTag(builder, parameter, ruleCache)(refTag.name, refTag.value))
          })
        collectionMethod
      }
    })

    tagMethodEndpoints(
      builder,
      collectionPointsFromDerivedTypeDecl.l,
      collectionRuleInfo,
      ruleCache,
      returnByName,
      methodUrlMap,
      classUrlMap
    )
  }

  private def tagMethodEndpoints(
    builder: DiffGraphBuilder,
    collectionPoints: List[Method],
    collectionRuleInfo: RuleInfo,
    ruleCache: RuleCache,
    returnByName: Boolean,
    methodUrlMap: mutable.HashMap[Long, String],
    classUrlMap: mutable.HashMap[Long, String]
  ) = {
    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, collectionRuleInfo, ruleCache)
      storeForTag(builder, collectionPoint, ruleCache)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint, returnByName, methodUrlMap, classUrlMap)
      )
    })
  }

  private def getAllDerivedTypeDecl(cpg: Cpg, objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
  }

  def getCollectionUrls(
    cpg: Cpg,
    methodUrlMap: mutable.HashMap[Long, String],
    classUrlMap: mutable.HashMap[Long, String]
  ): List[String] = {
    var combinedMethodUrls = List[String]()
    for (methodId <- methodUrlMap.keys.l) {

      val methodUrl = methodUrlMap.get(methodId).get
      // If collection method is defined under a class then append classUrl before methodUrl
      val completeUrl = Try(classUrlMap.getOrElse(cpg.method.id(methodId).head.typeDecl.head.id(), "")) match {
        case Success(classUrl) => classUrl + methodUrl
        case Failure(e)        => methodUrl
      }
      combinedMethodUrls = combinedMethodUrls :+ completeUrl
    }
    combinedMethodUrls
  }

  private def getFinalEndPoint(
    collectionPoint: Method,
    returnByName: Boolean,
    methodUrlMap: mutable.HashMap[Long, String],
    classUrlMap: mutable.HashMap[Long, String]
  ): String = {
    if (returnByName) {
      collectionPoint.name
    } else {
      val methodUrl = methodUrlMap.getOrElse(collectionPoint.id(), "")
      Try(classUrlMap.getOrElse(collectionPoint.typeDecl.head.id(), "")) match {
        case Success(classUrl) => classUrl + methodUrl
        case Failure(e) =>
          methodUrl
      }
    }
  }

  /** This function gets the URL from an Annotation object. It will first look for the URL in the parameterAssign node
    * order 1, then if that fails it will look for the URL in the parameterAssign node order 2, then if that fails it
    * will look for the URL in the typeDecl node, and finally if that fails it will look for the URL in the method node.
    * If none of these succeed, it will return an empty string.
    * @param parameterIn
    * @return
    */
  def getUrlFromAnnotation(annotation: Annotation): String = {
    annotation.parameterAssign.where(_.parameter.code("value")).value.l.headOption match {
      case Some(url) => url.code
      case None      => ""
    }
  }
}
