package ai.privado.languageEngine.java.tagger.collection

import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal
import scala.util.{Failure, Success, Try}

import scala.collection.mutable

object CollectionUtility {
  def tagDirectSources(builder: DiffGraphBuilder, collectionMethods: List[Method], sourceRuleInfos: List[RuleInfo], collectionRuleInfo: RuleInfo, returnByName: Boolean = false, methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String](), classUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()) = {
    val collectionPoints = Traversal(collectionMethods).flatMap(collectionMethod => {
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
    tagMethodEndpoints(builder, collectionPoints.l, collectionRuleInfo, returnByName, methodUrlMap, classUrlMap)
  }

  def tagDerivedSources(cpg: Cpg, builder: DiffGraphBuilder, collectionMethods: List[Method], collectionRuleInfo: RuleInfo, returnByName: Boolean = false, methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String](), classUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()) = {
    // Implementation to also mark the collection points which use derived type declaration as there parameters
    val derivedTypeDecl = (getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)).distinct

    val collectionPointsFromDerivedTypeDecl = Traversal(collectionMethods).flatMap(collectionMethod => {
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
            }
          }
        })
        collectionMethod
      }
    })

    tagMethodEndpoints(builder, collectionPointsFromDerivedTypeDecl.l, collectionRuleInfo, returnByName, methodUrlMap, classUrlMap)
  }

  private def tagMethodEndpoints(builder: DiffGraphBuilder, collectionPoints: List[Method], collectionRuleInfo: RuleInfo, returnByName: Boolean, methodUrlMap: mutable.HashMap[Long, String], classUrlMap: mutable.HashMap[Long, String]) = {
    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, collectionRuleInfo)
      storeForTag(builder, collectionPoint)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint, returnByName, methodUrlMap, classUrlMap)
      )
    })
  }

  private def getAllDerivedTypeDecl(cpg: Cpg, objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
  }

  private def getFinalEndPoint(collectionPoint: Method, returnByName: Boolean, methodUrlMap: mutable.HashMap[Long, String], classUrlMap: mutable.HashMap[Long, String]): String = {
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
}
