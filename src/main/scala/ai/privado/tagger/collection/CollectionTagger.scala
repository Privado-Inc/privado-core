package ai.privado.tagger.collection

import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._
import overflowdb.traversal.Traversal

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // A cached method so that we are not computing again
    val collectionMethodsCache = cpg.annotation.name(ruleInfo.patterns.head).method.l

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
          val refIdentifierTags = parameter.referencingIdentifiers
            .where(_.tag.name(Constants.privadoDerived + ".*"))
            .head
            .tag
            .name(Constants.privadoDerived + ".*")
          refIdentifierTags.foreach(refTag => storeForTag(builder, parameter)(refTag.name, refTag.value))
        })
        collectionMethod
      }
    })

    collectionPointsFromDerivedTypeDecl.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, ruleInfo)
    })

  }

  private def getAllDerivedTypeDecl(objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
  }
}
