package ai.privado.tagger.collection

import ai.privado.model.{Constants, InternalTags, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._
import overflowdb.traversal.Traversal

class CollectionTagger(cpg: Cpg, collectionRuleInfo: RuleInfo, sourceRuleInfos: List[RuleInfo])
    extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // A cached method so that we are not computing again
    val collectionMethodsCache = cpg.annotation.name(collectionRuleInfo.patterns.head).method.l

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
      addRuleTags(builder, collectionPoint, collectionRuleInfo)
    })

    // Implementation to also mark the collection points which use derived type declaration as there parameters
    val derivedTypeDecl = (getAllDerivedTypeDecl(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString) ++
      getAllDerivedTypeDecl(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString) ++
      getAllDerivedTypeDecl(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)).distinct

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
      addRuleTags(builder, collectionPoint, collectionRuleInfo)
    })

  }

  private def getAllDerivedTypeDecl(objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
  }
}
