package ai.privado.tagger.source

import ai.privado.model.{Constants, InternalTags, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.utility.Utilities._

import java.util.UUID

class IdentifierTagger(cpg: Cpg, ruleInfo: RuleInfo) extends SimpleCpgPass(cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME = UUID.randomUUID().toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE = UUID.randomUUID().toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE     = UUID.randomUUID().toString

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // Step 1.1
    val regexMatchingIdentifiers = cpg.identifier(ruleInfo.patterns.head).l
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTags.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
    })

    // Step 2.1 --> contains step 2.2, 2.3
    regexMatchingIdentifiers.name.dedup.foreach(identifier =>
      tagObjectOfTypeDeclHavingMemberName(builder, identifier, ruleInfo)
    )

  }

  /*
   Tag identifier of all the typeDeclaration who have a member as memberName in argument
   Represent Step 2.1
   */
  private def tagObjectOfTypeDeclHavingMemberName(
    builder: BatchedUpdate.DiffGraphBuilder,
    memberName: String,
    ruleInfo: RuleInfo
  ) = {
    val typeDeclHavingMemberName = cpg.typeDecl.where(_.member.name(memberName)).l
    typeDeclHavingMemberName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
          storeForTag(builder, impactedObject)(
            Constants.id,
            InternalTags.PRIVADO_DERIVED.toString + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
          )
          storeForTag(builder, impactedObject)(Constants.nodeType, NodeType.DERIVED_SOURCE.toString)
          // addRuleTags(builder, impactedObject, ruleInfo)
        }
        storeForTag(builder, impactedObject)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
          ruleInfo.id
        )
      })

      // To mark all the field access
      val impactedGetters = cpg.fieldAccess
        .where(_.argument(1).isIdentifier.typeFullName(typeDeclVal))
        .where(_.argument(2).code(memberName))
        .l
      impactedGetters.foreach(impactedGetter => {
        if (impactedGetter.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedGetter)(InternalTags.SENSITIVE_FIELD_ACCESS.toString)
          addRuleTags(builder, impactedGetter, ruleInfo)
        }
      })
    })

    typeDeclHavingMemberName.fullName.dedup.l.foreach(typeDeclName => {
      // Step 2.2
      tagObjectOfTypeDeclHavingMemberType(builder, typeDeclName, ruleInfo)
      // Step 2.3
      tagObjectOfTypeDeclExtendingType(builder, typeDeclName, ruleInfo)
    })
  }

  /*
   Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument
   Represent Step 2.2
   */
  private def tagObjectOfTypeDeclHavingMemberType(
    builder: BatchedUpdate.DiffGraphBuilder,
    memberType: String,
    ruleInfo: RuleInfo
  ) = {
    val typeDeclHavingMemberType = cpg.typeDecl.where(_.member.typeFullName(memberType))
    typeDeclHavingMemberType.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString)
          storeForTag(builder, impactedObject)(
            Constants.id,
            InternalTags.PRIVADO_DERIVED.toString + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE
          )
          storeForTag(builder, impactedObject)(Constants.nodeType, NodeType.DERIVED_SOURCE.toString)
          // addRuleTags(builder, impactedObject, ruleInfo)
        }
        storeForTag(builder, impactedObject)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE,
          ruleInfo.id
        )
      })
    })
  }

  /*
   Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument
   Represent Step 2.3
   */
  private def tagObjectOfTypeDeclExtendingType(
    builder: BatchedUpdate.DiffGraphBuilder,
    typeDeclName: String,
    ruleInfo: RuleInfo
  ) = {
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName))

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)
          storeForTag(builder, impactedObject)(
            Constants.id,
            InternalTags.PRIVADO_DERIVED.toString + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE
          )
          storeForTag(builder, impactedObject)(Constants.nodeType, NodeType.DERIVED_SOURCE.toString)
          // addRuleTags(builder, impactedObject, ruleInfo)
        }
        storeForTag(builder, impactedObject)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
          ruleInfo.id
        )
      })
    })
  }
}
