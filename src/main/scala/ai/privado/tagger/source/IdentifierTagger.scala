package ai.privado.tagger.source

import ai.privado.model.{InternalTags, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.utility.Utilities._

class IdentifierTagger(cpg: Cpg, ruleInfo: RuleInfo) extends SimpleCpgPass(cpg){
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    //Step 1.1
    val regexMatchingIdentifiers = cpg.identifier(ruleInfo.pattern).l
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTags.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, NodeType.SOURCE.toString)
    })


    //Step 2.1 --> contains step 2.2, 2.3
    regexMatchingIdentifiers.name.dedup.foreach(identifier => tagObjectOfTypeDeclHavingMemberName(builder, identifier, ruleInfo))

  }

  /*
   Tag identifier of all the typeDeclaration who have a member as memberName in argument
   Represent Step 2.1
   */
  private def tagObjectOfTypeDeclHavingMemberName(builder: BatchedUpdate.DiffGraphBuilder, memberName: String, ruleInfo: RuleInfo) = {
    val typeDeclHavingMemberName = cpg.typeDecl.where(_.member.name(memberName)).l
    typeDeclHavingMemberName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
          storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
          addRuleTags(builder, impactedObject, ruleInfo, NodeType.SOURCE.toString)
        })

      //To mark all the field access
      val impactedGetters = cpg.fieldAccess.where(_.argument(1).isIdentifier.typeFullName(typeDeclVal))
        .where(_.argument(2).code(memberName)).l
      impactedGetters.foreach(impactedGetter => {
        storeForTag(builder, impactedGetter)(InternalTags.SENSITIVE_FIELD_ACCESS.toString)
        addRuleTags(builder, impactedGetter, ruleInfo, NodeType.SOURCE.toString)
      })
    })

    typeDeclHavingMemberName.fullName.dedup.l.foreach(typeDeclName => {
        //Step 2.2
        tagObjectOfTypeDeclHavingMemberType(builder, typeDeclName, ruleInfo)
        //Step 2.3
        tagObjectOfTypeDeclExtendingType(builder, typeDeclName, ruleInfo)
    })
  }

  /*
   Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument
   Represent Step 2.2
   */
  private def tagObjectOfTypeDeclHavingMemberType(builder: BatchedUpdate.DiffGraphBuilder, memberType: String, ruleInfo: RuleInfo) = {
    val typeDeclHavingMemberType = cpg.typeDecl.where(_.member.typeFullName(memberType))
    typeDeclHavingMemberType.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
        storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString)
        addRuleTags(builder, impactedObject, ruleInfo, NodeType.SOURCE.toString)
      })
    })
  }

  /*
   Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument
   Represent Step 2.3
   */
  private def tagObjectOfTypeDeclExtendingType(builder: BatchedUpdate.DiffGraphBuilder, typeDeclName: String, ruleInfo: RuleInfo) = {
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName))

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => {
        storeForTag(builder, impactedObject)(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)
        addRuleTags(builder, impactedObject, ruleInfo, NodeType.SOURCE.toString)
      })
    })
  }
}


