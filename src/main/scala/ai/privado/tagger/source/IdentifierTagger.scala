package ai.privado.tagger.source

import ai.privado.model.{InternalTags, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.NewTag
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

class IdentifierTagger(cpg: Cpg, rule: RuleInfo) extends SimpleCpgPass(cpg){
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    //Step 1.1
    val regexMatchingIdentifiers = cpg.identifier(rule.pattern).l
    regexMatchingIdentifiers.foreach(identifier => builder.addEdge(identifier,
      NewTag().name(InternalTags.VARIABLE_REGEX_IDENTIFIER.toString), EdgeTypes.TAGGED_BY))

    //Step 2.1 --> contains step 2.2, 2.3
    regexMatchingIdentifiers.name.dedup.foreach(identifier => tagObjectOfTypeDeclHavingMemberName(builder, identifier))

  }

  /*
   Tag identifier of all the typeDeclaration who have a member as memberName in argument
   Represent Step 2.1
   */
  private def tagObjectOfTypeDeclHavingMemberName(builder: BatchedUpdate.DiffGraphBuilder, memberName: String) = {
    val typeDeclHavingMemberName = cpg.typeDecl.where(_.member.name(memberName)).l
    typeDeclHavingMemberName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => builder.addEdge(impactedObject,
        NewTag().name(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString), EdgeTypes.TAGGED_BY))
    })

    typeDeclHavingMemberName.fullName.dedup.l.foreach(typeDeclName => {
        //Step 2.2
        tagObjectOfTypeDeclHavingMemberType(builder, typeDeclName)
        //Step 2.3
        tagObjectOfTypeDeclExtendingType(builder, typeDeclName)
    })
  }

  /*
   Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument
   Represent Step 2.2
   */
  private def tagObjectOfTypeDeclHavingMemberType(builder: BatchedUpdate.DiffGraphBuilder, memberType: String) = {
    val typeDeclHavingMemberType = cpg.typeDecl.where(_.member.typeFullName(memberType))
    typeDeclHavingMemberType.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => builder.addEdge(impactedObject,
        NewTag().name(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString), EdgeTypes.TAGGED_BY))
    })
  }

  /*
   Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument
   Represent Step 2.3
   */
  private def tagObjectOfTypeDeclExtendingType(builder: BatchedUpdate.DiffGraphBuilder, typeDeclName: String) = {
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName))

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
      impactedObjects.foreach(impactedObject => builder.addEdge(impactedObject,
        NewTag().name(InternalTags.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString), EdgeTypes.TAGGED_BY))
    })
  }
}


