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

package ai.privado.languageEngine.java.tagger.source

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.java.tagger.source.Utility.*
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.{PrivadoParallelCpgPass, PrivadoSimpleCpgPass}
import ai.privado.utility.Utilities.*
import ai.privado.tagger.utility.SourceTaggerUtility.{getFilteredSourcesByTaggingDisabled, getMembersWithAdditionalDEDTags}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Identifier, Member, MethodParameterIn, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import org.apache.commons.lang3.builder.DiffBuilder
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.UUID
import scala.collection.concurrent.TrieMap

object SourceTagger {

  def runTagger(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache) = {

    val nodeCache = CPGNodeCacheForSourceTagger(cpg, ruleCache)
    new DirectNodeSourceTagger(cpg, nodeCache, ruleCache).createAndApply()
    new FirstLevelDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new FirstLevelDerivedSourceExternalClassTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new OCDDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new ExtendingDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
  }
}

class DirectNodeSourceTagger(cpg: Cpg, cpgNodeCache: CPGNodeCacheForSourceTagger, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  implicit val resolver: ICallResolver = NoResolve

  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    // Step 1.1
    val rulePattern              = ruleInfo.combinedRulePattern
    val regexMatchingIdentifiers = cpgNodeCache.cachedIdentifier.name(rulePattern)
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })

    val regexMatchingMembers = cpgNodeCache.cachedMember.name(rulePattern).l
    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member, ruleCache)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo, ruleCache)
    })

    val regexMatchingFieldIdentifiersIdentifiers =
      cpgNodeCache.cachedFieldAccess.where(_.argument(2).code(rulePattern)).l
    regexMatchingFieldIdentifiersIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })
  }
}

/** Tag all identifier and parameter which are external and have a fieldAccess of original source.
  *
  * Ex - user.firstName, If the class `User` is external, which means there will be no members in the User class, so
  * this tagger facilitates tagging such variables
  */
class FirstLevelDerivedSourceExternalClassTagger(
  cpg: Cpg,
  cpgNodeCache: CPGNodeCacheForSourceTagger,
  ruleCache: RuleCache,
  taggerCache: TaggerCache
) extends PrivadoSimpleCpgPass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val derivedSourceModels = cpgNodeCache.cachedFieldAccess
      .where(_.argument.argumentIndex(1).isIdentifier)
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
      .map(item => (item.tag.nameExact(Constants.id).value.headOption.getOrElse(""), item))
      .distinctBy(_._1)
      .map(_._2)
      .flatMap { fieldAccess =>
        val typeDeclFullName = fieldAccess.argument.argumentIndex(1).isIdentifier.typeFullName.headOption.getOrElse("")
        val isExternal       = cpg.typeDecl.fullName(typeDeclFullName).isExternal.nonEmpty
        if (isExternal) {
          val ruleId = fieldAccess.tag.nameExact(Constants.id).value.headOption.getOrElse("")
          val fieldIdentifierName =
            fieldAccess.start.collectAll[FieldAccess].fieldIdentifier.canonicalName.headOption.getOrElse("")
          Some(DerivedSourceModel(typeDeclFullName, ruleId, fieldIdentifierName))
        } else None
      }
      .l

    derivedSourceModels.foreach(deriveSource => {
      (cpgNodeCache.cachedIdentifier
        .where(_.typeFullName(deriveSource.typeDeclFullName))
        .l ::: cpgNodeCache.cachedParameter.where(_.typeFullName(deriveSource.typeDeclFullName)).l)
        .foreach(impactedObject => {
          addFirstLevelDerivedSourceTags(
            builder,
            impactedObject,
            ruleCache,
            cpgNodeCache,
            deriveSource.ruleId,
            deriveSource.memberName
          )
        })
    })
  }
}

/** Step 2.1 =>
  *
  * Tag identifier of all the typeDeclaration who have a member as memberName in argument Represent Step 2.1
  */
class FirstLevelDerivedSourceTagger(
  cpg: Cpg,
  cpgNodeCache: CPGNodeCacheForSourceTagger,
  ruleCache: RuleCache,
  taggerCache: TaggerCache
) extends PrivadoParallelCpgPass[DerivedSourceTaggerPart](cpg) {
  override def generateParts(): Array[DerivedSourceTaggerPart] = cpgNodeCache.derivedSourceTaggerParts

  override def runOnPart(builder: DiffGraphBuilder, taggerPart: DerivedSourceTaggerPart): Unit = {
    val typeDeclMember = taggerPart.member
    val typeDeclVal    = taggerPart.typeDecl.fullName
    // updating cache
    taggerCache.addItemToTypeDeclMemberCache(typeDeclVal, taggerPart.ruleInfo.id, typeDeclMember)
    val typeDeclMemberName = typeDeclMember.name
    // Have started tagging Parameters as well, as in collection points sometimes there is no referencing Identifier present for a local

    (cpgNodeCache.cachedIdentifier
      .where(_.typeFullName(typeDeclVal))
      .l ::: cpgNodeCache.cachedParameter.where(_.typeFullName(typeDeclVal)).l)
      .foreach(impactedObject => {
        addFirstLevelDerivedSourceTags(
          builder,
          impactedObject,
          ruleCache,
          cpgNodeCache,
          taggerPart.ruleInfo.id,
          typeDeclMemberName
        )
      })

    // To Mark all field Access and getters
    cpgNodeCache.tagAllFieldAccessAndGetters(builder, typeDeclVal, taggerPart.ruleInfo, typeDeclMemberName, true)
  }
}

/** Step 2.2 => Object Containment and Delegation =>
  *
  * Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument Represent Step 2.2
  */
class OCDDerivedSourceTagger(
  cpg: Cpg,
  cpgNodeCache: CPGNodeCacheForSourceTagger,
  ruleCache: RuleCache,
  taggerCache: TaggerCache
) extends PrivadoParallelCpgPass[DerivedSourceTaggerPart](cpg) {
  override def generateParts(): Array[DerivedSourceTaggerPart] =
    cpgNodeCache.secondLevelDerivedSourceTaggerParts
      .flatMap(memberTypeDecl =>
        cpgNodeCache.cachedMember
          .typeFullName(memberTypeDecl.typeDecl.fullName)
          .map(member => DerivedSourceTaggerPart(member.typeDecl, member, memberTypeDecl.ruleInfo))
          .dedup
      )
      .toArray

  override def runOnPart(builder: DiffGraphBuilder, taggerPart: DerivedSourceTaggerPart): Unit = {
    val typeDeclVal    = taggerPart.typeDecl.fullName
    val typeDeclMember = taggerPart.member
    taggerCache.addItemToTypeDeclMemberCache(typeDeclVal, taggerPart.ruleInfo.id, typeDeclMember)

    (cpgNodeCache.cachedIdentifier
      .where(_.typeFullName(typeDeclVal))
      .l ::: cpgNodeCache.cachedParameter.where(_.typeFullName(typeDeclVal)).l).foreach(impactedObject => {
      storeForTag(builder, impactedObject, ruleCache)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString)
      storeForTag(builder, impactedObject, ruleCache)(
        Constants.id,
        Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE
      )
      storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
      storeForTag(builder, impactedObject, ruleCache)(
        Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE,
        taggerPart.ruleInfo.id
      )
    })

    // To Mark all field Access and getters
    cpgNodeCache.tagAllFieldAccessAndGetters(builder, typeDeclVal, taggerPart.ruleInfo, typeDeclMember.name, true)
  }
}

/** Step 2.3 =>
  *
  * Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument Represent Step 2.3
  */
class ExtendingDerivedSourceTagger(
  cpg: Cpg,
  cpgNodeCache: CPGNodeCacheForSourceTagger,
  ruleCache: RuleCache,
  taggerCache: TaggerCache
) extends PrivadoParallelCpgPass[ExtendingDerivedSourceTaggerPart](cpg) {
  override def generateParts(): Array[ExtendingDerivedSourceTaggerPart] =
    cpgNodeCache.secondLevelDerivedSourceTaggerParts
      .flatMap(memberTypeDecl =>
        cpg.typeDecl
          .filter(_.inheritsFromTypeFullName.contains(memberTypeDecl.typeDecl.fullName))
          .dedup
          .map(typeDecl => ExtendingDerivedSourceTaggerPart(typeDecl, memberTypeDecl))
      )
      .toArray
  override def runOnPart(builder: DiffGraphBuilder, taggerPart: ExtendingDerivedSourceTaggerPart): Unit = {
    taggerCache.typeDeclDerivedByExtendsCache.put(taggerPart.typeDecl.fullName, taggerPart.typeDecl)

    taggerCache
      .typeDeclMemberCache(taggerPart.memberType.typeDecl.fullName)
      .filter(_._1.equals(taggerPart.memberType.ruleInfo.id))
      .foreach(entrySet => {
        val sourceRuleId = entrySet._1
        entrySet._2.foreach(taggerCache.addItemToTypeDeclMemberCache(taggerPart.typeDecl.fullName, sourceRuleId, _))
      })

    val membersOption = taggerCache.typeDeclMemberCache
      .get(taggerPart.typeDecl.fullName)
      .flatMap(_.get(taggerPart.memberType.ruleInfo.id))
    // To Mark all field Access and getters
    cpgNodeCache.tagAllFieldAccessAndGetters(
      builder,
      taggerPart.typeDecl.fullName,
      taggerPart.memberType.ruleInfo,
      membersOption.map(_.name).mkString("|"),
      true
    )
    val typeDeclVal = taggerPart.typeDecl.fullName

    if (!taggerCache.typeDeclExtendingTypeDeclCache.contains(typeDeclVal))
      taggerCache.typeDeclExtendingTypeDeclCache.put(typeDeclVal, TrieMap[String, TypeDecl]())
    taggerCache
      .getTypeDeclExtendingTypeDeclCacheItem(typeDeclVal)
      .put(taggerPart.memberType.ruleInfo.id, taggerPart.memberType.typeDecl)

    (cpgNodeCache.cachedIdentifier.where(_.typeFullName(typeDeclVal)).l ::: cpgNodeCache.cachedParameter
      .where(_.typeFullName(typeDeclVal))
      .l).foreach(impactedObject => {
      storeForTag(builder, impactedObject, ruleCache)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)
      storeForTag(builder, impactedObject, ruleCache)(
        Constants.id,
        Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE
      )
      storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
      storeForTag(builder, impactedObject, ruleCache)(
        Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
        taggerPart.memberType.ruleInfo.id
      )

      // Tag for storing memberName in derived Objects -> patient (patient extends user) --> (email, password)
      // Get the option of the set of members
      val membersOption = taggerCache.typeDeclMemberCache
        .get(typeDeclVal)
        .flatMap(_.get(taggerPart.memberType.ruleInfo.id))

      // Access the name property if it's present
      membersOption.foreach { members =>
        members.foreach { member =>
          storeForTag(builder, impactedObject, ruleCache)(
            taggerPart.memberType.ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
            member.name // Assuming "name" is a property of the Member class
          )
        }
      }
    })
  }
}

case class ExtendingDerivedSourceTaggerPart(typeDecl: TypeDecl, memberType: SecondLevelDerivedSourceTaggerPart)
case class SecondLevelDerivedSourceTaggerPart(typeDecl: TypeDecl, ruleInfo: RuleInfo)
case class DerivedSourceTaggerPart(typeDecl: TypeDecl, member: Member, ruleInfo: RuleInfo)
case class CPGNodeCacheForSourceTagger(cpg: Cpg, ruleCache: RuleCache) {
  val cachedIdentifier: List[Identifier] = cpg.identifier
    .filterNot(item => item.name.equals(item.name.toUpperCase))
    .filterNot(_.code.equals(Constants.thisConstant))
    .l
  val cachedParameter: List[MethodParameterIn] = cpg.parameter
    .filterNot(item => item.name.equals(item.name.toUpperCase))
    .filterNot(_.code.equals(Constants.thisConstant))
    .l
  // Filter Members & FieldAccess based on DISABLED_BY_DED_TAGGING
  val cachedMember = getFilteredSourcesByTaggingDisabled(
    cpg.member.filterNot(item => item.name.equals(item.name.toUpperCase)).l
  ).asInstanceOf[List[Member]]
  val cachedFieldAccess = getFilteredSourcesByTaggingDisabled(
    cpg.method
      .fullNameExact(Operators.fieldAccess, Operators.indirectFieldAccess)
      .callIn
      .dedup
      .l
  ).asInstanceOf[List[Call]]
  val derivedSourceTaggerParts = ruleCache.getRule.sources
    .flatMap(rule => {
      getMembersWithAdditionalDEDTags(cachedMember, rule)
        .map(memberNode => DerivedSourceTaggerPart(memberNode.typeDecl, memberNode, rule))
        .l
    })
    .toArray
  val secondLevelDerivedSourceTaggerParts = ruleCache.getRule.sources
    .flatMap(rule => {
      getMembersWithAdditionalDEDTags(cachedMember, rule)
        .name(rule.combinedRulePattern)
        .map(_.typeDecl)
        .distinctBy(_.fullName)
        .map(memberTypeDecl => SecondLevelDerivedSourceTaggerPart(memberTypeDecl, rule))
        .l
    })
  val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString
  val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE: String = UUID.randomUUID.toString
  val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE: String     = UUID.randomUUID.toString

  /** Function to tag all the field access operations and all the methods whose return code matches the member regex
    *
    * @param builder
    * @param typeDeclVal
    * @param memberNameRegex
    * @param ruleInfo
    * @param typeDeclMemberName
    */
  def tagAllFieldAccessAndGetters(
    builder: BatchedUpdate.DiffGraphBuilder,
    typeDeclVal: String,
    ruleInfo: RuleInfo,
    typeDeclMemberName: String,
    isDerived: Boolean = false
  ): Unit = {

    getFieldAccessCallsMatchingRegex(cpg, typeDeclVal, s"($typeDeclMemberName)", Option(cachedFieldAccess)).foreach(
      impactedGetter => {
        storeForTag(builder, impactedGetter, ruleCache)(InternalTag.SENSITIVE_FIELD_ACCESS.toString)
        addRuleTags(builder, impactedGetter, ruleInfo, ruleCache)
        if (isDerived)
          storeForTag(builder, impactedGetter, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
      }
    )

    val impactedReturnMethods = getCallsMatchingReturnRegex(cpg, typeDeclVal, s"($typeDeclMemberName)")
    impactedReturnMethods
      .foreach(storeForTag(builder, _, ruleCache)(InternalTag.SENSITIVE_METHOD_RETURN.toString, ruleInfo.id))

  }
}
case class DerivedSourceModel(typeDeclFullName: String, ruleId: String, memberName: String)

def addFirstLevelDerivedSourceTags(
  builder: DiffGraphBuilder,
  impactedObject: AstNode,
  ruleCache: RuleCache,
  cpgNodeCache: CPGNodeCacheForSourceTagger,
  ruleId: String,
  memberName: String
) = {

  storeForTag(builder, impactedObject, ruleCache)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString, ruleId)
  storeForTag(builder, impactedObject, ruleCache)(
    Constants.id,
    Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
  )
  storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
  storeForTag(builder, impactedObject, ruleCache)(
    Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
    ruleId
  )
  // Tag for storing memberName in derived Objects -> user --> (email, password)
  storeForTag(builder, impactedObject, ruleCache)(
    ruleId + Constants.underScore + Constants.privadoDerived + Constants.underScore + cpgNodeCache.RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
    memberName
  )
}
