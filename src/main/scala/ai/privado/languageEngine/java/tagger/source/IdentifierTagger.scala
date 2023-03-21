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
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.languageEngine.java.tagger.source.Utility._

import java.util.UUID
import scala.collection.mutable

class IdentifierTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {

  implicit val resolver: ICallResolver                              = NoResolve
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE: String     = UUID.randomUUID.toString

  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    // Step 1.1
    val rulePattern = ruleInfo.combinedRulePattern
    val regexMatchingIdentifiers =
      cpg.identifier(rulePattern).filterNot(item => item.name.equals(item.name.toUpperCase))
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
    })

    val regexMatchingMembers = cpg.member.name(rulePattern).l
    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo)
    })

    val regexMatchingFieldIdentifiersIdentifiers =
      cpg.fieldAccess
        .where(
          _.fieldIdentifier
            .canonicalName(rulePattern)
            .filterNot(item => item.canonicalName.equals(item.canonicalName.toUpperCase))
        )
        .isCall
        .l
    regexMatchingFieldIdentifiersIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
    })

    // Step 2.1 --> contains step 2.2, 2.3
    tagObjectOfTypeDeclHavingMemberName(builder, rulePattern, ruleInfo)

  }

  /** Tag identifier of all the typeDeclaration who have a member as memberName in argument Represent Step 2.1
    */
  private def tagObjectOfTypeDeclHavingMemberName(
    builder: BatchedUpdate.DiffGraphBuilder,
    memberNameRegex: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclWithMemberNameHavingMemberName = cpg.typeDecl
      .where(_.member.name(memberNameRegex).filterNot(item => item.name.equals(item.name.toUpperCase)))
      .map(typeDeclNode => (typeDeclNode, typeDeclNode.member.name(memberNameRegex).l))
      .l
    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        val typeDeclVal = typeDeclValEntry._1.fullName
        val typeDeclMemberName = typeDeclValEntry._2.headOption match {
          case Some(typeDeclMember) => // updating cache
            TaggerCache.addItemToTypeDeclMemberCache(typeDeclVal, ruleInfo.id, typeDeclMember)
            typeDeclMember.name
          case None =>
            "Member not found"
        }
        val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
        impactedObjects
          .whereNot(_.code("this"))
          .foreach(impactedObject => {
            if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
              storeForTag(builder, impactedObject)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
              storeForTag(builder, impactedObject)(
                Constants.id,
                Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
              )
              storeForTag(builder, impactedObject)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
            }
            storeForTag(builder, impactedObject)(
              Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
              ruleInfo.id
            )
            // Tag for storing memberName in derived Objects -> user --> (email, password)
            storeForTag(builder, impactedObject)(
              ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
              typeDeclMemberName
            )
          })

        // To Mark all field Access and getters
        tagAllFieldAccessAndGetters(builder, typeDeclVal, ruleInfo, typeDeclMemberName)
      })

    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        val typeDeclName = typeDeclValEntry._1.fullName
        // Step 2.2
        tagObjectOfTypeDeclHavingMemberType(builder, typeDeclName, ruleInfo)
        // Step 2.3
        tagObjectOfTypeDeclExtendingType(builder, typeDeclName, ruleInfo)
      })
  }

  /** Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument Represent Step 2.2
    */
  private def tagObjectOfTypeDeclHavingMemberType(
    builder: BatchedUpdate.DiffGraphBuilder,
    memberType: String,
    ruleInfo: RuleInfo
  ): Unit = {
    // stores tuple(Member, TypeDeclFullName)
    val typeDeclHavingMemberTypeTuple =
      cpg.typeDecl.member.typeFullName(memberType).map(member => (member, member.typeDecl.fullName)).dedup.l
    typeDeclHavingMemberTypeTuple.foreach(typeDeclTuple => {
      val typeDeclVal    = typeDeclTuple._2
      val typeDeclMember = typeDeclTuple._1
      TaggerCache.addItemToTypeDeclMemberCache(typeDeclVal, ruleInfo.id, typeDeclMember)
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal)).whereNot(_.code("this"))
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString)
          storeForTag(builder, impactedObject)(
            Constants.id,
            Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE
          )
          storeForTag(builder, impactedObject)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
        }
        storeForTag(builder, impactedObject)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE,
          ruleInfo.id
        )
      })

      // To Mark all field Access and getters
      tagAllFieldAccessAndGetters(builder, typeDeclVal, ruleInfo, typeDeclMember.name)
    })
  }

  /** Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument Represent Step
    * 2.3
    */
  private def tagObjectOfTypeDeclExtendingType(
    builder: BatchedUpdate.DiffGraphBuilder,
    typeDeclName: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName)).dedup.l

    typeDeclsExtendingTypeName.foreach(typeDecl => {
      TaggerCache.typeDeclDerivedByExtendsCache.addOne(typeDecl.fullName, typeDecl)

      TaggerCache
        .typeDeclMemberCache(typeDeclName)
        .filter(_._1.equals(ruleInfo.id))
        .foreach(entrySet => {
          val sourceRuleId = entrySet._1
          entrySet._2.foreach(TaggerCache.addItemToTypeDeclMemberCache(typeDecl.fullName, sourceRuleId, _))
        })
      // To Mark all field Access and getters
      tagAllFieldAccessAndGetters(
        builder,
        typeDecl.fullName,
        ruleInfo,
        TaggerCache.typeDeclMemberCache(typeDecl.fullName)(ruleInfo.id).name.mkString("|")
      )

    })

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {

      if (!TaggerCache.typeDeclExtendingTypeDeclCache.contains(typeDeclVal))
        TaggerCache.typeDeclExtendingTypeDeclCache.addOne(typeDeclVal -> mutable.HashMap[String, TypeDecl]())
      TaggerCache
        .typeDeclExtendingTypeDeclCache(typeDeclVal)
        .addOne(ruleInfo.id -> cpg.typeDecl.where(_.fullNameExact(typeDeclName)).head)

      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal)).whereNot(_.code("this"))
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)
          storeForTag(builder, impactedObject)(
            Constants.id,
            Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE
          )
          storeForTag(builder, impactedObject)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
        }
        storeForTag(builder, impactedObject)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
          ruleInfo.id
        )
        // Tag for storing memberName in derived Objects -> patient (patient extends user) --> (email, password)
        TaggerCache
          .typeDeclMemberCache(typeDeclName)(ruleInfo.id)
          .name
          .foreach(memberName =>
            storeForTag(builder, impactedObject)(
              ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
              memberName
            )
          )
      })
    })
  }

  /** Function to tag all the field access operations and all the methods whose return code matches the member regex
    * @param builder
    * @param typeDeclVal
    * @param memberNameRegex
    * @param ruleInfo
    * @param typeDeclMemberName
    */
  private def tagAllFieldAccessAndGetters(
    builder: BatchedUpdate.DiffGraphBuilder,
    typeDeclVal: String,
    ruleInfo: RuleInfo,
    typeDeclMemberName: String
  ): Unit = {
    val impactedGetters = getFieldAccessCallsMatchingRegex(cpg, typeDeclVal, s"($typeDeclMemberName)")
      .filterNot(item => item.code.equals(item.code.toUpperCase))

    impactedGetters.foreach(impactedGetter => {
      storeForTag(builder, impactedGetter)(InternalTag.SENSITIVE_FIELD_ACCESS.toString)
      addRuleTags(builder, impactedGetter, ruleInfo)
    })

    val impactedReturnMethods = getCallsMatchingReturnRegex(cpg, typeDeclVal, s"($typeDeclMemberName)")
    impactedReturnMethods
      .foreach(storeForTag(builder, _)(InternalTag.SENSITIVE_METHOD_RETURN.toString, ruleInfo.id))

  }
}
