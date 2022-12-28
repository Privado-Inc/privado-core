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
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.nodes.{Member, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import java.util.UUID
import scala.collection.mutable

class IdentifierTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE     = UUID.randomUUID.toString

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
            if (!TaggerCache.typeDeclMemberCache.contains(typeDeclVal))
              TaggerCache.typeDeclMemberCache.addOne(typeDeclVal -> mutable.HashMap[String, Member]())
            TaggerCache.typeDeclMemberCache(typeDeclVal).addOne(ruleInfo.id -> typeDeclMember)
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

        // To mark all the field access
        implicit val resolver: ICallResolver = NoResolve

        val impactedGetters = cpg.method
          .fullNameExact(Operators.fieldAccess, Operators.indirectFieldAccess)
          .callIn
          .where(_.argument(1).isIdentifier.typeFullName(typeDeclVal))
          .where(_.argument(2).code(memberNameRegex).filterNot(item => item.code.equals(item.code.toUpperCase)))
          // .where(_.inAst.isMethod.name("get.*"))
          .l

        impactedGetters.foreach(impactedGetter => {
          if (impactedGetter.tag.nameExact(Constants.id).l.isEmpty) {
            storeForTag(builder, impactedGetter)(InternalTag.SENSITIVE_FIELD_ACCESS.toString)
            addRuleTags(builder, impactedGetter, ruleInfo)
          }
        })

        val impactedReturnMethods = cpg.typeDecl
          .where(_.fullName(typeDeclVal))
          .method
          .block
          .astChildren
          .isReturn
          .code("(?i).*" + {
            TaggerCache
              .typeDeclMemberCache(typeDeclVal)
              .get(ruleInfo.id) match {
              case Some(a) => a.name
              case _       => "Member not found"
            }
          } + ".*")
          .method
          .callIn
          .l

        impactedReturnMethods.foreach(impactedReturnCall => {
          storeForTag(builder, impactedReturnCall)(InternalTag.SENSITIVE_METHOD_RETURN.toString, ruleInfo.id)
        })

      })

    if (ScanProcessor.config.disable2ndLevelClosure) {
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
  }

  /** Tag identifier of all the typeDeclaration who have a member of type -> memberType in argument Represent Step 2.2
    */
  private def tagObjectOfTypeDeclHavingMemberType(
    builder: BatchedUpdate.DiffGraphBuilder,
    memberType: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclHavingMemberTypeTuple =
      cpg.typeDecl.member.typeFullName(memberType).map(member => (member, member.typeDecl.fullName)).dedup.l
    typeDeclHavingMemberTypeTuple.foreach(typeDeclTuple => {
      val typeDeclVal    = typeDeclTuple._2
      val typeDeclMember = typeDeclTuple._1
      if (!TaggerCache.typeDeclMemberCache.contains(typeDeclVal))
        TaggerCache.typeDeclMemberCache.addOne(typeDeclVal -> mutable.HashMap[String, Member]())
      TaggerCache.typeDeclMemberCache(typeDeclVal).addOne(ruleInfo.id -> typeDeclMember)
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
    })
  }

  /** Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument Represent Step
    * 2.3
    */
  private def tagObjectOfTypeDeclExtendingType(
    builder: BatchedUpdate.DiffGraphBuilder,
    typeDeclName: String,
    ruleInfo: RuleInfo
  ) = {
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName)).dedup.l

    typeDeclsExtendingTypeName.foreach(typeDecl => {
      TaggerCache.typeDeclDerivedByExtendsCache.addOne(typeDecl.fullName, typeDecl)
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
        storeForTag(builder, impactedObject)(
          ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
          TaggerCache.typeDeclMemberCache(typeDeclName).get(ruleInfo.id) match {
            case Some(a) => a.name
            case _       => "Member not found"
          }
        )
      })
    })
  }
}
