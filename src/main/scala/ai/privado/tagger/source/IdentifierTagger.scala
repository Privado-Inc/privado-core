/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.tagger.source

import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.utility.Utilities._

import java.util.UUID
import scala.collection.mutable

class IdentifierTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE     = UUID.randomUUID.toString

  val typeDeclMemberNameCache = mutable.HashMap[String, mutable.HashMap[String, String]]()

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // Step 1.1
    val rulePattern              = ruleInfo.patterns.head
    val regexMatchingIdentifiers = cpg.identifier(rulePattern).l
    regexMatchingIdentifiers.foreach(identifier => {
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
      .where(_.member.name(memberNameRegex))
      .map(typeDeclNode => (typeDeclNode, typeDeclNode.member.name(memberNameRegex).l))
      .l
    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        val typeDeclVal = typeDeclValEntry._1.fullName
        val typeDeclMemberName = typeDeclValEntry._2.headOption match {
          case Some(typeDeclMember) => // updating cache
            if (!typeDeclMemberNameCache.contains(typeDeclVal))
              typeDeclMemberNameCache.addOne(typeDeclVal -> mutable.HashMap[String, String]())
            typeDeclMemberNameCache(typeDeclVal).addOne(ruleInfo.id -> typeDeclMember.name)
            typeDeclMember.name
          case None =>
            logger.debug("Exception when retreiving member name for derived class")
            "Member not found"
        }
        val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
        impactedObjects.foreach(impactedObject => {
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
          .where(_.argument(2).code(memberNameRegex))
          // .where(_.inAst.isMethod.name("get.*"))
          .l

        impactedGetters.foreach(impactedGetter => {
          if (impactedGetter.tag.nameExact(Constants.id).l.isEmpty) {
            storeForTag(builder, impactedGetter)(InternalTag.SENSITIVE_FIELD_ACCESS.toString)
            addRuleTags(builder, impactedGetter, ruleInfo)
          }
        })
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
    val typeDeclHavingMemberType = cpg.typeDecl.where(_.member.typeFullName(memberType))
    typeDeclHavingMemberType.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
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
    val typeDeclsExtendingTypeName = cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclName))

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {
      val impactedObjects = cpg.identifier.where(_.typeFullName(typeDeclVal))
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
          typeDeclMemberNameCache(typeDeclName).getOrElse(ruleInfo.id, "Member Not Found")
        )
      })
    })
  }
}
