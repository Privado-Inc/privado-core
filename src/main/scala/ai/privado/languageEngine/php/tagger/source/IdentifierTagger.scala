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

package ai.privado.languageEngine.php.tagger.source

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.java.tagger.source.Utility.{
  getCallsMatchingReturnRegex,
  getFieldAccessCallsMatchingRegex
}
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate

import java.util.UUID
import scala.util.{Failure, Success, Try}

class IdentifierTagger(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE: String     = UUID.randomUUID.toString

  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val rulePattern              = ruleInfo.combinedRulePattern
    val regexMatchingIdentifiers = cpg.identifier(rulePattern).l
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })

    val regexMatchingFieldIdentifiersIdentifiers =
      cpg.fieldAccess.where(_.fieldIdentifier.canonicalName(rulePattern)).isCall.l
    regexMatchingFieldIdentifiersIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })

    val regexMatchingMembers = cpg.member.name(rulePattern).l
    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member, ruleCache)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo, ruleCache)
    })

    Try(tagObjectOfTypeDeclHavingMemberName(builder, rulePattern, ruleInfo)) match {
      case Success(_) =>
      case Failure(reason) =>
        logger.debug(
          s"Error while tagging derived source for the rule - ${ruleInfo.id}, having pattern ${ruleInfo.combinedRulePattern}"
        )
        logger.error(s"Error while tagging entities - ${reason.toString}")
    }
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
        typeDeclValEntry._2.foreach(typeDeclMember => {
          val typeDeclFullName = typeDeclValEntry._1.fullName
          // updating cache
          taggerCache.addItemToTypeDeclMemberCache(typeDeclFullName, ruleInfo.id, typeDeclMember)
          val typeDeclMemberName = typeDeclMember.name
          // Have started tagging Parameters as well, as in collection points sometimes there is no referencing Identifier present for a local
          val impactedObjects =
            cpg.identifier
              .where(_.typeFullName(typeDeclFullName))
              .l ::: cpg.parameter.where(_.typeFullName(typeDeclFullName)).l

          impactedObjects
            .foreach(impactedObject => {
              if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
                storeForTag(builder, impactedObject, ruleCache)(
                  InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString,
                  ruleInfo.id
                )
                storeForTag(builder, impactedObject, ruleCache)(
                  Constants.id,
                  Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
                )
                storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
              }
              storeForTag(builder, impactedObject, ruleCache)(
                Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
                ruleInfo.id
              )
              // Tag for storing memberName in derived Objects -> user --> (email, password)
              storeForTag(builder, impactedObject, ruleCache)(
                ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
                typeDeclMemberName
              )
            })

          // To Mark all field Access and getters
          tagAllFieldAccessAndGetters(builder, typeDeclFullName, ruleInfo, typeDeclMemberName)
        })
      })

    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        val typeDeclName = typeDeclValEntry._1.fullName
        // Step 2.2
        tagObjectOfTypeDeclHavingMemberType(builder, typeDeclName, ruleInfo)
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
      taggerCache.addItemToTypeDeclMemberCache(typeDeclVal, ruleInfo.id, typeDeclMember)
      val impactedObjects =
        cpg.identifier.where(_.typeFullName(typeDeclVal)).l ::: cpg.parameter
          .where(_.typeFullName(typeDeclVal))
          .l
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject, ruleCache)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString)
          storeForTag(builder, impactedObject, ruleCache)(
            Constants.id,
            Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE
          )
          storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
        }
        storeForTag(builder, impactedObject, ruleCache)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE,
          ruleInfo.id
        )
      })

      // To Mark all field Access and getters
      tagAllFieldAccessAndGetters(builder, typeDeclVal, ruleInfo, typeDeclMember.name)
    })
  }

  /** Function to tag all the field access operations and all the methods whose return code matches the member regex
    *
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
      storeForTag(builder, impactedGetter, ruleCache)(InternalTag.SENSITIVE_FIELD_ACCESS.toString)
      addRuleTags(builder, impactedGetter, ruleInfo, ruleCache)
    })

    val impactedReturnMethods = getCallsMatchingReturnRegex(cpg, typeDeclVal, s"($typeDeclMemberName)")
    impactedReturnMethods
      .foreach(storeForTag(builder, _, ruleCache)(InternalTag.SENSITIVE_METHOD_RETURN.toString, ruleInfo.id))

  }
}
