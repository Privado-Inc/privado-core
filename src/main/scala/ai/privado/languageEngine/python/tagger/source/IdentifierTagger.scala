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

package ai.privado.languageEngine.python.tagger.source

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.SourceTaggerUtility.getTypeDeclWithMemberNameHavingMemberName
import ai.privado.utility.Utilities.{addOriginalSourceEdgeAndTag, addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

import java.util.UUID
import cats.Show.Shown.mat

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class IdentifierTagger(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE: String     = UUID.randomUUID.toString

  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val rulePattern = ruleInfo.combinedRulePattern

    val regexMatchingIdentifiers = cpg
      .identifier(rulePattern)
      .whereNot(_.astSiblings.isImport)
      .whereNot(_.astSiblings.isCall.name("import"))
      .l
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

    //    Example: row_vehicle['VEHICLE_REGISTRATION_NUMBER']
    //    Call("<operator>.indexAccess") // Tagging
    //      [Arguments]
    //      -Literal 'VEHICLE_REGISTRATION_NUMBER'
    //      -Identifier row_vehicle
    val indexAccessLiterals = cpg
      .call("<operator>.indexAccess")
      .argument
      .isLiteral
      .code("(?:\"|'|`)(" + rulePattern + ")(?:\"|'|`)")
      .whereNot(_.code(".*\\s.*"))
      .l
    val indexAccessCalls = indexAccessLiterals.astParent.isCall
      .whereNot(_.method.name(".*<meta.*>$"))
      .whereNot(_.name("__(iter|next)__|print"))
      .l
    indexAccessCalls.foreach(iaCall => {
      storeForTag(builder, iaCall, ruleCache)(InternalTag.INDEX_ACCESS_CALL.toString)
      addRuleTags(builder, iaCall, ruleInfo, ruleCache)
    })

    val regexMatchingMembers = cpg.member.name(rulePattern).l
    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member, ruleCache)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo, ruleCache)
    })

    // ----------------------------------------------------
    // Step 1: First Level derivation Implementation
    // ----------------------------------------------------
    // Tag the class object instances which has member PIIs
    tagObjectOfTypeDeclHavingMemberName(builder, rulePattern, ruleInfo)

  }

  /** Tag identifier of all the typeDeclaration who have a member as memberName in argument Represent
    */
  private def tagObjectOfTypeDeclHavingMemberName(
    builder: DiffGraphBuilder,
    rulePattern: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclWithMemberNameHavingMemberName = getTypeDeclWithMemberNameHavingMemberName(cpg, ruleInfo)
      .map(tD => {
        (tD._1, tD._2.filter(m => !m.dynamicTypeHintFullName.exists(_.matches(".*<metaClassAdapter>"))))
      })
      .l

    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        typeDeclValEntry._2
          .foreach(typeDeclMember => {
            val typeDeclVal = typeDeclValEntry._1.fullName.stripSuffix("<meta>")

            // Don't add ScoutSuite/core/rule.py:<module> this kind of entries
            if (!typeDeclVal.endsWith("<module>")) {
              // updating cache
              taggerCache.addItemToTypeDeclMemberCache(typeDeclVal, ruleInfo.id, typeDeclMember)
            }

            val typeDeclMemberName = typeDeclMember.name

            // Note: Partially Matching the typeFullName with typeDecl fullName
            // typeFullName: models.py:<module>.Profile.Profile<body>
            // typeDeclVal: models.py:<module>.Profile
            val impactedObjects = cpg.identifier
              .or(
                _.where(_.typeFullName(".*" + typeDeclVal + ".*")),
                _.filter(_.dynamicTypeHintFullName.exists(_.matches(".*" + typeDeclVal + ".*")))
              )
              .whereNot(_.astSiblings.isImport)
              .whereNot(_.astSiblings.isCall.name("import"))
              .whereNot(_.method.name(".*<meta.*>$"))
              .whereNot(_.code("this|self|cls"))
              .l ::: cpg.parameter
              .filter(n => typeDeclVal.matches(".*" + n.typeFullName))
              .whereNot(_.code("this|self|cls"))
              .l

            impactedObjects
              .foreach(impactedObject => {
                // Add edge between derived source node and the original source
                addOriginalSourceEdgeAndTag(builder, impactedObject, typeDeclMember, ruleCache)

                if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
                  storeForTag(builder, impactedObject, ruleCache)(
                    InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString,
                    ruleInfo.id
                  )
                  storeForTag(builder, impactedObject, ruleCache)(
                    Constants.id,
                    Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
                  )
                  storeForTag(builder, impactedObject, ruleCache)(
                    Constants.catLevelOne,
                    CatLevelOne.DERIVED_SOURCES.name
                  )
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
            // tagAllFieldAccessAndGetters(builder, typeDeclVal, ruleInfo, typeDeclMemberName)
          })
      })

    // ----------------------------------------------------
    // Step 2: Second Level derivation Implementation
    // ----------------------------------------------------
    // Tag the inherited object instances which has member PIIs from extended class
    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        val typeDeclName = typeDeclValEntry._1.fullName.stripSuffix("<meta>")
        tagObjectOfTypeDeclExtendingType(builder, typeDeclName, ruleInfo)
      })
  }

  /** Tag identifier of all the typeDeclaration who inherits from the type -> typeDeclName in argument Represent Step
    */
  private def tagObjectOfTypeDeclExtendingType(
    builder: DiffGraphBuilder,
    typeDeclVal: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclsExtendingTypeName =
      cpg.typeDecl.filter(_.inheritsFromTypeFullName.contains(typeDeclVal)).dedup.l

    typeDeclsExtendingTypeName.foreach(typeDecl => {
      taggerCache.typeDeclDerivedByExtendsCache.put(typeDecl.fullName, typeDecl)

      taggerCache
        .typeDeclMemberCache(typeDeclVal)
        .filter(_._1.equals(ruleInfo.id))
        .foreach(entrySet => {
          val sourceRuleId = entrySet._1
          entrySet._2.foreach(taggerCache.addItemToTypeDeclMemberCache(typeDecl.fullName, sourceRuleId, _))
        })
    })

    typeDeclsExtendingTypeName.fullName.dedup.foreach(typeDeclVal => {

      if (!taggerCache.typeDeclExtendingTypeDeclCache.contains(typeDeclVal))
        taggerCache.typeDeclExtendingTypeDeclCache.put(typeDeclVal, TrieMap[String, TypeDecl]())
      taggerCache
        .getTypeDeclExtendingTypeDeclCacheItem(typeDeclVal)
        .put(ruleInfo.id, cpg.typeDecl.where(_.fullNameExact(typeDeclVal)).head)

      val impactedObjects =
        cpg.identifier
          .where(_.typeFullName(".*" + typeDeclVal + ".*"))
          .whereNot(_.astSiblings.isImport)
          .whereNot(_.astSiblings.isCall.name("import"))
          .whereNot(_.code("this|self|cls"))
          .l ::: cpg.parameter
          .filter(n => typeDeclVal.matches(".*" + n.typeFullName))
          .whereNot(_.code("this|self|cls"))
          .l
      impactedObjects.foreach(impactedObject => {
        if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedObject, ruleCache)(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)
          storeForTag(builder, impactedObject, ruleCache)(
            Constants.id,
            Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE
          )
          storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
        }
        storeForTag(builder, impactedObject, ruleCache)(
          Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
          ruleInfo.id
        )

        // Tag for storing memberName in derived Objects -> patient (patient extends user) --> (email, password)
        // Get the option of the set of members
        val membersOption = taggerCache.typeDeclMemberCache
          .get(typeDeclVal)
          .flatMap(_.get(ruleInfo.id))

        // Access the name property if it's present
        membersOption.foreach { members =>
          members.foreach { member =>
            storeForTag(builder, impactedObject, ruleCache)(
              ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE,
              member.name // Assuming "name" is a property of the Member class
            )
          }
        }

      })
    })
  }
}
