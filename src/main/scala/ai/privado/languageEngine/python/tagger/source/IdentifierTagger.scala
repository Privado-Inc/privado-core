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
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

import java.util.UUID

class IdentifierTagger(cpg: Cpg, taggerCache: TaggerCache) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_TYPE: String = UUID.randomUUID.toString
  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_EXTENDING_TYPE: String     = UUID.randomUUID.toString

  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val rulePattern = ruleInfo.combinedRulePattern

    val regexMatchingIdentifiers = cpg
      .identifier(rulePattern)
      .whereNot(_.astSiblings.isImport)
      .whereNot(_.astSiblings.isCall.name("import"))
      .l
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
    })

    val regexMatchingFieldIdentifiersIdentifiers =
      cpg.fieldAccess.where(_.fieldIdentifier.canonicalName(rulePattern)).isCall.l
    regexMatchingFieldIdentifiersIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
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
    val indexAccessCalls = indexAccessLiterals.astParent.isCall.whereNot(_.name("__(iter|next)__|print")).l
    indexAccessCalls.foreach(iaCall => {
      storeForTag(builder, iaCall)(InternalTag.INDEX_ACCESS_CALL.toString)
      addRuleTags(builder, iaCall, ruleInfo)
    })

    val regexMatchingMembers = cpg.member.name(rulePattern).l
    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo)
    })

    // ----------------------------------------------------
    // Step 1: First Level derivation Implementation
    // ----------------------------------------------------
    // Tag the class object instances which has member PIIs
    tagObjectOfTypeDeclHavingMemberName(builder, rulePattern, ruleInfo)

    // ----------------------------------------------------
    // Step 2: Second Level derivation Implementation
    // ----------------------------------------------------
    // Tag the inherited object instances which has member PIIs from extended class
    // TODO: tagObjectOfTypeDeclExtendingType(builder, rulePattern, ruleInfo)

  }

  /** Tag identifier of all the typeDeclaration who have a member as memberName in argument Represent
    */
  private def tagObjectOfTypeDeclHavingMemberName(
    builder: DiffGraphBuilder,
    rulePattern: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val typeDeclWithMemberNameHavingMemberName = cpg.typeDecl
      .where(_.member.name(rulePattern).filterNot(item => item.name.equals(item.name.toUpperCase)))
      .map(typeDeclNode => (typeDeclNode, typeDeclNode.member.name(rulePattern).l))
      .l

    typeDeclWithMemberNameHavingMemberName
      .distinctBy(_._1.fullName)
      .foreach(typeDeclValEntry => {
        typeDeclValEntry._2.foreach(typeDeclMember => {
          var typeDeclVal = typeDeclValEntry._1.fullName.stripSuffix("<meta>")

          // updating cache
          taggerCache.addItemToTypeDeclMemberCache(typeDeclVal, ruleInfo.id, typeDeclMember)
          typeDeclVal = typeDeclVal.replaceAll(":<module>.", ":<module>.*")

          val typeDeclMemberName = typeDeclMember.name

          // Note: Partially Matching the typeFullName with typeDecl fullName
          // typeFullName: models.py:<module>.Profile.Profile<body>
          // typeDeclVal: models.py:<module>.Profile
          val impactedObjects = cpg.identifier
            .where(_.typeFullName(".*" + typeDeclVal + ".*"))
            .whereNot(_.astSiblings.isImport)
            .whereNot(_.astSiblings.isCall.name("import"))
            .whereNot(_.code("this|self|cls"))

          impactedObjects
            .foreach(impactedObject => {
              if (impactedObject.tag.nameExact(Constants.id).l.isEmpty) {
                storeForTag(builder, impactedObject)(
                  InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString,
                  ruleInfo.id
                )
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
          // tagAllFieldAccessAndGetters(builder, typeDeclVal, ruleInfo, typeDeclMemberName)
        })
      })
  }
}
