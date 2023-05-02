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

package ai.privado.languageEngine.java.semantic

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Semantic, Language}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

object JavaSemanticGenerator {

  implicit val resolver: ICallResolver = NoResolve
  private val logger                   = LoggerFactory.getLogger(getClass)

  /** Utility to get the default semantics for dataflow queries
    * @return
    */
  def getDefaultSemantics: Semantics = {
    DefaultSemantics()
  }

  /** Utility to get the semantics (default + custom) using cpg for dataflow queries
    *
    * @param cpg
    *   \- cpg for adding customSemantics
    * @return
    */
  def getSemantics(cpg: Cpg, privadoScanConfig: PrivadoInput, ruleCache: RuleCache): Semantics = {
    val lang = AppCache.repoLanguage
    if (lang != Language.JAVA) {
      getDefaultSemantics
    } else {
      val customSinkSemantics = cpg.call
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .methodFullName
        .dedup
        .l
        .map(generateSemanticForTaint(_, -1))
        .sorted

      val nonTaintingMethods = cpg.method.where(_.callIn).isExternal(true).fullName(".*:(void|boolean|long|int)\\(.*").l

      var customNonTaintDefaultSemantics   = List[String]()
      var specialNonTaintDefaultSemantics  = List[String]()
      var customStringSemantics            = List[String]()
      var customNonPersonalMemberSemantics = List[String]()

      if (!privadoScanConfig.disableRunTimeSemantics) {
        customNonTaintDefaultSemantics = nonTaintingMethods
          .fullNameNot(".*\\.(add|put|<init>|set|get|append|store|insert|update|merge).*")
          .fullName
          .l
          .map(generateNonTaintSemantic)
          .sorted

        specialNonTaintDefaultSemantics = nonTaintingMethods
          .fullName(".*\\.(add|put|set|get|append|store|insert|update|merge).*")
          .fullName
          .l
          .map(generateSemanticForTaint(_, 0))
          .sorted

        customStringSemantics = cpg.method
          .filter(_.isExternal)
          .where(_.callIn)
          .fullName(".*:java.lang.String\\(.*")
          .fullNameNot(".*\\.set[A-Za-z_]*:.*")
          .fullName
          .dedup
          .l
          .map(generateSemanticForTaint(_, -1))
          .sorted

        customNonPersonalMemberSemantics = generateNonPersonalMemberSemantics(cpg)
      }
      val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

      logger.debug("\nCustom Non taint default semantics")
      customNonTaintDefaultSemantics.foreach(logger.debug)
      logger.debug("\nCustom specialNonTaintDefaultSemantics semantics")
      specialNonTaintDefaultSemantics.foreach(logger.debug)
      logger.debug("\nCustom customStringSemantics semantics")
      customStringSemantics.foreach(logger.debug)
      logger.debug("\nCustom customNonPersonalMemberSemantics semantics")
      customNonPersonalMemberSemantics.foreach(logger.debug)
      logger.debug("\nCustom customSinkSemantics semantics")
      customSinkSemantics.foreach(logger.debug)
      logger.debug("\nCustom semanticFromConfig semantics")
      semanticFromConfig.foreach(logger.debug)

      val list =
        customNonTaintDefaultSemantics ++ specialNonTaintDefaultSemantics ++ customStringSemantics ++ customNonPersonalMemberSemantics ++ customSinkSemantics ++ semanticFromConfig
      val parsed         = new Parser().parse(list.mkString("\n"))
      val finalSemantics = JavaSemanticGenerator.getDefaultSemantics.elements ++ parsed
      Semantics.fromList(finalSemantics)
    }
  }

  /** Generate semantics for tainting passed argument based on the number of parameter in method signature
    * @param methodName
    *   \- complete signature of method
    * @return
    *   \- semantic string
    */
  private def generateSemanticForTaint(methodName: String, toTaint: Int) = {
    var parameterSemantics = ""
    var parameterNumber    = 2
    if (methodName.matches(".*:<unresolvedSignature>\\(\\d+\\).*")) {
      parameterNumber = 7
    } else {
      parameterNumber = methodName.count(_.equals(','))
    }
    for (i <- 0 to (parameterNumber + 1))
      parameterSemantics += s"$i->$toTaint "
    "\"" + methodName + "\" " + parameterSemantics.trim
  }

  /** Generate semantics for personal setters to taint the calling object based on the number of parameter in method
    * signature
    * @param methodName
    *   \- complete signature of method
    * @return
    *   \- semantic string
    */
  private def generateSetterSemantic(methodName: String) = {
    var parameterSemantics = "0->0 "
    var parameterNumber    = 2
    if (methodName.matches(".*:<unresolvedSignature>\\(\\d+\\).*")) {
      parameterNumber = 7
    } else {
      parameterNumber = methodName.count(_.equals(','))
    }
    for (i <- 1 to (parameterNumber + 1))
      parameterSemantics += s"$i->$i $i->0 "
    "\"" + methodName + "\" " + parameterSemantics.trim
  }

  /** Generate Semantic string based on input Semantic
    * @param semantic
    *   \- semantic object containing semantic information
    * @return
    */
  private def generateSemantic(semantic: Semantic) = {
    if (semantic.signature.nonEmpty) {
      val generatedSemantic = "\"" + semantic.signature.trim + "\" " + semantic.flow
      Some(generatedSemantic.trim)
    } else
      None
  }

  /** Returns the Semantics for functions with void return type Default behavior is not propagating the taint
    * @param String
    *   methodFullName
    * @return
    *   String
    */
  private def generateNonTaintSemantic(methodFullName: String): String = {
    "\"" + methodFullName + "\" "
  }

  /** Generates Semantics for non Personal member
    * @param cpg
    * @return
    *   non-tainting semantic rule
    */
  def generateNonPersonalMemberSemantics(cpg: Cpg): List[String] = {

    val nonPersonalGetterSemantics = cpg.tag
      .where(_.nameExact(InternalTag.INSENSITIVE_METHOD_RETURN.toString))
      .call
      .whereNot(_.tag.nameExact(InternalTag.SENSITIVE_METHOD_RETURN.toString))
      .methodFullName
      .dedup
      .map(methodName => generateNonTaintSemantic(methodName))
      .l

    val nonPersonalSetterMethodFullNames =
      cpg.tag
        .where(_.nameExact(InternalTag.INSENSITIVE_SETTER.toString))
        .call
        .whereNot(_.nameExact(InternalTag.SENSITIVE_SETTER.toString))
        .methodFullName
        .dedup
        .map(methodName => generateNonTaintSemantic(methodName))
        .l

    val personalSetterMethodFullNames =
      cpg.tag
        .where(_.nameExact(InternalTag.SENSITIVE_SETTER.toString))
        .call
        .methodFullName
        .dedup
        .map(methodName => generateSetterSemantic(methodName))
        .l
    (nonPersonalGetterSemantics ::: nonPersonalSetterMethodFullNames ::: personalSetterMethodFullNames).sorted
  }
}
