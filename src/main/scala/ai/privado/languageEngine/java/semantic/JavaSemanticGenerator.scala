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

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.semantic.SemanticGenerator
import ai.privado.utility.Utilities.semanticFileExporter
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

object JavaSemanticGenerator extends SemanticGenerator {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Utility to get the semantics (default + custom) using cpg for dataflow queries
    *
    * @param cpg
    *   \- cpg for adding customSemantics
    * @return
    */
  def getSemantics(
    cpg: Cpg,
    privadoScanConfig: PrivadoInput,
    ruleCache: RuleCache,
    exportRuntimeSemantics: Boolean = false
  ): Semantics = {
    val customSinkSemantics = Seq[String]()
      /*getMaximumFlowSemantic(
      cpg.call
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .map(generateSemanticForTaint(_, -1))
    )
      */

    var customNonTaintDefaultSemantics   = Seq[String]()
    var specialNonTaintDefaultSemantics  = Seq[String]()
    var customStringSemantics            = Seq[String]()
    var customNonPersonalMemberSemantics = Seq[String]()

    if (!privadoScanConfig.disableRunTimeSemantics) {
      /*
      val nonTaintingMethods = cpg.method.where(_.callIn).isExternal(true).fullName(".*:(void|boolean|long|int)\\(.*").l
      customNonTaintDefaultSemantics = getMaximumFlowSemantic(
        nonTaintingMethods
          .fullNameNot(".*\\.(add|put|<init>|set|get|append|store|insert|update|merge).*")
          .map(generateSemanticForTaint(_))
      )

      specialNonTaintDefaultSemantics = getMaximumFlowSemantic(
        nonTaintingMethods
          .fullName(".*\\.(add|put|set|get|append|store|insert|update|merge).*")
          .map(generateSemanticForTaint(_, 0))
      )

      customStringSemantics = getMaximumFlowSemantic(
        cpg.method
          .filter(_.isExternal)
          .where(_.callIn)
          .fullName(".*:java.lang.String\\(.*")
          .fullNameNot(".*\\.set[A-Za-z_]*:.*")
          .map(generateSemanticForTaint(_, -1))
      )
      */
      customNonTaintDefaultSemantics = ruleCache.getExternalSemantics

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

    if (exportRuntimeSemantics) {
      try {
        val headerAndSemanticPairs: Map[String, Seq[String]] = Map(
          "Custom Non taint default semantics"                -> customNonTaintDefaultSemantics,
          "Custom specialNonTaintDefaultSemantics semantics"  -> specialNonTaintDefaultSemantics,
          "Custom customStringSemantics semantics"            -> customStringSemantics,
          "Custom customNonPersonalMemberSemantics semantics" -> customNonPersonalMemberSemantics,
          "Custom customSinkSemantics semantics"              -> customSinkSemantics,
          "Custom semanticFromConfig semantics"               -> semanticFromConfig
        )
        semanticFileExporter(
          sourceRepoLocation = privadoScanConfig.sourceLocation.headOption.getOrElse(""),
          headerAndSemanticPairs
        )
      } catch {
        case e: Exception => logger.debug(s"There was a problem exporting the semantics. ${e.getMessage}")
      }
    }

    val list =
      customNonTaintDefaultSemantics ++ specialNonTaintDefaultSemantics ++ customStringSemantics ++ customNonPersonalMemberSemantics ++ customSinkSemantics ++ semanticFromConfig

    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = JavaSemanticGenerator.getDefaultSemantics.elements ++ parsed
    Semantics.fromList(finalSemantics)
  }

  /** Generates Semantics for non Personal member
    * @param cpg
    * @return
    *   non-tainting semantic rule
    */
  def generateNonPersonalMemberSemantics(cpg: Cpg): List[String] = {

    val nonPersonalGetterSemantics = getMaximumFlowSemantic(
      cpg.tag
        .where(_.nameExact(InternalTag.INSENSITIVE_METHOD_RETURN.toString))
        .call
        .whereNot(_.tag.nameExact(InternalTag.SENSITIVE_METHOD_RETURN.toString))
        .map(generateSemanticForTaint(_))
    ).l

    val nonPersonalSetterMethodFullNames = getMaximumFlowSemantic(
      cpg.tag
        .where(_.nameExact(InternalTag.INSENSITIVE_SETTER.toString))
        .call
        .whereNot(_.nameExact(InternalTag.SENSITIVE_SETTER.toString))
        .map(generateSemanticForTaint(_))
    ).l

    val personalSetterMethodFullNames =
      getMaximumFlowSemantic(
        cpg.tag
          .where(_.nameExact(InternalTag.SENSITIVE_SETTER.toString))
          .call
          .map(methodName => generateSemanticForTaint(methodName, 0))
      ).l
    (nonPersonalGetterSemantics ::: nonPersonalSetterMethodFullNames ::: personalSetterMethodFullNames).sorted
  }
}
