package ai.privado.languageEngine.javascript
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

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.semantic.SemanticGenerator
import ai.privado.utility.Utilities.semanticFileExporter
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

object JavascriptSemanticGenerator extends SemanticGenerator {

  private val logger = LoggerFactory.getLogger(getClass)

  def getSemantics(
    cpg: Cpg,
    privadoScanConfig: PrivadoInput,
    ruleCache: RuleCache,
    exportRuntimeSemantics: Boolean = false
  ): Semantics = {
    val customSinkSemantics = getMaximumFlowSemantic(
      cpg.call
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .where(_.tag.valueExact(Constants.leakages))
        .map(generateSemanticForTaint(_, -1, extraParameter = 1))
    )



    val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

    logger.debug("\nCustom customSinkSemantics semantics")
    customSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    if (exportRuntimeSemantics) {
      try {
        val headerAndSemanticPairs: Map[String, Seq[String]] = Map(
          "Custom generated semantics" -> ruleCache.getExternalSemantics,
          "Custom customSinkSemantics semantics" -> customSinkSemantics,
          "Custom semanticFromConfig semantics"  -> semanticFromConfig
        )
        semanticFileExporter(
          sourceRepoLocation = privadoScanConfig.sourceLocation.headOption.getOrElse(""),
          headerAndSemanticPairs
        )
      } catch {
        case e: Exception => logger.debug(s"There was a problem exporting the semantics. ${e.getMessage}")
      }
    }


    val list           = ruleCache.getExternalSemantics ++ customSinkSemantics ++ semanticFromConfig
    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = JavascriptSemanticGenerator.getDefaultSemantics.elements ++ parsed

    println("Final semantics")
    Semantics.fromList(finalSemantics).elements.foreach(item => println((item.methodFullName, item.mappings, item.regex)))
    Semantics.fromList(finalSemantics)
  }

}
