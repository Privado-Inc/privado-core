package ai.privado.languageEngine.python.semantic

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.semantic.SemanticGenerator
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

object PythonSemanticGenerator extends SemanticGenerator {

  private val logger = LoggerFactory.getLogger(getClass)

  def getSemantics(cpg: Cpg, ruleCache: RuleCache) = {
    val customSinkSemantics = getMaximumFlowSemantic(
      cpg.call
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.leakages))
        .map(generateSemanticForTaint(_, -1, extraParameter = 1))
    )

    val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

    logger.debug("\nCustom customSinkSemantics semantics")
    customSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    val list           = customSinkSemantics ++ semanticFromConfig
    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = PythonSemanticGenerator.getDefaultSemantics.elements ++ parsed
    Semantics.fromList(finalSemantics)
  }

}
