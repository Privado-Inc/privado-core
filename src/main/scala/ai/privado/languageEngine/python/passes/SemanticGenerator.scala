package ai.privado.languageEngine.python.passes

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, Semantic}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

object SemanticGenerator {

  implicit val resolver: ICallResolver = NoResolve
  private val logger                   = LoggerFactory.getLogger(getClass)

  /** Utility to get the default semantics for dataflow queries
    *
    * @return
    */
  def getDefaultSemantics: Semantics = {
    DefaultSemantics()
  }

  def getSemantics(cpg: Cpg, ruleCache: RuleCache): Semantics = {
    val customSinkSemantics = cpg.call
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .l
      .map(call => generateSemanticForTaint(call.methodFullName, -1, call.code))
      .dedup
      .sorted

    val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

    logger.debug("\nCustom customSinkSemantics semantics")
    customSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    val list           = customSinkSemantics ++ semanticFromConfig
    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = SemanticGenerator.getDefaultSemantics.elements ++ parsed
    Semantics.fromList(finalSemantics)
  }

  private def generateSemanticForTaint(methodName: String, toTaint: Int, code: String) = {
    var parameterNumber = code.count(_.equals(','))
    if (parameterNumber <= 1)
      parameterNumber = 2
    var parameterSemantics = ""
    for (i <- 0 to (parameterNumber + 1))
      parameterSemantics += s"$i->$toTaint "
    "\"" + methodName + "\" " + parameterSemantics.trim
  }

  private def generateSemantic(semantic: Semantic) = {
    if (semantic.signature.nonEmpty) {
      val generatedSemantic = "\"" + semantic.signature.trim + "\" " + semantic.flow
      Some(generatedSemantic.trim)
    } else
      None
  }

}
