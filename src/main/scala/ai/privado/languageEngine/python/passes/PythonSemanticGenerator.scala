package ai.privado.languageEngine.python.passes

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, Semantic}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

object PythonSemanticGenerator {

  implicit val resolver: ICallResolver = NoResolve
  private val logger                   = LoggerFactory.getLogger(getClass)

  /** Utility to get the default semantics for dataflow queries
    *
    * @return
    */
  def getDefaultSemantics: Semantics = {
    DefaultSemantics()
  }

  def getSemantics(cpg: Cpg, privadoScanConfig: PrivadoInput, ruleCache: RuleCache) = {
    val leakageSinkSemantics = cpg.call
      .where(_.tag.nameExact(Constants.id).value("Leakages.*"))
      .l
      .map(call => generateOneToOneSemanticForTaint(call.methodFullName, call.code))
      .dedup
      .sorted

    val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

    logger.debug("\nCustom customSinkSemantics semantics")
    leakageSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    val list           = leakageSinkSemantics ++ semanticFromConfig
    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = PythonSemanticGenerator.getDefaultSemantics.elements ++ parsed
    Semantics.fromList(finalSemantics)
  }

  private def generateOneToOneSemanticForTaint(methodName: String, code: String) = {
    var parameterNumber = code.count(_.equals(','))
    if (parameterNumber <= 2)
      parameterNumber = 5
    var parameterSemantics = ""
    for (i <- 1 to (parameterNumber))
      parameterSemantics += s"$i->$i "
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
