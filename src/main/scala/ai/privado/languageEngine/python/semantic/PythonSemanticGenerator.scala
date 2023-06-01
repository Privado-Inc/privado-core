package ai.privado.languageEngine.python.semantic

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.Language.UNKNOWN
import ai.privado.model.{CatLevelOne, Constants, Semantic}
import ai.privado.semantic.SemanticGenerator
import ai.privado.utility.Utilities.semanticFileExporter
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable

object PythonSemanticGenerator extends SemanticGenerator {

  private val logger = LoggerFactory.getLogger(getClass)

  def getSemantics(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoScanConfig: PrivadoInput,
    exportRuntimeSemantics: Boolean = false
  ) = {
    val customSinkSemantics = getMaximumFlowSemantic(
      cpg.call
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .map(generateSemanticForTaint(_, -1))
    )

    val semanticFromConfig = ruleCache.getRule.semantics.flatMap(generateSemantic).sorted

    logger.debug("\nCustom customSinkSemantics semantics")
    customSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    if (exportRuntimeSemantics) {
      try {
        val headerAndSemanticPairs: Map[String, Seq[String]] = Map(
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

    val list           = customSinkSemantics ++ semanticFromConfig
    val parsed         = new Parser().parse(list.mkString("\n"))
    val finalSemantics = PythonSemanticGenerator.getDefaultSemantics.elements ++ parsed
    Semantics.fromList(finalSemantics)
  }

  def generateSemanticForTaint(callNode: Call, toTaint: Int): Semantic = {
    val namedArgumentList = callNode.argument.flatMap { arg =>
      if (arg.argumentName.isDefined)
        Some("\"" + arg.argumentName.get + "\"")
      else
        None
    }.l
    val parameterSemantic = mutable.HashSet[String]()

    namedArgumentList.map { item =>
      if (toTaint != -2)
        parameterSemantic.add(s"$item->$toTaint")
      parameterSemantic.add(s"$item->$item")
    }

    val superSemantics = super.generateSemanticForTaint(callNode, toTaint, 1)

    Semantic(
      callNode.methodFullName,
      superSemantics.flow + " " + parameterSemantic.toList.sorted.mkString(" ").trim,
      "",
      UNKNOWN,
      Array()
    )
  }

}
