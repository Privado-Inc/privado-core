package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.passes.read.DatabaseReadUtility.{fromRegexPattern, selectRegexPattern}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class DatabaseQueryReadPass(
  cpg: Cpg,
  ruleCache: RuleCache,
  taggerCache: TaggerCache,
  privadoInputConfig: PrivadoInput,
  classTableMapping: Map[String, TypeDecl]
) extends PrivadoParallelCpgPass[Expression](cpg) {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[Expression] = {
//    CPG query to fetch the Literal with SQL string
//    'Repeat until' is used to combine multiline SQL queries into one
    cpg.literal
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(Operators.addition))))
      .isCall
      .argument
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .toArray ++ cpg.annotation.ast // This query is to match the query present in NamedQuery annotation in hibernate
      .collectAll[AnnotationLiteral]
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    DatabaseReadUtility.processDBReadNode(
      builder,
      ruleCache,
      taggerCache,
      classTableMapping,
      cpg,
      node,
      privadoInputConfig
    )
  }
}
