package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.TaggerCache
import ai.privado.languageEngine.java.passes.read.DatabaseReadUtility.{fromRegexPattern, selectRegexPattern}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

class DatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache, classTableMapping: Map[String, TypeDecl])
    extends ForkJoinParallelCpgPass[Expression](cpg) {

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
      .toArray ++ cpg.annotation.ast
      .collectAll[AnnotationLiteral]
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    DatabaseReadUtility.processDBReadNode(builder, taggerCache, classTableMapping, cpg, node)
  }
}
