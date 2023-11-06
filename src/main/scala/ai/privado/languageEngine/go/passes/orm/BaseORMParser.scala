package ai.privado.languageEngine.go.passes.orm

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLNodeBuilder
import better.files._
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

abstract class BaseORMParser(cpg: Cpg) extends PrivadoParallelCpgPass[TypeDecl](cpg) {

  override def generateParts(): Array[_ <: AnyRef] = ???

  override def runOnPart(builder: DiffGraphBuilder, model: TypeDecl): Unit = {
    Try(model.file.head) match {
      case Success(fileNode) =>
        buildAndAddSqlQueryNodes(builder, model, fileNode)
      case Failure(_) =>
        val fileNode = NewFile().name(Constants.Unknown)
        buildAndAddSqlQueryNodes(builder, model, fileNode)
    }
  }

  private def buildAndAddSqlQueryNodes(
    builder: DiffGraphBuilder,
    model: TypeDecl,
    fileNode: NodeOrDetachedNode
  ): Unit = {
    try {
      val sqlTable: SQLTable = SQLTable(
        model.name,
        model.lineNumber.getOrElse(Integer.valueOf(-1)),
        model.columnNumber.getOrElse(Integer.valueOf(-1))
      )
      val sqlColumns: List[SQLColumn] = model.member.l.map(x =>
        SQLColumn(x.code, x.lineNumber.getOrElse(Integer.valueOf(-1)), x.columnNumber.getOrElse(Integer.valueOf(-1)))
      )
      val queryModel = SQLQuery(SQLQueryType.CREATE, sqlTable, sqlColumns)
      SQLNodeBuilder.buildAndReturnIndividualQueryNode(
        builder,
        fileNode,
        queryModel,
        model.code,
        model.lineNumber.getOrElse(Integer.valueOf(-1)),
        0
      )
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        println(s"Error while building SQL nodes: ${ex.getMessage}")
    }
  }
}
