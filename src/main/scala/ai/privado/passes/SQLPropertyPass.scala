package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.semantic.language.*
import ai.privado.model.sql.{SQLColumn, SQLQuery}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLParser
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import org.slf4j.LoggerFactory

class SQLPropertyPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.iterator.filter(prop => prop.name.matches("(?i)query")).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: NewJavaProperty): Unit = {
    addSqlNodes(builder, property)
  }

  private def addSqlNodes(builder: SQLPropertyPass.this.DiffGraphBuilder, property: NewJavaProperty): Unit = {

    val query        = property.value
    val lineNumber   = property.lineNumber.getOrElse(-1).asInstanceOf[Int]
    val columnNumber = property.columnNumber.getOrElse(-1).asInstanceOf[Int]
    try {
      SQLParser.parseSqlQuery(query) match {
        case Some(parsedQueryList) =>
          parsedQueryList.zipWithIndex.foreach { case (parsedQueryItem: SQLQuery, queryOrder) =>
            buildAndReturnIndividualQueryNode(
              builder,
              property.sourceFileOut.next(),
              parsedQueryItem,
              query,
              lineNumber,
              queryOrder
            )
          }
        case None =>
          logger.debug("Failed to parse query: There might be a problem with the syntax.")
          None
      }
    } catch {
      case ex: Exception =>
        logger.debug(s"Error while parsing SQL query at line $lineNumber: ${ex.getMessage}")
        None
    }
  }

  private def buildAndReturnIndividualQueryNode(
    builder: DiffGraphBuilder,
    fileNode: File,
    queryModel: SQLQuery,
    query: String,
    queryLineNumber: Int,
    queryOrder: Int
  ): Unit = {
    // Have added tableName in name key
    // Have added columns in value key

    val queryNode = NewSqlQueryNode().name(queryModel.queryType).code(query).lineNumber(queryLineNumber)

    val tableNode = NewSqlTableNode()
      .name(queryModel.table.name)
      .code(query)
      .lineNumber(queryLineNumber + queryModel.table.lineNumber - 1)
      .columnNumber(queryModel.table.columnNumber)
      .order(queryOrder)

    builder.addEdge(queryNode, tableNode, EdgeTypes.AST)
    builder.addEdge(queryNode, fileNode, EdgeTypes.SOURCE_FILE)
    builder.addEdge(tableNode, fileNode, EdgeTypes.SOURCE_FILE)

    queryModel.column.zipWithIndex.foreach { case (queryColumn: SQLColumn, columnIndex) =>
      val columnNode = NewSqlColumnNode()
        .name(queryColumn.name)
        .code(queryColumn.name)
        .lineNumber(queryLineNumber + queryColumn.lineNumber - 1)
        .columnNumber(queryColumn.columnNumber)
        .order(columnIndex)
      builder.addEdge(tableNode, columnNode, EdgeTypes.AST)
      builder.addEdge(columnNode, fileNode, EdgeTypes.SOURCE_FILE)

    }
  }
}
