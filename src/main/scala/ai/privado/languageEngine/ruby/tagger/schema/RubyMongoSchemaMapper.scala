package ai.privado.languageEngine.ruby.tagger.schema

import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import ai.privado.model.{
  CatLevelOne,
  Constants,
  DatabaseColumn,
  DatabaseDetails,
  DatabaseSchema,
  DatabaseTable,
  FilterProperty,
  InternalTag,
  Language,
  NodeType,
  RuleInfo
}
import ai.privado.tagger.PrivadoSimpleCpgPass
import ai.privado.utility.ConfigParserUtility
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, File, NewDbNode, NewTag}
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

class RubyMongoSchemaMapper(cpg: Cpg, ruleCache: RuleCache) extends PrivadoSimpleCpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    addToDatabaseCache(builder)
  }
  def addToDatabaseCache(builder: DiffGraphBuilder): Unit = {
    val clientNodes =
      cpg.property.where(_.file.name(".*config/mongoid[.](yml|yaml)")).where(_.name(".*clients.*database")).l

    val deploymentTypeSet = mutable.HashSet[String]()
    val clientsSet        = mutable.HashSet[String]()

    clientNodes.foreach(client => {
      val clientSplit    = client.name.replace("clients.", "").replace(".database", "").split("[.]").toList
      val deploymentType = clientSplit.headOption.getOrElse("")
      deploymentTypeSet.add(deploymentType)
      val clientName = clientSplit.lastOption.getOrElse("")
      clientsSet.add(clientName)
    })

    val clientTableMap = clientsSet
      .map(clientName => {
        val tables = cpg.typeDecl
          .where(_.tag.nameExact(InternalTag.RUBY_MONGO_CLASS_CLIENT.toString).valueExact(clientName))
          .map(typeDecl => {
            val columns = typeDecl.ast.isLiteral
              .where(_.tag.nameExact(InternalTag.RUBY_MONGO_COLUMN.toString))
              .map(lit => {
                val columnName = lit.code.stripPrefix(":")
                val dataType =
                  lit.tag.nameExact(InternalTag.RUBY_MONGO_COLUMN_DATATYPE.toString).value.headOption.getOrElse("")
                val sourceId = lit.tag.nameExact(Constants.id).value.headOption.getOrElse("")
                DatabaseColumn(columnName, "", dataType, sourceId)
              })
              .l

            DatabaseTable(convertClassNameToTableName(typeDecl.name), "", columns)
          })
          .l

        (clientName, tables)
      })
      .toMap

    // Currently we just want to output production schema
    deploymentTypeSet
      .filter(_.equals("production"))
      .foreach(deploymentType => {

        clientsSet.foreach(client => {

          val dbName = clientNodes
            .where(c => c.nameExact(s"$deploymentType.clients.$client.database"))
            .value
            .headOption
            .getOrElse("")
          if (dbName.nonEmpty) {
            createDatabaseSinkRule(
              builder,
              deploymentType,
              dbName,
              "",
              "",
              DatabaseSchema("", deploymentType, "", clientTableMap(client)),
              ruleCache,
              clientNodes.file.head
            )
          }
        })
      })

  }

  /** For a given class name function converts it into a table name as per Active Record or mongo's naming convention Ex
    * \- User -> users UserAction -> user_actions
    * @param className
    * @return
    */
  private def convertClassNameToTableName(className: String): String = {
    // Convert the class name to underscored lowercase
    val underscored = className.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase()

    // Pluralize the underscored name
    val pluralized = if (underscored.endsWith("s")) underscored else underscored + "s"

    pluralized
  }

  private def createDatabaseSinkRule(
    builder: DiffGraphBuilder,
    projectName: String,
    dbName: String,
    dbPlatform: String,
    dbHost: String,
    schema: DatabaseSchema,
    ruleCache: RuleCache,
    fileNode: File
  ) = {
    val ruleId   = f"Storages.Ruby.ReadAndWrite.${projectName}.${dbName}"
    val ruleHost = dbHost

    val customDatabaseSinkRule = RuleInfo(
      ruleId,
      f"${dbName}",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array[String](ruleHost),
      List[String](),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.DEFAULT,
      Array[String]()
    )

    val dbDetails = DatabaseDetails(dbName, dbPlatform, dbHost, "", "", Some(schema))

    ruleCache.setRuleInfo(customDatabaseSinkRule)
    DatabaseDetailsCache.addDatabaseDetails(dbDetails, ruleId)

    val lineNumber = findLinesContainingText(fileNode.name, dbName).headOption.getOrElse(1)

    val databaseNode = NewDbNode()
      .name(dbName)
      .code(dbName)
      .lineNumber(lineNumber)
      .columnNumber(1)
      .order(0)

    builder.addNode(databaseNode)
    builder.addEdge(databaseNode, fileNode, EdgeTypes.SOURCE_FILE)

    addRuleTags(builder, databaseNode, customDatabaseSinkRule)
  }

  private def addRuleTags(builder: DiffGraphBuilder, databaseNode: AstNodeNew, ruleInfo: RuleInfo) = {
    def storeForTag(builder: DiffGraphBuilder, node: AstNodeNew, tagName: String, tagValue: String = "") = {
      builder.addEdge(node, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
      builder
    }

    storeForTag(builder, databaseNode, Constants.id, ruleInfo.id)
    storeForTag(builder, databaseNode, Constants.nodeType, ruleInfo.nodeType.toString)
    storeForTag(builder, databaseNode, Constants.catLevelOne, ruleInfo.catLevelOne.name)
    storeForTag(builder, databaseNode, Constants.catLevelTwo, ruleInfo.catLevelTwo)
  }

  private def findLinesContainingText(filePath: String, searchText: String): Seq[Int] = {
    import better.files.File as BFile
    val file = BFile(filePath)
    if (file.exists && file.isRegularFile) {
      val lines = file.lines.zipWithIndex.filter { case (line, index) =>
        line.contains(searchText)
      }
      lines.map(_._2 + 1).toList
    } else {
      Seq.empty[Int]
    }
  }

}
