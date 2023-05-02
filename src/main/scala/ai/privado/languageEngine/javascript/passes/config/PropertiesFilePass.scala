package ai.privado.languageEngine.javascript.passes.config

import ai.privado.cache.RuleCache
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, NewFile, NewJavaProperty}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

import java.io.File
import scala.io.Source
import java.util.Properties
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import io.circe.parser._
import io.circe._
import io.circe.generic.auto._

import scala.collection.mutable

class PropertiesFilePass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  // TODO: Modify the regex to make it more comprehensive
  val dbConnectionRegex =
    "^(db|database|jdbc|mysql|postgres|oracle|sqlserver)_(connection_)?(host|port|name|user|password|uri|driver|ssl|pool_size|timeout|connection_string)$"
  val apiConnectionRegex = ".*/(api|external)?(_|\\.)?(url|base(_|\\.)?path)/i"

  override def generateParts(): Array[_ <: AnyRef] =
    configFiles(projectRoot, Set(".json")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val propertyNodes = addPropertyNodesAndConnectToUsers(file, builder)
    println(matchProcessEnvAssignmentCalls("PORT"))
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
  }

  /** Returns a list of file paths that are config files, given a project root directory and a set of file extensions to
    * search for.
    * @param projectRoot
    *   the root directory of the project to search for config files in
    * @param extensions
    *   the set of file extensions to consider as config files
    * @return
    *   a list of file paths that are config files
    */
  private def configFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .concat(getListOfFiles(projectRoot).map(f => f.getAbsolutePath).filter(_.matches(".*\\.env.*")))
      .filter(Utilities.isFileProcessable(_, ruleCache))
  }

  private def addPropertyNodesAndConnectToUsers(
    file: String,
    builder: BatchedUpdate.DiffGraphBuilder
  ): List[NewJavaProperty] = {
    Try {
      obtainKeyValuePairs(file)
        .filter(pair => pair._1.nonEmpty && pair._2.nonEmpty)
    } match {
      case Success(keyValuePairs) =>
        val propertyNodes = keyValuePairs.map(addPropertyNode(_, builder))

        propertyNodes.foreach(node => {
          connectEnvCallsToProperties(node, builder)
        })

        propertyNodes
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  /** Finds all assignment calls that assign a value to a property of the `process.env` object with the given
    * `propertyName`.
    *
    * @param propertyName
    *   the name of the property to match (without the "process.env." prefix).
    * @return
    *   a list of Call nodes representing the matching assignment calls.
    */
  private def matchProcessEnvAssignmentCalls(propertyName: String): List[Call] = {
    // Match assignment calls on the right side for process.env.PROPERTY or process.env['PROPERTY']
    // Example const dbName = process.env['DB_NAME']
    val pattern = s".*process\\.env(\\.${propertyName}|\\[('|\")${propertyName}('|\")]).*"
    cpg.call("<operator>.assignment").where(_.astChildren.code(pattern)).l
  }

  private def connectEnvCallsToProperties(propertyNode: NewJavaProperty, builder: DiffGraphBuilder): Unit = {
    matchProcessEnvAssignmentCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  private def obtainKeyValuePairs(file: String): List[(String, String)] = {
    if (file.endsWith(".json"))
      getJSONKeyValuePairs(file)
    else
      getDotenvKeyValuePairs(file)
  }

  /** Parses a JSON file and returns a list of key-value pairs for properties related to database connections and API
    * endpoints.
    * @param file
    *   the path to the JSON file to parse
    * @return
    *   a list of key-value pairs where the keys match either the database connection or API endpoint naming conventions
    */
  private def getJSONKeyValuePairs(file: String): List[(String, String)] = {
    val json          = parse(Source.fromFile(file).mkString)
    val keyValuePairs = mutable.Map[String, Json]()

    // Recursively scan through the JSON to extract out all keys
    def traverseJSON(json: JsonObject, keyValues: mutable.Map[String, Json]): Unit = {
      json.toList.foreach { case (key, value) =>
        value.asObject match {
          case Some(jsonObj) =>
            // Nested object
            traverseJSON(jsonObj, keyValues)
          case None =>
            // Not nested, add to key-value map
            if (key.matches(dbConnectionRegex) || key.matches(apiConnectionRegex))
              keyValues += (key -> value)
        }
      }
    }

    json match {
      case Right(jsonObject) => {
        jsonObject.asObject match {
          case Some(value) => traverseJSON(value, keyValuePairs)
          case None        => logger.debug("")
        }
      }
      case Left(parsingError) => println(parsingError)
    }

    keyValuePairs.map { case (key: String, value: Json) =>
      (key, value.toString)
    }.toList
  }

  private def getDotenvKeyValuePairs(file: String): List[(String, String)] = {
    val envProps = new Properties()
    Source
      .fromFile(file)
      .getLines()
      .filter(line => line.trim.nonEmpty && !line.startsWith("#"))
      .foreach(line => {
        val Array(key, value) = line.split("=", 2)
        envProps.setProperty(key, value)
      })

    envProps.asScala
      .map(prop => (prop._1, prop._2))
      .toList
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def addPropertyNode(
    keyValuePair: (String, String),
    builder: BatchedUpdate.DiffGraphBuilder
  ): NewJavaProperty = {
    val (key, value) = keyValuePair
    val propertyNode = NewJavaProperty().name(key).value(value)
    builder.addNode(propertyNode)
    propertyNode
  }
}
