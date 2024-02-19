package ai.privado.passes

import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.addFileNode
import better.files.File.VisitOptions
import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.NewJavaProperty
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import better.files.File

import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}
class JsonPropertyParserPass(cpg: Cpg, projectRoot: String) extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[String] = {

    val files = Try(File(projectRoot).listRecursively.filter(_.isRegularFile).map(_.path.toString).toArray).toOption
      .getOrElse(Array[String]())
    files
  }

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val propertyNodes = getJSONKeyValuePairs(file).map(pair => addPropertyNode(pair, builder))
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
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

  /** Parses a JSON file and returns a list of key-value pairs for properties related to database connections and API
    * endpoints.
    *
    * @param file
    *   the path to the JSON file to parse
    * @return
    *   a list of key-value pairs where the keys match either the database connection or API endpoint naming conventions
    */
  private def getJSONKeyValuePairs(file: String): List[(String, String)] = {
    import better.files.File
    val json = parse(File(file).contentAsString)

    // Recursively scan through the JSON to extract out all keys
    def extractKeyValuePairs(json: Json, prefix: String = ""): List[(String, String)] = {
      json match {
        case obj if obj.isObject =>
          obj.asObject.get.toMap.toList.flatMap { case (key, value) =>
            val newPrefix = if (prefix.isEmpty) key else s"$prefix.$key"
            extractKeyValuePairs(value, newPrefix)
          }
        case arr if arr.isArray =>
          arr.asArray.get.toList.zipWithIndex.flatMap { case (value, index) =>
            val newPrefix = s"$prefix[$index]"
            extractKeyValuePairs(value, newPrefix)
          }
        case other =>
          List((prefix, other.asString.getOrElse(other.toString)))
      }
    }

    val keyValuePairs = json match {
      case Right(jsonObject) => extractKeyValuePairs(jsonObject)
      case Left(parsingError) =>
        logger.debug(parsingError.toString)
        List.empty
    }

    keyValuePairs
  }
}
