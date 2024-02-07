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

import scala.collection.mutable
import scala.io.Source
import scala.util.Using
class JsonPropertyParserPass(cpg: Cpg, projectRoot: String) extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[String] = {

    val files = SourceFiles
      .determine(projectRoot, Set(".properties", ".yaml", ".yml", ".xml", ".json", ".ini", ".env", ".conf"))
    files.toArray
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
    val json          = parse(File(file).contentAsString)
    val keyValuePairs = mutable.Map[String, Json]()

    // Recursively scan through the JSON to extract out all keys
    def traverseJSON(json: JsonObject, keyValues: mutable.Map[String, Json]): Unit = {
      json.toList.foreach { case (key, value) =>
        value.asObject match {
          case Some(jsonObj) =>
            // Nested object
            traverseJSON(jsonObj, keyValues)
          case None =>
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
      case Left(parsingError) => logger.debug(parsingError.toString)
    }

    keyValuePairs.map { case (key: String, value: Json) =>
      (key, value.toString)
    }.toList
  }
}
