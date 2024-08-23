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
class JsonPropertyParserPass(cpg: Cpg, projectRoot: String)
    extends PrivadoParallelCpgPass[String](cpg)
    with JsonParser {

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
}
