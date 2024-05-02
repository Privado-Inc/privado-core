package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.default.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLNodeBuilder.parseQueryAndCreateNodes
import better.files.File
import better.files.File.VisitOptions
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.circe.*
import io.circe.generic.auto.*
import ai.privado.utility.Utilities.{addFileNode, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.NewHightouchSink
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode}
import org.yaml.snakeyaml.constructor.SafeConstructor

import java.io.StringReader
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class Model(
  `model-slug`: Option[String],
  name: Option[String],
  source: String,
  `type`: String,
  rawSql: Option[String]
)
case class Sync(model: String, destination: String, schedulePaused: Boolean)
case class Manifest(sources: Map[String, Source], destinations: Map[String, Destination])
case class Source(name: String, `type`: String)
case class Destination(name: String, `type`: String)
case class EmptySchema()

case class YamlProperty(key: String, value: String, lineNumber: Int)

class HighTouchPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val slugToActualDestinationMap: mutable.Map[String, String] = mutable.Map.empty[String, String]
  val logger                                                  = LoggerFactory.getLogger(getClass)

  val ALLOWED_KEYS_MODEL: Set[String] =
    Set("name", "source", "type", "rawSql")
  val ALLOWED_KEYS_SYNC: Set[String] = Set("model", "destination", "schedulePaused", "type")

  override def generateParts(): Array[String] = {

    val manifestFiles = SourceFiles
      .determine(projectRoot, Set(".yaml", ".yml"), ignoredFilesRegex = Some(".*[.]privado.*".r))(VisitOptions.default)
      .filter(_.contains("manifest"))

    manifestFiles.foreach(createManifestMap)
    logger.debug(s"Number of manifest files: ${manifestFiles.size}")

    SourceFiles
      .determine(projectRoot, Set(".yaml", ".yml"), ignoredFilesRegex = Some(".*[.](privado|manifest).*".r))(
        VisitOptions.default
      )
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, fileName: String): Unit = {
    val fileNode = addFileNode(fileName, builder)
    val parserResult =
      parseYaml(fileName)
        .filter(p => (ALLOWED_KEYS_MODEL ++ ALLOWED_KEYS_SYNC).contains(p.key))
        .foldLeft(List.empty[YamlProperty]) { (acc: List[YamlProperty], curr: YamlProperty) =>
          acc :+ curr
        }

    val parserResultKeys = parserResult.map(_.key)
    if (parserResultKeys.forall(p => ALLOWED_KEYS_SYNC.contains(p))) {
      // Sync file
      val destinationNode    = parserResult.find(_.key.equals("destination")).getOrElse(YamlProperty("", "", -1))
      val correspondingModel = parserResult.find(_.key.equals("model")).getOrElse(YamlProperty("", "", -1)).value
      val hightouchSink =
        NewHightouchSink()
          .name(destinationNode.value)
          .lineNumber(destinationNode.lineNumber)
          .correspondingModel(correspondingModel)
          .actualDestinationName(slugToActualDestinationMap.getOrElse(destinationNode.value, destinationNode.value))
      builder.addNode(hightouchSink)
      builder.addEdge(hightouchSink, fileNode, EdgeTypes.SOURCE_FILE)
    } else if (parserResultKeys.forall(p => ALLOWED_KEYS_MODEL.contains(p))) {
      parserResult :+ YamlProperty("model-slug", fileName, -1)
      val rawSqlNode = parserResult.find(_.key == "rawSql").getOrElse(YamlProperty("", "", -1))
      parseQueryAndCreateNodes(builder, rawSqlNode.value, fileNode, rawSqlNode.lineNumber)
    }

  }

  private def createManifestMap(manifestFileName: String): Unit = {
    val manifestParsed = yaml.parser
      .parse(File(manifestFileName).contentAsString)
      .map(_.as[Manifest])
      .fold(
        l => Manifest(Map.empty[String, Source], Map.empty[String, Destination]),
        r => r.getOrElse(Manifest(Map.empty[String, Source], Map.empty[String, Destination]))
      )
    slugToActualDestinationMap.addAll(manifestParsed.destinations.map(p => (p._1, p._2.`type`)))
  }

  private def parseYaml(file: String): List[YamlProperty] = {
    try {
      val yamlContent = better.files
        .File(file)
        .contentAsString

      val yaml                       = new Yaml(new SafeConstructor(LoaderOptions()))
      val rootNode                   = yaml.compose(new StringReader(yamlContent))
      var result: List[YamlProperty] = List[YamlProperty]()
      processNode(rootNode, "")

      def processNode(node: Node, path: String): Unit = {
        node match {
          case mappingNode: MappingNode =>
            mappingNode.getValue.asScala.foreach { (nodeTuple: NodeTuple) =>
              val keyNode   = nodeTuple.getKeyNode.asInstanceOf[ScalarNode]
              val valueNode = nodeTuple.getValueNode
              processNode(valueNode, keyNode.getValue)
            }
          case sequenceNode: SequenceNode =>
            sequenceNode.getValue.asScala.zipWithIndex.foreach { case (valueNode, index) =>
              val _path = s"$path[$index]"
              processNode(valueNode, _path)
            }
          case scalarNode: ScalarNode =>
            val line   = scalarNode.getStartMark.getLine + 1
            val column = scalarNode.getStartMark.getColumn + 1
            val value  = scalarNode.getValue
            result = result.appended(YamlProperty(path, value, line))
        }
      }

      result
    } catch {
      case e: Throwable => {
        logger.debug(s"Could not parse YAML file. Please double check the syntax. ${e.getMessage}")
        List[YamlProperty]()
      }
    }
  }
}
