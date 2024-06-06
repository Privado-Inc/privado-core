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
import io.shiftleft.codepropertygraph.generated.nodes.{NewHightouchSink, NewJavaProperty}
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode}
import org.yaml.snakeyaml.constructor.SafeConstructor
import overflowdb.NodeOrDetachedNode

import java.io.StringReader
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object HightouchKeys {
  val NAME   = "name"
  val SOURCE = "source"
  val TYPE   = "type"

  val MODEL           = "model"
  val RAW_SQL         = "rawSql"
  val DESTINATION     = "destination"
  val SCHEDULE_PAUSED = "schedulePaused"

  val MODEL_SLUG = "model_slug"
}

case class Manifest(sources: Option[Map[String, Source]], destinations: Option[Map[String, Destination]])
case class Source(name: String, `type`: String)
case class Destination(name: String, `type`: String)

case class YamlProperty(key: String, value: String, lineNumber: Int)

class HighTouchPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val slugToActualDestinationMap: mutable.Map[String, String] = mutable.Map.empty[String, String]
  val logger                                                  = LoggerFactory.getLogger(getClass)

  val ALLOWED_KEYS_MODEL: Set[String] =
    Set(HightouchKeys.NAME, HightouchKeys.SOURCE, HightouchKeys.TYPE, HightouchKeys.RAW_SQL)
  val ALLOWED_KEYS_SYNC: Set[String] =
    Set(HightouchKeys.MODEL, HightouchKeys.DESTINATION, HightouchKeys.SCHEDULE_PAUSED, HightouchKeys.TYPE)

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
    val fileNode               = addFileNode(fileName, builder)
    val parseYamlWithHierarchy = parseYaml(fileName, true)
    // TODO: Remove this before final deployment -- Only for debugging
    parseYamlWithHierarchy.foreach(property => {
      val propertyNode = addPropertyNode(builder, property)
      builder.addEdge(propertyNode, fileNode, EdgeTypes.SOURCE_FILE)
    })

    val parserResult =
      parseYaml(fileName)
        .filter(p => (ALLOWED_KEYS_MODEL ++ ALLOWED_KEYS_SYNC).contains(p.key))
        .foldLeft(List.empty[YamlProperty]) { (acc: List[YamlProperty], curr: YamlProperty) =>
          acc :+ curr
        }

    val parserResultKeys = parserResult.map(_.key)
    if (parserResultKeys.forall(p => ALLOWED_KEYS_SYNC.contains(p))) {
      logger.debug(s"Sync file is parsed: ${fileName}")
      // Sync file
      addHighTouchSinkNode(builder, parserResult, fileNode)
    } else if (parserResultKeys.forall(p => ALLOWED_KEYS_MODEL.contains(p))) {
      logger.debug(s"Model file is parsed: ${fileName}")
      parserResult :+ YamlProperty(HightouchKeys.MODEL_SLUG, fileName, -1)
      val rawSqlNode = parserResult.find(_.key == HightouchKeys.RAW_SQL).getOrElse(YamlProperty("", "", -1))
      val patterns   = List("date", "string", "boolean", "boolean,", "date", "string")
      val regex      = patterns.map(word => s":: *${word}").mkString("|").r.pattern
      val filteredSql =
        regex
          .matcher(rawSqlNode.value)
          .replaceAll("")
          .replaceAll("::", ".")
          .replaceAll(":", ".") // As these result in invalid token
          .replaceAll(
            "(?i)final",
            "myFinal"
          ) // FINAL keyword is reserved keyword resulting into unexpected token in this case
          .trim
          .split("\n")
          .filter(p => !p.startsWith("--"))
          .mkString("\n")
          .replaceAll("\n\n\n", "")
      parseQueryAndCreateNodes(builder, filteredSql, fileNode, rawSqlNode.lineNumber, Some(fileName))
    } else {
      logger.debug(s"Could not parse file:  ${fileName}")
    }

  }

  private def createManifestMap(manifestFileName: String): Unit = {
    val manifestParsed = yaml.parser
      .parse(File(manifestFileName).contentAsString)
      .map(_.as[Manifest])
      .fold(
        l => Manifest(Some(Map.empty[String, Source]), Some(Map.empty[String, Destination])),
        r => r.getOrElse(Manifest(Some(Map.empty[String, Source]), Some(Map.empty[String, Destination])))
      )
    slugToActualDestinationMap.addAll(
      manifestParsed.destinations
        .getOrElse(Map.empty[String, Destination])
        .map(p => {
          (p._1, p._2.`type`)
        })
    )
  }

  private def parseYaml(file: String, preserveHierarchy: Boolean = false): List[YamlProperty] = {
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
              val fullPath  = if (path.isEmpty) keyNode.getValue else s"$path.${keyNode.getValue}"
              if (preserveHierarchy) processNode(valueNode, fullPath) else processNode(valueNode, keyNode.getValue)
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
        logger.debug(s"Could not parse YAML file. Please double check the syntax. ${file}")
        logger.debug(
          better.files
            .File(file)
            .contentAsString
        )
        List[YamlProperty]()
      }
    }
  }

  private def addPropertyNode(builder: DiffGraphBuilder, yamlProperty: YamlProperty): NewJavaProperty = {
    val YamlProperty(key: String, value: String, lineNumber: Int) = yamlProperty
    val propertyNode = NewJavaProperty().name(key).value(value).lineNumber(lineNumber)
    builder.addNode(propertyNode)
    propertyNode
  }

  private def addHighTouchSinkNode(
    builder: DiffGraphBuilder,
    parserResult: List[YamlProperty],
    fileNode: NodeOrDetachedNode
  ): Unit = {
    val destinationNode = parserResult.find(_.key.equals(HightouchKeys.DESTINATION)).getOrElse(YamlProperty("", "", -1))
    val correspondingModel =
      parserResult.find(_.key.equals(HightouchKeys.MODEL)).getOrElse(YamlProperty("", "", -1)).value
    val schedulePaused =
      parserResult
        .find(_.key.equals(HightouchKeys.SCHEDULE_PAUSED))
        .getOrElse(YamlProperty(HightouchKeys.SCHEDULE_PAUSED, "false", -1))
    val hightouchSink =
      NewHightouchSink()
        .name(destinationNode.value)
        .lineNumber(destinationNode.lineNumber)
        .correspondingModel(correspondingModel)
        .actualDestinationName(slugToActualDestinationMap.getOrElse(destinationNode.value, destinationNode.value))
        .schedulePaused(schedulePaused.value.toBoolean)
        .code(destinationNode.value)
    builder.addNode(hightouchSink)
    builder.addEdge(hightouchSink, fileNode, EdgeTypes.SOURCE_FILE)
  }
}
