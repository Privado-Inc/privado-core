package ai.privado.utility

import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.composer.Composer
import org.yaml.snakeyaml.{DumperOptions, LoaderOptions, Yaml}
import org.yaml.snakeyaml.constructor.SafeConstructor
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode}
import org.yaml.snakeyaml.parser.ParserImpl
import org.yaml.snakeyaml.representer.Representer
import org.yaml.snakeyaml.resolver.Resolver

import java.io.StringReader
import scala.jdk.CollectionConverters.*

object ConfigParserUtility {

  val PLACEHOLDER_TOKEN_START_END = "@@"
  val logger                      = LoggerFactory.getLogger(getClass)

  def parseYaml(file: String): List[(String, String, Int)] = {
    try {
      val yamlContent = better.files
        .File(file)
        .contentAsString
        .replaceAll(PLACEHOLDER_TOKEN_START_END, "")
        .replaceAll("<%=(.*?)%>", "") // replace erb strings

      val loaderOptions = new LoaderOptions()
      loaderOptions.setMaxAliasesForCollections(100) // default is 50 causing parsing failure
      val dumperOptions = new DumperOptions
      val yaml =
        new Yaml(new SafeConstructor(loaderOptions), new Representer(dumperOptions), dumperOptions, loaderOptions)
      val rootNode                            = yaml.compose(new StringReader(yamlContent))
      var result: List[(String, String, Int)] = List[(String, String, Int)]()
      processNode(rootNode, "")

      def processNode(node: Node, path: String): Unit = {
        node match {
          case mappingNode: MappingNode =>
            mappingNode.getValue.asScala.foreach { (nodeTuple: NodeTuple) =>
              val keyNode   = nodeTuple.getKeyNode.asInstanceOf[ScalarNode]
              val valueNode = nodeTuple.getValueNode
              val fullPath  = if (path.isEmpty) keyNode.getValue else s"$path.${keyNode.getValue}"
              processNode(valueNode, fullPath)
            }
          case sequenceNode: SequenceNode =>
            sequenceNode.getValue.asScala.zipWithIndex.foreach { case (valueNode, index) =>
              val fullPath = s"$path[$index]"
              processNode(valueNode, fullPath)
            }
          case scalarNode: ScalarNode =>
            val line   = scalarNode.getStartMark.getLine + 1
            val column = scalarNode.getStartMark.getColumn + 1
            val value  = scalarNode.getValue
            result = result.appended((path, value, line))
          case _ =>
        }
      }

      result
    } catch {
      case e: Throwable => {
        logger.debug(s"Could not parse YAML file. Please double check the syntax. ${e.getMessage}")
        List[(String, String, Int)]()
      }
    }
  }
}
