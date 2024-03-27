package ai.privado.utility

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJavaProperty
import overflowdb.BatchedUpdate
import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

import java.io.{File, StringReader}
import scala.io.Source
import java.util.Properties
import scala.jdk.CollectionConverters.*
import io.circe.parser.*
import io.circe.*

import scala.collection.mutable
import com.typesafe.config.*

import scala.xml.XML
import com.github.wnameless.json.flattener.JsonFlattener
import io.circe.yaml.parser
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode}

import scala.jdk.CollectionConverters.*
import ai.privado.model.{Constants, Language}
import ai.privado.tagger.PrivadoParallelCpgPass
import org.yaml.snakeyaml.constructor.SafeConstructor
import better.files.File.VisitOptions

import scala.collection.mutable.ListBuffer

object FileExtensions {
  val PROPERTIES = ".properties"
  val YAML       = ".yaml"
  val YML        = ".yml"
  val XML        = ".xml"
  val JSON       = ".json"
  val INI        = ".ini"
  val ENV        = ".env"
  val CONF       = ".conf"
}

class PropertyParserPass(
  cpg: Cpg,
  projectRoot: String,
  ruleCache: RuleCache,
  language: Language.Value,
  privadoInput: PrivadoInput = PrivadoInput()
) extends PrivadoParallelCpgPass[String](cpg) {
  val PLACEHOLDER_TOKEN_START_END = "@@"
  val logger                      = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] = {
    language match {
      case Language.JAVA => {
        configFiles(
          projectRoot,
          Set(
            FileExtensions.PROPERTIES,
            FileExtensions.YAML,
            FileExtensions.YML,
            FileExtensions.XML,
            FileExtensions.CONF
          )
        ).toArray
      }
      case Language.JAVASCRIPT =>
        if (privadoInput.enableIngressAndEgressUrls) {
          configFiles(
            projectRoot,
            Set(FileExtensions.JSON, FileExtensions.ENV, FileExtensions.YAML, FileExtensions.YML)
          ).toArray
        } else {
          configFiles(projectRoot, Set(FileExtensions.JSON, FileExtensions.ENV)).toArray
        }
      case Language.PYTHON =>
        configFiles(
          projectRoot,
          Set(FileExtensions.INI, FileExtensions.ENV, FileExtensions.YAML, FileExtensions.YML)
        ).toArray
      case Language.RUBY =>
        (configFiles(projectRoot, Set(FileExtensions.ENV)) ++ configFiles(
          projectRoot,
          Set(FileExtensions.YML, FileExtensions.YAML)
        ).filter(_.matches(".*(settings|config).*"))).toArray
      // Ruby has a lot of yaml files so creating property nodes for all of them, exposes a lot of property nodes,
      // which are incorrect, so we go by the approach of being selective and creating property nodes for only the impacted files
      case Language.GO =>
        (configFiles(projectRoot, Set(FileExtensions.YAML, FileExtensions.YML))).toArray
    }
  }

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val propertyNodes = obtainKeyValuePairs(file, builder).map(pair => addPropertyNode(pair, builder))
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
  }

  // TODO: Modify the regex to make it more comprehensive
  private val dbConnectionRegex =
    "^(db|database|jdbc|mysql|postgres|oracle|sqlserver)_(connection_)?(host|port|name|user|password|uri|driver|ssl|pool_size|timeout|connection_string)$"
  private val apiConnectionRegex = ".*/(api|external)?(_|\\.)?(url|base(_|\\.)?path)/i"

  private def obtainKeyValuePairs(file: String, builder: DiffGraphBuilder): List[(String, String, Int)] = {
    // Function return (key, value, lineNumber), for most parser we have not got the linenumber so returning -1 as default
    if (file.matches(""".*\.(?:yml|yaml)""")) {
      // the Yaml parser returns a line number
      loadAndConvertYMLtoProperties(file)
    } else if (file.endsWith(".xml")) {
      loadAndConvertXMLtoProperties(file, builder).map(item => (item._1, item._2, -1))
    } else if (file.endsWith(".ini")) {
      parseINIFiles(file).map(item => (item._1, item._2, -1))
    } else if (file.matches(".*\\.env(?!.*(?:.js|.py|.java|.sh|.ts)$).*")) {
      getDotenvKeyValuePairs(file).map(item => (item._1, item._2, -1))
    } else if (file.endsWith(".json")) {
      getJSONKeyValuePairs(file).map(item => (item._1, item._2, -1))
    } else if (file.endsWith(".conf")) {
      confFileParser(file)
    } else {
      loadFromProperties(file).map(item => (item._1, item._2, -1))
    }
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

  private def getDotenvKeyValuePairs(file: String): List[(String, String)] = {
    val envProps = new Properties()
    try {
      Source
        .fromFile(file)
        .getLines()
        .filter(line => line.trim.nonEmpty && !line.startsWith("#"))
        .foreach(line => {
          try {
            val Array(key, value) = line.split("=", 2)
            envProps.setProperty(key, value)
          } catch {
            case e: Throwable =>
              logger.debug(s"Error splitting the required line. ${e.toString}")
          }
        })
    } catch {
      case e: Throwable => logger.debug("Input is not in the correct format")
    }

    envProps.asScala
      .map(prop => (prop._1, prop._2))
      .toList
  }

  private def loadAndConvertYMLtoProperties(file: String): List[(String, String, Int)] = {
    try {
      val yamlContent = better.files
        .File(file)
        .contentAsString
        .replaceAll(PLACEHOLDER_TOKEN_START_END, "") // Read the YAML file content as a string

      val yaml                                = new Yaml(new SafeConstructor(LoaderOptions()))
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

  private def propertiesToKeyValuePairs(properties: Properties): List[(String, String)] = {
    properties
      .propertyNames()
      .asScala
      .collect { case key: String =>
        (key, properties.getProperty(key))
      }
      .toList
  }

  private def loadFromProperties(file: String): List[(String, String)] = {
    val properties  = new Properties()
    val inputStream = better.files.File(file).newFileInputStream
    properties.load(inputStream)
    inputStream.close()
    propertiesToKeyValuePairs(properties)
  }

  private def confFileParser(file: String): List[(String, String, Int)] = {
    try {
      val options        = ConfigParseOptions.defaults().setSyntax(ConfigSyntax.CONF).setOriginDescription(file)
      val config: Config = ConfigFactory.parseFile(new File(file), options)
      val propertyList   = ListBuffer[(String, String, Int)]()

      def parseConfigNode(
        configNode: ConfigObject,
        itemList: ListBuffer[(String, String, Int)],
        parentKey: String = ""
      ): Unit = {
        configNode.forEach { case (key, configValue) =>
          val lineNumber = configValue.origin().lineNumber()
          configValue match {
            case configObject: ConfigObject =>
              parseConfigNode(configObject, itemList, parentKey + "." + key)
            case _ =>
              val value   = configValue.unwrapped().toString
              val itemKey = (parentKey + "." + key).stripPrefix(".")
              itemList.addOne((itemKey, value, lineNumber))
          }
        }
      }

      parseConfigNode(config.root(), propertyList, "")
      propertyList.toList
    } catch {
      case e: Exception =>
        logger.debug(s"Error parsing pipeline config file: ${e.getMessage}")
        List[("", "", -1)]()
    }
  }

  // Used to extract (name, value) pairs from a bean config file
  private def XMLParserBean(xmlPath: String, builder: DiffGraphBuilder): List[(String, String)] = {
    try {
      val xml = XML.loadFile(xmlPath)
      val nameValuePairs = (xml \\ "bean").flatMap { bean =>
        {
          var result: (String, String) = ("", "")
          val className: String        = bean \@ "class"
          (bean \\ "property").map { prop =>
            {
              // Search for property tags inside a bean
              val propValue = prop \@ "value"
              if (propValue.startsWith("$") && propValue.endsWith("}")) {
                val value = resolvePlaceholderValuesXML(
                  propValue.substring(2, propValue.length - 1)
                ) // Pass placeholder name without ${ and }
                if (value.nonEmpty) {
                  result = ((prop \@ "name"), value)
                }
              } else {
                result = ((prop \@ "name"), propValue)
              }

            }

            val members = getMember((prop \@ "name"), className);
            if (members.nonEmpty) {
              val propertyNode = NewJavaProperty().name(result._1).value(result._2)
              val member       = members.head
              if (member != null) {
                builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
                builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY);
              }
            }
            result
          }
        }
      }

      return nameValuePairs.toList
        .collect { case (name, value) => if (value.nonEmpty) (name, value) else ("", "") }
        .filter { case (name, value) =>
          name.nonEmpty && value.nonEmpty // Filter out name, value pairs which could not be resolved
        }
    } catch {
      case e: Throwable => logger.debug(e.toString)
    }

    List[("", "")]()

  }

  private def getMember(member: String, className: String) =
    cpg.member.where(_.typeDecl.fullName(className)).where(_.name(member)).toList

  private def loadAndConvertXMLtoProperties(file: String, builder: DiffGraphBuilder): List[(String, String)] = {
    val properties  = new Properties();
    val inputStream = better.files.File(file).newInputStream
    try {
      properties.loadFromXML(inputStream)
      properties.propertyNames.asScala.toList
        .collect(p => (p.toString, properties.getProperty(p.toString)))
    } catch {
      case _: Throwable => {
        XMLParserBean(file, builder)
      }
    }
  }

  private def resolvePlaceholderValuesXML(placeholder: String): String = {
    val propertyFiles: List[String] = configFiles(projectRoot, Set(".properties"))
    propertyFiles.foreach(file => {
      // Search across properties to find the required
      loadFromProperties(file).foreach(propertyValue => {
        val (name, value) = propertyValue;
        if (name.equals(placeholder)) return value;
      })
    })
    ""
  }

  /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
    */

  private def getAllProperties(config: Config): List[(String, String)] = {
    val entries = config.entrySet().asScala.toList
    entries.map(entry => (entry.getKey, entry.getValue.unwrapped.toString))
  }

  private def parseINIFiles(filePath: String): List[(String, String)] = {
    val sourceFile = Source.fromFile(filePath)
    val fileContent =
      try sourceFile.getLines().mkString("\n")
      finally sourceFile.close()
    val iniFormat = ConfigParseOptions.defaults().setSyntax(ConfigSyntax.PROPERTIES)

    getAllProperties(ConfigFactory.parseString(fileContent, iniFormat))
  }

  private def addPropertyNode(
    keyValuePair: (String, String, Int),
    builder: BatchedUpdate.DiffGraphBuilder
  ): NewJavaProperty = {
    val (key, value, lineNumber) = keyValuePair
    val propertyNode             = NewJavaProperty().name(key).value(value).lineNumber(lineNumber)
    builder.addNode(propertyNode)
    propertyNode
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def configFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }

    val propertyFiles = SourceFiles
      .determine(projectRoot, extensions, ignoredFilesRegex = Some(".*[.]privado.*".r))(VisitOptions.default)
      .concat(
        getListOfFiles(projectRoot)
          .map(f => {
            f.getAbsolutePath
          })
          .filter(_.matches(".*\\.env(?!.*(?:.js|.py|.java|.sh|.ts)$).*"))
      )
      .filter(file => !file.contains("delombok"))
      .filter(file => Utilities.isFileProcessable(file, ruleCache) && (!file.matches(".*node_modules.*")))
      .filter(file => filterUsingFileSize(file, ruleCache))
      .distinct

    filterFilesWithDir(propertyFiles, ruleCache)
  }

  private def filterUsingFileSize(filePath: String, ruleCache: RuleCache): Boolean = {
    val fileLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileSizeLimit, true)
    if (fileLimit.nonEmpty) {
      val file               = new File(filePath)
      val fileSizeInKiloByte = file.length() / 1024 // Get the size in KB
      fileSizeInKiloByte <= fileLimit.toInt
    } else {
      true
    }
  }

  private def filterFilesWithDir(filePaths: List[String], ruleCache: RuleCache): List[String] = {
    val countLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileDirCountLimit, true)
    if (countLimit.nonEmpty) {
      val groupedByDirectory = filePaths.groupBy(filePath => new File(filePath).getParent)
      val filteredDirectories = groupedByDirectory.filter { case (_, filesInDirectory) =>
        filesInDirectory.length <= countLimit.toInt
      }
      filteredDirectories.values.flatten.toList
    } else {
      filePaths
    }
  }
}
