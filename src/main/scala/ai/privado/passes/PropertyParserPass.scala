package ai.privado.passes

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJavaProperty
import overflowdb.BatchedUpdate
import ai.privado.cache.{PropertyFilterCache, RuleCache}
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
import ai.privado.utility.{ConfigParserUtility, Utilities}
import org.yaml.snakeyaml.constructor.SafeConstructor
import better.files.File.VisitOptions

import java.nio.file.Path
import scala.collection.mutable.ListBuffer

object PropertyFileExtensions {
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
  propertyFilterCache: PropertyFilterCache = PropertyFilterCache(),
  privadoInput: PrivadoInput = PrivadoInput()
) extends PrivadoParallelCpgPass[String](cpg)
    with Utility {
  val PLACEHOLDER_TOKEN_START_END = "@@"
  val logger                      = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] = {
    language match {
      case Language.JAVA => {
        configFiles(
          projectRoot,
          Set(
            PropertyFileExtensions.PROPERTIES,
            PropertyFileExtensions.YAML,
            PropertyFileExtensions.YML,
            PropertyFileExtensions.XML,
            PropertyFileExtensions.CONF
          )
        ).toArray
      }
      case Language.JAVASCRIPT =>
        if (privadoInput.enableIngressAndEgressUrls) {
          configFiles(
            projectRoot,
            Set(
              PropertyFileExtensions.JSON,
              PropertyFileExtensions.ENV,
              PropertyFileExtensions.YAML,
              PropertyFileExtensions.YML
            )
          ).toArray
        } else {
          configFiles(projectRoot, Set(PropertyFileExtensions.JSON, PropertyFileExtensions.ENV)).toArray
        }
      case Language.PYTHON =>
        configFiles(
          projectRoot,
          Set(
            PropertyFileExtensions.INI,
            PropertyFileExtensions.ENV,
            PropertyFileExtensions.YAML,
            PropertyFileExtensions.YML
          )
        ).toArray
      case Language.RUBY =>
        (configFiles(projectRoot, Set(PropertyFileExtensions.ENV)) ++ configFiles(
          projectRoot,
          Set(PropertyFileExtensions.YML, PropertyFileExtensions.YAML)
        ).filter(_.matches(".*(settings|config).*"))).toArray
      // Ruby has a lot of yaml files so creating property nodes for all of them, exposes a lot of property nodes,
      // which are incorrect, so we go by the approach of being selective and creating property nodes for only the impacted files
      case Language.GO =>
        (configFiles(projectRoot, Set(PropertyFileExtensions.YAML, PropertyFileExtensions.YML))).toArray
    }
  }

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder, projectRoot)
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
      ConfigParserUtility.parseYaml(file)
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
      if (fileSizeInKiloByte > fileLimit.toInt) {
        // Adding the filtered file into the property cache
        propertyFilterCache.addIntoFileSkippedBySize(file.getAbsolutePath, fileSizeInKiloByte.toInt)
        false
      } else {
        true
      }
    } else {
      true
    }
  }

  private def filterFilesWithDir(filePaths: List[String], ruleCache: RuleCache): List[String] = {
    val countLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileDirCountLimit, true)
    if (countLimit.nonEmpty) {
      val groupedByDirectory = filePaths.groupBy(filePath => new File(filePath).getParent)
      val filteredDirectories = groupedByDirectory.filter { case (path, filesInDirectory) =>
        if (filesInDirectory.length > countLimit.toInt) {
          // Adding the filtered files into the property cache
          propertyFilterCache.addIntoFileSkippedByDirCount(path, filesInDirectory)
          false
        } else {
          true
        }
      }
      filteredDirectories.values.flatten.toList
    } else {
      filePaths
    }
  }
}
