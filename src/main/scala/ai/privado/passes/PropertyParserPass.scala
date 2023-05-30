package ai.privado.utility

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJavaProperty
import overflowdb.BatchedUpdate
import ai.privado.cache.RuleCache
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import java.io.File
import scala.io.Source
import java.util.Properties
import scala.jdk.CollectionConverters._
import io.circe.parser._
import io.circe._

import scala.collection.mutable
import com.typesafe.config._

import scala.xml.XML
import com.github.wnameless.json.flattener.JsonFlattener
import io.circe.yaml.parser
import ai.privado.model.Language
import ai.privado.tagger.PrivadoParallelCpgPass

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

class PropertyParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache, language: Language.Value)
    extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)

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
      case Language.JAVASCRIPT => configFiles(projectRoot, Set(FileExtensions.JSON)).toArray
      case Language.PYTHON =>
        configFiles(
          projectRoot,
          Set(FileExtensions.INI, FileExtensions.ENV, FileExtensions.YAML, FileExtensions.YML)
        ).toArray
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
      loadAndConvertYMLtoProperties(file).map(item => (item._1, item._2, -1))
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
      case Left(parsingError) => logger.debug(parsingError.toString)
    }

    keyValuePairs.map { case (key: String, value: Json) =>
      (key, value.toString)
    }.toList
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

  private def loadAndConvertYMLtoProperties(file: String): List[(String, String)] = {
    parser.parse(better.files.File(file).contentAsString) match {
      case Right(json) => {
        try {
          JsonFlattener
            .flattenAsMap(json.toString)
            .asScala
            .toList
            .map(p => {
              if (p._2 == null)
                ("", "")
              else
                (p._1, p._2.asInstanceOf[String])
            })
        } catch {
          case e: Throwable =>
            logger.trace(s"Error while creating properties node for file $file")
            logger.debug(s"Error while creating properties node for file : $file, error : ${e.getMessage}")
            List[("", "")]()
        }
      }
      case Left(error) => {
        List[("", "")]()
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
    val options        = ConfigParseOptions.defaults().setSyntax(ConfigSyntax.CONF).setOriginDescription(file)
    val config: Config = ConfigFactory.parseFile(new File(file), options)

    try {
      val propertyList = ListBuffer[(String, String, Int)]()

      def parseConfigNode(
        configNode: ConfigObject,
        itemList: ListBuffer[(String, String, Int)],
        parentKey: String = ""
      ): Unit = {
        configNode.forEach { case (key, configValue) =>
          val lineNumber = configValue.origin().lineNumber()
          if (configValue.isInstanceOf[ConfigObject]) {
            parseConfigNode(configValue.asInstanceOf[ConfigObject], itemList, parentKey + "." + key)
          } else {
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

    SourceFiles
      .determine(Set(projectRoot), extensions)
      .concat(
        getListOfFiles(projectRoot)
          .map(f => {
            f.getAbsolutePath
          })
          .filter(_.matches(".*\\.env(?!.*(?:.js|.py|.java|.sh|.ts)$).*"))
      )
      .filter(file => !file.contains("delombok"))
      .filter(file => Utilities.isFileProcessable(file, ruleCache) && (!file.matches(".*node_modules.*")))
      .distinct
  }

}
