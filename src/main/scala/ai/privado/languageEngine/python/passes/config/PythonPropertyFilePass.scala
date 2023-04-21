package ai.privado.languageEngine.python.passes.config

import ai.privado.cache.RuleCache
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, Member, NewFile, NewJavaProperty}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.jdk.CollectionConverters._
import java.util.Properties
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._
import com.typesafe.config._

import java.io.File
import scala.io.Source

class PythonPropertyFilePass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends ForkJoinParallelCpgPass[String](cpg) {
  override def generateParts(): Array[String] = {
    configFiles(projectRoot, Set(".ini", ".env", ".conf")).toArray
  }

  private val logger = LoggerFactory.getLogger(getClass)

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val propertyNodes = addPropertyNodesAndConnectToUsers(file, builder)
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
  }

  private def addPropertyNodesAndConnectToUsers(
    file: String,
    builder: BatchedUpdate.DiffGraphBuilder
  ): List[NewJavaProperty] = {
    Try {
      obtainKeyValuePairs(file)
        .concat(getConnectionMethodPairs(resolveConnectionMethodCalls()))
        .filter(pair => pair._1.nonEmpty && pair._2.nonEmpty)
    } match {
      case Success(keyValuePairs) =>
        val propertyNodes = keyValuePairs.map(addPropertyNode(_, builder))

        propertyNodes.foreach(propertyNode => {
          connectGetEnvironLiterals(propertyNode, builder)
          connectDBConfigMembers(propertyNode, builder)
        })

        propertyNodes
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  private def obtainKeyValuePairs(file: String): List[(String, String)] = {
    if (file.endsWith(".ini")) {
      parseINIFiles(file)
    } else {
      getDotenvKeyValuePairs(file)
    }
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

  /** Resolves methods used to connect to databases using some popular python modules
    */
  private def resolveConnectionMethodCalls(): List[Literal] = {
    cpg.literal
      .where(_.inCall.methodFullName("sqlalchemy.*create_engine.*"))
      .l
      .concat(cpg.literal.where(_.inCall.methodFullName("pyodbc.*.*connect.*")).l)
      .concat(cpg.literal.where(_.inCall.methodFullName("pyscopg2.*.*connect.*")).l)
      .concat(cpg.literal.where(_.inCall.methodFullName(".*MongoClient.*")).l)
  }

  // The parameters passed can only be database urls, so property name is db_url for all the method calls as there is no key
  private def getConnectionMethodPairs(connectionValues: List[Literal]): List[(String, String)] =
    connectionValues.map(lit => ("db_url", lit.code))

  /** Matches the exact key of the propertyNode to its corresponding os.environ.get() calls.
    */
  private def matchEnvironGetCalls(propertyName: String): List[Literal] = {
    cpg.literal
      .codeExact("\"" + propertyName + "\"")
      .where(_.inCall.methodFullName(".*\\(?environ\\)?\\.get"))
      .l
  }

  /** Gets all files to be processed as property files.
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

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  /** This method works for a specific implementation where a datbabase configuration class is created and configs are
    * exported from that class.
    */
  private def matchDBConfigCalls(propertyNode: String): List[Member] = {
    if (propertyNode.matches("(?i).*host.*")) {
      cpg.member("host").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else if (
      propertyNode.matches("(?i).*(url|uri).*") && (propertyNode.contains(".") || propertyNode.contains("__"))
    ) {
      cpg.member("url").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else if (propertyNode.matches("(?i).*(database|db).*")) {
      cpg.member("database").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else if (propertyNode.matches("(?i).*(port).*")) {
      cpg.member("port").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else if (propertyNode.matches("(?i).*(pass)word?.*")) {
      cpg.member("password").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else if (propertyNode.matches("(?i).*(user)name?.*")) {
      cpg.member("username").where(_.typeDecl.fullName(".*DatabaseConfiguration.*")).l
    } else {
      List[Member]()
    }
  }

  private def parseINIFiles(filePath: String): List[(String, String)] = {
    val fileContent = Source.fromFile(filePath).getLines().mkString("\n")
    val iniFormat   = ConfigParseOptions.defaults().setSyntax(ConfigSyntax.PROPERTIES)

    getAllProperties(ConfigFactory.parseString(fileContent, iniFormat))
  }

  private def getAllProperties(config: Config): List[(String, String)] = {
    val entries = config.entrySet().asScala.toList
    entries.map(entry => (entry.getKey, entry.getValue.unwrapped.toString))
  }

  /** Create an edge between the literals in the environ.get calls and the property nodes.
    */
  private def connectGetEnvironLiterals(
    propertyNode: NewJavaProperty,
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
    matchEnvironGetCalls(propertyNode.name.strip()).foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  /** Create an edge between the literals in the db config members and the property nodes.
    */
  private def connectDBConfigMembers(propertyNode: NewJavaProperty, builder: DiffGraphBuilder): Unit = {
    matchDBConfigCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
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
