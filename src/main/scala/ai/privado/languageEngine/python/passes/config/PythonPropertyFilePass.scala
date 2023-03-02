package ai.privado.languageEngine.python.passes.config

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, MethodParameterIn, NewFile, NewJavaProperty}
import io.shiftleft.passes.{ForkJoinParallelCpgPass, SimpleCpgPass}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._

import scala.jdk.CollectionConverters._
import java.util.Properties
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._
import io.circe.yaml.parser
import com.github.wnameless.json.flattener.JsonFlattener

import scala.io.Source

class PythonPropertyFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {
  override def generateParts(): Array[String] = propertiesFiles(projectRoot).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    println(propertiesFiles(projectRoot))
  }

  private def propertiesFiles(projectRoot: String): List[String] = {
    val sourceFiles = SourceFiles
      .determine(Set(projectRoot), Set(".ini", ".yml", ".yaml", ".env"))
      .filter(Utilities.isFileProcessable)

    for (file <- sourceFiles) {
      envParser(file)
    }

    sourceFiles
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def envParser(file: String): Unit = {
    val envProps = new Properties()
    Source
      .fromFile(file)
      .getLines
      .filter(line => line.trim.nonEmpty && !line.startsWith("#"))
      .foreach(line => {
        val Array(key, value) = line.split("=", 2)
        envProps.setProperty(key, value)
        println(cpg.method.l)
      })

    // Define a list of keywords that indicate database configurations
    // Update this for more coverage
    val dbKeywords = List("DATABASE", "DB_", "MYSQL", "POSTGRES", "SQLITE", "SQL")
    // Filter out properties that contain database configurations
    val dbProps = envProps.asScala.view.filterKeys(key => dbKeywords.exists(key.startsWith)).toMap
    println(dbProps)

  }

}
