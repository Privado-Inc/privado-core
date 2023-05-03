package ai.privado.languageEngine.javascript.passes.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, JavaProperty, NewFile, NewJavaProperty}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import java.io.File

class PropertiesFilePass(cpg: Cpg) extends ForkJoinParallelCpgPass[JavaProperty](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  // TODO: Modify the regex to make it more comprehensive
  val dbConnectionRegex =
    "^(db|database|jdbc|mysql|postgres|oracle|sqlserver)_(connection_)?(host|port|name|user|password|uri|driver|ssl|pool_size|timeout|connection_string)$"
  val apiConnectionRegex = ".*/(api|external)?(_|\\.)?(url|base(_|\\.)?path)/i"

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.l.toArray.filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperties(property, builder)
  }

  private def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    connectEnvCallsToProperties(property, builder)
  }

  /** Finds all assignment calls that assign a value to a property of the `process.env` object with the given
    * `propertyName`.
    *
    * @param propertyName
    *   the name of the property to match (without the "process.env." prefix).
    * @return
    *   a list of Call nodes representing the matching assignment calls.
    */
  private def matchProcessEnvAssignmentCalls(propertyName: String): List[Call] = {
    // Match assignment calls on the right side for process.env.PROPERTY or process.env['PROPERTY']
    // Example const dbName = process.env['DB_NAME']
    val pattern = s".*process\\.env(\\.${propertyName}|\\[('|\")${propertyName}('|\")]).*"
    cpg.call("<operator>.assignment").where(_.astChildren.code(pattern)).l
  }

  private def connectEnvCallsToProperties(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchProcessEnvAssignmentCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

}
