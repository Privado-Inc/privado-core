package ai.privado.languageEngine.javascript.passes.config

import ai.privado.semantic.language.*
import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class JSPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  // TODO: Modify the regex to make it more comprehensive
  val dbConnectionRegex =
    "^(db|database|jdbc|mysql|postgres|oracle|sqlserver)_(connection_)?(host|port|name|user|password|uri|driver|ssl|pool_size|timeout|connection_string)$"
  val apiConnectionRegex = ".*/(api|external)?(_|\\.)?(url|base(_|\\.)?path)/i"

  val cachedArguments: List[AstNode] = cpg.call("<operator>.(assignment|fieldAccess)").astChildren.l

  override def generateParts(): Array[_ <: AnyRef] = {
    // TODO Filter out property nodes not created from config files, remove in future
    cpg.property.l
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .whereNot(_.tag.name(InternalTag.SOURCE_PROPERTY.toString))
      .toArray
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
  private def matchProcessEnvAssignmentCalls(propertyName: String): List[AstNode] = {
    // Match assignment calls on the right side for process.env.PROPERTY or process.env['PROPERTY']
    // Example const dbName = process.env['DB_NAME']
    // conf.accontHost
    if (propertyName.contains("**")) {
      List()
    } else {
      val pattern =
        s".*process\\.env(\\.${propertyName}|\\[('|`|\")${propertyName}('|`|\")]).*|.*(conf|Conf).*${propertyName}.*"
      cachedArguments.code(pattern).l
    }
  }

  private def connectEnvCallsToProperties(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchProcessEnvAssignmentCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

}
