package ai.privado.languageEngine.javascript.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.InternalTag
import ai.privado.passes.PropertyEnvLinkerPassBase
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class JSEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  override def generateParts(): Array[_ <: AnyRef] = {
    // TODO Filter out property nodes not created from config files, remove in future
    cpg.property.l
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .whereNot(_.tag.name(InternalTag.SOURCE_PROPERTY.toString))
      .toArray
  }

  override def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchProcessEnvAssignmentCalls(property.name.strip()).foreach(lit => {
      connectEnvProperty(lit, property, builder)
    })
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
      cpg.call("<operator>.(assignment|fieldAccess)").astChildren.code(pattern).l
    }
  }
}
