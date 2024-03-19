package ai.privado.languageEngine.ruby.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.passes.PropertyEnvLinkerPassBase
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class RubyEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  private val cachedCall = cpg.call("<operator>.(assignment|fieldAccess)").l

  override def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchEnvAssignmentCalls(property.name.strip()).foreach(member => {
      connectEnvProperty(member, property, builder)
    })
    matchYamlAssignmentCalls(property.name.strip()).foreach(member => {
      connectEnvProperty(member, property, builder)
    })
  }

  private def matchEnvAssignmentCalls(propertyName: String): List[AstNode] = {
    if (propertyName.contains("**")) {
      List()
    } else {
      val pattern =
        s".*ENV(\\.fetch)?(\\.${propertyName}|\\[('|`|\")${propertyName}('|`|\")]).*"
      cachedCall.astChildren.code(pattern).l
    }
  }

  private def matchYamlAssignmentCalls(propertyName: String): List[AstNode] = {
    if (propertyName.contains("**")) {
      List()
    } else {
      val pattern =
        s".*Settings.*${propertyName}.*"
      cachedCall.astChildren.code(pattern).l
    }
  }
}
