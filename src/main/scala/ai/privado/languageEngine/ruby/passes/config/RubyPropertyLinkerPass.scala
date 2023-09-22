package ai.privado.languageEngine.ruby.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class RubyPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val cachedCall = cpg.call("<operator>.(assignment|fieldAccess)").l

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.l.filter(pair => pair.name.nonEmpty && pair.value.nonEmpty).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectEnvCallsToProperties(property, builder)
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

  private def connectEnvCallsToProperties(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchEnvAssignmentCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
    matchYamlAssignmentCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }
}
