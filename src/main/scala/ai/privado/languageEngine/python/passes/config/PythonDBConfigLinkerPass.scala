package ai.privado.languageEngine.python.passes.config

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Member}
import io.shiftleft.semanticcpg.language.*

class PythonDBConfigLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  private val matchingDBKey: String = "(?i).*(host|url|uri|database|db|port|password|username).*"

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.l
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty && pair.name.matches(matchingDBKey))
      .whereNot(_.tag.name(InternalTag.SOURCE_PROPERTY.toString))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectDBConfigMembers(property, builder)
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

  private def connectDBConfigMembers(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchDBConfigCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

}
