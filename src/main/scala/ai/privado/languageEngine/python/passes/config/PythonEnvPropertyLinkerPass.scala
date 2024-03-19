package ai.privado.languageEngine.python.passes.config

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Literal, Member}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.BatchedUpdate
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.InternalTag
import ai.privado.passes.PropertyEnvLinkerPassBase
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class PythonEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {
  override def generateParts(): Array[_ <: AnyRef] = {
    // TODO Filter out property nodes not created from config files, Remove in future
    cpg.property.l
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .whereNot(_.tag.name(InternalTag.SOURCE_PROPERTY.toString))
      .toArray
  }

  override def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchEnvironGetCalls(property.name.strip()).foreach(lit => {
      // Create an edge between the literals in the environ.get calls and the property nodes.
      connectEnvProperty(lit, property, builder)
    })
    matchDBConfigCalls(property.name.strip()).foreach(lit => {
      // Create an edge between the literals in the db config members and the property nodes.
      connectEnvProperty(lit, property, builder)
    })
  }

  /** Matches the exact key of the propertyNode to its corresponding os.environ.get() calls.
    */
  private def matchEnvironGetCalls(propertyName: String): List[Literal] = {
    cpg.literal
      .codeExact("\"" + propertyName + "\"")
      .where(_.inCall.methodFullName(".*\\(?environ\\)?\\.get"))
      .l
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
}
