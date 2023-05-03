package ai.privado.languageEngine.python.passes.config

import ai.privado.cache.RuleCache

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Literal, Member}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.BatchedUpdate
import ai.privado.languageEngine.java.language.NodeStarters

import io.shiftleft.semanticcpg.language._

class PythonPropertyFilePass(cpg: Cpg) extends ForkJoinParallelCpgPass[JavaProperty](cpg) {
  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.l.toArray.filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperties(property, builder)
  }

  private def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    connectGetEnvironLiterals(property, builder)
    connectDBConfigMembers(property, builder)
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

  /** Create an edge between the literals in the environ.get calls and the property nodes.
    */
  private def connectGetEnvironLiterals(propertyNode: JavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    matchEnvironGetCalls(propertyNode.name.strip()).foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  /** Create an edge between the literals in the db config members and the property nodes.
    */
  private def connectDBConfigMembers(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchDBConfigCalls(propertyNode.name.strip()).foreach(member => {
      builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
      builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

}
