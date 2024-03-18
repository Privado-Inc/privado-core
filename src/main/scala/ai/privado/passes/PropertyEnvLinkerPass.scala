package ai.privado.passes

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.Language
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

/** This pass creates a graph layer for env call from property node
 */

class PropertyEnvLinkerPass(cpg: Cpg, language: Language.Value) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  implicit val resolver: NoResolve.type = NoResolve
  val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property
      .iterator
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    language match
      case Language.JAVA => {
        connectEnvProperty(javaMatchingEnvLiteralToPropertyNode(property.name), property, builder)
        connectAnnotatedParameters(property, builder)
      }
      case Language.GO => {
        connectEnvProperty(goMatchingEnvLiteralsToPropertyNode(property.name), property, builder)
      }
  }

  // This method used to connect the property node with literal where it used
  private def connectEnvProperty(literalNodes: List[Literal], propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    literalNodes.foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  private def connectAnnotatedParameters(propertyNode: JavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val paramsAndValues = annotatedParameters()

    paramsAndValues.iterator
      .filter { case (_, value) => propertyNode.name == value }
      .foreach { case (param, _) =>
        builder.addEdge(propertyNode, param, EdgeTypes.IS_USED_AT)
        builder.addEdge(param, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
      }

    val membersAndValues = annotatedMembers()

    membersAndValues
      .filter { case (key, _) => propertyNode.name == Option(key.code.slice(3, key.code.length - 2)).getOrElse("") }
      .foreach { case (_, value) =>
        builder.addEdge(propertyNode, value, EdgeTypes.IS_USED_AT)
        builder.addEdge(value, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
      }

    val annotatedMethodsList = annotatedMethods()

    annotatedMethods()
      .filter { case (key, _) => propertyNode.name == Option(key.code.slice(3, key.code.length - 2)).getOrElse("") }
      .foreach { case (_, method) =>
        // TODO: Add support for linking multiple fieldAccess in a single method
        // This will work (as expected) only if a single fieldAccess is present in the method, when not the case it will connect the referenced member of the first fieldAccess to the property node
        val referencedMember = method.ast.fieldAccess.referencedMember.l.headOption.orNull
        if (referencedMember != null) {
          builder.addEdge(propertyNode, referencedMember, EdgeTypes.IS_USED_AT)
          builder.addEdge(referencedMember, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
        } else {
          logger.debug(s"Could not find a referenced member for fieldAccess in the method ${method.name}")
        }
      }
  }

  /** List of all methods annotated with Spring's `Value` annotation, along with the method node
   */
  private def annotatedMethods(): List[(AnnotationParameterAssign, Method)] = cpg.annotation
    .nameExact("Value")
    .filter(_.method.nonEmpty)
    .filter(_.parameterAssign.nonEmpty)
    .map { x => (x.parameterAssign.next(), x.method.next()) }
    .toList

  /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
   */
  private def annotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
    .nameExact("Value")
    .filter(_.parameter.nonEmpty)
    .filter(_.parameterAssign.code("\\\"\\$\\{.*\\}\\\"").nonEmpty)
    .map { x =>
      val literalName = x.parameterAssign.code.next()
      val value = Option(literalName.slice(3, literalName.length - 2)).getOrElse("")
      (x.parameter.next(), value)
    }
    .filter { (_, value) =>
      value.nonEmpty
    }
    .toList

  /** List of all members annotated with Spring's `Value` annotation, along with the property name.
   */
  private def annotatedMembers(): List[(AnnotationParameterAssign, Member)] = cpg.annotation
    .nameExact("Value")
    .filter(_.member.nonEmpty)
    .filter(_.parameterAssign.nonEmpty)
    .map { x => (x.parameterAssign.next(), x.member.next()) }
    .toList

  // List all literal fetching property value for GO
  // ex: os.get("KEY")
  private def goMatchingEnvLiteralsToPropertyNode(propertyName: String): List[Literal] = {
    //To get every dot seperated part of the env key where each element is progressively longer,
    // containing one more dot-separated value
    val parts = propertyName.split("\\.")
    val propertyKey = (1 to parts.length).map(i => parts.take(i).mkString(".")).mkString("|")
    cpg.literal
      .codeExact("\"" + propertyKey + "\"")
      .filter(_.inCall.name("(?i).*getenv").nonEmpty)
      .toList
  }

  // List all literal fetching property value for JAVA
  // ex: System.getProperty("KEY")
  private def javaMatchingEnvLiteralToPropertyNode(propertyName: String): List[Literal]
  = cpg.literal
    .codeExact("\"" + propertyName + "\"")
    .filter(_.inCall.name(".*getProperty").nonEmpty)
    .toList
}
