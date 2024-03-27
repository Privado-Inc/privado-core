package ai.privado.languageEngine.java.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.*

class JavaAnnotationPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[Annotation](cpg) {

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.annotation
      .name(".*(Value|Named).*")
      .filter(_.parameterAssign.nonEmpty)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, annotation: Annotation): Unit = {

    if (annotation.name == "Value") {

      /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
        */
      if (annotation.parameterAssign.code("\\\"\\$\\{.*\\}\\\"").nonEmpty && annotation.parameter.nonEmpty) {
        val literalName = annotation.parameterAssign.code.head
        val value       = Option(literalName.slice(3, literalName.length - 2)).getOrElse("")
        if (value.nonEmpty) {
          cpg.property
            .filter(p => p.name == value)
            .foreach(p => {
              connectEnvProperty(annotation.parameter.head, p, builder)
            })
        }
      }

      /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
        */
      if (annotation.member.nonEmpty) {
        cpg.property
          .filter(p =>
            p.name == Option(
              annotation.parameterAssign.head.code.slice(3, annotation.parameterAssign.head.code.length - 2)
            ).getOrElse("")
          )
          .foreach(p => {
            connectEnvProperty(annotation.member.head, p, builder)
          })
      }

      /** List of all methods annotated with Spring's `Value` annotation, along with the method node
        */
      if (annotation.method.nonEmpty) {
        val key = annotation.parameterAssign.head
        cpg.property
          .filter(p => p.name == Option(key.code.slice(3, key.code.length - 2)).getOrElse(""))
          .foreach(p => {
            val referenceMember = annotation.method.head.ast.fieldAccess.referencedMember.l.headOption.orNull
            if (referenceMember != null) {
              connectEnvProperty(referenceMember, p, builder)
            }
          })
      }
    } else if (annotation.name == "Named" && annotation.parameter.nonEmpty) {
      val value = annotation.parameterAssign.code.head.split("[.]").lastOption.getOrElse("")
      if (value.nonEmpty) {
        cpg.property
          .filter(p => p.name.endsWith(value))
          .foreach(p => {
            connectEnvProperty(annotation.parameter.head, p, builder)
          })
      }
    }
  }

  def connectEnvProperty(literalNode: AstNode, propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    builder.addEdge(propertyNode, literalNode, EdgeTypes.IS_USED_AT)
    builder.addEdge(literalNode, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
  }
}

//private def namedAnnotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
//  .nameExact("Named")
//  .filter(_.parameter.nonEmpty)
//  .map { x =>
//    val value = x.parameterAssign.code.next().split("[.]").lastOption.getOrElse("")
//    (x.parameter.next(), value)
//  }
//  .filter { (_, value) =>
//    value.nonEmpty
//  }
//  .toList
