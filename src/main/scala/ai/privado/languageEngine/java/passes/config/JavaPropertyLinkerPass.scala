/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.java.passes.config

import ai.privado.semantic.language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

/** This pass creates a graph layer for Java `.properties` files.
  */
class JavaPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  implicit val resolver: NoResolve.type = NoResolve
  val logger                            = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.iterator.filter(pair => pair.name.nonEmpty && pair.value.nonEmpty).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperties(property, builder)
  }

  private def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    connectGetPropertyLiterals(property, builder)
    connectAnnotatedParameters(property, builder)
  }

  private def connectAnnotatedParameters(propertyNode: JavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val paramsAndValues = annotatedParameters() ++ namedAnnotatedParameters()

    paramsAndValues.iterator
      .filter { case (_, value) => propertyNode.name == value || propertyNode.name.endsWith(s".$value") }
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

  private def namedAnnotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
    .nameExact("Named")
    .filter(_.parameter.nonEmpty)
    .map { x =>
      val value = x.parameterAssign.code.next().split("[.]").lastOption.getOrElse("")
      (x.parameter.next(), value)
    }
    .filter { (_, value) =>
      value.nonEmpty
    }
    .toList

  /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
    */
  private def annotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
    .nameExact("Value")
    .filter(_.parameter.nonEmpty)
    .filter(_.parameterAssign.code("\\\"\\$\\{.*\\}\\\"").nonEmpty)
    .map { x =>
      val literalName = x.parameterAssign.code.next()
      val value       = Option(literalName.slice(3, literalName.length - 2)).getOrElse("")
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

  /** In this method, we attempt to identify users of properties and connect them to property nodes.
    */
  private def connectGetPropertyLiterals(propertyNode: JavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    matchingLiteralsInGetPropertyCalls(propertyNode.name).foreach { lit =>
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    }
  }

  private def matchingLiteralsInGetPropertyCalls(propertyName: String): List[Literal] = cpg.literal
    .codeExact("\"" + propertyName + "\"")
    .toList

}
