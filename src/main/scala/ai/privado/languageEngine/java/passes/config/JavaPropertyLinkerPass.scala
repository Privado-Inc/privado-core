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

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

/** This pass creates a graph layer for Java `.properties` files.
  */
class JavaPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  implicit val resolver: NoResolve.type = NoResolve

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

    annotatedMethodsList
      .filter { case (key, _) =>
        propertyNode.name == Option(key.code.slice(3, key.code.length - 2)).getOrElse("")
      }
      .foreach { case (_, value) =>
        builder.addEdge(propertyNode, value, EdgeTypes.IS_USED_AT)
        builder.addEdge(value, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
      }
  }

  /** List of all methods annotated with Spring's `Value` annotation
    */
  private def annotatedMethods(): List[(AnnotationParameterAssign, Method)] = cpg.annotation
    .name(".*Value.*")
    .filter(_.method.nonEmpty)
    .filter(_.parameterAssign.nonEmpty)
    .map { x => (x.parameterAssign.next(), x.method.next()) }
    .toList

  /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
    */
  private def annotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
    .name(".*Value.*")
    .filter(_.parameter.nonEmpty)
    .filter(_.parameterAssign.code("\\\"\\$\\{.*\\}\\\"").nonEmpty)
    .map { x =>
      val literalName = x.parameterAssign.code.next()
      val value       = Option(literalName.slice(3, literalName.length - 2)).getOrElse("")
      (x.parameter.next(), value)
    }
    .toList

  /** List of all members annotated with Spring's `Value` annotation, along with the property name.
    */
  private def annotatedMembers(): List[(AnnotationParameterAssign, Member)] = cpg.annotation
    .name(".*Value.*")
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
    .filter(_.inCall.name(".*getProperty").nonEmpty)
    .toList

}
