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

import ai.privado.cache.RuleCache
import io.shiftleft.codepropertygraph.generated.nodes.{
  AnnotationParameterAssign,
  JavaProperty,
  Literal,
  Member,
  MethodParameterIn,
  NewFile,
  NewJavaProperty
}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._
import ai.privado.languageEngine.java.language.NodeStarters

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg) extends ForkJoinParallelCpgPass[JavaProperty](cpg) {
  override def generateParts(): Array[_ <: AnyRef] =
    cpg.property.l.toArray.filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperties(property, builder)
  }

  private def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    connectGetPropertyLiterals(property, builder)
    connectAnnotatedParameters(property, builder)
  }

  private def connectAnnotatedParameters(propertyNode: JavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val paramsAndValues = annotatedParameters()

    paramsAndValues
      .filter { case (_, value) => propertyNode.name == value }
      .foreach { case (param, _) =>
        builder.addEdge(propertyNode, param, EdgeTypes.IS_USED_AT)
        builder.addEdge(param, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
      }

    val membersAndValues = annotatedMembers()

    membersAndValues
      .filter { case (key, _) => propertyNode.name == key.code.slice(3, key.code.length - 2) }
      .foreach { case (_, value) =>
        builder.addEdge(propertyNode, value, EdgeTypes.IS_USED_AT)
        builder.addEdge(value, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
      }
  }

  /** List of all parameters annotated with Spring's `Value` annotation, along with the property name.
    */
  def annotatedParameters(): List[(MethodParameterIn, String)] = cpg.annotation
    .fullName("org.springframework.*Value")
    .where(_.parameter)
    .where(_.parameterAssign.code("\\\"\\$\\{.*\\}\\\""))
    .map { x =>
      val literalName = x.parameterAssign.code.head
      val value       = literalName.slice(3, literalName.length - 2)
      (x.start.parameter.head, value)
    }
    .l

  /** List of all members annotated with Spring's `Value` annotation, along with the property name.
    */
  private def annotatedMembers(): List[(AnnotationParameterAssign, Member)] = cpg.annotation
    .fullName(".*Value.*")
    .where(_.member)
    .filter(_.parameterAssign.l.length > 0)
    .map { x => (x.parameterAssign.head, x.member.head) }
    .l

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
    .where(_.inCall.name(".*getProperty"))
    .l

}
