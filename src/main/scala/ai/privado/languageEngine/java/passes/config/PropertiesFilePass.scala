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
import ai.privado.model.RuleInfo
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, MethodParameterIn, NewFile, NewJavaProperty}
import io.shiftleft.passes.{ForkJoinParallelCpgPass, SimpleCpgPass}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._

import scala.jdk.CollectionConverters._
import java.util.Properties
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._
import io.circe.yaml.parser
import com.github.wnameless.json.flattener.JsonFlattener

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] = propertiesFiles(projectRoot).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val propertyNodes = addPropertyNodesAndConnectToUsers(file, builder)
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
  }

  private def addPropertyNodesAndConnectToUsers(
    file: String,
    builder: BatchedUpdate.DiffGraphBuilder
  ): List[NewJavaProperty] = {
    Try {
      obtainKeyValuePairs(file)
    } match {
      case Success(keyValuePairs) =>
        val propertyNodes = keyValuePairs.map(addPropertyNode(_, builder))
        propertyNodes.foreach(connectGetPropertyLiterals(_, builder))
        connectAnnotatedParameters(propertyNodes, builder)

        propertyNodes
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  private def connectAnnotatedParameters(
    propertyNodes: List[NewJavaProperty],
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
    val paramsAndValues = annotatedParameters()
    propertyNodes.foreach { propertyNode =>
      paramsAndValues
        .filter { case (_, value) => propertyNode.name == value }
        .foreach { case (param, _) =>
          builder.addEdge(propertyNode, param, EdgeTypes.IS_USED_AT)
          builder.addEdge(param, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
        }
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

  /** In this method, we attempt to identify users of properties and connect them to property nodes.
    */
  private def connectGetPropertyLiterals(
    propertyNode: NewJavaProperty,
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
    matchingLiteralsInGetPropertyCalls(propertyNode.name).foreach { lit =>
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    }
  }

  private def matchingLiteralsInGetPropertyCalls(propertyName: String): List[Literal] = cpg.literal
    .codeExact("\"" + propertyName + "\"")
    .where(_.inCall.name(".*getProperty"))
    .l

  private def obtainKeyValuePairs(file: String): List[(String, String)] = {
    if (file.matches(""".*\.(?:yml|yaml)""")) {
      loadAndConvertYMLtoProperties(file)
    } else {
      loadFromProperties(file)
    }
  }

  private def loadFromProperties(file: String): List[(String, String)] = {
    val properties  = new Properties()
    val inputStream = better.files.File(file).newFileInputStream
    properties.load(inputStream)
    inputStream.close()
    propertiesToKeyValuePairs(properties)
  }

  private def loadAndConvertYMLtoProperties(file: String): List[(String, String)] = {
    parser.parse(better.files.File(file).contentAsString) match {
      case Right(json) => {
        JsonFlattener
          .flattenAsMap(json.toString)
          .asScala
          .toList
          .collect(p => (p._1, p._2.toString))
          .toList
      }
      case Left(error) => {
        List[("", "")]()
      }
    }
  }

  private def propertiesToKeyValuePairs(properties: Properties): List[(String, String)] = {
    properties
      .propertyNames()
      .asScala
      .collect { case key: String =>
        (key, properties.getProperty(key))
      }
      .toList
  }

  private def propertiesFiles(projectRoot: String): List[String] = {
    SourceFiles.determine(Set(projectRoot), Set(".properties", ".yml", ".yaml"))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def addPropertyNode(
    keyValuePair: (String, String),
    builder: BatchedUpdate.DiffGraphBuilder
  ): NewJavaProperty = {
    val (key, value) = keyValuePair
    val propertyNode = NewJavaProperty().name(key).value(value)
    builder.addNode(propertyNode)
    propertyNode
  }

}
