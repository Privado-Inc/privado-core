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

import ai.privado.utility.Utilities
import com.github.wnameless.json.flattener.JsonFlattener
import io.circe.yaml.parser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, MethodParameterIn, NewFile, NewJavaProperty}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._

import java.util.Properties
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import scala.xml._

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] =
    propertiesFiles(projectRoot, Set(".properties", ".yml", ".yaml", ".xml")).toArray

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
      obtainKeyValuePairs(file, builder)
    } match {
      case Success(keyValuePairs) =>
        val propertyNodes = keyValuePairs.map(addPropertyNode(_, builder))

        propertyNodes.foreach(propertyNode => {
          connectGetPropertyLiterals(propertyNode, builder)
          connectAnnotatedParameters(propertyNode, builder)
        })

        propertyNodes
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  private def connectAnnotatedParameters(
    propertyNode: NewJavaProperty,
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
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
  private def annotatedMembers() = cpg.annotation
    .fullName(".*Value.*")
    .where(_.member)
    .filter(_.parameterAssign.l.length > 0)
    .map { x => (x.parameterAssign.head, x.member.head) }
    .l
  private def getMember(member: String, className: String) =
    cpg.member.where(_.typeDecl.fullName(className)).where(_.name(member)).toList

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

  private def obtainKeyValuePairs(file: String, builder: DiffGraphBuilder): List[(String, String)] = {
    if (file.matches(""".*\.(?:yml|yaml)""")) {
      loadAndConvertYMLtoProperties(file)
    } else if (file.endsWith(".xml")) {
      loadAndConvertXMLtoProperties(file, builder)
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

  // if beans file contain placeholders, search in .properties files across the project
  private def resolvePlaceholderValuesXML(placeholder: String): String = {
    val propertyFiles: List[String] = propertiesFiles(projectRoot, Set(".properties"))
    propertyFiles.foreach(file => {
      // Search across properties to find the required
      loadFromProperties(file).foreach(propertyValue => {
        val (name, value) = propertyValue;
        if (name.equals(placeholder)) return value;
      })
    })
    ""
  }

  // Used to extract (name, value) pairs from a bean config file
  private def XMLParserBean(xmlPath: String, builder: DiffGraphBuilder): List[(String, String)] = {
    try {
      val xml = XML.loadFile(xmlPath)
      val nameValuePairs = (xml \\ "bean").flatMap { bean =>
        {
          var result: (String, String) = ("", "")
          val className: String        = bean \@ "class"
          (bean \\ "property").map { prop =>
            {
              // Search for property tags inside a bean
              val propValue = prop \@ "value"
              if (propValue.startsWith("$") && propValue.endsWith("}")) {
                val value = resolvePlaceholderValuesXML(
                  propValue.substring(2, propValue.length - 1)
                ) // Pass placeholder name without ${ and }
                if (value.nonEmpty) {
                  result = ((prop \@ "name"), value)
                }
              } else {
                result = ((prop \@ "name"), propValue)
              }

            }

            val members = getMember((prop \@ "name"), className);
            if (members.nonEmpty) {
              val propertyNode = NewJavaProperty().name(result._1).value(result._2)
              val member       = members.head
              if (member != null) {
                builder.addEdge(propertyNode, member, EdgeTypes.IS_USED_AT)
                builder.addEdge(member, propertyNode, EdgeTypes.ORIGINAL_PROPERTY);
              }
            }
            result
          }
        }
      }

      return nameValuePairs.toList
        .collect { case (name, value) => if (value.nonEmpty) (name, value) else ("", "") }
        .filter { case (name, value) =>
          name.nonEmpty && value.nonEmpty // Filter out name, value pairs which could not be resolved
        }
    } catch {
      case e: Throwable => println(e)
    }

    List[("", "")]()

  }

  private def loadAndConvertXMLtoProperties(file: String, builder: DiffGraphBuilder): List[(String, String)] = {
    val properties  = new Properties();
    val inputStream = better.files.File(file).newInputStream
    try {
      properties.loadFromXML(inputStream)
      properties.propertyNames.asScala.toList
        .collect(p => (p.toString, properties.getProperty(p.toString)))
    } catch {
      case _: Throwable => {
        XMLParserBean(file, builder)
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

  // Add extensions as a parameter to decouple it
  private def propertiesFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable)
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
