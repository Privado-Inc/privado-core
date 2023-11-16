package ai.privado.languageEngine.kotlin.passes.config

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

import ai.privado.cache.RuleCache
import ai.privado.utility._
import ai.privado.tagger.PrivadoParallelCpgPass
import better.files._
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewAndroidXmlLayoutNode, NewAndroidXmlPermissionNode}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.mutable
import scala.util.Try
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.xml.XML

class AndroidXmlParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] =
    // filter layout/*.xml and AndroidManifest
    getXMLFiles(projectRoot, Set(".xml"), ".*(?:layout[/\\\\][^/]*\\.xml|AndroidManifest\\.xml).*$").toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    if (file.contains("AndroidManifest.xml")) {
      buildAndAddPermissionXMLNodes(file, builder, fileNode)
    } else {
      buildAndAddLayoutXMLNodes(file, builder, fileNode)
    }
  }

  private def buildAndAddLayoutXMLNodes(file: String, builder: DiffGraphBuilder, fileNode: NewFile): Unit = {
    val parsedNodes = parseLayoutXMLFile(file)
    parsedNodes.flatMap { case (nodeType, nodeNames) =>
      nodeNames.map { nodeName =>
        val layoutNode = NewAndroidXmlLayoutNode().name(nodeName).typeFullName(nodeType)
        builder.addNode(layoutNode)
        builder.addEdge(layoutNode, fileNode, EdgeTypes.SOURCE_FILE)
      }
    }
  }

  private def buildAndAddPermissionXMLNodes(file: String, builder: DiffGraphBuilder, fileNode: NewFile): Unit = {
    val parsedNodes = parseManifestXMLFile(file)
    parsedNodes.foreach { typ =>
      val permissionNode = NewAndroidXmlPermissionNode().name("uses-permission").permissionType(typ)
      builder.addNode(permissionNode)
      builder.addEdge(permissionNode, fileNode, EdgeTypes.SOURCE_FILE)
    }
  }

  private def parseManifestXMLFile(filePath: String): ListBuffer[String] = {
    try {
      val xml             = XML.loadFile(filePath)
      val nodes           = xml \\ "_"
      val permissionNodes = nodes.filter(node => node.label.contains("uses-permission"))
      val permissions     = new ListBuffer[String]()

      permissionNodes.foreach(node => {
        val permissionType = node.attributes.find(_.key == "name") match {
          case Some(attr) if attr.value.nonEmpty =>
            attr.value.toString.split("android.permission.").last.stripSuffix("\"")
          case _ => ""
        }
        if (permissionType.nonEmpty) {
          permissions.addOne(permissionType)
        }
      })
      permissions
    } catch {
      case e: Exception =>
        println(s"Error parsing Android layout XML file: ${e.getMessage}")
        new ListBuffer[String]()
    }
  }

  // Returns a map (EditText -> emailEditText)
  private def parseLayoutXMLFile(filePath: String): HashMap[String, ListBuffer[String]] = {
    try {
      val xml           = XML.loadFile(filePath)
      val nodes         = xml \\ "_"
      val editTextNodes = nodes.filter(node => node.label.contains("EditText")) // variations of *editText as well
      val nodeMap       = new HashMap[String, ListBuffer[String]]()

      editTextNodes.foreach(node => {
        val id = node.attributes.find(_.key == "id") match {
          case Some(attr) if attr.value.nonEmpty => attr.value.toString.split("/").last.stripSuffix("\"")
          case _                                 => ""
        }
        if (id.nonEmpty) {
          val nodeName = node.label
          if (nodeMap.contains(nodeName)) {
            nodeMap(nodeName) += id
          } else {
            nodeMap += (nodeName -> ListBuffer(id))
          }
        }
      })
      nodeMap
    } catch {
      case e: Exception =>
        println(s"Error parsing Android layout XML file: ${e.getMessage}")
        new HashMap[String, ListBuffer[String]]()
    }
  }

  private def getXMLFiles(projectRoot: String, extensions: Set[String], allowedFiles: String): List[String] = {
    SourceFiles
      .determine(projectRoot, extensions, None, Some(ruleCache.getExclusionRegex.r), None)
      .filter(_.matches(allowedFiles))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
