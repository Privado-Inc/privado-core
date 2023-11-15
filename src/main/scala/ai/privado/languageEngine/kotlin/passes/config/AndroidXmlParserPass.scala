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
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewAndroidXmlLayoutNode}
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
    getXMLFiles(projectRoot, Set(".xml"), ".*(?:layout/[^/]*\\.xml|AndroidManifest\\.xml).*$").toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    buildAndAddLayoutXMLNodes(file, builder, fileNode)
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

  // Returns a map (EditText -> emailEditText)
  private def parseLayoutXMLFile(filePath: String): HashMap[String, ListBuffer[String]] = {
    val xml           = XML.loadFile(filePath)
    val nodes         = xml \\ "_"
    val editTextNodes = nodes.filter(node => node.label.contains("EditText")) // variations of *editText as well
    val nodeMap       = new HashMap[String, ListBuffer[String]]()

    editTextNodes.foreach(node => {
      val id       = node.attributes.filter(_.key.equals("id")).toString.split("/").last.stripSuffix("\"")
      val nodeName = node.label
      if (nodeMap.contains(nodeName)) {
        nodeMap(nodeName) += id
      } else {
        nodeMap += (nodeName -> ListBuffer(id))
      }
    })
    nodeMap
  }

  private def getXMLFiles(projectRoot: String, extensions: Set[String], allowedFiles: String): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(_.matches(allowedFiles))
      .filter(Utilities.isFileProcessable(_, ruleCache))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
