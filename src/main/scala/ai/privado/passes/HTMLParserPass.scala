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

package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.utility.Utilities
import com.gargoylesoftware.htmlunit.WebClient
import com.gargoylesoftware.htmlunit.html._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewTemplateDom}
import io.shiftleft.passes.ForkJoinParallelCpgPass

import scala.collection.mutable
import scala.tools.nsc.io.JFile

class HTMLParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends ForkJoinParallelCpgPass[String](cpg) {
  override def generateParts(): Array[String] =
    Utilities.getSourceFilesWithGivenExtension(projectRoot, Set(".html", ".hbs"), ruleCache).toArray

  def importHtmlNodes(htmlFilePath: String, fileNode: NewFile, builder: DiffGraphBuilder): Unit = {
    // TODO: Add file error handler
    val htmlFile                 = new JFile(htmlFilePath)
    val webClient                = new WebClient()
    val page: HtmlPage           = webClient.getPage(htmlFile.toURI.toURL)
    val htmlElement: HtmlElement = page.getFirstByXPath("//*")
    processElement(builder, htmlElement, fileNode)
  }

  private def processAttributes(
    builder: DiffGraphBuilder,
    element: DomElement,
    fileNode: NewFile
  ): (String, List[NewTemplateDom]) = {
    var elementAttributesStr = ""
    val attributeNodes       = mutable.ListBuffer[NewTemplateDom]()
    element.getAttributesMap.forEach((key, value) => {
      val attributeStr = s"${key}=\"${value.getNodeValue}\""
      val attributeNode = createTemplateDomNode(
        Constants.HTMLElementAttribute,
        attributeStr,
        Option(value.getStartLineNumber),
        Option(value.getStartColumnNumber)
      )
      Utilities.addNodeWithFileEdge(builder, attributeNode, fileNode)
      elementAttributesStr += " " + attributeStr
      attributeNodes.addOne(attributeNode)
    })
    (elementAttributesStr, attributeNodes.toList)
  }

  private def processOpenElement(
    builder: DiffGraphBuilder,
    element: DomElement,
    fileNode: NewFile,
    attributesStr: String
  ): (String, NewTemplateDom) = {
    val openElementStr = s"<${element.getTagName} ${attributesStr} >"
    val openElement = createTemplateDomNode(
      Constants.HTMLOpenElement,
      openElementStr,
      Option(element.getStartLineNumber),
      Option(element.getStartColumnNumber)
    )
    Utilities.addNodeWithFileEdge(builder, openElement, fileNode)
    (openElementStr, openElement)
  }

  private def processClosingElement(
    builder: DiffGraphBuilder,
    element: DomElement,
    fileNode: NewFile
  ): (String, NewTemplateDom) = {
    val closingElementStr = s"</${element.getTagName}>"
    val closingElement = createTemplateDomNode(
      Constants.HTMLClosingElement,
      closingElementStr,
      Option(element.getEndLineNumber),
      Option(element.getEndColumnNumber)
    )
    Utilities.addNodeWithFileEdge(builder, closingElement, fileNode)
    (closingElementStr, closingElement)
  }

  private def processElement(builder: DiffGraphBuilder, element: DomElement, fileNode: NewFile): Unit = {
    val (elementAttributesStr, attributeNodes) = processAttributes(builder, element, fileNode)
    val (openElementStr, openElement)          = processOpenElement(builder, element, fileNode, elementAttributesStr)
    val (closingElementStr, closingElement)    = processClosingElement(builder, element, fileNode)
    val htmlElement = createTemplateDomNode(
      Constants.HTMLElement,
      s"${openElementStr} ${closingElementStr}",
      Option(element.getStartLineNumber),
      Option(element.getStartColumnNumber)
    )
    Utilities.addNodeWithFileEdge(builder, htmlElement, fileNode)
    element.getChildElements.forEach(element => {
      processElement(builder, element, fileNode)
    })
  }

  override def runOnPart(builder: DiffGraphBuilder, htmlFilePath: String): Unit = {
    val fileNode = Utilities.addFileNode(htmlFilePath, builder)
    importHtmlNodes(htmlFilePath, fileNode, builder)
  }

  protected def createTemplateDomNode(
    name: String,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewTemplateDom =
    NewTemplateDom()
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
}
