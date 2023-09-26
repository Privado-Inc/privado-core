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
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities
import com.gargoylesoftware.htmlunit.html._
import com.gargoylesoftware.htmlunit.html.HtmlScript
import com.gargoylesoftware.htmlunit.{BrowserVersion, WebClient}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewTemplateDom}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.tools.nsc.io.JFile

/** This pass will search for all the files with .html and .hbs (EmberJS) in a given source directory and parse them.
  *
  * We are using HtmlUnit (https://htmlunit.sourceforge.io/) to parse the HTML files.
  *
  * It doesn't parse all the constructs of EmberJS. However this will at least support basic constructs of HTML
  *
  * This will parse the HTML files and add respective CPG nodes for HTMLElement, HTMLOpenElement, HTMLCloseElement and
  * HTMLElementAttribute.
  *
  * For now we are only processing sufficient data points which are required for identifying webform collection points.
  * @param cpg
  * @param projectRoot
  * @param ruleCache
  */
class HTMLParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Search for .html and .hbs files and generate tasks to process each file separately in its own thread
    * @return
    */
  override def generateParts(): Array[String] =
    Utilities.getSourceFilesWithGivenExtension(projectRoot, Set(".html", ".hbs"), ruleCache).toArray

  /** This method acts as a task executor method. Which will be invoked by thread service by passing individual file
    * paths to be processed independently in separate thread.
    *
    * This will first create a file node representing the file and then further pass the control to importHtmlNodes to
    * process the entire file
    *
    * @param builder
    * @param htmlFilePath
    */
  override def runOnPart(builder: DiffGraphBuilder, htmlFilePath: String): Unit = {
    logger.trace(s"processing file ->>> ${htmlFilePath}")
    val fileNode = Utilities.addFileNode(htmlFilePath, builder)
    importHtmlNodes(htmlFilePath, fileNode, builder)
  }

  /** This will parse the HTML file through HtmlUnit driver and get the first dom element for the processing.
    *
    * @param htmlFilePath
    * @param fileNode
    * @param builder
    */
  def importHtmlNodes(htmlFilePath: String, fileNode: NewFile, builder: DiffGraphBuilder): Unit = {
    try {
      val webClient = new WebClient(
        new BrowserVersion.BrowserVersionBuilder(BrowserVersion.CHROME).setOnLine(false).build()
      )
      val options = webClient.getOptions

      // Configure options to ignore certain errors
      options.setThrowExceptionOnFailingStatusCode(false) // Do not throw exceptions for failing HTTP status codes
      options.setThrowExceptionOnScriptError(false)       // Do not throw exceptions for JavaScript errors

      val htmlFile                 = new JFile(htmlFilePath)
      val page: HtmlPage           = webClient.getPage(htmlFile.toURI.toURL)
      val htmlElement: HtmlElement = page.getFirstByXPath("//*")
      processElement(builder, htmlElement, fileNode, htmlFilePath)
    } catch {
      case _: Throwable => logger.debug(s"Some error while parsing HTML file -> ${htmlFilePath}")
    }
  }

  /** This will process one HTML Element. It will in turn create Open, Close and list of Attribute nodes in the context
    * of given HTML element and save the nodes inside CPG along with File node edge.
    *
    * This method will also iterate through child elements of given HTML elements and recursively call the same method
    * to process each child element
    *
    * @param builder
    * @param element
    * @param fileNode
    */
  private def processElement(
    builder: DiffGraphBuilder,
    element: DomElement,
    fileNode: NewFile,
    htmlFilePath: String
  ): Unit = {
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
      processElement(builder, element, fileNode, htmlFilePath)
    })
  }

  /** This will process all the attributes of given HTML Element. Create CPG node (TemplateDom) for each attribute and
    * add it inside CPG.
    *
    * It will also return concatenated string of attributes reconstructed as a string. Which will be used by callee to
    * reconstruct the HTML Element.
    * @param builder
    * @param element
    * @param fileNode
    * @return
    */
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

    if (element.getTagName.equalsIgnoreCase("script")) {
      if (element.isInstanceOf[HtmlScript]) {
        val scriptElement = element.asInstanceOf[HtmlScript]
        val scriptContent = scriptElement.getTextContent()
        if (scriptContent.nonEmpty) {
          val attributeStr = s"content=\"${scriptContent}\""
          elementAttributesStr += " " + attributeStr
        }
      }
    }

    (elementAttributesStr, attributeNodes.toList)
  }

  /** This will create TemplateDom node for HTMLOpenElement, which represents separate node in CPG for Opening HTML tag.
    *
    * It will also reconstruct the Open tag HTML element use it for construction of TemplateDom node as well as return
    * it to callee to reconstruct entire HTML Element.
    *
    * @param builder
    * @param element
    * @param fileNode
    * @param attributesStr
    * @return
    */
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

  /** This will construct TemplateDom node for Closing tag of HTML Element.
    *
    * It will also reconstruct the Closing HTML tag string, use it for TemplateDom node construction and return it to
    * the callee to reconstruct the entire HTML element.
    * @param builder
    * @param element
    * @param fileNode
    * @return
    */
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

  /** Helper to Create TemplateDom CPG Node.
    *
    * @param name
    * @param code
    * @param line
    * @param column
    * @return
    */
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
