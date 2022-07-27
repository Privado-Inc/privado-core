package ai.privado.utility

import ai.privado.model.NodeType.NodeType
import ai.privado.model.{Constants, RuleInfo}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTag
import io.shiftleft.utils.{IOUtils, ProjectRoot}
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

import java.nio.file.Paths
import scala.util.Try

object Utilities {

  /*
   Utility to add a single tag to a object
   */
  def storeForTag(
    builder: BatchedUpdate.DiffGraphBuilder,
    source: NodeOrDetachedNode
  )(tagName: String, tagValue: String = "") = {
    builder.addEdge(source, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
  }

  /*
   Utility to add Tag based on a rule Object
   */
  def addRuleTags(builder: BatchedUpdate.DiffGraphBuilder, node: NodeOrDetachedNode, ruleInfo: RuleInfo): Unit = {
    val storeForTagHelper = storeForTag(builder, node) _
    storeForTagHelper(Constants.id, ruleInfo.id)
    storeForTagHelper(Constants.name, ruleInfo.name)
    storeForTagHelper(Constants.category, ruleInfo.category)
    storeForTagHelper(Constants.sensitivity, ruleInfo.sensitivity)
    storeForTagHelper(Constants.nodeType, ruleInfo.nodeType.toString)
    for ((key, value) <- ruleInfo.tags) {
      storeForTagHelper(key, value)
    }
  }

  /*
   Utility to get the default semantics for dataflow queries
   */
  def getDefaultSemantics() = {
    val semanticsFilename = ProjectRoot.relativise("src/main/resources/default.semantics")
    println(s"Using semantics from : $semanticsFilename")
    Semantics.fromList(new Parser().parseFile(semanticsFilename))
  }

  /*
   Utility to filter rules by node type
   */
  def getRulesByNodeType(rules: List[RuleInfo], nodeType: NodeType) =
    rules.filter(rule => rule.nodeType.equals(nodeType))

  /** For a given `filename`, `lineToHighlight`, return the corresponding code by reading it from the file. If
    * `lineToHighlight` is defined, then a line containing an arrow (as a source code comment) is included right before
    * that line.
    */
  def dump(filename: String, lineToHighlight: Option[Integer]): String = {
    val arrow: CharSequence = "/* <=== */ "
    val lines = Try(IOUtils.readLinesInFile(Paths.get(filename))).getOrElse {
      println("error reading from: " + filename);
      List()
    }
    val startLine: Integer = {
      if (lineToHighlight.isDefined)
        Math.max(0, lineToHighlight.get - 5)
      else
        0
    }
    val endLine: Integer = {
      if (lineToHighlight.isDefined)
        Math.min(lines.length, lineToHighlight.get + 5)
      else
        0
    }
    lines
      .slice(startLine - 1, endLine)
      .zipWithIndex
      .map { case (line, lineNo) =>
        if (lineToHighlight.isDefined && lineNo == lineToHighlight.get - startLine) {
          line + " " + arrow
        } else {
          line
        }
      }
      .mkString("\n")
  }
}
