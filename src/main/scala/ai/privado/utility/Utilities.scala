package ai.privado.utility

import ai.privado.cache.RuleCache
import ai.privado.model.CatLevelOne.CatLevelOne
import ai.privado.semantic.Language._
import ai.privado.model.{Constants, RuleInfo}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewTag, StoredNode}
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.util.{Failure, Success, Try}
import java.nio.file.Paths
import java.util.regex.{Pattern, PatternSyntaxException}
import scala.io.Source
import io.shiftleft.semanticcpg.language._

object Utilities {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Utility to add a single tag to a object
    */
  def storeForTag(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: StoredNode
  )(tagName: String, tagValue: String = ""): BatchedUpdate.DiffGraphBuilder = {
    if (isFileProcessable(node.location.filename)) {
      builder.addEdge(node, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
    }
    builder
  }

  /** Utility to add Tag based on a rule Object
    */
  def addRuleTags(builder: BatchedUpdate.DiffGraphBuilder, node: StoredNode, ruleInfo: RuleInfo): Unit = {
    if (isFileProcessable(node.location.filename)) {
      val storeForTagHelper = storeForTag(builder, node) _
      storeForTagHelper(Constants.id, ruleInfo.id)
      storeForTagHelper(Constants.nodeType, ruleInfo.nodeType.toString)
      storeForTagHelper(Constants.catLevelOne, ruleInfo.catLevelOne.name)
      storeForTagHelper(Constants.catLevelTwo, ruleInfo.catLevelTwo)

      // storing by catLevelTwo to get id
      storeForTagHelper(ruleInfo.catLevelTwo, ruleInfo.id)
    }
  }

  /** Utility to get the default semantics for dataflow queries
    */
  def getDefaultSemantics: Semantics = {
    val semanticsFilename = Source.fromResource("default.semantics")
    Semantics.fromList(new Parser().parse(semanticsFilename.getLines().mkString("")))
  }

  /** Utility to filter rules by catLevelOne
    */
  def getRulesByCatLevelOne(rules: List[RuleInfo], catLevelOne: CatLevelOne): Seq[RuleInfo] =
    rules.filter(rule => rule.catLevelOne.equals(catLevelOne))

  /** For a given `filename`, `lineToHighlight`, return the corresponding code by reading it from the file. If
    * `lineToHighlight` is defined, then a line containing an arrow (as a source code comment) is included right before
    * that line.
    */
  def dump(filename: String, lineToHighlight: Option[Integer]): String = {
    val arrow: CharSequence = "/* <=== */ "
    try {
      if (!filename.equals("<empty>")) {
        val lines = IOUtils.readLinesInFile(Paths.get(filename))
        val startLine: Integer = {
          if (lineToHighlight.isDefined)
            Math.max(1, lineToHighlight.get - 5)
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
      } else
        ""
    } catch {
      case e: Exception =>
        logger.debug("Error : ", e)
        ""
    }
  }

  /** To check if processed pattern is valid
    */
  def isValidRule(stringPattern: String, ruleId: String = "", fileName: String = ""): Boolean = {
    try {
      Pattern.compile(stringPattern)
      true
    } catch {
      case patternSyntaxException: PatternSyntaxException =>
        logger.error(
          s"Error parsing rule, ruleId : $ruleId, fileName : $fileName, stringPattern : $stringPattern, error : ${patternSyntaxException.toString}"
        )
        false
      case _: Throwable => false
    }
  }

  /** If environment variable present will return that otherwise the repoPath
    * @param repoPath
    * @return
    */
  def getRepoScanPath(repoPath: String): String = {
    sys.env.get("PRIVADO_HOST_SCAN_DIR") match {
      case Some(path) => path
      case _          => repoPath
    }
  }

  /** Checks if given filePath doesn't belong to the excluding file regex
    *
    * @param filePath
    * @return
    */
  def isFileProcessable(filePath: String) = {
    RuleCache.getRule.exclusions
      .flatMap(exclusionRule => {
        exclusionRule.patterns.headOption match {
          case Some(pattern) =>
            Try(!filePath.matches(pattern)) match {
              case Success(result) => Some(result)
              case Failure(e)      => None
            }
          case None => None
        }
      })
      .foldLeft(true)((a, b) => a && b)
  }
}
