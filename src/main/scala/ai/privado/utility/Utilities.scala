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
 */

package ai.privado.utility

import ai.privado.cache.{AppCache, DatabaseDetailsCache, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.metric.MetricHandler
import ai.privado.model.CatLevelOne.CatLevelOne
import ai.privado.model.Constants.outputDirectoryName
import ai.privado.model.*
import better.files.File
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty

import scala.collection.mutable
//import java.io.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  CfgNode,
  NewFile,
  NewTag,
  Block,
  Literal,
  Call,
  Identifier,
  FieldIdentifier,
  Local
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, DetachedNodeData}

import scala.collection.mutable.ListBuffer
import java.io.{PrintWriter, ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.math.BigInteger
import java.net.URL
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.regex.{Pattern, PatternSyntaxException}
import scala.util.{Failure, Success, Try}
import java.nio.file.Files
import scala.util.matching.Regex
import better.files.File.VisitOptions

import scala.Int.MaxValue

object Priority extends Enumeration {
  val MAX     = Value(3)
  val HIGHEST = Value(2)
  val HIGH    = Value(1)
  val MEDIUM  = Value(0)
  val LOW     = Value(-1)
}

object Utilities {

  implicit val resolver: ICallResolver = NoResolve

  private val logger = LoggerFactory.getLogger(getClass)

  var ingressUrls = mutable.ListBuffer.empty[String]

  def checkIfGTMOrSegment(ruleId: String): Boolean = {
    val ruleList = List(
      Constants.segmentPixelRuleId,
      Constants.segmentAnalyticsRuleId,
      Constants.googleTagManagerRuleId,
      Constants.googleTagManagerPixelRuleId
    )
    val ruleExists = ruleList.contains(ruleId)
    ruleExists
  }

  def getEngineContext(config: PrivadoInput, appCache: AppCache, maxCallDepthP: Int = 4)(implicit
    semanticsP: Semantics = DefaultSemantics()
  ): EngineContext = {
    val expanLimit =
      if config.limitArgExpansionDataflows > -1 then config.limitArgExpansionDataflows
      else Constants.defaultExpansionLimit

    EngineContext(
      semantics = semanticsP,
      config =
        if (appCache.repoLanguage == Language.RUBY || config.limitArgExpansionDataflows > -1) then
          EngineConfig(maxCallDepth = maxCallDepthP, maxArgsToAllow = expanLimit, maxOutputArgsExpansion = expanLimit)
        else EngineConfig(maxCallDepth = maxCallDepthP)
    )
  }

  /** Utility to add a single tag to a object
    */
  def storeForTag(builder: BatchedUpdate.DiffGraphBuilder, node: AstNode, ruleCache: RuleCache)(
    tagName: String,
    tagValue: String = ""
  ): BatchedUpdate.DiffGraphBuilder = {
    val fileName = getFileNameForNode(node)
    if (isFileProcessable(fileName, ruleCache)) {
      builder.addEdge(node, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
    }
    builder
  }

  def createCpgFolder(sourceRepoLocation: String): Unit = {
    if (!Files.exists(Paths.get(s"$sourceRepoLocation/$outputDirectoryName"))) {
      Files.createDirectory(Paths.get(s"$sourceRepoLocation/$outputDirectoryName"));
    }
  }

  /** Utility to add database detail tags to sink
    */
  def addDatabaseDetailTags(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: CfgNode,
    databaseDetails: DatabaseDetails,
    ruleCache: RuleCache
  ): Unit = {

    val storeForTagHelper = storeForTag(builder, node, ruleCache) _
    storeForTagHelper(Constants.dbName, databaseDetails.dbName)
    storeForTagHelper(Constants.dbVendor, databaseDetails.dbVendor)
    storeForTagHelper(Constants.dbLocation, databaseDetails.dbLocation)
    storeForTagHelper(Constants.dbOperation, databaseDetails.dbOperation)
  }

  def addArgumentsForGAPixelNode(node: Call, cpg: Cpg): List[(String, String)] = {
    var keyValueStructures = ListBuffer.empty[(String, String)]

    def processIdentifierNode(n: Identifier, topLeftKey: String): Unit = {
      def updateLeftKey(code: String): String = {
        if (topLeftKey.nonEmpty)
          topLeftKey + "." + code
        else
          code
      }

      val identifierTypeFullName = n.typeFullName
      val quotedTypeFullNameStr  = Regex.quote(identifierTypeFullName)

      // Check if typeDecl present, if yes get members
      val memberList = cpg.typeDecl(quotedTypeFullNameStr).member.name.l

      // Try to parse the structure identifier whose typeFullName { p1: __ecma.String; p2: __ecma.String; }
      // Define a regular expression to match keys
      val pattern = """\b([a-zA-Z_]\w*):""".r

      // Find all matches in the input string
      val matches = pattern.findAllMatchIn(identifierTypeFullName)
      if (!n.name.matches("(_tmp_|this|globalThis|eventEmitter|analyticsService|segmentPayloads).*")) {
        if (memberList.nonEmpty) {
          memberList.foreach((key) => {
            keyValueStructures += ((updateLeftKey(key), key))
          })

        } else if (matches.nonEmpty) {
          // Extract keys from matches
          val keys = matches.map(_.group(1)).toList
          keys.foreach((key) => {
            keyValueStructures += ((updateLeftKey(n.code + "." + key), key))
          })
        } else {
          keyValueStructures += ((updateLeftKey(n.code), identifierTypeFullName))
        }
      }
    }

    def processAssignmentNodes(assignmentNodes: List[Call], topLeftKey: String): Unit = {
      assignmentNodes.foreach { keyVal =>
        val (leftChild, rightChild) = extractKeyValPair(keyVal.astChildren.l, Some(topLeftKey))
        for {
          left  <- leftChild
          right <- rightChild
        } {
          if (right.nonEmpty) {
            keyValueStructures += ((left, right))
          }
        }
      }
    }

    def processSpreadOperatorNodes(spreadOperatorNodes: List[Call], topLeftKey: String): Unit = {
      spreadOperatorNodes.foreach { keyVal =>
        val callNodes       = keyVal.astChildren.isCall.l
        val childCallNodes  = callNodes.headOption.map(_.astChildren.l).getOrElse(List.empty)
        val identifierNodes = keyVal.astChildren.isIdentifier.lastOption.l
        callNodes.isCall.foreach { callN =>
          handlePayloadCallNode(callN, topLeftKey)
          handlePICKMethodCallNode(callN, topLeftKey)
        }
        (childCallNodes.collect { case n: Identifier => n } ++ identifierNodes.collect { case n: Identifier => n })
          .foreach(processIdentifierNode(_, topLeftKey))
      }
    }

    def getLeftKey(leftKey: AstNode, topLeftKey: Option[String]): Option[String] = {
      val res = Some(leftKey)
        .collect {
          case c: Call    => handleCallNode(c, topLeftKey).astChildren.isFieldIdentifier.head
          case l: Literal => l
        }
        .map(_.code)
      if (topLeftKey.getOrElse("").nonEmpty)
        Some(topLeftKey.getOrElse("") + "." + res.getOrElse(""))
      else
        res
    }

    def extractKeyValPair(objKeyVal: List[AstNode], topLeftKey: Option[String]): (Option[String], Option[String]) = {
      objKeyVal match {
        case leftKey :: rightVal :: _ =>
          (
            getLeftKey(leftKey, topLeftKey),
            Some(rightVal).collect {
              case l: Literal         => l.code
              case l: Identifier      => l.code
              case l: FieldIdentifier => l.code
              case l: Local           => l.code
              case l: Call            => handleCallNode(l, topLeftKey).code
              case l: Block           => handleBlockNode(l, getLeftKey(leftKey, topLeftKey))
            }
          )
        case _ =>
          (None, None)
      }
    }

    def handlePayloadCallNode(callNode: Call, topLeftKey: String): Unit = {
      if (callNode.name.equals("payload")) {
        val gpEventKey =
          callNode.astChildren.isCall.astChildren.isCall.astChildren.isCall.astChildren.isFieldIdentifier.code.headOption
            .getOrElse("")
        val blockNode = cpg.call.code(".*tmp.*" + gpEventKey + " =.*").astChildren.isBlock.l

        if (blockNode.nonEmpty) {
          val internalBlockNode = blockNode.head.astChildren.isCall
            .code(".*payload.*")
            .astChildren
            .isMethodRef
            .referencedMethod
            .astChildren
            .isBlock
            .l
          val returnBlockNode = internalBlockNode.astChildren.isReturn.astChildren.isBlock.l
          handleBlockNode(blockNode.head, Some(topLeftKey))
          handleBlockNode(returnBlockNode.head, Some(topLeftKey))
        }
      }
    }

    def handlePICKMethodCallNode(callNode: Call, topLeftKey: String): Unit = {
      if (callNode.name.equals("pick")) {
        var concatKey = ""
        val keys      = callNode.astChildren.isBlock.astChildren.isCall.astChildren.isLiteral.code.l
        val objName   = callNode.astChildren.isIdentifier.filter(i => !i.name.matches("this|pick")).name.l

        if (objName.nonEmpty) {
          concatKey = objName.head
        }

        keys.foreach { key =>
          keyValueStructures += ((topLeftKey + "." + key, concatKey + "." + key))
        }
      }
    }

    def handleCallNode(callNode: Call, topLeftKey: Option[String]): AstNode = {
      // Handling `payload` method for GTM differently
      handlePayloadCallNode(callNode, topLeftKey.getOrElse("") + ".payload")
      handlePICKMethodCallNode(callNode, topLeftKey.getOrElse(""))
      // TODO: Handle Json.stringify call node
      // TODO: Handle scriptSafe call node

      if (callNode.methodFullName.equals("__ecma.Array:")) {
        val blockNodes = callNode.astChildren.isBlock.l
        blockNodes.foreach { bNode =>
          handleBlockNode(bNode, Some(topLeftKey.getOrElse("") + ".[]"))
        }
      }

      callNode
    }

    def handleBlockNode(blockNode: Block, topLeftKey: Option[String]): String = {
      val assignmentNodes     = blockNode.astChildren.isCall.name(Operators.assignment).l
      val spreadOperatorNodes = blockNode.astChildren.isCall.name("<operator>.spread").l
      val childBlockNodes     = blockNode.astChildren.isBlock.l
      val arrayCallNodes      = blockNode.astChildren.isCall.methodFullName("__ecma.Array:").astChildren.isBlock.l

      processAssignmentNodes(assignmentNodes, topLeftKey.getOrElse(""))
      processSpreadOperatorNodes(spreadOperatorNodes, topLeftKey.getOrElse(""))
      childBlockNodes.foreach { bNode =>
        handleBlockNode(bNode, topLeftKey)
      }

      arrayCallNodes.foreach { bNode =>
        handleBlockNode(bNode, Some(topLeftKey.getOrElse("") + ".[]"))
      }

      ""
    }

    val assignmentNodes     = node.argument.isBlock.astChildren.isCall.name(Operators.assignment).l
    val spreadOperatorNodes = node.argument.isBlock.astChildren.isCall.name("<operator>.spread").l
    val identifierNodes     = node.argument.isIdentifier.l
    val literalNodes        = node.argument.isLiteral.l

    literalNodes.foreach { l =>
      keyValueStructures += ((l.code, l.code))
    }
    identifierNodes.foreach { i =>
      processIdentifierNode(i, "")
    }

    processAssignmentNodes(assignmentNodes, "")
    processSpreadOperatorNodes(spreadOperatorNodes, "")

    if (node.name.equals("logSegmentEvent")) {
      keyValueStructures += (("segmentData.page", "page"))
      keyValueStructures += (("segmentArgs.meta", "analyticsData"))
      keyValueStructures += (("segmentArgs.unataAppVersion ", "unataAppVersion"))
      keyValueStructures += (("segmentArgs.unataMobileAppVersion", "unataMobileAppVersion"))
      keyValueStructures += (("segmentArgs.unataPageViewId", "unataPageViewId"))
    }

    // TODO: Add PII check for the key/val & add its result with that key
    keyValueStructures.toList
  }

  /** Utility to add Tag based on a rule Object
    */
  def addRuleTags(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: AstNode,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache,
    ruleId: Option[String] = None
  ): Unit = {
    val fileName = getFileNameForNode(node)
    if (isFileProcessable(fileName, ruleCache)) {
      val storeForTagHelper = storeForTag(builder, node, ruleCache) _
      storeForTagHelper(Constants.id, ruleId.getOrElse(ruleInfo.id))
      storeForTagHelper(Constants.nodeType, ruleInfo.nodeType.toString)
      storeForTagHelper(Constants.catLevelOne, ruleInfo.catLevelOne.name)
      storeForTagHelper(Constants.catLevelTwo, ruleInfo.catLevelTwo)

      MetricHandler.totalRulesMatched.addOne(ruleInfo.id)
      ruleCache.internalRules.get(ruleInfo.id) match {
        case Some(_) => MetricHandler.internalRulesMatched.addOne(ruleInfo.id)
        case _       => ()
      }
      // storing by catLevelTwo and nodeType to get id
      storeForTagHelper(ruleInfo.catLevelTwo + ruleInfo.nodeType.toString, ruleId.getOrElse(ruleInfo.id))
    }
  }

  def addRuleTagsForGA(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: AstNode,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache,
    cpg: Cpg,
    ruleId: Option[String] = None
  ): Unit = {
    // Generate Arguments for the GTM & Segment Pixels
    if (checkIfGTMOrSegment(ruleInfo.id)) {
      if (node.isInstanceOf[Call]) {
        val storeForTagHelper                    = storeForTag(builder, node, ruleCache) _
        val callNode                             = node.asInstanceOf[Call]
        val argumentList: List[(String, String)] = addArgumentsForGAPixelNode(callNode, cpg)
        storeForTagHelper(Constants.arguments, serializedArgumentString(argumentList))
      }
    }
    addRuleTags(builder, node, ruleInfo, ruleCache, ruleId)

  }

  /** Utility to filter rules by catLevelOne
    */
  def getRulesByCatLevelOne(rules: List[RuleInfo], catLevelOne: CatLevelOne): Seq[RuleInfo] =
    rules.filter(rule => rule.catLevelOne.equals(catLevelOne))

  /** For a given `filename`, `lineToHighlight`, return the corresponding code by reading it from the file. If
    * `lineToHighlight` is defined, then a line containing an arrow (as a source code comment) is included right before
    * that line.
    */
  def dump(
    filename: String,
    lineToHighlight: Option[Integer],
    message: String = "",
    excerptStartLine: Int = -5,
    excerptEndLine: Int = 5
  ): String = {
    val arrow: CharSequence = "/* <=== " + message + " */ "
    try {
      if (!filename.equals("<empty>")) {
        val lines = File(filename).lines.toList
        val startLine: Integer = {
          if (lineToHighlight.isDefined)
            Math.max(1, lineToHighlight.get + excerptStartLine)
          else
            0
        }
        val endLine: Integer = {
          if (lineToHighlight.isDefined)
            Math.min(lines.length, lineToHighlight.get + excerptEndLine)
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
      } else ""
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
  def isFileProcessable(filePath: String, ruleCache: RuleCache): Boolean = {
    ruleCache.getRule.exclusions
      .flatMap(exclusionRule => {
        Try(!filePath.matches(exclusionRule.combinedRulePattern)) match {
          case Success(result) => Some(result)
          case Failure(_)      => None
        }
      })
      .foldLeft(true)((a, b) => a && b)
  }

  /** Checks if given sinkName doesn't belong to the sink skip rule file regex
    *
    * @param sinkName
    * @return
    */
  def isPrivacySink(sinkName: String, ruleCache: RuleCache): Boolean = {
    ruleCache.getRule.sinkSkipList
      .flatMap(sinkSkipRule => {
        Try(!sinkName.matches(sinkSkipRule.combinedRulePattern)) match {
          case Success(result) => Some(result)
          case Failure(_)      => None
        }
      })
      .foldLeft(true)((a, b) => a && b)
  }

  /** Returns all files matching the given extensions
    * @param folderPath
    * @param extension
    * @return
    */
  def getAllFilesRecursively(
    folderPath: String,
    extensions: Set[String],
    ruleCache: RuleCache
  ): Option[List[String]] = {
    try {
      if (File(folderPath).isDirectory)
        Some(
          SourceFiles
            .determine(Set(folderPath), extensions)(VisitOptions.default)
            .filter(isFileProcessable(_, ruleCache))
        )
      else
        None
    } catch {
      case e: Exception =>
        logger.debug("Exception ", e)
        None
    }

  }

  /** Returns all files matching the fileName
    *
    * @param folderPath
    * @param fileName
    * @return
    */
  def getAllFilesRecursivelyWithoutExtension(folderPath: String, fileName: String): Option[List[String]] = {
    try {
      val dir = File(folderPath)
      if (dir.isDirectory) {
        val matchingFiles = dir.glob(s"**/$fileName").toList.map(_.pathAsString)
        val subdirectoryFiles = dir.listRecursively
          .filter(_.isDirectory)
          .flatMap(subdir => subdir.glob(fileName).toList.map(_.pathAsString))
          .toList
        val topLevelFile = dir / fileName
        val topLevelFilePath =
          if (topLevelFile.exists && topLevelFile.isRegularFile) Some(topLevelFile.pathAsString) else None
        Some(matchingFiles ++ topLevelFilePath ++ subdirectoryFiles)
      } else {
        None
      }
    } catch {
      case e: Exception =>
        logger.debug("Exception ", e)
        None
    }
  }

  /** Returns the SHA256 hash for a given string.
    * @param value
    * @return
    *   the SHA256 hash for the value
    */
  def getSHA256Hash(value: String): String =
    String.format("%032x", new BigInteger(1, MessageDigest.getInstance("SHA-256").digest(value.getBytes("UTF-8"))))

  /** Returns only rules which belong to the correponding passed language along with Default and Unknown
    * @param rules
    * @param lang
    * @return
    */
  def filterRuleByLanguage(rules: ConfigAndRules, lang: Language.Value): ConfigAndRules = {
    def getRuleByLang(rule: RuleInfo) =
      rule.language == lang || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    def getSemanticRuleByLang(rule: Semantic) =
      rule.language == lang || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    def getSystemConfigByLang(rule: SystemConfig) =
      rule.language == lang || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN

    val sources      = rules.sources.filter(getRuleByLang)
    val sinks        = rules.sinks.filter(getRuleByLang)
    val collections  = rules.collections.filter(getRuleByLang)
    val exclusions   = rules.exclusions.filter(getRuleByLang)
    val semantics    = rules.semantics.filter(getSemanticRuleByLang)
    val sinkSkipList = rules.sinkSkipList.filter(getRuleByLang)
    val systemConfig = rules.systemConfig.filter(getSystemConfigByLang)
    val auditConfig  = rules.auditConfig.filter(getRuleByLang)

    ConfigAndRules(
      sources,
      sinks,
      collections,
      rules.policies,
      rules.threats,
      exclusions,
      semantics,
      sinkSkipList,
      systemConfig,
      auditConfig
    )
  }

  /** Returns file name for a node
    * @param node
    * @return
    */
  def getFileNameForNode(node: AstNode): String = {
    Iterator(node).file.name.headOption.getOrElse(Constants.EMPTY)
  }

  /** Returns a domain for a given url string
    * @param urlString
    * @return
    */
  def getDomainFromString(urlString: String) = {
    try {
      val cleanedUrlString = urlString.replaceAll("'", "").replaceAll("\"", "")
      val prefixToReplace  = if (cleanedUrlString.contains("http://")) "http://" else "https://"
      val url              = new URL("https://" + cleanedUrlString.replaceAll(prefixToReplace, "").trim)
      url.getHost.replaceAll("www.", "").replaceAll("\"", "")
    } catch {
      case e: Exception =>
        logger.debug("Exception while getting domain from string : ", e)
        urlString
    }

  }

  /** Get the domains from complete script tag <Script id="googletag-script" data-testid="googletag-script"
    * src="https://www.googletagservices.com/tag/js/gpt.js" strategy="lazyOnload" />
    *
    * @param templateStr
    * @return
    */
  def getDomainFromTemplates(templateStr: String): (String, String) = {
    val regex =
      """(http:|https:){0,1}\/\/(www.){0,1}([\w_\-]+(?:(?:\.[\w_\-]+)+))([\w.,@?^=%&:\/~+#\-]*[\w@?^=%&\/~+#\-])""".r
    Try(regex.findFirstMatchIn(templateStr).headOption.get) match {
      case Success(matched) =>
        (matched.matched, matched.group(3))
      case Failure(e) =>
        logger.debug("Exception : ", e)
        (Constants.UnknownDomain, Constants.UnknownDomain)
    }
  }

  def getAPIIdentifierFromCode(codeSnippet: String, variablePattern: String): Option[String] = {
    val variableRegex = s"\\b$variablePattern\\b".r
    val words         = codeSnippet.split("\\s+")
    words.find(variableRegex.pattern.matcher(_).matches())
  }

  /** Get list of source files with given file extensions
    *
    * @param projectRoot
    * @param extensions
    * @param ruleCache
    * @return
    */
  def getSourceFilesWithGivenExtension(
    projectRoot: String,
    extensions: Set[String],
    ruleCache: RuleCache
  ): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)(VisitOptions.default)
      .filter(Utilities.isFileProcessable(_, ruleCache))
  }

  /** Add new file node into CPG / Diff Graph
    * @param fileName
    * @param builder
    * @return
    */
  def addFileNode(fileName: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(fileName)
    builder.addNode(fileNode)
    fileNode
  }

  /** Add new node to Diff graph builder as well as establish the edge between new node and file node.
    *
    * @param builder
    * @param node
    * @param fileNode
    */
  def addNodeWithFileEdge(builder: BatchedUpdate.DiffGraphBuilder, node: DetachedNodeData, fileNode: NewFile): Unit = {
    builder.addNode(node)
    builder.addEdge(node, fileNode, EdgeTypes.SOURCE_FILE)
  }

  def semanticFileExporter(sourceRepoLocation: String, headerAndSemanticPairs: Map[String, Seq[String]]): Unit = {
    if (headerAndSemanticPairs.keys.toList.length != headerAndSemanticPairs.values.toList.length) {
      logger.debug(
        "Semantic Exporter failed: Headers and semantics mismatch, please provide matching number of headers and semantics."
      )
      return;
    }

    var runTimeSemanticsString: String = ""
    for ((header, semantics) <- headerAndSemanticPairs) {
      runTimeSemanticsString += header + "\n"
      for (semantic <- semantics) {
        runTimeSemanticsString += semantic + "\n"
      }
      runTimeSemanticsString += "------------------------------------------\n"
    }

    try {
      new PrintWriter(s"${sourceRepoLocation}/$outputDirectoryName/${Constants.outputSemanticFileName}") {
        write(runTimeSemanticsString)
        close()
      }
    } catch {
      case e: Throwable => logger.debug(e.getMessage)
    }

  }

  def databaseURLPriority(url: String, file: String): Priority.Value = {
    val ipPortRegex =
      "^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}:[0-9]{1,4})(:[0-9]{1,4})?$" // For database urls which contain an IP address
    val cloudDomainRegex =
      ".*(amazonaws\\.com|orcalecloud\\.com|azure\\.com|mongodb\\.net).*" // For cloud domains

    val cloudDomainRegexProd = ".*(prd|prod).*(amazonaws\\.com|orcalecloud\\.com|azure\\.com|mongodb\\.net).*"

    val prodFileRegex = ".*(prd|prod).*\\.(properties|yaml|yml|xml|conf)$"

    // Priority - URLs in Prod files > PROD URLS w/ cloud > Cloud URLS > IP Urls > localhost or test urls
    if (file.matches(prodFileRegex)) Priority.MAX
    else if (url.matches(cloudDomainRegexProd)) Priority.HIGHEST
    else if (url.matches(cloudDomainRegex)) Priority.HIGH
    else if (url.matches(ipPortRegex)) Priority.MEDIUM
    else Priority.LOW
  }

  def addDatabaseDetailsMultiple(
    rules: List[(String, String)],
    dbUrl: JavaProperty,
    dbName: String,
    dbLocation: String,
    dbVendor: String
  ): Unit = {
    rules.foreach(rule => {
      if (DatabaseDetailsCache.getDatabaseDetails(rule._2).isDefined) {
        val fileName = dbUrl.file.name.headOption.getOrElse("")
        if (
          databaseURLPriority(
            DatabaseDetailsCache.getDatabaseDetails(rule._1).get.dbLocation,
            DatabaseDetailsCache.getDatabaseDetails(rule._1).get.configFile
          ) < databaseURLPriority(
            dbUrl.value,
            fileName
          ) // Compare the priority of the database url with already present url in the database cache
        ) {

          DatabaseDetailsCache.removeDatabaseDetails(rule._2)
          DatabaseDetailsCache.addDatabaseDetails(
            DatabaseDetails(dbName, dbVendor, dbLocation, rule._1, fileName),
            rule._2
          ) // Remove if current url has higher priority
        }
      } else {
        DatabaseDetailsCache.addDatabaseDetails(
          DatabaseDetails(dbName, dbVendor, dbLocation, rule._1, dbUrl.sourceFileOut.head.name),
          rule._2
        )
      }
    })
  }

  def serializedArgumentString(originalList: List[(String, String)]): String = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val objectOutputStream    = new ObjectOutputStream(byteArrayOutputStream)
    objectOutputStream.writeObject(originalList)
    objectOutputStream.close()
    byteArrayOutputStream.toString("ISO-8859-1")
  }

  def deserializedArgumentString(serializedString: String): List[(String, String)] = {
    val byteArrayInputStream = new ByteArrayInputStream(serializedString.getBytes("ISO-8859-1"))
    val objectInputStream    = new ObjectInputStream(byteArrayInputStream)
    val result               = objectInputStream.readObject().asInstanceOf[List[(String, String)]]
    objectInputStream.close()
    result
  }
}
