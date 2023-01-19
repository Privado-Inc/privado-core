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

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.CatLevelOne.CatLevelOne
import ai.privado.model.{
  CatLevelOne,
  ConfigAndRules,
  Constants,
  DatabaseDetails,
  InternalTag,
  Language,
  RuleInfo,
  Semantic
}
import better.files.File
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  CfgNode,
  FieldIdentifier,
  Identifier,
  Literal,
  Member,
  MethodParameterIn,
  NewTag,
  TypeDecl
}
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.util.{Failure, Success, Try}
import java.nio.file.Paths
import java.util.regex.{Pattern, PatternSyntaxException}
import scala.io.Source
import io.shiftleft.semanticcpg.language._
import ai.privado.semantic.Language.finder
import overflowdb.traversal.Traversal

import java.math.BigInteger
import java.security.MessageDigest
import scala.collection.mutable.ListBuffer

object Utilities {

  implicit val resolver: ICallResolver = NoResolve

  private val logger = LoggerFactory.getLogger(getClass)

  /** Utility to add a single tag to a object
    */
  def storeForTag(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: CfgNode
  )(tagName: String, tagValue: String = ""): BatchedUpdate.DiffGraphBuilder = {
    val fileName = getFileNameForNode(node)
    if (isFileProcessable(fileName)) {
      builder.addEdge(node, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
    }
    builder
  }

  /** Utility to add database detail tags to sink
    */
  def addDatabaseDetailTags(
    builder: BatchedUpdate.DiffGraphBuilder,
    node: CfgNode,
    databaseDetails: DatabaseDetails
  ): Unit = {
    val storeForTagHelper = storeForTag(builder, node) _
    storeForTagHelper(Constants.dbName, databaseDetails.dbName)
    storeForTagHelper(Constants.dbVendor, databaseDetails.dbVendor)
    storeForTagHelper(Constants.dbLocation, databaseDetails.dbLocation)
    storeForTagHelper(Constants.dbOperation, databaseDetails.dbOperation)
  }

  /** Utility to add Tag based on a rule Object
    */
  def addRuleTags(builder: BatchedUpdate.DiffGraphBuilder, node: CfgNode, ruleInfo: RuleInfo): Unit = {
    val fileName = getFileNameForNode(node)
    if (isFileProcessable(fileName)) {
      val storeForTagHelper = storeForTag(builder, node) _
      storeForTagHelper(Constants.id, ruleInfo.id)
      storeForTagHelper(Constants.nodeType, ruleInfo.nodeType.toString)
      storeForTagHelper(Constants.catLevelOne, ruleInfo.catLevelOne.name)
      storeForTagHelper(Constants.catLevelTwo, ruleInfo.catLevelTwo)

      MetricHandler.totalRulesMatched.addOne(ruleInfo.id)
      RuleCache.internalRules.get(ruleInfo.id) match {
        case Some(_) => MetricHandler.internalRulesMatched.addOne(ruleInfo.id)
        case _       => ()
      }
      // storing by catLevelTwo and nodeType to get id
      storeForTagHelper(ruleInfo.catLevelTwo + ruleInfo.nodeType.toString, ruleInfo.id)
    }
  }

  /** Utility to get the default semantics for dataflow queries
    * @return
    */
  def getDefaultSemantics: Semantics = {
    val semanticsFilename = Source.fromResource("default.semantics")
    Semantics.fromList(new Parser().parse(semanticsFilename.getLines().mkString("\n")))
  }

  /** Utility to get the semantics (default + custom) using cpg for dataflow queries
    *
    * @param cpg
    *   \- cpg for adding customSemantics
    * @return
    */
  def getSemantics(cpg: Cpg): Semantics = {
    val semanticsFilename = Source.fromResource("default.semantics")

    val defaultSemantics = semanticsFilename.getLines().toList
    val customSinkSemantics = cpg.call
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .methodFullName
      .dedup
      .l
      .map(generateSemanticForTaint(_, -1))

    val nonTaintingMethods = cpg.method.where(_.callIn).isExternal(true).fullName(".*:(void|boolean|long|int)\\(.*").l

    var customNonTaintDefaultSemantics   = List[String]()
    var specialNonTaintDefaultSemantics  = List[String]()
    var customStringSemantics            = List[String]()
    var customNonPersonalMemberSemantics = List[String]()

    if (!ScanProcessor.config.disableRunTimeSemantics) {
      customNonTaintDefaultSemantics = nonTaintingMethods
        .fullNameNot(".*\\.(add|put|<init>|set|get|append|store|insert|update|merge).*")
        .fullName
        .l
        .map(generateNonTaintSemantic)

      specialNonTaintDefaultSemantics = nonTaintingMethods
        .fullName(".*\\.(add|put|set|get|append|store|insert|update|merge).*")
        .fullName
        .l
        .map(generateSemanticForTaint(_, 0))

      customStringSemantics = cpg.method
        .filter(_.isExternal)
        .where(_.callIn)
        .fullName(".*:java.lang.String\\(.*")
        .fullNameNot(".*\\.set[A-Za-z_]*:.*")
        .fullName
        .dedup
        .l
        .map(generateSemanticForTaint(_, -1))

      customNonPersonalMemberSemantics = generateNonPersonalMemberSemantics(cpg)
    }
    val semanticFromConfig = RuleCache.getRule.semantics.flatMap(generateSemantic)

    logger.debug("\nCustom Non taint default semantics")
    customNonTaintDefaultSemantics.foreach(logger.debug)
    logger.debug("\nCustom specialNonTaintDefaultSemantics semantics")
    specialNonTaintDefaultSemantics.foreach(logger.debug)
    logger.debug("\nCustom customStringSemantics semantics")
    customStringSemantics.foreach(logger.debug)
    logger.debug("\nCustom customNonPersonalMemberSemantics semantics")
    customNonPersonalMemberSemantics.foreach(logger.debug)
    logger.debug("\nCustom customSinkSemantics semantics")
    customSinkSemantics.foreach(logger.debug)
    logger.debug("\nCustom semanticFromConfig semantics")
    semanticFromConfig.foreach(logger.debug)

    val finalSemantics =
      (defaultSemantics ++ customNonTaintDefaultSemantics ++ specialNonTaintDefaultSemantics
        ++ customStringSemantics ++ customNonPersonalMemberSemantics
        ++ customSinkSemantics ++ semanticFromConfig)
        .mkString("\n")

    Semantics.fromList(new Parser().parse(finalSemantics))
  }

  /** Utility to filter rules by catLevelOne
    */
  def getRulesByCatLevelOne(rules: List[RuleInfo], catLevelOne: CatLevelOne): Seq[RuleInfo] =
    rules.filter(rule => rule.catLevelOne.equals(catLevelOne))

  /** For a given `filename`, `lineToHighlight`, return the corresponding code by reading it from the file. If
    * `lineToHighlight` is defined, then a line containing an arrow (as a source code comment) is included right before
    * that line.
    */
  def dump(filename: String, lineToHighlight: Option[Integer], message: String = ""): String = {
    val arrow: CharSequence = "/* <=== " + message + " */ "
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
  def isFileProcessable(filePath: String): Boolean = {
    RuleCache.getRule.exclusions
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
  def isPrivacySink(sinkName: String): Boolean = {
    RuleCache.getRule.sinkSkipList
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
  def getAllFilesRecursively(folderPath: String, extensions: Set[String]): Option[List[String]] = {
    try {
      if (File(folderPath).isDirectory)
        Some(SourceFiles.determine(Set(folderPath), extensions).filter(isFileProcessable))
      else
        None
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

  /** Generate semantics for tainting passed argument based on the number of parameter in method signature
    * @param methodName
    *   \- complete signature of method
    * @return
    *   \- semantic string
    */
  private def generateSemanticForTaint(methodName: String, toTaint: Int) = {
    var parameterSemantics = ""
    var parameterNumber    = 2
    if (methodName.matches(".*:<unresolvedSignature>\\(\\d+\\).*")) {
      parameterNumber = 7
    } else {
      parameterNumber = methodName.count(_.equals(','))
    }
    for (i <- 0 to (parameterNumber + 1))
      parameterSemantics += s"$i->$toTaint "
    "\"" + methodName + "\" " + parameterSemantics.trim
  }

  /** Generate Semantic string based on input Semantic
    * @param semantic
    *   \- semantic object containing semantic information
    * @return
    */
  private def generateSemantic(semantic: Semantic) = {
    if (semantic.signature.nonEmpty) {
      val generatedSemantic = "\"" + semantic.signature.trim + "\" " + semantic.flow
      Some(generatedSemantic.trim)
    } else
      None
  }

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

    val sources      = rules.sources.filter(getRuleByLang)
    val sinks        = rules.sinks.filter(getRuleByLang)
    val collections  = rules.collections.filter(getRuleByLang)
    val exclusions   = rules.exclusions.filter(getRuleByLang)
    val semantics    = rules.semantics.filter(getSemanticRuleByLang)
    val sinkSkipList = rules.sinkSkipList.filter(getRuleByLang)

    ConfigAndRules(sources, sinks, collections, rules.policies, rules.threats, exclusions, semantics, sinkSkipList)
  }

  /** Returns file name for a node
    * @param node
    * @return
    */
  def getFileNameForNode(node: AstNode): String = {
    Traversal(node).file.name.headOption.getOrElse(Constants.EMPTY)
  }

  /** Returns the Semantics for functions with void return type Default behavior is not propagating the taint
    * @param String
    *   methodFullName
    * @return
    *   String
    */
  private def generateNonTaintSemantic(methodFullName: String): String = {
    "\"" + methodFullName + "\" "
  }

  /** Generates Semantics for non Personal member
    * @param cpg
    * @return
    *   non-tainting semantic rule
    */
  def generateNonPersonalMemberSemantics(cpg: Cpg): List[String] = {

    val semanticList = ListBuffer[String]()

    TaggerCache.typeDeclMemberCache.keys.foreach(typeDeclValue => {
      val typeDeclNode       = cpg.typeDecl.where(_.fullName(typeDeclValue)).l
      val allMembers         = typeDeclNode.member.name.toSet
      val personalMembers    = TaggerCache.typeDeclMemberCache(typeDeclValue).values.name.toSet
      val nonPersonalMembers = allMembers.diff(personalMembers)

      val nonPersonalMembersRegex = nonPersonalMembers.mkString("|")
      if (nonPersonalMembersRegex.nonEmpty) {
        typeDeclNode.method.block.astChildren.isReturn
          .code("(?i).*(" + nonPersonalMembersRegex + ").*")
          .method
          .callIn
          .whereNot(_.tag.nameExact(InternalTag.SENSITIVE_METHOD_RETURN.toString))
          .methodFullName
          .dedup
          .foreach(methodName => {
            semanticList.addOne(generateNonTaintSemantic(methodName))
          })
      }
    })
    semanticList.toList
  }
}
