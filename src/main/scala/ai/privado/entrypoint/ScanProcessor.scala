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

package ai.privado.entrypoint

import ai.privado.cache.*
import ai.privado.entrypoint.ScanProcessor.statsRecorder
import ai.privado.languageEngine.csharp.processor.CSharpProcessor
import ai.privado.languageEngine.default.processor.DefaultProcessor
import ai.privado.languageEngine.go.processor.GoProcessor
import ai.privado.languageEngine.java.processor.JavaProcessor
import ai.privado.languageEngine.javascript.processor.JavascriptProcessor
import ai.privado.languageEngine.kotlin.processor.KotlinProcessor
import ai.privado.languageEngine.php.processor.PhpProcessor
import ai.privado.languageEngine.python.processor.PythonProcessor
import ai.privado.languageEngine.ruby.processor.RubyProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.*
import ai.privado.model.Language.{Language, UNKNOWN}
import ai.privado.rulevalidator.YamlFileValidator
import ai.privado.utility.Utilities.isValidRule
import better.files.File
import io.circe.Json
import io.circe.yaml.parser
import io.joern.console.cpgcreation.guessLanguage
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory
import privado_core.BuildInfo
import ai.privado.languageEngine.csharp.processor.CSharpProcessor
import ai.privado.utility.StatsRecorder
import io.joern.x2cpg.SourceFiles

import java.util.Calendar
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.sys.exit
import scala.util.{Failure, Success, Try}

object ScanProcessor extends CommandProcessor {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def getEmptyConfigAndRule: ConfigAndRules =
    ConfigAndRules(
      List[RuleInfo](),
      List[RuleInfo](),
      List[RuleInfo](),
      List[PolicyOrThreat](),
      List[PolicyOrThreat](),
      List[RuleInfo](),
      List[Semantic](),
      List[RuleInfo](),
      List[SystemConfig](),
      List[RuleInfo]()
    )

  def parseRules(rulesPath: String, lang: Set[Language]): ConfigAndRules = {
    logger.trace(s"parsing rules from -> '$rulesPath'")
    val ir: File = {
      // e.g. rulesPath = /home/pandurang/projects/rules-home/
      try File(rulesPath)
      catch {
        case ex: Throwable =>
          logger.debug("File error: ", ex)
          logger.error(s"Exception while processing rules on path $rulesPath")
          exit(1)
      }
    }

    def filterByLang(rule: RuleInfo): Boolean =
      lang.contains(rule.language) || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    def filterSemanticByLang(rule: Semantic): Boolean =
      lang.contains(rule.language) || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    def filterSystemConfigByLang(rule: SystemConfig): Boolean =
      lang.contains(rule.language) || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    val parsedRules =
      try
        ir.listRecursively.toList.par
          .filter(f =>
            ((f.extension(toLowerCase = true).toString.contains(".yaml") ||
              f.extension(toLowerCase = true).toString.contains(".yml")) &&
              YamlFileValidator.isValidRuleFile(f, ir))
          )
          .map(file => {
            // e.g. fullPath = /home/pandurang/projects/rules-home/rules/sources/accounts.yaml
            val fullPath = file.pathAsString
            logger.trace(s"parsing -> '$fullPath'")
            // e.g. relPath = rules/sources/accounts
            val relPath  = fullPath.substring(ir.pathAsString.length + 1).split("\\.").head
            val pathTree = relPath.split("/")
            parser.parse(file.contentAsString) match {
              case Right(json) =>
                import ai.privado.model.CirceEnDe._
                json.as[ConfigAndRules] match {
                  case Right(configAndRules) =>
                    configAndRules.copy(
                      exclusions = configAndRules.exclusions
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last)
                          )
                        )
                        .filter(filterByLang),
                      sources = configAndRules.sources
                        .filter(rule => isValidRule(rule.combinedRulePattern, rule.id, fullPath))
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            catLevelOne = CatLevelOne.withNameWithDefault(pathTree.apply(1)),
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last),
                            nodeType = NodeType.REGULAR
                          )
                        )
                        .filter(filterByLang),
                      sinks = configAndRules.sinks
                        .filter(rule => isValidRule(rule.combinedRulePattern, rule.id, fullPath))
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            catLevelOne = CatLevelOne.withNameWithDefault(pathTree.apply(1)),
                            catLevelTwo = pathTree.apply(2),
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last),
                            nodeType = NodeType.withNameWithDefault(pathTree.apply(3))
                          )
                        )
                        .filter(filterByLang),
                      collections = configAndRules.collections
                        .filter(rule => isValidRule(rule.combinedRulePattern, rule.id, fullPath))
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            catLevelOne = CatLevelOne.withNameWithDefault(pathTree.apply(1)),
                            catLevelTwo = pathTree.apply(2),
                            categoryTree = pathTree,
                            nodeType = NodeType.REGULAR
                          )
                        )
                        .filter(filterByLang),
                      policies = configAndRules.policies.map(x => x.copy(file = fullPath, categoryTree = pathTree)),
                      semantics = configAndRules.semantics
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last)
                          )
                        )
                        .filter(filterSemanticByLang),
                      sinkSkipList = configAndRules.sinkSkipList
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last)
                          )
                        )
                        .filter(filterByLang),
                      systemConfig = configAndRules.systemConfig
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last)
                          )
                        )
                        .filter(filterSystemConfigByLang),
                      auditConfig = configAndRules.auditConfig
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            categoryTree = pathTree,
                            catLevelTwo = pathTree.apply(2),
                            language = Language.withNameWithDefault(pathTree.last)
                          )
                        )
                        .filter(filterByLang),
                      inferences = configAndRules.inferences
                        .filter(rule => isValidRule(rule.combinedRulePattern, rule.id, fullPath))
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            catLevelOne = CatLevelOne.INFERENCES,
                            catLevelTwo = pathTree.apply(2),
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last),
                            nodeType = NodeType.withNameWithDefault(pathTree.apply(3))
                          )
                        )
                        .filter(filterByLang)
                    )
                  case Left(error) =>
                    logger.error("Error while parsing this file -> '" + fullPath)
                    logger.error("ERROR : ", error)
                    getEmptyConfigAndRule
                }
              case Left(error) =>
                logger.error("Error while parsing this file -> '" + fullPath)
                logger.error("ERROR : ", error)
                getEmptyConfigAndRule
            }
          })
          .foldLeft(getEmptyConfigAndRule)((a, b) =>
            a.copy(
              sources = a.sources ++ b.sources,
              sinks = a.sinks ++ b.sinks,
              collections = a.collections ++ b.collections,
              policies = a.policies ++ b.policies,
              exclusions = a.exclusions ++ b.exclusions,
              threats = a.threats ++ b.threats,
              semantics = a.semantics ++ b.semantics,
              sinkSkipList = a.sinkSkipList ++ b.sinkSkipList,
              systemConfig = a.systemConfig ++ b.systemConfig,
              auditConfig = a.auditConfig ++ b.auditConfig,
              inferences = a.inferences ++ b.inferences
            )
          )
      catch {
        case ex: Throwable =>
          logger.debug("File error: ", ex)
          logger.error(s"Rules path $rulesPath is not accessible")
          exit(1)
      }
    parsedRules
  }
  def mergePatterns(ruleInfoList: List[RuleInfo]): List[RuleInfo] = {
    ruleInfoList
      .groupBy(_.id)
      .map { case (_, item) =>
        val combinedPatterns = item.flatMap(_.patterns)
        item.head.copy(patterns = combinedPatterns)
      }
      .toList
  }
  def processRules(lang: Set[Language], ruleCache: RuleCache, appCache: AppCache): ConfigAndRules = {
    statsRecorder.initiateNewStage("Processing rules")
    var internalConfigAndRules = getEmptyConfigAndRule
    if (!config.ignoreInternalRules) {
      try {
        appCache.privadoVersionMain = File((s"${config.internalConfigPath.head}/version.txt")).contentAsString
      } catch {
        case _: Exception =>
          appCache.privadoVersionMain = Constants.notDetected
      }
      println(s"Privado Main Version: ${appCache.privadoVersionMain}")
      internalConfigAndRules = parseRules(config.internalConfigPath.head, lang)
      ruleCache.setInternalRules(internalConfigAndRules)
    }
    var externalConfigAndRules = getEmptyConfigAndRule
    if (config.externalConfigPath.nonEmpty) {
      externalConfigAndRules = parseRules(config.externalConfigPath.head, lang)
    }
    /*
     * NOTE: We want to override the external rules over internal in case of duplicates by id.
     * While concatenating two lists (internal and external) and get the distinct list of elements.
     * Elements from the first collection will be kept and elements from second collection will be discarded.
     *
     * e.g
     * val sources     = externalRules.sources ++ internalRules.sources
     * sources.distinctBy(_.id) - this will return unique list of elements duplicated by id.
     * In case of duplicates it will keep the elements from "externalRules.sources".
     * We don't know the internal logic. We came to this conclusion based on testing few samples.
     */
    val exclusions   = externalConfigAndRules.exclusions ++ internalConfigAndRules.exclusions
    val sources      = externalConfigAndRules.sources ++ internalConfigAndRules.sources
    val sinks        = externalConfigAndRules.sinks ++ internalConfigAndRules.sinks
    val collections  = externalConfigAndRules.collections ++ internalConfigAndRules.collections
    val policies     = externalConfigAndRules.policies ++ internalConfigAndRules.policies
    val threats      = externalConfigAndRules.threats ++ internalConfigAndRules.threats
    val semantics    = externalConfigAndRules.semantics ++ internalConfigAndRules.semantics
    val sinkSkipList = externalConfigAndRules.sinkSkipList ++ internalConfigAndRules.sinkSkipList
    val systemConfig = externalConfigAndRules.systemConfig ++ internalConfigAndRules.systemConfig
    val auditConfig  = externalConfigAndRules.auditConfig ++ internalConfigAndRules.auditConfig
    val inferences   = externalConfigAndRules.inferences ++ internalConfigAndRules.inferences
    val mergedRules =
      ConfigAndRules(
        sources = mergePatterns(sources),
        sinks = mergePatterns(sinks),
        collections = mergePatterns(collections),
        policies = policies.distinctBy(_.id),
        exclusions = mergePatterns(exclusions),
        threats = threats.distinctBy(_.id),
        semantics = semantics.distinctBy(_.signature),
        sinkSkipList = sinkSkipList.distinctBy(_.id),
        systemConfig = systemConfig,
        auditConfig = auditConfig.distinctBy(_.id),
        inferences = mergePatterns(inferences)
      )
    logger.trace(mergedRules.toString)
    println(s"${Calendar.getInstance().getTime} - Configuration parsed...")

    ruleCache.internalPolicies.addAll(internalConfigAndRules.policies.map(policy => (policy.id)))
    ruleCache.internalPolicies.addAll(internalConfigAndRules.threats.map(threat => (threat.id)))
    MetricHandler.metricsData("noOfRulesUsed") = {
      Json.fromInt(
        mergedRules.sources.size +
          mergedRules.sinks.size +
          mergedRules.collections.size +
          mergedRules.policies.size +
          mergedRules.exclusions.size +
          mergedRules.auditConfig.size +
          mergedRules.inferences.size
      )
    }
    statsRecorder.endLastStage()
    mergedRules
  }
  override def process(appCache: AppCache): Either[String, Unit] = {
    println(s"Privado CLI Version: ${Environment.privadoVersionCli.getOrElse(Constants.notDetected)}")
    println(s"Privado Core Version: ${Environment.privadoVersionCore}")
    println(s"Privado Language Engine Version: ${BuildInfo.joernVersion}")
    if (!File(config.sourceLocation.head).isWritable) {
      println(s"Warning: Privado doesn't have write permission on give repo location - ${config.sourceLocation.head}")
    }
    processCpg(appCache)
  }

  private def getAuditCache: AuditCache = {
    new AuditCache()
  }

  private def getS3DatabaseDetailsCache: S3DatabaseDetailsCache = {
    new S3DatabaseDetailsCache()
  }

  private val auditCache             = new AuditCache
  private val s3DatabaseDetailsCache = new S3DatabaseDetailsCache
  private val propertyFilterCache    = new PropertyFilterCache()
  private def getDataflowCache: DataFlowCache = {
    new DataFlowCache(config, auditCache)
  }

  /** Helper function to process rule for a language
    * @param lang
    * @return
    *   processed rules
    */
  def getProcessedRule(lang: Set[Language], appCache: AppCache): RuleCache = {
    appCache.repoLanguage =
      lang.head // we are caching the repo language here, and we will use this to get the repo's lang
    val ruleCache      = new RuleCache()
    val processedRules = processRules(lang, ruleCache, appCache)
    ruleCache.setRule(processedRules)
    ruleCache
  }

  private def processCpg(appCache: AppCache): Either[String, Unit] = {
    val sourceRepoLocation = File(config.sourceLocation.head).path.toAbsolutePath.toString.stripSuffix("/")
    // Setting up the application cache
    appCache.init(sourceRepoLocation)
    statsRecorder.initiateNewStage("Language detection")
    val languageDetected = if (config.forceLanguage == UNKNOWN) {
      println(
        s"${TimeMetric.getNewTime()} - Language detection done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      Language.withJoernLangName(Try(guessLanguage(sourceRepoLocation)))
    } else {
      println(s"${TimeMetric.getNewTime()} - Language forced ...")
      config.forceLanguage
    }
    MetricHandler.metricsData("language") = Json.fromString(languageDetected.toString)

    languageDetected match {
      case Language.JAVA =>
        statsRecorder.justLogMessage("Detected language 'Java'")
        val kotlinPlusJavaRules = getProcessedRule(Set(Language.KOTLIN, Language.JAVA), appCache)
        val filesWithKtExtension = SourceFiles.determine(
          sourceRepoLocation,
          Set(".kt"),
          ignoredFilesRegex = Option(kotlinPlusJavaRules.getExclusionRegex.r)
        )
        if (filesWithKtExtension.isEmpty)
          JavaProcessor(
            getProcessedRule(Set(Language.JAVA), appCache),
            this.config,
            sourceRepoLocation,
            dataFlowCache = getDataflowCache,
            auditCache,
            s3DatabaseDetailsCache,
            appCache,
            propertyFilterCache = propertyFilterCache,
            statsRecorder = statsRecorder
          ).processCpg()
        else
          KotlinProcessor(
            kotlinPlusJavaRules,
            this.config,
            sourceRepoLocation,
            dataFlowCache = getDataflowCache,
            auditCache,
            s3DatabaseDetailsCache,
            appCache,
            propertyFilterCache = propertyFilterCache,
            statsRecorder = statsRecorder
          ).processCpg()
      case Language.JAVASCRIPT =>
        statsRecorder.justLogMessage("Detected language 'JavaScript'")
        JavascriptProcessor(
          getProcessedRule(Set(Language.JAVASCRIPT), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).createJavaScriptCpg()
      case Language.PYTHON =>
        statsRecorder.justLogMessage("Detected language 'Python'")
        PythonProcessor(
          getProcessedRule(Set(Language.PYTHON), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).createPythonCpg()
      case Language.RUBY =>
        statsRecorder.justLogMessage("Detected language 'Ruby'")
        RubyProcessor(
          getProcessedRule(Set(Language.RUBY), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).createRubyCpg()
      case Language.GO =>
        statsRecorder.justLogMessage("Detected language 'Go'")
        GoProcessor(
          getProcessedRule(Set(Language.GO), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).createGoCpg()
      case Language.KOTLIN =>
        statsRecorder.justLogMessage("Detected language 'Kotlin'")
        KotlinProcessor(
          getProcessedRule(Set(Language.KOTLIN, Language.JAVA), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).processCpg()
      case Language.CSHARP =>
        statsRecorder.justLogMessage("Detected language 'C#'")
        CSharpProcessor(
          getProcessedRule(Set(Language.CSHARP), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        ).processCpg()
      case Language.PHP =>
        statsRecorder.justLogMessage("Detected language 'PHP'")
        PhpProcessor(
          getProcessedRule(Set(Language.PHP), appCache),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder
        )
          .processCpg()
      case _ =>
        processCpgWithDefaultProcessor(sourceRepoLocation, appCache)
    } match {
      case Left(err: String) => Left(err)
      case _ =>
        Right(
          ()
        ) // Ignore the result as not needed for further step, and due to discrepency in output for New and old frontends
    }
  }

  private def processCpgWithDefaultProcessor(
    sourceRepoLocation: String,
    appCache: AppCache,
    statsRecorder: StatsRecorder
  ) = {
    MetricHandler.metricsData("language") = Json.fromString("default")
    statsRecorder.justLogMessage("Running scan with default processor.")
    DefaultProcessor(
      getProcessedRule(Set(Language.UNKNOWN), appCache),
      this.config,
      sourceRepoLocation,
      getDataflowCache,
      getAuditCache,
      getS3DatabaseDetailsCache,
      appCache,
      propertyFilterCache = propertyFilterCache,
      statsRecorder = statsRecorder
    ).processCpg()
  }

  private def checkJavaSourceCodePresent(sourcePath: String): Boolean = {
    logger.trace(s"parsing rules from -> '${sourcePath}'")
    val sourceLocation: File = {
      try File(sourcePath)
      catch {
        case ex: Throwable =>
          logger.debug("File error: ", ex)
          logger.error(s"Exception while reading source location '$sourcePath'")
          exit(1)
      }
    }
    sourceLocation.listRecursively.count(f => f.extension(toLowerCase = true).toString.contains(".java")) > 0
  }
}
