package ai.privado.inputprocessor

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.metric.MetricHandler
import ai.privado.model.*
import ai.privado.model.Language.Language
import ai.privado.rulevalidator.YamlFileValidator
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.{isValidDEDRule, isValidRule}
import better.files.File
import io.circe.Json
import io.circe.yaml.parser
import org.slf4j.LoggerFactory

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.sys.exit

trait RuleProcessor extends DynamicRuleMerger {

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
      List[RuleInfo](),
      List[RuleInfo](),
      List[DEDRuleInfo]()
    )

  /** Helper function to process rule for a language
    *
    * @param lang
    * @return
    *   processed rules
    */
  def getProcessedRule(
    lang: Set[Language],
    appCache: AppCache,
    statsRecorder: StatsRecorder,
    config: PrivadoInput
  ): RuleCache = {
    appCache.repoLanguage =
      lang.head // we are caching the repo language here, and we will use this to get the repo's lang
    val ruleCache      = new RuleCache()
    val processedRules = processRules(lang, ruleCache, appCache, statsRecorder, config)
    ruleCache.setRule(processedRules)
    ruleCache
  }

  def parseRules(rulesPath: String, lang: Set[Language], isExternal: Boolean = false): ConfigAndRules = {
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

    def filterDEDByLang(rule: DEDRuleInfo): Boolean =
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
                import ai.privado.model.CirceEnDe.*
                json.as[ConfigAndRules] match {
                  case Right(configAndRules: ConfigAndRules) =>
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
                            nodeType = NodeType.REGULAR,
                            isExternal = isExternal
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
                            nodeType = NodeType.withNameWithDefault(pathTree.apply(3)),
                            isExternal = isExternal
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
                            nodeType = NodeType.REGULAR,
                            isExternal = isExternal
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
                            nodeType = NodeType.withNameWithDefault(pathTree.apply(3)),
                            isExternal = isExternal
                          )
                        )
                        .filter(filterByLang),
                      dedRules = configAndRules.dedRules
                        .filter(rule => isValidDEDRule(rule))
                        .map(x =>
                          x.copy(
                            file = fullPath,
                            catLevelOne = CatLevelOne.DED,
                            catLevelTwo = pathTree.apply(1),
                            categoryTree = pathTree,
                            language = Language.withNameWithDefault(pathTree.last),
                            nodeType = NodeType.withNameWithDefault(pathTree.apply(2)),
                            isExternal = isExternal
                          )
                        )
                        .filter(filterDEDByLang)
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
              inferences = a.inferences ++ b.inferences,
              dedRules = a.dedRules ++ b.dedRules
            )
          )
      catch {
        case ex: Throwable =>
          logger.error("File error: ", ex)
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

  def processRules(
    lang: Set[Language],
    ruleCache: RuleCache,
    appCache: AppCache,
    statsRecorder: StatsRecorder,
    config: PrivadoInput
  ): ConfigAndRules = {
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
      externalConfigAndRules = parseRules(config.externalConfigPath.head, lang, isExternal = true)
    }
    if (appCache.excludeFileRegex.isDefined && appCache.excludeFileRegex.get.nonEmpty) {
      val excludeFileRegexRule = RuleInfo(
        "PrivadoInput.Exclusion",
        "Command Line Exclusion Rule",
        "",
        FilterProperty.CODE,
        Array.empty,
        List(appCache.excludeFileRegex.get)
      )
      externalConfigAndRules =
        externalConfigAndRules.copy(exclusions = externalConfigAndRules.exclusions.appended(excludeFileRegexRule))
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
    val exclusions = externalConfigAndRules.exclusions ++ internalConfigAndRules.exclusions
    val sources    = externalConfigAndRules.sources ++ internalConfigAndRules.sources
    val sinks =
      mergeDynamicRuleSinkForDependencyDiscovery(externalConfigAndRules.sinks, internalConfigAndRules.sinks, ruleCache)
    val collections  = externalConfigAndRules.collections ++ internalConfigAndRules.collections
    val policies     = externalConfigAndRules.policies ++ internalConfigAndRules.policies
    val threats      = externalConfigAndRules.threats ++ internalConfigAndRules.threats
    val semantics    = externalConfigAndRules.semantics ++ internalConfigAndRules.semantics
    val sinkSkipList = externalConfigAndRules.sinkSkipList ++ internalConfigAndRules.sinkSkipList
    val systemConfig = externalConfigAndRules.systemConfig ++ internalConfigAndRules.systemConfig
    val auditConfig  = externalConfigAndRules.auditConfig ++ internalConfigAndRules.auditConfig
    val inferences   = externalConfigAndRules.inferences ++ internalConfigAndRules.inferences
    val dedRules     = externalConfigAndRules.dedRules ++ internalConfigAndRules.dedRules
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
        inferences = mergePatterns(inferences),
        dedRules = dedRules
      )
    logger.trace(mergedRules.toString)
    statsRecorder.justLogMessage(s"- Configuration parsed...")

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
          mergedRules.inferences.size +
          mergedRules.dedRules.size
      )
    }
    statsRecorder.endLastStage()
    mergedRules
  }

}
