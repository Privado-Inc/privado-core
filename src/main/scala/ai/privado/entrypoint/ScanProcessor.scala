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

import ai.privado.cache.{AppCache, Environment, RuleCache}
import ai.privado.languageEngine.java.processor.JavaProcessor
import ai.privado.languageEngine.javascript.processor.JavascriptProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model._
import ai.privado.rulevalidator.YamlFileValidator
import ai.privado.utility.Utilities.isValidRule
import better.files.File
import io.circe.Json
import io.circe.yaml.parser
import io.joern.console.cpgcreation.guessLanguage
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import java.util.Calendar
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
      List[RuleInfo]()
    )

  def parseRules(rulesPath: String, lang: String): ConfigAndRules = {
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
    val langToFilter = lang match {
      case Languages.JAVASRC => Language.JAVA
      case Languages.JSSRC   => Language.JAVASCRIPT
      case _                 => Language.JAVA
    }
    def filterByLang(rule: RuleInfo): Boolean =
      rule.language == langToFilter || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    def filterSemanticByLang(rule: Semantic): Boolean =
      rule.language == langToFilter || rule.language == Language.DEFAULT || rule.language == Language.UNKNOWN
    val parsedRules =
      try
        ir.listRecursively
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
                        .filter(filterByLang)
                    )
                  case Left(error) =>
                    logger.error("Error while parsing this file -> '" + fullPath)
                    logger.error("ERROR : " + error)
                    getEmptyConfigAndRule
                }
              case Left(error) =>
                logger.error("Error while parsing this file -> '" + fullPath)
                logger.error("ERROR : " + error)
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
              sinkSkipList = a.sinkSkipList ++ b.sinkSkipList
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

  def processRules(lang: String): ConfigAndRules = {
    var internalConfigAndRules = getEmptyConfigAndRule
    if (!config.ignoreInternalRules) {
      internalConfigAndRules = parseRules(config.internalConfigPath.head, lang)
      RuleCache.setInternalRules(internalConfigAndRules)

      try {
        AppCache.privadoVersionMain = File((s"${config.internalConfigPath.head}/version.txt")).contentAsString
      } catch {
        case _: Exception =>
          AppCache.privadoVersionMain = Constants.notDetected
      }
      println(s"Privado Main Version: ${AppCache.privadoVersionMain}")
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
    val mergedRules =
      ConfigAndRules(
        sources = sources.distinctBy(_.id),
        sinks = sinks.distinctBy(_.id),
        collections = collections.distinctBy(_.id),
        policies = policies.distinctBy(_.id),
        exclusions = exclusions.distinctBy(_.id),
        threats = threats.distinctBy(_.id),
        semantics = semantics.distinctBy(_.signature),
        sinkSkipList = sinkSkipList.distinctBy(_.id)
      )
    logger.trace(mergedRules.toString)
    println(s"${Calendar.getInstance().getTime} - Configuration parsed...")

    RuleCache.internalPolicies.addAll(internalConfigAndRules.policies.map(policy => (policy.id)))
    RuleCache.internalPolicies.addAll(internalConfigAndRules.threats.map(threat => (threat.id)))
    MetricHandler.metricsData("noOfRulesUsed") = {
      Json.fromInt(
        mergedRules.sources.size +
          mergedRules.sinks.size +
          mergedRules.collections.size +
          mergedRules.policies.size +
          mergedRules.exclusions.size
      )
    }

    mergedRules
  }
  override def process(): Either[String, Unit] = {
    println(s"Privado CLI Version: ${Environment.privadoVersionCli.getOrElse(Constants.notDetected)}")
    println(s"Privado Core Version: ${Environment.privadoVersionCore}")
    if (!File(config.sourceLocation.head).isWritable) {
      println(s"Warning: Privado doesn't have write permission on give repo location - ${config.sourceLocation.head}")
    }
    processCpg()
  }

  /** Helper function to process rule for a language and cache the result in ruleCache
    * @param lang
    * @return
    *   rule
    */
  def processAndCacheRule(lang: String): ConfigAndRules = {
    val processedRules = processRules(lang)
    logger.info("Caching rules")
    RuleCache.setRule(processedRules)
    processedRules
  }

  private def processCpg() = {
    val sourceRepoLocation = config.sourceLocation.head
    // Setting up the application cache
    AppCache.init(sourceRepoLocation)
    Try(guessLanguage(sourceRepoLocation)) match {
      case Success(languageDetected) => {
        println(
          s"${TimeMetric.getNewTime()} - Language detection done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
        )
        languageDetected match {
          case Some(lang) =>
            MetricHandler.metricsData("language") = Json.fromString(lang)
            lang match {
              case language if language == Languages.JAVASRC || language == Languages.JAVA =>
                println(s"${Calendar.getInstance().getTime} - Detected language 'Java'")
                JavaProcessor.createJavaCpg(processAndCacheRule(lang), sourceRepoLocation, language)
              case language if language == Languages.JSSRC && config.enableJS =>
                println(s"${Calendar.getInstance().getTime} - Detected language 'JavaScript'")
                JavascriptProcessor.createJavaScriptCpg(processAndCacheRule(lang), sourceRepoLocation, lang)
              case _ =>
                if (checkJavaSourceCodePresent(sourceRepoLocation)) {
                  println(
                    s"We detected presence of 'Java' code base along with other major language code base '${lang}'."
                  )
                  println(s"However we only support 'Java' code base scanning as of now.")
                  JavaProcessor.createJavaCpg(processAndCacheRule(Languages.JAVASRC), sourceRepoLocation, lang)
                } else {
                  println(s"As of now we only support privacy code scanning for 'Java' code base.")
                  println(s"We detected this code base of '${lang}'.")
                  exit(1)
                }
            }
          case _ =>
            logger.error("Unable to detect language! Is it supported yet?")
            Left("Unable to detect language!")
        }
      }
      case Failure(exc) =>
        logger.debug("Error while guessing language", exc)
        println(s"Error Occurred: ${exc.getMessage}")
        exit(1)
    }
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

  override var config: PrivadoInput = _
}
