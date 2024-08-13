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
import ai.privado.languageEngine.c.processor.CProcessor
import ai.privado.languageEngine.csharp.processor.CSharpProcessor
import ai.privado.languageEngine.default.processor.DefaultProcessor
import ai.privado.languageEngine.go.processor.GoProcessor
import ai.privado.languageEngine.java.processor.JavaProcessor
import ai.privado.languageEngine.javascript.processor.JavascriptProcessor
import ai.privado.languageEngine.kotlin.processor.KotlinProcessor
import ai.privado.languageEngine.php.processor.PhpProcessor
import ai.privado.languageEngine.python.processor.PythonProcessor
import ai.privado.languageEngine.ruby.processor.RubyProcessor
import ai.privado.metadata.SystemInfo
import ai.privado.metric.MetricHandler
import ai.privado.model.*
import ai.privado.model.Language.{Language, UNKNOWN}
import ai.privado.utility.StatsRecorder
import better.files.File
import io.circe.Json
import io.joern.console.cpgcreation.guessLanguage
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory
import privado_core.BuildInfo

import scala.sys.exit
import scala.util.{Try}

object ScanProcessor extends CommandProcessor with RuleProcessor {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def process(appCache: AppCache): Either[String, Unit] = {
    println(s"Privado CLI Version: ${Environment.privadoVersionCli.getOrElse(Constants.notDetected)}")
    println(s"Privado Core Version: ${Environment.privadoVersionCore}")
    println(s"Privado Language Engine Version: ${BuildInfo.joernVersion}")
    if (!File(config.sourceLocation.head).isWritable) {
      println(s"Warning: Privado doesn't have write permission on give repo location - ${config.sourceLocation.head}")
    }
    SystemInfo.getInfo
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
  private val databaseDetailsCache   = new DatabaseDetailsCache()
  private val fileLinkingMetadata    = new FileLinkingMetadata()
  def getDataflowCache: DataFlowCache = {
    new DataFlowCache(config, auditCache)
  }

  private def processCpg(appCache: AppCache): Either[String, Unit] = {
    val sourceRepoLocation = File(config.sourceLocation.head).path.toAbsolutePath.toString.stripSuffix("/")
    val excludeFileRegex   = config.excludeFileRegex
    // Setting up the application cache
    appCache.init(sourceRepoLocation, excludeFileRegex = excludeFileRegex)
    statsRecorder.initiateNewStage("Language detection")
    val languageDetected = if (config.forceLanguage == UNKNOWN) {
      val langDect = Try(guessLanguage(sourceRepoLocation))
      statsRecorder.endLastStage()
      Language.withJoernLangName(langDect)
    } else {
      statsRecorder.justLogMessage("Language forced ...")
      statsRecorder.endLastStage()
      config.forceLanguage
    }
    MetricHandler.metricsData("language") = Json.fromString(languageDetected.toString)

    languageDetected match {
      case Language.JAVA =>
        statsRecorder.justLogMessage("Detected language 'Java'")
        val kotlinPlusJavaRules = getProcessedRule(Set(Language.KOTLIN, Language.JAVA), appCache, statsRecorder, config)
        val filesWithKtExtension = SourceFiles.determine(
          sourceRepoLocation,
          Set(".kt"),
          ignoredFilesRegex = Option(kotlinPlusJavaRules.getExclusionRegex.r)
        )
        if (filesWithKtExtension.isEmpty)
          JavaProcessor(
            getProcessedRule(Set(Language.JAVA), appCache, statsRecorder, config),
            this.config,
            sourceRepoLocation,
            dataFlowCache = getDataflowCache,
            auditCache,
            s3DatabaseDetailsCache,
            appCache,
            statsRecorder = statsRecorder,
            databaseDetailsCache = databaseDetailsCache,
            propertyFilterCache = propertyFilterCache,
            fileLinkingMetadata = fileLinkingMetadata
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
            statsRecorder = statsRecorder,
            databaseDetailsCache = databaseDetailsCache,
            propertyFilterCache = propertyFilterCache,
            fileLinkingMetadata = fileLinkingMetadata
          ).processCpg()
      case Language.JAVASCRIPT =>
        statsRecorder.justLogMessage("Detected language 'JavaScript'")
        new JavascriptProcessor(
          getProcessedRule(Set(Language.JAVASCRIPT), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.PYTHON =>
        statsRecorder.justLogMessage("Detected language 'Python'")
        new PythonProcessor(
          getProcessedRule(Set(Language.PYTHON), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          databaseDetailsCache = databaseDetailsCache,
          statsRecorder = statsRecorder,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.RUBY =>
        statsRecorder.justLogMessage("Detected language 'Ruby'")
        new RubyProcessor(
          getProcessedRule(Set(Language.RUBY), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.GO =>
        statsRecorder.justLogMessage("Detected language 'Go'")
        new GoProcessor(
          getProcessedRule(Set(Language.GO), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.KOTLIN =>
        statsRecorder.justLogMessage("Detected language 'Kotlin'")
        KotlinProcessor(
          getProcessedRule(Set(Language.KOTLIN, Language.JAVA), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.CSHARP =>
        statsRecorder.justLogMessage("Detected language 'C#'")
        CSharpProcessor(
          getProcessedRule(Set(Language.CSHARP), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.PHP =>
        statsRecorder.justLogMessage("Detected language 'PHP'")
        PhpProcessor(
          getProcessedRule(Set(Language.PHP), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        )
          .processCpg()
      case Language.C =>
        statsRecorder.justLogMessage("Detected language 'C'")
        CProcessor(
          getProcessedRule(Set(Language.C), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        )
          .processCpg()
      case _ =>
        processCpgWithDefaultProcessor(sourceRepoLocation, appCache, statsRecorder, fileLinkingMetadata)
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
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ) = {
    MetricHandler.metricsData("language") = Json.fromString("default")
    statsRecorder.justLogMessage("Running scan with default processor.")
    DefaultProcessor(
      getProcessedRule(Set(Language.UNKNOWN), appCache, statsRecorder, config),
      this.config,
      sourceRepoLocation,
      getDataflowCache,
      getAuditCache,
      getS3DatabaseDetailsCache,
      appCache,
      statsRecorder = statsRecorder,
      databaseDetailsCache = databaseDetailsCache,
      propertyFilterCache = propertyFilterCache,
      fileLinkingMetadata = fileLinkingMetadata
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
