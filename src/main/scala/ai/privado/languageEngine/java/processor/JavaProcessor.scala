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

package ai.privado.languageEngine.java.processor

import ai.privado.audit.{AuditReportEntryPoint, DependencyReport}
import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.{JavaAnnotationPropertyLinkerPass, JavaEnvPropertyLinkerPass, ModuleFilePass}
import ai.privado.languageEngine.java.passes.methodFullName.LoggerLombokPass
import ai.privado.languageEngine.java.passes.module.{DependenciesCategoryPass, DependenciesNodePass}
import ai.privado.languageEngine.java.semantic.Language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.Language.Language
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.{AndroidXmlParserPass, DBTParserPass, ExperimentalLambdaDataFlowSupportPass, HTMLParserPass, JsonPropertyParserPass, SQLParser}
import ai.privado.semantic.Language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.x2cpg.utils.dependency.DependencyResolver
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class JavaProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  lang: Language,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      lang,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache
    ) {

  override val logger: Logger = LoggerFactory.getLogger(getClass)
  private var cpgconfig       = Config()

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List({
      if (privadoInput.assetDiscovery)
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
      else
        new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.JAVA)
    }) ++
      List(
        new JavaEnvPropertyLinkerPass(cpg),
        new JavaAnnotationPropertyLinkerPass(cpg),
        new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput),
        new SQLParser(cpg, sourceRepoLocation, ruleCache),
        new DBTParserPass(cpg, sourceRepoLocation, ruleCache),
        new AndroidXmlParserPass(cpg, sourceRepoLocation, ruleCache)
      )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit =
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, s3DatabaseDetailsCache)

  override def processCpg(): Either[String, Unit] = {
    val excludeFileRegex = ruleCache.getExclusionRegex
    println(s"${Calendar.getInstance().getTime} - Processing source code using ${Languages.JAVASRC} engine")
    if (!privadoInput.skipDownloadDependencies)
      println(s"${Calendar.getInstance().getTime} - Downloading dependencies and Parsing source code...")
    else
      println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    cpgconfig = Config(fetchDependencies = !privadoInput.skipDownloadDependencies)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)

    // Create delomboked directory if source code uses lombok
    val dependencies        = getDependencyList(cpgconfig)
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    if (hasLombokDependency) {
      Delombok.run(AppCache.scanPath) match
        case Left(_) =>
        case Right(delombokPath) =>
          AppCache.isLombokPresent = true
          // Update the new ScanPath with delombok folder path
          AppCache.scanPath = delombokPath
          // Creating a new CpgConfig which uses the delombokPath
          cpgconfig =
            Config(fetchDependencies = !privadoInput.skipDownloadDependencies, delombokMode = Some("no-delombok"))
              .withInputPath(delombokPath)
              .withOutputPath(cpgOutputPath)
              .withIgnoredFilesRegex(excludeFileRegex)
    }

    val javasrc = JavaSrc2Cpg()
    val xtocpg = javasrc.createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )

      new LoggerLombokPass(cpg).createAndApply()

      applyDefaultOverlays(cpg)
      cpg
    }

    val msg = tagAndExport(xtocpg)

    // Delete the delomboked directory after scanning is completed
    if (AppCache.isLombokPresent) {
      val dirName = AppCache.scanPath // scanPath is  already set to delombok path
      Try(File(dirName).delete()) match {
        case Success(_)         => logger.debug("Succesfully deleted delomboked code")
        case Failure(exception) => logger.debug(s"Exception :", exception)
      }
    }
    msg
  }

  def getDependencyList(config: Config): Set[String] = {
    val codeDir = config.inputPath
    DependencyResolver.getDependencies(Paths.get(codeDir)) match {
      case Some(dependencies) => dependencies.toSet
      case None =>
        logger.warn(s"Could not fetch dependencies for project at path $codeDir")
        Set()
    }
  }
}

object Delombok {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def systemJavaPath: String = {
    sys.env
      .get("JAVA_HOME")
      .flatMap { javaHome =>
        val javaExecutable = File(javaHome, "bin", "java")
        Option.when(javaExecutable.exists && javaExecutable.isExecutable) {
          javaExecutable.canonicalPath
        }
      }
      .getOrElse("java")
  }

  private def delombokToTempDirCommand(tempDirPath: String, analysisJavaHome: Option[String]) = {
    val javaPath = analysisJavaHome.getOrElse(systemJavaPath)
    val classPathArg = Try(File.newTemporaryFile("classpath").deleteOnExit()) match {
      case Success(file) =>
        // Write classpath to a file to work around Windows length limits.
        file.write(System.getProperty("java.class.path"))
        s"@${file.canonicalPath}"

      case Failure(t) =>
        logger.warn(
          s"Failed to create classpath file for delombok execution. Results may be missing on Windows systems",
          t
        )
        System.getProperty("java.class.path")
    }

    s"$javaPath -cp $classPathArg lombok.launch.Main delombok . -d $tempDirPath -f pretty"
  }

  def run(projectDir: String, analysisJavaHome: Option[String] = None): Either[String, String] = {
    val dirName = s"$projectDir/${Constants.delombok}"
    ExternalCommand.run(delombokToTempDirCommand(dirName, analysisJavaHome), cwd = projectDir) match {
      case Success(_) =>
        Right(dirName)

      case Failure(t) =>
        logger.warn(s"Executing delombok failed", t)
        logger.warn("Creating AST with original source instead. Some methods and type information will be missing.")
        Left(projectDir)
    }
  }
}
