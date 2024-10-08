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

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.passes.config.{JavaPropertyLinkerPass, JavaYamlLinkerPass}
import ai.privado.languageEngine.java.passes.methodFullName.LoggerLombokPass
import ai.privado.languageEngine.java.semantic.Language.*
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.x2cpg.utils.dependency.DependencyResolver
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.passes.CpgPassBase
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class JavaProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  statsRecorder: StatsRecorder,
  returnClosedCpg: Boolean = true,
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache(),
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata(),
  dependencies: List[DependencyInfo]
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.JAVA,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache,
      fileLinkingMetadata,
      dependencies
    ) {

  override val logger: Logger = LoggerFactory.getLogger(getClass)
  private var cpgconfig       = Config()

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    super.applyPrivadoPasses(cpg) ++ List({
      if (privadoInput.assetDiscovery)
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
      else
        new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.JAVA, propertyFilterCache)
    }) ++
      List(
        new JavaPropertyLinkerPass(cpg),
        new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput),
        new SQLParser(cpg, sourceRepoLocation, ruleCache),
        new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache),
        new AndroidXmlParserPass(cpg, sourceRepoLocation, ruleCache),
        new JavaYamlLinkerPass(cpg)
      )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    super.runPrivadoTagger(cpg, taggerCache)
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      s3DatabaseDetailsCache,
      appCache,
      databaseDetailsCache,
      statsRecorder,
      fileLinkingMetadata
    )
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    val excludeFileRegex = ruleCache.getExclusionRegex
    statsRecorder.justLogMessage("Processing source code using Java engine")
    if (!privadoInput.skipDownloadDependencies)
      statsRecorder.justLogMessage("Downloading dependencies and Parsing source code...")
    else
      statsRecorder.justLogMessage("Parsing source code...")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    cpgconfig = Config(fetchDependencies = !privadoInput.skipDownloadDependencies)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
    // .withKeepTypeArguments(true)

    // Create delomboked directory if source code uses lombok
    val dependencies        = getDependencyList(cpgconfig)
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    if (hasLombokDependency) {
      statsRecorder.initiateNewStage("Delombok")
      Delombok.run(appCache.scanPath) match {
        case Left(_) =>
        case Right(delombokPath) =>
          appCache.isLombokPresent = true
          // Update the new ScanPath with delombok folder path
          appCache.scanPath = delombokPath
          // Creating a new CpgConfig which uses the delombokPath
          cpgconfig =
            Config(fetchDependencies = !privadoInput.skipDownloadDependencies, delombokMode = Some("no-delombok"))
              .withInputPath(delombokPath)
              .withOutputPath(cpgOutputPath)
              .withIgnoredFilesRegex(excludeFileRegex)
      }
      statsRecorder.endLastStage()
      // .withKeepTypeArguments(true)
    }

    val javasrc = JavaSrc2Cpg()
    statsRecorder.initiateNewStage("Base source processing")
    val xtocpg = javasrc.createCpg(cpgconfig).map { cpg =>
      statsRecorder.endLastStage()
      statsRecorder.initiateNewStage("Preprocessing before overlay passes")
      new LoggerLombokPass(cpg).createAndApply()
      statsRecorder.endLastStage()
      statsRecorder.initiateNewStage("Default overlays")
      applyDefaultOverlays(cpg)
      statsRecorder.endLastStage()
      statsRecorder.setSupressSubstagesFlag(false)
      cpg
    }

    val tagAndExportOutput = tagAndExport(xtocpg)

    // Delete the delomboked directory after scanning is completed
    if (appCache.isLombokPresent) {
      val dirName = appCache.scanPath // scanPath is  already set to delombok path
      Try(File(dirName).delete()) match {
        case Success(_)         => logger.debug("Succesfully deleted delomboked code")
        case Failure(exception) => logger.debug(s"Exception :", exception)
      }
    }

    tagAndExportOutput match {
      case Left(msg)               => Left(msg)
      case Right(cpgWithOutputMap) => Right(cpgWithOutputMap)
    }
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
