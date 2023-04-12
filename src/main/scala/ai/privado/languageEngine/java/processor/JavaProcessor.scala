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

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.{AppCache, DataFlowCache, TaggerCache}
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.JSONExporter
import ai.privado.exporter.ExcelExporter
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.{ModuleFilePass, PropertiesFilePass}
import ai.privado.languageEngine.java.passes.methodFullName.LoggerLombokPass
import ai.privado.languageEngine.java.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{
  cpgOutputFileName,
  outputAuditFileName,
  outputDirectoryName,
  outputFileName,
  outputIntermediateFileName,
  storages
}
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.semantic.Language._
import ai.privado.model.Language
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory
import ai.privado.languageEngine.java.passes.module.DependenciesNodePass

import java.util.Calendar
import scala.util.{Failure, Success, Try}
import io.joern.x2cpg.utils.ExternalCommand
import better.files.File

import scala.collection.mutable.ListBuffer

object JavaProcessor {

  private val logger    = LoggerFactory.getLogger(getClass)
  private var cpgconfig = Config()
  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          println(s"${Calendar.getInstance().getTime} - Processing property files pass")
          new PropertiesFilePass(cpg, sourceRepoLocation).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Property file pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          logger.info("Applying data flow overlay")
          val context = new LayerCreatorContext(cpg)
          val options = new OssDataFlowOptions()
          new OssDataFlow(options).run(context)
          logger.info("=====================")
          println(
            s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          // Unresolved function report
          if (config.showUnresolvedFunctionsReport) {
            val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
            UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.JAVA)
          }

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          val taggerCache = new TaggerCache
          cpg.runTagger(processedRules, taggerCache, ScanProcessor.config)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow(ScanProcessor.config)
          println(s"${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${DataFlowCache.finalDataflow.size}")
          println(
            s"\n\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n\n"
          )
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()
          // Exporting Results
          JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap, taggerCache) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              println(
                s"${Calendar.getInstance().getTime} - Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder..."
              )
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
          }

          // Exporting the Audit report
          if (ScanProcessor.config.generateAuditReport) {
            val moduleCache: ModuleCache = new ModuleCache()
            new ModuleFilePass(cpg, sourceRepoLocation, moduleCache).createAndApply()
            new DependenciesNodePass(cpg, moduleCache).createAndApply()

            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint.getAuditWorkbook(xtocpg, taggerCache),
              sourceRepoLocation
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported Audit report to '${AppCache.localScanPath}/$outputDirectoryName' folder..."
                )
            }
          }

          // Exporting the Intermediate report
          if (ScanProcessor.config.testOutput) {
            JSONExporter.IntermediateFileExport(
              outputIntermediateFileName,
              sourceRepoLocation,
              DataFlowCache.getIntermediateDataFlow()
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
                )
            }
          }

          // Check if any of the export failed
          if (errorMsg.toList.isEmpty)
            Right(())
          else
            Left(errorMsg.toList.mkString("\n"))
        } finally {
          cpg.close()
          import java.io.File
          val cpgFile = new File(cpgconfig.outputPath)
          println(s"\n\n\nBinary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n")
        }
      }

      case Failure(exception) => {
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
      }
    }
  }

  /** Create cpg using Java Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createJavaCpg(processedRules: ConfigAndRules, sourceRepoLocation: String, lang: String): Either[String, Unit] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using ${Languages.JAVASRC} engine")
    if (!config.skipDownloadDependencies)
      println(s"${Calendar.getInstance().getTime} - Downloading dependencies and Parsing source code...")
    else
      println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    cpgconfig = Config(
      inputPath = sourceRepoLocation,
      outputPath = cpgOutputPath,
      fetchDependencies = !config.skipDownloadDependencies
    )

    // Create delomboked directory if source code uses lombok
    val dependencies        = JavaSrc2Cpg.getDependencyList(cpgconfig)
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    if (hasLombokDependency) {
      val delombokPath = Delombok.run(AppCache.scanPath)
      AppCache.isLombokPresent = true

      // Creating a new CpgConfig which uses the delombokPath
      cpgconfig = Config(
        inputPath = delombokPath,
        fetchDependencies = !config.skipDownloadDependencies,
        delombokMode = Some("no-delombok"),
        outputPath = cpgOutputPath
      )
    }

    val javasrc = JavaSrc2Cpg()
    val xtocpg = javasrc.createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      println(s"${Calendar.getInstance().getTime} - Processing Logger Lombok pass")
      new LoggerLombokPass(cpg).createAndApply()
      println(
        s"${TimeMetric.getNewTime()} - Logger Lombok pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      applyDefaultOverlays(cpg)
      cpg
    }

    val msg = processCPG(xtocpg, processedRules, sourceRepoLocation)

    // Delete the delomboked directory after scanning is completed
    if (AppCache.isLombokPresent) {
      val dirName = AppCache.scanPath + "/" + Constants.delombok
      Try(File(dirName).delete()) match {
        case Success(_)         => logger.debug("Succesfully deleted delomboked code")
        case Failure(exception) => logger.debug(s"Exception :", exception)
      }
    }
    msg
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

  private def delombokToTempDirCommand(tempDir: File, analysisJavaHome: Option[String]) = {
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
    s"$javaPath -cp $classPathArg lombok.launch.Main delombok . -d ${tempDir.canonicalPath}"
  }

  def run(projectDir: String, analysisJavaHome: Option[String] = None): String = {
    val dirName = projectDir + "/" + Constants.delombok
    Try(File(dirName).createDirectoryIfNotExists()) match {
      case Success(tempDir) =>
        ExternalCommand.run(delombokToTempDirCommand(tempDir, analysisJavaHome), cwd = projectDir) match {
          case Success(_) =>
            tempDir.path.toAbsolutePath.toString

          case Failure(t) =>
            logger.warn(s"Executing delombok failed", t)
            logger.warn("Creating AST with original source instead. Some methods and type information will be missing.")
            projectDir
        }

      case Failure(e) =>
        logger.warn(s"Failed to create temporary directory for delomboked source. Methods and types may be missing", e)
        projectDir
    }
  }

}
