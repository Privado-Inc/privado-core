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
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.{JavaPropertyLinkerPass, ModuleFilePass}
import ai.privado.languageEngine.java.passes.methodFullName.LoggerLombokPass
import ai.privado.languageEngine.java.passes.module.{DependenciesCategoryPass, DependenciesNodePass}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.languageEngine.java.semantic.Language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.llm.SemanticResponseModel
import ai.privado.model.{CatLevelOne, Constants, Language, Semantic}
import ai.privado.passes.{DBTParserPass, HTMLParserPass, SQLParser}
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.{createCpgFolder, resolver}
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import better.files.File
import com.fasterxml.jackson.databind.json.JsonMapper
import dotty.tools.dotc.util.HashMap
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.x2cpg.utils.dependency.DependencyResolver
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import requests.*

object JavaProcessor {

  private val logger    = LoggerFactory.getLogger(getClass)
  private var cpgconfig = Config()
  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          // Generating Semantic
          val url = "http://127.0.0.1:8000/generate-semantic/"
          //val data = Map("key1" -> "value1", "key2" -> "value2")
          val callMapping = cpg.method.filter(_.isExternal).where(_.nameNot("(?i).*(operator|<init>|require|import).*"))
            .callIn.map(call => (call.id(), call.typeFullName, call.name, call.code, call.argument.map(arg => (arg.argumentIndex, arg.argumentName, arg.code))))
            .dedupBy(_._3)
            .map { call => List(("id", call._1),("returnValue", call._2), ("name", call._3), ("code", call._4), ("arguments", call._5.l)).toMap }.l

          val callMap = callMapping.groupBy(mapping => mapping.get("id"))

          val data = s"""{"callMappings": ${callMapping.toJson}}""".stripMargin

          val response = requests.post(url, data=data, readTimeout=100000, connectTimeout=100000, headers = Map("Content-Type" -> "application/json"))

          if (response.statusCode == 200) {
            val responseBody = response.text()

            import io.circe.parser._
            import ai.privado.model.llm.SemanticResponseEncoderDecoder._

            val parsedData = decode[SemanticResponseModel](responseBody)
            parsedData match
              case Left(error) => println(s"Failed to parse the JSON response: ${error.getMessage}")
              case Right(responseModel) =>
                // Process responseBody as needed
                val totalPaths = responseModel.output.flatMap(_.arguments.map(_.result.label))
                val removedPaths = totalPaths.filter(_.equals("No"))
                println(s"Total path : ${totalPaths.size}")
                println(s"Removed path : ${removedPaths.size}")
                println(s"Percentage path reduced : ${(totalPaths.size-removedPaths.size)*100/totalPaths.size}")
                val generatedSemantics = JavaSemanticGenerator.getMaximumFlowSemantic(responseModel.output.map{ scm =>
                  val semanticModel = JavaSemanticGenerator.generateSemanticForTaint(cpg.call.where(_.id(scm.id)).head)

                  val newSemantic = scm.arguments.filter(_.result.label == "Yes").map(ar => s"${ar.param1._1.get}->${ar.param2._1.get}")
                  Semantic(semanticModel.signature, semanticModel.flow.trim + " " + newSemantic.mkString(" "), semanticModel.file, semanticModel.language, semanticModel.categoryTree)
                }.iterator)
                ruleCache.addExternalSemantics(generatedSemantics)
                println(responseModel)
                println(generatedSemantics)
          } else {
            println(s"Request failed with status code: ${response.statusCode}")
          }

          new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.JAVA).createAndApply()
          new JavaPropertyLinkerPass(cpg).createAndApply()

          println(s"${Calendar.getInstance().getTime} - HTML parser pass")
          new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = ScanProcessor.config.copy())
            .createAndApply()

          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new DBTParserPass(cpg, sourceRepoLocation, ruleCache).createAndApply()

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
          cpg.runTagger(ruleCache, taggerCache, ScanProcessor.config, dataFlowCache)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow(ScanProcessor.config, ruleCache, dataFlowCache, auditCache)
          println(s"${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${dataFlowCache.finalDataflow.size}")
          println(
            s"\n\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n\n"
          )
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()
          // Exporting Results
          JSONExporter.fileExport(
            cpg,
            outputFileName,
            sourceRepoLocation,
            dataflowMap,
            ruleCache,
            taggerCache,
            dataFlowCache
          ) match {
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
            new ModuleFilePass(cpg, sourceRepoLocation, moduleCache, ruleCache).createAndApply()
            new DependenciesNodePass(cpg, moduleCache).createAndApply()
            // Fetch all dependency after pass
            val dependencies = DependencyReport.getDependencyList(xtocpg)
            new DependenciesCategoryPass(xtocpg.get, ruleCache, dependencies.toList).createAndApply()
            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint.getAuditWorkbook(xtocpg, taggerCache, dependencies, sourceRepoLocation, auditCache),
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

            // Exporting the Unresolved report
            JSONExporter.UnresolvedFlowFileExport(
              outputUnresolvedFilename,
              sourceRepoLocation,
              dataFlowCache.getJsonFormatDataFlow(auditCache.unfilteredFlow)
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported Unresolved flow output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
                )
            }
          }

          // Exporting the Intermediate report
          if (ScanProcessor.config.testOutput || ScanProcessor.config.generateAuditReport) {
            JSONExporter.IntermediateFileExport(
              outputIntermediateFileName,
              sourceRepoLocation,
              dataFlowCache.getJsonFormatDataFlow(dataFlowCache.getIntermediateDataFlow())
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
  def createJavaCpg(
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    lang: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache
  ): Either[String, Unit] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using ${Languages.JAVASRC} engine")
    if (!config.skipDownloadDependencies)
      println(s"${Calendar.getInstance().getTime} - Downloading dependencies and Parsing source code...")
    else
      println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    cpgconfig = Config(fetchDependencies = !config.skipDownloadDependencies)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)

    // Create delomboked directory if source code uses lombok
    val dependencies        = getDependencyList(cpgconfig)
    val hasLombokDependency = dependencies.exists(_.contains("lombok"))
    if (hasLombokDependency) {
      val delombokPath = Delombok.run(AppCache.scanPath)
      AppCache.isLombokPresent = true

      // Creating a new CpgConfig which uses the delombokPath
      cpgconfig = Config(fetchDependencies = !config.skipDownloadDependencies, delombokMode = Some("no-delombok"))
        .withInputPath(delombokPath)
        .withOutputPath(cpgOutputPath)
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

    val msg = processCPG(xtocpg, ruleCache, sourceRepoLocation, dataFlowCache, auditCache)

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
