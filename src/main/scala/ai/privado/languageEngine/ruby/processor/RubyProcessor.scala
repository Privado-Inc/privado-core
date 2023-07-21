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

package ai.privado.languageEngine.ruby.processor

import ai.privado.languageEngine.ruby.semantic.Language.*
import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.java.processor.JavaProcessor.logger
import ai.privado.languageEngine.ruby.download.ExternalDependenciesResolver
import ai.privado.languageEngine.ruby.passes.{ImportPass, MethodFullNamePassForRORBuiltIn, RubyImportResolverPass}
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.model.Constants.{cpgOutputFileName, outputDirectoryName, outputFileName}
import ai.privado.semantic.Language.*
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.model.Language
import ai.privado.passes.SQLParser
import ai.privado.utility.Utilities.createCpgFolder
import io.shiftleft.codepropertygraph
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.rubysrc2cpg.RubySrc2Cpg.packageTableInfo
import io.joern.rubysrc2cpg.passes.{ImportResolverPass, RubyTypeHintCallLinker, RubyTypeRecoveryPass}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, ConfigFileCreationPass}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.{newEmptyCpg, withNewEmptyCpg}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{X2Cpg, X2CpgConfig}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages, Operators}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.util.Calendar
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object RubyProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) =>
        logger.info("Applying default overlays")
        X2Cpg.applyDefaultOverlays(cpg)

        logger.info("Enhancing Ruby graph")
        if (!config.skipDownloadDependencies) {
          println(s"${Calendar.getInstance().getTime} - Downloading dependencies and parsing ...")
          val packageTable = ExternalDependenciesResolver.downloadDependencies(cpg, sourceRepoLocation)
          RubySrc2Cpg.packageTableInfo.set(packageTable)
          new ImportPass(cpg, RubySrc2Cpg.packageTableInfo).createAndApply()
        }
        new MethodFullNamePassForRORBuiltIn(cpg).createAndApply()

        logger.info("Enhancing Ruby graph by post processing pass")

        // Using our own pass by overriding languageEngine's pass
        new RubyImportResolverPass(cpg, packageTableInfo).createAndApply()
        new RubyTypeRecoveryPass(cpg).createAndApply()
        new RubyTypeHintCallLinker(cpg).createAndApply()
        new NaiveCallLinker(cpg).createAndApply()

        // Some of passes above create new methods, so, we
        // need to run the ASTLinkerPass one more time
        new AstLinkerPass(cpg).createAndApply()

        // Not using languageEngine's passes
        // RubySrc2Cpg.postProcessingPasses(cpg).foreach(_.createAndApply())

        // TODO remove below lines in GA release, need these for dubugging
        // cpg.call.whereNot(_.name("(?i)(.*operator.*|require.*)")).whereNot(_.code("<empty>")).map(cl => (cl.name, cl.file.name.headOption.getOrElse(""), cl.methodFullName, cl.dynamicTypeHintFullName.l)).foreach(println)
        // cpg.call.whereNot(_.name("(?i)(.*operator.*|require.*)")).whereNot(_.code("<empty>")).sortBy(_.name).map(cl => (cl.name, cl.methodFullName, cl.file.name.headOption.getOrElse(""), cl.lineNumber)).foreach(println)

        logger.info("Applying data flow overlay")
        val context = new LayerCreatorContext(cpg)
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
        logger.info("=====================")

        logger.debug("Running custom passes")
        new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()

        // Unresolved function report
        if (config.showUnresolvedFunctionsReport) {
          val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
          UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.RUBY)
        }
        logger.info("=====================")

        // Run tagger
        println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
        cpg.runTagger(ruleCache)
        println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
        val dataflowMap = cpg.dataflow(ScanProcessor.config, ruleCache)
        println(s"${Calendar.getInstance().getTime} - No of flows found -> ${dataflowMap.size}")
        println(s"${Calendar.getInstance().getTime} - Brewing result...")
        MetricHandler.setScanStatus(true)
        // Exporting
        JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap, ruleCache) match {
          case Left(err) =>
            MetricHandler.otherErrorsOrWarnings.addOne(err)
            Left(err)
          case Right(_) =>
            println(s"Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder")
            logger.debug(
              s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
            )

            Right(())
        }

      case Failure(exception) =>
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
    }
  }

  /** Create cpg using Javascript Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createRubyCpg(ruleCache: RuleCache, sourceRepoLocation: String, lang: String): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    // Need to convert path to absolute path as ruby cpg needs abolute path of repo
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath.normalize().toString

    val config = Config().withInputPath(absoluteSourceLocation).withOutputPath(cpgOutputPath)
    // val xtocpg = new RubySrc2Cpg().createCpg(config)

    val global = new Global()
    val xtocpg = withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>

      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, RubySrc2Cpg.packageTableInfo)
      astCreationPass.createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }

    processCPG(xtocpg, ruleCache, sourceRepoLocation)
  }

  def withNewEmptyCpg[T <: X2CpgConfig[_]](outPath: String, config: T)(applyPasses: (Cpg, T) => Unit): Try[Cpg] = {
    val outputPath = if (outPath != "") Some(outPath) else None
    Try {
      val cpg = newEmptyCpg(outputPath)
      Try {
        applyPasses(cpg, config)
      } match {
        case Success(_) => cpg
        case Failure(exception) =>
          println(
            s"Exception occurred in cpg generation, but continuing with failed cpg. The number of nodes in cpg are : ${cpg.all.size}, $exception"
          )
          cpg
      }
    }
  }

}
