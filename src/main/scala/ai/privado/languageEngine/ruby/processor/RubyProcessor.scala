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

import ai.privado.audit.{AuditReportEntryPoint, DEDSourceDiscovery}
import ai.privado.cache.*
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.exporter.monolith.MonolithExporter
import ai.privado.languageEngine.ruby.passes.*
import ai.privado.languageEngine.ruby.passes.config.RubyPropertyLinkerPass
import ai.privado.languageEngine.ruby.passes.download.DownloadDependenciesPass
import ai.privado.languageEngine.ruby.passes.*
import ai.privado.languageEngine.ruby.semantic.Language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{cpgOutputFileName, outputAuditFileName, outputDirectoryName, outputFileName}
import ai.privado.model.{CatLevelOne, Constants, CpgWithOutputMap, Language}
import ai.privado.passes.{DBTParserPass, ExperimentalLambdaDataFlowSupportPass, JsonPropertyParserPass, SQLParser}
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.rubysrc2cpg.deprecated.astcreation.ResourceManagedParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.*
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.joern.rubysrc2cpg.passes.ConfigFileCreationPass
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.layers.*
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.controlflow.cfgcreation.{Cfg, CfgCreator}
import io.joern.x2cpg.passes.controlflow.cfgdominator.CfgDominatorPass
import io.joern.x2cpg.passes.controlflow.codepencegraph.CdgPass
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.joern.x2cpg.{SourceFiles, ValidationMode, X2Cpg, X2CpgConfig}
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ai.privado.dataflow.Dataflow
import ai.privado.cache.*
import ai.privado.languageEngine.base.processor.BaseProcessor
import io.shiftleft.passes.CpgPassBase

import java.util
import java.util.Calendar
import java.util.concurrent.{Callable, Executors}
import scala.collection.mutable.ListBuffer
import scala.concurrent.*
import scala.concurrent.duration.DurationLong
import scala.util.{Failure, Success, Try, Using}
import scala.concurrent.ExecutionContext.Implicits.global

class RubyProcessor(
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
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache()
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.RUBY,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache
    ) {

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    val passesList = List(
      new MethodFullNamePassForRORBuiltIn(cpg), {
        if (privadoInput.assetDiscovery)
          new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
        else
          new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.RUBY, propertyFilterCache)
      },
      new RubyPropertyLinkerPass(cpg),
      new RubyExternalTypesPass(cpg, RubySrc2Cpg.packageTableInfo)
    )
    // Using our own pass by overriding languageEngine's pass
    // new RubyImportResolverPass(cpg, packageTableInfo).createAndApply()
    val globalSymbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)
    passesList ++ List(new GlobalImportPass(cpg, globalSymbolTable)) ++
      new PrivadoRubyTypeRecoveryPassGenerator(cpg, globalSymbolTable).generate() ++
      List(
        new RubyTypeHintCallLinker(cpg),
        new AstLinkerPass(cpg),
        new SchemaParser(cpg, sourceRepoLocation, ruleCache),
        new SQLParser(cpg, sourceRepoLocation, ruleCache),
        new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache)
      )
    passesList
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache, databaseDetailsCache, statsRecorder)
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using Ruby engine")
    statsRecorder.initiateNewStage("Base source processing")

    createCpgFolder(sourceRepoLocation)

    val cpgOutputPath          = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath.normalize().toString
    val excludeFileRegex       = ruleCache.getRule.exclusions.flatMap(rule => rule.patterns).mkString("|")
    val RubySourceFileExtensions: Set[String] = Set(".rb")

    val cpgconfig = Config()
      .withInputPath(absoluteSourceLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
      .withSchemaValidation(ValidationMode.Enabled)
      // TODO: Remove old ruby frontend this once we have the new frontend ready upstream
      .withUseDeprecatedFrontend(true)

    val xtocpg = withNewEmptyCpg(cpgconfig.outputPath, cpgconfig: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      // TODO: Either get rid of the second timeout parameter or take this one as an input parameter
      Using.resource(new ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
        val parsedFiles: List[(String, ProgramContext)] = {
          val tasks = SourceFiles
            .determine(
              config.inputPath,
              RubySourceFileExtensions,
              ignoredFilesRegex = Option(config.ignoredFilesRegex),
              ignoredFilesPath = Option(config.ignoredFiles)
            )
            .map { x =>
              ParserTask(x, parser)
            }
          val results = tasks.map(task =>
            Future {
              task.call()
            }
          )
          var errorFileCount   = 0
          var timeoutFileCount = 0
          val finalResult      = ListBuffer[(String, ProgramContext)]()
          results.zip(tasks).foreach { case (result, task) =>
            try {
              finalResult += Await.result(result, privadoInput.rubyParserTimeout.seconds)
            } catch {
              case ex: TimeoutException =>
                statsRecorder.justLogMessage(s"Parser timed out for file -> '${task.file}'")
                timeoutFileCount += 1
              case ex: Exception =>
                statsRecorder.justLogMessage(s"Error while parsing file -> '${task.file}'")
                errorFileCount += 1
            }
          }

          statsRecorder.justLogMessage(s"No of files skipped because of timeout - '$timeoutFileCount'")
          statsRecorder.justLogMessage(s"No of files that are skipped because of error - '$errorFileCount'")
          finalResult.toList
        }

        new io.joern.rubysrc2cpg.deprecated.ParseInternalStructures(parsedFiles, cpg.metaData.root.headOption)
          .populatePackageTable()
        val astCreationPass =
          new AstCreationPass(cpg, parsedFiles, RubySrc2Cpg.packageTableInfo, config)
        astCreationPass.createAndApply()
        statsRecorder.initiateNewStage("Default overlays")
        applyDefaultOverlays(cpg)
      }
    }
    statsRecorder.endLastStage()
    RubySrc2Cpg.packageTableInfo.clear()
    if (!privadoInput.skipDownloadDependencies) {
      statsRecorder.initiateNewStage("Download Dependency")
      val packageTable =
        new DownloadDependenciesPass(new PackageTable(), sourceRepoLocation, ruleCache).createAndApply()
      RubySrc2Cpg.packageTableInfo.set(packageTable)
      statsRecorder.endLastStage()
    }
    tagAndExport(xtocpg)
  }

  // Start: Here be dragons

  private def applyDefaultOverlays(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    defaultOverlayCreators().foreach { creator =>
      creator.run(context)
    }
  }

  /** This should be the only place where we define the list of default overlays.
    */
  private def defaultOverlayCreators(): List[LayerCreator] = {
    List(new Base(), new RubyControlFlow(), new TypeRelations(), new CallGraph())
  }

  class RubyCfgCreationPass(cpg: Cpg) extends CfgCreationPass(cpg) {
    override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
      val localDiff = new DiffGraphBuilder
      new RubyCfgCreator(method, localDiff).run()
      diffGraph.absorb(localDiff)
    }
  }

  class RubyControlFlow extends ControlFlow {

    override def create(context: LayerCreatorContext): Unit = {
      val cpg    = context.cpg
      val passes = Iterator(new RubyCfgCreationPass(cpg), new CfgDominatorPass(cpg), new CdgPass(cpg))
      passes.zipWithIndex.foreach { case (pass, index) =>
        runPass(pass, context, index)
      }
    }

  }

  class RubyCfgCreator(entryNode: Method, diffGraph: DiffGraphBuilder) extends CfgCreator(entryNode, diffGraph) {

    override protected def cfgForContinueStatement(node: ControlStructure): Cfg = {
      node.astChildren.find(_.order == 1) match {
        case Some(jumpLabel: JumpLabel) =>
          val labelName = jumpLabel.name
          Cfg(entryNode = Option(node), jumpsToLabel = List((node, labelName)))
        case Some(literal: Literal) =>
          // In case we find a literal, it is assumed to be an integer literal which
          // indicates how many loop levels the continue shall apply to.
          Try(Integer.valueOf(literal.code)) match {
            case Failure(exception) =>
              logger.error(
                s"Error when typeCasting literal to integer in file ${literal.file.headOption.map(_.name).getOrElse("unknown file")} ",
                exception
              )
              Cfg(entryNode = Option(node), continues = List((node, 1)))
            case Success(numberOfLevels) => Cfg(entryNode = Option(node), continues = List((node, numberOfLevels)))
          }
        case Some(x) =>
          logger.error(
            s"Unexpected node ${x.label} (${x.code}) from ${x.file.headOption.map(_.name).getOrElse("unknown file")}.",
            new NotImplementedError(
              "Only jump labels and integer literals are currently supported for continue statements."
            )
          )
          Cfg(entryNode = Option(node), continues = List((node, 1)))
        case None =>
          Cfg(entryNode = Option(node), continues = List((node, 1)))
      }
    }
    override protected def cfgForAndExpression(call: Call): Cfg =
      Try(super.cfgForAndExpression(call)) match
        case Failure(exception) =>
          logger.error(
            s"Error when generating Cfg for expression in file ${call.file.headOption.map(_.name).getOrElse("unknown file")} ",
            exception
          )
          Cfg.empty
        case Success(cfg) => cfg

  }

  // End: Here be dragons

  private class ParserTask(val file: String, val parser: ResourceManagedParser) {

    def call(): (String, ProgramContext) = {
      parser.parse(file) match
        case Failure(exception) =>
          logger.error(s"Could not parse file: $file, skipping", exception); throw exception
        case Success(ast) => file -> ast
    }
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
