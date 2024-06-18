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

package ai.privado.languageEngine.javascript.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.javascript.passes.config.{JSPropertyLinkerPass, JsConfigPropertyPass}
import ai.privado.languageEngine.javascript.semantic.Language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{cpgOutputFileName, outputDirectoryName}
import ai.privado.model.{CatLevelOne, Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

class JavascriptProcessor(
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
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.JAVASCRIPT,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache
    ) {

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    val passesList = List(new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput))

    passesList ++ List({
      if (privadoInput.assetDiscovery)
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
        new JsConfigPropertyPass(cpg)
      else
        new PropertyParserPass(
          cpg,
          sourceRepoLocation,
          ruleCache,
          Language.JAVASCRIPT,
          propertyFilterCache,
          privadoInput
        )
    }) ++ List(
      new JSPropertyLinkerPass(cpg),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache),
      new AndroidXmlParserPass(cpg, sourceRepoLocation, ruleCache)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit =
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache, databaseDetailsCache)

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
    JsSrc2Cpg.postProcessingPasses(cpg).foreach(_.createAndApply())
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using Javascript engine")
    statsRecorder.initiateNewStage("Base source processing")


    createCpgFolder(sourceRepoLocation)

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
//    val cpgOutput = Paths.get(sourceRepoLocation, outputDirectoryName, cpgOutputFileName)
    val cpgConfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(ruleCache.getExclusionRegex)

    val xtocpg = new JsSrc2Cpg().createCpg(cpgConfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )

      applyDefaultOverlays(cpg)
      cpg
    }

    tagAndExport(xtocpg)
  }
}
