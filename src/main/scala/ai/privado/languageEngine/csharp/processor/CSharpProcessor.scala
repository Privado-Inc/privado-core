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

package ai.privado.languageEngine.csharp.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, S3DatabaseDetailsCache, TaggerCache}
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.model.Constants.outputFileName
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory
import ai.privado.model.Constants.*
import ai.privado.utility.Utilities.createCpgFolder
import better.files.File
import io.joern.csharpsrc2cpg.*
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.csharp.semantic.Language.tagger
import ai.privado.dataflow.Dataflow
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.passes.config.JavaPropertyLinkerPass
import ai.privado.passes.{AndroidXmlParserPass, HTMLParserPass, JsonPropertyParserPass, SQLParser, SQLPropertyPass}
import ai.privado.semantic.Language.*
import ai.privado.utility.PropertyParserPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import ai.privado.entrypoint.*
import ai.privado.model.Language.Language
import io.circe.Json

import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class CSharpProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.CSHARP,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache
    ) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List[CpgPassBase]()
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache)
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
  }

  override def processCpg(): Either[String, (Cpg, Map[String, Json])] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using CSharp engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);
    val excludeFileRegex = ruleCache.getExclusionRegex

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)

    val xtocpg = new CSharpSrc2Cpg().createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      // Apply default overlays
      X2Cpg.applyDefaultOverlays(cpg)
      cpg
    }
    tagAndExport(xtocpg)
  }
}
