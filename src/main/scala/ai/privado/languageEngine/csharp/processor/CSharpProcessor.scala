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

import ai.privado.cache.*
import ai.privado.entrypoint.*
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.csharp.semantic.Language.tagger
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.csharpsrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import scala.util.Try

class CSharpProcessor(
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
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata()
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.CSHARP,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache,
      fileLinkingMetadata
    ) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List[CpgPassBase]()
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      appCache,
      databaseDetailsCache,
      statsRecorder,
      fileLinkingMetadata
    )
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using CSharp engine")
    statsRecorder.initiateNewStage("Base source processing")
    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);
    val excludeFileRegex = ruleCache.getExclusionRegex

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
      .withDownloadDependencies(Try(!config.skipDownloadDependencies).getOrElse(false))

    val xtocpg = new CSharpSrc2Cpg().createCpg(cpgconfig).map { cpg =>
      statsRecorder.endLastStage()
      statsRecorder.initiateNewStage("Default overlays")
      // Apply default overlays
      X2Cpg.applyDefaultOverlays(cpg)
      statsRecorder.endLastStage()
      cpg
    }
    tagAndExport(xtocpg)
  }
}
