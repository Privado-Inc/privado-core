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

package ai.privado.languageEngine.default.processor

import ai.privado.cache.*
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.default.passes.{PropertyVerificationPass}
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.passes.{DBTParserPass, HTMLParserPass, HighTouchPass, SQLParser}
import ai.privado.languageEngine.default.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.javasrc2cpg.Config as JavaConfig
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory
import ai.privado.languageEngine.default.tagger.PrivadoTagger
import ai.privado.utility.StatsRecorder

import java.util.Calendar

class DefaultProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  returnClosedCpg: Boolean = true,
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  statsRecorder: StatsRecorder
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.UNKNOWN,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg,
      propertyFilterCache,
      statsRecorder
    ) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List[CpgPassBase](
      new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new DBTParserPass(cpg, sourceRepoLocation, ruleCache),
      new HighTouchPass(cpg, sourceRepoLocation, ruleCache),
      new PropertyVerificationPass(cpg)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache)
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using Default engine")
    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);
    val excludeFileRegex = ruleCache.getExclusionRegex

    val cpgconfig = JavaConfig(fetchDependencies = !privadoInput.skipDownloadDependencies)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)

    val xtocpg = withNewEmptyCpg(cpgOutputPath, cpgconfig: JavaConfig) { (cpg, config) =>
      {
        cpg
      }
    }
    tagAndExport(xtocpg)
  }
}
