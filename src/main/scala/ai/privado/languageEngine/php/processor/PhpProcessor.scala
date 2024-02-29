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

package ai.privado.languageEngine.php.processor

import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.php.semantic.Language.tagger
import ai.privado.model.Constants.*
import ai.privado.model.Language.Language
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.passes.CpgPassBase
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import java.util.Calendar
import java.io.File

class PhpProcessor(
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

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = List[CpgPassBase]()

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit =
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache)

  override def processCpg(): Either[String, Unit] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")

    createCpgFolder(sourceRepoLocation)

    // get vendored php parser path
    val parserBinPath: String = {
      val dir        = getClass.getProtectionDomain.getCodeSource.getLocation.toString
      val indexOfLib = dir.lastIndexOf("lib")
      val fixedDir = if (indexOfLib != -1) {
        new File(dir.substring("file:".length, indexOfLib)).toString
      } else {
        val indexOfTarget = dir.lastIndexOf("target")
        if (indexOfTarget != -1) {
          new File(dir.substring("file:".length, indexOfTarget)).toString
        } else {
          "."
        }
      }
      Paths.get(fixedDir, "/bin/php-parser/php-parser.php").toAbsolutePath.toString
    }

    val cpgOutput = Paths.get(sourceRepoLocation, outputDirectoryName, cpgOutputFileName)
    val cpgConfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutput.toString)
      .withIgnoredFilesRegex(ruleCache.getExclusionRegex)
      .withPhpParserBin(parserBinPath)

    val xtocpg = new Php2Cpg().createCpg(cpgConfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      applyDefaultOverlays(cpg)
      cpg
    }

    tagAndExport(xtocpg)
  }
}
