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

package ai.privado.languageEngine.php

import ai.privado.rule.RuleInfoTestData
import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.php.processor.PhpProcessor
import ai.privado.languageEngine.php.tagger.source.IdentifierTagger
import ai.privado.model.*
import ai.privado.tagger.source.LiteralTagger
import ai.privado.threatEngine.ThreatEngineExecutor
import better.files.File
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.collection.mutable

abstract class PhpTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val taggerCache = new TaggerCache()

  val configAndRules: ConfigAndRules =
    ConfigAndRules(RuleInfoTestData.sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())

  def code(code: String): (Cpg, ThreatEngineExecutor) = {
    val ruleCache     = new RuleCache()
    val auditCache    = new AuditCache()
    val privadoInput  = PrivadoInput()
    val dataFlowCache = new DataFlowCache(privadoInput, auditCache)

    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "main.php").write(code)

    val outputFile: File = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
      .withPhpParserBin(PhpProcessor.parserBinPath)

    ruleCache.setRule(configAndRules)
    val cpg = new Php2Cpg().createCpg(config).get
    AppCache.repoLanguage = Language.PHP

    X2Cpg.applyDefaultOverlays(cpg)
    Php2Cpg.postProcessingPasses(cpg).foreach(_.createAndApply())
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new LiteralTagger(cpg, ruleCache).createAndApply()

    cpgs.addOne(cpg)
    val threatEngine =
      new ThreatEngineExecutor(
        cpg,
        config.inputPath,
        ruleCache,
        null,
        dataFlowCache.getDataflowAfterDedup,
        privadoInput
      )
    (cpg, threatEngine)
  }
}
