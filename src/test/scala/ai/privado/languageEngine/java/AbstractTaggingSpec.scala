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

package ai.privado.languageEngine.java

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.*
import ai.privado.model.Language.{JAVA, KOTLIN, Language}
import better.files.File
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.kotlin2cpg.Kotlin2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

abstract class AbstractTaggingSpec(val language: Language)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  var inputDir: File = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    inputDir = File.newTemporaryDirectory()
    (inputDir / "unrelated.file").write("foo")
    AppCache.repoLanguage = this.language
  }

  override def afterAll(): Unit = {
    super.afterAll()
    inputDir.delete()
  }

  def buildCpg(sourceSnippet: String): Cpg = {
    File
      .newTemporaryFile("sourceFile", LanguageFileExt.withLanguage(this.language), Some(inputDir))
      .writeText(sourceSnippet)
    val outputFile = File.newTemporaryFile()
    var cpg: Cpg   = null
    if (language == JAVA) {
      val config =
        io.joern.javasrc2cpg.Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
      cpg = new JavaSrc2Cpg().createCpg(config).get
    } else if (language == KOTLIN) {
      val config =
        io.joern.kotlin2cpg.Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
      cpg = new Kotlin2Cpg().createCpg(config).get
    } else {
      throw RuntimeException("Unknown language encountered while building test CPG")
    }
    applyDefaultOverlays(cpg)
    cpg
  }

  def ruleCacheWithCollectionRule(ruleInfo: RuleInfo): RuleCache = {
    val ruleCache = RuleCache()
    ruleCache.setRule(ConfigAndRules(collections = List(ruleInfo)))
    ruleCache
  }

}
