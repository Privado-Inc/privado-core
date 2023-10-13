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

package ai.privado.languageEngine.go.tagger

import ai.privado.cache.{AppCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class GoTaggingTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val goFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()
  val privadoInput     = PrivadoInput(limitArgExpansionDataflows = 20)

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "generalFile.go").write(goFileContents)
    (inputDir / "unrelated.file").write("foo")
    outputFile = File.newTemporaryFile()
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    cpg = new GoSrc2Cpg().createCpg(config).get

    // Caching Rule
    ruleCache.setRule(rule)
    AppCache.repoLanguage = Language.GO
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.GO,
      Array()
    )
  )

  val sinkRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      Array(),
      List(
        "((?i)((?:http:|https:|ftp:|ssh:|udp:|wss:){0,1}(\\/){0,2}[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg))"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.JAVASCRIPT,
      Array()
    )
  )

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "^(?i)(net/http|github.com/parnurzeal/gorequest|gopkg.in/resty|github.com/gojektech/heimdall/v\\\\d/httpclient|github.com/levigross/grequests|github.com/PuerkitoBio/rehttp|github.com/machinebox/graphql).*",
      Language.GO,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|open|request|execute|newCall|load|host|access|list|set|put|post|proceed|trace|patch|Path|send|remove|delete|write|read|postForEntity|call|createCall|createEndpoint|dispatch|invoke|getInput|getOutput|getResponse|do)",
      Language.GO,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.GO,
      "",
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())

  val taggerCache = new TaggerCache()
}
