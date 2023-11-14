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

import ai.privado.dataflow.Dataflow
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.orm.GormParser
import ai.privado.languageEngine.go.tagger.sink.GoAPITagger
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.{
  CatLevelOne,
  CollectionFilter,
  ConfigAndRules,
  Constants,
  DataFlow,
  Language,
  NodeType,
  PolicyAction,
  PolicyOrThreat,
  PolicyThreatType,
  RuleInfo,
  SinkFilter,
  SourceFilter,
  SystemConfig
}
import ai.privado.tagger.source.SqlQueryTagger
import better.files.File
import ai.privado.passes.{HTMLParserPass, SQLParser}
import io.joern.x2cpg.X2Cpg
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}

abstract class GoTaggingTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  val ruleCache                    = new RuleCache()
  val privadoInput                 = PrivadoInput(generateAuditReport = true, enableAuditSemanticsFilter = true)
  val dataFlows: Map[String, Path] = Map()
  val auditCache                   = new AuditCache
  val dataFlowCache                = new DataFlowCache(auditCache)
  var inputDir: File               = File.newTemporaryDirectory()
  var outputFile: File             = File.newTemporaryFile()
  var config: Config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
  var cpg: Cpg       = new GoSrc2Cpg().createCpg(config).get

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
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )

  val threat = PolicyOrThreat(
    "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables",
    "{DataElement} was found in multiple tables",
    "{DataElement} found in multiple tables",
    """
      |Avoid storing same PII in multiple tables.
      |Reference link: https://github.com/OWASP/owasp-mstg/blob/v1.4.0/Document/0x05d-Testing-Data-Storage.md#testing-local-storage-for-sensitive-data-mstg-storage-1-and-mstg-storage-2
      |""".stripMargin,
    PolicyThreatType.THREAT,
    PolicyAction.DENY,
    DataFlow(
      List(),
      SourceFilter(Option(true), "", ""),
      List[String](),
      SinkFilter(List[String](), "", ""),
      CollectionFilter("")
    ),
    List("**"),
    Map[String, String](),
    Map[String, String](),
    "",
    Array[String]()
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
      Language.GO,
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

  val configAndRules: ConfigAndRules =
    ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())

  val taggerCache = new TaggerCache()

  def code(code: String, fileExtension: String = ".go"): Cpg = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"generalFile${fileExtension}").write(code)
    (inputDir / "unrelated.file").write("foo")
    config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    ScanProcessor.config = privadoInput
    ruleCache.setRule(configAndRules)
    cpg = new GoSrc2Cpg().createCpg(config).get
    AppCache.repoLanguage = Language.GO

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new GormParser(cpg).createAndApply()
    new SQLParser(cpg, inputDir.pathAsString, ruleCache).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new GoAPITagger(cpg, ruleCache, privadoInput).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)

    cpg
  }
}
