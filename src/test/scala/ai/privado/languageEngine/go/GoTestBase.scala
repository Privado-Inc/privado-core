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

package ai.privado.languageEngine.go

import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.languageEngine.go.tagger.sink.GoAPITagger
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.*
import ai.privado.passes.SQLParser
import ai.privado.tagger.source.SqlQueryTagger
import ai.privado.threatEngine.ThreatEngineExecutor
import better.files.File
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterAll}

import scala.collection.mutable

abstract class GoTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.LastName",
      "LastName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*lastName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.DateofBirth",
      "Date of Birth",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*dob.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.PhoneNumber",
      "Phone",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*phone.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.FinancialData.Salary",
      "Salary",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*salary.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    )
  )

  val sinkRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      FilterProperty.METHOD_FULL_NAME,
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
    ),
    RuleInfo(
      "Storages.GormFramework.Read",
      "Gorm (Read)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("gorm.io"),
      List("(?i).*(github.com)(/)(go-gorm|jinzhu)(/)(gorm).*(Find).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    ),
    RuleInfo(
      "Storages.GormFramework.Write",
      "Gorm (Write)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("gorm.io"),
      List("(?i).*(github.com)(/)(go-gorm|jinzhu)(/)(gorm).*(Create|Update|Delete|Save).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    ),
    RuleInfo(
      "Storages.GorpFramework.Read",
      "Gorp (Read)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("pkg.go.dev/github.com/go-gorp/gorp"),
      List("(?i).*(github.com|gopkg.in)(/)(gorp|go-gorp/gorp).*(Select).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    ),
    RuleInfo(
      "Storages.GormFramework.Write",
      "Gorp (Write)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("pkg.go.dev/github.com/go-gorp/gorp"),
      List("(?i).*(github.com)(/)(go-gorm|jinzhu)(/)(gorm).*(Create|Update|Delete|Save).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    ),
    RuleInfo(
      "Storages.MongoDB.Read",
      "MongoDB(Read)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("mongodb.com"),
      List("(?i)(go.mongodb.org/mongo-driver/mongo).*(Find)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    ),
    RuleInfo(
      "Storages.MongoDB.Write",
      "MongoDB(Write)",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("mongodb.com"),
      List("(?i)(go.mongodb.org/mongo-driver/mongo).*(InsertOne|DeleteOne|UpdateOne)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    )
  )

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "^(?i)(net/http|github.com/parnurzeal/gorequest|(gopkg.in|github.com/go-resty)/resty|valyala/fasthttp|github.com/gojektech/heimdall/v\\\\d/httpclient|github.com/levigross/grequests|github.com/PuerkitoBio/rehttp|github.com/machinebox/graphql).*",
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

  def code(
    code: String,
    downloadDependency: Boolean = false,
    fileExtension: String = ".go"
  ): (Cpg, ThreatEngineExecutor) = {
    val ruleCache                    = new RuleCache()
    val dataFlows: Map[String, Path] = Map()
    val auditCache                   = new AuditCache
    val privadoInput                 = PrivadoInput()
    val dataFlowCache                = new DataFlowCache(privadoInput, auditCache)

    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / s"generalFile${fileExtension}").write(code)
    val outputFile: File = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
      .withFetchDependencies(downloadDependency)

    ruleCache.setRule(configAndRules)
    val cpg = new GoSrc2Cpg().createCpg(config).get
    AppCache.repoLanguage = Language.GO

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new ORMParserPass(cpg, ruleCache).createAndApply()
    new SQLQueryParser(cpg).createAndApply()
    new SQLParser(cpg, inputDir.pathAsString, ruleCache).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new GoAPITagger(cpg, ruleCache, privadoInput).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)
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

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
    outPutFiles.foreach(_.delete())
    inputDirs.foreach(_.delete())
    super.afterAll()
  }
}
