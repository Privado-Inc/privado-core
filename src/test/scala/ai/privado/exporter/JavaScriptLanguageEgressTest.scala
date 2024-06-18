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

package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.HttpConnectionMetadataExporter
import ai.privado.languageEngine.javascript.JavascriptTaggingTestBase
import ai.privado.model.{ConfigAndRules, Language}
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.rule.RuleInfoTestData

import scala.collection.mutable

class JavaScriptLanguageEgressTest extends JavaScriptFrontendTestSuite {

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sources = RuleInfoTestData.rule.sources, sinks = RuleInfoTestData.rule.sinks)
  )

  val appCache = new AppCache()

  "Javascript code egresses" should {
    val cpg = code("""
        | import { requests } from "service/settings"
        | const endpoint = { getUserDetails: (id) => `v1/api/user${id}`, getLogin: "v1/api/login" }
        | const signup = "v1/api" + "/signup"
        | const tag = "<div>something else <h1>heelo</h1></div>"
        | const newEndpoint = "api/v1/" + "user/meta" + "/profile"
        | const signup = requests("v1/api/users/meta")
        |""".stripMargin)
    "collect egress url for javascript code" in {
      appCache.repoLanguage = Language.JAVASCRIPT
      val httpConnectionExporter    = new HttpConnectionMetadataExporter(cpg, ruleCache, appCache)
      val egressesFromLanguageFiles = httpConnectionExporter.getEgressUrlsFromCodeFiles
      egressesFromLanguageFiles.size shouldBe 6
      egressesFromLanguageFiles shouldBe List(
        "v1/api/userid",
        "v1/api/login",
        "v1/api/signup",
        "api/v1/user/meta",
        "api/v1/ + user/meta/profile",
        "v1/api/users/meta"
      )
    }
  }

}

//class JavaScriptLanguageEgressTest extends JavascriptTaggingTestBase {
//
//  override val rule: ConfigAndRules            = ConfigAndRules()
//  override val packageJsonFileContents: String = ""
//  val appCache                                 = new AppCache()
//
//  override def beforeAll(): Unit = {
//    super.beforeAll()
//    appCache.repoLanguage = Language.JAVA
//  }
//
//  override val javascriptFileContents: String =
//    """
//      | import { requests } from "service/settings"
//      | const endpoint = { getUserDetails: (id) => `v1/api/user${id}`, getLogin: "v1/api/login" }
//      | const signup = "v1/api" + "/signup"
//      | const tag = "<div>something else <h1>heelo</h1></div>"
//      | const newEndpoint = "api/v1/" + "user/meta" + "/profile"
//      | const signup = requests("v1/api/users/meta")
//      |""".stripMargin
//
//  "Javascript code egresses" should {
//    "collect egress url for javascript code" in {
//      val httpConnectionExporter    = new HttpConnectionMetadataExporter(cpg, ruleCache, appCache)
//      val egressesFromLanguageFiles = httpConnectionExporter.getEgressUrlsFromCodeFiles
//      egressesFromLanguageFiles.size shouldBe 6
//      egressesFromLanguageFiles shouldBe List(
//        "v1/api/userid",
//        "v1/api/login",
//        "v1/api/signup",
//        "api/v1/user/meta",
//        "api/v1/ + user/meta/profile",
//        "v1/api/users/meta"
//      )
//    }
//  }
//
//}
