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

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.JavaTaggingTestBase
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.ingressUrls
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.testfixtures.JavaFrontendTestSuite
import ai.privado.rule.RuleInfoTestData

class CollectionUtilityTest extends JavaFrontendTestSuite {

  val privadoInput = PrivadoInput(enableIngressAndEgressUrls = true)
  val ruleCache    = new RuleCache()

  val collectionRule = List(
    RuleInfo(
      "Collections.Annotation.Spring",
      "Spring Web Interface Annotation",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("RequestMapping|PostMapping|PutMapping|GetMapping|DeleteMapping"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      Constants.annotations,
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules = ConfigAndRules(sources = RuleInfoTestData.sourceRule, collections = collectionRule)
  ruleCache.setRule(rule)

  "Collection utility annotation" should {

    val cpg = code("""
        |
        |@RequestMapping("/api/public/user")
        |public class AuthenticationService {
        |
        |
        | @PostMapping("/signup")
        |	public UserProfileD signup(@RequestBody String firstName) {
        | }
        |
        | @PostMapping(value = "/signin")
        |	public UserProfileD signin(@RequestBody String firstName) {
        | }
        |
        | @PostMapping
        |	public UserProfileD sample3(@RequestBody String firstName) {
        | }
        |
        | @GetMapping(value = "/login")
        |	public Token login(@RequestBody String somestring) {
        | }
        |
        | @RequestMapping(method = RequestMethod.GET, value = "/products", produces = "application/json")
        | public List<Product> getProducts() {
        |    }
        |
        | @PostMapping(produces = "application/json")
        | public List<Product> createProducts() {
        |    }
        |
        | @PutMapping(path = "/account/{uuid}")
        |	public Token updateAccount(@RequestBody String uuid) {
        | }
        |
        |}
        |""".stripMargin)
      .withPrivadoInput(privadoInput)
      .withRuleCache(ruleCache)

    "Get Url for annotation for sample1" in {
      CollectionUtility.getUrlFromAnnotation(cpg.typeDecl.annotation.head) shouldBe "/api/public/user"
    }

    "Get Url for annotation for sample2" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("signup").annotation.head) shouldBe "/signup"
    }

    "Get Url for annotation where first parameter is other than 'value' for getProducts" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("getProducts").annotation.head) shouldBe "/products"
    }

    "gGet Url for annotation where value parameter or direct url is not defined for createProducts" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("createProducts").annotation.head) shouldBe ""
    }

    "Get Url for annotation for sample3" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("sample3").annotation.head) shouldBe ""
    }

    "Get Url for annotation for updateAccount using path variable name" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("updateAccount").annotation.head) shouldBe "/account/{uuid}"
    }

    "Get Url for annotation without having PII and check ingress url" in {
      val jsonOutput  = cpg.getPrivadoJson()
      val ingressUrls = jsonOutput(Constants.ingressUrls).asArray.get.toList.distinct

      ingressUrls.size shouldBe 7
      ingressUrls.map(_.noSpaces).toList shouldBe List(
        "\"/address/{id}\"",
        "\"/api/public/user\"",
        "\"/api/public/user/signin\"",
        "\"/api/public/user/products\"",
        "\"/api/public/user/signup\"",
        "\"/api/public/user/login\"",
        "\"/api/public/user/account/{uuid}\""
      )
    }
  }
}
