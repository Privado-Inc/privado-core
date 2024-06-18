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

import ai.privado.cache.{AppCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.HttpConnectionMetadataExporter
import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.testfixtures.JavaFrontendTestSuite
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}

import scala.collection.mutable

class FeignEgressExportTest extends JavaFrontendTestSuite {

  val ruleCache = new RuleCache()
  val appCache  = new AppCache()
  appCache.repoLanguage = Language.JAVA

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

  val rule: ConfigAndRules = ConfigAndRules(collections = collectionRule)
  ruleCache.setRule(rule)

  "Java code for Feign Client" should {
    val cpg = code("""
      |package com.gfg.employeaap.feignclient;
      |
      |import com.gfg.employeaap.response.AddressResponse;
      |import org.springframework.cloud.openfeign.FeignClient;
      |import org.springframework.http.ResponseEntity;
      |import org.springframework.web.bind.annotation.GetMapping;
      |import org.springframework.web.bind.annotation.PathVariable;
      |
      |@FeignClient(name = "address-service", url = "http://localhost:8081", path = "/address-service")
      |public interface AddressClient {
      |
      |	@GetMapping(value = "/address/{id}")
      |	public ResponseEntity<AddressResponse> getAddressByEmployeeId(@PathVariable("id") int id);
      |
      |}
      |
      |""".stripMargin)

    "collect egress url from feign client" in {
      val propertyExporter = new HttpConnectionMetadataExporter(cpg, ruleCache, appCache)
      val egresses         = propertyExporter.getEgressUrls
      egresses.size shouldBe 1
      egresses.head shouldBe "address-service/address/{id}"
    }
  }

}
