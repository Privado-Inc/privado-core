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
import ai.privado.model.{Constants, Language}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.testfixtures.JavaFrontendTestSuite

import scala.collection.mutable

class JavaLanguageEgressTest extends JavaFrontendTestSuite {

  val privadoInput = PrivadoInput(enableIngressAndEgressUrls = true)

  "Java code egresses" should {

    val cpg = code(
      """
      |package com.twilio;
      |
      |import org.springframework.beans.factory.annotation.Autowired;
      |import org.springframework.http.ResponseEntity;
      |import org.springframework.stereotype.Service;
      |import org.springframework.web.client.RestTemplate;
      |
      |@Service
      |public class NasaApodService {
      |
      |    private final RestTemplate restTemplate;
      |
      |    @Autowired
      |    public NasaApodService(RestTemplate restTemplate) {
      |        this.restTemplate = restTemplate;
      |    }
      |
      |    public NasaApodResponse getNasaApod(String apiKey) {
      |        String login = "api/v1/login";
      |        String userMetaPath = "api/v1/" + "user/meta";
      |        String userId = "123";
      |        String apiKey = "key";
      |        String formatted = String.format("https://vineet%s/hellow", "api/hellow");
      |        String apiUrl = "https://abc.com/planetary/apod?api_key=" + apiKey;
      |        ResponseEntity<NasaApodResponse> responseEntity = restTemplate.getForEntity(apiUrl, NasaApodResponse.class);
      |        ResponseEntity<NasaApodResponse> responseEntity = restTemplate.getForEntity(apiUrl, NasaApodResponse.class);
      |        return responseEntity.getBody();
      |    }
      |}
      |""".stripMargin,
      "generalFile.java"
    ).withPrivadoInput(privadoInput)

    "collect egress url for java code" in {
      val jsonOutput = cpg.getPrivadoJson()

      val k = jsonOutput(Constants.egressUrlsFromCode).asArray.get.map(_.noSpaces).toList shouldBe List(
        "\"api/v1/login\"",
        "\"api/v1/user/meta\"",
        "\"https://vineet%s/hellow\"",
        "\"api/hellow\"",
        "\"https://abc.com/planetary/apod?api_key=apiKey\""
      )
    }
  }
}
