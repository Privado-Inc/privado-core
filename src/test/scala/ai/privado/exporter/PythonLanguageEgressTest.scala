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

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.HttpConnectionMetadataExporter
import ai.privado.languageEngine.javascript.JavascriptTaggingTestBase
import ai.privado.languageEngine.python.{PrivadoPySrc2CpgFixture, PrivadoPySrcTestCpg}
import ai.privado.model.ConfigAndRules
import ai.privado.tagger.sink.RegularSinkTagger
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class PythonLanguageEgressTest extends PrivadoPySrc2CpgFixture {
  val ruleCache = new RuleCache()
  val cpg: PrivadoPySrcTestCpg = code("""
      | LOGGING = {
      |    'version': 1,
      |    'disable_existing_loggers': True,
      |    'formatters': {
      |        'standard': {
      |            'format': "[%(asctime)s] %(levelname)s [%(name)s:%(lineno)s] %(message)s",
      |            'datefmt': "%d/%b/%Y %H:%M:%S"
      |        },
      |    },
      |    'handlers': {
      |        'slack_admins': {
      |            'level': 'ERROR',
      |            'class': 'django_slack.log.SlackExceptionHandler',
      |            'filters': ['slack_filter']
      |        },
      |    },
      |    'loggers': {
      |        'django': {
      |            'handlers': ['slack_admins', 'console', 'mail_admins'],
      |            'propagate': True,
      |            'level': 'ERROR',
      |        },
      |    }
      | }
      | API_HOST = ""
      | API_1 =  API_HOST + "/ce/something/customers/{customerId}/init"
      | API_2 =  API_HOST + "/ce/customers/{customerId}/repo/{repoId}/file"
      | API_3 =      API_HOST + "/ce/customers/{customerId}/scan/{repoId}"
      | BASE_HOST = "api/v1"
      | API_4 =      BASE_HOST + "/ce/customers/{customerId}/scan/{repoId}"
      | API_5 =      "api/v2" + "/ce/customers"
      |""".stripMargin)

  "Python code egresses" should {
    "collect egress url for python code" in {
      val propertyExporter          = new HttpConnectionMetadataExporter(cpg, ruleCache)
      val egressesFromLanguageFiles = propertyExporter.getLiteralsFromLanguageFiles
      print(egressesFromLanguageFiles)
      egressesFromLanguageFiles.size shouldBe 6
      egressesFromLanguageFiles shouldBe List(
        "API_HOST/ce/something/customers/{customerId}/init",
        "API_HOST/ce/customers/{customerId}/repo/{repoId}/file",
        "API_HOST/ce/customers/{customerId}/scan/{repoId}",
        "api/v1",
        "BASE_HOST/ce/customers/{customerId}/scan/{repoId}",
        "api/v2/ce/customers"
      )

    }
  }

}
