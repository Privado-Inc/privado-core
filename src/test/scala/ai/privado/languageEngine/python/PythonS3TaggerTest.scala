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

package ai.privado.languageEngine.python

import ai.privado.cache.{AppCache, DatabaseDetailsCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.SinkExporter
import ai.privado.languageEngine.python.config.PythonDBConfigTagger
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.model.*
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.testfixtures.PythonFrontendTestSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers

class PythonS3TaggerTest extends PythonFrontendTestSuite with Matchers with BeforeAndAfterAll {
  private val ruleCache              = new RuleCache()
  private val privadoInput           = PrivadoInput()
  private val s3DatabaseDetailsCache = new S3DatabaseDetailsCache()
  val appCache                       = new AppCache()

  private val sinks = List(
    RuleInfo(
      "Storages.AmazonS3.ReadAndWrite",
      "Amazon S3",
      "Storage",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(".*Bucket.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.PYTHON,
      Array()
    )
  )

  ruleCache.setRule(ConfigAndRules(List(), sinks, List(), List(), List(), List(), List(), List(), List(), List()))

  "Python code using boto client for S3 buckets with simple assignment" should {
    val cpg = code("""
        |import boto3
        |
        |OPTIMIZELY_EVENTS_BUCKET = "mera-bucket"
        |
        |def get_s3():
        |    return boto3.resource(
        |        's3',
        |        aws_access_key_id="ID",
        |        aws_secret_access_key="KEY",
        |    )
        |
        |bucket = get_s3().Bucket(OPTIMIZELY_EVENTS_BUCKET)
        |""".stripMargin)
      .withRuleCache(ruleCache)

    val databaseDetailsCache = DatabaseDetailsCache()
    new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()
    new PythonDBConfigTagger(cpg, databaseDetailsCache).createAndApply()
    new PythonS3Tagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache).createAndApply()

    "have bucket name" in {
      val sinkExporter =
        new SinkExporter(
          cpg,
          ruleCache,
          privadoInput,
          None,
          s3DatabaseDetailsCache,
          appCache = appCache,
          databaseDetailsCache = DatabaseDetailsCache()
        )
      sinkExporter.getSinks.head.databaseDetails.dbName shouldBe "mera-bucket"
    }
  }

  // TODO: Enable when we support deeper extraction of s3 literals from assignments
  "Python code using boto client having complex bucket assignment" ignore {
    val cpg = code("""
        |import boto3
        |from botocore.client import BaseClient
        |
        |Environment = namedtuple('Environment', ['bucket'])
        |
        |SOME_ENVIRONMENT = {
        |    'prod': Environment('meri-prod-bucket'),
        |}
        |
        |def upload_to_s3():
        |	s3_client = BaseClient()
        |	environment = 'prod'
        |        s3_bucket = SOME_ENVIRONMENT[environment].bucket
        |        s3_client.put_object(Body=content, Bucket=s3_bucket, Key=key,  None)
        |
        |""".stripMargin)

    "have bucket name" in {
      val sinkExporter =
        new SinkExporter(
          cpg,
          ruleCache,
          privadoInput,
          None,
          s3DatabaseDetailsCache,
          appCache = appCache,
          databaseDetailsCache = DatabaseDetailsCache()
        )
      sinkExporter.getSinks.head.databaseDetails.dbName shouldBe "meri-prod-bucket"
    }
  }
}
