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

import ai.privado.cache.{AppCache, DatabaseDetailsCache, RuleCache, S3DatabaseDetailsCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.SinkExporter
import ai.privado.languageEngine.python.config.PythonDBConfigTagger
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.model.*
import ai.privado.tagger.sink.RegularSinkTagger
import better.files.File
import io.joern.pysrc2cpg.{
  ImportsPass,
  Py2CpgOnFileSystem,
  Py2CpgOnFileSystemConfig,
  PythonInheritanceNamePass,
  PythonTypeRecoveryPassGenerator
}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class PythonS3TaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs                   = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles            = mutable.ArrayBuffer.empty[File]
  private val inputDirs              = mutable.ArrayBuffer.empty[File]
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

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "sample.py").write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(List(), sinks, List(), List(), List(), List(), List(), List(), List(), List())
    ruleCache.setRule(rule)
    val taggerCache = new TaggerCache
    appCache.repoLanguage = Language.PYTHON
    val databaseDetailsCache = DatabaseDetailsCache()

    // Generate CPG and run overlays for S3 tagger prep
    val cpgconfig = Py2CpgOnFileSystemConfig(Option(File(".venv").path), ignoreVenvDir = true)
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
    new Py2CpgOnFileSystem()
      .createCpg(cpgconfig)
      .map { cpg =>
        X2Cpg.applyDefaultOverlays(cpg)
        new ImportsPass(cpg).createAndApply()
        new PythonInheritanceNamePass(cpg).createAndApply()
        new PythonTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
        new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
        new NaiveCallLinker(cpg).createAndApply()
        new AstLinkerPass(cpg).createAndApply()
        new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()
        new PythonDBConfigTagger(cpg, databaseDetailsCache).createAndApply()

        // Run S3 tagger - needs sink tagging to be run before
        new PythonS3Tagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache).createAndApply()
        cpgs.addOne(cpg)
        cpg
      }
      .get
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
    outPutFiles.foreach(_.delete())
    inputDirs.foreach(_.delete())
    super.afterAll()
  }

}
