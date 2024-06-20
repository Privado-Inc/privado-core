package ai.privado.languageEngine.python.tagger

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.python.tagger.sink.AirflowOperatorSinkTagger
import ai.privado.languageEngine.python.{PrivadoPySrc2CpgFixture, PrivadoPySrcTestCpg}
import ai.privado.model.*
import better.files.File
import io.joern.pysrc2cpg
import io.joern.pysrc2cpg.{Py2CpgOnFileSystem, Py2CpgOnFileSystemConfig}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AirflowSinkPassTest extends PythonAirflowOperatorTestBase {

  override val codeFileContents: String =
    """
      |from airflow import DAG
      |from airflow.operators import EmailOperator
      |from datetime import datetime
      |
      |class main:
      | default_args = {
      |   'owner': 'airflow',
      |   'start_date': datetime(2024, 4, 1)
      | }
      |
      | dag = DAG(
      |   'email_dag',
      |   default_args=default_args,
      |   description='DAG with EmailOperator',
      |   schedule_interval='@once'
      | )
      |
      | email_content = "Hello"
      |
      | email_task = EmailOperator(
      |   task_id='send_email',
      |   to='recipient@example.com',
      |   subject='Email Notification',
      |   html_content=email_content,
      |   dag=dag
      | )
      |""".stripMargin

  "Python airflow core operator" should {
    "should correct tagging" in {
      val List(callNode) = cpg.call.name(".*Email.*").l
      callNode.tag.size shouldBe 5
      callNode.tag.nameExact("id").value.head shouldBe "ThirdParties.Operator.Email"
      callNode.tag.nameExact("nodeType").value.head shouldBe "REGULAR"
      callNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
    }
  }
}

class AirflowCustomOperatorSinkTest extends PythonAirflowOperatorTestBase {

  override val codeFileContents: String =
    """
      |from airflow.models import BaseOperator
      |from airflow.utils.decorators import apply_defaults
      |
      |class CustomOperator(BaseOperator):
      |  @apply_defaults
      |  def __init__(self, my_parameter, *args, **kwargs):
      |   super(CustomOperator, self).__init__(*args, **kwargs)
      |   self.parameter = parameter
      |
      |  def execute(self, context):
      |   s3.upload(context.firstName)
      |
      |class main:
      |  dag = DAG(
      |   'dag',
      |	  default_args=default_args,
      |   description='DAG with CustomOperator'
      |  )
      |
      |  custom_task = CustomOperator(
      |   task_id='cutom_task',
      |   parameter = "something",
      |   html_content=firstName,
      |   dag=dag,
      |  )
      |""".stripMargin

  "python airflow custom operator" should {
    "should correct tagging" in {
      val List(callNode) = cpg.call.name("CustomOperator").l
      callNode.tag.size shouldBe 5
      callNode.tag.nameExact("id").value.head shouldBe "Airflow.Custom.Operator"
      callNode.tag.nameExact("nodeType").value.head shouldBe "REGULAR"
      callNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
    }
  }
}

abstract class PythonAirflowOperatorTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  var cpg: Cpg = _
  val codeFileContents: String
  var inputDir: File       = _
  var outputFile: File     = _
  var ruleCache: RuleCache = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.py").write(codeFileContents)

    outputFile = File.newTemporaryDirectory()

    val pythonConfig = Py2CpgOnFileSystemConfig()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
    cpg = new Py2CpgOnFileSystem().createCpg(pythonConfig).get

    X2Cpg.applyDefaultOverlays(cpg)
    ruleCache.setRule(rule)

    new AirflowOperatorSinkTagger(cpg, ruleCache).createAndApply()
    super.beforeAll()
  }

  val sinks = List(
    RuleInfo(
      "ThirdParties.Operator.Email",
      "Email Operator",
      "third parties",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(".*EmailOperator.*"),
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

  val rule = ConfigAndRules(sinks = sinks)

}
