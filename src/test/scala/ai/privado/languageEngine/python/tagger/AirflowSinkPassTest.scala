package ai.privado.languageEngine.python.tagger

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.python.tagger.sink.AirflowOperatorSinkPass
import ai.privado.languageEngine.python.{PrivadoPySrc2CpgFixture, PrivadoPySrcTestCpg}
import ai.privado.model.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class AirflowSinkPassTest extends PrivadoPySrc2CpgFixture {
  val ruleCache = new RuleCache()

  private val sinks = List(
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

  val cpg: PrivadoPySrcTestCpg = code("""
      |from airflow import DAG
      |from airflow.operators.email_operator import EmailOperator
      |from datetime import datetime
      |
      |# Define the default arguments for the DAG
      |default_args = {
      |    'owner': 'airflow',
      |    'start_date': datetime(2024, 4, 1),
      |}
      |
      |# Define the DAG
      |dag = DAG(
      |    'email_dag',
      |    default_args=default_args,
      |    description='DAG with EmailOperator',
      |    schedule_interval='@once',
      |)
      |
      |# Define the email content
      |email_content = "Hello"
      |
      |# Define the task using EmailOperator
      |email_task = EmailOperator(
      |    task_id='send_email',
      |    to='recipient@example.com',
      |    subject='Email Notification',
      |    html_content=email_content,
      |    dag=dag,
      |)
      |""".stripMargin)

  val rule: ConfigAndRules =
    ConfigAndRules(sinks = sinks)

  override def beforeAll(): Unit = {
    super.beforeAll()
    ruleCache.setRule(rule)
    new AirflowOperatorSinkPass(cpg, ruleCache).createAndApply()
  }

  "Python code airflow" should {
    "correct" in {
      val List(callNode) = cpg.call.name(".*Email.*").l
      callNode.tag.size shouldBe 5
      callNode.tag.nameExact("id").value.head shouldBe "ThirdParties.Operator.Email"
      callNode.tag.nameExact("nodeType").value.head shouldBe "REGULAR"
      callNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
    }
  }
}

class AirflowCustomOperatorSinkTest extends PrivadoPySrc2CpgFixture {
  val cpg: PrivadoPySrcTestCpg = code("""
      |from airflow.models import BaseOperator
      |from airflow.utils.decorators import apply_defaults
      |
      |class CustomOperator(BaseOperator):
      |
      |    @apply_defaults
      |    def __init__(self, my_parameter, *args, **kwargs):
      |        super(CustomOperator, self).__init__(*args, **kwargs)
      |        self.parameter = parameter
      |
      |    def execute(self, context):
      |        s3.upload(context.firstName)
      |
      |class main():
      |   dag = DAG(
      |     'dag',
      |    default_args=default_args,
      |    description='DAG with CustomOperator'
      |   )
      |
      |   custom_task = CustomOperator(
      |     task_id='cutom_task',
      |     parameter = "something",
      |     html_content=firstName,
      |     dag=dag,
      |   )
      |""".stripMargin)

  override def beforeAll(): Unit = {
    super.beforeAll()
    new AirflowOperatorSinkPass(cpg, new RuleCache()).createAndApply()
  }

  "should rr" should {
    "correct" in {
      val List(callNode) = cpg.call.name("CustomOperator").l
      callNode.tag.size shouldBe 5
      callNode.tag.nameExact("id").value.head shouldBe "Airflow.Custom.Operator"
      callNode.tag.nameExact("nodeType").value.head shouldBe "REGULAR"
      callNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
    }
  }
}
