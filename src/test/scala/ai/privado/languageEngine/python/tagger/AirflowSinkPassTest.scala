package ai.privado.languageEngine.python.tagger

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.python.tagger.sink.AirflowOperatorSinkTagger
import ai.privado.model.*
import ai.privado.testfixtures.PythonFrontendTestSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

object AirflowRules {
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

  def getRuleCache: RuleCache = {
    val rule                 = ConfigAndRules(sinks = sinks)
    val ruleCache: RuleCache = new RuleCache()
    ruleCache.setRule(rule)

    ruleCache
  }
}

class AirflowSinkPassTest extends PythonFrontendTestSuite {
  val ruleCache = AirflowRules.getRuleCache
  val cpg = code(
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
      |""".stripMargin,
    "code.py"
  )
    .withRuleCache(ruleCache)

  new AirflowOperatorSinkTagger(cpg, ruleCache).createAndApply()

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

class AirflowCustomOperatorSinkTest extends PythonFrontendTestSuite {
  val ruleCache = AirflowRules.getRuleCache
  val cpg = code(
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
      |""".stripMargin,
    "code.py"
  ).withRuleCache(ruleCache)

  new AirflowOperatorSinkTagger(cpg, ruleCache).createAndApply()

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
