package ai.privado.languageEngine.python.passes.analytics

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.python.{PrivadoPySrc2CpgFixture, PrivadoPySrcTestCpg}
import ai.privado.model.{CatLevelOne, ConfigAndRules, FilterProperty, Language, NodeType, RuleInfo}

class AirflowSinkPassTest extends PrivadoPySrc2CpgFixture {
  val ruleCache = new RuleCache()

  private val sinks = List(
    RuleInfo(
      "ThirdParties.Operator.Email",
      "Email Operator",
      "third parties",
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

  val cpg: PrivadoPySrcTestCpg = code(
    """
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
      |email_task = EmailOperator(  <= Our aim will be to tag operator like this that are provided by airflow
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
    new AirflowSinkPass(cpg, ruleCache).createAndApply()
  }

  "Python code airflow" should {
    "correct" in {

    }
  }
}

//abstract class AirflowSinkPassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {
//  var cpg: Cpg = _
//  var javaContent: String
//  var inputDir: File = _
//  var outputFile: File = _
//  val ruleCache = new RuleCache()
//
//  override def beforeAll(): Unit = {
//    inputDir = File.newTemporaryDirectory()
//    (inputDir / "test.java").write(javaContent)
//    outputFile = File.newTemporaryDirectory()
//
//    val pythonConfig = Py2CpgOnFileSystemConfig()
//      .withInputPath(inputDir.pathAsString)
//      .withOutputPath(outputFile.pathAsString)
//    cpg = new Py2CpgOnFileSystem().createCpg(pythonConfig).get
//
//    X2Cpg.applyDefaultOverlays(cpg)
//
//
//    new AirflowSinkPass(cpg, ruleCache)
//
//    super.beforeAll()
//  }
//
//  override def afterAll(): Unit = {
//    inputDir.delete()
//    cpg.close()
//    outputFile.delete()
//    super.afterAll()
//  }
//
//  val rule: ConfigAndRules =
//    ConfigAndRules()
//}
