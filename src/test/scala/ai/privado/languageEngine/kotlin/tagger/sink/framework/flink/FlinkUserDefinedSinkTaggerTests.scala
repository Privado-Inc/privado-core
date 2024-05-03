package ai.privado.languageEngine.kotlin.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.sink.JavaLeakageValidator
import ai.privado.tagger.sink.api.APIValidator
import ai.privado.model.{Constants, InternalTag, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FlinkUserDefinedSinkTaggerTests extends KotlinFrontendTestSuite with APIValidator with JavaLeakageValidator {

  // overriding to unresolvedNamespace, due to kotlin's type resolution
  override val apiHttpLibraries = SystemConfig(Constants.apiHttpLibraries, ".*Namespace.*")
  "Flink tagger" should {

    val systemConfig = List(apiIdentifier, apiHttpLibraries, apiSinks)
    val ruleCache = RuleCache().setRule(
      RuleInfoTestData.rule.copy(sinks = RuleInfoTestData.rule.sinks ++ List(leakageRule), systemConfig = systemConfig)
    )

    val cpg = code(
      """
        |import org.apache.flink.configuration.Configuration
        |import org.apache.flink.streaming.api.functions.sink.RichSinkFunction
        |import org.apache.http.HttpEntity
        |import org.apache.http.HttpResponse
        |import org.apache.http.client.methods.HttpPost
        |import org.apache.http.entity.StringEntity
        |import org.apache.http.impl.client.CloseableHttpClient
        |import org.apache.http.impl.client.HttpClients
        |import java.io.BufferedReader
        |import java.io.InputStreamReader
        |
        |class ApiSink<T>(private val apiUrl: String) : RichSinkFunction<T>() {
        |    private var httpClient: CloseableHttpClient? = null
        |
        |    override fun open(parameters: Configuration) {
        |        super.open(parameters)
        |        // Initialize HTTP client
        |        httpClient = HttpClients.createDefault()
        |    }
        |
        |    override fun close() {
        |        super.close()
        |        // Close HTTP client
        |        httpClient?.close()
        |    }
        |
        |    override fun invoke(value: T, context: Context) {
        |        // Assuming value is a string in this example
        |        val data = value.toString()
        |        // Prepare HTTP request
        |        val httpPost = HttpPost(apiUrl)
        |        httpPost.setHeader("Content-Type", "application/json")
        |        val stringEntity = StringEntity(data)
        |        httpPost.entity = stringEntity
        |        // Execute HTTP request
        |        val response = httpClient!!.execute(httpPost)
        |        // Handle response
        |        val statusCode = response.statusLine.statusCode
        |        if (statusCode == 200) {
        |            // Request successful
        |            // Optionally, you can read the response content
        |            val entity: HttpEntity? = response.entity
        |            if (entity != null) {
        |                val reader = BufferedReader(InputStreamReader(entity.content))
        |                var line: String?
        |                while (reader.readLine().also { line = it } != null) {
        |                    // Process response content
        |                    println(line)
        |                }
        |            }
        |        } else {
        |            // Request failed
        |            throw RuntimeException("Failed to send data to API. Status code: $statusCode")
        |        }
        |    }
        |}
        |""".stripMargin,
      "ApiSink.kt"
    ).moreCode(
      """
        |import org.apache.flink.streaming.api.datastream.DataStream
        |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
        |
        |fun main(args: Array<String>) {
        |    sample1()
        |    sample2()
        |}
        |
        |fun sample1() {
        |    val env = StreamExecutionEnvironment.getExecutionEnvironment()
        |    val dataStream: DataStream<String> = env.fromElements("Data 1", "Data 2", "Data 3")
        |    val apiUrl = "http://example.com/api"
        |    dataStream.addSink(ApiSink(apiUrl))
        |    env.execute("Flink API Sink Example")
        |}
        |
        |fun sample2() {
        |    val env = StreamExecutionEnvironment.getExecutionEnvironment()
        |    val dataStream: DataStream<String> = env.fromElements("Data 1", "Data 2", "Data 3")
        |    val apiUrl = "http://example.com/api"
        |    val apiSink = ApiSink(apiUrl)
        |    dataStream.addSink(apiSink)
        |    env.execute("Flink API Sink Example")
        |}
        |""".stripMargin,
      "Main.kt"
    ).withRuleCache(ruleCache)

    "tag api call defined inside custom flink connector" in {
      val List(apiSink) = cpg.call("execute").where(_.method.name("invoke")).l
      assertAPISinkCall(apiSink)
      assertAPIEndpointURL(apiSink, "apiUrl")
    }

    "tag leakage sink, but not flink sink as leakage" in {
      val List(leakageSink) = cpg.call("println").where(_.method.name("invoke")).l
      assertLeakageSink(leakageSink)

      val List(flinkSink1) = cpg.call("addSink").where(_.method.name("sample1")).l
      assertNotLeakageSink(flinkSink1)

      val List(flinkSink2) = cpg.call("addSink").where(_.method.name("sample2")).l
      assertNotLeakageSink(flinkSink2)
    }

    "tag flink sink node when custom sink is initialised in the same method as that of flink sink from `sample1`" in {
      val List(flinkSink) = cpg.call("addSink").where(_.method.name("sample1")).l
      assertAPISinkCall(flinkSink)
      assertAPIEndpointURL(flinkSink, "apiUrl")
    }

    // TODO This test is not working, as in kotlin local nodes are not created when objects are created
    "tag flink sink node when custom sink is initialised in the same method as that of flink sink from `sample2`" ignore {
      val List(flinkSink) = cpg.call("addSink").where(_.method.name("sample2")).l
      assertAPISinkCall(flinkSink)
      assertAPIEndpointURL(flinkSink, "apiUrl")
    }
  }
}
