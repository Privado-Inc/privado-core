package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, Language, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.sink.api.APIValidator
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FlinkUserDefinedSinkTaggerTests extends JavaFrontendTestSuite with APIValidator {

  "Flink tagger" should {

    val systemConfig = List(apiIdentifier, apiHttpLibraries, apiSinks)
    val ruleCache    = RuleCache().setRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))

    val cpg = code(
      """
        |import org.apache.flink.configuration.Configuration;
        |import org.apache.flink.streaming.api.functions.sink.RichSinkFunction;
        |import org.apache.http.HttpEntity;
        |import org.apache.http.HttpResponse;
        |import org.apache.http.client.methods.HttpPost;
        |import org.apache.http.entity.StringEntity;
        |import org.apache.http.impl.client.CloseableHttpClient;
        |import org.apache.http.impl.client.HttpClients;
        |import java.io.BufferedReader;
        |import java.io.InputStreamReader;
        |
        |public class ApiSink<T> extends RichSinkFunction<T> {
        |
        |    private String apiUrl;
        |    private transient CloseableHttpClient httpClient;
        |
        |    public ApiSink(String apiUrl) {
        |        this.apiUrl = apiUrl;
        |    }
        |
        |    @Override
        |    public void open(Configuration parameters) throws Exception {
        |        super.open(parameters);
        |        // Initialize HTTP client
        |        httpClient = HttpClients.createDefault();
        |    }
        |
        |    @Override
        |    public void close() throws Exception {
        |        super.close();
        |        // Close HTTP client
        |        if (httpClient != null) {
        |            httpClient.close();
        |        }
        |    }
        |
        |    @Override
        |    public void invoke(T value, Context context) throws Exception {
        |        // Assuming value is a string in this example
        |        String data = value.toString();
        |        // Prepare HTTP request
        |        HttpPost httpPost = new HttpPost(apiUrl);
        |        httpPost.setHeader("Content-Type", "application/json");
        |        StringEntity stringEntity = new StringEntity(data);
        |        httpPost.setEntity(stringEntity);
        |        // Execute HTTP request
        |        HttpResponse response = httpClient.execute(httpPost);
        |        // Handle response
        |        int statusCode = response.getStatusLine().getStatusCode();
        |        if (statusCode == 200) {
        |            // Request successful
        |            // Optionally, you can read the response content
        |            HttpEntity entity = response.getEntity();
        |            if (entity != null) {
        |                BufferedReader reader = new BufferedReader(new InputStreamReader(entity.getContent()));
        |                String line;
        |                while ((line = reader.readLine()) != null) {
        |                    // Process response content
        |                    System.out.println(line);
        |                }
        |            }
        |        } else {
        |            // Request failed
        |            throw new RuntimeException("Failed to send data to API. Status code: " + statusCode);
        |        }
        |    }
        |}
        |
        |""".stripMargin,
      "ApiSink.java"
    ).moreCode(
      """
        |import org.apache.flink.streaming.api.datastream.DataStream;
        |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment;
        |
        |public class FlinkApiSinkExample {
        |
        |    public static void main(String[] args) throws Exception {
        |     sample1();
        |
        |    }
        |
        |    public static void sample1() {
        |        StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
        |
        |        DataStream<String> dataStream = env.fromElements("Data 1", "Data 2", "Data 3");
        |
        |        String apiUrl = "http://example.com/api";
        |
        |        dataStream.addSink(new ApiSink<String>(apiUrl));
        |
        |        env.execute("Flink API Sink Example");
        |    }
        |}
        |""".stripMargin,
      "Main.java"
    ).withRuleCache(ruleCache)

    "tag api call defined inside custom flink connector" in {
      val List(apiSink) = cpg.call("execute").where(_.method.name("invoke")).l
      assertAPISinkCall(apiSink)
      assertAPIEndpointURL(apiSink, "apiUrl")
    }

    "tag flink sink node when custom sink is initialised in the same method as that of flink sink from `sample1`" in {
      val List(flinkSink) = cpg.call("addSink").where(_.method.name("sample1")).l
      assertAPISinkCall(flinkSink)
      assertAPIEndpointURL(flinkSink, "apiUrl")
    }
  }
}
