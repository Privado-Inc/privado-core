package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.model.Constants
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FlinkUntaggedSinkTaggerTests extends JavaFrontendTestSuite {

  "Flink sink which is not tagged by any connector" should {

    val cpg = code("""
        |import org.apache.flink.streaming.api.datastream.DataStream;
        |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment;
        |import org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer;
        |import org.apache.flink.streaming.util.serialization.SimpleStringSchema;
        |import ai.privado.pipeline.sink.getKafkaSink;
        |
        |import java.util.Properties;
        |
        |public class FlinkKafkaExample {
        |    public static void main(String[] args) throws Exception {
        |        // Set up the execution environment
        |        StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
        |
        |        // Create a stream from a socket source
        |        DataStream<String> inputStream = env.socketTextStream("localhost", 9999);
        |
        |        KafkaProducer<String> kafkaProducer = getKafkaSink();
        |
        |        // Add the Kafka producer as a sink to the stream
        |        inputStream.addSink(kafkaProducer);
        |
        |        // Execute the Flink job
        |        env.execute("Flink Kafka Example");
        |    }
        |}
        |""".stripMargin)

    "tag the flink's sink default flink rule" in {
      val List(flinkSink) = cpg.call("addSink").l
      flinkSink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.Flink.Producer")
    }
  }

}
