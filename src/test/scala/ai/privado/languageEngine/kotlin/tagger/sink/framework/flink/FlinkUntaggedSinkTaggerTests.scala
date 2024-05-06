package ai.privado.languageEngine.kotlin.tagger.sink.framework.flink

import ai.privado.model.Constants
import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FlinkUntaggedSinkTaggerTests extends KotlinFrontendTestSuite {

  "Flink sink which is not tagged by any connector" should {
    val cpg = code("""
        |import org.apache.flink.streaming.api.datastream.DataStream
        |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
        |import org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer
        |import org.apache.flink.streaming.util.serialization.SimpleStringSchema
        |import ai.privado.pipeline.sink.getKafkaSink
        |
        |import java.util.Properties
        |
        |class FlinkKafkaExample {
        |    fun main(args: Array<String>) {
        |        // Set up the execution environment
        |        val env = StreamExecutionEnvironment.getExecutionEnvironment()
        |
        |        // Create a stream from a socket source
        |        val inputStream: DataStream<String> = env.socketTextStream("localhost", 9999)
        |
        |        val kafkaProducer: KafkaProducer<String> = getKafkaSink()
        |
        |        // Add the Kafka producer as a sink to the stream
        |        inputStream.addSink(kafkaProducer)
        |
        |        // Execute the Flink job
        |        env.execute("Flink Kafka Example")
        |    }
        |}
        |""".stripMargin).moreCode("", "sample.kts")

    "tag the flink's sink default flink rule" in {
      val List(flinkSink) = cpg.call("addSink").l
      flinkSink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.Flink.Producer")
    }
  }
}
