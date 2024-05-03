package ai.privado.languageEngine.kotlin.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, FilterProperty, Language, RuleInfo, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FlinkDefaultConnectorToFlinkSinkViaDataflowTaggerTests extends KotlinFrontendTestSuite {

  val flinkKafkaConnectorRule = RuleInfo(
    "Messaging.Queue.Kafka.Producer",
    "Apache Kafka (Producer)",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array("apache.com"),
    List(
      "(org.apache.flink.connector.kafka.sink.KafkaSink.builder|org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer).*"
    ),
    language = Language.JAVA
  )

  val systemConfig = SystemConfig(Constants.flinkConnectorProducerRuleIds, "(Messaging.Queue.Kafka.Producer)")

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sinks = RuleInfoTestData.rule.sinks ++ List(flinkKafkaConnectorRule), systemConfig = List(systemConfig))
  )

  "Flink default connector not present in the same method as the flink sink" should {
    val cpg = code("""
        |import org.apache.flink.streaming.api.datastream.DataStream
        |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
        |import org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer
        |import org.apache.flink.streaming.util.serialization.SimpleStringSchema
        |import java.util.Properties
        |
        |object FlinkKafkaExample {
        |    @JvmStatic
        |    fun main(args: Array<String>) {
        |        // Set up the execution environment
        |        val env = StreamExecutionEnvironment.getExecutionEnvironment()
        |
        |        // Create a stream from a socket source
        |        val inputStream: DataStream<String> = env.socketTextStream("localhost", 9999)
        |
        |        val kafkaProducer = getKafkaSink()
        |
        |        // Add the Kafka producer as a sink to the stream
        |        inputStream.addSink(kafkaProducer)
        |
        |        // Execute the Flink job
        |        env.execute("Flink Kafka Example")
        |    }
        |
        |    fun getKafkaSink(): FlinkKafkaProducer<String> {
        |        // Define the properties for the Kafka producer
        |        val kafkaProps = Properties()
        |        kafkaProps.setProperty("bootstrap.servers", "localhost:9092")
        |
        |        // Define the Kafka producer
        |        return FlinkKafkaProducer("kafka-topic", SimpleStringSchema(), kafkaProps)
        |    }
        |}
        |
        |""".stripMargin).withRuleCache(ruleCache)

    // This needs a fix in joern, which David will be working on
    "tag the flink connector as a sink" ignore {
      val List(flinkConnectorSink) = cpg.call.name("<init>").methodFullName(".*FlinkKafkaProducer.*").l
      flinkConnectorSink.tag.nameExact(Constants.id).value.l shouldBe List("Messaging.Queue.Kafka.Producer")
    }

    // Fixing above test case should fix this as well
    "tag the flink's sink with connector's rule" ignore {
      val List(flinkSink) = cpg.call("addSink").l
      flinkSink.tag.nameExact(Constants.id).value.l shouldBe List("Messaging.Queue.Kafka.Producer")
    }

    // Fixing above test case should fix this as well
    "not tag the flink sink with default rule" ignore {
      val List(flinkSink) = cpg.call("addSink").l
      flinkSink.tag.nameExact(Constants.id).valueExact("ThirdParties.SDK.Flink.Producer").size shouldBe (0)
    }
  }
}
