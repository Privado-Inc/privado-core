package ai.privado.utility

import ai.privado.cache.RuleCache
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class SampleConcurrentProcessor(result: ListBuffer[String])
    extends ConcurrentProcessor[String, ListBuffer[String]](result, new RuleCache) {
  override def generateParts(): Array[String] = Array("first", "second", "third", "forth")

  override def runOnPart(part: String): Unit = {
    addInWriterQueue("ADD", s"${part}-message")
  }

  override def processCommand(command: String, item: Any, result: ListBuffer[String]): Unit = {
    command match
      case "ADD" => result += item.asInstanceOf[String]
      case _     => println("This command is not handled")
  }
}

class ConcurrentProcessorTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Concurrent processor sample" in {
    val results = new SampleConcurrentProcessor(ListBuffer.empty[String]).createAndApply()
    results.contains("first-message") shouldBe true
    results.contains("second-message") shouldBe true
    results.contains("third-message") shouldBe true
    results.contains("forth-message") shouldBe true
  }
}
