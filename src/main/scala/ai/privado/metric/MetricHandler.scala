package ai.privado.metric
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MetricHandler {

  private val logger    = LoggerFactory.getLogger(this.getClass)
  val metricsData       = new mutable.HashMap[String, Any]()
  val scanProcessErrors = new ArrayBuffer[String]()

  metricsData("Privado Version Core") = sys.env.get("PRIVADO_VERSION_CORE") match {
    case Some(value) => value
    case _           => None
  }

  metricsData("Privado Core Command") = None

  def timeMetric[R](block: => R, call: String): R = {
    val startTime = System.nanoTime()
    val result    = block
    val endTime   = System.nanoTime()
    metricsData(s"time taken to $call") = (endTime - startTime) / 1000
    println(metricsData)
    result
  }
}
