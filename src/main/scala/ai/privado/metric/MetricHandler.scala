package ai.privado.metric
import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.exporter.GitMetaDataExporter
import ai.privado.utility.Utilities
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MetricHandler {

  private val logger             = LoggerFactory.getLogger(this.getClass)
  val metricsData                = mutable.HashMap[String, Any]()
  val scanProcessErrors          = ArrayBuffer[String]()
  var totalRulesMatched: Integer = 0
  val flowCategoryData           = mutable.HashMap[String, Int]()

  metricsData("Privado Version Core") = sys.env.get("PRIVADO_VERSION_CORE") match {
    case Some(value) => value
    case _           => None
  }
  metricsData("Privado Core Command") = None
  val gitMetaData = GitMetaDataExporter.getMetaData(AppCache.localScanPath)
  metricsData("Hashed Repo Identifier") = Utilities.getSHA256Hash(gitMetaData.size match {
    case 0 => AppCache.repoName
    case _ => gitMetaData("remoteUrl")
  })

  def timeMetric[R](block: => R, call: String): R = {
    val startTime = System.nanoTime()
    val result    = block
    val endTime   = System.nanoTime()
    // TODO remove debug statements
    // TODO confirm time units
    metricsData(s"time taken to $call") = (endTime - startTime) / 1000
    result
  }
}
