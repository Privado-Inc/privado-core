package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.hasDataElements
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Try

object PIIHavingDifferentRetentionPeriod {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    taggerCache: TaggerCache
  ): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val violatingFlows                               = ListBuffer[ViolationProcessingModel]()
      val entityMapper                                 = EntityMapper.getClassTableMapping(cpg)
      val sourceIdRetentionPeriodMap: Map[String, Int] = getSourceIdRetentionMap(threat.config)

      entityMapper.foreach((entityWithTable) => {
        val tableName                   = entityWithTable._1
        val typeDeclFullName            = entityWithTable._2
        val retentionPeriodListForTable = ListBuffer[Int]()

        if (taggerCache.typeDeclMemberCache.contains(typeDeclFullName.fullName)) {
          val typeDeclWithMembersMap = taggerCache.typeDeclMemberCache.get(typeDeclFullName.fullName).get
          typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
            val sourceId = dataElementMemberMap._1
            if (sourceIdRetentionPeriodMap.contains(sourceId)) {
              retentionPeriodListForTable.append(sourceIdRetentionPeriodMap(sourceId))
            }
          })

          if (retentionPeriodListForTable.toSet.size > 1) {
            var excerpt =
              s"Found below table with PIIs having different retention period.\n" + s"TableName: ${tableName}\n"
            var cacheSourceId: String = ""
            typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
              val sourceId = dataElementMemberMap._1
              val retentionPeriod =
                if (sourceIdRetentionPeriodMap.contains(sourceId)) sourceIdRetentionPeriodMap(sourceId)
              excerpt += s"PII: ${sourceId} -> Retention Period: ${retentionPeriod}\n"
              if (cacheSourceId.isEmpty) {
                cacheSourceId = sourceId
              }
            })

            val occurrence = ExporterUtility.convertIndividualPathElement(typeDeclFullName).get
            val newOccurrence = DataFlowSubCategoryPathExcerptModel(
              cacheSourceId,
              occurrence.lineNumber,
              occurrence.columnNumber,
              occurrence.fileName,
              s"${excerpt}\n${occurrence.excerpt}"
            )
            violatingFlows.append(ViolationProcessingModel(cacheSourceId, newOccurrence))
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

  private def getSourceIdRetentionMap(config: Map[String, String]): Map[String, Int] = {
    val RETENTION_PERIOD_KEY                         = "retentionPeriods"
    var sourceIdRetentionPeriodMap: Map[String, Int] = HashMap()
    if (config.contains(RETENTION_PERIOD_KEY)) {
      val keyValuePairs = config.get(RETENTION_PERIOD_KEY).get.split(",")
      sourceIdRetentionPeriodMap = keyValuePairs.map { pair =>
        val keyValue = pair.split(":")
        val key      = keyValue(0).trim()
        val value    = keyValue(1).trim().toInt

        key -> value
      }.toMap
    }
    sourceIdRetentionPeriodMap
  }
}
