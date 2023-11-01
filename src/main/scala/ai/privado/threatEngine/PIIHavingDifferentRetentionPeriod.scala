package ai.privado.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ThreatUtility.{getPIINameFromSourceId, hasDataElements}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
          val typeDeclWithMembersMap = taggerCache.getTypeDeclMemberCacheItem(typeDeclFullName.fullName)
          typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
            val sourceId = dataElementMemberMap._1
            if (sourceIdRetentionPeriodMap.contains(sourceId)) {
              retentionPeriodListForTable.append(sourceIdRetentionPeriodMap(sourceId))
            }
          })

          if (retentionPeriodListForTable.toSet.size > 1) {
            var additionalDetails = "-------------------------------------------------------------------------\n"
            additionalDetails += s"Data Element\t\t | \t\tRetention Period (days)\n"
            additionalDetails += "-------------------------------------------------------------------------\n"
            var cacheSourceId: String = ""
            typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
              val sourceId = dataElementMemberMap._1
              val piiName  = getPIINameFromSourceId(sourceId)
              val retentionPeriod =
                if (sourceIdRetentionPeriodMap.contains(sourceId)) sourceIdRetentionPeriodMap(sourceId)
              additionalDetails += s"${piiName}\t\t | \t\t${retentionPeriod}\n"
              if (cacheSourceId.isEmpty) {
                cacheSourceId = sourceId
              }
            })
            additionalDetails += "-------------------------------------------------------------------------"

            ThreatUtility.convertToViolationProcessingModelAndAddToViolatingFlows(
              Some(cacheSourceId),
              typeDeclFullName,
              violatingFlows,
              tableName,
              Some(additionalDetails)
            )
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

  private def getSourceIdRetentionMap(config: Map[String, String]): Map[String, Int] = {
    val sourceIdRetentionPeriodMap: ArrayBuffer[(String, Int)] = ArrayBuffer()

    config.foreach { case (source, value) =>
      val sourceId        = source.trim()
      val retentionPeriod = value.trim().toInt
      sourceIdRetentionPeriodMap.addAll(List((sourceId, retentionPeriod)))
    }

    sourceIdRetentionPeriodMap.toMap
  }
}
