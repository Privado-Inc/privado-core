package ai.privado.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.model.exporter.ViolationProcessingModel
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.semantic.Language.*
import ai.privado.threatEngine.ThreatUtility.{getPIINameFromSourceId, hasDataElements}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{SqlColumnNode, SqlTableNode, StoredNode}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

object PIIHavingDifferentRetentionPeriodWithSQL {

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
      val sourceIdRetentionPeriodMap: Map[String, Int] = getSourceIdRetentionMap(threat.config)
      val tablesWithPIIS                               = getSources(cpg)

      tablesWithPIIS.foreach((table, columns) => {
        val tableName                   = table.name
        val retentionPeriodListForTable = ListBuffer[Int]()
        var additionalDetails           = "-------------------------------------------------------------------------\n"
        additionalDetails += s"Data Element\t\t | \t\tRetention Period (days)\n"
        additionalDetails += "-------------------------------------------------------------------------\n"
        var cacheSourceId: String = ""
        columns.foreach(col => {
          val sourceId = getSourceId(col)
          if (sourceIdRetentionPeriodMap.contains(sourceId)) {
            retentionPeriodListForTable.append(sourceIdRetentionPeriodMap(sourceId))
          }
          val piiName = getPIINameFromSourceId(sourceId)
          val retentionPeriod =
            if (sourceIdRetentionPeriodMap.contains(sourceId)) sourceIdRetentionPeriodMap(sourceId)
          additionalDetails += s"${piiName}\t\t | \t\t${retentionPeriod}\n"
          if (cacheSourceId.isEmpty) {
            cacheSourceId = sourceId
          }
        })
        additionalDetails += "-------------------------------------------------------------------------"
        if (retentionPeriodListForTable.toSet.size > 1) {
          ThreatUtility.convertToViolationProcessingModelAndAddToViolatingFlows(
            Some(cacheSourceId),
            table,
            violatingFlows,
            tableName,
            Some(additionalDetails)
          )
        }
      })
      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

  private def getSourceId(node: StoredNode): String = {
    node.tag.nameExact(Constants.id).value.headOption.getOrElse(UUID.randomUUID().toString)
  }

  private def getSources(cpg: Cpg): List[(SqlTableNode, List[SqlColumnNode])] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(Constants.sources)
    }

    def groupBy(node: SqlColumnNode): SqlTableNode = {
      node.sqlTable.get
    }

    cpg.sqlColumn
      .where(filterSources)
      .groupBy(groupBy)
      .l
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
