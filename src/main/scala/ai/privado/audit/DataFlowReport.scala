package ai.privado.audit

import ai.privado.cache.{AuditCache, DataFlowCache}

import scala.collection.mutable.ListBuffer

object DataFlowReport {

  def processDataFlowAudit(): List[List[String]] = {
    val workbookResult = ListBuffer[List[String]]()

    AuditCache.getFlowBeforeFirstFiltering.foreach(flow => {
      val existInSecondFiltering = if (AuditCache.checkFlowExistInSecondFiltering(flow)) "YES" else "--"
      val existInFirstDedup      = if (AuditCache.checkFlowExistInFirstDedup(flow)) "YES" else "--"
      val existInSecondDedup     = if (AuditCache.checkFlowExistInSecondDedup(flow)) "YES" else "--"
      val existInFinal           = if (AuditCache.checkFlowExistInFinal(flow)) "YES" else "--"
      workbookResult += List(
        flow.sourceId,
        flow.sinkId,
        flow.pathId,
        getShortPath(flow.pathId),
        AuditReportConstants.AUDIT_CHECKED_VALUE,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        AuditReportConstants.AUDIT_CHECKED_VALUE,
        existInSecondFiltering,
        existInFirstDedup,
        existInSecondDedup,
        existInFinal
      )
    })

    List(
      List(
        AuditReportConstants.DATA_FLOW_SOURCE_NAME,
        AuditReportConstants.DATA_FLOW_SINK_NAME,
        AuditReportConstants.DATA_FLOW_FLOW_NAME,
        AuditReportConstants.DATA_FLOW_SHORT_FLOW_NAME,
        AuditReportConstants.DATA_FLOW_JOERN_OUTPUT_NAME,
        AuditReportConstants.DATA_FLOW_SEMANTIC_FILTER_NAME,
        AuditReportConstants.DATA_FLOW_FILTER_1_NAME,
        AuditReportConstants.DATA_FLOW_FILTER_2_NAME,
        AuditReportConstants.DATA_FLOW_DEDUP_1_NAME,
        AuditReportConstants.DATA_FLOW_DEDUP_2_NAME,
        AuditReportConstants.DATA_FLOW_FINAL_RESULT_NAME
      )
    ) ++ workbookResult.toList.groupBy(_.head).values.toList.flatten
  }

  private def getShortPath(pathId: String): String = {
    val shortPathBuilder = new StringBuffer()
    if (!AuditCache.pathIdExist(pathId)) {
      shortPathBuilder.append("NA")
    } else {
      AuditCache
        .getPathFromId(pathId)
        .elements
        .foreach(node => {
          if (shortPathBuilder.isEmpty) {
            shortPathBuilder.append(node.code)
          } else {
            shortPathBuilder.append(s" -> ${node.code}")
          }
        })
    }
    shortPathBuilder.toString
  }

}
