package ai.privado.audit

import ai.privado.cache.{AuditCache, DataFlowCache}

import scala.collection.mutable.ListBuffer

object DataFlowReport {

  def processDataFlowAudit(auditCache: AuditCache): List[List[String]] = {
    val workbookResult = ListBuffer[List[String]]()

    auditCache.getFlowBeforeSemantics.foreach(flow => {
      val existInFirstFiltering  = if (auditCache.checkFlowExistInFirstFiltering(flow)) "YES" else "--"
      val existInSecondFiltering = if (auditCache.checkFlowExistInSecondFiltering(flow)) "YES" else "--"
      val existInFirstDedup      = if (auditCache.checkFlowExistInFirstDedup(flow)) "YES" else "--"
      val existInSecondDedup     = if (auditCache.checkFlowExistInSecondDedup(flow)) "YES" else "--"
      val existInFinal           = if (auditCache.checkFlowExistInFinal(flow)) "YES" else "--"

      workbookResult += List(
        flow.sourceId,
        flow.sinkId,
        flow.pathId,
        getShortPath(flow.pathId, auditCache),
        AuditReportConstants.AUDIT_CHECKED_VALUE,
        AuditReportConstants.AUDIT_CHECKED_VALUE,
        existInFirstFiltering,
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

  private def getShortPath(pathId: String, auditCache: AuditCache): String = {
    val shortPathBuilder = new StringBuffer()
    if (!auditCache.pathIdExist(pathId)) {
      shortPathBuilder.append("NA")
    } else {
      auditCache
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
