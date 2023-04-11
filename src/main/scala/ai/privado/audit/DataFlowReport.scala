package ai.privado.audit

import ai.privado.cache.DataFlowCache
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.collection.mutable.ListBuffer
import scala.util.Try

object DataFlowReport {

  def processDataFlowAudit(): List[List[String]] = {
    val workbookResult = ListBuffer[List[String]]()
    val intermediateData = DataFlowCache.getIntermediateDataFlow()

    workbookResult += List(AuditReportConstants.DATA_FLOW_SOURCE_NAME,
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

    intermediateData.foreach(sourceDataFlow => {
      val sourceId = sourceDataFlow.sourceId
      val sinkData = sourceDataFlow.sinks
      sinkData.foreach(sinkDataFlow => {
        val sinkId = sinkDataFlow.id
        val pathData = sinkDataFlow.paths
        pathData.foreach(path => {
          workbookResult += List(sourceId, sinkId, path.pathId, "--", "--", "--", "--", "--", "--", "--", "--")
        })
      })
    })

    workbookResult.toList
  }

}
