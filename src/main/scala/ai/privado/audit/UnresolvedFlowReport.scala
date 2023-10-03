package ai.privado.audit

import ai.privado.cache.AuditCache
import ai.privado.model.exporter.DataFlowSubCategoryPathExcerptModel
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.ListBuffer

object UnresolvedFlowReport {
  val workbookResult: ListBuffer[List[String]] = ListBuffer[List[String]]()
  def processUnresolvedFlow(auditCache: AuditCache): List[List[String]] = {
    val expendedSourceSinkInfo = auditCache.getUnfilteredFlow()

    expendedSourceSinkInfo.foreach(sourceSinkInfo => {
      workbookResult += List(
        sourceSinkInfo.sourceId,
        sourceSinkInfo.sinkId,
        sourceSinkInfo.pathId,
        getCodePath(sourceSinkInfo.paths)
      )
    })

    List(
      List(
        AuditReportConstants.DEPENDENCY_UNRESOLVED_SOURCE_NAME,
        AuditReportConstants.DEPENDENCY_UNRESOLVED_SINK_NAME,
        AuditReportConstants.DEPENDENCY_UNRESOLVED_FLOW_ID_NAME,
        AuditReportConstants.DEPENDENCY_UNRESOLVED_CODE_SNIPPET_NAME
      )
    ) ++ workbookResult.groupBy(_.head).values.flatMap(identity).toList
  }

  def getUnresolvedSink(cpg: Cpg): List[CfgNode] = {
    cpg.call.where(_.methodFullName(".*unresolvedNamespace.*")).l
  }

  private def getCodePath(paths: List[Option[DataFlowSubCategoryPathExcerptModel]]): String = {
    val pathCodeList = ListBuffer[String]()
    paths.foreach(path => {
      if (path.isDefined) pathCodeList.append(path.get.sample)
    })
    pathCodeList.mkString(" -> ")
  }
}
