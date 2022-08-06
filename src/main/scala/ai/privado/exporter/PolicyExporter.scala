package ai.privado.exporter

import ai.privado.cache.AppCache
import ai.privado.model.{Constants, PolicyViolationFlowModel}
import ai.privado.policyEngine.PolicyExecutor
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

class PolicyExporter(dataflows: Map[String, Path]) {

  val policyExecutor = new PolicyExecutor(dataflows, AppCache.repoName)

  def getViolations: immutable.Iterable[mutable.LinkedHashMap[String, Json]] = {

    policyExecutor.getProcessingViolations
      .filter(entrySet => entrySet._2.nonEmpty)
      .map(policyViolationEntrySet =>
        convertProcessingPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
      ) ++ policyExecutor.getDataflowViolations
      .filter(entrySet => entrySet._2.nonEmpty)
      .map(policyViolationEntrySet =>
        convertDataflowPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
      )
  }

  private def convertProcessingPolicyViolation(policyId: String, sourceIds: Set[String]) = {
    val output = mutable.LinkedHashMap[String, Json]()
    output.addOne(Constants.policyId      -> policyId.asJson)
    output.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(policyId).asJson)
    output.addOne(Constants.processing    -> sourceIds.map(sourceId => convertProcessingSources(sourceId)).asJson)
    output
  }

  private def convertProcessingSources(sourceId: String) = {
    val sourceOutput = mutable.LinkedHashMap[String, String]()
    sourceOutput.addOne(Constants.sourceId -> sourceId)
    sourceOutput
  }

  private def convertViolatingFlow(flow: PolicyViolationFlowModel) = {
    val flowOutput = mutable.LinkedHashMap[String, Json]()
    flowOutput.addOne(Constants.sourceId -> flow.sourceId.asJson)
    flowOutput.addOne(Constants.sinkId   -> flow.sinkId.asJson)
    flowOutput.addOne(Constants.pathIds  -> flow.pathIds.asJson)
    flowOutput
  }

  private def convertDataflowPolicyViolation(policyId: String, violatingFlows: ListBuffer[PolicyViolationFlowModel]) = {
    val output = mutable.LinkedHashMap[String, Json]()
    output.addOne(Constants.policyId      -> policyId.asJson)
    output.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(policyId).asJson)
    output.addOne(Constants.dataFlow      -> violatingFlows.map(flow => convertViolatingFlow(flow)).asJson)
    output

  }

}
