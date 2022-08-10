package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.{Constants, PolicyViolationFlowModel}
import ai.privado.policyEngine.PolicyExecutor
import ai.privado.threatEngine.ThreatEngineExecutor
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import org.slf4j.LoggerFactory

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

class PolicyAndThreatExporter(cpg: Cpg, dataflows: Map[String, Path]) {

  val logger = LoggerFactory.getLogger(getClass)

  val policyExecutor = new PolicyExecutor(cpg, dataflows, AppCache.repoName)
  val threatExecutor = new ThreatEngineExecutor()

  def getViolations(repoPath: String): immutable.Iterable[mutable.LinkedHashMap[String, Json]] = {
    try {

      threatExecutor.execute(RuleCache.getAllThreat, repoPath) ++ policyExecutor.getProcessingViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertProcessingPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        ) ++ policyExecutor.getDataflowViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertDataflowPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        )
    } catch {
      case e: Exception =>
        logger.error("Exception : ", e)
        immutable.Iterable[mutable.LinkedHashMap[String, Json]]()
    }
  }

  def convertProcessingPolicyViolation(policyId: String, sourceNodes: List[(String, CfgNode)]) = {
    val output = mutable.LinkedHashMap[String, Json]()
    output.addOne(Constants.policyId      -> policyId.asJson)
    output.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(policyId).asJson)
    output.addOne(Constants.processing    -> sourceNodes.map(sourceNode => convertProcessingSources(sourceNode)).asJson)
    output
  }

  private def convertProcessingSources(sourceNode: (String, CfgNode)) = {
    val sourceOutput = mutable.LinkedHashMap[String, Json]()
    sourceOutput.addOne(Constants.sourceId -> sourceNode._1.asJson)
    try {
      sourceOutput.addOne(Constants.occurrence -> ExporterUtility.convertIndividualPathElement(sourceNode._2).asJson)
    } catch {
      case e: Exception => logger.debug("Exception : ", e)
    }

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
