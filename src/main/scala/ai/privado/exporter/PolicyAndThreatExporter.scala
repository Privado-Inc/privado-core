/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

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

  private val logger = LoggerFactory.getLogger(getClass)

  def getViolations(repoPath: String): immutable.Iterable[mutable.LinkedHashMap[String, Json]] = {
    val policyExecutor = new PolicyExecutor(cpg, dataflows, AppCache.repoName)
    val threatExecutor = new ThreatEngineExecutor(cpg, dataflows, repoPath)

    try {
      threatExecutor.getProcessingViolations(RuleCache.getAllThreat) ++ policyExecutor.getProcessingViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertProcessingPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        ) ++ threatExecutor.getDataflowViolations(RuleCache.getAllThreat) ++ policyExecutor.getDataflowViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertDataflowPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        )
    } catch {
      case e: Exception =>
        logger.debug("Exception : ", e)
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
