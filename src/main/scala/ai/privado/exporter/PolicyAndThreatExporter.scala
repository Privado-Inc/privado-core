/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.exporter

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.threatEngine.ThreatEngineExecutor
import ai.privado.model.exporter.{ViolationDataFlowModel, ViolationModel, ViolationProcessingModel}
import ai.privado.policyEngine.PolicyExecutor
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import org.slf4j.LoggerFactory

import scala.collection.mutable

class PolicyAndThreatExporter(
  cpg: Cpg,
  ruleCache: RuleCache,
  dataflows: Map[String, Path],
  taggerCache: TaggerCache,
  dataFlowCache: DataFlowCache,
  privadoInput: PrivadoInput
) {

  private val logger = LoggerFactory.getLogger(getClass)

  def getViolations(repoPath: String): List[ViolationModel] = {
    val policyExecutor = new PolicyExecutor(cpg, dataFlowCache, AppCache.repoName, ruleCache, privadoInput)
    val threatExecutor =
      new ThreatEngineExecutor(cpg, dataflows, repoPath, ruleCache, taggerCache, dataFlowCache, privadoInput)

    try {
      threatExecutor.getProcessingViolations(ruleCache.getAllThreat) ++ policyExecutor.getProcessingViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertProcessingPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        ) ++ threatExecutor.getDataflowViolations(ruleCache.getAllThreat) ++ policyExecutor.getDataflowViolations
        .filter(entrySet => entrySet._2.nonEmpty)
        .map(policyViolationEntrySet =>
          convertDataflowPolicyViolation(policyViolationEntrySet._1, policyViolationEntrySet._2)
        )
    } catch {
      case e: Exception =>
        logger.debug("Exception : ", e)
        List[ViolationModel]()
    }
  }

  def convertProcessingPolicyViolation(policyId: String, sourceNodes: List[(String, CfgNode)]): ViolationModel = {
    ViolationModel(
      policyId,
      ExporterUtility.getPolicyInfoForExporting(ruleCache, policyId),
      None,
      Some(sourceNodes.map(sourceNode => convertProcessingSources(sourceNode)))
    )
  }

  private def convertProcessingSources(sourceNode: (String, CfgNode)) = {
    try {
      ViolationProcessingModel(sourceNode._1, ExporterUtility.convertIndividualPathElement(sourceNode._2).get, None)
    } catch {
      case e: Exception =>
        logger.debug("Exception : ", e)
        ViolationProcessingModel(sourceNode._1, null, None)
    }
  }

  private def convertDataflowPolicyViolation(
    policyId: String,
    violatingFlows: mutable.HashSet[ViolationDataFlowModel]
  ) = {
    ViolationModel(
      policyId,
      ExporterUtility.getPolicyInfoForExporting(ruleCache, policyId),
      Some(violatingFlows.toList),
      None
    )
  }

}
