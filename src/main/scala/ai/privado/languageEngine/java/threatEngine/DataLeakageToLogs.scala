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

package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache}
import ai.privado.model.exporter.ViolationDataFlowModel
import ai.privado.policyEngine.PolicyExecutor
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.PolicyOrThreat
import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory

import scala.util.Try

object DataLeakageToLogs {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    * @param threat
    *   the object of threat rule
    * @param cpg
    *   cpg
    * @param dataflows
    *   generated dataflows for the repo source filepath of manifest file
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    dataflows: Map[String, Path],
    ruleCache: RuleCache,
    dataFlowCache: DataFlowCache
  ): Try[(Boolean, List[ViolationDataFlowModel])] = Try {
    // use policy executor to directly process existing flows
    // we already have this implementation as part of policy enforcement
    // threat being type of suggestive policy
    // might restructure this in future and have central utilities consumed by both
    val policyExecutor = new PolicyExecutor(cpg, dataFlowCache, AppCache.repoName, ruleCache)
    val violatingFlows = policyExecutor.getViolatingFlowsForPolicy(threat)

    // violation if empty
    (violatingFlows.nonEmpty, violatingFlows.toList)
  }
}
