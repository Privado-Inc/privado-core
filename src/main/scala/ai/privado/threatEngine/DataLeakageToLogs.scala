package ai.privado.threatEngine

import io.circe.Json
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.{Constants, PolicyOrThreat, PolicyViolationFlowModel}
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory

import scala.util.Try

object DataLeakageToLogs {

  private val SET_FLAG_METHOD_PATTERN = ".*(add|set)Flags.*"
  private val SAFE_FLAG               = "WindowManager.LayoutParams.FLAG_SECURE"
  private val logger                  = LoggerFactory.getLogger(getClass)

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
    dataflows: Map[String, Path]
  ): Try[(Boolean, List[PolicyViolationFlowModel])] = Try {

    // violation if empty
    (false, List())
  }
}
