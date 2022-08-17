package ai.privado.threatEngine

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import ai.privado.threatEngine.ThreatUtility._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}
import scala.xml.{Elem, MetaData, XML}

object BackgroundScreenshot {

  private val SET_FLAG_METHOD_PATTERN = ".*(add|set)Flags.*"
  private val SAFE_FLAG               = "WindowManager.LayoutParams.FLAG_SECURE"
  private val logger                  = LoggerFactory.getLogger(getClass)

  /** Check for violation for auto-generated background screenshot threat
    * @param cpg
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg): Try[(Boolean, List[Json])] = Try {
    // implicit for callIn
    implicit val resolver: ICallResolver = NoResolve
    if (hasDataElements(cpg)) {
      val safeFlagCalls = cpg.method
        .fullName(SET_FLAG_METHOD_PATTERN)
        .callIn
        .where(_.argument.code(s".*${SAFE_FLAG}.*"))

      // violation if empty
      (safeFlagCalls.isEmpty, List())
    } else (false, List())
  }
}
