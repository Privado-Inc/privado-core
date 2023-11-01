package ai.privado.threatEngine

import ai.privado.exporter.ExporterUtility
import ThreatUtility.hasDataElements
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.model.exporter.ViolationProcessingModel
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try

object ObjectsWithPIIsPassedAsParameter {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(cpg: Cpg): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val violatingFlows = ListBuffer[ViolationProcessingModel]()
      val parameters =
        cpg.parameter.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.DERIVED_SOURCES.name)).l

      parameters.distinctBy(_.name) foreach ((parameter) => {
        violatingFlows.append(
          ViolationProcessingModel(parameter.name, ExporterUtility.convertIndividualPathElement(parameter), None)
        )
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }
}
