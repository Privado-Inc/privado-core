package ai.privado.threatEngine

import ai.privado.exporter.ExporterUtility
import ThreatUtility.hasDataElements
import ai.privado.cache.AppCache
import ai.privado.model.exporter.ViolationProcessingModel
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try

object DataMethodParameterHardcoded {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(cpg: Cpg, appCache: AppCache): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val violatingFlows      = ListBuffer[ViolationProcessingModel]()
      val hardCodedParameters = cpg.parameter.code(".*=.*").filter(parameter => parameter.code != parameter.name).l

      hardCodedParameters.distinctBy(_.name) foreach ((parameter) => {
        val relatedMethod = parameter.method
        if (relatedMethod.nonEmpty) {
          violatingFlows.append(
            ViolationProcessingModel(
              parameter.name,
              ExporterUtility.convertIndividualPathElement(relatedMethod, appCache = appCache),
              None
            )
          )
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }
}
