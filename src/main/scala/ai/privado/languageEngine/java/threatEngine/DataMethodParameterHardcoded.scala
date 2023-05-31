package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{getOccurrenceObjectWithCustomExcerpt, hasDataElements}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.ViolationProcessingModel
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
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
  def getViolations(cpg: Cpg): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val violatingFlows      = ListBuffer[ViolationProcessingModel]()
      val hardCodedParameters = cpg.parameter.code(".*=.*").filter(i => i.code != i.name).l

      hardCodedParameters.foreach((i) => {
        val relatedMethod = i.astParent.head
        if (relatedMethod.nonEmpty) {
          violatingFlows.append(
            ViolationProcessingModel(i.name, ExporterUtility.convertIndividualPathElement(relatedMethod).get)
          )
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }
}
