package ai.privado.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.model.exporter.ViolationProcessingModel
import ai.privado.semantic.Language.*
import ai.privado.threatEngine.ThreatUtility.{getPIINameFromSourceId, hasDataElements}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{SqlColumnNode, StoredNode}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object PIIShouldNotBePresentInMultipleTablesWithSQL {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    taggerCache: TaggerCache
  ): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val violatingFlows          = ListBuffer[ViolationProcessingModel]()

    if (hasDataElements(cpg)) {
      val taggedSources = getSources(cpg)
      taggedSources.foreach(groupedSource => {
        println(groupedSource._1)
        val tableNames   = groupedSource._2.collect(col => col.sqlTable.get.name)
        val piiName   = getPIINameFromSourceId(groupedSource._1)
        val violationDescription = s"${piiName} was found in the following tables:\n"

        if (tableNames.size > 1) {
          val additionalDetail = createDetailBlock(violationDescription, tableNames)

          ThreatUtility.convertToViolationProcessingModelAndAddToViolatingFlows(
            None,
            groupedSource._2.head,
            violatingFlows,
            piiName,
            Some(additionalDetail)
          )
        }
      })
      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (violatingFlows.nonEmpty, violatingFlows.toList)
  }

  def createDetailBlock(initialString: String, array: List[String]): String = {
    val result = new StringBuilder
    result.append(initialString)
    for (string <- array) {
      result.append("\t - ").append(string).append("\n")
    }
    result.toString()
  }

  def getSources(cpg: Cpg): List[(String, List[SqlColumnNode])] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(Constants.sources)
    }
    def groupBy(node: SqlColumnNode): String = {
      // Using randomUUID in case value is not attached it should be grouped under different
      node.tag.nameExact(Constants.id).value.headOption.getOrElse(UUID.randomUUID().toString)
    }
    cpg.sqlColumn
      .where(filterSources)
      .groupBy(groupBy)
      .l
  }
}
