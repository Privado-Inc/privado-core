package ai.privado.threatEngine

import ai.privado.cache.{AppCache, RuleCache, TaggerCache}
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.model.exporter.ViolationProcessingModel
import ai.privado.semantic.language.*
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

  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    taggerCache: TaggerCache,
    appCache: AppCache,
    ruleCache: RuleCache
  ): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val violatingFlows = ListBuffer[ViolationProcessingModel]()

    if (hasDataElements(cpg)) {
      val taggedSources = getSources(cpg)
      taggedSources.foreach(groupedSource => {
        val tableNames           = groupedSource._2.collect(col => col.sqlTable.map(_.name).getOrElse("TableName"))
        val piiName              = getPIINameFromSourceId(groupedSource._1)
        val violationDescription = s"${piiName} was found in the following tables:\n"

        if (isUniqueElements(tableNames)) {
          val detailedDescription = createDetailBlock(violationDescription, tableNames)

          ThreatUtility.convertToViolationProcessingModelAndAddToViolatingFlows(
            None,
            groupedSource._2.head,
            violatingFlows,
            piiName,
            Some(detailedDescription),
            appCache = appCache,
            ruleCache = ruleCache
          )
        }
      })
    }
    (violatingFlows.nonEmpty, violatingFlows.toList)
  }

  def isUniqueElements(tableNames: List[String]): Boolean = {
    val lowercasedList = tableNames.map(_.toLowerCase)
    val distinctList   = lowercasedList.distinct
    distinctList.length > 1
  }

  def createDetailBlock(initialString: String, tableNames: List[String]): String = {
    val result = new StringBuilder
    result.append(initialString)
    for (tableName <- tableNames) {
      result.append("\t - ").append(tableName).append("\n")
    }
    result.toString()
  }

  def getSources(cpg: Cpg): List[(String, List[SqlColumnNode])] = {
    def filterSources(traversal: Iterator[StoredNode]) = {
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
