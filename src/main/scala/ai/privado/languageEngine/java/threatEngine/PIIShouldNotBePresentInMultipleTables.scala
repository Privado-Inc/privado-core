package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{getSourceNode, hasDataElements}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object PIIShouldNotBePresentInMultipleTables {

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
    if (hasDataElements(cpg)) {
      val violatingFlows        = ListBuffer[ViolationProcessingModel]()
      val entityMapper          = EntityMapper.getClassTableMapping(cpg)
      val dataElementToTableMap = mutable.HashMap[String, ListBuffer[String]]()

      entityMapper.foreach((entityWithTable) => {
        val tableName        = entityWithTable._1
        val typeDeclFullName = entityWithTable._2

        if (taggerCache.typeDeclMemberCache.contains(typeDeclFullName.fullName)) {
          val typeDeclWithMembersMap = taggerCache.typeDeclMemberCache.get(typeDeclFullName.fullName).get
          typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
            val sourceId = dataElementMemberMap._1
            if (!dataElementToTableMap.contains(sourceId)) {
              dataElementToTableMap.addOne(sourceId, ListBuffer[String]())
            }
            dataElementToTableMap.addOne(sourceId, dataElementToTableMap(sourceId).append(tableName))
          })
        }
      })

      dataElementToTableMap.foreach(piiWithTables => {
        val piiId     = piiWithTables._1
        val tableList = piiWithTables._2

        if (tableList.size > 1) {
          val relatedNode = getSourceNode(cpg, piiId).head
          if (relatedNode._2.nonEmpty) {
            val occurrence = ExporterUtility.convertIndividualPathElement(relatedNode._2).get
            val newOccurrenceExcerpt =
              s"Violating PII: ${piiId} \nTable Names: ${tableList.toList.toString()} \n\n" + occurrence.excerpt
            val newOccurrence = DataFlowSubCategoryPathExcerptModel(
              occurrence.sample,
              occurrence.lineNumber - 3,
              occurrence.columnNumber,
              occurrence.fileName,
              newOccurrenceExcerpt
            )
            violatingFlows.append(ViolationProcessingModel(piiId, newOccurrence))
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

}
