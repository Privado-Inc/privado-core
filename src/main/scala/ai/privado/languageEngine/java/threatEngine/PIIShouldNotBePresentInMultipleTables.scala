package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{
  getPIINameFromSourceId,
  getSourceNode,
  hasDataElements
}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Member
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
      val violatingFlows          = ListBuffer[ViolationProcessingModel]()
      val entityMapper            = EntityMapper.getClassTableMapping(cpg)
      val dataElementToTableMap   = mutable.HashMap[String, ListBuffer[String]]()
      val dataElementToMembersMap = mutable.HashMap[String, ListBuffer[Member]]()

      entityMapper.foreach((entityWithTable) => {
        val tableName        = entityWithTable._1
        val typeDeclFullName = entityWithTable._2

        if (taggerCache.typeDeclMemberCache.contains(typeDeclFullName.fullName)) {
          val typeDeclWithMembersMap = taggerCache.typeDeclMemberCache.get(typeDeclFullName.fullName).get
          typeDeclWithMembersMap.foreach((piiMemberMap) => {
            val sourceId = piiMemberMap._1
            val members  = piiMemberMap._2

            if (!dataElementToTableMap.contains(sourceId)) {
              dataElementToTableMap.addOne(sourceId, ListBuffer[String]())
              dataElementToMembersMap.addOne(sourceId, ListBuffer[Member]())
            }
            dataElementToTableMap.addOne(sourceId, dataElementToTableMap(sourceId).append(tableName))
            dataElementToMembersMap.addOne(sourceId, dataElementToMembersMap(sourceId).addAll(members))
          })
        }
      })

      dataElementToTableMap.foreach(piiWithTables => {
        val piiId     = piiWithTables._1
        val tableList = piiWithTables._2
        val piiName   = getPIINameFromSourceId(piiId)

        if (tableList.size > 1) {
          val relatedNode      = getSourceNode(cpg, piiId).head
          var additionalDetail = s"${piiName} was found in the following tables:\n"
          if (relatedNode._2.nonEmpty) {
            val occurrence           = ExporterUtility.convertIndividualPathElement(relatedNode._2).get
            var newOccurrenceExcerpt = ""

            for ((table, member) <- piiWithTables._2.zip(dataElementToMembersMap(piiId))) {
              val dataFlowSubCategoryPathExcerptModel = ExporterUtility.convertIndividualPathElement(member).get
              newOccurrenceExcerpt = newOccurrenceExcerpt + "Table Name: " + table
              additionalDetail = s"${additionalDetail}\t- ${table}\n"
              newOccurrenceExcerpt =
                s"${newOccurrenceExcerpt}\nFileName: ${dataFlowSubCategoryPathExcerptModel.fileName}"
              newOccurrenceExcerpt =
                s"${newOccurrenceExcerpt}\nOccurrence: ${dataFlowSubCategoryPathExcerptModel.excerpt}\n\n"
            }

            val newOccurrence = DataFlowSubCategoryPathExcerptModel(
              occurrence.sample,
              1, // (occurrence.lineNumber - 3),
              occurrence.columnNumber,
              occurrence.fileName,
              newOccurrenceExcerpt
            )
            violatingFlows.append(ViolationProcessingModel(piiName, newOccurrence, Some(additionalDetail)))
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

}
