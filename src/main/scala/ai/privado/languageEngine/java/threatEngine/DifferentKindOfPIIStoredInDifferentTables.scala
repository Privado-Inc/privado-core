package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{getPIINameFromSourceId, hasDataElements}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

object DifferentKindOfPIIStoredInDifferentTables {

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
      val violatingFlows                           = ListBuffer[ViolationProcessingModel]()
      val entityMapper                             = EntityMapper.getClassTableMapping(cpg)
      val sourceIdCategoryMap: Map[String, String] = getSourceIdCategoryMap(threat.config)

      entityMapper.foreach((entityWithTable) => {
        val tableName               = entityWithTable._1
        val typeDeclFullName        = entityWithTable._2
        val piiCategoryListForTable = ListBuffer[String]()

        if (taggerCache.typeDeclMemberCache.contains(typeDeclFullName.fullName)) {
          val typeDeclWithMembersMap = taggerCache.getTypeDeclMemberCacheItem(typeDeclFullName.fullName)
          typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
            val sourceId = dataElementMemberMap._1
            if (sourceIdCategoryMap.contains(sourceId)) {
              piiCategoryListForTable.append(sourceIdCategoryMap(sourceId))
            }
          })

          if (piiCategoryListForTable.toSet.size > 1) {
            val categoryWithSourceNames = mutable.Map[String, ListBuffer[String]]()
            var additionalDetail        = s"The below data categories where stored in the table: ${tableName}\n"
            var cacheSourceId: String   = ""
            typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
              val sourceId = dataElementMemberMap._1
              val piiName  = getPIINameFromSourceId(sourceId)
              val category =
                if (sourceIdCategoryMap.contains(sourceId)) sourceIdCategoryMap(sourceId) else ""

              if (categoryWithSourceNames.contains(category)) {
                categoryWithSourceNames.get(category).get.append(piiName)
              } else {
                categoryWithSourceNames.addOne(category -> ListBuffer(piiName))
              }

              if (cacheSourceId.isEmpty) {
                cacheSourceId = sourceId
              }
            })

            categoryWithSourceNames.toList.foreach((categoryWithSourceName) => {
              val piiNameList = categoryWithSourceName._2
              val category    = categoryWithSourceName._1
              if (category.nonEmpty) {
                additionalDetail += s"\t- ${category}\n"
                piiNameList.foreach((piiName) => {
                  additionalDetail += s"\t\t- ${piiName}\n"
                })
              }
            })

            ThreatUtility.convertToViolationProcessingModelAndAddToViolatingFlows(
              Some(cacheSourceId),
              typeDeclFullName,
              violatingFlows,
              tableName,
              Some(additionalDetail)
            )
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

  private def getSourceIdCategoryMap(config: Map[String, String]): Map[String, String] = {
    val sourceIdCategoryMap: ArrayBuffer[(String, String)] = ArrayBuffer()

    config.foreach { case (category, sources) =>
      val sourceIds = sources.split(",").map(_.trim())
      sourceIdCategoryMap.addAll(sourceIds.map(sourceId => (sourceId, category)))
    }

    sourceIdCategoryMap.toMap
  }
}
