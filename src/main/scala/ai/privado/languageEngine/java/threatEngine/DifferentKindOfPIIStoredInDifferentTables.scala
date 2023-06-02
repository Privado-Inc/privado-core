package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.{TaggerCache}
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.passes.read.EntityMapper
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{hasDataElements}
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Try

object DifferentKindOfPIIStoredInDifferentTables {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(cpg: Cpg, taggerCache: TaggerCache): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val violatingFlows = ListBuffer[ViolationProcessingModel]()
      val entityMapper   = EntityMapper.getClassTableMapping(cpg)
      val sourceIdCategoryMap: HashMap[String, String] = HashMap(
        "Data.Sensitive.PersonalIdentification.FirstName"                           -> "PersonalCharacteristics",
        "Data.Sensitive.PersonalIdentification.LastName"                            -> "PersonalCharacteristics",
        "Data.Sensitive.PersonalIdentification.DateofBirth"                         -> "PersonalCharacteristics",
        "Data.Sensitive.PersonalCharacteristics.Height"                             -> "PersonalCharacteristics",
        "Data.Sensitive.PersonalCharacteristics.Weigth"                             -> "PersonalCharacteristics",
        "Data.Sensitive.ContactData.PhoneNumber"                                    -> "PersonalCharacteristics",
        "Data.Sensitive.ContactData.Address"                                        -> "PersonalCharacteristics",
        "Data.Sensitive.NationalIdentificationNumbers.SocialSecurityNumber"         -> "NationalIdentity",
        "Data.Sensitive.NationalIdentificationNumbers.TaxpayerIdentificationNumber" -> "NationalIdentity"
      )

      entityMapper.foreach((entityWithTable) => {
        val tableName               = entityWithTable._1
        val typeDeclFullName        = entityWithTable._2
        val piiCategoryListForTable = ListBuffer[String]()

        if (taggerCache.typeDeclMemberCache.contains(typeDeclFullName.fullName)) {
          val typeDeclWithMembersMap = taggerCache.typeDeclMemberCache.get(typeDeclFullName.fullName).get
          typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
            val sourceId = dataElementMemberMap._1
            if (sourceIdCategoryMap.contains(sourceId)) {
              piiCategoryListForTable.append(sourceIdCategoryMap(sourceId))
            }
          })

          if (piiCategoryListForTable.toSet.size > 1) {
            var excerpt =
              s"Found below table with PIIs having different categories.\n" + s"TableName: ${tableName}\n"
            var cacheSourceId: String = ""
            typeDeclWithMembersMap.foreach((dataElementMemberMap) => {
              val sourceId = dataElementMemberMap._1
              val retentionPeriod =
                if (sourceIdCategoryMap.contains(sourceId)) sourceIdCategoryMap(sourceId)
              excerpt += s"PII: ${sourceId} -> Category: ${retentionPeriod}\n"
              if (cacheSourceId.isEmpty) {
                cacheSourceId = sourceId
              }
            })

            val occurrence = ExporterUtility.convertIndividualPathElement(typeDeclFullName).get
            val newOccurrence = DataFlowSubCategoryPathExcerptModel(
              cacheSourceId,
              occurrence.lineNumber,
              occurrence.columnNumber,
              occurrence.fileName,
              s"${excerpt}\n${occurrence.excerpt}"
            )
            violatingFlows.append(ViolationProcessingModel(cacheSourceId, newOccurrence))
          }
        }
      })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }
}
