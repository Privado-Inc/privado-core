package ai.privado.audit

import ai.privado.cache.{AuditCache, TaggerCache}
import ai.privado.exporter.JSONExporter
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import org.apache.poi.ss.usermodel.*
import org.apache.poi.xssf.usermodel.{XSSFCellStyle, XSSFColor, XSSFWorkbook}
import org.apache.xmlbeans.XmlException
import org.apache.commons.collections4.ListValuedMap

import scala.collection.mutable.ListBuffer
import scala.util.Try

object AuditReportEntryPoint {

  case class DataElementDiscoveryAudit(
    isPII: String,
    excerpt: String,
    sample: String,
    fileName: String,
    dataElementId: String
//    tagged: Boolean,
//    sourceRuleId: String,
//    inputToCollection: Boolean,
//    collectionEndpointPath: String,
//    collectionMethodFullName: String
  )

  implicit val DataElementDiscoveryAuditModelDecoder: Decoder[DataElementDiscoveryAudit] =
    deriveDecoder[DataElementDiscoveryAudit]
  implicit val DataElementDiscoveryAuditEncoder: Encoder[DataElementDiscoveryAudit] =
    deriveEncoder[DataElementDiscoveryAudit]

  def eliminateEmptyCellValueIfExist(str: String): String =
    if (str == AuditReportConstants.AUDIT_EMPTY_CELL_VALUE) "" else str

  def createDataElementDiscoveryJson(dataElementDiscoveryData: List[List[String]], repoPath: String) = {

    val auditDataList = new ListBuffer[DataElementDiscoveryAudit]()

    for (item <- dataElementDiscoveryData.drop(1)) {
      val rItem = item.map(el => el.replace("\"", "\\"))
      println("------")
      println(rItem)
      println("===>>>>")
      println(item)
      auditDataList += DataElementDiscoveryAudit(rItem(0), rItem(1), rItem(2), rItem(3), rItem(4))
    }
    JSONExporter.dataElementDiscoveryAuditFileExport(
      AuditReportConstants.AUDIT_SOURCE_FILE_NAME,
      repoPath,
      auditDataList.toList
    )

  }

  // Audit report generation for java
  def getAuditWorkbook(
    xtocpg: Try[Cpg],
    taggerCache: TaggerCache,
    dependencies: Set[ModuleDependency],
    repoPath: String,
    auditCache: AuditCache
  ): Workbook = {
    val workbook: Workbook = new XSSFWorkbook()
    // Set Element Discovery Data into Sheet
    val dataElementDiscoveryData = DataElementDiscovery.processDataElementDiscovery(xtocpg, taggerCache)
    println(dataElementDiscoveryData)
    createDataElementDiscoveryJson(dataElementDiscoveryData, repoPath)
    println("Created JSON")
//    createSheet(workbook, AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME, dataElementDiscoveryData)
    // Changed Background colour when tagged
//    changeTaggedBackgroundColour(workbook, List(4, 6))

    // Set Dependency Report data into Sheet
//    createSheet(
//      workbook,
//      AuditReportConstants.AUDIT_DEPENDENCY_SHEET_NAME,
//      DependencyReport.processDependencyAudit(dependencies)
//    )

    // Set Data Flow report into Sheet
//    createSheet(
//      workbook,
//      AuditReportConstants.AUDIT_DATA_FLOW_SHEET_NAME,
//      DataFlowReport.processDataFlowAudit(auditCache)
//    )

    // Set Unresolved flow into Sheet
//    createSheet(
//      workbook,
//      AuditReportConstants.AUDIT_UNRESOLVED_SHEET_NAME,
//      UnresolvedFlowReport.processUnresolvedFlow(auditCache)
//    )

    workbook
  }

  def getAuditWorkbookPy(auditCache: AuditCache): Workbook = {
    val workbook: Workbook = new XSSFWorkbook()
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_DATA_FLOW_SHEET_NAME,
      DataFlowReport.processDataFlowAudit(auditCache)
    )
    workbook
  }
  // Audit report generation for Python and javaScript
  def getAuditWorkbookJS(
    xtocpg: Try[Cpg],
    taggerCache: TaggerCache,
    repoPath: String,
    auditCache: AuditCache
  ): Workbook = {
    val workbook: Workbook       = new XSSFWorkbook()
    val dataElementDiscoveryData = DataElementDiscoveryJS.processDataElementDiscovery(xtocpg, taggerCache)

    createDataElementDiscoveryJson(dataElementDiscoveryData, repoPath = repoPath)
    createSheet(workbook, AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME, dataElementDiscoveryData)
    // Changed Background colour when tagged
    changeTaggedBackgroundColour(workbook, List(4, 6))
    // Set Data Flow report into Sheet
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_DATA_FLOW_SHEET_NAME,
      DataFlowReport.processDataFlowAudit(auditCache)
    )
    // Set Unresolved flow into Sheet
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_UNRESOLVED_SHEET_NAME,
      UnresolvedFlowReport.processUnresolvedFlow(auditCache)
    )

    workbook
  }

  private def createSheet(workbook: Workbook, sheetName: String, sheetData: List[List[String]]) = {
    val sheet = workbook.createSheet(sheetName)

    // Iterate over the data and write it to the sheet
    for ((rowValues, rowIndex) <- sheetData.zipWithIndex) {
      val row: Row = sheet.createRow(rowIndex)
      rowValues.zipWithIndex.foreach { case (cellValue, cellIndex) =>
        val cell: Cell = row.createCell(cellIndex)
        cell.setCellValue(cellValue)
      }
    }
  }

  private def changeTaggedBackgroundColour(workbook: Workbook, columnList: List[Integer]) = {

    val sheet = workbook.getSheet(AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME)

    val greenCellStyle: XSSFCellStyle = workbook.createCellStyle().asInstanceOf[XSSFCellStyle]
    val greenColor: XSSFColor         = new XSSFColor(IndexedColors.LIGHT_GREEN, null)
    greenCellStyle.setFillForegroundColor(greenColor)
    greenCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND)

    val rowIterator = sheet.rowIterator()
    while (rowIterator.hasNext) {
      val row = rowIterator.next()
      columnList.foreach(columnNo => {
        val cell = row.getCell(columnNo)
        if (cell != null && cell.getStringCellValue == AuditReportConstants.AUDIT_CHECKED_VALUE) {
          cell.setCellStyle(greenCellStyle)
        }
      })
    }
  }

}
