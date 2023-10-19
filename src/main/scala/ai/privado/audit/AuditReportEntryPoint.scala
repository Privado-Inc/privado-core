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
    className: String,
    fileName: String,
    filePriorityScore: Double,
    memberName: String,
    memberType: String,
    tagged: Boolean,
    sourceRuleId: String,
    inputToCollection: Boolean,
    collectionEndpointPath: String,
    collectionMethodFullName: String,
    excerpt: String
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
      auditDataList += DataElementDiscoveryAudit(
        eliminateEmptyCellValueIfExist(item(0)),
        eliminateEmptyCellValueIfExist(item(1)),
        if (item(2) == AuditReportConstants.AUDIT_EMPTY_CELL_VALUE) 0.0 else item(2).toDouble,
        eliminateEmptyCellValueIfExist(item(3)),
        eliminateEmptyCellValueIfExist(item(4)),
        if (item(5) == "YES") true else false,
        eliminateEmptyCellValueIfExist(item(6)),
        if (item(5) == "YES") true else false,
        eliminateEmptyCellValueIfExist(item(8)),
        if (item.size >= 10) eliminateEmptyCellValueIfExist(item(9)) else AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        item(10)
      )
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
    createDataElementDiscoveryJson(dataElementDiscoveryData, repoPath)
    createSheet(workbook, AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME, dataElementDiscoveryData)
    // Changed Background colour when tagged
    changeTaggedBackgroundColour(workbook, List(4, 6))

    // Set Dependency Report data into Sheet
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_DEPENDENCY_SHEET_NAME,
      DependencyReport.processDependencyAudit(dependencies)
    )

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
