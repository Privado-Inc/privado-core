package ai.privado.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import org.apache.poi.ss.usermodel.{Cell, FillPatternType, IndexedColors, Row, Workbook}
import org.apache.poi.xssf.usermodel.{XSSFCellStyle, XSSFColor, XSSFWorkbook}

import scala.util.Try

object AuditReportEntryPoint {

  // Audit report generation for java
  def getAuditWorkbook(xtocpg: Try[Cpg], taggerCache: TaggerCache, dependencies: Set[ModuleDependency]): Workbook = {
    val workbook: Workbook = new XSSFWorkbook()
    // Set Element Discovery Data into Sheet
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME,
      DataElementDiscovery.processDataElementDiscovery(xtocpg, taggerCache)
    )
    // Changed Background colour when tagged
    changeTaggedBackgroundColour(workbook, List(4, 6))

    // Set Dependency Report data into Sheet
    createSheet(
      workbook,
      AuditReportConstants.AUDIT_DEPENDENCY_SHEET_NAME,
      DependencyReport.processDependencyAudit(dependencies)
    )

    // Set Data Flow report into Sheet
    createSheet(workbook, AuditReportConstants.AUDIT_DATA_FLOW_SHEET_NAME, DataFlowReport.processDataFlowAudit())

    // Set Unresolved flow into Sheet
    createSheet(workbook, AuditReportConstants.AUDIT_UNRESOLVED_SHEET_NAME, UnresolvedFlowReport.processUnresolvedFlow())

    workbook
  }

  // Audit report generation for Python and javaScript
  def getAuditWorkbook(): Workbook = {
    val workbook: Workbook = new XSSFWorkbook()

    // Set Data Flow report into Sheet
    createSheet(workbook, AuditReportConstants.AUDIT_DATA_FLOW_SHEET_NAME, DataFlowReport.processDataFlowAudit())

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
