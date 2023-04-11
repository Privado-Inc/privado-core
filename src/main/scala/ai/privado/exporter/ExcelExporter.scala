package ai.privado.exporter

import ai.privado.audit.{AuditReportConstants, DataFlowReport}
import ai.privado.model.Constants.outputDirectoryName
import better.files.File
import org.apache.poi.ss.usermodel.{Cell, FillPatternType, IndexedColors, Row, Sheet, Workbook}
import org.apache.poi.xssf.usermodel.{XSSFCellStyle, XSSFColor, XSSFWorkbook}
import org.slf4j.LoggerFactory

import java.io.FileOutputStream

object ExcelExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def auditExport(outputFileName: String, output: List[List[String]], repoPath: String): Either[String, Unit] = {

    logger.info("Initiated the Audit exporter engine")
    try {
      val workbook: Workbook = new XSSFWorkbook()
      val sheet: Sheet       = workbook.createSheet(AuditReportConstants.AUDIT_ELEMENT_DISCOVERY_SHEET_NAME)

      // Iterate over the data and write it to the sheet
      for ((rowValues, rowIndex) <- output.zipWithIndex) {
        val row: Row = sheet.createRow(rowIndex)
        rowValues.zipWithIndex.foreach { case (cellValue, cellIndex) =>
          val cell: Cell = row.createCell(cellIndex)
          cell.setCellValue(cellValue)
        }
      }

      val dd = workbook.createSheet("Data Flow Report")

      for ((rowValues, rowIndex) <- DataFlowReport.processDataFlowAudit().zipWithIndex) {
        val row: Row = dd.createRow(rowIndex)
        rowValues.zipWithIndex.foreach { case (cellValue, cellIndex) =>
          val cell: Cell = row.createCell(cellIndex)
          cell.setCellValue(cellValue)
        }
      }

      // Changed Background colour when tagged
      changeTaggedBackgroundColour(workbook, List(4, 6))
      logger.info("Successfully added audit report to excel file")

      // create directory if not exist
      val outputDir                 = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val fileOut: FileOutputStream = new FileOutputStream(s"$repoPath/$outputDirectoryName/$outputFileName")
      workbook.write(fileOut)
      fileOut.close()

      // Close the workbook
      workbook.close()
      logger.info("Shutting down Audit Exporter engine")
      Right(())
    } catch {
      case ex: Exception =>
        println("Failed to export Audit Report")
        logger.debug("Failed to export Audit Report", ex)
        Left(ex.toString)
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
