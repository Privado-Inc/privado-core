package ai.privado.exporter

import ai.privado.model.Constants.outputDirectoryName
import better.files.File
import org.apache.poi.ss.usermodel.{Cell, Row, Sheet, Workbook}
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.slf4j.LoggerFactory

import java.io.FileOutputStream

object ExcelExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def auditExport(outputFileName: String, output: List[List[String]], repoPath: String): Either[String, Unit] = {

    logger.info("Initiated the Audit exporter engine")
    try {
      val workbook: Workbook = new XSSFWorkbook()
      val sheet: Sheet       = workbook.createSheet("Member-Tag")

      // Iterate over the data and write it to the sheet
      for ((rowValues, rowIndex) <- output.zipWithIndex) {
        val row: Row = sheet.createRow(rowIndex)
        rowValues.zipWithIndex.foreach { case (cellValue, cellIndex) =>
          val cell: Cell = row.createCell(cellIndex)
          cell.setCellValue(cellValue)
        }
      }
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
}
