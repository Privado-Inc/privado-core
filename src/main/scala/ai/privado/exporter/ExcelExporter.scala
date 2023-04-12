package ai.privado.exporter

import ai.privado.audit.AuditReportConstants
import ai.privado.model.Constants.outputDirectoryName
import better.files.File
import org.apache.poi.ss.usermodel.{Cell, FillPatternType, IndexedColors, Row, Sheet, Workbook}
import org.apache.poi.xssf.usermodel.{XSSFCellStyle, XSSFColor, XSSFWorkbook}
import org.slf4j.LoggerFactory

import java.io.FileOutputStream

object ExcelExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def auditExport(outputFileName: String, workbook: Workbook, repoPath: String): Either[String, Unit] = {
    try {
      logger.info("Initiated the Audit exporter engine")

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
        println(ex.printStackTrace())
        Left(ex.toString)
    }
  }
}
