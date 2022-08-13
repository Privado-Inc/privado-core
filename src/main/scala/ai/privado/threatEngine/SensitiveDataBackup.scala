package ai.privado.threatEngine

import ai.privado.model.{Constants, RuleInfo}
import ai.privado.utility.Utilities
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, MetaData, XML}

object SensitiveDataBackup {

  private val KEY         = "application"
  private val ALLOW_BACKUP_KEY = "android:allowBackup"
  private val logger     = LoggerFactory.getLogger(getClass)

  /** Fetch all the violations which violate Key-board cache threat
    * @param repoPath path of repo
    * @param androidManifestFile source filepath of manifest file
    * @return
    */
  def getViolations(repoPath: String, androidManifestFile: String): Try[List[Json]] = Try {
    val occurrenceList = ListBuffer[mutable.LinkedHashMap[String, Json]]()
    val xml: Elem = XML.loadFile(androidManifestFile)
    val applicationNodes = xml \\ KEY
    if(applicationNodes.nonEmpty) {
      // we expect only one node, but a NodeSeq is returned
      applicationNodes.foreach {
        case Elem(prefix, label, attributes, scope, child@_*) =>
          if (getBackupAttribute(attributes)) {
            val backupAttribute = attributes.filter(attribute => attribute.prefixedKey == ALLOW_BACKUP_KEY).value.head

            val lineNumber = getLineNumberOfMatchingEditText(androidManifestFile, ALLOW_BACKUP_KEY + "=\"" + backupAttribute.text + "\"")
            val occurrenceOutput = mutable.LinkedHashMap[String, Json]()
            occurrenceOutput.addOne(Constants.sample -> backupAttribute.text.asJson)
            occurrenceOutput.addOne(Constants.lineNumber -> lineNumber.asJson)
            occurrenceOutput.addOne(Constants.columnNumber -> (-1).asJson)
            occurrenceOutput.addOne(Constants.fileName -> androidManifestFile.asJson)
            occurrenceOutput.addOne(Constants.excerpt -> Utilities.dump(androidManifestFile, Some(lineNumber)).asJson)
            occurrenceList.append(occurrenceOutput)
          }
        case _ => // Node not found
      }
    }

    occurrenceList
      .map(occurrence =>
        mutable.Map[String, Json](Constants.sourceId -> "".asJson, Constants.occurrence -> occurrence.asJson).asJson
      )
      .toList
  }

  /** Checks if the field id is sensitive
    * @param attributes the attributes of xml node
    * @return boolean value of backup attribute
    */
  private def getBackupAttribute(attributes: MetaData): Boolean = {
    val backupValue = attributes.filter(attribute => attribute.prefixedKey == ALLOW_BACKUP_KEY).head.value.head.text
    backupValue.toBoolean
  }

  /** Returns matching line number from the file
    * @param fileName name of file
    * @param matchingText match text
    * @return
    */
  private def getLineNumberOfMatchingEditText(fileName: String, matchingText: String) = {
    var matchedLineNumber = -2
    try {
      val lines = File(fileName).lines.toList
      breakable {
        for (lineNumber <- 1 until lines.size) {
          if (lines(lineNumber).contains(matchingText)) {
            matchedLineNumber = lineNumber
            break()
          }
        }
      }
    } catch {
      case e: Exception => logger.debug("Exception", e)
    }
    matchedLineNumber + 1
  }
}
