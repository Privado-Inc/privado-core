package ai.privado.threatEngine

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import ai.privado.threatEngine.ThreatUtility._
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, MetaData, XML}

object SensitiveDataBackup {

  private val KEY              = "application"
  private val ALLOW_BACKUP_KEY = "android:allowBackup"
  private val logger           = LoggerFactory.getLogger(getClass)

  /** Check for violations for sensitive data backup threat
    * @param androidManifestFile
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg, androidManifestFile: String): Try[(Boolean, List[Json])] = Try {
    val occurrenceList   = ListBuffer[mutable.LinkedHashMap[String, Json]]()
    val xml: Elem        = XML.loadFile(androidManifestFile)
    val applicationNodes = xml \\ KEY

    if (hasDataElements(cpg) && applicationNodes.nonEmpty) {
      // we expect only one node, but a NodeSeq is returned
      applicationNodes.foreach {
        case Elem(prefix, label, attributes, scope, child @ _*) =>
          val backupAttribute = getAttribute(attributes, ALLOW_BACKUP_KEY) match {
            case Some(x) => x
            case _       => ""
          }
          if (backupAttribute == "true") {
            val lineNumber =
              getLineNumberOfMatchingEditText(androidManifestFile, ALLOW_BACKUP_KEY + "=\"" + backupAttribute + "\"")
            val occurrenceOutput = mutable.LinkedHashMap[String, Json]()
            occurrenceOutput.addOne(Constants.sample       -> s"${ALLOW_BACKUP_KEY}=\"${backupAttribute}\"".asJson)
            occurrenceOutput.addOne(Constants.lineNumber   -> lineNumber.asJson)
            occurrenceOutput.addOne(Constants.columnNumber -> (-1).asJson)
            occurrenceOutput.addOne(Constants.fileName     -> androidManifestFile.asJson)
            occurrenceOutput.addOne(Constants.excerpt -> Utilities.dump(androidManifestFile, Some(lineNumber)).asJson)
            occurrenceList.append(occurrenceOutput)
          }
        case _ => // Node not found
      }
    }

    val sanitizedOccurrenceList = transformOccurrenceList(occurrenceList)
    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }
}
