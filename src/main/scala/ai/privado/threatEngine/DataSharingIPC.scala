package ai.privado.threatEngine

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}
import scala.xml.{Elem, MetaData, XML}

object DataSharingIPC {

  private val PROVIDER_KEY              = "provider"
  private val PROVIDER_EXPORT_ATTRIBUTE = "exported"
  private val PROVIDER_PERM_ATTRIBUTE   = "permission"
  private val PROVIDER_PERM_R_ATTRIBUTE = "readPermission"
  private val PROVIDER_PERM_W_ATTRIBUTE = "writePermission"

  private val INTENT_FILTER_KEY    = "intent-filter"
  private val SDK_KEY              = "uses-sdk"
  private val TARGET_SDK_ATTRIBUTE = "targetSdkVersion"

  private val PERM_KEY                         = "permission"
  private val PROTECTION_LEVEL_ATTRIBUTE       = "protectionLevel"
  private val PROTECTION_LEVEL_ATTRIBUTE_VALUE = "signature"

  private val logger = LoggerFactory.getLogger(getClass)

  /** Fetch all the violations which violate Key-board cache threat
    * @param androidManifestFile
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg, androidManifestFile: String): Try[(Boolean, List[Json])] = Try {
    val occurrenceList = ListBuffer[mutable.LinkedHashMap[String, Json]]()
    val xml: Elem      = XML.loadFile(androidManifestFile)

    val providerNodes = xml \\ PROVIDER_KEY

    if (hasDataElements(cpg) && providerNodes.nonEmpty) {
      providerNodes.foreach {
//        Elem(prefix, label, attributes, scope, child @ _*)
        case provider: Elem =>
          var isExport = getAttribute(provider.attributes, PROVIDER_EXPORT_ATTRIBUTE) match {
            case Some(x) if x == "true"  => true
            case Some(x) if x == "false" => false
            case _                       =>
              // non-existent, use defaults
              // default true unless SDK target defined >=17
              val targetSDK = getDefinedTargetSDKValue(xml)
              if (targetSDK.nonEmpty && targetSDK.toInt >= 17) false else true
          }

          // if isExport is false, check for intent-filter
          if (!isExport) {
            val intentFilter = provider \\ INTENT_FILTER_KEY
            // if intent-filter is defined, export is true, even when defined "false"
            isExport = if (intentFilter.nonEmpty) true else false
          }

//          val providerExport = getAttribute(attributes, PROVIDER_EXPORT_ATTRIBUTE)
//          if (providerExport == "") {
//
//          }
//          println("GOT ALUE", providerExport)
//          val processPermissions = if (providerExport == "true") true else false

//          if (getBackupAttribute(attributes)) {
//            val lineNumber = getLineNumberOfMatchingEditText(
//              androidManifestFile,
//              ALLOW_BACKUP_KEY + "=\"" + backupAttribute.text + "\""
//            )
//            val occurrenceOutput = mutable.LinkedHashMap[String, Json]()
//            occurrenceOutput.addOne(Constants.sample       -> s"${ALLOW_BACKUP_KEY}=\"${backupAttribute.text}\"".asJson)
//            occurrenceOutput.addOne(Constants.lineNumber   -> lineNumber.asJson)
//            occurrenceOutput.addOne(Constants.columnNumber -> (-1).asJson)
//            occurrenceOutput.addOne(Constants.fileName     -> androidManifestFile.asJson)
//            occurrenceOutput.addOne(Constants.excerpt -> Utilities.dump(androidManifestFile, Some(lineNumber)).asJson)
//            occurrenceList.append(occurrenceOutput)
//          }
        case _ => // Node not found
      }
    }

    val sanitizedOccurrenceList = occurrenceList
      .map(occurrence =>
        mutable.Map[String, Json](Constants.sourceId -> "".asJson, Constants.occurrence -> occurrence.asJson).asJson
      )
      .toList

    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }

  /** Gets the value of a key from attributes
    * @param attributes
    *   the attributes of xml node
    * @return
    *   Option[string] value of attribute
    */
  private def getAttribute(attributes: MetaData, key: String): Option[String] = {
    val filteredAttrs = attributes.filter(_.key == key)
    if (filteredAttrs.nonEmpty) Some(filteredAttrs.head.value.head.text) else None
  }

  /** Returns matching line number from the file
    * @param fileName
    *   name of file
    * @param matchingText
    *   match text
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

  private def hasDataElements(cpg: Cpg): Boolean = {
    val taggedSources = cpg.tag
      .nameExact(Constants.catLevelOne)
      .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
      .l
    if (taggedSources.nonEmpty) true else false
  }

  private def getDefinedTargetSDKValue(xml: Elem): String = {
    val sdkNode = xml \\ SDK_KEY
    if (sdkNode.nonEmpty) {
      val node = sdkNode.head
      getAttribute(node.attributes, TARGET_SDK_ATTRIBUTE) match {
        case Some(v) => v
        case _       => ""
      }
    } else ""
  }
}
