package ai.privado.threatEngine

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities._
import ai.privado.threatEngine.ThreatUtility._
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, MetaData, XML}
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object KeyboardCache {

  private val ID         = "android:id"
  private val INPUT_TYPE = "android:inputType"
  private val logger     = LoggerFactory.getLogger(getClass)

  val sensitiveInputTypeList = List(
    "numberPassword",
    "phone",
    "textEmailAddress",
    "textPassword",
    "textPersonName",
    "textPostalAddress",
    "textWebEmailAddress",
    "textWebPassword"
  )

  /** Fetch all the violations which violate Key-board cache threat
    * @param repoPath
    * @return
    */
  def getViolations(repoPath: String): Try[(Boolean, List[Json])] = Try {
    val occurrenceList = ListBuffer[mutable.LinkedHashMap[String, Json]]()
    getAllFilesRecursively(repoPath, Set(".xml")) match {
      case Some(sourceFileNames) =>
        sourceFileNames.foreach(sourceFile => {
          val xml: Elem = XML.loadFile(sourceFile)
          val editText  = xml \ "EditText"
          if (editText.nonEmpty) {
            editText.foreach {
              case Elem(prefix, label, attributes, scope, child @ _*) =>
                if (isSensitiveInputTypePresent(attributes) || isSensitiveId(attributes, RuleCache.getRule.sources)) {
                  if (!isTextNoSuggestionsInInputTypePresent(attributes)) {
                    val idAttribute = attributes.filter(attribute => attribute.prefixedKey == ID).value.head
                    val lineNumber  = getLineNumberOfMatchingEditText(sourceFile, ID + "=\"" + idAttribute.text + "\"")
                    val occurrenceOutput = mutable.LinkedHashMap[String, Json]()
                    occurrenceOutput.addOne(Constants.sample       -> idAttribute.text.asJson)
                    occurrenceOutput.addOne(Constants.lineNumber   -> lineNumber.asJson)
                    occurrenceOutput.addOne(Constants.columnNumber -> (-1).asJson)
                    occurrenceOutput.addOne(Constants.fileName     -> sourceFile.asJson)
                    occurrenceOutput.addOne(Constants.excerpt -> Utilities.dump(sourceFile, Some(lineNumber)).asJson)
                    occurrenceList.append(occurrenceOutput)
                  }
                }
              case _ => // Node not found
            }
          }
        })
      case None => // repo is not correct
    }

    val sanitizedOccurrenceList = transformOccurrenceList(occurrenceList)

    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }

  /** Checks if the field id is sensitive
    * @param attributes
    * @param sources
    * @return
    */
  private def isSensitiveId(attributes: MetaData, sources: List[RuleInfo]): Boolean = {
    try {
      val elementId = attributes.filter(attribute => attribute.prefixedKey == ID).head.value.head.text
      sources
        .map(source => {
          var androidId = elementId
          if (elementId.startsWith("@+id/"))
            androidId = elementId.slice(5, elementId.size)
          androidId.matches(source.patterns.head)
        })
        .foldLeft(false)((a, b) => a || b)
    } catch {
      case _: Exception => false
    }

  }

  /** Checks if the field "inputType" contains item from sensitiveInputTypeList
    *
    * @param attributes
    * @return
    */
  private def isSensitiveInputTypePresent(attributes: MetaData, attributeType: String = INPUT_TYPE): Boolean = {
    Try(
      attributes.filter(attribute =>
        attribute.prefixedKey == attributeType && isAttributeValuePresentInSensitiveInputTypeList(
          attribute.value.head.text
        )
      )
    ) match {
      case Success(filteredAttributes) => filteredAttributes.nonEmpty

      case Failure(exception) =>
        logger.debug("Exception : ", exception)
        false
    }
  }

  /** Helper function to check if the attribute value contains any sensitive Type
    * @param attributeValue
    * @return
    */
  private def isAttributeValuePresentInSensitiveInputTypeList(attributeValue: String) = {
    Try(
      attributeValue.split("\\|").map(value => sensitiveInputTypeList.contains(value)).reduce((a, b) => a || b)
    ) match {
      case Success(result) => result
      case Failure(e) =>
        logger.debug("Exception : ", e)
        false
    }
  }

  private def isTextNoSuggestionsInInputTypePresent(attributes: MetaData): Boolean = {
    Try(
      attributes
        .filter(attribute => attribute.prefixedKey == INPUT_TYPE)
        .head
        .value
        .text
        .split("\\|")
        .contains("textNoSuggestions")
    ) match {
      case Success(value) => value
      case Failure(e) =>
        logger.debug("Exception : ", e)
        false
    }

  }
}
