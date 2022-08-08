package ai.privado.threatEngine

import ai.privado.cache.RuleCache
import ai.privado.model.{ConfigAndRules, RuleInfo}
import ai.privado.utility.Utilities._

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, MetaData, XML}

object KeyboardCache {

  val THREAT_ID = "Threats.Collections.KeyboardCache"

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

  def getViolations(repoPath: String) = {

    getAllFilesRecursively(repoPath, Set(".xml")) match {
      case Some(sourceFileNames) =>
        val editTexts = sourceFileNames.map(sourceFile => {
          val xml: Elem = XML.loadFile(sourceFile)
          xml \ "EditText"
        })
        editTexts
          .filter(_.nonEmpty)
          .foreach(editText => {
            editText.foreach {
              case Elem(prefix, label, attributes, scope, child @ _*) =>
                if (isSensitiveInputTypePresent(attributes) || isSensitiveId(attributes, RuleCache.getRule.sources)) {
                  println("Found a sensitive item... ")
                }
              case _ => println("could not find node");
            }
            // println(texts)
          })
      case None => // repoPath is not a directory
    }

  }

  private def isUsingTextNoSuggestions(attributes: MetaData) = {}

  /** Checks if the field id is sensitive
    * @param attributes
    * @param sources
    * @return
    */
  private def isSensitiveId(attributes: MetaData, sources: List[RuleInfo]): Boolean = {
    try {
      val elementId = attributes.filter(attribute => attribute.prefixedKey == "android:id").head.value.head.text
      sources.map(source => elementId.matches(source.patterns.head)).foldLeft(false)((a, b) => a || b)
    } catch {
      case e: Exception => false
    }

  }

  /** Checks if the field "inputType" contains item from sensitiveInputTypeList
    *
    * @param attributes
    * @return
    */
  private def isSensitiveInputTypePresent(attributes: MetaData): Boolean = {
    Try(
      attributes.filter(attribute =>
        attribute.prefixedKey == "android:inputType" && sensitiveInputTypeList.contains(attribute.value.head.text)
      )
    ) match {
      case Success(filteredAttributes) => filteredAttributes.nonEmpty

      case Failure(exception) => false
    }
  }
}
