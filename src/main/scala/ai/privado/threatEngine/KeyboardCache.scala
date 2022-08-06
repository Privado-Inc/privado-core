package ai.privado.threatEngine

import io.joern.x2cpg.SourceFiles

import scala.xml.{Elem, XML}

object KeyboardCache {

  val THREAT_ID = "Threats.Collections.KeyboardCache"
  val repoPath  = "/Users/khemrajrathore/Privado/joern-testing/GpsTrackerWithFirebase"

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

  def getAllXmlFilePath() = {
    val sourceFileNames = SourceFiles.determine(Set(repoPath), Set(".xml"))
    // .filter(source => source.endsWith("activity_friend_validation_drawer.xml"))
    val editText = sourceFileNames.map(sourceFile => {
      val xml: Elem = XML.loadFile(sourceFile)
      xml \ "EditText"
    })
    editText
      .filter(_.nonEmpty)
      .foreach(texts => {
        texts.foreach(text => {

          text match {
            case Elem(prefix, label, attributes, scope, child @ _*) =>
              attributes.foreach(attribute => {
                if (attribute.key == "inputType" && sensitiveInputTypeList.contains(attribute.value.head.text))
                  println(attribute.key, attribute.value.head)
              })
            case _ => println("could not find node");
          }

        })
        // println(texts)
      })
  }

  getAllXmlFilePath()
}
