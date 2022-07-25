package ai.privado.entrypoint

import ai.privado.exporter.JSONExporter
import ai.privado.model.{RuleInfo, Rules}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Languages
import ai.privado.semantic.Language._
import better.files.File
import io.shiftleft.semanticcpg.language._

import java.util.UUID
import scala.util.{Failure, Success}
import io.circe.yaml.parser

object ScanProcessor extends CommandProcessor {

  def processRules(): Unit = {
    println(f"Internal rule path - ${config.internalRulesPath}")
    println(f"External rule path - ${config.externalRulePath}")
    val ir: File = File(config.internalRulesPath.head)
    ir.listRecursively
      .filter(f => f.extension == Some(".yaml") || f.extension == Some(".YAML"))
      .foreach(file => {
        parser.parse(file.contentAsString) match {
          case Right(json) =>
            import ai.privado.model.CirceEnDe._
            json.as[Rules] match {
              case Right(rules) =>
                println(rules)
              case _ =>
                println("No rules found")
            }
          case _ =>
            println("No rules found")
        }
      })
  }
  override def process(): Unit = {
    println("Hello Joern")
    println("Creating CPG... ")
    processRules()
    processCPG()
  }

  def processCPG(): Unit = {
    val directory = config.sourceLocation.head
    import io.joern.console.cpgcreation.guessLanguage
    val xtocpg = guessLanguage(directory) match {
      case Some(Languages.JAVASRC) =>
        val config = Config(inputPaths = Set(directory))
        JavaSrc2Cpg().createCpg(config)

      case _ =>
        Failure(new RuntimeException("Language Not Detected"))
    }
    xtocpg match {
      case Success(cpg) =>
        println("[DONE]")
        println("Applying default overlays")
        applyDefaultOverlays(cpg)
        println("Printing all methods:")
        println("=====================")

        val rules: List[RuleInfo] = List(RuleFeeder.sourceRule, RuleFeeder.apiRule)

        // Run tagger
        cpg.runTagger(rules)
        val dataflows = cpg.dataflow.l

        // Attach each dataflow with a unique id
        val dataflowMap = dataflows.map(dataflow => (UUID.randomUUID().toString, dataflow)).toMap

        // Exporting
        val outputFileName = "privado"
        JSONExporter.fileExport(cpg, outputFileName, directory, dataflowMap)

        // Utility to debug
        for (tagName <- cpg.tag.name.dedup.l) {
          val tags = cpg.tag(tagName).l
          println(s"tag Name : ${tagName}, size : ${tags.size}")
          println("Values : ")
          for (tag <- tags) {
            print(s"${tag.value}, ")
          }
          println("\n----------------------------------------")
        }
      case Failure(exception) =>
        println("[FAILED]")
        println(exception)
    }
  }

  override var config: PrivadoInput = _
}
