package ai.privado.entrypoint

import ai.privado.exporter.JSONExporter
import ai.privado.model.{NodeType, RuleInfo, Rules}
import ai.privado.semantic.Language._
import better.files.File
import io.circe.yaml.parser
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._

import java.util.UUID
import scala.util.{Failure, Success}

object ScanProcessor extends CommandProcessor {

  def processRules(): Rules = {
    val ir: File = File(config.internalRulesPath.head)
    val internalRules = ir.listRecursively
      .filter(f => f.extension == Some(".yaml") || f.extension == Some(".YAML"))
      .map(file => {
        val filePath            = file.pathAsString
        val fileName            = file.nameWithoutExtension
        val immediateParentName = file.parent.nameWithoutExtension
        parser.parse(file.contentAsString) match {
          case Right(json) =>
            import ai.privado.model.CirceEnDe._
            json.as[Rules] match {
              case Right(rules) =>
                rules.copy(
                  sources = rules.sources.map(x =>
                    x.copy(
                      filePath = filePath,
                      fileName = fileName,
                      parentName = immediateParentName,
                      nodeType = NodeType.SOURCE.toString
                    )
                  ),
                  sinks = rules.sinks.map(x =>
                    x.copy(
                      filePath = filePath,
                      fileName = fileName,
                      parentName = immediateParentName,
                      nodeType = NodeType.withNameWithDefault(immediateParentName).toString
                    )
                  )
                )
              case _ =>
                Rules(List[RuleInfo](), List[RuleInfo]())
            }
          case _ =>
            Rules(List[RuleInfo](), List[RuleInfo]())
        }
      })
      .reduce((a, b) => a.copy(sources = a.sources ++ b.sources, sinks = a.sinks ++ b.sinks))
    // TODO: remove this println
    println(internalRules)
    internalRules
  }
  override def process(): Unit = {
    println("Hello Joern")
    println("Creating CPG... ")
    processCPG(processRules())
  }

  def processCPG(processedRules: Rules): Unit = {
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

        val rules: List[RuleInfo] = processedRules.sources ++ processedRules.sinks

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
