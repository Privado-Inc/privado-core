package ai.privado.entrypoint

import ai.privado.joern.language._
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Languages}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCredentials, NewMynodetype}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import scopt.OParser

import scala.util.{Failure, Success}

/** Example program that makes use of Joern as a library
  */
object Main {
  def main(args: Array[String]): Unit = {

    CommandParser.parse(args) match {
      case Some(config) =>
        if (config.cmd.head == "scan") {
          println("Hello Joern")
          print("Creating CPG... ")
          val directory = config.sourceFile.head
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
              cpg.method.name.foreach(println)
              println("=====================")
              println("Running a custom pass to add some custom nodes")
              new MyPass(cpg).createAndApply()
              new MyCredPass(cpg).createAndApply()
              println("Running custom queries")
              cpg.mynodetype.foreach(println)
              cpg.mynodetype.myCustomStep.l
            case Failure(exception) =>
              println("[FAILED]")
              println(exception)
          }
        }
      case _ =>
      // arguments are bad, error message will have been displayed
    }

  }
}

/** Example of a custom pass that creates and stores a node in the CPG.
  */
class MyPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val n = NewMynodetype().myproperty("foo")
    builder.addNode(n)
  }
}

class MyCredPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.literal.code(".*password.*").foreach { literal =>
      val credential = NewCredentials().code(literal.code)
      builder.addNode(credential)
      builder.addEdge(literal, credential, EdgeTypes.IS_CREDENTIAL)
    }
  }
}
