package ai.privado.core

import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays

import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello Joern")
    print("Creating CPG")
    val directory = "/Users/pandurang/projects/joern-samples/x42/java/first/temp"
    val config = Config(inputPaths = Set(directory))
    val cpgOrException = JavaSrc2Cpg().createCpg(config)

    cpgOrException match {
      case Success(cpg) =>
        println("DONE")
        println("Applying default overlays")
        applyDefaultOverlays(cpg)
        println("Print all methods")
        println("======================")
      case Failure(exception) =>
        println("FAILED")
        println(exception)
    }
  }
}
