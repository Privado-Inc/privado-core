package ai.privado.entrypoint

import ai.privado.auth.AuthenticationHandler
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.metric.MetricHandler
import io.shiftleft.codepropertygraph.generated.nodes.{NewCredentials, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

/** Privado Core main entry point
  */
object Main {
  def main(args: Array[String]): Unit = {

    CommandParser.parse(args) match {
      case Some(processor) =>
        processor.process()
        val sourceRepoLocation = config.sourceLocation.head
        AuthenticationHandler.authenticate(sourceRepoLocation)
        MetricHandler.timeMetric(processor.process(), "TOTAL")
      case _ =>
      // arguments are bad, error message should get displayed from inside CommandParser.parse
    }

  }
}

/** TODO: get rid of this class Example of a custom pass that creates and stores a node in the CPG.
  */
class MyPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    // val n = NewMynodetype().myproperty("foo")
    // builder.addNode(n)
    cpg.literal.foreach(ident => builder.addEdge(ident, NewTag().name("Name").value("Sensitive"), EdgeTypes.TAGGED_BY))
  }
}

/** TODO: get rid of this class Example of a custom pass that creates and stores a node in the CPG.
  */
class MyCredPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.literal.code(".*password.*").foreach { literal =>
      val credential = NewCredentials().code(literal.code)
      builder.addNode(credential)
      builder.addEdge(literal, credential, EdgeTypes.IS_CREDENTIAL)
    }
  }
}
