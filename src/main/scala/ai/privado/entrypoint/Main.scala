package ai.privado.entrypoint

import ai.privado.auth.AuthenticationHandler
import ai.privado.cache.RuleCache
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.metric.MetricHandler
import io.shiftleft.codepropertygraph.generated.nodes.{NewCredentials, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

/** Privado Core main entry point
  */
object Main {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {

    CommandParser.parse(args) match {
      case Some(processor) =>
        try {
          MetricHandler.timeMetric(processor.process(), "Complete") match {
            case Right(_) =>
              logger.debug("Success from scan process! Proceeding to initiate auth flow")
              val sourceRepoLocation = config.sourceLocation.head
              AuthenticationHandler.authenticate(sourceRepoLocation)
              MetricHandler.compileAndSend()
            // raise error in case of failure, and collect
            // all handled & unhandled exceptions in catch
            case Left(err) => throw new Exception(err)
          }
        } catch {
          case e: Exception =>
            // any user-facing non-debug logging to be done internally
            logger.debug("Failure from scan process:", e)
            logger.debug("Skipping auth flow due to scan failure")
        }
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
