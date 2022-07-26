package ai.privado.entrypoint

import ai.privado.model.{NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCredentials, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import scala.collection.immutable.HashMap

/** Privado Core main entry point
  */
object Main {
  def main(args: Array[String]): Unit = {

    CommandParser.parse(args) match {
      case Some(processor) =>
        processor.process()
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

object RuleFeeder {
  val sourceRule = RuleInfo(
    "Data.Sensitive.Personal.Address",
    "Address",
    "Personal",
    List[String](".*(?i)zipCode.*"),
    true,
    "medium",
    HashMap("Pii" -> "Some info", "Law" -> "some other"),
    NodeType.SOURCE.toString,
    "",
    "",
    ""
  )

  val sourceRule2 = RuleInfo(
    "Data.Sensitive.Salesforce.url",
    "salesforce",
    "Salesforce",
    List[String](".*(?i)salesforce.*"),
    false,
    "high",
    HashMap("line1" -> "Some info of line1"),
    NodeType.SOURCE.toString,
    "",
    "",
    ""
  )

  val apiRule = RuleInfo(
    "Api.sensitive.urls",
    "Url",
    "API",
    List[String](
      "(?i).*http[s]{0,1}:.*|.*localhost.*|.*[.](?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me)(?:\\/|\"|'|`|:|\\|).*|.*\\d{1,3}[.]\\d{1,3}[.]\\d{1,3}[.]\\d{1,3}.*"
    ),
    false,
    "high",
    HashMap(),
    NodeType.API.toString,
    "",
    "",
    ""
  )
}
