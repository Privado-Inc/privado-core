package ai.privado.languageEngine.go.passes.config

import ai.privado.passes.PropertyEnvLinkerPassBase
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Literal}
import io.shiftleft.semanticcpg.language.*

class GoEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  override def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchingLiteralsToPropertyNode(property.name).foreach(lit => {
      connectEnvProperty(lit, property, builder)
    })
  }

  // List all literal fetching property value
  // ex: os.get("KEY")
  def matchingLiteralsToPropertyNode(propertyName: String): List[Literal] = {
    // To get every dot seperated part of the env key where each element is progressively longer,
    // containing one more dot-separated value
    // example for config.prod.API_URL => get ["API_URL", "prod.API_URL", "config.prod.API_URL"]
    val parts       = propertyName.split("\\.")
    val propertyKey = parts.reverse.indices.map(i => parts.takeRight(i + 1).mkString(".")).mkString("|")
    cpg.literal
      .code(s"\"($propertyKey)\"")
      .filter(_.inCall.name("(?i).*getenv").nonEmpty)
      .toList
  }
}
