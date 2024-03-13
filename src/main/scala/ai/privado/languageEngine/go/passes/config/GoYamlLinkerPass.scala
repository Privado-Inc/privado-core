package ai.privado.languageEngine.go.passes.config

import ai.privado.passes.YamlLinkerPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class GoYamlLinkerPass(cpg: Cpg) extends YamlLinkerPass(cpg) {
  override def matchingLiteralsToPropertyNode(propertyName: String): List[Literal] = {
    val propertyKey = propertyName.split("\\.").last
    cpg.literal
      .codeExact("\"" + propertyKey + "\"")
      .filter(_.inCall.name("(?i).*getenv").nonEmpty)
      .toList
  }
}
