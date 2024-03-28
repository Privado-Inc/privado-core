package ai.privado.languageEngine.java.passes.config

import io.shiftleft.codepropertygraph.generated.nodes.*
import ai.privado.passes.YamlLinkerPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class JavaYamlLinkerPass(cpg: Cpg) extends YamlLinkerPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def matchingLiteralsToPropertyNode(propertyName: String): List[Literal] = {
    val propertyKey = propertyName.split("\\.").last
    cpg.literal
      .codeExact("\"" + propertyKey + "\"")
      .filter(_.inCall.name("(?i).*getenv").nonEmpty)
      .toList
  }
}
