package ai.privado.languageEngine.java.passes.config

import ai.privado.passes.YamlLinkerPass
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

class JavaYamlLinkerPass(cpg: Cpg) extends YamlLinkerPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)
}
