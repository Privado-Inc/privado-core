package ai.privado.languageEngine.python.tagger

import ai.privado.model.ConfigAndRules
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.APITagger
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")

    println(s"${Calendar.getInstance().getTime} - LiteralTagger invoked...")
    new LiteralTagger(cpg).createAndApply()
    println(s"${Calendar.getInstance().getTime} - APITagger invoked...")
    new APITagger(cpg).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}