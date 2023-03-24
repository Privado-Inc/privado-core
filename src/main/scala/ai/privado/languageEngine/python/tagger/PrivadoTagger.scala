package ai.privado.languageEngine.python.tagger

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.TimeMetric
import ai.privado.languageEngine.python.tagger.sink.PythonAPITagger
import ai.privado.languageEngine.python.tagger.collection.CollectionTagger
import ai.privado.languageEngine.python.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
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

    println(s"${TimeMetric.getNewTimeAndSetItToStageLast()} - --LiteralTagger invoked...")
    new LiteralTagger(cpg).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --LiteralTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    println(s"${Calendar.getInstance().getTime} - --IdentifierTagger invoked...")
    new IdentifierTagger(cpg).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --IdentifierTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    println(s"${Calendar.getInstance().getTime} - --APITagger invoked...")
    new PythonAPITagger(cpg).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --APITagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --RegularSinkTagger invoked...")
    new RegularSinkTagger(cpg).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --RegularSinkTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --CollectionTagger invoked...")
    new CollectionTagger(cpg, RuleCache.getRule.sources).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --CollectionTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    logger.info("Done with tagging")

    cpg.tag
  }

}
