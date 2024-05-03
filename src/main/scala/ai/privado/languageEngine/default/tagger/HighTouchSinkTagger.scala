package ai.privado.languageEngine.default.tagger

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, FilterProperty, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.languageEngine.default.NodeStarters
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.HightouchSink
import org.slf4j.LoggerFactory

import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.Try

case class JsonRules(rules: List[HighTouchRule])
case class HighTouchRule(id: String, domain: String, name: String, pattern: String)

class HighTouchSinkTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    val classLoader = getClass.getClassLoader
    val inputStream = classLoader.getResourceAsStream("hightouch/rules/hightouch_rules.json")
    val jsonString  = Source.fromInputStream(inputStream).mkString
    val jsonObject  = ujson.read(jsonString).arr
    jsonObject
      .map(obj =>
        val asObj = obj.obj
        val ruleInfo = RuleInfo(
          asObj.get("id").get.str,
          asObj.get("name").get.str,
          "",
          FilterProperty.CODE,
          Array[String](asObj.get("domain").get.str),
          List[String](asObj.get("pattern").get.str),
          false,
          "",
          HashMap[String, String](),
          NodeType.REGULAR,
          "",
          CatLevelOne.SINKS,
          "third_parties",
          Language.DEFAULT,
          Array[String](),
          isGenerated = true
        )
        ruleCache.setRuleInfo(ruleInfo)
        ruleInfo
      )
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val matchingSinks =
      cpg.highTouchSink.filter(_.actualDestinationName.matches(ruleInfo.combinedRulePattern)).toList
    matchingSinks.foreach(sink => {
      addRuleTags(builder, sink, ruleInfo, ruleCache)
    })
  }
}
