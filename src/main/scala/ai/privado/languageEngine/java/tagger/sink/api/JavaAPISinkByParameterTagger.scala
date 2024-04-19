package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.sink.api.Utility.tagAPICallByItsUrlMethod
import ai.privado.model.{Constants, InternalTag, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.CollectionHasAsScala
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.nodes.Method

import scala.collection.mutable

class JavaAPISinkByParameterTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[(Method, String)](cpg) {

  private val apiMatchingRegex =
    ruleCache.getAllRuleInfo.filter(_.nodeType == NodeType.API).map(_.combinedRulePattern).mkString("(", "|", ")")

  override def generateParts(): Array[(Method, String)] = {

    /* Below query looks for methods whose parameter names ends with `url|endpoint`,
    for such method, get the typeFullName of the returned object by this method
     */

    val typeFullNameByUrlLikeMatch = mutable.ListBuffer[(Method, String)]()
    val (initMethods, nonInitMethods) = cpg.method
      .where(_.parameter.filter(_.index != 0).annotation.parameterAssign.code("(?i).*(url|endpoint)"))
      .l
      .partition(_.name.equals("<init>"))

    val initMethodFullNames = initMethods.map { m => (m, m.fullName.split("[.]<init").headOption.getOrElse("")) }.l

    initMethodFullNames.foreach { entrySet1 =>
      val m        = entrySet1._1
      val fullName = entrySet1._2

      val (firstLevelConfig, firstLevelNonConfig) = cpg.parameter
        .filter(_.index != 0)
        .typeFullName(fullName)
        .method
        .name("<init>")
        .fullName
        .map(_.split("[.]<init").headOption.getOrElse(""))
        .filter(_.nonEmpty)
        .l
        .partition(cpg.member.typeFullName(_).nonEmpty)

      firstLevelConfig.foreach(typeFullNameByUrlLikeMatch.addOne(m, _))

      val (secondLevelConfig, secondLevelNonConfig) = cpg.parameter
        .filter(_.index != 0)
        .typeFullName(firstLevelNonConfig: _*)
        .method
        .name("<init>")
        .fullName
        .map(_.split("[.]<init").headOption.getOrElse(""))
        .filter(_.nonEmpty)
        .l
        .partition(cpg.member.typeFullName(_).nonEmpty)

      secondLevelConfig.foreach(typeFullNameByUrlLikeMatch.addOne(m, _))

    }

    typeFullNameByUrlLikeMatch.appendAll(
      nonInitMethods.map(m => (m, m.signature.split("\\(").headOption.getOrElse(""))).filter(_._2.nonEmpty).l
    )

    typeFullNameByUrlLikeMatch.dedup.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, methodWithTypeFullName: (Method, String)): Unit = {

    val methodNode   = methodWithTypeFullName._1
    val typeFullName = methodWithTypeFullName._2

    cpg.member.typeFullName(typeFullName).foreach { m =>
      val memberName = m.name
      val fileName   = m.file.name.headOption.getOrElse("")

      /* Below query looks for fieldIdentifier in the given file and return calls which are made on top of this fieldIdentifier
       The 1st callIn returns the field Access node and the 2nd returns the actual call
       */

      val sinkCalls = cpg.fieldAccess.fieldIdentifier
        .canonicalName(memberName)
        .where(_.file.nameExact(fileName))
        .l
        .inCall
        .inCall
        .where(_.nameNot(Operators.ALL.asScala.toSeq.appended("<init>"): _*))
        .l

      // Mark the nodes as API sink
      tagAPICallByItsUrlMethod(cpg, builder, methodNode, sinkCalls, apiMatchingRegex, ruleCache)

    }
  }
}
