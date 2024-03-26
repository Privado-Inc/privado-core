package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.CollectionHasAsScala
import ai.privado.utility.Utilities.storeForTag

class JavaAPISinkByParameterTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {
  override def generateParts(): Array[String] = {

    /* Below query looks for methods whose parameter names ends with `url|endpoint`,
    for such method, get the typeFullName of the returned object by this method
     */

    val typeFullNameByUrlLikeMatch = cpg.method
      .where(_.parameter.filter(_.index != 0).name("(?i).*(url|endpoint)"))
      .signature
      .map(_.split("\\(").headOption.getOrElse(""))
      .filter(_.nonEmpty)
      .l

    /* Below query looks for methods whose parameter names ends with `config`, and is part of a constructor
        for such method, get the typeFullName of the object for which this constructor is added
     */
    val typeFullNameByConfigLikeMatch = cpg.method
      .where(_.parameter.filter(_.index != 0).name("(?i).*(config)"))
      .where(_.name("<init>"))
      .fullName
      .map(_.split("[.]<init").headOption.getOrElse(""))
      .filter(_.nonEmpty)
      .l

    (typeFullNameByUrlLikeMatch ++ typeFullNameByConfigLikeMatch).dedup.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, typeFullName: String): Unit = {

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
      sinkCalls.foreach(storeForTag(builder, _, ruleCache)(InternalTag.API_SINK_MARKED.toString))

    }
  }
}
