package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.sink.api.Utility.tagAPICallByItsUrlMethod
import ai.privado.model.{Constants, InternalTag, NodeType}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method, MethodParameterIn}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.CollectionHasAsScala

class JavaAPISinkByParameterMarkByAnnotationTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[(Method, List[String])](cpg) {

  private val apiMatchingRegex =
    ruleCache.getAllRuleInfo.filter(_.nodeType == NodeType.API).map(_.combinedRulePattern).mkString("(", "|", ")")

  override def generateParts(): Array[(Method, List[String])] = {

    val methodWithAnnotationCode = cpg.parameter
      .filter(_.index != 0)
      .where(_.annotation.parameterAssign.code("(?i).*(url|endpoint)"))
      .map { para =>
        (para.method, para.method.annotation.codeNot("@(Inject|Provides|Singleton)").code.l)
      }
      .filter(_._2.nonEmpty)
      .l

    methodWithAnnotationCode.map { entry =>
      val method         = entry._1
      val annotationCode = entry._2

      val (initMethods, nonInitMethods) =
        cpg.method.where(_.parameter.annotation.code(annotationCode: _*)).l.partition(_.name.equals("<init>"))

      val fullNames = (initMethods.fullName
        .map(_.split("[.]<init").headOption.getOrElse(""))
        .filter(_.nonEmpty)
        .l ++ nonInitMethods.signature
        .map(_.split("\\(").headOption.getOrElse(""))
        .filter(_.nonEmpty)
        .l).dedup.l
      (method, fullNames)
    }.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, methodWithTypeFullNames: (Method, List[String])): Unit = {

    val methodNode    = methodWithTypeFullNames._1
    val typeFullNames = methodWithTypeFullNames._2

    cpg.member.typeFullName(typeFullNames: _*).foreach { m =>
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
