package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

class AnnotationsCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.annotations).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    classUrlMap.addAll(collectAnnotatedUrlsFromClasses(ruleInfo.combinedRulePattern))
    val (methodUrls, collectionMethodsCache) = collectAnnotatedUrlsFromMethods(ruleInfo.combinedRulePattern)
    methodUrlMap.addAll(methodUrls)
    tagSources(builder, ruleInfo, collectionMethodsCache)
  }

  protected def tagSources(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    collectionMethodsCache: List[Method]
  ): Unit = {
    CollectionUtility.tagDirectSources(
      builder,
      collectionMethodsCache,
      ruleCache.getRule.sources,
      ruleInfo,
      ruleCache,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )

    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      collectionMethodsCache,
      ruleInfo,
      ruleCache,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
  }

  private def getUrlFromAnnotationsCode(annotation: Annotation): String = {
    annotation.astChildren.headOption.code.headOption.getOrElse("").replaceAll(".*\"(.*?)\".*", "/$1").stripPrefix("/")
  }

  private def collectAnnotatedUrlsFromMethods(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodAnnotations =
      cpg.annotation
        .name(combinedRulePatterns)
        .filter(_.method.nonEmpty)
        .l
    (
      methodAnnotations
        .map(ma => ma.method.head.id() -> getUrlFromAnnotationsCode(ma))
        .toMap,
      methodAnnotations.method.l
    )
  }

  private def collectAnnotatedUrlsFromClasses(combinedRulePatterns: String): Map[Long, String] = {
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .map(classAnnotation => {
        classAnnotation.typeDecl.head
          .id() -> getUrlFromAnnotationsCode(classAnnotation)
      })
      .toMap
  }

}
