package ai.privado.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable

trait AnnotationsCollectionTaggerBase(cpg: Cpg, ruleCache: RuleCache) {
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)

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

  protected def getUrlFromAnnotationsCode(annotation: Annotation): String = {
    annotation.astChildren.headOption.code.headOption.getOrElse("").replaceAll(".*\"(.*?)\".*", "/$1").stripPrefix("/")
  }

  protected def collectAnnotatedUrlsFromMethods(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
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

  protected def collectAnnotatedUrlsFromClasses(combinedRulePatterns: String): Map[Long, String] = {
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
