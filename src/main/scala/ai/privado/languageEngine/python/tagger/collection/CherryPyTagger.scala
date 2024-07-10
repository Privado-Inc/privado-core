package ai.privado.languageEngine.python.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{CatLevelOne, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.{PrivadoParallelCpgPass, PrivadoSimpleCpgPass}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

class CherryPyTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoSimpleCpgPass(cpg) {
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)
  private val CHERRY_PY_CONNECT_METHOD_FULL_NAME            = "cherrypy.py:<module>.dispatch.RoutesDispatcher.connect"

  val cherryPyRule = RuleInfo(
    "Collections.CherryPy",
    "CherryPy Endpoints",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(CHERRY_PY_CONNECT_METHOD_FULL_NAME),
    false,
    "high",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.COLLECTIONS,
    "",
    Language.PYTHON,
    Array()
  )

  override def run(builder: DiffGraphBuilder): Unit = {
    val methodToUrlCache = cpg.call
      .methodFullName(CHERRY_PY_CONNECT_METHOD_FULL_NAME)
      .map(call => {
        val (path, action)   = getRelevantDataFromCallArgs(call)
        val pertainingMethod = getActionMethod(action)
        (pertainingMethod, path)
      })
      .filter((method, path) => method.isDefined && path.nonEmpty)
      .toList

    val collectionMethodsCache = methodToUrlCache.map((method, _) => method.get).toList
    methodUrlMap.addAll(methodToUrlCache.map((method, path) => (method.id.headOption.getOrElse(-1.toLong), path)))
    tagSources(builder, collectionMethodsCache)
  }

  protected def tagSources(builder: DiffGraphBuilder, collectionMethodsCache: List[Method]): Unit = {
    CollectionUtility.tagDirectSources(
      builder,
      collectionMethodsCache,
      ruleCache.getRule.sources,
      cherryPyRule,
      ruleCache,
      methodUrlMap = methodUrlMap
    )

    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      collectionMethodsCache,
      cherryPyRule,
      ruleCache,
      methodUrlMap = methodUrlMap
    )
  }

  private def getRelevantDataFromCallArgs(call: Call): (String, String) = {
    val path =
      call.argument
        .where(_.argumentIndex(2))
        .astChildren
        .isLiteral
        .code
        .l
        .headOption
        .getOrElse("")
        .replaceAll("(\"|')", "")
    val action =
      call.argument.where(_.argumentName("action")).isLiteral.code.l.headOption.getOrElse("").replaceAll("(\"|')", "")
    (path, action)
  }

  private def getActionMethod(action: String): Option[Method] = {
    cpg.method.nameExact(action).headOption
  }
}
