package ai.privado.languageEngine.go.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.go.feeder.CollectionTaggerRule
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.*

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val methodUrlMap            = mutable.HashMap[Long, String]()
  private val classUrlMap             = mutable.HashMap[Long, String]()
  private val ROUTES_FILE_PATTERN     = ".*(routes|routers).go"
  private val COLLECTION_FUNC_PATTERN = "(?i).*(Get|Post|Put|Patch|Delete).*"
  private val COLLECTION_REST_PATTERN = "(?i)(Get|Post|Put|Patch|Delete)"

  def getIngressUrls(): List[String] =
    CollectionUtility.getCollectionUrls(cpg, methodUrlMap, classUrlMap)
  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, sourceRuleInfo: RuleInfo): Unit = {
    tagFuncCallCollection(builder, sourceRuleInfo)
    tagRestCallCollection(builder, sourceRuleInfo)
  }

  private def tagFuncCallCollection(builder: DiffGraphBuilder, sourceRuleInfo: RuleInfo): Unit = {
    val collectionCallMethod = cpg.call
      .name("Methods")
      .where(_.astChildren.isLiteral.code(COLLECTION_FUNC_PATTERN))
      .astChildren
      .isCall
      .name("HandleFunc")
      .l

    // smaple route -> myrouter.HandleFunc("/user/", getUser).Method("GET")
    val collectionMethodsCache = collectionCallMethod
      .map { m =>
        if (m.argument.size >= 2) {
          val targetCollectionUrl =
            m.argument(1).code

          if (targetCollectionUrl.nonEmpty) {
            val methodName             = m.argument(2).code
            val targetCollectionMethod = cpg.method.name(methodName).l
            if (targetCollectionMethod.nonEmpty) {
              methodUrlMap.addOne(targetCollectionMethod.head.id -> m.argument.isLiteral.code.head)
              targetCollectionMethod
            } else None
          } else None
        } else None
      }
      .l
      .flatten(method => method)

    tagDirectSource(cpg, builder, collectionMethodsCache.l, sourceRuleInfo)
  }

  private def tagRestCallCollection(builder: DiffGraphBuilder, sourceRuleInfo: RuleInfo): Unit = {

    val collectionCallMethod = cpg
      .call(COLLECTION_REST_PATTERN)
      .where(_.file.name(ROUTES_FILE_PATTERN))
      .l

    // sample route -> r.Get("/user/{id}", getUser), r.Get("/user/{id}", handler.getUser)
    val collectionMethodsCache = collectionCallMethod
      .map { m =>
        if (m.argument.size >= 2) {
          val targetCollectionUrl =
            m.argument(1).code

          if (targetCollectionUrl.nonEmpty) {
            val methodName             = m.argument(2).code.split("\\.").lastOption.getOrElse("")
            val targetCollectionMethod = cpg.method.name(methodName).l
            if (targetCollectionMethod.nonEmpty) {
              methodUrlMap.addOne(targetCollectionMethod.head.id -> m.argument(1).code)
              targetCollectionMethod
            } else None
          } else None
        } else None
      }
      .l
      .flatten(method => method)

    tagDirectSource(cpg, builder, collectionMethodsCache.l, sourceRuleInfo)
  }

  private def tagDirectSource(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    sourceRuleInfo: RuleInfo
  ): Unit = {
    val collectionPoints = collectionMethods.flatMap(collectionMethod => {
      val parameters = collectionMethod.parameter
      val locals     = collectionMethod.local
      val literals   = collectionMethod.literal

      val matchingParameters = parameters.where(_.name(sourceRuleInfo.combinedRulePattern)).whereNot(_.code("self")).l
      val matchingLocals     = locals.code(sourceRuleInfo.combinedRulePattern).l
      val matchingLiterals = literals
        .code(sourceRuleInfo.combinedRulePattern)
        .l

      if (!(matchingParameters.isEmpty && matchingLocals.isEmpty && matchingLiterals.isEmpty)) {
        matchingParameters.foreach(parameter =>
          storeForTag(builder, parameter, ruleCache)(Constants.id, sourceRuleInfo.id)
        )
        matchingLocals.foreach(local => storeForTag(builder, local, ruleCache)(Constants.id, sourceRuleInfo.id))
        matchingLiterals.foreach(literal => storeForTag(builder, literal, ruleCache)(Constants.id, sourceRuleInfo.id))
        Some(collectionMethod)
      } else {
        None
      }
    })

    tagMethodEndpoints(builder, collectionPoints.l, sourceRuleInfo)
  }

  private def tagMethodEndpoints(
    builder: DiffGraphBuilder,
    collectionPoints: List[Method],
    sourceRuleInfo: RuleInfo,
    returnByName: Boolean = false
  ) = {
    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, CollectionTaggerRule.rule, ruleCache)
      storeForTag(builder, collectionPoint, ruleCache)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint, returnByName)
      )
    })
  }

  private def getFinalEndPoint(collectionPoint: Method, returnByName: Boolean): String = {
    if (returnByName) {
      collectionPoint.name
    } else {
      val methodUrl = methodUrlMap.getOrElse(collectionPoint.id(), "")
      Try(classUrlMap.getOrElse(collectionPoint.typeDecl.head.id(), "")) match {
        case Success(classUrl) => classUrl + methodUrl
        case Failure(e) =>
          methodUrl
      }
    }
  }
}
