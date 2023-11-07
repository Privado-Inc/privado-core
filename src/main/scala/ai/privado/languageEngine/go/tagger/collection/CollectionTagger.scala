package ai.privado.languageEngine.go.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.*

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val methodUrlMap        = mutable.HashMap[Long, String]()
  private val classUrlMap         = mutable.HashMap[Long, String]()
  private val ROUTES_FILE_PATTERN = ".*(routes|routers).go"

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {
    tagFuncCallCollection(builder, collectionRuleInfo)
    tagRestCallCollection(builder, collectionRuleInfo)
  }

  private def tagFuncCallCollection(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {
    val collectionCallMethod = cpg.call
      .name("Methods")
      .where(_.astChildren.isLiteral.code("(?i).*(Get|Post|Put|Patch|Delete).*"))
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

    tagDirectSource(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
  }

  private def tagRestCallCollection(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {

    val collectionCallMethod = cpg
      .call("(?i)(Get|Post|Put|Patch|Delete)")
      .where(_.file.name(ROUTES_FILE_PATTERN))
      .l

    // sample route -> r.Get("/user/{id}", getUser)
    val collectionMethodsCache = collectionCallMethod
      .map { m =>
        if (m.argument.size >= 2) {
          val targetCollectionUrl =
            m.argument(1).code

          if (targetCollectionUrl.nonEmpty) {
            val methodName             = m.argument(2).code
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

    tagDirectSource(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
  }

  private def tagDirectSource(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    collectionRuleInfo: RuleInfo
  ): Unit = {
    val collectionPoints = collectionMethods.flatMap(collectionMethod => {
      ruleCache.getRule.sources.flatMap(sourceRule => {
        val parameters = collectionMethod.parameter
        val locals     = collectionMethod.local
        val literals   = collectionMethod.literal

        val matchingParameters = parameters.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("self")).l
        val matchingLocals     = locals.code(sourceRule.combinedRulePattern).l
        val matchingLiterals = literals
          .code(sourceRule.combinedRulePattern)
          .l

        if (!(matchingParameters.isEmpty && matchingLocals.isEmpty && matchingLiterals.isEmpty)) {
          matchingParameters.foreach(parameter =>
            storeForTag(builder, parameter, ruleCache)(Constants.id, sourceRule.id)
          )
          matchingLocals.foreach(local => storeForTag(builder, local, ruleCache)(Constants.id, sourceRule.id))
          matchingLiterals.foreach(literal => storeForTag(builder, literal, ruleCache)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        } else {
          None
        }
      })
    })

    tagMethodEndpoints(builder, collectionPoints.l, collectionRuleInfo)
  }

  private def tagMethodEndpoints(
    builder: DiffGraphBuilder,
    collectionPoints: List[Method],
    collectionRuleInfo: RuleInfo,
    returnByName: Boolean = false
  ) = {
    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, collectionRuleInfo, ruleCache)
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
