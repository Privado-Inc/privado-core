package ai.privado.languageEngine.ruby.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import java.io.File

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger                              = LoggerFactory.getLogger(this.getClass)
  private val methodUrlMap                        = mutable.HashMap[Long, String]()
  private val classUrlMap                         = mutable.HashMap[Long, String]()
  private val COLLECTION_METHOD_REFERENCE_PATTERN = "to:.*"
  private val SYMBOL_HASH                         = "#"
  private val ROUTES_FILE_PATTERN                 = ".*routes.rb"
  private val RESOURCES                           = "resources"

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {
    tagRestCallCollection(builder, collectionRuleInfo)
    tagResourceCollection(builder, collectionRuleInfo)
  }

  private def tagResourceCollection(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {

    val collectionRoutes =
      cpg.call.where(_.file.name(ROUTES_FILE_PATTERN)).name(RESOURCES).where(_.argument.isLiteral.size==1).l

    val collectionMethodsCache = collectionRoutes
      .map { m =>
        //        sample route -> resources :phone_numbers, only: %i(index destroy)
        val targetCollectionUrl = m.argument.isLiteral.code.headOption.getOrElse("").strip().stripPrefix(":")
        if (targetCollectionUrl.nonEmpty) {
          val methodName             = "show|update|new|create|destroy|index"
          val fileName               = ".*" + File.separator + targetCollectionUrl + "_controller.rb"
          val targetCollectionMethod = cpg.method.name(methodName).where(_.file.name(fileName)).l
          if (targetCollectionMethod.nonEmpty) {
            for (method <- targetCollectionMethod) {
              methodUrlMap.addOne(method.id -> s"${m.argument.isLiteral.code.head} -> ${method.name}")
            }
            targetCollectionMethod
          } else None
        } else None
      }
      .l
      .flatten(method => method) // returns the handler method list

    tagDirectSources(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
    //    TODO: tag derived sources too
  }
  private def tagRestCallCollection(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {
    val collectionMethods = cpg
      .call("get|post|put|patch|delete")
      .where(_.file.name(ROUTES_FILE_PATTERN))
      .filter(_.argument.isCall.code(COLLECTION_METHOD_REFERENCE_PATTERN).argument.isLiteral.nonEmpty)
      .l

    /*
    creating map having key as method Id which is getting invoked by collection and value as url
     */
    val collectionMethodsCache = collectionMethods
      .map { m =>
        //        sample route -> get '/hotels/new', to: 'hotels#new'
        // TODO: check scenarios involving single, double and no quote
        val targetCollectionUrl =
          m.argument.isCall.code(COLLECTION_METHOD_REFERENCE_PATTERN).argument.isLiteral.code.head

        if (targetCollectionUrl.nonEmpty && targetCollectionUrl.contains(SYMBOL_HASH)) {
          val routeMethodCall: String = targetCollectionUrl.stripPrefix("\"").stripSuffix("\"")
          val routeSeparatedStrings   = routeMethodCall.split(SYMBOL_HASH)
          if (routeSeparatedStrings.size >= 2) {
            val methodName = routeSeparatedStrings(1)
            val fileName =
              ".*" + File.separator + routeSeparatedStrings.headOption.getOrElse("UNKNOWN") + "_controller.rb"
            val targetCollectionMethod = cpg.method.name(methodName).where(_.file.name(fileName)).l
            if (targetCollectionMethod.nonEmpty) {
              methodUrlMap.addOne(
                targetCollectionMethod.head.id -> m.argument.isLiteral.code.head
                //          TODO: ⬆️use .stripPrefix("\"").stripSuffix("\"") if required
              )
              targetCollectionMethod
            } else None
          } else None
        } else None
      }
      .l
      .flatten(method => method) // returns the handler method list

    tagDirectSources(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
//    TODO: tag derived sources too
  }

  def tagDirectSources(
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

        // TODO: handle cases where `request.args.get('id', None)` used directly in handler block without method param
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
