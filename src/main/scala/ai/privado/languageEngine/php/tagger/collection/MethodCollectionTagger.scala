package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.utility.Utilities.{resolver, storeForTag}

import scala.collection.mutable

class MethodCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[Call](cpg) {

  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()

  override def generateParts(): Array[Call] = {
    // replace with methodFullName when available
    cpg.call.name("(get|post|put|delete)").toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, callNode: Call): Unit = {
    val (path, controllerFullName) = getPathAndControllerFromCall(callNode)

    val controllerName = extractControllerNameFromFullName(controllerFullName.split("::").headOption.getOrElse(""))
    methodUrlMap.addOne(
      (
        cpg.method.fullName(s"$controllerName->handle").headOption match {
          case Some(method) =>
            method.id()
          case None => -1
        },
        path
      )
    )

    // The RuleInfo is intended to be empty
    tagSources(
      builder,
      RuleInfo(
        "",
        "",
        "",
        FilterProperty.CODE,
        Array.empty[String],
        List.empty[String],
        false,
        "",
        Map.empty[String, String],
        NodeType.UNKNOWN,
        "",
        CatLevelOne.COLLECTIONS,
        catLevelTwo = Constants.annotations,
        Language.PHP,
        Array()
      ),
      methodUrlMap.flatMap((id, _) => cpg.method.id(id)).toList
    )
  }

  protected def tagSources(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    collectionMethodsCache: List[Method]
  ): Unit = {
    tagDirectSources(cpg, builder, collectionMethodsCache, ruleInfo)

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

  private def getPathAndControllerFromCall(call: Call): (String, String) = {
    (call.argument(1).code, call.argument(2).code)
  }

  private def extractControllerNameFromFullName(fullName: String): String = {
    fullName.split("\\\\").lastOption.getOrElse("")
  }

  private def tagDirectSources(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    collectionRuleInfo: RuleInfo
  ): Unit = {
    val collectionPoints = collectionMethods.flatMap(collectionMethod => {
      ruleCache.getRule.sources.flatMap(sourceRule => {
        val parameters = collectionMethod.parameter
        val locals     = collectionMethod.local
        val literals   = collectionMethod.call("(?:get).*").argument.isLiteral

        // TODO: handle cases where `request.args.get('id', None)` used directly in handler block without method param
        val matchingParameters = parameters.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("self")).l
        val matchingLocals     = locals.code(sourceRule.combinedRulePattern).l
        val matchingLiterals = literals
          .code("(\"|'|`)(" + sourceRule.combinedRulePattern + ")(\"|'|`)")
          .whereNot(_.code(".*\\s.*"))
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

    CollectionUtility.tagMethodEndpoints(
      builder,
      collectionPoints.l,
      collectionRuleInfo,
      ruleCache,
      false,
      methodUrlMap,
      classUrlMap
    )

  }
}
