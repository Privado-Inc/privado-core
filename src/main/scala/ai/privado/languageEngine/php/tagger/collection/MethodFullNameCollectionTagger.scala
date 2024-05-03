package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.utility.Utilities.{resolver, storeForTag}

import scala.collection.mutable

val slimCollectionRule = RuleInfo(
  "Collections.Slim",
  "Slim MVC Endpoints",
  "",
  FilterProperty.METHOD_FULL_NAME,
  Array(),
  List("(?i).*(Route).*"),
  false,
  "",
  Map(),
  NodeType.REGULAR,
  "",
  CatLevelOne.COLLECTIONS,
  catLevelTwo = Constants.annotations,
  Language.PHP,
  Array()
)

class MethodFullNameCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[Call](cpg) {

  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()

  override def generateParts(): Array[Call] = {
    cpg.call.methodFullName("(Slim\\\\.*Routing\\\\RouteCollectorProxy)->(get|post|put|delete|any).*").toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, callNode: Call): Unit = {
    val (path, controllerFullName) = getPathAndControllerFromCall(callNode)

    val controllerName = extractControllerNameFromFullName(controllerFullName.split("::").headOption.getOrElse(""))
    methodUrlMap.addOne(
      (
        cpg.method.fullName(s".*$controllerName->handle.*").headOption match {
          case Some(method) =>
            method.id()
          case None => -1
        },
        path
      )
    )

    // The RuleInfo is intended to be empty
    tagSources(builder, slimCollectionRule, methodUrlMap.flatMap((id, _) => cpg.method.id(id)).toList)
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
    if (call.argument.size > 2)
      if (call.argument(1).isLiteral) {
        (call.argument(1).code.replaceAll(".*\"(.*?)\".*", "$1"), call.argument(2).code)
      } else if (call.argument(1).isCallTo(Operators.fieldAccess)) {
        // Here we possibly don't have a URL directly but a field access call like this: $app->post(self::MY_ROUTE, SomeController::class)
        // We first extract constant identifier MY_ROUTE
        val constantId = call
          .argument(1)
          .isCallTo(Operators.fieldAccess)
          .argument(2)
          .argument(2)
          .isFieldIdentifier
          .canonicalName
          .headOption
          .getOrElse("")
        // Now we check all assignments for this constant and get URL from code there
        // for example, MY_ROUTE = "/route"
        (
          cpg.assignment
            .where(_.argument(1).isIdentifier.nameExact(s"$constantId"))
            .argument(2)
            .code
            .replaceAll(".*\"(.*?)\".*", "$1"),
          call.argument(2).code
        )
      }
    else {
      ("", "")
    }
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
