package ai.privado.languageEngine.python.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger       = LoggerFactory.getLogger(this.getClass)
  private val methodUrlMap = mutable.HashMap[Long, String]()
  private val classUrlMap  = mutable.HashMap[Long, String]()

  def getCollectionUrls(): List[String] =
    CollectionUtility.getCollectionUrls(cpg, methodUrlMap, classUrlMap)

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {

    // TODO: Currently matches basic stuff like "/user/id", "/" etc. Possibly improve
    val urlPattern = ".*(\\\"|')(\\/.*?)(\\\"|').*"

    /*
    Python frontend currently populates decorated route handlers as methodRef nodes
    For example, in the following case,

       @bp.route("/user/add", methods=["POST", "GET"])
       def create_user():
          ...

       create_user = @bp.route("/user/add", methods=["POST", "GET"])

    frontend creates a call node with its 'code' string as 'create_user = bp.route(\"/user/add\" ..)'
    In order to create a proper pair of handler and its attached route, we need to navigate the AST
    from the methodRef node, find the relevant call node and then extract route info from these methodRef
    nodes and and begin tagging.
     */
    val collectionMethodRefs = cpg.methodRef.where(_.astParent.astParent.isCall.argument.code(urlPattern))

    var collectionMethodsCache = collectionMethodRefs
      .map { m =>
        methodUrlMap.addOne(
          // we only get methodFullName here from the call node, so we have to get the relevant method for key
          cpg.method.fullNameExact(m.methodFullName).l.head.id() ->
            getRoute(
              m.astParent.astParent.iterator
                .where(_.isCall)
                .head
                .asInstanceOf[Call]
                .argument
                .isCall
                .code
                .headOption
                .getOrElse("")
            )
        )
        cpg.method.fullNameExact(m.methodFullName).l
      }
      .l
      .flatten(method => method) // returns the handler method list
    /*
    Pattern: django.*[.](url|path)
    Method Full Name starts with django and ends with .url or .path
     */
    val djangoCollectionRedirectCalls = cpg.call.methodFullName("django.*[.](url|path)").l
    val djangoCollectionMethods       = mutable.ListBuffer[Method]()

    for (call <- djangoCollectionRedirectCalls) {
      if (!(call.isArgument.isEmpty || call.argument.isMethodRef.l.isEmpty)) {
        methodUrlMap.addOne(call.argument.isMethodRef.head.referencedMethod.id() -> call.argument.isLiteral.head.code)
        djangoCollectionMethods += call.argument.isMethodRef.head.referencedMethod
      }
    }

    collectionMethodsCache = collectionMethodsCache ::: djangoCollectionMethods.toList

    tagDirectSources(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
    tagDerivedSources(cpg, builder, collectionMethodsCache.l, collectionRuleInfo)
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
        val literals   = collectionMethod.call("(?:get).*").argument.isLiteral

        // TODO: handle cases where `request.args.get('id', None)` used directly in handler block without method param
        val matchingParameters = parameters.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("self")).l
        val matchingLocals     = locals.code(sourceRule.combinedRulePattern).l
        val matchingLiterals = literals
          .code("(\"|')(" + sourceRule.combinedRulePattern + ")(\"|')")
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

    tagMethodEndpoints(builder, collectionPoints.l, collectionRuleInfo)
  }

  def tagDerivedSources(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    collectionRuleInfo: RuleInfo
  ): Unit = {
    // Implementation to also mark the collection points which use derived type declaration as there parameters
    val derivedTypeDecl = (getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE.toString) ++
      getAllDerivedTypeDecl(cpg, InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE.toString)).toSet

    val collectionPointsFromDerivedTypeDecl = collectionMethods.flatMap(collectionMethod => {
      var parameters = collectionMethod.parameter.l
      parameters = collectionMethod.parameter
        .where(_.typeFullName.filter(fullName => !derivedTypeDecl.contains(fullName)))
        .l

      if (parameters.isEmpty) {
        None
      } else {
        // Have removed the earlier code, where we were fetching all the referencing identifiers of parameter and then tagging, because we were missing on cases where the parameter is not used in the code
        parameters
          .whereNot(_.code("this"))
          .foreach(parameter => {
            parameter.tag
              .name(Constants.privadoDerived + ".*")
              .foreach(refTag => storeForTag(builder, parameter, ruleCache)(refTag.name, refTag.value))
          })
        collectionMethod
      }
    })

    tagMethodEndpoints(builder, collectionPointsFromDerivedTypeDecl.l, collectionRuleInfo)
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

  private def getAllDerivedTypeDecl(cpg: Cpg, objectName: String) = {
    cpg.identifier.where(_.tag.name(objectName)).typeFullName.dedup.l
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

  /** Returns the route extracted from the code
    */
  private def getRoute(code: String): String = {
    val regex = """\((\"|\')(/.*?)(\"|\')""".r
    Try(regex.findFirstMatchIn(code).map(_.group(2))) match {
      case Success(url) => if (url == None) "" else url.get
      case Failure(e) =>
        logger.debug("Exception : ", e)
        ""
    }
  }
}
