package ai.privado.languageEngine.python.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.collections.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val methodUrlMap = mutable.HashMap[Long, String]()
    def getFinalEndPoint(collectionPoint: Method): String = {
      methodUrlMap.getOrElse(collectionPoint.id(), "")
    }

    // TODO: Currently matches basic stuff like "/user/id", "/" etc. Possibly improve
    val urlPattern = ".*(\\\"|')(\\/.*?)(\\\"|').*"

    /*
    Python frontend currently populates decorated route handlers as methodRef nodes
    For example, in the following case,

       @bp.route("/user/add", methods=["POST", "GET"])
       def create_user():
          ...

    frontend creates a call node with its 'code' string as 'create_user = bp.route(\"/user/add\" ..)'
    In order to create a proper pair of handler and its attached route, we need to navigate the AST
    from the methodRef node, find the relevant call node and then extract route info from these methodRef
    nodes and and begin tagging.
    */
    val collectionMethodRefs = cpg.methodRef
      .where(_.astParent
        .astParent
        .isCall
        .argument
        .code(urlPattern)
      )

    val collectionMethodsCache = collectionMethodRefs.map{ m =>
      methodUrlMap.addOne(
        // we only get methodFullName here from the call node, so we have to get the relevant method for key
        cpg.method.fullNameExact(m.methodFullName).l.head.id() ->
        getRoute(m.astParent.astParent.where(_.isCall).head.asInstanceOf[Call].argument.isCall.code.head)
      )
      cpg.method.fullNameExact(m.methodFullName).l
    }.l.flatten(method => method) // returns the handler method list

    val collectionPoints = Traversal(collectionMethodsCache).flatMap(collectionMethod => {
      sourceRuleInfos.flatMap(sourceRule => {
        // TODO: handle cases where `request.args.get('id', None)` used directly in handler block without method param
        val parameters =
          collectionMethod.parameter.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("self")).l
        if (parameters.isEmpty) {
          None
        } else {
          parameters.foreach(parameter => storeForTag(builder, parameter)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        }
      })
    })

    collectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, ruleInfo)
      storeForTag(builder, collectionPoint)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint)
      )
    })

  }

  /** Returns the route extracted from the code
   */
  private def getRoute(code: String): String = {
    val regex = """\((\"|\')(/.*?)(\"|\')""".r
    Try(regex.findFirstMatchIn(code).map(_.group(2))) match {
      case Success(url) => url.get
      case Failure(e) =>
        logger.debug("Exception : ", e)
        ""
    }
  }
}