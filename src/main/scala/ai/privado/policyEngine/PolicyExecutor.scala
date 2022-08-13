package ai.privado.policyEngine

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, PolicyAction, PolicyOrThreat, PolicyViolationFlowModel}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Tag}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class PolicyExecutor(cpg: Cpg, dataflowMap: Map[String, Path], repoName: String) {

  private val logger = LoggerFactory.getLogger(getClass)

  val ALL_MATCH_REGEX = "**"
  val actionMap       = Map(PolicyAction.ALLOW -> false, PolicyAction.DENY -> true)
  lazy val policies   = RuleCache.getAllPolicy.filter(policy => filterByRepoName(policy, repoName))

  // Map to contain sourceId -> List(pathIds)
  lazy val dataflowSourceIdMap = getDataflowBySourceIdMapping

  // Map to contain sinkId -> List(pathIds)
  lazy val dataflowSinkIdMap = getDataflowBySinkIdMapping

  /** Processes Processing style of policy and returns affected SourceIds
    */
  def getProcessingViolations: Map[String, List[(String, CfgNode)]] = {
    val processingTypePolicy = policies.filter(policy => policy.dataFlow.sinks.isEmpty)
    val processingResult = processingTypePolicy
      .map(policy => (policy.id, getSourcesMatchingRegex(policy).toList.flatMap(sourceId => getSourceNode(sourceId))))
      .toMap
    processingResult
  }

  /** Processes Dataflow style of policy and returns affected SourceIds
    */
  def getDataflowViolations = {

    val dataflowResult = policies
      .map(policy =>
        (
          policy.id, {
            val violatingFlowList = ListBuffer[PolicyViolationFlowModel]()
            val sourceMatchingIds = getSourcesMatchingRegex(policy)
            val sinksMatchingIds  = getSinksMatchingRegex(policy)
            sourceMatchingIds.foreach(sourceId => {
              sinksMatchingIds.foreach(sinkId => {
                val intersectingPathIds = dataflowSourceIdMap(sourceId).intersect(dataflowSinkIdMap(sinkId))
                if (intersectingPathIds.nonEmpty)
                  violatingFlowList.append(PolicyViolationFlowModel(sourceId, sinkId, intersectingPathIds.toList))
              })
            })
            violatingFlowList
          }
        )
      )
      .toMap
    dataflowResult
  }

  /** Filters outs based on Repository name
    */
  private def filterByRepoName(policy: PolicyOrThreat, repoName: String): Boolean = {
    actionMap.get(policy.action) match {
      case Some(value) =>
        value == policy.repositories
          .map(repoPattern => { repoPattern.equals(ALL_MATCH_REGEX) || repoName.matches(repoPattern) })
          .reduce((a, b) => a || b)
      case None => true
    }
  }

  private def getDataflowBySourceIdMapping = {
    val dataflowSourceIdMap = mutable.HashMap[String, ListBuffer[String]]()
    dataflowMap.foreach(entrySet => {
      def addToMap(sourceId: String) = {
        if (!dataflowSourceIdMap.contains(sourceId))
          dataflowSourceIdMap(sourceId) = ListBuffer[String]()
        dataflowSourceIdMap(sourceId).append(entrySet._1)
      }
      try {
        val source = entrySet._2.elements.head
        source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
        source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
      } catch {
        case e: Exception => logger.debug("Exception : ", e)
      }

    })
    dataflowSourceIdMap
  }

  private def getDataflowBySinkIdMapping = {
    val dataflowSinkIdMap = mutable.HashMap[String, ListBuffer[String]]()
    dataflowMap.foreach(entrySet => {
      entrySet._2.elements.last.tag
        .nameExact(Constants.id)
        .value
        .l
        .foreach(sinkId => {
          if (!dataflowSinkIdMap.contains(sinkId))
            dataflowSinkIdMap(sinkId) = ListBuffer[String]()
          dataflowSinkIdMap(sinkId).append(entrySet._1)
        })
    })
    dataflowSinkIdMap
  }

  private def getSourcesMatchingRegex(policy: PolicyOrThreat): Set[String] = {
    policy.dataFlow.sources
      .flatMap(policySourceRegex => {
        if (policySourceRegex.equals(ALL_MATCH_REGEX)) {
          dataflowSourceIdMap.keys
        } else {
          dataflowSourceIdMap.flatMap(sourceIdEntry => {
            if (sourceIdEntry._1.matches(policySourceRegex))
              Some(sourceIdEntry._1)
            else
              None
          })
        }
      })
      .toSet
  }

  private def getSinksMatchingRegex(policy: PolicyOrThreat) = {
    policy.dataFlow.sinks
      .flatMap(policySinkRegex => {
        if (policySinkRegex.equals(ALL_MATCH_REGEX)) {
          dataflowSinkIdMap.keys
        } else {
          dataflowSinkIdMap.flatMap(sinkIdEntry => {
            if (sinkIdEntry._1.matches(policySinkRegex) == actionMap.getOrElse(policy.action, true))
              Some(sinkIdEntry._1)
            else
              None
          })
        }
      })
      .toSet
  }

  private def getSourceNode(sourceId: String): Option[(String, CfgNode)] = {
    def filterBySource(tag: Traversal[Tag]): Traversal[Tag] =
      tag.where(_.nameExact(Constants.id)).where(_.valueExact(sourceId))
    Try(cpg.tag.where(filterBySource).identifier.head) match {
      case Success(identifierNode) => Some(sourceId, identifierNode)
      case Failure(_) =>
        Try(cpg.tag.where(filterBySource).literal.head) match {
          case Success(literalNode) => Some(sourceId, literalNode)
          case Failure(_) =>
            Try(cpg.tag.where(filterBySource).call.head) match {
              case Success(callNode) => Some(sourceId, callNode)
              case Failure(_)        => None
            }
        }
    }

  }
}
