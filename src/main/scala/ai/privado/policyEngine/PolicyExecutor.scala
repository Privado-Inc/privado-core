package ai.privado.policyEngine

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, PolicyOrThreat, PolicyAction, PolicyViolationFlowModel}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class PolicyExecutor(dataflowMap: Map[String, Path], repoName: String) {

  val ALL_MATCH_REGEX = "**"
  val actionMap       = Map(PolicyAction.ALLOW -> false, PolicyAction.DENY -> true)
  lazy val policies   = RuleCache.getAllPolicy.filter(policy => filterByRepoName(policy, repoName))

  // Map to contain sourceId -> List(pathIds)
  lazy val dataflowSourceIdMap = getDataflowBySourceIdMapping

  // Map to contain sinkId -> List(pathIds)
  lazy val dataflowSinkIdMap = getDataflowBySinkIdMapping

  /** Processes Processing style of policy and returns affected SourceIds
    */
  def getProcessingViolations: Map[String, Set[String]] = {
    val processingTypePolicy = policies.filter(policy => policy.dataFlow.sinks.isEmpty)
    val processingResult = processingTypePolicy
      .map(policy => (policy.id, getSourcesMatchingRegex(policy)))
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
      val source = entrySet._2.elements.head
      if (source.tag.nameExact(Constants.catLevelOne).value.head.equals(CatLevelOne.SOURCES.name)) {
        addToMap(source.tag.nameExact(Constants.id).l.head.value)
      } else {
        source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
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
}
