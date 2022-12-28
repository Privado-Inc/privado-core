/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.dataflow

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, NodeType}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Expression, Identifier}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks.{break, breakable}

object DuplicateFlowProcessor {

  private val NODE_PATH_SEPARATOR = "-"

  private val logger = LoggerFactory.getLogger(getClass)

  val falsePositiveSources = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  /** Process the given dataflows and return only the distinct one
    *
    * What do we want - 8 Flows
    *
    * Source 1 to Sink 1, 2
    *
    * Source 2 to Sink 1, 2
    *
    * Source 3 to Sink 1, 2
    *
    * Source 4 to Sink 1, 2
    *
    * Algorithmic idea
    *
    *   1. Sort by length in descending order
    *
    * 2. for each path
    *
    * 2.1. calculate id 2.2. check for distinct (set) 2.2.1. if not distinct, discard 2.3. add all possible subpaths to
    * set
    *
    * @param dataflows
    * @return
    *   Unique dataflows with PathId
    */
  /*
  def process(dataflows: List[Path]): Map[String, Path] = {
    // Stores pathId -> Path
    val dataflowMap = dataflows.map(path => (calculatePathId(path).getOrElse(""), path)).toMap
    // Stores sourceId -> Set(pathIds)
    val dataflowMapBySourceId = mutable.HashMap[String, mutable.Set[String]]()
    dataflowMap.foreach(dataflowEntry => {
      def addToMap(sourceId: String) = {
        if (!dataflowMapBySourceId.contains(sourceId))
          dataflowMapBySourceId.addOne(sourceId -> mutable.Set())
        dataflowMapBySourceId(sourceId).add(dataflowEntry._1)
      }
      val sourceNode = dataflowEntry._2.elements.head
      sourceNode.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
      sourceNode.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
    })
    dataflowMapBySourceId
      .flatMap(dataflowMapBySourceIdEntrySet => pathIdsPerSourceIdAfterDedup(dataflowMapBySourceIdEntrySet._2.toSet))
      .toSet
      .map((pathId: String) => (pathId, dataflowMap(pathId)))
      .toMap
  }

   */

  /** Filter unique path ids which are super set of overlapping paths
    *
    * @param pathIds
    * @return
    */
  def pathIdsPerSourceIdAfterDedup(pathIds: Set[String]) = {
    val visitedFlows = mutable.Set[String]()
    pathIds.foreach(pathId => {
      if (!visitedFlows.contains(pathId)) {
        val pathSubIds = getSubPathIds(pathId)
        if (pathSubIds.nonEmpty)
          visitedFlows.addAll(pathSubIds)
      }
    })
    pathIds.diff(visitedFlows) // This will give us all the Path ids which are super set of overlapping paths
  }

  /** Generates a pathId for a given path, based on node Id
    * @param flow
    * @return
    */
  def calculatePathId(flow: Path) = Try {
    flow.elements.map(node => node.id()).mkString(NODE_PATH_SEPARATOR)
  }

  /** Returns all the sub pathIds for a given path Id Ex - For path Id - 121-23-47-1999-2143-8 SubpathIds are -
    * 23-47-1999-2143-8, 47-1999-2143-8, 1999-2143-8, 2143-8
    * @param pathId
    * @return
    */
  private def getSubPathIds(pathId: String) = {
    val subIdList = ListBuffer[String]()
    val pathIds   = pathId.split(NODE_PATH_SEPARATOR)
    for (i <- 1 until (pathIds.size - 1)) {
      subIdList.append(pathIds.slice(i, pathIds.size).mkString(NODE_PATH_SEPARATOR))
    }
    subIdList.toList
  }

  /** Filter out flows which are irrelevant and store the result in DataFlowCache
    * @param dataflowMapByPathId
    *   \- map containing pathId -> path
    */
  def filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId: Map[String, Path]): Unit = {

    val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
    RuleCache.getRule.sinks.foreach(sinkRule => {
      if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
        sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
      sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
    })

    sinkSubCategories.foreach(sinkSubTypeEntry =>
      filterFlowsBySubCategoryNodeTypeAndStoreInCache(
        dataflowMapByPathId,
        sinkSubTypeEntry._1,
        sinkSubTypeEntry._2.toSet
      )
    )
  }

  /** Helper function to filter flows by sub-category and node type and store in cache
    * @param dataflowMapByPathId
    *   \- map containing pathId -> path
    * @param sinkSubCategory
    *   \- sinkSubCategory - Ex - leakages, third-parties
    * @param sinkNodetypes
    *   \- REGULAR, API etc
    */
  private def filterFlowsBySubCategoryNodeTypeAndStoreInCache(
    dataflowMapByPathId: Map[String, Path],
    sinkSubCategory: String,
    sinkNodetypes: Set[String]
  ): Unit = {
    val dataflowsMapByType = dataflowMapByPathId.filter(dataflowEntrySet =>
      dataflowEntrySet._2.elements.last
        .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(sinkSubCategory))
        .nonEmpty
    )
    // Metric for sinkSubCategory size
    MetricHandler.flowCategoryData(sinkSubCategory) = dataflowsMapByType.size

    // Store sourceId -> List[PathIds] Paths which have sourceId as the source
    val dataflowsMapBySourceId = mutable.HashMap[String, ListBuffer[String]]()
    dataflowsMapByType.foreach(entrySet => {
      def addToMap(sourceId: String) = {
        if (!dataflowsMapBySourceId.contains(sourceId))
          dataflowsMapBySourceId.addOne(sourceId, ListBuffer())
        dataflowsMapBySourceId(sourceId) += entrySet._1
      }

      val source = entrySet._2.elements.head
      try {
        source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
        source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
      } catch {
        case e: Exception => logger.debug("Exception while fetching sourceId in dataflow : ", e)
      }
    })

    dataflowsMapBySourceId.foreach(flow => {
      filterSinkListAndStoreInCache(flow._1, flow._2.toList, dataflowsMapByType, sinkSubCategory, sinkNodetypes)
    })
  }

  /** Helper function to filter all sink flows for a given sourceID
    * @param pathSourceId
    *   \- sourceId of the path
    * @param sinkPathIds
    *   \- path ids of all flows for the corresponding sourceID
    * @param dataflowsMapByType
    *   \- Map containing pathId -> path
    * @param dataflowSinkType
    *   \- Ex - leakages, third-parties
    * @param dataflowNodeTypes
    *   \- REGULAR, API etc
    */
  private def filterSinkListAndStoreInCache(
    pathSourceId: String,
    sinkPathIds: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String,
    dataflowNodeTypes: Set[String]
  ) = {

    def addToCache(sinkPathId: String, dataflowNodeType: String) = {
      val sinkCatLevelTwoCustomTag = dataflowsMapByType(sinkPathId).elements.last.tag
        .filter(node => node.name.equals(dataflowSinkType + dataflowNodeType))
      if (sinkCatLevelTwoCustomTag.nonEmpty) {
        val sinkId = sinkCatLevelTwoCustomTag.head.value
        def filterFlowsOverlappingWithOtherDataElement() = {
          // Logic to filter flows which are interfering with the current source item
          // Ex - If traversing flow for email, discard flow which uses password

          // approach 2.1
          val matchedDataElement = mutable.HashSet[String]()
          breakable {
            dataflowsMapByType(sinkPathId).elements.reverse
              .foreach(pathItem => {
                val matchRes = pathItem.tag
                  .where(_.value("Data.Sensitive.*"))
                  .value
                  .l
                if (matchRes.nonEmpty) {
                  matchedDataElement.addAll(matchRes)
                  break()
                }
              })
          }

          val isFpByApproach1 = !matchedDataElement.contains(pathSourceId) && !dataflowSinkType.equals("storages")

          // approach 2.2
          val sinkNode  = dataflowsMapByType(sinkPathId).elements.isCall.last
          val arguments = sinkNode.argument.filter(_.argumentIndex > 0).l

          val identifierArguments       = arguments.isIdentifier.l
          var callArguments             = arguments.isCall.l
          val identifierInCallArguments = mutable.HashSet[Identifier]()
          var recCnt                    = 0
          var callArgumentForIdentifier = arguments.isCall.l
          while (callArgumentForIdentifier.argument.isIdentifier.nonEmpty && recCnt < 3) {
            identifierInCallArguments.addAll(callArgumentForIdentifier.argument.isIdentifier.toSet)
            callArgumentForIdentifier = callArgumentForIdentifier.argument.isCall.l
            recCnt += 1
          }
          val isDerivedSourcePresent = (identifierInCallArguments ++ identifierArguments.toSet).tag
            .where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.DERIVED_SOURCES.name))
            .nonEmpty

          val identifierMatchedDataElement =
            identifierArguments.tag.where(_.value("Data.Sensitive.*")).value.toSet

          val callMatchedDataElement = mutable.HashSet[String]()
          recCnt = 0
          while (callArguments.nonEmpty && recCnt < 3) {
            callMatchedDataElement.addAll(callArguments.tag.where(_.value("Data.Sensitive.*")).value.l)
            callArguments = callArguments.argument.isCall.l
            recCnt += 1
          }
          val finalMatchedDataElement = identifierMatchedDataElement ++ callMatchedDataElement

          val isFpByApproach2 = !finalMatchedDataElement
            .contains(pathSourceId) && !dataflowSinkType.equals("storages")

          if (isFpByApproach1 && isFpByApproach2) {
            logger.debug(
              s"Discarding the flow for sourceId : $pathSourceId, other matched Data Elements : ${matchedDataElement
                  .mkString(" || ")}, identifier matched Data Elements :" +
                s" ${identifierMatchedDataElement.mkString(" || ")}, call matched Data Elements : ${callMatchedDataElement} Sink type : ${dataflowSinkType} Derived source was present : ${isDerivedSourcePresent}"
            )
            logger.debug(
              s"Derived sources are : ${(identifierInCallArguments ++ identifierArguments.toSet).code.mkString("|||")}"
            )
            logger.debug(s"${dataflowsMapByType(sinkPathId).elements.code.mkString("|||")}")
            logger.debug("----------------------------")
            AppCache.fpByOverlappingDE += 1
            AppCache.fpMap.put(dataflowSinkType, AppCache.fpMap.getOrElse(dataflowSinkType, 0) + 1)
          } // Add this to Cache
          else if (
            isCorrectDataSourceConsumedInSink(
              pathSourceId,
              sinkPathId,
              dataflowsMapByType(sinkPathId),
              dataflowSinkType,
              dataflowNodeType
            )
          )
            DataFlowCache.setDataflow(
              DataFlowPathModel(pathSourceId, sinkId, dataflowSinkType, dataflowNodeType, sinkPathId)
            )
        }
        if (ScanProcessor.config.disableFlowSeparationByDataElement)
          DataFlowCache.setDataflow(
            DataFlowPathModel(pathSourceId, sinkId, dataflowSinkType, dataflowNodeType, sinkPathId)
          )
        else
          filterFlowsOverlappingWithOtherDataElement()
        AppCache.totalMap.put(dataflowSinkType, AppCache.totalMap.getOrElse(dataflowSinkType, 0) + 1)
      }
    }

    sinkPathIds.foreach(sinkPathId => {
      dataflowNodeTypes.foreach(dataflowNodeType => {
        // Add this to Cache
        addToCache(sinkPathId, dataflowNodeType)
      })
    })
  }

  /** Check to classify if correct path Source Id is getting consumed in sink for derived sources
    *
    * @param pathSourceId
    *   \- source id of the given path
    * @param path
    *   \- dataflow path
    * @return
    */
  def isCorrectDataSourceConsumedInSink(
    pathSourceId: String,
    pathId: String,
    path: Path,
    dataflowSinkType: String,
    dataflowNodeType: String
  ): Boolean = {

    var isCorrect = true // By default we set it to true, mark it false if matches some condition

    if (dataflowSinkType.equals("leakages")) {
      val sourceNode      = path.elements.head
      val derivedByIdTags = sourceNode.tag.name(Constants.privadoDerived + ".*").map(tg => (tg.name, tg.value)).l
      if (derivedByIdTags.nonEmpty) {
        val disallowedMemberNameList = ListBuffer[String]()
        val sinkCode                 = path.elements.last.code
        val sinkArguments            = getArgumentListByRegex(sinkCode)
        val isCorrectFlowFromExemptedMembers = derivedByIdTags
          .filter(derivedByTag => !derivedByTag._2.equals(pathSourceId))
          .map(derivedByTag => {
            getMemberNameForDerivedSourceNode(derivedByTag, sourceNode) match {
              case Some((memberName, memberRuleId)) =>
                disallowedMemberNameList.append(memberName)
                !(isArgumentMatchingMemberName(sinkArguments, memberName) || isArgumentMatchingMemberPattern(
                  sinkArguments,
                  memberRuleId
                ))
              case None => true
            }
          })
          .foldLeft(true)((a, b) => a && b)

        if (!isCorrectFlowFromExemptedMembers) {
          var allowedMemberName = ""
          val isCorrectFlowFromSourceMember = derivedByIdTags
            .filter(derivedByTag => derivedByTag._2.equals(pathSourceId))
            .map(derivedByIdTag => {
              getMemberNameForDerivedSourceNode(derivedByIdTag, sourceNode) match {
                case Some((memberName, memberRuleId)) =>
                  allowedMemberName = memberName
                  isArgumentMatchingMemberName(sinkArguments, memberName) || isArgumentMatchingMemberPattern(
                    sinkArguments,
                    memberRuleId
                  )
                case None => false
              }
            })
            .foldLeft(true)((a, b) => a && b)
          if (isCorrectFlowFromSourceMember) {
            logger.debug(
              s"Flow Removal : Flow removal overturned by correct SourceMember presence " +
                s"\nallowedMemberName : $allowedMemberName " +
                s"\npathSourceId : $pathSourceId " +
                s"\npathId : $pathId " +
                s"\nlogStatement : $sinkCode"
            )
          } else {
            logger.debug(
              s"Flow Removal : Flow marked incorrect due to presence of other member" +
                s"\ndisallowedMembers : $disallowedMemberNameList" +
                s"\nallowedMemberName : $allowedMemberName " +
                s"\npathSourceId : $pathSourceId " +
                s"\npathId : $pathId " +
                s"\nlogStatement : $sinkCode"
            )
            isCorrect = false
          }
        }
      }
    } else if (dataflowSinkType.equals("third_parties") || dataflowNodeType.equals(NodeType.API.toString)) {
      // To explicity remove Sources which result in FP
      if (falsePositiveSources.contains(pathSourceId))
        isCorrect = false
    }
    isCorrect
  }

  /** Fetch the member name for the given soureNode
    *
    * @param derivedByTag
    *   tag (name, value) of privadoDerived tag for derived objects
    * @param sourceNode
    * @return
    */
  private def getMemberNameForDerivedSourceNode(derivedByTag: (String, String), sourceNode: CfgNode) = {
    val tagName  = derivedByTag._1
    val tagValue = derivedByTag._2
    sourceNode.tag.nameExact(tagValue + Constants.underScore + tagName).value.l.headOption match {
      case Some(memberName) => Some((memberName, tagValue))
      case None             => None
    }
  }

  /** Apply regex to fetch the arguments in the given source code of a sink
    * @param sinkCode
    * @return
    */
  private def getArgumentListByRegex(sinkCode: String): List[String] = {
    sinkCode.split("[(,+]").map(argument => argument.strip().replaceAll("\".*\"", "")).filter(_.nonEmpty).toList
  }

  /** Verify if pattern of matched Member, matches any of the argument name by regex
    *
    * @param sinkArgument
    * @param memberRuleId
    * @return
    */
  private def isArgumentMatchingMemberPattern(sinkArgument: List[String], memberRuleId: String): Boolean = {
    Try(
      sinkArgument
        .map(argument => argument.matches(RuleCache.getRuleInfo(memberRuleId).get.combinedRulePattern))
        .foldLeft(false)((a, b) => a || b)
    ) match {
      case Success(result) => result
      case Failure(_)      => false
    }
  }

  /** Verify if Member name matches any of the argument name by regex
    *
    * @param sinkArgument
    * @param memberName
    * @return
    */
  private def isArgumentMatchingMemberName(sinkArgument: List[String], memberName: String): Boolean = {
    sinkArgument.map(argument => argument.matches("(?i).*" + memberName + ".*")).foldLeft(false)((a, b) => a || b)
  }

  /** Filters the flow where 'this' is overtainting
    *
    * @param flow:
    *   incoming flow that needs to be checked
    * @return
    *   bool: if the flow should be removed or not
    */
  def filterFlowsByContext(flow: Path): Boolean = {
    val reversedPath                          = flow.elements.reverse
    val observedThisTypeFullNameAndIdentifier = mutable.HashMap[String, String]()
    var isFlowCorrect                         = true
    breakable {
      for (i <- reversedPath.indices) {
        val node = reversedPath(i)
        if (node.isCall) {
          val traversalNode = Traversal(node).isCall.l
          var thisNode      = List[Expression]()

          if (traversalNode.name.headOption.getOrElse("") == Operators.fieldAccess)
            thisNode = traversalNode.argument.where(_.argumentIndex(1)).code("this").l
          else
            thisNode = traversalNode.argument.where(_.argumentIndex(0)).code("this").l

          if (thisNode.nonEmpty) {
            val currentThisTypeFullName = thisNode.isIdentifier.typeFullName.headOption.getOrElse("")
            val currentThisCode         = traversalNode.code.headOption.getOrElse("")
            observedThisTypeFullNameAndIdentifier.get(currentThisTypeFullName) match {
              case Some(prevThisCode) =>
                if (prevThisCode != currentThisCode) {
                  logger.debug(s"Removed Flow due to 'this' tainting: ${flow.elements.code.mkString("||")}")
                  isFlowCorrect = false
                  break()
                }
              case _ => observedThisTypeFullNameAndIdentifier(currentThisTypeFullName) = currentThisCode
            }
          }
        }
      }
    }
    isFlowCorrect
  }

  /** Filter flows which doesn't lead to sink via 'this'
    * @param flow
    *   \- the path to be analyzed
    * @return
    */
  def flowNotTaintedByThis(flow: Path): Boolean = {
    val flowSize = flow.elements.size
    !flow.elements(flowSize - 2).code.equals("this")
  }
}
