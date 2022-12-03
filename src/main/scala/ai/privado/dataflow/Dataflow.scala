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
 *
 */

package ai.privado.dataflow

import ai.privado.cache
import ai.privado.cache.{AppCache, DataFlowCache, RuleCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, InternalTag, NodeType}
import ai.privado.semantic.Language.finder
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.{Path, _}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Expression, Identifier, StoredNode}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Random, Success, Try}

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  implicit val engineContext: EngineContext =
    EngineContext(semantics = Utilities.getSemantics(cpg), config = EngineConfig(4))

  val falsePositiveSources = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  /** Compute the flow of data from tagged Sources to Sinks
    * @return
    *   \- Map of PathId -> Path corresponding to source to sink path
    */
  def dataflow: Map[String, Path] = {

    logger.info("Generating dataflow")
    val sources = getSources
    val sinks   = getSinks

    if (sources.isEmpty || sinks.isEmpty)
      Map[String, Path]()
    else {
      val dataflowPathsUnfiltered = sinks.reachableByFlows(sources).l
      AppCache.totalFlowFromReachableBy = dataflowPathsUnfiltered.size
      val dataflowPaths = dataflowPathsUnfiltered.filter(filterFlowsByContext).filter(flowNotTaintedByThis)
      AppCache.totalFlowAfterThisFiltering = dataflowPaths.size
      // Stores key -> PathID, value -> Path
      var dataflowMapByPathId = Map[String, Path]()
      if (ScanProcessor.config.disableDeDuplication) {
        dataflowMapByPathId = dataflowPaths
          .flatMap(dataflow => {
            DuplicateFlowProcessor.calculatePathId(dataflow) match {
              case Success(pathId) => Some(pathId, dataflow)
              case Failure(e) =>
                logger.debug("Exception : ", e)
                None
            }
          })
          .toMap
      } else {
        println(s"${Calendar.getInstance().getTime} - Deduplicating data flows...")
        dataflowMapByPathId = DuplicateFlowProcessor.process(dataflowPaths)
      }
      filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId)
      // Need to return the filtered result
      val dataflowFromCache = DataFlowCache.getDataflow
      dataflowFromCache.map(_.pathId).toSet.map((pathId: String) => (pathId, dataflowMapByPathId(pathId))).toMap
    }
  }

  private def getSources: List[CfgNode] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
    }
    cpg.literal
      .where(filterSources)
      .l ++ cpg.identifier
      .where(filterSources)
      .l ++ cpg.call
      .where(filterSources)
      .l ++ cpg.argument.isFieldIdentifier.where(filterSources).l

  }

  private def getSinks: List[CfgNode] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }

  private def filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId: Map[String, Path]) = {

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
  private def filterFlowsBySubCategoryNodeTypeAndStoreInCache(
    dataflowMapByPathId: Map[String, Path],
    sinkSubCategory: String,
    sinkNodetypes: Set[String]
  ) = {
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
        AppCache.totalFlows += 1
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

        def approach1() = {
          // Logic to filter flows which are interfering with the current source item
          // Ex - If traversing flow for email, discard flow which uses password
          var isSourceDEPresent = false
          val otherMatchedRules = new mutable.HashSet[String]()
          val res = dataflowsMapByType(sinkPathId).elements
            .flatMap(pathItem => {
              val matchRes = pathItem.tag.where(_.name(Constants.id).value("Data.Sensitive.*")).value.l
              if (matchRes.nonEmpty) {
                if (matchRes.head.equals(pathSourceId)) {
                  isSourceDEPresent = true
                  Some(false)
                } else {
                  otherMatchedRules.add(matchRes.head)
                  Some(true)
                }
              } else
                Some(false)
            })
            .foldLeft(false)((a, b) => a || b)

          val dataElementBlackList = List(
            "Data.Sensitive.AccountData.AccountID",
            "Data.Sensitive.PurchaseData.OrderDetails",
            "Data.Sensitive.AccountData.LanguagePreferences"
          )

          val blackListElementPresence = otherMatchedRules
            .map(item => dataElementBlackList.contains(item))
            .foldLeft(false)((a, b) => a || b)
          if (res && !isSourceDEPresent && !(blackListElementPresence && dataflowSinkType.equals("storages"))) {
            // discard this flow
            logger.debug(
              s"Discarding the flow for sourceId : $pathSourceId, other matched Data Elements : ${otherMatchedRules
                  .mkString(" || ")}, Sink type : ${dataflowSinkType}"
            )
            logger.debug(s"${dataflowsMapByType(sinkPathId).elements.code.mkString("|||")}")
            logger.debug("----------------------------")
            AppCache.fpByOverlappingDE += 1
          } else // Add this to Cache
            DataFlowCache.setDataflow(
              DataFlowPathModel(pathSourceId, sinkId, dataflowSinkType, dataflowNodeType, sinkPathId)
            )
        }
        def approach2() = {
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
            // if(false){
            logger.debug(
              s"Discarding the flow for sourceId : $pathSourceId, other matched Data Elements : ${matchedDataElement
                  .mkString(" || ")}, Sink type : ${dataflowSinkType}"
            )
            logger.debug(s"${dataflowsMapByType(sinkPathId).elements.code.mkString("|||")}")
            logger.debug("----------------------------")

            logger.debug(
              s"Discarding the flow for sourceId : $pathSourceId, identifier matched Data Elements :" +
                s" ${identifierMatchedDataElement.mkString(" || ")}, call matched Data Elements : ${callMatchedDataElement} Sink type : ${dataflowSinkType}"
            )
            logger.debug(s"${dataflowsMapByType(sinkPathId).elements.code.mkString("|||")}")
            println(s"Derived source was present : ${isDerivedSourcePresent}")
            println(
              s"Derived sources are : ${(identifierInCallArguments ++ identifierArguments.toSet).code.mkString("|||")}"
            )
            logger.debug("----------------------------")
            AppCache.fpByDerivedSourcePresence += 1
            AppCache.fpByOverlappingDE += 1
            AppCache.fpMap.put(dataflowSinkType, AppCache.fpMap.getOrElse(dataflowSinkType, 0) + 1)
          } else { // Add this to Cache

            DataFlowCache.setDataflow(
              DataFlowPathModel(pathSourceId, sinkId, dataflowSinkType, dataflowNodeType, sinkPathId)
            )
          }
        }
        // approach1()
        approach2()
        AppCache.totalMap.put(dataflowSinkType, AppCache.totalMap.getOrElse(dataflowSinkType, 0) + 1)
      }
    }

    if (ScanProcessor.config.disableDeDuplication) {
      sinkPathIds.foreach(sinkPathId => {
        dataflowNodeTypes.foreach(dataflowNodeType => {
          // Add this to Cache
          addToCache(sinkPathId, dataflowNodeType)
        })
      })
    } else {
      distinctBySinkLineNumber(sinkPathIds, dataflowsMapByType).foreach(sinkPathId => {
        dataflowNodeTypes.foreach(dataflowNodeType => {
          // Check to filter if correct Source is consumed in Sink
          /*if (
            isCorrectDataSourceConsumedInSink(
              pathSourceId,
              sinkPathId,
              dataflowsMapByType(sinkPathId),
              dataflowSinkType,
              dataflowNodeType
            )
          ) {

           */
          addToCache(sinkPathId, dataflowNodeType)
          // }
        })
      })
    }
  }

  /** Distinct by operation on sink -> fileName + lineNumber to return a unique path
    * @param sinkPathIds
    * @param dataflowsMapByType
    * @return
    */
  def distinctBySinkLineNumber(sinkPathIds: List[String], dataflowsMapByType: Map[String, Path]): List[String] = {
    val uniqueSinkMap = mutable.HashMap[String, String]() // Stores fileLineNo --> pathId
    sinkPathIds.foreach(sinkPathId => {
      val sinkNodeWithLocation = dataflowsMapByType(sinkPathId).elements.last.location
      val fileLineNo           = sinkNodeWithLocation.lineNumber.getOrElse(0).toString + sinkNodeWithLocation.filename
      val flowSize             = dataflowsMapByType(sinkPathId).elements.size

      if (uniqueSinkMap.contains(fileLineNo)) {
        if (flowSize < dataflowsMapByType(uniqueSinkMap(fileLineNo)).elements.size)
          uniqueSinkMap(fileLineNo) = sinkPathId
      } else
        uniqueSinkMap(fileLineNo) = sinkPathId
    })
    AppCache.groupingByLineNumber += sinkPathIds.size - uniqueSinkMap.values.size
    uniqueSinkMap.values.toList
  }

  /** Check to classify if path Source Id is getting consumed in sink for derived sources
    *
    * @param pathSourceId
    * @param path
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
        .map(argument => argument.matches(RuleCache.getRuleInfo(memberRuleId).get.patterns.head))
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
  private def filterFlowsByContext(flow: Path) = {
    val reversedPath                 = flow.elements.reverse
    var prevThisTypeFullName: String = ""
    var prevThisCode: String         = ""
    var isFlowCorrect                = true
    breakable {
      for (i <- 0 to reversedPath.length - 1) {
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
            if (prevThisTypeFullName.isEmpty) {
              prevThisTypeFullName = currentThisTypeFullName
              prevThisCode = currentThisCode
            } else {
              if (prevThisTypeFullName == currentThisTypeFullName && prevThisCode != currentThisCode) {
                logger.debug(s"Removed Flow due to 'this' tainting: ${flow.elements.code.mkString("||")}")
                isFlowCorrect = false
                break()
              }
            }
          }
        }
      }
    }

    isFlowCorrect
  }

  /** Filter flows which doesn't lead to sink via 'this'
    * @param flow
    * @return
    */
  def flowNotTaintedByThis(flow: Path) = {
    val flowSize = flow.elements.size
    !flow.elements(flowSize - 2).code.equals("this")
  }
}
