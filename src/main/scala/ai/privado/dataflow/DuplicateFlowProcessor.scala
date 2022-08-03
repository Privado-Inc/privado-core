package ai.privado.dataflow

import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory

import java.security.MessageDigest
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DuplicateFlowProcessor {

  private val NODE_PATH_SEPARATOR = "-"

  private val logger = LoggerFactory.getLogger(getClass)

  /** Process the given dataflows and return only the distinct one
    *
    * What do we have - 8 Flows
    *
    * Source 1 to Sink 1, 2
    *
    * Source 2 to Sink 1, 2
    *
    * Source 3 to Sink 1, 2
    *
    * Source 4 to Sink 1, 2
    *
    * What do we want? 2 Flows
    *
    * Source 1 to Sink 1
    *
    * Source 1 to Sink 2
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
    *   Unique dataflows
    */
  def process(dataflows: List[Path]): List[Path] = {
    val distinctDataflows = ListBuffer[Path]()

    val sortedDataflows = dataflows.sorted(Ordering.by((_: Path).elements.size).reverse)
    val visitedFlows    = mutable.HashSet[String]()
    sortedDataflows.foreach(flow => {
      calculatePathId(flow) match {
        case Success(pathId) =>
          if (!visitedFlows.contains(pathId)) {
            distinctDataflows.append(flow)
            val pathSubIds = getSubPathIds(pathId)
            if (pathSubIds.nonEmpty)
              visitedFlows.addAll(pathSubIds)
          }
        case Failure(exception) =>
          logger.debug("Exception : ", exception)
          logger.error(s"Exception while calculating PathId in deduplication of dataflow")
      }
    })
    distinctDataflows.toList
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
}
