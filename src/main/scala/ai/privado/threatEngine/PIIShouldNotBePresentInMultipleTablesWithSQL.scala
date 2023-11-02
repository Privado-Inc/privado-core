package ai.privado.threatEngine

import ai.privado.cache.TaggerCache
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.model.exporter.ViolationProcessingModel
import ai.privado.semantic.Language.*
import ai.privado.threatEngine.ThreatUtility.hasDataElements
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{SqlColumnNode, StoredNode}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.collection.mutable
import scala.util.Try

object PIIShouldNotBePresentInMultipleTablesWithSQL {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    taggerCache: TaggerCache
  ): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val taggedSources = getSources(cpg)
      taggedSources.foreach(t => {
        println(t._1)
        t._2.foreach(col => println(col.sqlTable.get.name))
      })
    }
    (false, List())
  }

  def getSources(cpg: Cpg): List[(String, List[SqlColumnNode])] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(Constants.sources)
    }
    def groupBy(node: SqlColumnNode): String = {
      // Using randomUUID in case value is not attached it should be grouped under different
      node.tag.nameExact(Constants.id).value.headOption.getOrElse(UUID.randomUUID().toString)
    }
    cpg.sqlColumn
      .where(filterSources)
      .groupBy(groupBy)
      .l
  }
}
