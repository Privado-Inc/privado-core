package ai.privado.threatEngine

import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{SqlColumnNode, SqlTableNode, StoredNode}
import ai.privado.semantic.language.*
import io.shiftleft.semanticcpg.language.*
import java.util.UUID

class BasePIIThreatEvaluator {

  def getSourceId(node: StoredNode): String = {
    node.tag.nameExact(Constants.id).value.headOption.getOrElse(UUID.randomUUID().toString)
  }

  def getTablesMappedToPIIColumns(cpg: Cpg): List[(SqlTableNode, List[SqlColumnNode])] = {
    def filterSources(traversal: Iterator[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(Constants.sources)
    }

    def groupBy(node: SqlColumnNode): SqlTableNode = {
      node.sqlTable.get
    }

    cpg.sqlColumn
      .where(filterSources)
      .groupBy(groupBy)
      .l
  }
}
