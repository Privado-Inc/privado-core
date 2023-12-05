package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.exporter.{
  AndroidPermissionDetailModel,
  AndroidPermissionModel,
  DataFlowSubCategoryPathExcerptModel
}
import ai.privado.semantic.Language.NodeStarterForAndroidXmlPermissionNode
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable.ListBuffer

class AndroidPermissionsExporter(cpg: Cpg, ruleCache: RuleCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  def getPermissions: List[AndroidPermissionModel] = {
    val permissions = ListBuffer[AndroidPermissionModel]()
    try {
      // take only those nodes that have source tags
      cpg.androidXmlPermissionNode
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(Constants.sources))
        .foreach(node => {
          permissions.addOne(
            AndroidPermissionModel(
              permissionType = node.permissionType,
              isUsed = true, // we consider permission to be used always
              permissionDetail = getPermissionDetail(node)
            )
          )
        })
    } catch {
      case e: Exception => logger.debug("Exception : ", e)
    }
    permissions.toList
  }

  private def getPermissionDetail(node: AndroidXmlPermissionNode): AndroidPermissionDetailModel = {
    AndroidPermissionDetailModel(
      sourceId = node.tag.nameExact(Constants.id).value.headOption.getOrElse(""),
      occurrence = ExporterUtility.convertIndividualPathElement(node).get
    )
  }
}
