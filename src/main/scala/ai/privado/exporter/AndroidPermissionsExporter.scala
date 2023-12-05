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

import scala.collection.mutable
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
          val detail = getPermissionDetail(node).get
          if (detail.occurrences.nonEmpty) {
            permissions.addOne(
              AndroidPermissionModel(
                permissionType = node.permissionType,
                isUsed = true, // we consider permission to be used always
                permissionDetail = detail
              )
            )
          }
        })
    } catch {
      case e: Exception => logger.debug("Exception : ", e)
    }
    // group and merge
    val groupedPermissions = permissions
      .groupBy(_.permissionType)
      .map { case (permTypes, permModels) =>
        val combinedPathElements = permModels.flatMap(_.permissionDetail.occurrences).distinct
        val combinedDetailModel =
          AndroidPermissionDetailModel(permModels.head.permissionDetail.sourceId, combinedPathElements.toList)
        AndroidPermissionModel(permTypes, permModels.head.isUsed, combinedDetailModel)
      }
      .toList
    groupedPermissions
  }

  private def getPermissionDetail(node: AndroidXmlPermissionNode): Some[AndroidPermissionDetailModel] = {
    Some(
      AndroidPermissionDetailModel(
        node.tag.nameExact(Constants.id).value.headOption.getOrElse(""),
        ExporterUtility.convertIndividualPathElement(node) match {
          case Some(pathElement) => List(pathElement)
          case None              => List()
        }
      )
    )
  }
}
