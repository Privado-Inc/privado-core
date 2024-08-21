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

package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.exporter.{
  AndroidPermissionDetailModel,
  AndroidPermissionModel,
  DataFlowSubCategoryPathExcerptModel
}
import ai.privado.semantic.language.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AndroidPermissionsExporter(
  cpg: Cpg,
  ruleCache: RuleCache,
  repoItemTagName: Option[String] = None,
  appCache: AppCache
) {

  private val logger = LoggerFactory.getLogger(getClass)

  def getPermissions: List[AndroidPermissionModel] = {
    val permissions = ListBuffer[AndroidPermissionModel]()
    try {
      // take only those nodes that have source tags
      ExporterUtility
        .filterNodeBasedOnRepoItemTagName(
          cpg.androidXmlPermissionNode
            .where(_.tag.nameExact(Constants.catLevelOne).valueExact(Constants.sources))
            .l,
          repoItemTagName
        )
        .collectAll[AndroidXmlPermissionNode]
        .foreach(node => {
          getPermissionDetail(node) match
            case Some(permissionDetail) =>
              permissions.addOne(
                AndroidPermissionModel(
                  permissionType = node.permissionType,
                  isUsed = true, // we consider permission to be used always
                  permissionDetail = permissionDetail
                )
              )
            case None =>
        })
    } catch {
      case e: Exception =>
        logger.debug("Exception : ", e)
        logger.error("Exception caught : ", e.getMessage)
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

  private def getPermissionDetail(node: AndroidXmlPermissionNode): Option[AndroidPermissionDetailModel] = {
    ExporterUtility.convertIndividualPathElement(node, appCache = appCache, ruleCache = ruleCache) match {
      case Some(pathElement) =>
        Some(
          AndroidPermissionDetailModel(
            node.tag.nameExact(Constants.id).value.headOption.getOrElse(""),
            List(pathElement)
          )
        )
      case None => None
    }
  }
}
