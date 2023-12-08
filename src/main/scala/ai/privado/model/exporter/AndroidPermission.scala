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

package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import ai.privado.model.exporter.DataFlowEncoderDecoder._

case class AndroidPermissionModel(
  permissionType: String,
  isUsed: Boolean,
  permissionDetail: AndroidPermissionDetailModel
)

case class AndroidPermissionDetailModel(sourceId: String, occurrences: List[DataFlowSubCategoryPathExcerptModel])

object AndroidPermissionsEncoderDecoder {

  implicit val androidPermissionModelDecoder: Decoder[AndroidPermissionModel] = deriveDecoder[AndroidPermissionModel]
  implicit val androidPermissionModelEncoder: Encoder[AndroidPermissionModel] = deriveEncoder[AndroidPermissionModel]

  implicit val androidPermissionDetailModelDecoder: Decoder[AndroidPermissionDetailModel] =
    deriveDecoder[AndroidPermissionDetailModel]
  implicit val androidPermissionDetailModelEncoder: Encoder[AndroidPermissionDetailModel] =
    deriveEncoder[AndroidPermissionDetailModel]

}
