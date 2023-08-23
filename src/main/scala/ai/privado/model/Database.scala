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

package ai.privado.model

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}

import scala.collection.immutable.HashMap

case class DatabaseSchema(kind: String, projectName: String, platform: String, tables: List[DatabaseTable])
case class DatabaseTable(name: String, description: String, columns: List[DatabaseColumn])
case class DatabaseColumn(name: String, description: String, datatype: String, sourceId: String)

case class DatabaseDetails(
  dbName: String,
  dbVendor: String,
  dbLocation: String,
  dbOperation: String,
  configFile: String,
  schema: Option[DatabaseSchema] = None
)

object DatabaseEncoderDecoder {
  implicit val databaseDetailsDecoder: Decoder[DatabaseDetails] =
    deriveDecoder[DatabaseDetails]
  implicit val databaseDetailsEncoder: Encoder[DatabaseDetails] =
    deriveEncoder[DatabaseDetails]

  implicit val databaseSchemaDecoder: Decoder[DatabaseSchema] =
    deriveDecoder[DatabaseSchema]
  implicit val databaseSchemaEncoder: Encoder[DatabaseSchema] =
    deriveEncoder[DatabaseSchema]

  implicit val databaseTableDecoder: Decoder[DatabaseTable] =
    deriveDecoder[DatabaseTable]
  implicit val databaseTableEncoder: Encoder[DatabaseTable] =
    deriveEncoder[DatabaseTable]

  implicit val databaseColumnDecoder: Decoder[DatabaseColumn] =
    deriveDecoder[DatabaseColumn]
  implicit val databaseColumnEncoder: Encoder[DatabaseColumn] =
    deriveEncoder[DatabaseColumn]
}


//
//import io.circe.Decoder.Result
//import io.circe.{Decoder, Encoder}
//import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
//import ai.privado.model.exporter.DataFlowEncoderDecoder._
//
//import scala.collection.immutable.HashMap
//
//case class DatabaseSchema(kind: String, projectName: String, platform: String, tables: List[DatabaseTable])
//case class DatabaseTable(name: String, description: String, columns: List[DatabaseColumn])
//case class DatabaseColumn(name: String, description: String, datatype: String, sourceId: String)
//
//case class DatabaseDetails(
//  dbName: String,
//  dbVendor: String,
//  dbLocation: String,
//  dbOperation: String,
//  configFile: String,
//  schema: Option[DatabaseSchema] = None
//)
//
//implicit val databaseDetailsDecoder: Decoder[DatabaseDetails] =
//  deriveDecoder[DatabaseDetails]
//implicit val databaseDetailsEncoder: Encoder[DatabaseDetails] =
//  deriveEncoder[DatabaseDetails]
//
//implicit val databaseSchemaDecoder: Decoder[DatabaseSchema] =
//  deriveDecoder[DatabaseSchema]
//implicit val databaseSchemaEncoder: Encoder[DatabaseSchema] =
//  deriveEncoder[DatabaseSchema]
//
//implicit val databaseTableDecoder: Decoder[DatabaseTable] =
//  deriveDecoder[DatabaseTable]
//implicit val databaseTableEncoder: Encoder[DatabaseTable] =
//  deriveEncoder[DatabaseTable]
//
//implicit val databaseColumnDecoder: Decoder[DatabaseColumn] =
//  deriveDecoder[DatabaseColumn]
//implicit val databaseColumnEncoder: Encoder[DatabaseColumn] =
//  deriveEncoder[DatabaseColumn]
