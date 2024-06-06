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

package ai.privado.cache

import ai.privado.model.{DatabaseDetails, DatabaseSchema}

import scala.collection.mutable

/** Cache to store Rules specific things
  */
object DatabaseDetailsCache {

  private val databaseDetailsMap = mutable.HashMap[String, DatabaseDetails]()

  def addDatabaseDetails(databaseDetails: DatabaseDetails, ruleId: String): Unit = {

    if (databaseDetailsMap.contains(ruleId) && databaseDetailsMap(ruleId).schema.isDefined) {
      // rule already exists and schema is also present, append new schema tables

      val oldSchema = databaseDetailsMap(ruleId).schema.get
      val newSchema = databaseDetails.schema
      if (newSchema.isDefined)
        databaseDetailsMap.addOne(
          ruleId -> databaseDetails
            .copy(schema = Option(newSchema.get.copy(tables = oldSchema.tables ++ newSchema.get.tables)))
        )
      else
        databaseDetailsMap.addOne(ruleId -> databaseDetails.copy(schema = Option(oldSchema)))
    } else databaseDetailsMap.addOne(ruleId -> databaseDetails)
  }

  def getDatabaseDetails(ruleId: String): Option[DatabaseDetails] = databaseDetailsMap.get(ruleId)

  def getAllDatabaseDetails: List[DatabaseDetails] = databaseDetailsMap.values.toList

  def removeDatabaseDetails(ruleId: String): Unit = {
    databaseDetailsMap.remove(ruleId)
  }

}
