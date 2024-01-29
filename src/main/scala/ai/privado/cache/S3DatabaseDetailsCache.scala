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

import ai.privado.model.DatabaseDetails

import scala.collection.mutable

/** Cache to store Rules specific things
  */
class S3DatabaseDetailsCache {

  private val databaseDetailsMap = mutable.HashMap[String, List[DatabaseDetails]]()

  def addS3DatabaseDetails(databaseDetails: List[DatabaseDetails], ruleId: String): Unit =
    databaseDetailsMap.addOne(ruleId -> databaseDetails)
  def getS3DatabaseDetails(ruleId: String): Option[List[DatabaseDetails]] = databaseDetailsMap.get(ruleId)

}
