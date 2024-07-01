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

package ai.privado.languageEngine.go.tagger.config

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoDBConfigBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal.*

class GoDBConfigTagger(cpg: Cpg, databaseDetailsCache: DatabaseDetailsCache)
    extends PrivadoDBConfigBaseTagger(cpg, databaseDetailsCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.property.dedup.toArray
      .filter(prop => prop.name.nonEmpty && prop.value.nonEmpty)
      .foreach(dbUrl => {
        try {
          if (dbUrl.value.contains("dynamodb")) {
            parsePropForDynamoDB(dbUrl)
          } else if (dbUrl.value.contains("postgres:")) {
            parsePropForPostgreSQL(dbUrl)
          } else if (dbUrl.value.contains("mongo")) {
            parsePropForMongoDB(dbUrl)
          }
        } catch {
          case e: Exception => logger.debug("Exception while processing db config: " + e)
        }
      })
  }

}
