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

package ai.privado.tagger.config

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import ai.privado.cache.DatabaseDetailsCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.databaseURLPriority
import org.slf4j.LoggerFactory

class DBConfigTagger(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def addDatabaseDetailsMultiple(
    rules: List[(String, String)],
    dbUrl: JavaProperty,
    dbName: String,
    dbLocation: String,
    dbVendor: String
  ): Unit = {
    rules.foreach(rule => {
      if (DatabaseDetailsCache.getDatabaseDetails(rule._2).isDefined) {
        if (
          databaseURLPriority(DatabaseDetailsCache.getDatabaseDetails(rule._2).get.dbLocation) < databaseURLPriority(
            dbUrl.value
          ) // Compare the priority of the database url with already present url in the database cache
        ) {
          DatabaseDetailsCache.removeDatabaseDetails(rule._2) // Remove if current url has higher priority
        }
      }
      DatabaseDetailsCache.addDatabaseDetails(DatabaseDetails(dbName, dbVendor, dbLocation, rule._1), rule._2)
    })
  }
  override def generateParts(): Array[JavaProperty] = {
    // Spring Data JDBC
    // We are seeing duplicate values. NEED TO INVESTIGATE
    // Let's deduplicate the properties for the time being
    // Databases:
    // val propertySinks = cpg.property.filter(p => p.value matches ("jdbc:.*://.*/.*|mongodb(\\+srv)?:.*")).l.groupBy(_.value).map(_._2.head)
    // val propertySinks = cpg.property.filter(p => p.value matches (".*")).l.groupBy(_.value).map(_._2.head)
    cpg.property.dedup.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, dbUrl: JavaProperty): Unit = {
    try {
      if (dbUrl.value.contains("jdbc:h2")) {
        parsePropForSpringJdbcAndJpaH2(dbUrl)
      } else if (dbUrl.value.contains("jdbc:oracle")) {
        parsePropForSpringJdbcAndJpaOracle(dbUrl)
      } else if (dbUrl.value.contains("jdbc:")) {
        parsePropForSpringJDBCAndJPA(dbUrl)
      } else if (dbUrl.value.contains("mongodb")) {
        parsePropForSpringDataMongo(dbUrl)
      } else if (
        dbUrl.name.contains("neo4j.host")
        && dbUrl.value.matches("(localhost|[^{]*\\..*\\.[^}]*)")
        // The regex above is an attempt to match actual database host rather than the test ones or invalid ones
        // It is supposed to match -> `123456789.databases.neo4j.io`, `neo4j.hosted.amazonaws.com`
        // rather than `{$neo4j.host}` or empty values in config
      ) {
        parsePropForNeo4jNativeDriver(dbUrl)
      } else if (dbUrl.name.contains("neo4j.driver.uri")) {
        parsePropForNeo4jSpringBootDriver(dbUrl)
      }
    } catch {
      case e: Exception => logger.debug("Exception while processing db config: " + e)
    }
  }

  private def parsePropForSpringJdbcAndJpaOracle(dbUrl: JavaProperty): Unit = {
    val rules = List(
      ("Write/Read", "Storages.SpringFramework.Jdbc"),
      ("Write", "Storages.SpringFramework.Jdbc.Write"),
      ("Read", "Storages.SpringFramework.Jdbc.Read"),
      ("Read", "Sinks.Database.JPA.Read"),
      ("Write", "Sinks.Database.JPA.Write")
    )

    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1).toString
    var dbLocation = ""
    var dbName     = ""

    if (tokens.length == 5) {
      dbLocation = tokens(3).split("@")(1).toString + ":" + tokens(4).split("/")(0)
      dbName = tokens(4).split("/")(1)
    } else {
      dbLocation = tokens(3).split("@")(1).toString + ":" + tokens(4).split("/")(0)
      dbName = tokens(5)
    }

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  private def parsePropForSpringJdbcAndJpaH2(dbUrl: JavaProperty): Unit = {
    val rules = List(
      ("Write/Read", "Storages.SpringFramework.Jdbc"),
      ("Write", "Storages.SpringFramework.Jdbc.Write"),
      ("Read", "Storages.SpringFramework.Jdbc.Read"),
      ("Read", "Sinks.Database.JPA.Read"),
      ("Write", "Sinks.Database.JPA.Write")
    )

    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1)
    var dbLocation = ""
    var dbName     = ""

    if (tokens.length == 3) {
      dbLocation = tokens(2)
      val slashTokens = dbUrl.value.split("/")
      dbName = slashTokens(slashTokens.length - 1).split("\\?")(0)
    } else {
      if (tokens(2) == "mem") {
        dbLocation = "In-memory"
        dbName = tokens(3)
      } else if (tokens(2).matches("file|tcp")) {
        dbLocation = tokens(2) + ":" + tokens(3).split(";")(0)
        val slashTokens = dbUrl.value.split("/")
        dbName = slashTokens(slashTokens.length - 1).split("\\?")(0).split(";")(0)
      }
    }

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  private def parsePropForSpringDataMongo(dbUrl: JavaProperty): Unit = {
    val rules =
      List(("Write", "Storages.MongoDB.SpringFramework.Write"), ("Read", "Storages.MongoDB.SpringFramework.Read"))

    val dbVendor   = dbUrl.value.split(":")(0).split("\\+")(0)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  private def parsePropForSpringJDBCAndJPA(dbUrl: JavaProperty): Unit = {
    val rules = List(
      ("Write/Read", "Storages.SpringFramework.Jdbc"),
      ("Write", "Storages.SpringFramework.Jdbc.Write"),
      ("Read", "Storages.SpringFramework.Jdbc.Read"),
      ("Read", "Sinks.Database.JPA.Read"),
      ("Write", "Sinks.Database.JPA.Write"),
      ("Write/Read", "Storages.SpringFramework.Jooq")
    )
    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  private def parsePropForNeo4jNativeDriver(dbUrl: JavaProperty): Unit = {
    val rules = List(
      ("Write/Read", "Storages.Neo4jGraphDatabase"),
      ("Read", "Storages.Neo4jGraphDatabase.Read"),
      ("Write", "Storages.Neo4jGraphDatabase.Write")
    )
    val dbVendor   = "bolt"
    val dbLocation = dbUrl.value
    val dbName     = "Neo4j Graph Database"

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  private def parsePropForNeo4jSpringBootDriver(dbUrl: JavaProperty): Unit = {

    val rules = List(
      ("Write/Read", "Storages.Neo4jGraphDatabase"),
      ("Read", "Storages.Neo4jGraphDatabase.Read"),
      ("Write", "Storages.Neo4jGraphDatabase.Write")
    )

    val dbVendor   = dbUrl.value.split(":")(0)
    val dbLocation = dbUrl.value.split("/")(2)
    // The uri for Neo4j driver does not require a dbName, usually Neo4j is deployed with just one db
    val dbName = "Neo4j Graph Database"

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

}
