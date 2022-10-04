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

import ai.privado.language._
import ai.privado.model.Constants
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, JavaProperty}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.cache.DatabaseDetailsCache
import ai.privado.model.DatabaseDetails
import com.sun.jndi.dns.DnsUrl

class DBConfigTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // Spring Data JDBC
    // We are seeing duplicate values. NEED TO INVESTIGATE
    // Let's deduplicate the properties for the time being
    // Databases:
    // val propertySinks = cpg.property.filter(p => p.value matches ("jdbc:.*://.*/.*|mongodb(\\+srv)?:.*")).l.groupBy(_.value).map(_._2.head)
    // val propertySinks = cpg.property.filter(p => p.value matches (".*")).l.groupBy(_.value).map(_._2.head)
    val propertySinks = cpg.property.dedup.l

    // Display the value of mylist using for loop
    for (dbUrl <- propertySinks) {
      if (dbUrl.value.contains("jdbc:h2")) {
        parsePropForSpringJdbcAndJpaH2(dbUrl)
      } else if (dbUrl.value.contains("jdbc:oracle")) {
        parsePropForSpringJdbcAndJpaOracle(dbUrl)
      } else if (dbUrl.value.contains("jdbc:")) {
        parsePropForSpringJDBCAndJPA(dbUrl)
      } else if (dbUrl.value.contains("mongodb")) {
        parsePropForSpringDataMongo(dbUrl)
      }
    }
  }

  private def parsePropForSpringJdbcAndJpaOracle(dbUrl: JavaProperty): Unit = {
    // println("Inside parsePropForSpringJdbcAndJpaOracle..")
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
    /* println("DB TYPE: " + dbVendor)
    println("DB NAME: " + dbName)
    println("DB LOCATION: " + dbLocation) */

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write/Read"),
      "Storages.SpringFramework.Jdbc"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.SpringFramework.Jdbc.Write"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.SpringFramework.Jdbc.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Sinks.Database.JPA.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Sinks.Database.JPA.Write"
    )
  }

  private def parsePropForSpringJdbcAndJpaH2(dbUrl: JavaProperty): Unit = {
    // println("Inside parsePropForSpringJdbcAndJpaH2..")
    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1).toString
    var dbLocation = ""
    var dbName     = ""

    if (tokens.length == 3) {
      dbLocation = tokens(2).toString
      val slashTokens = dbUrl.value.split("/")
      dbName = slashTokens(slashTokens.length - 1).split("\\?")(0)
    } else {
      if (tokens(2).toString == "mem") {
        dbLocation = "In-memory"
        dbName = tokens(3).toString
      } else if (tokens(2).toString.matches("file|tcp")) {
        dbLocation = tokens(2).toString + ":" + tokens(3).toString.split(";")(0)
        val slashTokens = dbUrl.value.split("/")
        dbName = slashTokens(slashTokens.length - 1).split("\\?")(0).split(";")(0)
      }
    }
    /* println("DB TYPE: " + dbVendor)
    println("DB NAME: " + dbName)
    println("DB LOCATION: " + dbLocation) */

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write/Read"),
      "Storages.SpringFramework.Jdbc"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.SpringFramework.Jdbc.Write"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.SpringFramework.Jdbc.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Sinks.Database.JPA.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Sinks.Database.JPA.Write"
    )
  }

  private def parsePropForSpringDataMongo(dbUrl: JavaProperty): Unit = {
    // println("Inside parsePropForSpringDataMongo..")
    val dbVendor   = dbUrl.value.split(":")(0).split("\\+")(0)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)
    /* println("DB TYPE: " + dbVendor)
    println("DB NAME: " + dbName)
    println("DB LOCATION: " + dbLocation) */

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.MongoDB.SpringFramework.Write"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.MongoDB.SpringFramework.Read"
    )
  }

  private def parsePropForSpringJDBCAndJPA(dbUrl: JavaProperty): Unit = {
    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)
    /* println("DB TYPE: " + dbVendor)
    println("DB NAME: " + dbName)
    println("DB LOCATION: " + dbLocation) */

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write/Read"),
      "Storages.SpringFramework.Jdbc"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.SpringFramework.Jdbc.Write"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.SpringFramework.Jdbc.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Sinks.Database.JPA.Read"
    )
    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Sinks.Database.JPA.Write"
    )
  }

}
