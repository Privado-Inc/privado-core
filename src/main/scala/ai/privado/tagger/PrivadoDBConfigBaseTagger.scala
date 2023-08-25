package ai.privado.tagger

import ai.privado.tagger.PrivadoSimpleCpgPass
import ai.privado.utility.Utilities.addDatabaseDetailsMultiple
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import org.slf4j.LoggerFactory

abstract class PrivadoDBConfigBaseTagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  def parsePropForDynamoDB(dbUrl: JavaProperty): Unit = {
    // Example dynamodb url :- https://dynamodb.<region>.amazonaws.com/<table-name>
    val rules = List(("Write", "Storages.AmazonDynamoDB.Write"), ("Read", "Storages.AmazonDynamoDB.Write"))

    try {
      val dbVendor   = "dynamodb"
      val dbLocation = dbUrl.value.split("\\.")(1)
      val dbName     = dbUrl.value.split("/").last

      addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)
    } catch {
      case e: Throwable => logger.debug("Error parsing details for dynamo db")
    }
  }

  def parsePropForPostgreSQL(dbUrl: JavaProperty): Unit = {
    // Example postgre URL: - postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase
    val rules = List(("Write", "Storages.Postgres.ReadAndWrite"), ("Read", "Storages.Postgres.Read"))

    try {
      val tokens     = dbUrl.value.split("@")
      val dbLocation = tokens.last.split("/")(0)
      val dbName     = dbUrl.value.split("/").last
      val dbVendor   = "postgresql"

      addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)
    } catch {
      case e: Throwable => logger.debug("Error parsing details for postgre SQL")
    }
  }

  def parsePropForMongoDB(dbUrl: JavaProperty): Unit = {
    // Example mongo url :- mongodb://<username>:<password>@<host>:<port>/<database>?<options>
    val rules = List(("Write", "Storages.MongoDB.Write"), ("Read", "Storages.MongoDB.Read"))

    val dbVendor   = dbUrl.value.split(":")(0).split("\\+")(0)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)

    try {
      val tokens     = dbUrl.value.split("@")
      val dbVendor   = "mongodb"
      val dbLocation = tokens.last.split("/")(0)
      val dbName     = dbUrl.value.split("/").last.split("\\?")(0)

      addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

    } catch {
      case e: Throwable => logger.debug("Error parsing details for mongodb")
    }
  }

  def parsePropForJdbcAndJpaOracle(dbUrl: JavaProperty, rules: List[(String, String)]): Unit = {

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

  def parsePropForJdbcAndJpaH2(dbUrl: JavaProperty, rules: List[(String, String)]): Unit = {

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

  def parsePropForJDBCAndJPA(dbUrl: JavaProperty, rules: List[(String, String)]): Unit = {
    val tokens     = dbUrl.value.split(":")
    val dbVendor   = tokens(1)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)

    addDatabaseDetailsMultiple(rules, dbUrl, dbName, dbLocation, dbVendor)

  }

  def parsePropForNeo4jNativeDriver(dbUrl: JavaProperty): Unit = {
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
}
