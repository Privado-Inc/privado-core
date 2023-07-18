package ai.privado.tagger.config

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import ai.privado.tagger.{PrivadoParallelCpgPass, PrivadoSimpleCpgPass}
import ai.privado.utility.Utilities.addDatabaseDetailsMultiple
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import org.slf4j.LoggerFactory
import overflowdb.traversal._

class RubyDBConfigTagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: DiffGraphBuilder): Unit = {
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

  private def parsePropForDynamoDB(dbUrl: JavaProperty): Unit = {
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

  private def parsePropForPostgreSQL(dbUrl: JavaProperty): Unit = {
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

  private def parsePropForMongoDB(dbUrl: JavaProperty): Unit = {
    // Example mongo url :- mongodb://<username>:<password>@<host>:<port>/<database>?<options>
    val rules = List(("Write", "Storages.MongoDB.Write"), ("Read", "Storages.MongoDB.Read"))

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
}
