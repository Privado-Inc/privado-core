package ai.privado.tagger.config

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

class PythonDBConfigTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[JavaProperty](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[JavaProperty] = {
    cpg.property.dedup.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, dbUrl: JavaProperty): Unit = {
    try {
      if (dbUrl.value.contains("dynamodb")) {
        parsePropForDynamoDB(dbUrl)
      } else if (dbUrl.value.contains("postgresql")) {
        parsePropForPostgreSQL(dbUrl)
      } else if (dbUrl.value.contains("mongodb")) {
        parsePropForMongoDB(dbUrl)
      }
    } catch {
      case e: Exception => logger.debug("Exception while processing db config: " + e)
    }

  }

  private def parsePropForDynamoDB(dbUrl: JavaProperty): Unit = {
    // Example dynamodb url :- https://dynamodb.<region>.amazonaws.com/<table-name>
    val dbVendor   = "dynamodb"
    val dbLocation = dbUrl.value.split("\\.")(1)
    val dbName     = dbUrl.value.split("/").last

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.AmazonDynamoDB.Write"
    )

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.AmazonDynamoDB.Read"
    )
  }

  private def parsePropForPostgreSQL(dbUrl: JavaProperty): Unit = {
    // Example postgre URL: - postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase

    val tokens     = dbUrl.value.split("@")
    val dbLocation = tokens.last.split("/")(0)
    val dbName     = dbUrl.value.split("/").last
    val dbVendor   = "postgresql"

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.Postgres.ReadAndWrite"
    )

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.Postgres.Read"
    )
  }

  private def parsePropForMongoDB(dbUrl: JavaProperty): Unit = {
    // Example mongo url :- mongodb://<username>:<password>@<host>:<port>/<database>?<options>

    val tokens     = dbUrl.value.split("@")
    val dbVendor   = "mongodb"
    val dbLocation = tokens.last.split("/")(0)
    val dbName     = dbUrl.value.split("/").last.split("\\?")(0)

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Write"),
      "Storages.MongoDB.Write"
    )

    DatabaseDetailsCache.addDatabaseDetails(
      DatabaseDetails(dbName, dbVendor, dbLocation, "Read"),
      "Storages.MongoDB.Read"
    )

  }
}
