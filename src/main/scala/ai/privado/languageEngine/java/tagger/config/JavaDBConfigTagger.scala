package ai.privado.languageEngine.java.tagger.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoDBConfigBaseTagger
import ai.privado.utility.Utilities.addDatabaseDetailsMultiple
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import org.slf4j.LoggerFactory
import overflowdb.traversal.*

class JavaDBConfigTagger(cpg: Cpg) extends PrivadoDBConfigBaseTagger(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.property.dedup.toArray
      .filter(prop => prop.name.nonEmpty && prop.value.nonEmpty)
      .foreach(dbUrl => {
        try {
          if (dbUrl.value.contains("jdbc:h2")) {
            parsePropForJdbcAndJpaH2(
              dbUrl,
              List(
                ("Write/Read", "Storages.SpringFramework.Jdbc"),
                ("Write", "Storages.SpringFramework.Jdbc.Write"),
                ("Read", "Storages.SpringFramework.Jdbc.Read"),
                ("Read", "Sinks.Database.JPA.Read"),
                ("Write", "Sinks.Database.JPA.Write")
              )
            )
          } else if (dbUrl.value.contains("jdbc:oracle")) {
            parsePropForJdbcAndJpaOracle(
              dbUrl,
              List(
                ("Write/Read", "Storages.SpringFramework.Jdbc"),
                ("Write", "Storages.SpringFramework.Jdbc.Write"),
                ("Read", "Storages.SpringFramework.Jdbc.Read"),
                ("Read", "Sinks.Database.JPA.Read"),
                ("Write", "Sinks.Database.JPA.Write")
              )
            )
          } else if (dbUrl.value.contains("jdbc:")) {
            parsePropForJDBCAndJPA(
              dbUrl,
              List(
                ("Write/Read", "Storages.SpringFramework.Jdbc"),
                ("Write", "Storages.SpringFramework.Jdbc.Write"),
                ("Read", "Storages.SpringFramework.Jdbc.Read"),
                ("Read", "Sinks.Database.JPA.Read"),
                ("Write", "Sinks.Database.JPA.Write"),
                ("Write/Read", "Storages.SpringFramework.Jooq")
              )
            )
          } else if (dbUrl.value.contains("mongodb")) {
            parsePropForMongoDB(dbUrl)
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
      })
  }

  override def parsePropForMongoDB(dbUrl: JavaProperty): Unit = {
    val rules =
      List(("Write", "Storages.MongoDB.SpringFramework.Write"), ("Read", "Storages.MongoDB.SpringFramework.Read"))

    val dbVendor   = dbUrl.value.split(":")(0).split("\\+")(0)
    val dbLocation = dbUrl.value.split("/")(2)
    val dbName     = dbUrl.value.split("/")(3).split("\\?")(0)

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
