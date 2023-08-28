package ai.privado.languageEngine.javascript.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoDBConfigBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal.*

class JSDBConfigTagger(cpg: Cpg) extends PrivadoDBConfigBaseTagger(cpg) {

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
