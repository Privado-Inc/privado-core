package ai.privado.languageEngine.ruby.config

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.semantic.language.*
import ai.privado.tagger.PrivadoDBConfigBaseTagger
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import org.slf4j.LoggerFactory

class RubyDBConfigTagger(cpg: Cpg, databaseDetailsCache: DatabaseDetailsCache)
    extends PrivadoDBConfigBaseTagger(cpg, databaseDetailsCache) {

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
}
