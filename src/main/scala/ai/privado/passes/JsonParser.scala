package ai.privado.passes

import io.circe.Json
import io.circe.parser.parse
import org.slf4j.LoggerFactory
import better.files.File

trait JsonParser {

  /** Parses a JSON file and returns a list of key-value pairs for properties related to database connections and API
    * endpoints.
    *
    * @param file
    *   the path to the JSON file to parse
    * @return
    *   a list of key-value pairs where the keys match either the database connection or API endpoint naming conventions
    */
  def getJSONKeyValuePairs(file: String): List[(String, String)] = {
    val json = parse(File(file).contentAsString)

    // Recursively scan through the JSON to extract out all keys
    def extractKeyValuePairs(json: Json, prefix: String = ""): List[(String, String)] = {
      json match {
        case obj if obj.isObject =>
          obj.asObject.get.toMap.toList.flatMap { case (key, value) =>
            val newPrefix = if (prefix.isEmpty) key else s"$prefix.$key"
            extractKeyValuePairs(value, newPrefix)
          }
        case arr if arr.isArray =>
          arr.asArray.get.toList.zipWithIndex.flatMap { case (value, index) =>
            val newPrefix = s"$prefix[$index]"
            extractKeyValuePairs(value, newPrefix)
          }
        case other =>
          List((prefix, other.asString.getOrElse(other.toString)))
      }
    }

    val keyValuePairs = json match {
      case Right(jsonObject) => extractKeyValuePairs(jsonObject)
      case Left(parsingError) =>
        List.empty
    }

    keyValuePairs
  }

}
