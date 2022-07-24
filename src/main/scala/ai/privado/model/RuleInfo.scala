package ai.privado.model

import scala.collection.immutable.HashMap

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  pattern: String,
  sensitivity: String,
  tags: HashMap[String, String],
  nodeType: String
)
