package ai.privado.model

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  pattern: String,
  sensitivity: String,
  tags: Map[String, String],
  nodeType: String
)
