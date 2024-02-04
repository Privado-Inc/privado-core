package ai.privado.tagger

import ai.privado.cache.RuleCache
import ai.privado.model.{InternalTag, RuleInfo}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Languages, Operators}
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.nodes.NewTag

class AssetTagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {

  val sourceRegex = "(?i).*(mongo|sql|s3|oracle|dynamo|maria|hive|redis|dbName|database|sqs|queue|engine).*"
  val valueRegex  = "(?i).*(mongo|sql|s3|oracle|dynamo|maria|hive|redis|sqs|queue).*"
  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.literal
      .code(valueRegex)
      .whereNot(_.code(".*[.](png|jpg|jpeg|jar|zip|xml|json|yml)$"))
      .foreach(builder.addEdge(_, NewTag().name(InternalTag.PROBABLE_ASSET.toString), EdgeTypes.TAGGED_BY))

    cpg.identifier
      .name(sourceRegex)
      .foreach(builder.addEdge(_, NewTag().name(InternalTag.PROBABLE_ASSET.toString), EdgeTypes.TAGGED_BY))

    cpg.member
      .name(sourceRegex)
      .foreach(builder.addEdge(_, NewTag().name(InternalTag.PROBABLE_ASSET.toString), EdgeTypes.TAGGED_BY))

    cpg.fieldAccess.fieldIdentifier
      .canonicalName(sourceRegex)
      .foreach(builder.addEdge(_, NewTag().name(InternalTag.PROBABLE_ASSET.toString), EdgeTypes.TAGGED_BY))
  }
}
