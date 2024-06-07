package ai.privado.languageEngine.ruby.tagger.schema

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, DatabaseColumn, DatabaseTable, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, TypeDecl}
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

class RubyMongoSchemaTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[TypeDecl](cpg) {

  val sourceRules: List[RuleInfo] = ruleCache.getAllRuleInfo.filter(_.catLevelOne == CatLevelOne.SOURCES).l
  override def generateParts(): Array[TypeDecl] = {
    /*
    Check if the given class have a statement `Mongoid::Document` which corresponds to it being a mongo document
    which can be used to determine the schema of the collection

    class User
      include Mongoid::Document
      include Mongoid::Timestamps

      belongs_to :team
      belongs_to :permission_set

      field :last_logged_in, type: DateTime
      field :last_logged_out, type: DateTime
    end

    In the above code `User` is a Mongo repository, with `last_logged_in` and `last_logged_out` being its field
     */
    cpg.typeDecl
      .filter(
        _.code.startsWith("class")
      ) // It has been observed the code has `class User` kind of text, this is to exclude namespace and modules
      .where(
        _.ast.isCall
          .nameExact("<operator>.scopeResolution")
          .codeExact("Mongoid::Document")
          .astParent
          .isCall
          .name("include")
      )
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, typeDeclNode: TypeDecl): Unit = {

    val className         = typeDeclNode.name
    val columnLiteralList = typeDeclNode.ast.isLiteral.where(_.astParent.isCall.name("field")).l

    columnLiteralList.foreach(lit => {

      storeForTag(builder, lit, ruleCache)(InternalTag.RUBY_MONGO_COLUMN.toString)

      val dataType = lit.astSiblings.headOption match
        case Some(i: Identifier) => i.name
        case Some(c: Call) if c.name.equals("<operator>.activeRecordAssociation") =>
          c.code.split(":").lastOption.getOrElse("").strip()
        case Some(x) => x.code
        case _       => ""

      storeForTag(builder, lit, ruleCache)(InternalTag.RUBY_MONGO_COLUMN_DATATYPE.toString, dataType)
    })

    val clientName = Try(typeDeclNode.ast.isCall.name("store_in").head.astChildren.head.astChildren.last.code).toOption
      .getOrElse("default")
      .replaceAll("\"", "")
      .replaceAll("'", "")

    storeForTag(builder, typeDeclNode, ruleCache)(InternalTag.RUBY_MONGO_COLUMN.toString)
    storeForTag(builder, typeDeclNode, ruleCache)(InternalTag.RUBY_MONGO_CLASS_CLIENT.toString, clientName)

  }
}
