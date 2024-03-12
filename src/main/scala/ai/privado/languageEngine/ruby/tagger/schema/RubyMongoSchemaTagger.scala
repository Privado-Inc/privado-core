package ai.privado.languageEngine.ruby.tagger.schema

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, DatabaseColumn, DatabaseTable, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, TypeDecl}
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

class RubyMongoSchemaTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[TypeDecl](cpg){


  val sourceRules: List[RuleInfo] = ruleCache.getAllRuleInfo.filter(_.catLevelOne == CatLevelOne.SOURCES).l
  override def generateParts(): Array[TypeDecl] = {
    cpg.typeDecl.where(_.ast.isCall.nameExact("<operator>.scopeResolution").codeExact("Mongoid::Document").astParent.isCall.name("include")).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, typeDeclNode: TypeDecl): Unit = {

    val className = typeDeclNode.name
    val columnLiteralList = typeDeclNode.ast.isLiteral.where(_.astParent.isCall.name("field")).l

    columnLiteralList.foreach(lit => {

      storeForTag(builder, lit, ruleCache)("RUBY_MONGO_COLUMN")

      val dataType = lit.astSiblings.headOption match
        case Some(i: Identifier) => i.name
        case Some(c: Call) if c.name.equals("<operator>.activeRecordAssociation") => c.code.split(":").lastOption.getOrElse("").strip()
        case Some(x) => x.code
        case _ => ""

      storeForTag(builder, lit, ruleCache)("RUBY_MONGO_COLUMN_DATATYPE", dataType)
    })

    val clientName = Try(typeDeclNode.ast.isCall.name("store_in").head.astChildren.head.astChildren.last.code).toOption.getOrElse("default").replaceAll("\"", "").replaceAll("'", "")

    storeForTag(builder, typeDeclNode, ruleCache)("RUBY_MONGO_CLASS")
    storeForTag(builder, typeDeclNode, ruleCache)("RUBY_MONGO_CLASS_CLIENT", clientName)

  }
}
