package ai.privado.languageEngine.ruby.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.sql.SQLQueryType
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{SqlColumnNode, SqlTableNode, TypeDecl}

import java.util.UUID
class IdentifierDerivedTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[SqlColumnNode](cpg) {

  lazy val RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME: String = UUID.randomUUID.toString

  private val cachedTypeDecl: List[TypeDecl] = cpg.typeDecl.where(_.file.name(".*models/.*")).l

  override def generateParts(): Array[SqlColumnNode] = cpg.sqlQuery
    .name(SQLQueryType.CREATE)
    .astChildren
    .astChildren
    .collectAll[SqlColumnNode]
    .where(_.tag.nameExact(InternalTag.VARIABLE_REGEX_LITERAL.toString))
    .toArray

  override def runOnPart(builder: DiffGraphBuilder, sqlColumn: SqlColumnNode): Unit = {

    sqlColumn.tag.nameExact(Constants.id).value.foreach { ruleId =>
      ruleCache.getRuleInfo(ruleId) match
        case Some(ruleInfo) =>
          sqlColumn.astParent match
            case tableNode: SqlTableNode =>
              val cleanedTableName  = cleanString(tableNode.name)
              val typeDeclFullNames = cachedTypeDecl.filter(_.name.equalsIgnoreCase(cleanedTableName)).fullName.l
              if (typeDeclFullNames.nonEmpty) {
                val typeDeclFullNameRegex = typeDeclFullNames.mkString("(", "|", ")")
                cpg.identifier
                  .or(
                    _.typeFullName(s"$typeDeclFullNameRegex.*"),
                    _.filter(_.dynamicTypeHintFullName.exists(_.matches(s"$typeDeclFullNameRegex.*"))),
                    _.name(s"(?i)$cleanedTableName"),
                    _.name(s"(?i)${tableNode.name.stripSuffix("s")}")
                  )
                  .foreach { impactedObject =>

                    storeForTag(builder, impactedObject, ruleCache)(
                      InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString,
                      ruleInfo.id
                    )
                    storeForTag(builder, impactedObject, ruleCache)(
                      Constants.id,
                      Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME
                    )
                    storeForTag(builder, impactedObject, ruleCache)(
                      Constants.catLevelOne,
                      CatLevelOne.DERIVED_SOURCES.name
                    )
                    storeForTag(builder, impactedObject, ruleCache)(
                      Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
                      ruleInfo.id
                    )
                    // Tag for storing memberName in derived Objects -> user --> (email, password)
                    storeForTag(builder, impactedObject, ruleCache)(
                      ruleInfo.id + Constants.underScore + Constants.privadoDerived + Constants.underScore + RANDOM_ID_OBJECT_OF_TYPE_DECL_HAVING_MEMBER_NAME,
                      sqlColumn.name
                    )
                  }
              }
            case _ =>
        case None =>
    }
  }

  private def cleanString(inputString: String) = {
    inputString.replace("_", "").stripSuffix("s")
  }

}
