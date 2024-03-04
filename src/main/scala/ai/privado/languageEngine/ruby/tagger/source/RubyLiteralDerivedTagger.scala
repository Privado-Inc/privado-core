package ai.privado.languageEngine.ruby.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  FieldIdentifier,
  Identifier,
  Literal,
  Local,
  TypeDecl
}
import io.shiftleft.semanticcpg.language.*

class RubyLiteralDerivedTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[(TypeDecl, List[Literal])](cpg) {

  private val cachedColonLiteral =
    cpg.literal.filter(_.code.startsWith(":")).where(_.astParent.isCall.name("argument")).l

  // Refer RubyLiteralDerivedTaggerTest test to understand the use case
  override def generateParts(): Array[(TypeDecl, List[Literal])] = {
    val mapping = cpg.literal
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
      .where(_.astParent.isCall.name("argument"))
      .map(lit => (lit, lit.start.repeat(_.astParent)(_.until(_.isTypeDecl)).l))
      .filter(_._2.nonEmpty)
      .map(item => (item._1, item._2.head.asInstanceOf[TypeDecl]))
      .l

    val groupedByTypeDecl = mapping.groupBy(_._2).map(item => (item._1, item._2.map(_._1))).toArray
    groupedByTypeDecl
  }

  override def runOnPart(builder: DiffGraphBuilder, part: (TypeDecl, List[Literal])): Unit = {

    val typeDeclNode = part._1
    val literalList  = part._2

    val derivedLiterals = cachedColonLiteral
      .filter(_.astSiblings.nonEmpty)
      .filter(_.astSiblings.head match
        case i: Identifier =>
          i.name.equals(typeDeclNode.name) && !List("ID", "String", "Boolean", "Float", "Integer", "JSON", "Int")
            .contains(i.name)
        case sr: Call if sr.name.equals("<operator>.scopeResolution") => {

          val code        = sr.code.stripPrefix("::").replaceAll("::", "\\.")
          val packageName = typeDeclNode.fullName.split("::program[.]").lastOption.getOrElse("")

          code.equals(packageName)
        }
        case _ => false
      )

    derivedLiterals.dedup.foreach { impactedLiteral =>
      literalList.foreach { originalSource =>

        val impactedLiteralName = impactedLiteral.code.stripPrefix(":")

        val impactedIdentifiers = impactedLiteral.start
          .repeat(_.astParent)(_.until(_.isTypeDecl))
          .ast
          .flatMap(x =>
            x match {
              case x: Identifier if x.name.equals(impactedLiteralName)               => Some(x)
              case x: FieldIdentifier if x.canonicalName.equals(impactedLiteralName) => Some(x)
              case _                                                                 => None
            }
          )
          .dedup
          .l

        val ruleId     = originalSource.tag.nameExact(Constants.id).value.headOption.getOrElse("")
        val memberName = originalSource.code.stripPrefix(":")

        if (ruleId.nonEmpty) {
          impactedIdentifiers.concat(List(impactedLiteral)).foreach { impactedObject =>
            storeForTag(builder, impactedObject, ruleCache)(
              InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString,
              ruleId
            )
            storeForTag(builder, impactedObject, ruleCache)(
              Constants.id,
              Constants.privadoDerived + Constants.underScore + ruleId
            )
            storeForTag(builder, impactedObject, ruleCache)(Constants.catLevelOne, CatLevelOne.DERIVED_SOURCES.name)
            storeForTag(builder, impactedObject, ruleCache)(
              Constants.privadoDerived + Constants.underScore + ruleId,
              ruleId
            )
            // Tag for storing memberName in derived Objects -> user --> (email, password)
            storeForTag(builder, impactedObject, ruleCache)(
              ruleId + Constants.underScore + Constants.privadoDerived + Constants.underScore + ruleId,
              memberName
            )
          }
        }
      }
    }

  }

}
