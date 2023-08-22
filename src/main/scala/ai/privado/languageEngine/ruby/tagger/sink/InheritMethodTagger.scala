package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.feeder.StorageInheritRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, TypeDecl}
import io.shiftleft.semanticcpg.language.*

class InheritMethodTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[RuleInfo] = StorageInheritRule.rules.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val typeDeclMapping =
      cpg.typeDecl.where(_.file.name(ruleInfo.patterns.head)).map(typeDecl => (typeDecl.name, typeDecl)).toMap
    val typeDeclNames = typeDeclMapping.keySet

    if (ruleInfo.id.equals("Sink.DataBase.ROR.Read")) {
      // Handle read sinks

      val readCalls = cpg
        .call(ruleInfo.patterns(1))
        .filter(isFirstArgumentIdentifier)
        .filter(callNode => typeDeclNames.contains(callNode.argument.head.asInstanceOf[Identifier].name))
        .l

      readCalls.foreach(addRuleTags(builder, _, ruleInfo, ruleCache))

    } else if (ruleInfo.id.equals("Sink.DataBase.ROR.Write")) {
      // Handle write sinks
      val processedTypeDeclNames = typeDeclNames.map(_.toLowerCase)
      val writeCalls = cpg
        .call(ruleInfo.patterns(1))
        .filter(isFirstArgumentIdentifier)
        .filter(callNode =>
          processedTypeDeclNames.contains(getProcessedName(callNode.argument.head.asInstanceOf[Identifier].name))
        )
        .l

      writeCalls.foreach(addRuleTags(builder, _, ruleInfo, ruleCache))

    } else if (ruleInfo.id.equals("Sink.DataBase.ROR.ReadAndWrite")) {
      typeDeclMapping.foreachEntry { (_, typeDecl) =>
        val scopeCallNames = typeDecl.ast.isCall.nameExact("scope").argument.isLiteral.code.map(_.stripPrefix(":")).l
        val scopeCalls = cpg.call
          .name(scopeCallNames: _*)
          .filter(isFirstArgumentIdentifier)
          .filter(callNode => typeDeclNames.contains(callNode.argument.head.asInstanceOf[Identifier].name))
          .l
        scopeCalls.foreach(addRuleTags(builder, _, ruleInfo, ruleCache))
      }
    }
  }
  private def isFirstArgumentIdentifier(callNode: Call) = callNode.argument.headOption.exists(_.isIdentifier)
  private def getProcessedName(identifierName: String) = {
    identifierName.replace("_", "").toLowerCase
  }
}
