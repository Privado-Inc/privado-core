package ai.privado.languageEngine.javascript.passes.config

import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.nodes.AstNode.Edges
import io.shiftleft.semanticcpg.language.*

class JsConfigPropertyPass(cpg: Cpg) extends PrivadoParallelCpgPass[Call](cpg) {

  def generateParts(): Array[Call] = {
    cpg.assignment.where(_.astChildren.isBlock).where(_.astParent.isBlock.code("<empty>")).toArray
  }

  def runOnPart(builder: DiffGraphBuilder, assignmentCall: Call): Unit = {
    processKeyPairAndCreatePropertyNode(builder, assignmentCall)
  }

  def processKeyPairAndCreatePropertyNode(
    builder: DiffGraphBuilder,
    assignmentCall: Call,
    parentKey: String = ""
  ): List[(String, String, Int)] = {
    assignmentCall.astChildren.toList match {
      case (c: Call) :: (b: Block) :: _ if c.name.equals(Operators.fieldAccess) =>
        b.astChildren.isCall
          .name(Operators.assignment)
          .flatMap(assign =>
            processKeyPairAndCreatePropertyNode(builder, assign, s"$parentKey.${getKeyNameFromFieldAccessCode(c)}")
          )
          .toList
      case (i: Identifier) :: (b: Block) :: _ =>
        b.astChildren.isCall
          .name(Operators.assignment)
          .flatMap(assign => processKeyPairAndCreatePropertyNode(builder, assign, s"$parentKey.${i.name}"))
          .toList
      case (c: Call) :: (a: AstNode) :: _ if c.name.equals(Operators.fieldAccess) =>
        val partialKey   = getKeyNameFromFieldAccessCode(c)
        val keyValuePair = (cleanCode(s"$parentKey.$partialKey"), cleanCode(a.code), a.lineNumber.get)
        val propertyNode = addPropertyNode(keyValuePair, builder)

        // Add a tag to discrimate between property nodes from config and source code files
        builder.addEdge(propertyNode, NewTag().name(InternalTag.SOURCE_PROPERTY.toString), EdgeTypes.TAGGED_BY)

        // Add used at and orginal property edge
        builder.addEdge(propertyNode, a, EdgeTypes.IS_USED_AT)
        builder.addEdge(a, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)

        // Add file edge
        builder.addEdge(propertyNode, a.file.head, EdgeTypes.SOURCE_FILE)
        List(keyValuePair)
      case _ => List()
    }
  }
  private def getKeyNameFromFieldAccessCode(fieldAccess: Call): String = {
    val splittedCode = fieldAccess.code.split("[.]")
    if (splittedCode.headOption.getOrElse("").contains("_tmp_"))
      splittedCode.lastOption.getOrElse("")
    else
      fieldAccess.code
  }

  private def addPropertyNode(
    keyValuePair: (String, String, Int),
    builder: DiffGraphBuilder
  ): NewJavaProperty = {
    val (key, value, lineNumber) = keyValuePair
    val propertyNode             = NewJavaProperty().name(key.stripPrefix(".")).value(value).lineNumber(lineNumber)
    builder.addNode(propertyNode)
    propertyNode
  }

  private def cleanCode(code: String) = {
    code.stripPrefix("'").stripSuffix("'").stripPrefix("\"").stripSuffix("\"")
  }
}
