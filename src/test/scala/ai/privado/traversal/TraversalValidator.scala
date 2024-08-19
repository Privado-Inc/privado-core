package ai.privado.traversal

import ai.privado.semantic.language.*
import ai.privado.model.InternalTag
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

trait TraversalValidator extends Matchers {

  def originalSourceTraversalValidator(derivedSourceNode: AstNode, sourceId: String): Assertion = {
    derivedSourceNode
      .originalSource(sourceId)
      .get
      .tag
      .name(InternalTag.ORIGINAL_SOURCE_FOR_DERIVED_NODE.toString)
      .nonEmpty shouldBe true
  }

  def derivedSourceTraversalValidator(originalSourceNode: AstNode): Assertion = {
    originalSourceNode.derivedSource.tag
      .name(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)
      .nonEmpty shouldBe true
  }

}
