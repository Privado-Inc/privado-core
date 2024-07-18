package ai.privado.tagger

import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.nodes.Call
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import io.shiftleft.semanticcpg.language.*

/** Use this trait for all the basic tagging related validation
  */
trait TaggerValidator extends Matchers {

  /** Asserts if the given callNode has given values, default tag name is Id
    *
    * @param callNode
    *   \- Expected API call node
    * @return
    */
  def assertCallByTag(callNode: Call, values: List[String], name: String = Constants.id): Assertion = {
    callNode.tag.nameExact(name).value.l shouldBe values
  }

}
