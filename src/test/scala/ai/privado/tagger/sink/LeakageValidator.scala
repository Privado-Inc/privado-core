package ai.privado.tagger.sink

import ai.privado.model.{CatLevelOne, Constants}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
import org.scalatest.{Assertion, FixtureContext, Succeeded}
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}

trait LeakageValidator extends Matchers {

  def assertLeakageSink(leakageSink: AstNode): Assertion = {
    leakageSink.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name).size shouldBe 1
    leakageSink.tag.nameExact(Constants.catLevelTwo).valueExact(Constants.leakages).size shouldBe 1
  }

  def assertNotLeakageSink(leakageSink: AstNode): Assertion = Try(assertLeakageSink(leakageSink)) match
    case Failure(_) => assert(true)
    case Success(_) => assert(false)

}
