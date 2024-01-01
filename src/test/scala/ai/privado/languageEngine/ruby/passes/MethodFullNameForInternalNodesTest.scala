package ai.privado.languageEngine.ruby.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import better.files.File
import io.joern.rubysrc2cpg.deprecated.passes.RubyTypeHintCallLinker
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.frontend.{LocalKey, SBKey, SymbolTable}
import io.shiftleft.semanticcpg.language._

case class SourceCodeModel(sourceCode: String, fileName: String)

class MethodFullNameForInternalNodesTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "method full name for internal nodes belonging to the same module" should {
    val cpg = code(
      List(
        SourceCodeModel(
          """
        |module Pay
        |  class PaymentsController < ApplicationController
        |    def show
        |      @payment1 = Payment.from_id(params[:id])
        |      @payment2 = Pay::Payment.from_id(params[:id])
        |    end
        |  end
        |end
        |
        |""".stripMargin,
          "payments_controller.rb"
        ),
        SourceCodeModel(
          """
        |module Pay
        |  class Payment
        |    attr_reader :intent
        |
        |    delegate :id, :amount, :client_secret, :currency, :customer, :status, :confirm, to: :intent
        |
        |    def self.from_id(id)
        |      intent = id.start_with?("seti_") ? ::Stripe::SetupIntent.retrieve(id) : ::Stripe::PaymentIntent.retrieve(id)
        |      new(intent)
        |    end
        |  end
        |end
        |""".stripMargin,
          "payment.rb"
        )
      )
    )

    "have a resolved type when LHS is a call node and called without scopeResolution" in {
      val paymentFromIdCall = cpg.call.name("from_id").lineNumber(5).l
      paymentFromIdCall.methodFullName.l shouldBe List("payment.rb::program.Pay.Payment.from_id")

      val paymentNode = cpg.member.name("payment1").l
      paymentNode.size shouldBe 1
      paymentNode.typeFullName.l shouldBe List("payment.rb::program.Pay.Payment.from_id.<returnValue>")
    }

    "have a resolved type when LHS is a call node and called with scopeResolution" in {
      val paymentFromIdCall = cpg.call.name("from_id").lineNumber(6).l
      paymentFromIdCall.methodFullName.l shouldBe List("payment.rb::program.Pay.Payment.from_id")

      val paymentNode = cpg.member.name("payment2").l
      paymentNode.size shouldBe 1
      paymentNode.typeFullName.l shouldBe List("payment.rb::program.Pay.Payment.from_id.<returnValue>")
    }
  }

  "method fullname propagation due to object creation with `new`" should {

    val cpg = code(
      List(
        SourceCodeModel(
          """
        |module MyModule
        |    class Mapping
        |        attr_reader :class_name, :method_name
        |
        |        def initialize(class_name:, method_name:)
        |          @class_name = class_name
        |          @method_name = method_name
        |        end
        |    end
        |end
        |""".stripMargin,
          "mapping.rb"
        ),
        SourceCodeModel(
          """
          |class Demo
          |     def foo
          |         val myMapping = MyModule::Mapping.new("myClass", "methodName")
          |         val notMyMapping = MyModule::NotMyMapping.new("somerandom")
          |     end
          |end
          |""".stripMargin,
          "demo.rb"
        )
      )
    )

    "have correct type for the new node" in {

      val myMapping = cpg.identifier("myMapping").l
      myMapping.typeFullName.l shouldBe List("mapping.rb::program.MyModule.Mapping.<init>.<returnValue>")
      cpg.call("<init>").lineNumber(4).methodFullName.l shouldBe List("mapping.rb::program.MyModule.Mapping.<init>")

      cpg.call("<init>").lineNumber(5).methodFullName.l shouldBe List("<unknownFullName>")

    }
  }

  "types for nodes accessed via module" should {

    val cpg = code(
      List(
        SourceCodeModel(
          """
        |module Pay
        |    attr_reader :intent
        |    def display
        |      puts "debug"
        |    end
        |end
        |""".stripMargin,
          "myModule.rb"
        ),
        SourceCodeModel(
          """
        |class Demo
        |   def foo
        |      Pay.display
        |   end
        |end
        |""".stripMargin,
          "demo.rb"
        )
      )
    )

    "be generated" in {
      cpg.call("display").lineNumber(4).methodFullName.l shouldBe List("myModule.rb::program.Pay.display")
    }
  }

}

def code(sourceCodes: List[SourceCodeModel]): Cpg = {
  val inputDir = File.newTemporaryDirectory()
  for (sourceCode <- sourceCodes) {
    (inputDir / sourceCode.fileName).write(sourceCode.sourceCode)
  }
  val outputFile = File.newTemporaryFile()
  val config = Config()
    .withInputPath(inputDir.pathAsString)
    .withOutputPath(outputFile.pathAsString)
    .withUseDeprecatedFrontend(true)
  val rubySrc = new RubySrc2Cpg()
  val xtocpg = rubySrc.createCpg(config).map { cpg =>
    applyDefaultOverlays(cpg)
    cpg
  }
  val cpg               = xtocpg.get
  val globalSymbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)
  new GlobalImportPass(cpg, globalSymbolTable).createAndApply()

  new PrivadoRubyTypeRecoveryPassGenerator(cpg, globalSymbolTable).generate().foreach(_.createAndApply())
  new RubyTypeHintCallLinker(cpg).createAndApply()

  cpg
}
