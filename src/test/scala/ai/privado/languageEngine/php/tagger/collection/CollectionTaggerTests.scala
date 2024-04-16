package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, SourceCodeModel}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.{PhpFrontendTestSuite, TestCpgWithPhp}
import io.shiftleft.semanticcpg.language.*

class CollectionTaggerTests extends PhpFrontendTestSuite {

  val ruleCache =
    RuleCache()
  ruleCache.setRule(ConfigAndRules(sources = RuleInfoTestData.sourceRule, collections = collectionRules))

  "methods with Route annotations" should {
    "be tagged as part of collection tagger" in {
      val cpg = code("""
          |<?php
          | #[Route('/profile'), IsGranted(User::ROLE_USER)]
          | final class UserController {
          |   #[Route("/edit")]
          |   public function edit($firstName) {
          |      return $firstName;
          |   }
          | }
          |>
          |""".stripMargin)
        .withRuleCache(ruleCache)

      val List(editControllerMethod) = cpg.method("edit").l
      editControllerMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      editControllerMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/profile/edit")
    }
  }

  "controllers defined in yaml files" should {
    "be tagged as part of collection tagger" in {
      val cpg = code(
        """
          |<?php
          |final class SomeController {
          | public function __invoke() {}
          |}
          |>
          |""".stripMargin,
        "test.php"
      ).moreCode("""
          |some:
          |    path: /checkout/{token}/some
          |    methods: [GET]
          |    defaults:
          |        _controller: App\Controller\Checkout\SomeController
          |""".stripMargin)

//      val List(someControllerInvokeMethod) = cpg.typeDecl("SomeController").astChildren.isMethod.name("__invoke").l
//      someControllerInvokeMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(
//        CatLevelOne.COLLECTIONS.name
//      )
//      someControllerInvokeMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/profile/edit")
    }
  }
}
