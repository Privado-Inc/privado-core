package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{
  CatLevelOne,
  Constants,
  DataFlowPathModel,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SourceCodeModel
}
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.sink.api.CollectionValidator
import ai.privado.testfixtures.PhpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class AnnotationsCollectionTaggerTests extends PhpFrontendTestSuite with CollectionValidator {

  val collectionRules: List[RuleInfo] = List(
    RuleInfo(
      "Collections.Symfony",
      "Symfony MVC Endpoints",
      "",
      FilterProperty.CODE,
      Array(),
      List("(?i).*(Route).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.PHP,
      Array()
    )
  )

  val ruleCache =
    RuleCache()
  ruleCache.setRule(RuleInfoTestData.rule.copy(collections = collectionRules))

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
      assertCollectionMethod(editControllerMethod)
      assertCollectionUrl(editControllerMethod, "/profile/edit")
      assertCollectionInFinalJson(cpg, 1)
    }
  }
}
