package ai.privado.languageEngine.php.tagger.collection

import ai.privado.testfixtures.PhpFrontendTestSuite
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.tagger.sink.api.CollectionValidator
import io.shiftleft.semanticcpg.language.*

class ConfigCollectionTaggerTests extends PhpFrontendTestSuite with CollectionValidator {
  "controllers defined in yaml files" should {
    "be tagged as part of collection tagger (where controller is defined without alias and default method)" in {
      val cpg = code(
        """
          |<?php
          |final class SomeController {
          | public function __invoke($firstName) {
          |   return $firstName;
          | }
          |}
          |>
          |""".stripMargin,
        "test.php"
      ).moreCode(
        """
          |some_list:
          |    path: /checkout/{token}/accounts
          |    methods: GET
          |    controller: Abc\Def\SomeController::search
          |    requirements:
          |        type: '[a-z-]+'
          |""".stripMargin,
        "routes.yaml"
      )

      val List(someControllerInvokeMethod) = cpg.typeDecl("SomeController").astChildren.isMethod.name("__invoke").l
      assertCollectionMethod(someControllerInvokeMethod)
      assertCollectionUrl(someControllerInvokeMethod, "/checkout/{token}/accounts")
      assertCollectionInFinalJson(cpg, 1)
    }

    "be tagged as part of collection tagger (where controller is defined without alias and non-default method)" in {
      val cpg = code(
        """
          |<?php
          |final class SomeController {
          | public function search($firstName) {
          |   return $firstName;
          | }
          |}
          |>
          |""".stripMargin,
        "test.php"
      ).moreCode(
        """
          |some_list:
          |    path: /{dynamic}
          |    methods: GET
          |    controller: Abc\Def\SomeController::search
          |    requirements:
          |        type: '[a-z-]+'
          |""".stripMargin,
        "routes.yaml"
      )

      val List(someControllerInvokeMethod) = cpg.typeDecl("SomeController").astChildren.isMethod.name("search").l
      assertCollectionMethod(someControllerInvokeMethod)
      assertCollectionUrl(someControllerInvokeMethod, "/{dynamic}")
      assertCollectionInFinalJson(cpg, 1)
    }
  }

  "be tagged as part of collection tagger (where controller is defined with an alias and non-default method)" in {
    val cpg = code(
      """
        |<?php
        |final class SomeController {
        | public function __invoke($firstName) {
        |   return $firstName;
        | }
        |}
        |>
        |""".stripMargin,
      "test.php"
    ).moreCode(
      """
        |app.some_feature:
        |    path: /some/api/route
        |    methods: [GET]
        |    defaults:
        |        _controller: 'app.some_controller_alias'
        |""".stripMargin,
      "routes.yaml"
    ).moreCode(
      """
        |app.some_controller_alias:
        |   class: App\Controller\SomeController
        |""".stripMargin,
      "controller_service.yaml"
    )

    val List(someControllerInvokeMethod) = cpg.typeDecl("SomeController").astChildren.isMethod.name("__invoke").l
    assertCollectionMethod(someControllerInvokeMethod)
    assertCollectionUrl(someControllerInvokeMethod, "/some/api/route")
    assertCollectionInFinalJson(cpg, 1)
  }
}
