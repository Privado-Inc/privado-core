package ai.privado.languageEngine.php.tagger.collection

import ai.privado.tagger.sink.api.CollectionValidator
import ai.privado.testfixtures.PhpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class MethodFullNameCollectionTaggerTests extends PhpFrontendTestSuite with CollectionValidator {
  "collection methods from Slim" should {
    "tag controllers" in {
      val cpg = code("""
                       |<?php
                       |use Slim\App;
                       |use Slim\CallableResolver;
                       |use Slim\Exception\HttpNotFoundException;
                       |use Slim\Handlers\ErrorHandler;
                       |use Slim\Psr7\Factory\ResponseFactory;
                       |use Slim\Routing\RouteCollector;
                       |use Slim\Routing\RouteCollectorProxy;
                       |use Pimple\Psr11\Container as PsrContainer;
                       |use Psr\Log\NullLogger;
                       |use App\SController as SomeController;
                       |
                       |class Router {
                       |
                       |  public function create() {
                       |        $app = new App();
                       |       $app->post('/test-route', SomeController::class)
                       |            ->setName('image_crop');
                       |   }
                       |}
                       |>
                       |""".stripMargin).moreCode("""
                                                    |<?php
                                                    |
                                                    |namespace App;
                                                    |
                                                    |class SController extends RequestHandlerInterface {
                                                    | public function handle(Request $request) {
                                                    |   $salary = $request->salary;
                                                    | }
                                                    |}
                                                    |
                                                    |>
                                                    |""".stripMargin)

      val List(controllerMethod) = cpg.method("handle").l
      assertCollectionMethod(controllerMethod)
      assertCollectionUrl(controllerMethod, "/test-route")
      assertCollectionInFinalJson(cpg, 1)
    }

    "tag controllers and get correct route from assignment" in {
      val cpg = code("""
          |<?php
          |use Slim\App;
          |use Slim\CallableResolver;
          |use Slim\Exception\HttpNotFoundException;
          |use Slim\Handlers\ErrorHandler;
          |use Slim\Psr7\Factory\ResponseFactory;
          |use Slim\Routing\RouteCollector;
          |use Slim\Routing\RouteCollectorProxy;
          |use Pimple\Psr11\Container as PsrContainer;
          |use Psr\Log\NullLogger;
          |use App\SController as SomeController;
          |
          |class Router {
          |
          |  private const MY_ROUTE = self::HOME_ROUTE . '/route';
          |  public function create() {
          |       $app = new App();
          |       $app->post(self::MY_ROUTE, SomeController::class)
          |            ->setName('image_crop');
          |   }
          |}
          |>
          |""".stripMargin).moreCode("""
          |<?php
          |
          |namespace App;
          |
          |class SController extends RequestHandlerInterface {
          | public function handle(Request $request) {
          |   $salary = $request->salary;
          | }
          |}
          |
          |>
          |""".stripMargin)

      val List(controllerMethod) = cpg.method("handle").l
      assertCollectionMethod(controllerMethod)
      assertCollectionUrl(controllerMethod, "/route")
      assertCollectionInFinalJson(cpg, 1)
    }
  }
}
