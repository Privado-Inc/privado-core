package ai.privado.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language._

class InferenceAPIEndpointTaggerTest extends JavaFrontendTestSuite with APIValidator {

  "Inference tagger when used against filterProperty method_full_name_with_literal" should {

    val inferences = List(
      RuleInfo(
        id = "Inferences.API.UserServiceClient.ByLiteral",
        name = "User service client",
        category = "",
        filterProperty = FilterProperty.METHOD_FULL_NAME_WITH_LITERAL,
        domains = Array("http://user-service.com"),
        patterns = List(".*UserServiceClient[.]\\w+[:].*"),
        catLevelOne = CatLevelOne.INFERENCES,
        catLevelTwo = Constants.apiEndpoint
      )
    )
    val ruleCache = RuleCache().withRule(RuleInfoTestData.rule.copy(inferences = inferences))

    val cpg = code("""
        |import com.privado.clients.UserServiceClient;
        |public class Main {
        |    public static void main(String[] args) {
        |        try {
        |            UserServiceClient client = new UserServiceClient();
        |            String usersResponse = client.getUsers();
        |            System.out.println(usersResponse);
        |        } catch (Exception e) {
        |            e.printStackTrace();
        |        }
        |    }
        |}
        |""".stripMargin).withRuleCache(ruleCache)

    "tag the api sink matching fullName" in {
      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUsersCall)
    }

    "tag the api sink with passed literal" in {
      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPIEndpointURL(getUsersCall, "http://user-service.com")
    }
  }

  "Inference tagger when used against filterProperty method_full_name_with_property_name" should {

    val inferences = List(
      RuleInfo(
        id = "Inferences.API.UserServiceClient.ByPropertyName",
        name = "User service client",
        category = "",
        filterProperty = FilterProperty.METHOD_FULL_NAME_WITH_PROPERTY_NAME,
        domains = Array(".*user.*url"),
        patterns = List(".*UserServiceClient[.]\\w+[:].*"),
        catLevelOne = CatLevelOne.INFERENCES,
        catLevelTwo = Constants.apiEndpoint
      )
    )
    val ruleCache = RuleCache().withRule(RuleInfoTestData.rule.copy(inferences = inferences))

    val cpg = code("""
        |import com.privado.clients.UserServiceClient;
        |public class Main {
        |    public static void main(String[] args) {
        |        try {
        |            UserServiceClient client = new UserServiceClient();
        |            String usersResponse = client.getUsers();
        |            System.out.println(usersResponse);
        |        } catch (Exception e) {
        |            e.printStackTrace();
        |        }
        |    }
        |}
        |""".stripMargin)
      .moreCode(
        """
        |user.service.url = http://user-service.com
        |""".stripMargin,
        "application.properties"
      )
      .withRuleCache(ruleCache)

    "tag the api sink matching fullName" in {
      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUsersCall)
    }

    "tag the api sink with passed literal" in {
      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPIEndpointURL(getUsersCall, "http://user-service.com")
    }
  }
}
