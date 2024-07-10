package ai.privado.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, RuleInfo, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

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

    val ruleCache = RuleCache().setRule(RuleInfoTestData.rule.copy(inferences = inferences))

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
    val ruleCache = RuleCache().setRule(RuleInfoTestData.rule.copy(inferences = inferences))

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

  "Inference tagger when used against filterProperty endpoint_domain_with_literal" should {

    val inferences = List(
      RuleInfo(
        id = "Inferences.API.UserServiceUrl.ByLiteral",
        name = "User service url",
        category = "",
        filterProperty = FilterProperty.ENDPOINT_DOMAIN_WITH_LITERAL,
        domains = Array("http://user-service.com"),
        patterns = List("userServiceUrl"),
        catLevelOne = CatLevelOne.INFERENCES,
        catLevelTwo = Constants.apiEndpoint
      )
    )

    val systemConfig = List(SystemConfig(Constants.apiIdentifier, ".*Url"))
    val ruleCache =
      RuleCache().setRule(RuleInfoTestData.rule.copy(inferences = inferences, systemConfig = systemConfig))

    val cpg = code("""
        |import org.apache.http.client.fluent.Request;
        |import java.io.IOException;
        |import io.privado.urls.userServiceUrl;
        |
        |public class Main {
        |    public static void main(String[] args) throws IOException {
        |
        |        String responseBody = Request.Get(userServiceUrl).execute().returnContent().asString();
        |        System.out.println("Response: " + responseBody);
        |    }
        |}
        |
        |""".stripMargin).withRuleCache(ruleCache)

    "tag the api sink matching fullName" in {
      val List(getUsersCall) = cpg.call("Get").l
      assertAPISinkCall(getUsersCall)
    }

    "tag the api sink with passed literal" in {
      val List(getUsersCall) = cpg.call("Get").l
      assertAPIEndpointURL(getUsersCall, "http://user-service.com")
    }
  }

  "Inference tagger when used against filterProperty endpoint_domain_with_property_name" should {

    val inferences = List(
      RuleInfo(
        id = "Inferences.API.UserServiceUrl.ByPropertyName",
        name = "User service url",
        category = "",
        filterProperty = FilterProperty.ENDPOINT_DOMAIN_WITH_PROPERTY_NAME,
        domains = Array(".*user.*url"),
        patterns = List(".*userServiceUrl"),
        catLevelOne = CatLevelOne.INFERENCES,
        catLevelTwo = Constants.apiEndpoint
      )
    )
    val systemConfig = List(SystemConfig(Constants.apiIdentifier, ".*Url"))
    val ruleCache =
      RuleCache().setRule(RuleInfoTestData.rule.copy(inferences = inferences, systemConfig = systemConfig))

    val cpg = code("""
        |import org.apache.http.client.fluent.Request;
        |import java.io.IOException;
        |import io.privado.urls.userServiceUrl;
        |
        |public class Main {
        |    public static void main(String[] args) throws IOException {
        |
        |        String responseBody = Request.Get(userServiceUrl).execute().returnContent().asString();
        |        System.out.println("Response: " + responseBody);
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
      val List(getUsersCall) = cpg.call("Get").l
      assertAPISinkCall(getUsersCall)
    }

    "tag the api sink with passed literal" in {
      val List(getUsersCall) = cpg.call("Get").l
      assertAPIEndpointURL(getUsersCall, "http://user-service.com")
    }
  }

  "Inference tagger overlapping with Api tagger" should {

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

    val systemConfig = List(
      SystemConfig(Constants.apiHttpLibraries, ".*UserServiceClient.*"),
      SystemConfig(Constants.apiSinks, "getUsers")
    )

    "tag the api sink once without inference rule" in {

      val ruleCache = RuleCache().setRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))
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

      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUsersCall)
      assertCallByTag(getUsersCall, values = List("Sinks.ThirdParties.API"))
    }

    "tag the api sink once with inference rule" in {

      val ruleCache =
        RuleCache().setRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig, inferences = inferences))
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

      val List(getUsersCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUsersCall)
      assertCallByTag(getUsersCall, values = List("Sinks.ThirdParties.API.user-service.com"))
    }
  }
}
