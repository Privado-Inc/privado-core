package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, Language, NodeType, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.sink.api.APIValidator
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class JavaAPIRetrofitTaggerTest extends JavaFrontendTestSuite with APIValidator {

  "Java api retrofit tagger (case - finding url by literal)" should {
    val cpg = code(
      """
        |import retrofit2.Call;
        |import retrofit2.http.GET;
        |import java.util.List;
        |
        |public interface UserService {
        |    @GET("users")
        |    Call<List<User>> getUsers();
        |}
        |
        |""".stripMargin,
      "UserService.java"
    ).moreCode(
      """
        |import retrofit2.Retrofit;
        |import retrofit2.converter.gson.GsonConverterFactory;
        |
        |public class RetrofitClient {
        |
        |    public static UserService getUserService() {
        |        return new Retrofit.Builder()
        |                    .baseUrl("https://api.example.com/")
        |                    .addConverterFactory(GsonConverterFactory.create())
        |                    .build()
        |                    .create(UserService.class);
        |    }
        |}
        |""".stripMargin,
      "RetrofitClient.java"
    ).moreCode(
      """
        |import retrofit2.Call;
        |import retrofit2.Callback;
        |import retrofit2.Response;
        |
        |public class Main {
        |    public static void main(String[] args) {
        |        UserService userService = RetrofitClient.getUserService();
        |
        |        Call<List<User>> call = userService.getUsers();
        |        call.enqueue(new Callback<List<User>>() {
        |            @Override
        |            public void onResponse(Call<List<User>> call, Response<List<User>> response) {
        |                if (response.isSuccessful()) {
        |                    List<User> users = response.body();
        |                    for (User user : users) {
        |                        System.out.println(user.getName());
        |                    }
        |                } else {
        |                    System.out.println("Failed to fetch users: " + response.message());
        |                }
        |            }
        |
        |            @Override
        |            public void onFailure(Call<List<User>> call, Throwable t) {
        |                System.out.println("Failed to fetch users: " + t.getMessage());
        |            }
        |        });
        |    }
        |}
        |
        |""".stripMargin,
      "Main.java"
    )

    "tag retrofit sink as api sink" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUserCall)
    }

    "tag retrofit sink with endpoint" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPIEndpointURL(getUserCall, "\"https://api.example.com/\"")
    }
  }

  "Java api retrofit tagger (case - finding url by method parameter regex match)" should {

    val systemConfig = List(SystemConfig(Constants.apiIdentifier, "(?i).*endpoint", Language.JAVA, "", Array()))
    val ruleCache    = RuleCache().withRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))

    val cpg = code(
      """
        |import retrofit2.Call;
        |import retrofit2.http.GET;
        |import java.util.List;
        |
        |public interface UserService {
        |    @GET("users")
        |    Call<List<User>> getUsers();
        |}
        |
        |""".stripMargin,
      "UserService.java"
    ).moreCode(
      """
        |import retrofit2.Retrofit;
        |import retrofit2.converter.gson.GsonConverterFactory;
        |
        |public class RetrofitClient {
        |
        |    public static UserService getUserService(String exampleAPIEndpoint) {
        |        return new Retrofit.Builder()
        |                    .baseUrl(exampleAPIEndpoint)
        |                    .addConverterFactory(GsonConverterFactory.create())
        |                    .build()
        |                    .create(UserService.class);
        |    }
        |}
        |""".stripMargin,
      "RetrofitClient.java"
    ).moreCode(
      """
        |import retrofit2.Call;
        |import retrofit2.Callback;
        |import retrofit2.Response;
        |
        |public class Main {
        |    public static void main(String[] args) {
        |        UserService userService = RetrofitClient.getUserService("someXYZEndpoint");
        |
        |        Call<List<User>> call = userService.getUsers();
        |        call.enqueue(new Callback<List<User>>() {
        |            @Override
        |            public void onResponse(Call<List<User>> call, Response<List<User>> response) {
        |                if (response.isSuccessful()) {
        |                    List<User> users = response.body();
        |                    for (User user : users) {
        |                        System.out.println(user.getName());
        |                    }
        |                } else {
        |                    System.out.println("Failed to fetch users: " + response.message());
        |                }
        |            }
        |
        |            @Override
        |            public void onFailure(Call<List<User>> call, Throwable t) {
        |                System.out.println("Failed to fetch users: " + t.getMessage());
        |            }
        |        });
        |    }
        |}
        |
        |""".stripMargin,
      "Main.java"
    ).withRuleCache(ruleCache)

    "tag retrofit sink as api sink" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUserCall)
    }

    "tag retrofit sink with endpoint" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPIEndpointURL(getUserCall, "exampleAPIEndpoint")
    }
  }

  "Java api retrofit tagger (case - finding url by matching identifier in method)" should {

    val systemConfig = List(SystemConfig(Constants.apiIdentifier, "(?i).*endpoint", Language.JAVA, "", Array()))
    val ruleCache    = RuleCache().withRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))

    val cpg = code(
      """
        |import retrofit2.Call;
        |import retrofit2.http.GET;
        |import java.util.List;
        |
        |public interface UserService {
        |    @GET("users")
        |    Call<List<User>> getUsers();
        |}
        |
        |""".stripMargin,
      "UserService.java"
    ).moreCode(
      """
        |import retrofit2.Retrofit;
        |import retrofit2.converter.gson.GsonConverterFactory;
        |
        |public class RetrofitClient {
        |
        |    public static UserService getUserService() {
        |    String exampleAPIEndpoint = "someXYZEndpoint";
        |
        |        return new Retrofit.Builder()
        |                    .baseUrl(exampleAPIEndpoint)
        |                    .addConverterFactory(GsonConverterFactory.create())
        |                    .build()
        |                    .create(UserService.class);
        |    }
        |}
        |""".stripMargin,
      "RetrofitClient.java"
    ).moreCode(
      """
        |import retrofit2.Call;
        |import retrofit2.Callback;
        |import retrofit2.Response;
        |
        |public class Main {
        |    public static void main(String[] args) {
        |        UserService userService = RetrofitClient.getUserService();
        |
        |        Call<List<User>> call = userService.getUsers();
        |        call.enqueue(new Callback<List<User>>() {
        |            @Override
        |            public void onResponse(Call<List<User>> call, Response<List<User>> response) {
        |                if (response.isSuccessful()) {
        |                    List<User> users = response.body();
        |                    for (User user : users) {
        |                        System.out.println(user.getName());
        |                    }
        |                } else {
        |                    System.out.println("Failed to fetch users: " + response.message());
        |                }
        |            }
        |
        |            @Override
        |            public void onFailure(Call<List<User>> call, Throwable t) {
        |                System.out.println("Failed to fetch users: " + t.getMessage());
        |            }
        |        });
        |    }
        |}
        |
        |""".stripMargin,
      "Main.java"
    ).withRuleCache(ruleCache)

    "tag retrofit sink as api sink" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPISinkCall(getUserCall)
    }

    "tag retrofit sink with endpoint" in {
      val List(getUserCall) = cpg.call("getUsers").l
      assertAPIEndpointURL(getUserCall, "exampleAPIEndpoint")
    }
  }
}
