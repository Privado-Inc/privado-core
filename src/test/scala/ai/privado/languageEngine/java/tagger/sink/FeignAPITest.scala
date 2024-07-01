package ai.privado.languageEngine.java.tagger.sink

import ai.privado.tagger.sink.api.APIValidator
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language._

class FeignAPITest extends JavaFrontendTestSuite with APIValidator {

  "Feign api tagger" should {
    "be able to tag api call with correct url (Case 1)" in {

      val cpg = code(
        """
          |import dto.ResponseOnboardingDTO;
          |import org.springframework.cloud.openfeign.FeignClient;
          |import org.springframework.data.domain.Pageable;
          |import org.springframework.format.annotation.DateTimeFormat;
          |import org.springframework.http.MediaType;
          |import org.springframework.web.bind.annotation.GetMapping;
          |import org.springframework.web.bind.annotation.PathVariable;
          |import org.springframework.web.bind.annotation.RequestParam;
          |import org.springframework.web.bind.annotation.ResponseBody;
          |
          |import java.time.LocalDateTime;
          |
          |@FeignClient(
          |    name = "${rest-client.onboarding.serviceCode}",
          |    url = "${rest-client.onboarding.uri}")
          |public interface OnboardingRestClient {
          |
          |  @GetMapping(
          |      value = "/idpay/onboarding/{initiativeId}",
          |      produces = MediaType.APPLICATION_JSON_VALUE)
          |  @ResponseBody
          |  ResponseOnboardingDTO getOnboarding(
          |      @PathVariable("initiativeId") String initiativeId,
          |      Pageable pageable,
          |      @RequestParam(required = false) String userId,
          |      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
          |      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
          |      @RequestParam(required = false) String status);
          |
          |}
          |""".stripMargin,
        "OnboardingRestClient.java"
      ).moreCode("""
          |import dto.ResponseOnboardingDTO;
          |import org.springframework.data.domain.Pageable;
          |import org.springframework.stereotype.Service;
          |
          |import java.time.LocalDateTime;
          |
          |@Service
          |public class OnboardingRestConnectorImpl implements OnboardingRestConnector {
          |
          |  private final OnboardingRestClient onboardingRestClient;
          |
          |  public OnboardingRestConnectorImpl(
          |      OnboardingRestClient onboardingRestClient) {
          |    this.onboardingRestClient = onboardingRestClient;
          |  }
          |
          |  @Override
          |  public ResponseOnboardingDTO getOnboarding(String initiativeId, Pageable pageable, String userId,
          |      LocalDateTime startDate, LocalDateTime endDate, String status) {
          |    return onboardingRestClient.getOnboarding(initiativeId,pageable,userId,startDate,endDate,status);
          |  }
          |}
          |""".stripMargin)

      val List(apiCall) = cpg.call("getOnboarding").l
      assertAPISinkCall(apiCall)
      assertAPIEndpointURL(apiCall, "rest-client_onboarding_uri")
    }
  }

}
