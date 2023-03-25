package ai.privado.languageEngine.java.audit.TestData

object AuditTestClassData {

  val user =
    """
      |package com.ai.privado.Entity;
      |
      |@Entity
      |public class User {
      |   public String firstName;
      |
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;}
      |}
      |""".stripMargin

  val account =
    """
      |package com.ai.privado.Entity;
      |
      |@Entity
      |public class Account {
      |   public String accountNo;
      |
      |   public void setAccountNo(String accountNo) {this.accountNo = accountNo;}
      |}
      |""".stripMargin

  val address =
    """
      |package com.ai.privado.Entity;
      |
      |public class Address {
      |   public String houseNo;
      |}
      |""".stripMargin

  val userController =
    """
      |package com.ai.privado.Controller;
      |
      |import com.ai.privado.Entity.User;
      |import org.springframework.web.bind.annotation.*;
      |
      |@RestController
      |@RequestMapping("/user")
      |public class UserController {
      |
      |   @PostMapping("/add")
      |   public String userHandler(@RequestBody User user) {
      |     return user.getFirstName();
      |   }
      |}
      |""".stripMargin

  val salaryLombok =
    """
      |package com.ai.privado.Entity;
      |
      |import lombok.Getter;
      |import lombok.Setter;
      |
      |@Getter
      |@Setter
      |public class Salary {
      |
      |   public String payment;
      |
      |}
      |""".stripMargin

  val addressController =
    """
      |package com.ai.privado.Controller;
      |
      |public class AddressController {
      |
      |   public String addressInfo;
      |
      |   public String getAddressInfo() {return addressInfo;}
      |}
      |""".stripMargin
}
