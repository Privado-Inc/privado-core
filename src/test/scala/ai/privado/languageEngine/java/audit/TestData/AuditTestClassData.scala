package ai.privado.languageEngine.java.audit.TestData

object AuditTestClassData {

  val user =
    """
      |package com.test.privado.Entity;
      |
      |public class User {
      |   public String firstName;
      |
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;}
      |}
      |""".stripMargin

  val account =
    """
      |package com.test.privado.Entity;
      |
      |public class Account {
      |   public String accountNo;
      |
      |   public void setAccountNo(String accountNo) {this.accountNo = accountNo;}
      |}
      |""".stripMargin

  val address =
    """
      |package com.test.privado.Entity;
      |
      |public class Address {
      |   public String houseNo;
      |
      |   public String giveHouseNo() {
      |     return houseNo;
      |   }
      |}
      |""".stripMargin

  val userController =
    """
      |package com.test.privado.Controller;
      |
      |import com.test.privado.Entity.User;
      |import org.springframework.web.bind.annotation.PostMapping;
      |import org.springframework.web.bind.annotation.RequestBody;
      |import org.springframework.web.bind.annotation.RequestMapping;
      |import org.springframework.web.bind.annotation.RestController;
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
      |package com.test.privado.Entity;
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
      |package com.test.privado.Controller;
      |
      |public class AddressController {
      |
      |   public String addressInfo;
      |
      |   public String getAddressInfo() {return addressInfo;}
      |}
      |""".stripMargin

  val invoice =
    """
      |package com.test.privado.Entity;
      |
      |public class Invoice {
      |
      |   public String invoiceNo;
      |
      |   @Override
      |    public int hashCode() {
      |        return super.hashCode();
      |    }
      |
      |    @Override
      |    public boolean equals(Object obj) {
      |        return super.equals(obj);
      |    }
      |}
      |""".stripMargin

  val adminDao =
    """
      |package com.test.privado.Dao;
      |
      |public class AdminDao {
      |
      |   public String adminName;
      |
      |   public String getAdminName() {
      |     return adminName;
      |   }
      |}
      |""".stripMargin
}
