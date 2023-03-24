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
}
