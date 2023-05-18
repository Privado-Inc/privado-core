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

  val parentPOMFile =
    """
      |<project xmlns="http://maven.apache.org/POM/4.0.0"
      |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
      |         http://maven.apache.org/maven-v4_0_0.xsd">
      |
      |  <modelVersion>4.0.0</modelVersion>
      |  <groupId>com.example</groupId>
      |  <artifactId>my-java-project</artifactId>
      |  <version>1.0.0</version>
      |
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.springframework</groupId>
      |      <artifactId>spring-core</artifactId>
      |      <version>5.3.9</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>junit</groupId>
      |      <artifactId>junit</artifactId>
      |      <version>4.13.2</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |     <groupId>org.elasticsearch.client</groupId>
      |     <artifactId>rest</artifactId>
      |     <version>5.5.3</version>
      |    </dependency>
      |    <dependency>
      |     <groupId>org.http4k</groupId>
      |     <artifactId>http4k-connect-core</artifactId>
      |     <version>3.39.2.0</version>
      |    </dependency>
      |    <dependency>
      |     <groupId>com.github.scala-incubator.io</groupId>
      |     <artifactId>scala-io-file_2.11</artifactId>
      |     <version>0.4.3-1</version>
      |    </dependency>
      |  </dependencies>
      |
      |</project>
      |""".stripMargin

  val person =
    """
      |package com.test.privado.audit;
      |
      |public class Person {
      |   public String firstName;
      |   public String password;
      |
      |   public Person(String firstName, String password) {
      |     this.firstName = firstName;
      |     this.password = password;
      |   }
      |
      |   public String getFirstName() {return firstName;}
      |   public String getPassword() {return password;}
      |}
      |""".stripMargin

  val filter2File =
    """package com.test.privado.audit;
      |
      |import org.apache.logging.log4j.Logger;
      |
      |public class Filter2File {
      |
      |   private static final Logger logger = LogManager.getLogger("HelloWorld");
      |
      |   public void process() {
      |     Person person1 = new Person("name", "password");
      |     String firstName = person1.getFirstName();
      |
      |     logger.info(firstName);
      |   }
      |}
      |""".stripMargin

  val teacher =
    """
      |package com.test.privado.audit;
      |
      |public class Teacher {
      |   public String firstName;
      |   public String password;
      |
      |   public Teacher(String firstName, String password) {
      |     this.firstName = firstName;
      |     this.password = password;
      |   }
      |
      |   public String getFirstName() {return firstName;}
      |   public String getPassword() {return password;}
      |}
      |""".stripMargin

  val dedup2File =
    """package com.test.privado.audit;
      |
      |import org.apache.logging.log4j.Logger;
      |
      |public class Dedup2File {
      |
      |   private static final Logger logger = LogManager.getLogger("HelloWorld");
      |
      |   public void process1() {
      |
      |     Teacher teacher1 = new Teacher("personName", "Password");
      |     String firstName1 = teacher1.getFirstName();
      |     firstName1 = firstName1 + "value";
      |
      |     logFirstName(firstName1);
      |   }
      |
      |   public void process2() {
      |     Teacher teacher2 = new Teacher("NewPersonName", "NewPassword");
      |     String firstName2 = teacher2.getFirstName();
      |
      |     logFirstName(firstName2);
      |   }
      |
      |   public void logFirstName(String name) {
      |     logger.info(name);
      |   }
      |}
      |
      |""".stripMargin

  val userSemantic =
    """
      |package com.test.privado.audit;
      |
      |import org.apache.logging.log4j.Logger;
      |
      |public class BaseClass {
      |   private static final Logger logger = LogManager.getLogger("HelloWorld");
      |   public String firstName;
      |   public String id;
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;}
      |   public String getId() {return id;}
      |   public void setId(String id) {this.id = id;}
      |   public foo1() {
      |     BaseClass b = new BaseClass();
      |     b.setFirstName("Alex");
      |     b.setId("101");
      |     String newValue1 = b.getFirstName();
      |     String newValue2 = b.getId();
      |
      |     logger.info(newValue1);
      |     logger.info(newValue2);
      |   }
      |}
      |
      |""".stripMargin

  val unresolvedBaseClass =
    """package com.test.privado.audit;
      |
      |import com.test.privado.audit.UnresolvedTeacherClass;
      |import org.apache.logging.log4j.Logger;
      |
      |import java.util.Map;
      |import java.util.ArrayList;
      |
      |public class UnresolvedBaseClass {
      |
      |   public void process1() {
      |
      |     String firstName = "name";
      |     String first = UnresolvedTeacherClass.builder().firstName(firstName);
      |   }
      |
      |}
      |
      |""".stripMargin
}
