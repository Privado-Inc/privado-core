package ai.privado.threatEngine

import ai.privado.languageEngine.go.GoTestBase
import ai.privado.model.*
import ai.privado.model.exporter.ViolationModel
import ai.privado.threatEngine.ThreatEngineExecutor

import scala.collection.immutable.Map

class GoThreatTests extends GoTestBase {

  "Validate Threat  PIIShouldNotBePresentInMultipleTables" should {
    val threat = PolicyOrThreat(
      "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables",
      "{DataElement} was found in multiple tables",
      "{DataElement} found in multiple tables",
      """
        |Avoid storing same PII in multiple tables.
        |Reference link: https://github.com/OWASP/owasp-mstg/blob/v1.4.0/Document/0x05d-Testing-Data-Storage.md#testing-local-storage-for-sensitive-data-mstg-storage-1-and-mstg-storage-2
        |""".stripMargin,
      PolicyThreatType.THREAT,
      PolicyAction.DENY,
      DataFlow(
        List(),
        SourceFilter(Option(true), "", ""),
        List[String](),
        SinkFilter(List[String](), "", ""),
        CollectionFilter("", "")
      ),
      List("**"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    "When same data-element is part of multiple table in go file" in {

      val (_, threatEngine) = code("""
          |package models
          |
          |import (
          |	"log"
          |	"time"
          |
          |	"github.com/google/uuid"
          |	"github.com/jinzhu/gorm"
          |)
          |
          |// User Model
          |type User struct {
          |	// we need to use our own custom model because the id is different of the "gorm.Model"
          |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
          |	Name    string `gorm:"type:varchar(255); NOT NULL" json:"name" validate:"required"`
          |	Email   string `gorm:"type:varchar(255)" json:"email"`
          |	Phone   string `gorm:"type:varchar(100); NOT NULL; UNIQUE; UNIQUE_INDEX" json:"phone" validate:"required"`
          |	Address string `gorm:"type:text" json:"address"`
          |	MyModel
          |}
          |
          |// User Model
          |type Student struct {
          |	// we need to use our own custom model because the id is different of the "gorm.Model"
          |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
          |	Name    string `gorm:"type:varchar(255); NOT NULL" json:"name" validate:"required"`
          |	Email   string `gorm:"type:varchar(255)" json:"email"`
          |	Address string `gorm:"type:text" json:"address"`
          |	MyModel
          |}
          |
          |// Check if Users Model implements/match Model interface
          |var _ Model = &User{}
          |
          |
          |// FindOneByID gets a user by id
          |func (u *User) FindOneByID(db *gorm.DB) (Model, error) {
          |	err := db.Find(&u).Error
          |	if err != nil {
          |		return nil, err
          |	}
          |	return u, nil
          |}
          |
          |
          |// FindOneByID gets a user by id
          |func (u *Student) FindOneByID(db *gorm.DB) (Model, error) {
          |	err := db.Find(&u).Error
          |	if err != nil {
          |		return nil, err
          |	}
          |	return u, nil
          |}
          |
          |""".stripMargin)

      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables"
      result.get.processing.get.head.sourceId shouldBe "EmailAddress"
    }

    "When same data-element is part of multiple table in sql file" in {

      val (_, threatEngine) = code(
        """
          |CREATE TABLE IF NOT EXISTS Customer (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		email VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |
          |CREATE TABLE IF NOT EXISTS User (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		email VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |""".stripMargin,
        fileExtension = ".sql"
      )

      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables"
      result.get.processing.get.head.sourceId shouldBe "EmailAddress"
    }

    "When same data-element is not part of multiple table in go file" in {

      val (_, threatEngine) = code("""
          |package models
          |
          |import (
          |	"log"
          |	"time"
          |
          |	"github.com/google/uuid"
          |	"github.com/jinzhu/gorm"
          |)
          |
          |// User Model
          |type User struct {
          |	// we need to use our own custom model because the id is different of the "gorm.Model"
          |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
          |	Phone   string `gorm:"type:varchar(100); NOT NULL; UNIQUE; UNIQUE_INDEX" json:"phone" validate:"required"`
          |	Address string `gorm:"type:text" json:"address"`
          |	MyModel
          |}
          |
          |// User Model
          |type Student struct {
          |	// we need to use our own custom model because the id is different of the "gorm.Model"
          |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
          |	Name    string `gorm:"type:varchar(255); NOT NULL" json:"name" validate:"required"`
          |	Email   string `gorm:"type:varchar(255)" json:"email"`
          |	MyModel
          |}
          |
          |// Check if Users Model implements/match Model interface
          |var _ Model = &User{}
          |
          |
          |// FindOneByID gets a user by id
          |func (u *User) FindOneByID(db *gorm.DB) (Model, error) {
          |	err := db.Find(&u).Error
          |	if err != nil {
          |		return nil, err
          |	}
          |	return u, nil
          |}
          |
          |
          |// FindOneByID gets a user by id
          |func (u *Student) FindOneByID(db *gorm.DB) (Model, error) {
          |	err := db.Find(&u).Error
          |	if err != nil {
          |		return nil, err
          |	}
          |	return u, nil
          |}
          |
          |""".stripMargin)

      val result = threatEngine.processProcessingViolations(threat)
      assert(result.isEmpty)
    }
  }

  "Threat DifferentKindOfPIIStoredInDifferentTables" should {
    val threat = PolicyOrThreat(
      "PrivadoPolicy.Storage.IsDifferentKindOfPIIStoredInDifferentTables",
      "Table containing multiple PIIs must be of same category",
      "Multiple PII categories saved to {TableName}",
      "Create separate tables for fields belongs to different categories.",
      PolicyThreatType.THREAT,
      PolicyAction.DENY,
      DataFlow(
        List(),
        SourceFilter(Option(true), "", ""),
        List[String](),
        SinkFilter(List[String](), "", ""),
        CollectionFilter("", "")
      ),
      List("**"),
      Map[String, String](),
      Map[String, String](
        "PersonalCharacteristics" -> "Data.Sensitive.PersonalIdentification.FirstName,Data.Sensitive.PersonalIdentification.LastName,Data.Sensitive.PersonalIdentification.DateofBirth,Data.Sensitive.PersonalCharacteristics.Height,Data.Sensitive.PersonalCharacteristics.Weigth,Data.Sensitive.ContactData.PhoneNumber,Data.Sensitive.ContactData.Address,Data.Sensitive.NationalIdentificationNumbers.SocialSecurityNumber,Data.Sensitive.NationalIdentificationNumbers.TaxpayerIdentificationNumber",
        "PurchaseData" -> "Data.Sensitive.PurchaseData.OrderDetails,Data.Sensitive.PurchaseData.OfferDetails,Data.Sensitive.PurchaseData.ProductReturnHistory,Data.Sensitive.PurchaseData.PurchaseHistory",
        "FinancialData" -> "Data.Sensitive.FinancialData.BankAccountDetails,Data.Sensitive.FinancialData.CardNumber,Data.Sensitive.FinancialData.CreditScore,Data.Sensitive.FinancialData.Salary"
      ),
      "",
      Array[String]()
    )

    "Identify violation when multiple PIIs of different category stored in the same table in SQL Code" in {
      val (_, threatEngine) = code(
        """
          |CREATE TABLE IF NOT EXISTS Customer (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		address VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |
          |CREATE TABLE IF NOT EXISTS User (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |     firstName VARCHAR(50) NOT NULL
          |		email VARCHAR(50) NOT NULL,
          |     salary int,
          |		PRIMARY KEY (id)
          |	);
          |""".stripMargin,
        fileExtension = ".sql"
      )
      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsDifferentKindOfPIIStoredInDifferentTables"
      result.get.processing.get.head.sourceId shouldBe "User"
    }

    "Identify violation when multiple PIIs of different category stored in the same table with Go Mongo driver" in {
      val (_, threatEngine) = code(
        """
          |package main
          |
          |import (
          |    "context"
          |    "fmt"
          |    "log"
          |
          |    "go.mongodb.org/mongo-driver/bson"
          |    "go.mongodb.org/mongo-driver/mongo"
          |    "go.mongodb.org/mongo-driver/mongo/options"
          |)
          |
          |// Book - We will be using this Book type to perform crud operations
          |type UserMongo struct {
          |  firstName     string
          |  email    string
          |  salary      string
          |  lastName string
          |}
          |
          |func main() {
          |
          |  // Set client options
          |  clientOptions := options.Client().ApplyURI("mongodb://localhost:27017")
          |
          |  // Connect to MongoDB
          |  client, err := mongo.Connect(context.TODO(), clientOptions)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  // Check the connection
          |  err = client.Ping(context.TODO(), nil)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  fmt.Println("Connected to MongoDB!")
          |  usersCollection := client.Database("testdb").Collection("users")
          |  // Insert One document
          |  user1 := UserMongo{"Some first name", "test@email.com", "10000", "Some last name"}
          |  insertResult, err := usersCollection.InsertOne(context.TODO(), user1)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
        downloadDependency = true
      )
      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsDifferentKindOfPIIStoredInDifferentTables"
      result.get.processing.get.head.sourceId shouldBe "UserMongo"
    }
    "No violation reported when multiple PIIs of same kind stored in the same table with Go Mongo driver" in {
      val (_, threatEngine) = code(
        """
          |package main
          |
          |import (
          |    "context"
          |    "fmt"
          |    "log"
          |
          |    "go.mongodb.org/mongo-driver/bson"
          |    "go.mongodb.org/mongo-driver/mongo"
          |    "go.mongodb.org/mongo-driver/mongo/options"
          |)
          |
          |// Book - We will be using this Book type to perform crud operations
          |type UserMongo struct {
          |  firstName     string
          |  email    string
          |  dob      string
          |  lastName string
          |}
          |
          |func main() {
          |
          |  // Set client options
          |  clientOptions := options.Client().ApplyURI("mongodb://localhost:27017")
          |
          |  // Connect to MongoDB
          |  client, err := mongo.Connect(context.TODO(), clientOptions)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  // Check the connection
          |  err = client.Ping(context.TODO(), nil)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  fmt.Println("Connected to MongoDB!")
          |  usersCollection := client.Database("testdb").Collection("users")
          |  // Insert One document
          |  user1 := UserMongo{"Some first name", "test@email.com", "1234567890", "Some last name"}
          |  insertResult, err := usersCollection.InsertOne(context.TODO(), user1)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
        downloadDependency = true
      )
      val result = threatEngine.processProcessingViolations(threat)
      result shouldBe empty
    }
  }

  "Threat PIIHavingDifferentRetentionPeriod" should {
    val threat = PolicyOrThreat(
      "PrivadoPolicy.Storage.IsPIIHavingDifferentRetentionPeriod",
      "If table has multiple PII elements in the same row with different retention policies, store elements in a different table with elements that share the same retention policy",
      "Retention policies for all field must match",
      "Create separate tables for fields having different retention period",
      PolicyThreatType.THREAT,
      PolicyAction.DENY,
      DataFlow(
        List(),
        SourceFilter(Option(true), "", ""),
        List[String](),
        SinkFilter(List[String](), "", ""),
        CollectionFilter("", "")
      ),
      List("**"),
      Map[String, String](),
      Map[String, String](
        "Data.Sensitive.NationalIdentificationNumbers.SocialSecurityNumber"         -> "7",
        "Data.Sensitive.NationalIdentificationNumbers.TaxpayerIdentificationNumber" -> "7",
        "Data.Sensitive.AccountData.AccountID"                                      -> "7",
        "Data.Sensitive.PersonalIdentification.FirstName"                           -> "7",
        "Data.Sensitive.PersonalIdentification.LastName"                            -> "7",
        "Data.Sensitive.ContactData.PhoneNumber"                                    -> "30",
        "Data.Sensitive.ContactData.Address"                                        -> "30",
        "Data.Sensitive.PersonalIdentification.DateofBirth"                         -> "7",
        "Data.Sensitive.PersonalCharacteristics.Height"                             -> "30",
        "Data.Sensitive.PersonalCharacteristics.Weigth"                             -> "30",
        "Data.Sensitive.PurchaseData.OrderDetails"                                  -> "30"
      ),
      "",
      Array[String]()
    )

    "Identify violation when multiple PIIs of different retention period stored in the same table in SQL Code" in {
      val (_, threatEngine) = code(
        """
          |CREATE TABLE IF NOT EXISTS Customer (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		address VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |
          |CREATE TABLE IF NOT EXISTS User (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |     firstName VARCHAR(100),
          |		email VARCHAR(6) NOT NULL,
          |     phone VARCHAR(100),
          |		PRIMARY KEY (id)
          |	);
          |""".stripMargin,
        fileExtension = ".sql"
      )
      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsPIIHavingDifferentRetentionPeriod"
      result.get.processing.get.head.sourceId shouldBe "User"
    }

    "Identify violation when multiple PIIs of different retention period stored in the same table with Go Mongo driver" in {
      val (_, threatEngine) = code(
        """
          |package main
          |
          |import (
          |    "context"
          |    "fmt"
          |    "log"
          |
          |    "go.mongodb.org/mongo-driver/bson"
          |    "go.mongodb.org/mongo-driver/mongo"
          |    "go.mongodb.org/mongo-driver/mongo/options"
          |)
          |
          |// Book - We will be using this Book type to perform crud operations
          |type UserMongo struct {
          |  firstName     string
          |  email    string
          |  phone      string
          |  lastName string
          |}
          |
          |func main() {
          |
          |  // Set client options
          |  clientOptions := options.Client().ApplyURI("mongodb://localhost:27017")
          |
          |  // Connect to MongoDB
          |  client, err := mongo.Connect(context.TODO(), clientOptions)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  // Check the connection
          |  err = client.Ping(context.TODO(), nil)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  fmt.Println("Connected to MongoDB!")
          |  usersCollection := client.Database("testdb").Collection("users")
          |  // Insert One document
          |  user1 := UserMongo{"Some first name", "test@email.com", "1234567890", "Some last name"}
          |  insertResult, err := usersCollection.InsertOne(context.TODO(), user1)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
        downloadDependency = true
      )
      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsPIIHavingDifferentRetentionPeriod"
      result.get.processing.get.head.sourceId shouldBe "UserMongo"
    }

    "No violation reported when multiple PIIs of same retention period stored in the same table with Go Mongo driver" in {
      val (_, threatEngine) = code(
        """
          |package main
          |
          |import (
          |    "context"
          |    "fmt"
          |    "log"
          |
          |    "go.mongodb.org/mongo-driver/bson"
          |    "go.mongodb.org/mongo-driver/mongo"
          |    "go.mongodb.org/mongo-driver/mongo/options"
          |)
          |
          |// Book - We will be using this Book type to perform crud operations
          |type UserMongo struct {
          |  firstName     string
          |  email    string
          |  dob      string
          |  lastName string
          |}
          |
          |func main() {
          |
          |  // Set client options
          |  clientOptions := options.Client().ApplyURI("mongodb://localhost:27017")
          |
          |  // Connect to MongoDB
          |  client, err := mongo.Connect(context.TODO(), clientOptions)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  // Check the connection
          |  err = client.Ping(context.TODO(), nil)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  fmt.Println("Connected to MongoDB!")
          |  usersCollection := client.Database("testdb").Collection("users")
          |  // Insert One document
          |  user1 := UserMongo{"Some first name", "test@email.com", "1234567890", "Some last name"}
          |  insertResult, err := usersCollection.InsertOne(context.TODO(), user1)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
        downloadDependency = true
      )
      val result = threatEngine.processProcessingViolations(threat)
      result shouldBe empty
    }
  }
}
