package ai.privado.threatEngine.go

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.model.*
import ai.privado.model.exporter.ViolationModel
import ai.privado.threatEngine.ThreatEngineExecutor

import scala.collection.immutable.Map

class ThreatTests extends GoTaggingTestBase {

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
        CollectionFilter("")
      ),
      List("**"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    "When same data-element is part of multiple table in go file" in {

      val (cpg, threatEngine) = code("""
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
          |	Phone   string `gorm:"type:varchar(100); NOT NULL; UNIQUE; UNIQUE_INDEX" json:"phone" validate:"required"`
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

    "When same data-element is not part of multiple table in go file" in {

      val (cpg, threatEngine) = code("""
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
}
