package ai.privado.threatEngine.go

import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.go.passes.orm.GormParser
import ai.privado.model.*
import ai.privado.model.exporter.ViolationModel
import ai.privado.passes.{SQLParser, SQLPropertyPass}
import ai.privado.tagger.source.SqlQueryTagger
import ai.privado.threatEngine.ThreatEngineExecutor
import better.files.File
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.Map

class ThreatTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  val sourceRule: List[RuleInfo] = List(
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )

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

      val threatEngine = code("""
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
  }

  "No Threat PIIShouldNotBePresentInMultipleTables when both model have different PII" should {
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

      val threatEngine = code("""
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

  def code(code: String): ThreatEngineExecutor = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "generalFile.go").write(code)
    (inputDir / "unrelated.file").write("foo")
    val outputFile = File.newTemporaryFile()
    val config     = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    val privadoInput =
      PrivadoInput(generateAuditReport = true, enableAuditSemanticsFilter = true)
    val configAndRules =
      ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())
    ScanProcessor.config = privadoInput
    val ruleCache = new RuleCache()
    ruleCache.setRule(configAndRules)
    val cpg           = new GoSrc2Cpg().createCpg(config).get
    val auditCache    = new AuditCache
    val dataFlowCache = new DataFlowCache(auditCache)

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new GormParser(cpg).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)
    val dataFlows: Map[String, Path] = Map()
    new ThreatEngineExecutor(cpg, dataFlows, config.inputPath, ruleCache, null, dataFlowCache, privadoInput)
  }
}
