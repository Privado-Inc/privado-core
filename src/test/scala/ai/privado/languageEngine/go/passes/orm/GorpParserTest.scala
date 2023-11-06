package ai.privado.languageEngine.go.passes.orm

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.model.*
import ai.privado.semantic.Language.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}

class GorpParserTest extends GoTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    X2Cpg.applyDefaultOverlays(cpg)

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new GorpParser(cpg).createAndApply()
  }

  override val goFileContents =
    """
      package main
      |
      |import (
      |	"database/sql"
      |	"fmt"
      |	"log"
      |
      |	"github.com/go-gorp/gorp"
      |	_ "github.com/go-sql-driver/mysql"
      |)
      |
      |// User represents a user in the database
      |type User struct {
      |	ID   int64
      |	Name string
      |	Age  int
      |}
      |
      |func main() {
      |	// Create a new database connection
      |	db, err := sql.Open("mysql", "root:password@tcp(localhost:3306)/mydatabase")
      |	if err != nil {
      |		log.Fatal(err)
      |	}
      |	defer db.Close()
      |
      |	// Create a new Gorp database mapper
      |	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.MySQLDialect{}}
      |
      |	// Define the User table mapping
      |	userTable := dbmap.AddTableWithName(User{}, "users").SetKeys(true, "ID")
      |
      |	// Create the User table if it doesn't exist
      |	err = dbmap.CreateTablesIfNotExists()
      |	if err != nil {
      |		log.Fatal(err)
      |	}
      |
      |	// Insert a new user
      |	user := &User{Name: "John Doe", Age: 25}
      |	err = dbmap.Insert(user)
      |	if err != nil {
      |		log.Fatal(err)
      |	}
      |
      |	// Fetch all users from the database
      |	var users []User
      |	_, err = dbmap.Select(&users, "SELECT * FROM users")
      |	if err != nil {
      |		log.Fatal(err)
      |	}
      |
      |	// Print the users
      |	for _, u := range users {
      |		fmt.Printf("ID: %d, Name: %s, Age: %d\n", u.ID, u.Name, u.Age)
      |	}
      |}
      |
      |""".stripMargin

  "Adding sql nodes for GORP framework" should {
    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "User"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 3

      val List(id, name, age) = cpg.sqlColumn.l
      id.code shouldBe "ID"
      name.code shouldBe "Name"
      age.code shouldBe "Age"
    }

  }

}
