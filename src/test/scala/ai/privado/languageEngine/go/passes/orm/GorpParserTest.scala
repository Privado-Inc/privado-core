package ai.privado.languageEngine.go.passes.orm

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.semantic.Language.*
import io.shiftleft.semanticcpg.language.*

class GorpParserTest extends GoTaggingTestBase {

  cpg = code("""
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
      |""".stripMargin)

  "Adding sql nodes for GORP framework" should {
    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "User"
    }

    "check query nodes" in {
      val queryNodes = cpg.sqlQuery.l
      queryNodes.size shouldBe 1
      queryNodes.head.name shouldBe "CREATE"
      queryNodes.code.head shouldBe "User struct {\n\tID   int64\n\tName string\n\tAge  int\n}"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 3

      val List(id, name, age) = cpg.sqlColumn.l
      id.code shouldBe "ID"
      id.lineNumber shouldBe Some(15)
      id.columnNumber shouldBe Some(7)

      name.code shouldBe "Name"
      name.lineNumber shouldBe Some(16)
      name.columnNumber shouldBe Some(7)

      age.code shouldBe "Age"
      age.lineNumber shouldBe Some(17)
      age.columnNumber shouldBe Some(7)
    }

  }

}
