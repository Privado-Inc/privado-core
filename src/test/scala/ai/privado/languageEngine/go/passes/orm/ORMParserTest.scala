/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */
package ai.privado.languageEngine.go.passes.orm

import ai.privado.languageEngine.go.GoTestBase
import ai.privado.semantic.Language.*
import io.shiftleft.semanticcpg.language.*

class ORMParserTest extends GoTestBase {

  "Adding sql nodes for GORM framework" should {
    val (cpg, _) = code("""
          package models
          |
          |import (
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
          |// Check if Users Model implements/match Model interface
          |var _ Model = &User{}
          |
          |// Save saves a User inside the database
          |func (u *User) Save(db *gorm.DB) error {
          |	uuidResult, err := uuid.NewRandom()
          |	if err != nil {
          |		log.Fatal(err)
          |	}
          |
          |	u.ID = uuidResult.String()
          |	u.CreatedAt = time.Now().String()
          |	u.UpdatedAt = time.Now().String()
          |
          |	err = db.Save(&u).Error
          |	if err != nil {
          |		return err
          |	}
          |	return nil
          |}
          |""".stripMargin)

    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "User"
    }

    "check query nodes" in {
      val queryNodes = cpg.sqlQuery.l
      queryNodes.size shouldBe 1
      queryNodes.head.name shouldBe "CREATE"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 5

      val List(id, name, email, phone, address) = cpg.sqlColumn.l
      id.code shouldBe "ID"
      name.code shouldBe "Name"
      email.code shouldBe "Email"
      phone.code shouldBe "Phone"
      address.code shouldBe "Address"
    }
  }

  "Adding sql nodes for GORP framework" should {
    val (cpg, _) = code("""
          |package main
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
    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 2
      tableNodes.head.name shouldBe "User"
    }

    "check query nodes" in {
      val queryNodes = cpg.sqlQuery.l
      queryNodes.size shouldBe 2
      queryNodes.head.name shouldBe "CREATE"
      queryNodes.code.head shouldBe "User struct {\n\tID   int64\n\tName string\n\tAge  int\n}"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 4

      val List(id, name, age, _) = cpg.sqlColumn.l
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

  "SQL Node checks for Mongo" should {
    val (cpg, _) = code(
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
          |type Book struct {
          |  Title     string
          |  Author    string
          |  ISBN      string
          |  Publisher string
          |  Copies     int
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
          |  booksCollection := client.Database("testdb").Collection("books")
          |  // Insert One document
          |  book1 := Book{"Animal Farm", "George Orwell", "0451526341", "Signet Classics", 100}
          |  insertResult, err := booksCollection.InsertOne(context.TODO(), book1)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
      downloadDependency = true
    )

    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "Book"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 5

      val List(title, auther, isbn, publisher, copies) = cpg.sqlColumn.l
      title.code shouldBe "Title"
      title.lineNumber shouldBe Some(16)

      auther.code shouldBe "Author"
      auther.lineNumber shouldBe Some(17)

      isbn.code shouldBe "ISBN"
      isbn.lineNumber shouldBe Some(18)

      publisher.code shouldBe "Publisher"
      publisher.lineNumber shouldBe Some(19)

      copies.code shouldBe "Copies"
      copies.lineNumber shouldBe Some(20)
    }
  }

  "Adding sql nodes for GORM framework having array object passed into db" should {
    val (cpg, _) = code("""
        package models
        |
        |import (
        |	"github.com/jinzhu/gorm"
        |)
        |
        |type User struct {
        |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
        |	MyModel
        |}
        |
        |var _ Model = &User{}
        |
        |func (u *User) Save(db *gorm.DB) error {
        |	uuidResult, err := uuid.NewRandom()
        |	if err != nil {
        |		log.Fatal(err)
        |	}
        |
        | users := []User{
        |     {ID: 1, Name: "Alice", Age: 25},
        |     {ID: 2, Name: "Bob", Age: 30},
        |     {ID: 3, Name: "Charlie", Age: 22},
        |  }
        |
        |	err = db.Save(users).Error
        |	if err != nil {
        |		return err
        |	}
        |	return nil
        |}
        |""".stripMargin)

    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "User"
    }

    "check query nodes" in {
      val queryNodes = cpg.sqlQuery.l
      queryNodes.size shouldBe 1
      queryNodes.head.name shouldBe "CREATE"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 1

      val List(id) = cpg.sqlColumn.l
      id.code shouldBe "ID"
    }
  }

  "SQL Node checks for Mongo having double pointer passed into db" should {
    val (cpg, _) = code(
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
          |type Book struct {
          |  Title     string
          |}
          |
          |func main() {
          |  clientOptions := options.Client().ApplyURI("mongodb://localhost:27017")
          |  client, err := mongo.Connect(context.TODO(), clientOptions)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  err = client.Ping(context.TODO(), nil)
          |
          |  if err != nil {
          |    log.Fatal(err)
          |  }
          |
          |  booksCollection := client.Database("testdb").Collection("books")
          |  book1 := Book{"Animal Farm", "George Orwell", "0451526341", "Signet Classics", 100}
          |  book2 := &book1
          |  insertResult, err := booksCollection.InsertOne(context.TODO(), book2)
          |  if err != nil {
          |      log.Fatal(err)
          |  }
          |
          |  fmt.Println("Inserted a single document: ", insertResult.InsertedID)
          |}
          |""".stripMargin,
      downloadDependency = true
    )

    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "Book"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 1

      val List(title) = cpg.sqlColumn.l
      title.code shouldBe "Title"
      title.lineNumber shouldBe Some(15)
    }
  }
}
