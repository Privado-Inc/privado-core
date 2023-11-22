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
package ai.privado.languageEngine.go.tagger.sink

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class GoAPITaggerTestCase1 extends GoTaggingTestBase {

  "Tagging api sink: When nothing is matching(identifier or url) it should tagged with API" should {
    val (cpg, _) = code("""
        | package main
        |
        |import (
        |	"bytes"
        |	"encoding/json"
        |	"fmt"
        |	"net/http"
        |)
        |
        |type User struct {
        |	FirstName     string
        |	Age      int
        |	Location string
        |	Email    string
        |}
        |func (client *APIClient) SendUser(user User) error {
        |	http_url := client.BaseURL + "/users"
        |	payload, err := json.Marshal(user)
        |	if err != nil {
        |		return err
        |	}
        |
        |	resp, err := http.Post(http_url, "application/json", bytes.NewBuffer(payload))
        |	if err != nil {
        |		return err
        |	}
        |	defer resp.Body.Close()
        |
        |	if resp.StatusCode != http.StatusOK {
        |		return fmt.Errorf("failed to send user: %s", resp.Status)
        |	}
        |
        |	fmt.Println("User sent successfully!")
        |	return nil
        |}
        |
        |func main() {
        |	// Create a new User object
        |	user := User{
        |		Name:     "John Doe",
        |		Age:      25,
        |		Location: "New York",
        |		Email: "abc@gmail.com",
        |	}
        |
        |	// Create a new API client
        |	client := APIClient{
        |		BaseURL: "https://api.example.com",
        |	}
        |
        |	// Send the User object to the API client
        |	err := client.SendUser(user)
        |	if err != nil {
        |		fmt.Printf("Error sending user: %s\n", err.Error())
        |	}
        |}
        |
        |""".stripMargin)
    "check tag of api sink" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"

      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 9
      postCallNode.tag.nameExact("id").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      postCallNode.tag.nameExact("third_partiesapi").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
    }

  }

}

class GoAPITaggerTestCase2 extends GoTaggingTestBase {

  "Tagging api sink: When Identifier is matching with apiIdentifier pattern" should {
    val (cpg, _) = code("""
        | package main
        |
        |import (
        |	"bytes"
        |	"encoding/json"
        |	"fmt"
        |	"net/http"
        |)
        |
        |type User struct {
        |	FirstName     string
        |	Age      int
        |	Location string
        |	Email    string
        |}
        |func (client *APIClient) SendUser(user User) error {
        |	gateway_url := "its not a url"
        |	payload, err := json.Marshal(user)
        |	if err != nil {
        |		return err
        |	}
        |
        |	resp, err := http.Post(gateway_url, "application/json", bytes.NewBuffer(payload))
        |	if err != nil {
        |		return err
        |	}
        |	defer resp.Body.Close()
        |
        |	if resp.StatusCode != http.StatusOK {
        |		return fmt.Errorf("failed to send user: %s", resp.Status)
        |	}
        |
        |	fmt.Println("User sent successfully!")
        |	return nil
        |}
        |
        |func main() {
        |	// Create a new User object
        |	user := User{
        |		Name:     "John Doe",
        |		Age:      25,
        |		Location: "New York",
        |		Email: "abc@gmail.com",
        |	}
        |
        |	// Create a new API client
        |	client := APIClient{
        |		BaseURL: "https://api.example.com",
        |	}
        |
        |	// Send the User object to the API client
        |	err := client.SendUser(user)
        |	if err != nil {
        |		fmt.Printf("Error sending user: %s\n", err.Error())
        |	}
        |}
        |
        |""".stripMargin)
    "check tag of api sink" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"

      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 9
      postCallNode.tag.nameExact("id").value.head shouldBe "Sinks.ThirdParties.API.gmail.com"
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      postCallNode.tag.nameExact("third_partiesapi").value.head shouldBe "Sinks.ThirdParties.API.gmail.com"
    }

  }

}

class GoAPITaggerTestCase3 extends GoTaggingTestBase {

  "Tagging api sink: When Identifier is matching with apiIdentifier pattern" should {
    val (cpg, _) = code("""
        | package main
        |
        |import (
        |	"bytes"
        |	"encoding/json"
        |	"fmt"
        |	"net/http"
        |)
        |
        |type User struct {
        |	FirstName     string
        |	Age      int
        |	Location string
        |	Email    string
        |}
        |func (client *APIClient) SendUser(user User) error {
        |	gateway_url := "https://api.example.com"
        |	payload, err := json.Marshal(user)
        |	if err != nil {
        |		return err
        |	}
        |
        |	resp, err := http.Post(gateway_url, "application/json", bytes.NewBuffer(payload))
        |	if err != nil {
        |		return err
        |	}
        |	defer resp.Body.Close()
        |
        |	if resp.StatusCode != http.StatusOK {
        |		return fmt.Errorf("failed to send user: %s", resp.Status)
        |	}
        |
        |	fmt.Println("User sent successfully!")
        |	return nil
        |}
        |
        |func main() {
        |	// Create a new User object
        |	user := User{
        |		Name:     "John Doe",
        |		Age:      25,
        |		Location: "New York",
        |		Email: "abc@gmailin.com",
        |	}
        |
        |	// Create a new API client
        |	client := APIClient{
        |		BaseURL: "https://api.example.com",
        |	}
        |
        |	// Send the User object to the API client
        |	err := client.SendUser(user)
        |	if err != nil {
        |		fmt.Printf("Error sending user: %s\n", err.Error())
        |	}
        |}
        |
        |""".stripMargin)

    "check tag of api sink" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"

      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 9
      postCallNode.tag.nameExact("id").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      postCallNode.tag.nameExact("third_partiesapi").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
    }

  }

}
