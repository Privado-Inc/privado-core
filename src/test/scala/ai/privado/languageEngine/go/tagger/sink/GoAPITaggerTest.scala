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

import ai.privado.languageEngine.go.GoTestBase
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class GoAPITaggerTestCase1 extends GoTestBase {

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
      identifierNodes.value.head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"

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

class GoAPITaggerTestCase2 extends GoTestBase {

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
      identifierNodes.value.head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"

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

class GoAPITaggerTestCase3 extends GoTestBase {

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
      identifierNodes.value.head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"

      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 6
      postCallNode.tag.nameExact("id").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      postCallNode.tag.nameExact("third_partiesapi").value.head shouldBe "Sinks.ThirdParties.API.api.example.com"
    }

  }

}

class GoAPITaggerTestCase4 extends GoTestBase {
  "Tagging api sink: using go-resty" should {
    val (cpg, _) = code(
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/go-resty/resty"
        |)
        |type User struct {
        |	FirstName     string
        |	Age      int
        |	Location string
        |	Email    string
        |}
        |
        |func (apiClient *APIClient) SendUser(user User) error {
        | client := resty.New()
        | http_url := "https://api.example.com/users"
        |
        | response, err := client.R().
        |		SetHeader("Content-Type", "application/json").
        |		SetBody(user).
        |		Post(http_url)
        |
        |  if err != nil {
        |   return err
        |  }
        |
        |	fmt.Println("User sent successfully!")
        |	return nil
        |}
        |
        |func main() {
        | user = User{
        |   Name:     "John Doe",
        |		Age:      25,
        |		Location: "New York",
        |		Email: "abc@gmail.com",
        | }
        |
        | client := APIClient{
        |		BaseURL: "https://api.example.com",
        |	}
        |
        | err := client.SendUser(user)
        | if err != nil {
        |   fmt.Printf("Error sending user: %s\n", err.Error())
        | }
        |}
        |""".stripMargin,
      downloadDependency = true
    )

    "check tag of api sink" in {
      val identifierNode = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNode.size shouldBe 1
      identifierNode.value.head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"

      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 9
      val idTags = postCallNode.tag.nameExact("id").value.l
      idTags should contain("Sinks.ThirdParties.API.gmail.com")
      idTags should contain("Sinks.ThirdParties.API.api.example.com")
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      val thirdPartyTags = postCallNode.tag.nameExact("third_partiesapi").value.l
      thirdPartyTags should contain("Sinks.ThirdParties.API.gmail.com")
      thirdPartyTags should contain("Sinks.ThirdParties.API.api.example.com")
    }
  }
}

class GoAPITaggerForSOAPAPI extends GoTestBase {
  "Tagging api sink: having SOAP API" should {
    val (cpg, _) = code("""
        |package main
        |
        |import (
        |	"bytes"
        |	"crypto/tls"
        |	"encoding/xml"
        |	"fmt"
        |	"io/ioutil"
        |	"net/http"
        |	"strings"
        |)
        |
        |type SoapHeader struct {
        |	XMLName xml.Name `xml:"x:Header"`
        |}
        |
        |type SoapBody struct {
        |	XMLName xml.Name `xml:"x:Body"`
        |	Request interface{}
        |}
        |
        |type SoapRoot struct {
        |	XMLName xml.Name `xml:"x:Envelope"`
        |	X       string   `xml:"xmlns:x,attr"`
        |	Sch     string   `xml:"xmlns:sch,attr"`
        |	Header  SoapHeader
        |	Body    SoapBody
        |}
        |
        |type GetCitiesRequest struct {
        |	XMLName xml.Name `xml:"sch:GetCitiesRequest"`
        |}
        |
        |type GetCitiesResponse struct {
        |	XMLName xml.Name `xml:"ns3:GetCitiesResponse"`
        |	result  struct{} `xml:result`
        |	cities  struct{} `xml:cities`
        |}
        |
        |func SoapCall(service string, request interface{}) string {
        |	var root = SoapRoot{}
        |	root.X = "http://schemas.xmlsoap.org/soap/envelope/"
        |	root.Sch = "http://www.n11.com/ws/schemas"
        |	root.Header = SoapHeader{}
        |	root.Body = SoapBody{}
        |	root.Body.Request = request
        |
        |	out, _ := xml.MarshalIndent(&root, " ", "  ")
        |	body := string(out)
        |
        |	client := &http.Client{
        |		Transport: &http.Transport{
        |			TLSClientConfig: &tls.Config{
        |				InsecureSkipVerify: true,
        |			},
        |		},
        |	}
        |	response, err := client.Post(service, "text/xml", bytes.NewBufferString(body))
        |
        |	if err != nil {
        |		fmt.Println(err)
        |	}
        |	defer response.Body.Close()
        |
        |	content, _ := ioutil.ReadAll(response.Body)
        |	s := strings.TrimSpace(string(content))
        |	return s
        |}
        |
        |func main() {
        | http_url := "https://api.example.com/users"
        |	SoapCall(http_url, GetCitiesRequest{})
        |}
        |""".stripMargin)

    "check tag of api sink" in {
      val List(postCallNode) = cpg.call("Post").l

      postCallNode.tag.size shouldBe 12
      val idTags = postCallNode.tag.nameExact("id").value.l
      idTags should contain("Sinks.ThirdParties.API.api.example.com")
      postCallNode.tag.nameExact("nodeType").value.head shouldBe "api"
      postCallNode.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCallNode.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      val thirdPartyTags = postCallNode.tag.nameExact("third_partiesapi").value.l
      thirdPartyTags should contain("Sinks.ThirdParties.API.api.example.com")
    }
  }
}
