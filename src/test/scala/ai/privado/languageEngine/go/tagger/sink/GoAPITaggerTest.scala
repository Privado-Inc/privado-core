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

import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class GoAPITaggerTest extends GoTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new GoAPITagger(cpg, ruleCache, privadoInput).createAndApply()
  }

  override val goFileContents =
    """
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
      |	url := client.BaseURL + "/users"
      |	payload, err := json.Marshal(user)
      |	if err != nil {
      |		return err
      |	}
      |
      |	resp, err := http.Post(url, "application/json", bytes.NewBuffer(payload))
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
      |""".stripMargin

  "Tagging api sink" should {
    "check tag of api sink" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"

      val List(postCallNode) = cpg.call("Post").l
      //TODO: check tags on postCallNode for below cases
        // 1. When literal holds some url
        // 2. When Identifier is matching with apiIdentifier pattern
        // 3. When nothing is matching it should tagged with "API"
    }

  }

}
