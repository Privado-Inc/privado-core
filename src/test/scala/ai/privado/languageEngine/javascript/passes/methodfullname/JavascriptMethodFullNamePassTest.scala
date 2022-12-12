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

package ai.privado.languageEngine.javascript.passes.methodfullname

import ai.privado.languageEngine.javascript.JavascriptTaggingTestBase
import ai.privado.model.ConfigAndRules
import io.shiftleft.semanticcpg.language._

class JavascriptMethodFullNamePassRequireStyleTest extends JavascriptTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new MethodFullName(cpg).createAndApply()
    new MethodFullNameFromIdentifier(cpg).createAndApply()
  }

  override val javascriptFileContents: String =
    """
      |
      |const sgMail = require('@sendgrid/mail');
      |const {WebClient} = require('@slack/web-api');
      |const cors = require('cors');
      |
      |cors();
      |const web = new WebClient(token);
      |
      |var msg = "some message";
      |sgMail.send(msg);
      |web.chat.postMessage(msg);
      |
      |""".stripMargin
  override val packageJsonFileContents: String =
    """
      |{
      | "dependencies": {
      |    "@sendgrid/mail": "^7.7.0",
      |    "@slack/web-api": "^6.7.2",
      |    "cors": "^2.8.5"
      |  }
      |}
      |""".stripMargin

  override val rule: ConfigAndRules = ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List())

  "Javascript MethodFullName pass" should {
    "add methodFullName for called by identifier node" in {
      val sendGridNode = cpg.call.methodFullName("pkg.@sendgrid/mail.send").l
      sendGridNode.size shouldBe 1
    }

    "add methodFullName for called by identifier node chaining fieldIdentifier" in {
      val slackNode = cpg.call.methodFullName("pkg.@slack/web-api.*").l

      slackNode.size shouldBe 2
      slackNode.methodFullName.l shouldBe List("pkg.@slack/web-api.<operator>.new", "pkg.@slack/web-api.postMessage")
    }

    "add methodFullName for directly call node" in {
      val corsNode = cpg.call.methodFullName("pkg.cors.cors").l

      corsNode.size shouldBe 1
      corsNode.methodFullName.l shouldBe List("pkg.cors.cors")
    }

    // Note - currently not handling internal imports as specifically, as it doesn't add any value.
    // will take this up when needed
    /*
    "handle internal import" in {}
     */

  }
}

class JavascriptMethodFullNamePassImportStyleTest extends JavascriptTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new MethodFullName(cpg).createAndApply()
    new MethodFullNameFromIdentifier(cpg).createAndApply()
  }

  override val javascriptFileContents: String =
    """
      |
      |import sgMail from '@sendgrid/mail';
      |import {WebClient} from '@slack/web-api';
      |import * as cors from 'cors';
      |
      |cors();
      |const web = new WebClient(token);
      |
      |var msg = "some message";
      |sgMail.send(msg);
      |web.chat.postMessage(msg);
      |
      |""".stripMargin
  override val packageJsonFileContents: String =
    """
      |{
      | "dependencies": {
      |    "@sendgrid/mail": "^7.7.0",
      |    "@slack/web-api": "^6.7.2",
      |    "cors": "^2.8.5"
      |  }
      |}
      |""".stripMargin

  override val rule: ConfigAndRules = ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List())

  "Javascript MethodFullName pass" should {
    "add methodFullName for called by identifier node" in {
      val sendGridNode = cpg.call.methodFullName("pkg.@sendgrid/mail.send").l
      sendGridNode.size shouldBe 1
    }

    "add methodFullName for called by identifier node chaining fieldIdentifier" in {
      val slackNode = cpg.call.methodFullName("pkg.@slack/web-api.*").l

      slackNode.size shouldBe 2
      slackNode.methodFullName.l shouldBe List("pkg.@slack/web-api.<operator>.new", "pkg.@slack/web-api.postMessage")
    }

    "add methodFullName for directly call node" in {
      val corsNode = cpg.call.methodFullName("pkg.cors.cors").l

      corsNode.size shouldBe 1
      corsNode.methodFullName.l shouldBe List("pkg.cors.cors")
    }

    // Note - currently not handling internal imports as specifically, as it doesn't add any value.
    // will take this up when needed
    /*
    "handle internal import" in {}
     */

  }
}
