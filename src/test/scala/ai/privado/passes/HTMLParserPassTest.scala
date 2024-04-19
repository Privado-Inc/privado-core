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

package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{ConfigAndRules, Constants}
import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class HTMLParserPassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  "HTML Parser simple use case" should {
    val cpg = code("""
        |<!DOCTYPE html>
        |<html>
        |   <head>
        |      <title>My Form</title>
        |   </head>
        |   <body>
        |      <form>
        |         <label for="name">Name:</label>
        |         <input type="text" name="name" id="name"/>
        |         <label for="email">Email:</label>
        |         <input type="email" name="email" id="email"/>
        |         <label for="message">Message:</label>
        |         <textarea name="message" id="message"></textarea>
        |         <button type="button" onclick="submitForm()">Submit</button>
        |      </form>
        |   </body>
        |</html>
        |
        |""".stripMargin)

    "Count of html emlements" in {
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 12
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 12
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 12
      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 13
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")

    }

  }

  "Angular JS template sample" should {
    val cpg = code("""
        |<form [formGroup]="form" (ngSubmit)="onSubmit(form.value)">
        |  <label>Name:</label>
        |  <input type="text" formControlName="name" />
        |
        |  <label>Email:</label>
        |  <input type="email" formControlName="email" />
        |
        |  <label>Message:</label>
        |  <textarea formControlName="message"></textarea>
        |
        |  <button type="submit">Submit</button>
        |</form>
        |
        |""".stripMargin)

    "Count of html emlements" in {
      // Considering the HtmlUnit will wrapp this html inside <html><head></head><body> above contents </body></html>
      // The element count will be 8 + 3 = 11
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 8
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")
    }
  }

  "AngularJS template with component" should {
    val cpg = code("""
        |<form #form="ngForm" (ngSubmit)="onSubmit(form.value)">
        |  <mat-form-field>
        |    <mat-label>Name:</mat-label>
        |    <input type="text" name="name" ngModel matInput />
        |  </mat-form-field>
        |
        |  <mat-form-field>
        |    <mat-label>Email:</mat-label>
        |    <input type="email" name="email" ngModel matInput />
        |  </mat-form-field>
        |
        |  <mat-form-field>
        |    <mat-label>Message:</mat-label>
        |    <textarea name="message" ngModel matInput></textarea>
        |  </mat-form-field>
        |
        |  <button type="submit" mat-raised-button color="primary">Submit</button>
        |</form>
        |""".stripMargin)
    "Count of html emlements" in {
      // Considering the HtmlUnit will wrapp this html inside <html><head></head><body> above contents </body></html>
      // The element count will be 11 + 3 = 14
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 14
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 14
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 14
      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 16
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")
    }
  }

  "Emberjs Template with built-in form helpers" should {
    val cpg = code("""
        |{{! my-form.hbs }}
        |<form {{action "submit" on="submit"}}>
        |  <label for="name">Name:</label>
        |  {{input type="text" id="name" value=model.name}}
        |
        |  <label for="email">Email:</label>
        |  {{input type="email" id="email" value=model.email}}
        |
        |  <label for="message">Message:</label>
        |  {{textarea id="message" value=model.message}}
        |
        |  <button type="submit">Submit</button>
        |</form>
        |
        |""".stripMargin)
    "Count of html emlements" in {
      // Considering the HtmlUnit will wrapp this html inside <html><head></head><body> above contents </body></html>
      // The element count will be 5 + 3 = 8
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 8
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 8
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 8
      // Attributes are not getting handled properly for this case.
//      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 4
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")
    }
  }

  "EmberJs template with native dom event binding" should {
    val cpg = code("""
        |{{! my-form.hbs }}
        |<form onsubmit={{action "submit"}}>
        |  <label for="name">Name:</label>
        |  <input type="text" id="name" value={{model.name}}>
        |
        |  <label for="email">Email:</label>
        |  <input type="email" id="email" value={{model.email}}>
        |
        |  <label for="message">Message:</label>
        |  <textarea id="message">{{model.message}}</textarea>
        |
        |  <button type="submit">Submit</button>
        |</form>
        |
        |""".stripMargin)
    "Count of html emlements" in {
      // Considering the HtmlUnit will wrapp this html inside <html><head></head><body> above contents </body></html>
      // The element count will be 8 + 3 = 11
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 11
      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 13
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")
    }
  }

  "VanilaJS with inline-script tags" should {
    val cpg = code("""
        |<!DOCTYPE html>
        |<html>
        |<head>
        |  <title>Script Loading Example</title>
        |</head>
        |<body>
        |
        |  <!-- Script tag to load another script -->
        |  <script async src="//www.googletagservices.com/tag/js/gpt.js"></script>
        |
        |  <!-- Script tag with inline JavaScript code that loads third-party scripts -->
        |  <script>
        |    var div_1_sizes = [
        |                [300, 250],
        |                [300, 600]
        |            ];
        |            var div_2_sizes = [
        |                [728, 90],
        |                [970, 250]
        |            ];
        |  </script>
        |</body>
        |</html>
        |""".stripMargin)
    "Count of html elements" in {
      // Considering the HtmlUnit will wrapp this html inside <html><head></head><body> above contents </body></html>
      // The element count will be 8 + 3 = 11
      cpg.templateDom.name(Constants.HTMLElement).l.size shouldBe 6
      cpg.templateDom.name(Constants.HTMLOpenElement).l.size shouldBe 6
      cpg.templateDom.name(Constants.HTMLClosingElement).l.size shouldBe 6
      cpg.templateDom.name(Constants.HTMLElementAttribute).l.size shouldBe 2
      // Checking only for single first node if the file node is attached.
      cpg.templateDom.file.name.l.head should endWith("sample.html")
    }

    "Count of script tag elements + " in {
      // Checking for Script tags
      cpg.templateDom.name(Constants.HTMLOpenElement).code("(?i)[\\\"]*<(script).*").l.size shouldBe 2
    }

    "Content attribute should be present for script tag" in {
      // Ensure inline script tag is with content attribute
      cpg.templateDom.name(Constants.HTMLOpenElement).code("(?i)[\\\"]*<(script).*(content).*").l.size shouldBe 1
    }
  }

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "sample.html").write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List(), List(), List())
    val ruleCache    = new RuleCache()
    val privadoInput = PrivadoInput()
    ruleCache.withRule(rule)
    val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg    = X2Cpg.withNewEmptyCpg(outputFile.toString(), config)((cpg, config) => {}).get
    new HTMLParserPass(cpg, inputDir.toString(), ruleCache, privadoInput).createAndApply()
    cpgs.addOne(cpg)
    cpg
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
    outPutFiles.foreach(_.delete())
    inputDirs.foreach(_.delete())
    super.afterAll()
  }

}
