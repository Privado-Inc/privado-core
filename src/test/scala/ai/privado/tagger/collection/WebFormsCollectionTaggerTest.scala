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

package ai.privado.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model._
import ai.privado.passes.HTMLParserPass
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class WebFormsCollectionTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val sourceId = "Data.Sensitive.Email"
  val sourceRule = List(
    RuleInfo(
      sourceId,
      "Email",
      "",
      Array(),
      List("(?i).*email.*"),
      false,
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

  val collectionRule = List(
    RuleInfo(
      "Collections.Webforms",
      "Webform data collection",
      "",
      Array(),
      List("^<(?i)(?:input|textarea|\\w{0,}TextBox|\\w{0,}Field)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "webforms",
      Language.DEFAULT,
      Array()
    )
  )

  "Simple ReactJS sample " should {
    val cpg = jsxcode("""
        |import React, { useState } from "react";
        |
        |function Form() {
        |  const [formData, setFormData] = useState({
        |    name: "",
        |    something: "",
        |    message: "",
        |  });
        |
        |  const handleSubmit = (event) => {
        |    event.preventDefault();
        |    console.log(formData);
        |    // Submit form data to backend
        |  };
        |
        |  const handleChange = (event) => {
        |    setFormData({ ...formData, [event.target.name]: event.target.value });
        |  };
        |
        |  return (
        |    <form onSubmit={handleSubmit}>
        |      <label>
        |        Name:
        |        <input type="text" name="name" onChange={handleChange} />
        |      </label>
        |      <label>
        |        Email:
        |        <input type="email" name="email" onChange={handleChange} />
        |        <Input label={`Email Address`} placeholder={t`What happened?`} {...form.register('subject')} />
        |        <TextArea label={t`emailId`} placeholder={t`How can we help?`} {...form.register('message')} />
        |      </label>
        |      <label>
        |        Message:
        |        <textarea name="message" onChange={handleChange} />
        |      </label>
        |      <button type="submit">Submit</button>
        |    </form>
        |  );
        |}
        |
        |""".stripMargin)

    "Email input element collection point identified " in {
      val taggedJsxOpenElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
        .l
      taggedJsxOpenElements.size shouldBe 3
      val jsxOpenElement = taggedJsxOpenElements.head.get()
      jsxOpenElement.tag.name(Constants.collectionSource).value.head shouldBe sourceId
      jsxOpenElement.name shouldBe Constants.jsxOpenElement

      val taggedJsxElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.id).valueExact(sourceId))
        .l
      taggedJsxElements.size shouldBe 3
      val jsxElement = taggedJsxElements.head.get()
      jsxElement.name shouldBe Constants.jsxElement
    }
  }

  "Simple HTML sample" should {
    val cpg = htmlCode("""
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
        |""".stripMargin)
    "Email input element collection point identified " in {
      val taggedHTMLOpenElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
        .l
      taggedHTMLOpenElements.size shouldBe 1
      val HTMLOpenElement = taggedHTMLOpenElements.head.get()
      HTMLOpenElement.tag.name(Constants.collectionSource).value.head shouldBe sourceId
      HTMLOpenElement.name shouldBe Constants.HTMLOpenElement

      val taggedHTMLElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.id).valueExact(sourceId))
        .l
      taggedHTMLElements.size shouldBe 1
      val HTMLElement = taggedHTMLElements.head.get()
      HTMLElement.name shouldBe Constants.HTMLElement
    }
  }

  "Angular JS template sample" should {
    val cpg = htmlCode("""
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

    "Email input element collection point identified " in {
      val taggedHTMLOpenElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
        .l
      taggedHTMLOpenElements.size shouldBe 1
      val HTMLOpenElement = taggedHTMLOpenElements.head.get()
      HTMLOpenElement.tag.name(Constants.collectionSource).value.head shouldBe sourceId
      HTMLOpenElement.name shouldBe Constants.HTMLOpenElement

      val taggedHTMLElements = cpg.templateDom
        .where(_.tag.nameExact(Constants.id).valueExact(sourceId))
        .l
      taggedHTMLElements.size shouldBe 1
      val HTMLElement = taggedHTMLElements.head.get()
      HTMLElement.name shouldBe Constants.HTMLElement
    }
  }

  def htmlCode(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "sample.html").write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())
    val ruleCache = new RuleCache()
    ruleCache.setRule(rule)
    val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg    = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    new HTMLParserPass(cpg, inputDir.toString(), ruleCache).createAndApply()
    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()

    cpgs.addOne(cpg)
    cpg
  }

  def jsxcode(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "sample.jsx").write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())
    val ruleCache = new RuleCache()
    ruleCache.setRule(rule)
    val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg    = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()
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
