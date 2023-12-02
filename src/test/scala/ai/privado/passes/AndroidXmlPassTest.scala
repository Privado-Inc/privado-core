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
import ai.privado.feeder.PermissionSourceRule
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.kotlin2cpg.Config
import io.joern.kotlin2cpg.Kotlin2Cpg
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.nodes.AndroidXmlLayoutNode
import io.shiftleft.codepropertygraph.generated.{Cpg, NodeTypes}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.semantic.Language.*
import ai.privado.tagger.collection.AndroidCollectionTagger
import ai.privado.tagger.source.AndroidXmlPermissionTagger

import scala.collection.mutable

class AndroidXmlPassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]
  private val ruleCache   = new RuleCache()

  private val sources = List(
    RuleInfo(
      "Data.Sensitive.UserContentData.EmailsorTextMessages",
      "Emails or Text Messages",
      "User Content",
      Array(),
      List(""),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVA,
      Array()
    )
  )

  private val collections = List(
    RuleInfo(
      "Collections.Android.Form.Email",
      "Android app email input",
      "",
      Array(),
      List("(?i).*email.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "android",
      Language.KOTLIN,
      Array()
    )
  )

  case class Codes(layoutXmlCode: String, manifestXmlCode: String, kotlinCode: String)

  val xmlLayoutCode =
    """<?xml version="1.0" encoding="utf-8"?>
        |<layout
        |	xmlns:android="http://schemas.android.com/apk/res/android"
        |	xmlns:app="http://schemas.android.com/apk/res-auto">
        |	<com.google.android.material.textfield.TextInputLayout
        |                        android:id="@+id/email"
        |                        android:layout_width="match_parent"
        |                        android:layout_height="wrap_content"
        |                        android:layout_marginHorizontal="4dp"
        |                        app:errorEnabled="true">
        |		<com.google.android.material.textfield.TextInputEditText
        |                            android:id="@+id/emailEditText"
        |                            android:layout_width="match_parent"
        |                            android:layout_height="wrap_content"
        |                            android:drawableStart="@drawable/ic_email_black_24dp"
        |                            android:drawablePadding="8dp"
        |                            android:hint="@string/login_email_edit_text_hint"
        |                            android:inputType="textEmailAddress"
        |                            android:textColor="@color/primaryTextColor"
        |                            android:textSize="16sp" />
        |	</com.google.android.material.textfield.TextInputLayout>
        |</layout>
      |""".stripMargin

  val xmlManifestCode =
    """<?xml version="1.0" encoding="utf-8"?>
      |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
      |    package="com.example.app">
      |
      |    <uses-permission android:name="android.permission.READ_SMS" />
      |
      |    <application
      |        android:icon="@mipmap/ic_launcher"
      |        android:label="@string/app_name"
      |        android:roundIcon="@mipmap/ic_launcher_round"
      |        android:supportsRtl="true"
      |        android:theme="@style/Theme.AppCompat.Light.NoActionBar">
      |        <!-- Add your activities and other components here -->
      |    </application>
      |
      |</manifest>
      |""".stripMargin

  val kotlinCode =
    """
      |override fun onActivityCreated(savedInstanceState: Bundle?) {
      |        super.onActivityCreated(savedInstanceState)
      |
      |        viewModel = ViewModelProviders.of(this).get(LoginViewModel::class.java)
      |
      |        binding.gotoSignUpFragmentTextView.setOnClickListener {
      |            it.findNavController().navigate(R.id.action_loginFragment_to_signupFragment)
      |        }
      |
      |        //Report text change to viewModel and Observe if email format is correct
      |        binding.emailEditText.afterTextChanged { email ->
      |            viewModel.isEmailFormatCorrect(email).observe(this, Observer { isEmailFormatCorrect ->
      |                if (!isEmailFormatCorrect) {//email format is not correct
      |                    binding.email.error = getString(R.string.wrong_email_format)
      |                } else {
      |                    binding.email.isErrorEnabled = false
      |                }
      |
      |            })
      |        }
      |""".stripMargin

  "CPG with Android layout and Manifest XML" should {
    val cpg = code(Codes(xmlLayoutCode, xmlManifestCode, kotlinCode))

    "have correct number of layout nodes" in {
      cpg.androidXmlLayoutNode.l.size shouldBe 1
      cpg.androidXmlLayoutNode.name.headOption shouldBe Some("emailEditText")
      cpg.androidXmlLayoutNode.lineNumber.headOption shouldBe Some(20)   // goes to end of node :(
      cpg.androidXmlLayoutNode.columnNumber.headOption shouldBe Some(55) // ditto
    }

    "have correct number of permission nodes" in {
      cpg.androidXmlPermissionNode.l.size shouldBe 1
      cpg.androidXmlPermissionNode.permissionType.headOption shouldBe Some("android.permission.READ_SMS")
      cpg.androidXmlPermissionNode.lineNumber.headOption shouldBe Some(5)
      cpg.androidXmlPermissionNode.columnNumber.headOption shouldBe Some(67)
    }

    "have tagged permission node" in {
      new AndroidXmlPermissionTagger(
        cpg,
        ruleCache = ruleCache,
        permissionRules = PermissionSourceRule.miniatureRuleList
      ).createAndApply()
      val taggedNodes = cpg.androidXmlPermissionNode.where(_.tag).l
      taggedNodes.tag.where(_.nameExact(Constants.id)).value.headOption shouldBe Some(
        "Data.Sensitive.UserContentData.EmailsorTextMessages"
      )
      taggedNodes.tag.where(_.nameExact(Constants.catLevelOne)).value.headOption shouldBe Some(CatLevelOne.SOURCES.name)
    }

    "have tagged layout collection nodes" in {
      val taggedNodes = cpg.fieldAccess.astChildren.isFieldIdentifier
        .canonicalName("emailEditText")
        .l

      taggedNodes.tag.where(_.nameExact(Constants.collectionSource)).value.headOption shouldBe Some("emailEditText")
      taggedNodes.tag.where(_.nameExact(Constants.id)).value.headOption shouldBe Some("Collections.Android.Form.Email")
    }
  }

  def code(codes: Codes): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    val layoutDir = (inputDir / "layout").createDirectory()
    val codeDir   = (inputDir / "src").createDirectory()
    (layoutDir / "fragment.xml").write(codes.layoutXmlCode)
    (codeDir / "Main.kt").write(codes.kotlinCode)
    (inputDir / "AndroidManifest.xml").write(codes.manifestXmlCode)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(sources, List(), collections, List(), List(), List(), List(), List(), List(), List())
    ruleCache.setRule(rule)
    val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg    = new Kotlin2Cpg().createCpgWithOverlays(config).get
    new AndroidXmlParserPass(cpg, inputDir.toString(), ruleCache).createAndApply()
    new AndroidCollectionTagger(cpg, inputDir.toString(), ruleCache).createAndApply()
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
