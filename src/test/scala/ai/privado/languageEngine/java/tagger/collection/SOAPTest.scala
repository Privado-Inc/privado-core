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

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.Constants
import io.shiftleft.semanticcpg.language._

class SOAPTest extends JavaTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    val nodeCache = CPGNodeCacheForSourceTagger(cpg, ruleCache)
    new DirectNodeSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new FirstLevelDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new OCDDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new ExtendingDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new SOAPCollectionTagger(cpg, ruleCache).createAndApply()

  }

  override val javaFileContents: String =
    """
      |@WebService(name = "ContourSoapService", targetNamespace = "http://v3.ws.contour.jamasoftware.com/")
      |@XmlSeeAlso({
      |        ObjectFactory.class
      |})
      |public interface ContourSoapService {
      |
      |
      |    @WebMethod(action = "updateUser")
      |    @WebResult(targetNamespace = "")
      |    @RequestWrapper(localName = "updateUser", targetNamespace = "http://v3.ws.contour.jamasoftware.com/", className = "com.jamasoftware.examples.soap.wsdl.UpdateUser")
      |    @ResponseWrapper(localName = "updateUserResponse", targetNamespace = "http://v3.ws.contour.jamasoftware.com/", className = "com.jamasoftware.examples.soap.wsdl.UpdateUserResponse")
      |    WsUser updateUser(
      |            @WebParam(name = "token", targetNamespace = "")
      |            WsAuth token,
      |            @WebParam(name = "firstName", targetNamespace = "")
      |            String firstName);
      |}
      |""".stripMargin

  "SOAP collection point" should {
    "be tagged" in {
      cpg.method
        .where(_.tag.nameExact(Constants.id).valueExact("Collections.Annotation.SOAP"))
        .size shouldBe 1
    }
  }
}
