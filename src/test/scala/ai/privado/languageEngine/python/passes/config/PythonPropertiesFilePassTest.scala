package ai.privado.languageEngine.python.passes.config

import ai.privado.languageEngine.java.passes.config.PropertiesFilePassTestBase
import ai.privado.languageEngine.java.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Literal}

class GetEnvironmentTest extends PropertiesFilePassTestBase(".env", language = "python") {
  override val configFileContents: String =
    """
       |MONGO_URL=mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority
       |DEV=False
       |""".stripMargin
  override val codeFileContents: String =
    """
      |import os
      |
      |class Config:
      |   url = environ.get("MONGO_URL")
      |   db = MongoClient(url)
      |""".stripMargin
  override val propertyFileContents: String = ""

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val List(name: String) = cpg.file.name.l
      name.endsWith("/test.env") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("MONGO_URL")
        .contains(
          "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"
        ) shouldBe true
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/test.env") shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val List(lit: Literal) = cpg.property.usedAt.l
      lit.code shouldBe "\"MONGO_URL\""
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val List(javaP: JavaProperty) = cpg.property.usedAt.originalProperty.l
      javaP.value shouldBe "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"

      val List(lit: Literal) = cpg.property.usedAt.l
      lit.originalProperty.head.value shouldBe "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"
      lit.originalPropertyValue.head shouldBe "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"
    }
  }
}
