package ai.privado.languageEngine.python.passes.config

import ai.privado.languageEngine.python.PrivadoPySrc2CpgFixture
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import io.shiftleft.semanticcpg.language.*

class PythonConfigPropertyPassTest extends PrivadoPySrc2CpgFixture {

  "Python config property pass" should {
    "able to create property node" in {

      val cpg = code("""
          |DATABASES = {
          |    'default': {
          |        'ENGINE': 'django.db.backends.postgresql',
          |        'NAME': 'your_database_name',
          |        'USER': 'your_database_user',
          |        'PASSWORD': 'your_database_password',
          |        'HOST': 'localhost',
          |        'PORT': '5432',
          |    }
          |}
          |
          |config = {
          |    'server': {
          |        'port': 3000,
          |        'host': 'localhost'
          |    },
          |    'database': {
          |        'url': 'mongodb://localhost:27017/mydatabase',
          |        'username': 'admin',
          |        'password': 'password123'
          |    },
          |    'logging': {
          |        'level': "info",
          |        'file': 'app.log'
          |    }
          |}
          |""".stripMargin)

      new PythonConfigPropertyPass(cpg).createAndApply()

      cpg.property.size shouldBe 13
      cpg.property.nameExact("DATABASES.default.ENGINE").value.l shouldBe List("django.db.backends.postgresql")
      cpg.property.nameExact("config.server.host").value.l shouldBe List("localhost")
    }

  }

}
