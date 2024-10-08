package ai.privado.languageEngine.python.passes.config

import ai.privado.semantic.language.*
import ai.privado.testfixtures.PythonFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class GetEnvironmentTest extends PythonFrontendTestSuite {
  val mongourl = "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"

  "ConfigFilePass" should {
    val cpg = code(
      """
       |MONGO_URL=mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority
       |""".stripMargin,
      "test.env"
    ).moreCode(
      """
                |import os
                |
                |mongourl = os.environ.get("MONGO_URL")""".stripMargin,
      "code.py"
    )

    "create a file node for the property file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head shouldBe ("test.env")
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("MONGO_URL")
        .contains(mongourl) shouldBe true
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename shouldBe ("test.env")
    }
    "connect property node to literal via `IS_USED_AT` edge" in {
      val lit = cpg.property.usedAt.l.head
      lit.code shouldBe "\"MONGO_URL\""
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val javaP = cpg.property.usedAt.originalProperty.l.head
      javaP.value shouldBe mongourl

      val lit = cpg.property.usedAt.l.head
      lit.originalProperty.head.value shouldBe mongourl
      lit.originalPropertyValue.head shouldBe mongourl
    }
  }
}

class ConnectionMethodResolution extends PythonFrontendTestSuite {

  val mongo_url = "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"
  val cpg = code("", "config.env")
    .moreCode(
      """
      |import pymongo
      |import sqlalchemy
      |import pyscopg2
      |import pyodbc
      |
      |mongo_connection = pymongo.MongoClient("mongodb://abc:123@localhost:27017/journey?<options>")
      |
      |sqlalchemy_conn = sqlalchemy.create_engine("postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase")
      |
      |postgre_connection = pyscopg2.connect("postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase")
      |
      |odbc_connection = pyodbc.connect("postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase")""".stripMargin,
      "code.py"
    )

  "create a `property` node for each property" in {
    val properties = cpg.property.map(x => (x.name, x.value)).toList
    val propertyValues = Set(
      "mongodb://abc:123@localhost:27017/journey?<options>",
      "postgresql://myuser:mypassword@10.0.0.1:5432/mydatabase"
    )
    // Count pairs where key is not db_url and value is not in the above set to check if there are any outliers
    properties.count(pair => pair._1 != "db_url" && !propertyValues.contains(pair._2)) shouldBe 0
  }
}

class DBConfigImplementationTests extends PythonFrontendTestSuite {
  val cpg = code(
    """
      |POSTGRES_DB=finance
      |POSTGRES_USER=postgres
      |POSTGRES_PASSWORD=password
      |POSTGRES_HOSTNAME=db
      |POSTGRES_PORT=5432
      |""".stripMargin,
    "test.env"
  ).moreCode(
    """
      |from functools import cached_property
      |from typing import Dict, List, Optional
      |from urllib.parse import ParseResult, parse_qs, urlparse
      |
      |import logging
      |import os
      |
      |from pydantic import BaseModel
      |from zzz.yyy import load
      |from ldclient.config import Config as LDConfig
      |from ldclient.client import LDClient
      |from ldclient.integrations import Files as LDFiles
      |
      |logger = logging.getLogger(__name__)
      |
      |
      |class DatabaseConfiguration(BaseModel):
      |    url: str
      |    connect_timeout: int = 5000  # ms
      |    statement_timeout: int = 500  # ms
      |
      |    @property
      |    def host(self) -> str:
      |        return self._parsed_url.hostname
      |
      |    @property
      |    def port(self) -> int:
      |        return self._parsed_url.port
      |
      |    @property
      |    def username(self) -> str:
      |        return self._parsed_url.username
      |
      |    @property
      |    def password(self) -> str:
      |        return self._parsed_url.password
      |
      |    @property
      |    def database(self) -> str:
      |        return self._parsed_url.path.strip("/")
      |
      |    @property
      |    def use_ssl(self) -> bool:
      |        ssl = self._parsed_query.get("ssl", [])
      |        return "true" in [s.lower() for s in ssl]
      |
      |    @cached_property
      |    def _parsed_url(self) -> ParseResult:
      |        return urlparse(configuration.db.url)
      |
      |    @cached_property
      |    def _parsed_query(self) -> Dict[str, List[str]]:
      |        return parse_qs(self._parsed_url.query)
      |
      |
      |class DjangoConfiguration(BaseModel):
      |    secret_key: str
      |    allowed_hosts: str
      |    debug: bool = False
      |    launchdarkly_key: str = ""
      |    aws_storage_bucket_name: str
      |    aws_static_storage_bucket_name: str
      |
      |
      |class GoogleConfiguration(BaseModel):
      |    client_id: Optional[str] = None
      |
      |
      |class SentryConfiguration(BaseModel):
      |    dsn: str
      |
      |
      |class LaunchDarklyConfiguration(BaseModel):
      |    sdk_key: Optional[str] = None
      |    config_file: Optional[str] = None  # for local development only
      |
      |    def client_config(self) -> LDConfig:
      |        if self.sdk_key:
      |            return self.__online_config()
      |        if self.config_file:
      |            return self.__file_config()
      |        return self.__offline_config()
      |
      |    def __online_config(self) -> LDConfig:
      |        logger.info("LaunchDarkly configured in online mode")
      |        return LDConfig(self.sdk_key)
      |
      |    def __file_config(self) -> LDConfig:
      |        config_file = os.path.abspath(self.config_file)
      |        logger.info("LaunchDarkly configured from file %s", config_file)
      |        data_source = LDFiles.new_data_source(paths=[config_file], auto_update=True)
      |        return LDConfig("fake-sdk-key", update_processor_class=data_source, send_events=False)
      |
      |    # noinspection PyMethodMayBeStatic
      |    def __offline_config(self) -> LDConfig:
      |        logger.info("LaunchDarkly configured in offline mode; flags will use default values")
      |        return LDConfig("fake-sdk-key", offline=True, send_events=False)
      |
      |
      |class LoggingConfiguration(BaseModel):
      |    default_level: str = "INFO"
      |    django_level: str = "INFO"
      |
      |
      |class Configuration(BaseModel):
      |    environment: str
      |    django: DjangoConfiguration
      |    google: GoogleConfiguration
      |    sentry: Optional[SentryConfiguration] = None
      |    launchdarkly: LaunchDarklyConfiguration = LaunchDarklyConfiguration()
      |    log: LoggingConfiguration = LoggingConfiguration()
      |
      |
      |configuration = Configuration.parse_obj(load("finance", key_separator="__"))
      |launchdarkly = LDClient(configuration.launchdarkly.client_config())""".stripMargin,
    "code.py"
  )

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head shouldBe ("test.env")
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("POSTGRES_DB")
        .contains("finance") shouldBe true
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename shouldBe ("test.env")
    }

    "Two way edge between member and propertyNode" in {
      val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap;
      properties
        .get("POSTGRES_HOSTNAME")
        .contains("db") shouldBe true
    }
  }

}

class INIFileTest extends PythonFrontendTestSuite {
  val cpg = code(
    """
      |[mysql]
      |host = localhost
      |user = user7
      |passwd = s$cret
      |db = ydb""".stripMargin,
    "test.ini"
  ).moreCode(
    """
      |import configparser
      |
      |config = configparser.ConfigParser()
      |db_host = config['mysql']['host']
      |""".stripMargin,
    "code.py"
  )

  "create a file node for the property file" in {
    val files = cpg.file.name.l
    files.filter(_.endsWith(".ini")).head shouldBe ("test.ini")
  }

  "create a `property` node for each property" in {
    val properties = cpg.property.map(x => (x.name, x.value)).toMap
    properties
      .get("host")
      .contains("localhost") shouldBe true
  }

  "connect property nodes to file" in {
    val List(filename: String) = cpg.property.file.name.dedup.l
    filename shouldBe ("test.ini")
  }
}
