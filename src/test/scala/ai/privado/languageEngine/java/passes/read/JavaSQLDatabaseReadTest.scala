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

package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{AppCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.*
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JavaSQLDatabaseReadTest extends DatabaseReadPassTestBase {
  override val codeFileContents: String = s"""
                                             |import java.sql.Connection;
                                             |import java.sql.DriverManager;
                                             |import java.sql.PreparedStatement;
                                             |import java.sql.ResultSet;
                                             |import java.sql.SQLException;
                                             |
                                             |public class User {
                                             |    private int id;
                                             |    private String username;
                                             |    private String password;
                                             |    private String email;
                                             |
                                             |    // Constructor
                                             |    public User(int id, String username, String password, String email) {
                                             |        this.id = id;
                                             |        this.username = username;
                                             |        this.password = password;
                                             |        this.email = email;
                                             |    }
                                             |
                                             |    // Getters and setters
                                             |    public int getId() {
                                             |        return id;
                                             |    }
                                             |
                                             |    public void setId(int id) {
                                             |        this.id = id;
                                             |    }
                                             |
                                             |    public String getUsername() {
                                             |        return username;
                                             |    }
                                             |
                                             |    public void setUsername(String username) {
                                             |        this.username = username;
                                             |    }
                                             |
                                             |    public String getPassword() {
                                             |        return password;
                                             |    }
                                             |
                                             |    public void setPassword(String password) {
                                             |        this.password = password;
                                             |    }
                                             |
                                             |    public String getEmail() {
                                             |        return email;
                                             |    }
                                             |
                                             |    public void setEmail(String email) {
                                             |        this.email = email;
                                             |    }
                                             |
                                             |    // Method to read a user from the database by ID
                                             |    public static User getUserById(int id) throws SQLException {
                                             |        Connection conn = null;
                                             |        PreparedStatement stmt = null;
                                             |        ResultSet rs = null;
                                             |        User user = null;
                                             |
                                             |        try {
                                             |            // Connect to the database
                                             |            conn = DriverManager.getConnection("jdbc:mysql://localhost/mydatabase", "username", "password");
                                             |
                                             |            // Prepare the SQL statement
                                             |            stmt = conn.prepareStatement("SELECT firstName, lastName FROM users WHERE id = ?");
                                             |            stmt.setInt(1, id);
                                             |
                                             |            // Execute the query and get the result set
                                             |            rs = stmt.executeQuery();
                                             |
                                             |            // If a user with the given ID exists, create a User object and populate its fields
                                             |            if (rs.next()) {
                                             |                user = new User(rs.getInt("id"), rs.getString("username"), rs.getString("password"), rs.getString("email"));
                                             |            }
                                             |        } catch (SQLException e) {
                                             |            e.printStackTrace();
                                             |        } finally {
                                             |            // Close the database resources
                                             |            if (rs != null) rs.close();
                                             |            if (stmt != null) stmt.close();
                                             |            if (conn != null) conn.close();
                                             |        }
                                             |
                                             |        return user;
                                             |    }
                                             |}
                                             |""".stripMargin

  "DatabaseReadPass" should {
    "Tag the Select SQL Query as param" in {
      cpg.tag.name(InternalTag.VARIABLE_REGEX_LITERAL.toString).size shouldNot (equal(0))
    }

    "Tag the SQL query with source node id" in {
      cpg.tag.name("id").value("Data.Sensitive.FirstName").size shouldNot (equal(0))
    }
  }

}
class JavaSQLDatabaseReadVariableTest extends DatabaseReadPassTestBase {
  override val codeFileContents: String = s"""
                                             |import java.sql.Connection;
                                             |import java.sql.DriverManager;
                                             |import java.sql.PreparedStatement;
                                             |import java.sql.ResultSet;
                                             |import java.sql.SQLException;
                                             |
                                             |public class User {
                                             |    private int id;
                                             |    private String username;
                                             |    private String password;
                                             |    private String email;
                                             |
                                             |    // Constructor
                                             |    public User(int id, String username, String password, String email) {
                                             |        this.id = id;
                                             |        this.username = username;
                                             |        this.password = password;
                                             |        this.email = email;
                                             |    }
                                             |
                                             |    // Getters and setters
                                             |    public int getId() {
                                             |        return id;
                                             |    }
                                             |
                                             |    public void setId(int id) {
                                             |        this.id = id;
                                             |    }
                                             |
                                             |    public String getUsername() {
                                             |        return username;
                                             |    }
                                             |
                                             |    public void setUsername(String username) {
                                             |        this.username = username;
                                             |    }
                                             |
                                             |    public String getPassword() {
                                             |        return password;
                                             |    }
                                             |
                                             |    public void setPassword(String password) {
                                             |        this.password = password;
                                             |    }
                                             |
                                             |    public String getEmail() {
                                             |        return email;
                                             |    }
                                             |
                                             |    public void setEmail(String email) {
                                             |        this.email = email;
                                             |    }
                                             |
                                             |    // Method to read a user from the database by ID
                                             |    public static User getUserById(int id) throws SQLException {
                                             |        Connection conn = null;
                                             |        PreparedStatement stmt = null;
                                             |        ResultSet rs = null;
                                             |        User user = null;
                                             |
                                             |        try {
                                             |            // Connect to the database
                                             |            conn = DriverManager.getConnection("jdbc:mysql://localhost/mydatabase", "username", "password");
                                             |
                                             |            // Prepare the SQL statement
                                             |            String query = "SELECT lastName, firstName,email from user where (lower(firstName) like lower(?) or lower(lastName) "
                                             |                    + "like lower(?) or lower(email) like lower(?)) and userType = 0";
                                             |            stmt = conn.prepareStatement("SELECT firstName, lastName FROM users WHERE id = ?");
                                             |            stmt.setInt(1, id);
                                             |
                                             |            // Execute the query and get the result set
                                             |            rs = stmt.executeQuery();
                                             |
                                             |            // If a user with the given ID exists, create a User object and populate its fields
                                             |            if (rs.next()) {
                                             |                user = new User(rs.getInt("id"), rs.getString("username"), rs.getString("password"), rs.getString("email"));
                                             |            }
                                             |        } catch (SQLException e) {
                                             |            e.printStackTrace();
                                             |        } finally {
                                             |            // Close the database resources
                                             |            if (rs != null) rs.close();
                                             |            if (stmt != null) stmt.close();
                                             |            if (conn != null) conn.close();
                                             |        }
                                             |
                                             |        return user;
                                             |    }
                                             |}
                                             |""".stripMargin

  "DatabaseReadPass" should {
    "Tag the multi-line SQL Query as param" in {
      cpg.tag.name(InternalTag.VARIABLE_REGEX_LITERAL.toString).size shouldNot (equal(0))
    }

    "Parse the query properly and tag with Source ID" in {
      cpg.tag.name("id").value("Data.Sensitive.FirstName").size shouldNot (equal(0))
    }
  }

}
abstract class DatabaseReadPassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val codeFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()

    (inputDir / "unrelated.file").write("foo")
    outputFile = File.newTemporaryFile()

    (inputDir / "SQLQueryFile.java").write(codeFileContents)
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    cpg = new JavaSrc2Cpg().createCpg(config).get
    val taggerCache: TaggerCache = new TaggerCache
    val sourceRule = List(
      RuleInfo(
        "Data.Sensitive.FirstName",
        "FirstName",
        "",
        FilterProperty.METHOD_FULL_NAME,
        Array(),
        List("(?i).*firstName.*"),
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
    val collectionRule = List(
      RuleInfo(
        "Collections.Annotation.Spring",
        "Spring Web Interface Annotation",
        "",
        FilterProperty.METHOD_FULL_NAME,
        Array(),
        List("RequestMapping|PostMapping|PutMapping|GetMapping|DeleteMapping"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.COLLECTIONS,
        "",
        Language.JAVA,
        Array()
      )
    )
    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())
    ruleCache.setRule(rule)
    val nodeCache = CPGNodeCacheForSourceTagger(cpg, ruleCache)
    new DirectNodeSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new FirstLevelDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new OCDDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new ExtendingDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
    new DatabaseQueryReadPass(
      cpg,
      ruleCache,
      taggerCache,
      PrivadoInput(),
      EntityMapper.getClassTableMapping(cpg),
      appCache = new AppCache()
    )
      .createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }
}
