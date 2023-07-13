import sbt.Credentials
name                     := "privado-core"
ThisBuild / organization := "ai.privado"
ThisBuild / scalaVersion := "3.3.0"
ThisBuild / version      := sys.env.getOrElse("BUILD_VERSION", "dev-SNAPSHOT")
// parsed by project/Versions.scala, updated by updateDependencies.sh

val cpgVersion        = "1.4.8"
val joernVersion      = "2.0.13"
val overflowdbVersion = "1.180"

//External dependency versions
val circeVersion   = "0.14.2"
val jacksonVersion = "2.15.2"
val mockitoVersion = "1.17.14"

lazy val schema         = Projects.schema
lazy val domainClasses  = Projects.domainClasses
lazy val schemaExtender = Projects.schemaExtender

dependsOn(domainClasses)
libraryDependencies ++= Seq(
  "com.github.pathikrit"            %% "better-files"            % "3.9.2",
  "com.github.scopt"                %% "scopt"                   % "4.1.0",
  "io.joern"                        %% "x2cpg"                   % Versions.joern,
  "io.joern"                        %% "javasrc2cpg"             % Versions.joern,
  "io.joern"                        %% "pysrc2cpg"               % Versions.joern,
  "io.joern"                        %% "rubysrc2cpg"             % Versions.joern,
  "io.joern"                        %% "joern-cli"               % Versions.joern,
  "io.joern"                        %% "semanticcpg"             % Versions.joern,
  "io.joern"                        %% "semanticcpg"             % Versions.joern % Test classifier "tests",
  "org.scalatest"                   %% "scalatest"               % "3.2.16"       % Test,
  "io.circe"                        %% "circe-core"              % circeVersion,
  "io.circe"                        %% "circe-generic"           % circeVersion,
  "io.circe"                        %% "circe-parser"            % circeVersion,
  // NOTE: circe-yaml currently only goes until 0.14.2 (Last checked 06/07/2023)
  "io.circe"                        %% "circe-yaml"              % circeVersion exclude("org.yaml", "snakeyaml"),
  "com.lihaoyi"                     %% "upickle"                 % "2.0.0",
  "com.lihaoyi"                     %% "requests"                % "0.7.0",
  "org.scala-lang.modules"          %% "scala-xml"               % "2.1.0",
  "commons-io"                       % "commons-io"              % "2.11.0",
  "com.networknt"                    % "json-schema-validator"   % "1.0.72",
  "com.fasterxml.jackson.module"    %% "jackson-module-scala"    % jacksonVersion,
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion exclude("org.yaml", "snakeyaml"),
  "com.github.wnameless.json"        % "json-flattener"          % "0.14.0",
  "org.apache.logging.log4j"         % "log4j-core"              % "2.19.0",
  "org.apache.logging.log4j"         % "log4j-slf4j2-impl"       % "2.19.0"       % Runtime,
  "org.apache.poi"                   % "poi-ooxml"               % "5.2.2",
  "com.github.jsqlparser"            % "jsqlparser"              % "4.6",
  "org.apache.maven"                 % "maven-model"             % "3.9.0",
  "net.sourceforge.htmlunit"         % "htmlunit"                % "2.70.0",
  "org.yaml"                         % "snakeyaml"               % "1.33",
  "org.scala-lang"                   % "scala-reflect"           % "2.13.8",
  "org.scala-lang"                   % "scala-compiler"          % "2.13.8",
  "com.iheart"                      %% "ficus"                   % "1.5.2" exclude ("com.typesafe", "config")
)

ThisBuild / Compile / scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions")

enablePlugins(JavaAppPackaging)

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases"
) ++ Resolver.sonatypeOssRepos("snapshots")
lazy val astGenDlUrl       = "https://github.com/joernio/astgen/releases/download/v3.1.0/"
lazy val astGenBinaryNames = Seq("astgen-linux", "astgen-macos", "astgen-win.exe", "astgen-macos-arm")

lazy val astGenDlTask = taskKey[Unit](s"Download astgen binaries")
astGenDlTask := {
  val astGenDir = baseDirectory.value / "bin" / "astgen"
  astGenDir.mkdirs()

  astGenBinaryNames.foreach { fileName =>
    val dest = astGenDir / fileName
    if (!dest.exists) {
      val url            = s"$astGenDlUrl$fileName"
      val downloadedFile = SimpleCache.downloadMaybe(url)
      IO.copyFile(downloadedFile, dest)
    }
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(astGenDir, distDir)

  // permissions are lost during the download; need to set them manually
  astGenDir.listFiles().foreach(_.setExecutable(true, false))
  distDir.listFiles().foreach(_.setExecutable(true, false))
}
Compile / compile := ((Compile / compile) dependsOn astGenDlTask).value

// Also remove astgen binaries with clean, e.g., to allow for updating them.
// Sadly, we can't define the bin/ folders globally,
// as .value can only be used within a task or setting macro
cleanFiles ++= Seq(
  baseDirectory.value / "bin" / "astgen",
  (Universal / stagingDirectory).value / "bin" / "astgen"
) ++ astGenBinaryNames.map(fileName => SimpleCache.encodeFile(s"$astGenDlUrl$fileName"))
Compile / doc / sources                := Seq.empty
Compile / packageDoc / publishArtifact := false

val repoPass = sys.env.getOrElse("CODEARTIFACT_AUTH_TOKEN", "")
credentials += Credentials("privado/core", sys.env.getOrElse("CODE_ARTIFACT_URL", ""), "aws", repoPass)
publishMavenStyle := true
resolvers += "privado--core" at "https://" + sys.env.getOrElse("CODE_ARTIFACT_URL", "") + "/maven/core"
ThisBuild / publishTo := Some(
  "privado--core" at "https://" + sys.env.getOrElse("CODE_ARTIFACT_URL", "") + "/maven/core"
)

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoKeys := Seq[BuildInfoKey]("joernVersion" -> joernVersion), buildInfoPackage := "privado_core")