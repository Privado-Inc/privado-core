import sbt.{Credentials}
name                     := "privado-core"
ThisBuild / organization := "ai.privado"
ThisBuild / scalaVersion := "2.13.7"
ThisBuild / version      := sys.env.getOrElse("BUILD_VERSION", "dev-SNAPSHOT")
// parsed by project/Versions.scala, updated by updateDependencies.sh
val cpgVersion = "1.3.561"
val joernVersion = "1.1.1078"
val overflowdbVersion = "1.147"
//External dependency versions
val circeVersion = "0.14.1"
val jacksonVersion = "2.13.4"

lazy val schema         = Projects.schema
lazy val domainClasses  = Projects.domainClasses
lazy val schemaExtender = Projects.schemaExtender

dependsOn(domainClasses)
libraryDependencies ++= Seq(
  "com.github.pathikrit"    %% "better-files"     % "3.9.1",
  "com.github.scopt"        %% "scopt"            % "3.7.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.17.1"       % Runtime,
  "io.joern"                %% "x2cpg"            % Versions.joern,
  "io.joern"                %% "javasrc2cpg"      % Versions.joern,
  "io.joern"                %% "joern-cli"        % Versions.joern,
  "io.joern"                %% "semanticcpg"      % Versions.joern,
  "io.joern"                %% "semanticcpg"      % Versions.joern % Test classifier "tests",
  "org.scalatest"           %% "scalatest"        % "3.1.1"        % Test,
  "io.circe"                %% "circe-core"       % circeVersion,
  "io.circe"                %% "circe-generic"    % circeVersion,
  "io.circe"                %% "circe-parser"     % circeVersion,
  "io.circe"                %% "circe-yaml"       % circeVersion,
  "com.lihaoyi"             %% "upickle"          % "2.0.0",
  "com.lihaoyi"             %% "requests"         % "0.7.0",
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
  "commons-io" % "commons-io" % "2.11.0",
  "com.networknt" % "json-schema-validator" % "1.0.72",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion,
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion
)

ThisBuild / Compile / scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions")

enablePlugins(JavaAppPackaging)

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases",
  Resolver.sonatypeRepo("snapshots"))


Compile / doc / sources                := Seq.empty
Compile / packageDoc / publishArtifact := false


val repoPass = sys.env.get("CODEARTIFACT_AUTH_TOKEN").getOrElse("")
credentials += Credentials("privado/core", sys.env.get("CODE_ARTIFACT_URL").getOrElse(""), "aws", repoPass)
publishMavenStyle := true
resolvers += "privado--core" at "https://" + sys.env.get("CODE_ARTIFACT_URL").getOrElse("") + "/maven/core"
ThisBuild / publishTo := Some("privado--core" at "https://" + sys.env.get("CODE_ARTIFACT_URL").getOrElse("")+ "/maven/core")
