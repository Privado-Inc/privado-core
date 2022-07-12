name := "privado-core"
ThisBuild / organization := "ai.privado"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.8"

libraryDependencies ++=Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.17.1" % Runtime,
  "io.joern" %% "x2cpg" % "1.1.873",
  "io.joern" %% "javasrc2cpg" % "1.1.873",
  "io.joern" %% "joern-cli" % "1.1.873",
  "io.joern" %% "semanticcpg" % "1.1.873",
  "io.joern" %% "semanticcpg" % "1.1.873" % Test classifier "tests",
  "org.scalatest" %% "scalatest" % "3.1.1" % Test
)