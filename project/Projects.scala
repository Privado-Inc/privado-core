import sbt._
import codeartifact.CodeArtifactKeys._

object Projects {
  lazy val schema = project.in(file("schema"))
  lazy val domainClasses = project.in(file("domain-classes"))
  lazy val schemaExtender = project.in(file("schema-extender"))
}
