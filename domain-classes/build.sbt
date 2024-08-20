name := "joern-standalone-domain-classes"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
)

ThisBuild / resolvers ++= Seq(
  "Github Package Registry Codepropertygraph" at "https://maven.pkg.github.com/Privado-Inc/codepropertygraph",
  Resolver.mavenLocal
)

lazy val generatedSrcDir = settingKey[File]("root for generated sources - we want to check those in")
generatedSrcDir := (Compile / sourceDirectory).value / "generated"

Compile / unmanagedSourceDirectories += generatedSrcDir.value
Compile / compile := (Compile / compile).dependsOn(Projects.schema / Compile / generateDomainClasses).value

Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Wunused", "-Ywarn-unused")
