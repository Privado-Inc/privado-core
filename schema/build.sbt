name := "standalone-schema"

libraryDependencies += "io.shiftleft" %% "overflowdb-codegen"       % "2.98"
libraryDependencies += "io.shiftleft" %% "codepropertygraph-schema" % Versions.cpg

ThisBuild / resolvers ++= Seq("Github Package Registry" at "https://maven.pkg.github.com/Privado-Inc/codepropertygraph")

lazy val generatedSrcDir = settingKey[File]("root for generated sources - we want to check those in")
enablePlugins(OdbCodegenSbtPlugin)
generateDomainClasses / classWithSchema := "CpgExtSchema$"
generateDomainClasses / fieldName       := "instance"
generateDomainClasses / outputDir       := (Projects.domainClasses / generatedSrcDir).value

credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
