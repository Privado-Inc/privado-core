name := "standalone-schema"

libraryDependencies += "io.shiftleft" %% "overflowdb-codegen"       % "2.98"
libraryDependencies += "io.shiftleft" %% "codepropertygraph-schema" % Versions.cpg

lazy val generatedSrcDir = settingKey[File]("root for generated sources - we want to check those in")
enablePlugins(OdbCodegenSbtPlugin)
generateDomainClasses / classWithSchema := "CpgExtSchema$"
generateDomainClasses / fieldName       := "instance"
generateDomainClasses / outputDir       := (Projects.domainClasses / generatedSrcDir).value
