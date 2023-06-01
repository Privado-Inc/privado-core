name := "standalone-schema"

libraryDependencies += "io.shiftleft" %% "overflowdb-codegen" % "2.87"
libraryDependencies += "io.shiftleft" %% "codepropertygraph-schema" % Versions.cpg

Compile / generateDomainClasses / classWithSchema := "CpgExtSchema$"
Compile / generateDomainClasses / fieldName := "instance"
