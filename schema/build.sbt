name := "standalone-schema"

libraryDependencies += "io.shiftleft" %% "overflowdb-codegen" % "2.21"
libraryDependencies += "io.shiftleft" %% "codepropertygraph-schema" % Versions.cpg

Compile / generateDomainClasses / classWithSchema := "CpgExtSchema$"
Compile / generateDomainClasses / fieldName := "instance"
