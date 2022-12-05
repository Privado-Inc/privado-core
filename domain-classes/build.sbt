name := "joern-standalone-domain-classes"

libraryDependencies += "io.shiftleft" %% "overflowdb-traversal" % Versions.overflowdb
Compile / sourceGenerators += Projects.schema / Compile / generateDomainClasses
Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Wunused", "-Ywarn-unused")
