package ai.privado.languageEngine.java.language

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Dependency, File, JavaConfiguration, JavaDependency, NewJavaDependency}
import overflowdb.traversal.{Traversal, jIteratortoTraversal}

object configuration {

  implicit class NodeStarters(cpg: Cpg) {

    def configuration: Traversal[JavaConfiguration] =
      cpg.graph.nodes(NodeTypes.JAVA_CONFIGURATION).cast[JavaConfiguration]
  }

  implicit class StepsForConfiguration(val trav: Traversal[JavaConfiguration]) extends AnyVal {

    def file: Traversal[File] = trav.out(EdgeTypes.SOURCE_FILE).cast[File]

    def dependencies: Traversal[JavaDependency] = trav.out(EdgeTypes.DEPENDENCIES).cast[JavaDependency]
  }

}
