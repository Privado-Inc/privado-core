package ai.privado.languageEngine.java.language

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{File, ModuleDependency}
import overflowdb.traversal._

import scala.jdk.CollectionConverters.IteratorHasAsScala

object module {

  implicit class NodeStarters(cpg: Cpg) {

    def module: Traversal[io.shiftleft.codepropertygraph.generated.nodes.Module] =
      cpg.graph.nodes(NodeTypes.MODULE).asScala.cast[io.shiftleft.codepropertygraph.generated.nodes.Module]
  }

  implicit class StepsForModule(val trav: Traversal[io.shiftleft.codepropertygraph.generated.nodes.Module])
      extends AnyVal {

    def file: Traversal[File] = trav.out(EdgeTypes.SOURCE_FILE).cast[File]

    def dependencies: Traversal[ModuleDependency] = trav.out(EdgeTypes.DEPENDENCIES).cast[ModuleDependency]
  }

  implicit class StepsForDependency(
    val traversal: Traversal[io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency]
  ) extends AnyVal {

    def file: Traversal[File] = traversal.out(EdgeTypes.SOURCE_FILE).cast[File]

  }

}
