package ai.privado.languageEngine.java.passes.module

import ai.privado.languageEngine.java.cache.ModuleCache
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate

import scala.collection.mutable

class DependenciesNodePass(cpg: Cpg) extends CpgPass(cpg) {

  override def init(): Unit = {
    super.init()
    ModuleCache.convertToParentChildMap
    ModuleCache.processRootDependency
  }

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    processDependencies(builder)
  }

  private def processDependencies(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val moduleProcessorQueue = mutable.Queue[String]()

    val childInfo = ModuleCache.getRootDependenciesList
    childInfo.foreach(child => {
      moduleProcessorQueue.enqueue(child)
    })

    while (moduleProcessorQueue.nonEmpty) {

      val currentModule = moduleProcessorQueue.dequeue()

      ModuleCache
        .getChildList(currentModule)
        .foreach(module => {
          moduleProcessorQueue.enqueue(module)
        })

      ModuleCache.inheritParentDependenciesFromChild(currentModule)
      addDependenciesToBuilder(builder, currentModule)
    }
  }

  private def addDependenciesToBuilder(builder: BatchedUpdate.DiffGraphBuilder, currentModule: String): Unit = {
    val module = ModuleCache.getModule(currentModule)
    ModuleCache
      .getDependencyModuleList(currentModule)
      .foreach(dependency => {
        builder.addEdge(module, dependency, EdgeTypes.DEPENDENCIES)
      })
  }
}
