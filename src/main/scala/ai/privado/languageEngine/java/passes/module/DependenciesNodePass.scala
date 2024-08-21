package ai.privado.languageEngine.java.passes.module

import ai.privado.languageEngine.java.cache.ModuleCache
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, DiffGraphBuilder}
import io.shiftleft.passes.CpgPass

import scala.collection.mutable

class DependenciesNodePass(cpg: Cpg, moduleCache: ModuleCache) extends CpgPass(cpg) {

  override def init(): Unit = {
    super.init()
    moduleCache.convertToParentChildMap
    moduleCache.processRootDependency
  }

  override def run(builder: DiffGraphBuilder): Unit = {
    processDependencies(builder)
  }

  private def processDependencies(builder: DiffGraphBuilder): Unit = {

    val moduleProcessorQueue = mutable.Queue[String]()

    val childInfo = moduleCache.getRootDependenciesList
    childInfo.foreach(child => {
      moduleProcessorQueue.enqueue(child)
    })

    while (moduleProcessorQueue.nonEmpty) {

      val currentModule = moduleProcessorQueue.dequeue()

      moduleCache
        .getChildList(currentModule)
        .foreach(module => {
          moduleProcessorQueue.enqueue(module)
        })

      moduleCache.inheritParentDependenciesFromChild(currentModule)
      addDependenciesToBuilder(builder, currentModule)
    }
  }

  private def addDependenciesToBuilder(builder: DiffGraphBuilder, currentModule: String): Unit = {
    val module = moduleCache.getModule(currentModule)
    moduleCache
      .getDependencyModuleList(currentModule)
      .foreach(dependency => {
        builder.addEdge(module, dependency, EdgeTypes.DEPENDENCIES)
      })
  }
}
