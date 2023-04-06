package ai.privado.languageEngine.java.cache

import io.shiftleft.codepropertygraph.generated.nodes.NewModuleDependency

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ModuleCache {

  private val dependenciesModuleMap = new mutable.HashMap[String, List[NewModuleDependency]]()

  private val subModuleParentMap = new mutable.HashMap[String, String]()

  private val subModuleChildMap = new mutable.HashMap[String, ListBuffer[String]]()

  def processDependencies(): Unit = {

    val moduleProcessorQueue = mutable.Queue[String]()

    moduleProcessorQueue.enqueue(getParentAndReverse())

    while(moduleProcessorQueue.nonEmpty) {

      val currentModule = moduleProcessorQueue.dequeue()

    }
  }

  private def getParentAndReverse(): String = {
    var rootModuleFile = ""

    subModuleParentMap.foreach{
      case (key, value) => {
        if (value == null) {
          rootModuleFile = key
        } else {
          if (!subModuleChildMap.contains(value)) {
            subModuleChildMap.put(value, new ListBuffer[String])
          }
          subModuleChildMap(value) += key
        }
      }
    }
    rootModuleFile
  }
}
