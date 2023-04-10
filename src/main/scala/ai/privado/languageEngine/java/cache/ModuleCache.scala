package ai.privado.languageEngine.java.cache

import io.shiftleft.codepropertygraph.generated.nodes.{NewModule, NewModuleDependency}

import scala.collection.mutable

object ModuleCache {

  private val dependenciesModuleMap = new mutable.HashMap[String, mutable.Set[NewModuleDependency]]()

  private val subModuleParentMap = new mutable.HashMap[String, String]()

  private val subModuleChildMap = new mutable.HashMap[String, mutable.HashSet[String]]()

  private val rootDependencyList = new mutable.HashSet[String]()

  private val moduleNameMap = new mutable.HashMap[String, NewModule]()

  def addDependenciesModule(moduleName: String, dependency: mutable.Set[NewModuleDependency]): Unit =
    dependenciesModuleMap.put(moduleName, dependency)

  def getDependencyModuleList(moduleName: String): Set[NewModuleDependency] =
    if (dependenciesModuleMap.contains(moduleName)) dependenciesModuleMap(moduleName).toSet else mutable.Set.empty.toSet

  def getChildList(parentName: String): List[String] =
    if (subModuleChildMap.contains(parentName)) subModuleChildMap(parentName).toList else List.empty

  def getRootDependenciesList: Set[String] = rootDependencyList.toSet

  def addSubModuleParent(childModuleName: String, parentModuleName: String): Unit =
    subModuleParentMap.put(childModuleName, parentModuleName)

  def addModule(moduleName: String, module: NewModule) = moduleNameMap.put(moduleName, module)

  def getModule(moduleName: String): NewModule = moduleNameMap.getOrElse(moduleName, null)

  def convertToParentChildMap: Unit = {
    for ((child, parent) <- subModuleParentMap) {
      if (subModuleParentMap(child) != null) {
        if (!subModuleChildMap.contains(parent)) {
          subModuleChildMap.put(parent, new mutable.HashSet[String]())
        }
        subModuleChildMap(parent) += child
      }
    }
  }

  def processRootDependency: Unit = {
    for ((child, parent) <- subModuleParentMap) {
      if (parent == null) {
        rootDependencyList += child
      }
    }
    val newParentMap = subModuleParentMap.collect { case (key, value) if value != null => key -> value }
    val rootNode     = newParentMap.values.toSet.diff(newParentMap.keySet)
    rootNode.foreach(node => rootDependencyList.add(node))
  }

  def inheritParentDependenciesFromChild(childName: String): Unit = {
    if (subModuleParentMap.contains(childName)) {
      if (dependenciesModuleMap.contains(subModuleParentMap(childName))) {
        dependenciesModuleMap(childName) ++= dependenciesModuleMap(subModuleParentMap(childName))
      }
    }
  }

  // This function is mainly for Testing purposes only
  def cleanCache(): Unit = {
    dependenciesModuleMap.clear()
    subModuleParentMap.clear()
    subModuleChildMap.clear()
    rootDependencyList.clear()
    moduleNameMap.clear()
  }
}
