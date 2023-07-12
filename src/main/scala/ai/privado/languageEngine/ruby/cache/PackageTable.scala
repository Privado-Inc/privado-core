package ai.privado.languageEngine.ruby.cache

import io.joern.x2cpg.Defines.DynamicCallUnknownFullName

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MethodTableModel(methodName: String, parentClassPath: String, classType: String, methodFullName: String)

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() {

  private val methodTableMap = new ConcurrentHashMap[String, mutable.HashSet[MethodTableModel]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String, classType: String): Unit = {
    val packageMethod =
      MethodTableModel(methodName, parentClassPath, classType, s"$moduleName::program:${parentClassPath}:$methodName")
    val moduleMethodSet = methodTableMap.synchronized {
      methodTableMap.computeIfAbsent(moduleName, _ => mutable.HashSet.empty[MethodTableModel])
    }
    moduleMethodSet.add(packageMethod)
  }

  // TODO: will improve the lookup later
  def getMethodFullNameUsingName(packageUsed: List[String], methodName: String): Option[String] = {
    val finalMethodName = ListBuffer[String]()
    packageUsed.foreach(module => {
      if (methodTableMap.containsKey(module)) {
        methodTableMap
          .get(module)
          .filter(_.methodName == methodName)
          .foreach(method => {
            finalMethodName.addOne(method.methodFullName)
          })
      }
    })
    finalMethodName.headOption
  }

  def containsModule(moduleName: String): Boolean = {
    methodTableMap.containsKey(moduleName)
  }
}
