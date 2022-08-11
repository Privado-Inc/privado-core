package ai.privado.cache
import ai.privado.utility.Utilities._

/** Cache Used to store Application/Scan specific information
  */
object AppCache {

  var localScanPath: String      = ""
  var repoName: String           = ""
  var privadoVersionMain: String = ""

  def init(scanPath: String) = {
    this.localScanPath = getRepoScanPath(scanPath)
    this.repoName = this.localScanPath.split("/").last
  }
}
