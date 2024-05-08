package ai.privado.testfixtures

import better.files.File

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.annotation.nowarn
import scala.collection.mutable

trait TestCodeWriter {

  private val codeFileNamePairs             = mutable.ArrayBuffer.empty[(String, Option[String])]
  private var fileNameCounter               = 0
  private var outputDirectory: Option[File] = None

  @nowarn
  protected def codeFilePreProcessing(codeFile: Path): Unit = {}

  @nowarn
  protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit = {}

  def moreCode(code: String): this.type = {
    codeFileNamePairs.append((code, None))
    this
  }

  def moreCode(code: String, fileName: String): this.type = {
    codeFileNamePairs.append((code, Option(fileName)))
    this
  }

  def writeCode(extension: String): Path = {
    if (outputDirectory.nonEmpty) {
      throw new RuntimeException("TestCodeWriter may only be used to write code once")
    }
    val tmpDir = if isKotlin(extension) then {
      // Create a subDirectory and store the contents in the subDirectory
      val rootDir           = File.newTemporaryDirectory("x2cpgTestTmpDir")
      val extraKotlinFolder = rootDir / "extraKotlinFolder"
      extraKotlinFolder.createDirectories()
    } else File.newTemporaryDirectory("x2cpgTestTmpDir").deleteOnExit()
    outputDirectory = Some(tmpDir)
    val tmpPath = tmpDir.path
    val codeFiles = codeFileNamePairs.map { case (code, explicitFileName) =>
      val fileName = explicitFileName.getOrElse {
        val filename = s"Test$fileNameCounter$extension"
        fileNameCounter += 1
        filename
      }
      val filePath = Path.of(fileName)
      if (filePath.getParent != null) {
        Files.createDirectories(tmpPath.resolve(filePath.getParent))
      }
      val codeAsBytes = code.getBytes(StandardCharsets.UTF_8)
      val codeFile    = tmpPath.resolve(filePath)
      Files.write(codeFile, codeAsBytes)
      codeFilePreProcessing(codeFile)
      codeFile
    }.toList
    codeDirPreProcessing(tmpPath, codeFiles)
    val item = if isKotlin(extension) then tmpPath.getParent else tmpPath
    item
  }

  def cleanupOutput(): Unit = {
    outputDirectory.foreach(_.delete())
  }

  private def isKotlin(extension: String): Boolean = extension.equals(".kt")
}
