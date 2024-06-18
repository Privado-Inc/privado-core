package ai.privado.metadata

import ai.privado.model.Constants.outputDirectoryName
import better.files.File
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import SystemInfoEncoderDecoder.*

import java.nio.file.{Files, Paths}

object SystemInfo {

  def getInfo: SystemInfo = {
    import java.lang.management._

    // Operating System Information
    val osBean: OperatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean
    val operatingSystemInfo =
      OperatingSystemInfo(osBean.getName, osBean.getVersion, osBean.getArch, osBean.getAvailableProcessors)

    // Memory Information
    import com.sun.management.OperatingSystemMXBean
    val memoryOSBean = ManagementFactory.getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
    // Convert bytes to gigabytes for readability
    val systemMemoryInfo = SystemMemoryInfo(
      convertBytesToGB(memoryOSBean.getTotalMemorySize),
      convertBytesToGB(memoryOSBean.getTotalSwapSpaceSize)
    )

    // Get the file store for the current path
    val fileStore = Files.getFileStore(Paths.get("").toAbsolutePath)
    val systemDiskInfo = SystemDiskInfo(
      fileStore.name(),
      convertBytesToGB(fileStore.getTotalSpace),
      convertBytesToGB(fileStore.getUnallocatedSpace)
    )

    val systemInfo = SystemInfo(operatingSystemInfo, systemMemoryInfo, systemDiskInfo)
    println("Printing system Info")
    println(systemInfo.asJson.toString)
    systemInfo
  }

  def dumpInfoToFile(repoPath: String, outputFileName: String, systemInfo: SystemInfo): File = {
    val outputDirectory = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
    val f               = File(s"$repoPath/$outputDirectoryName/$outputFileName")
    f.write(systemInfo.asJson.toString)
    f
  }

  private def convertBytesToGB(size: Long) = size / (1024 * 1024 * 1024)
}

case class SystemInfo(
  operatingSystemInfo: OperatingSystemInfo,
  systemMemoryInfo: SystemMemoryInfo,
  systemDiskInfo: SystemDiskInfo
)

case class OperatingSystemInfo(name: String, version: String, architecture: String, processorCount: Int)

case class SystemMemoryInfo(memorySizeInGB: Long, swapSizeInGB: Long)

case class SystemDiskInfo(fileStoreName: String, totalSpaceInGB: Long, freeSpaceInGB: Long)

object SystemInfoEncoderDecoder {

  implicit val systemInfoDecoder: Decoder[SystemInfo] = deriveDecoder[SystemInfo]
  implicit val systemInfoEncoder: Encoder[SystemInfo] = deriveEncoder[SystemInfo]

  implicit val operatingSystemInfoDecoder: Decoder[OperatingSystemInfo] = deriveDecoder[OperatingSystemInfo]
  implicit val operatingSystemInfoEncoder: Encoder[OperatingSystemInfo] = deriveEncoder[OperatingSystemInfo]

  implicit val systemMemoryInfoDecoder: Decoder[SystemMemoryInfo] = deriveDecoder[SystemMemoryInfo]
  implicit val systemMemoryInfoEncoder: Encoder[SystemMemoryInfo] = deriveEncoder[SystemMemoryInfo]

  implicit val systemDiskInfoDecoder: Decoder[SystemDiskInfo] = deriveDecoder[SystemDiskInfo]
  implicit val systemDiskInfoEncoder: Encoder[SystemDiskInfo] = deriveEncoder[SystemDiskInfo]

}
