/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.threatEngine

import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ai.privado.threatEngine.ThreatUtility._
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.xml.{Elem, MetaData, XML}

object DataSharingIPC {

  private val PROVIDER_KEY              = "provider"
  private val PROVIDER_EXPORT_ATTRIBUTE = "exported"
  private val PROVIDER_PERM_ATTRIBUTE   = "permission"
  private val PROVIDER_PERM_R_ATTRIBUTE = "readPermission"
  private val PROVIDER_PERM_W_ATTRIBUTE = "writePermission"
  private val PATH_PERM_KEY             = "path-permission"

  private val INTENT_FILTER_KEY    = "intent-filter"
  private val SDK_KEY              = "uses-sdk"
  private val TARGET_SDK_ATTRIBUTE = "targetSdkVersion"

  private val PERM_KEY                         = "permission"
  private val PROTECTION_LEVEL_ATTRIBUTE       = "protectionLevel"
  private val PROTECTION_LEVEL_ATTRIBUTE_VALUE = "signature"

  private val logger = LoggerFactory.getLogger(getClass)

  /** Fetch all the violations which violate IPC data sharing policy
    * @param androidManifestFile
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg, androidManifestFile: String): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val occurrenceList = ListBuffer[DataFlowSubCategoryPathExcerptModel]()
    val xml: Elem      = XML.loadFile(androidManifestFile)

    val providerNodes = xml \\ PROVIDER_KEY

    if (hasDataElements(cpg) && providerNodes.nonEmpty) {
      var checkForGlobalPermissions = false

      providerNodes.foreach {
//        Elem(prefix, label, attributes, scope, child @ _*)
        case provider: Elem =>
          var isExport = getAttribute(provider.attributes, PROVIDER_EXPORT_ATTRIBUTE) match {
            case Some(x) if x == "true"  => true
            case Some(x) if x == "false" => false
            case _                       =>
              // non-existent, use defaults
              // default true unless SDK target defined >=17
              val targetSDK = getDefinedTargetSDKValue(xml)
              if (targetSDK.nonEmpty && targetSDK.toInt >= 17) false else true
          }

          // if isExport is false, check for intent-filter
          if (!isExport) {
            val intentFilter = provider \\ INTENT_FILTER_KEY
            // if intent-filter is defined, export is true, even when defined "false"
            isExport = intentFilter.nonEmpty
          }

          // mark true if any export is enabled
          checkForGlobalPermissions = checkForGlobalPermissions || isExport

          // check for permissions
          if (isExport) {
            // check if permission attributes are set on provider
            val hasPermAttribute = hasPermissionAttribute(provider.attributes)
            if (!hasPermAttribute) {
              // if no perm attribute, check if path-permission is set
              var isPathPermSet       = false
              val pathPermissionNodes = provider \\ PATH_PERM_KEY
              if (pathPermissionNodes.nonEmpty) {
                pathPermissionNodes.foreach {
                  case pathPermission: Elem =>
                    if (hasPermissionAttribute(pathPermission.attributes)) isPathPermSet = true
                  case _ =>
                }
              }

              if (!isPathPermSet) {
                // negative case: best case to show place expected
                // we lose the formatting of exact string from xml
//              // hence use with custom excerpt
                val occurrenceOutput = getOccurrenceObjectWithCustomExcerpt(
                  provider.toString,
                  provider.attributes.toString,
                  androidManifestFile,
                  excerptPostfix =
                    s"> Missing ${PROVIDER_PERM_ATTRIBUTE}/${PROVIDER_PERM_R_ATTRIBUTE}/${PROVIDER_PERM_W_ATTRIBUTE} for ${PROVIDER_KEY}>"
                )
                occurrenceList.append(occurrenceOutput)
              }
            }
          }
        case _ => // Node not found
      }

      if (checkForGlobalPermissions) {
        val permissionNodes = xml \\ PERM_KEY
        if (permissionNodes.nonEmpty) {
          permissionNodes.foreach {
            case permissionNode: Elem =>
              getAttribute(permissionNode.attributes, PROTECTION_LEVEL_ATTRIBUTE) match {
                case Some(protection) if protection == PROTECTION_LEVEL_ATTRIBUTE_VALUE =>
                case Some(protection)                                                   =>
                  // attribute does not has the correct value
                  val occurrenceOutput = getOccurrenceObject(
                    s"${PROTECTION_LEVEL_ATTRIBUTE}=",
                    s"${PROTECTION_LEVEL_ATTRIBUTE}=\"${protection}\"",
                    androidManifestFile
                  )
                  occurrenceList.append(occurrenceOutput)
                case _ =>
                  // attribute is not set (default: "normal")
                  // https://developer.android.com/guide/topics/manifest/permission-element#plevel

                  // negative case: construct with custom excerpt
                  val occurrenceOutput = getOccurrenceObjectWithCustomExcerpt(
                    permissionNode.toString,
                    s"${PROTECTION_LEVEL_ATTRIBUTE}=\"normal\"",
                    androidManifestFile,
                    excerptPostfix = s"> Missing: ${PROTECTION_LEVEL_ATTRIBUTE}=\"signature\" (default: \"normal\")"
                  )
                  occurrenceList.append(occurrenceOutput)
              }
            case _ =>
          }
        } else {
          // permission key not found. default value for protectionLevel is "normal"
          val occurrenceOutput = getOccurrenceObject(
            "<manifest ", // because permission key is child of manifest key
            s"${PROTECTION_LEVEL_ATTRIBUTE}=\"normal\"",
            androidManifestFile,
            excerptPostfix = s"> Missing: ${PROTECTION_LEVEL_ATTRIBUTE}=\"signature\" (default: \"normal\")"
          )
          occurrenceList.append(occurrenceOutput)
        }
      }
    }

    val sanitizedOccurrenceList = transformOccurrenceList(occurrenceList.toList)
    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }

  private def getDefinedTargetSDKValue(xml: Elem): String = {
    val sdkNode = xml \\ SDK_KEY
    if (sdkNode.nonEmpty) {
      val node = sdkNode.head
      getAttribute(node.attributes, TARGET_SDK_ATTRIBUTE) match {
        case Some(v) => v
        case _       => ""
      }
    } else ""
  }

  private def hasPermissionAttribute(attributes: MetaData): Boolean = {
    // check for permission attributes and return first is found to be set
    getAttribute(attributes, PROVIDER_PERM_ATTRIBUTE) match {
      case Some(x) if x != "" => true
      case _ =>
        getAttribute(attributes, PROVIDER_PERM_W_ATTRIBUTE) match {
          case Some(x) if x != "" => true
          case _ =>
            getAttribute(attributes, PROVIDER_PERM_R_ATTRIBUTE) match {
              case Some(x) if x != "" => true
              case _                  => false
            }
        }
    }
  }
}
