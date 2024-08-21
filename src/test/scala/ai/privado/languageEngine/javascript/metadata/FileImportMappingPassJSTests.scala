package ai.privado.languageEngine.javascript.metadata

import ai.privado.cache.FileLinkingMetadata
import ai.privado.testfixtures.JavaScriptBaseCpgFrontendTestSuite

import scala.collection.immutable.Set

class FileImportMappingPassJSTests extends JavaScriptBaseCpgFrontendTestSuite {

  "File import mapping in javascript" should {
    "resolve import files case 1" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from './module.js';
          |functionName();
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from module');
          |}
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 2" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import defaultExport from './module.js';
          |defaultExport();
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |export default function defaultExport() {}
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 3" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import * as module from './module.js';
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |export const someValue = 42;
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 4" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |const module = require('./module.js');
          |module();
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |module.exports = function() {
          |  console.log('Function from module');
          |};
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 5" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |const { functionName } = require('./module.js');
          |functionName();
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |module.exports.functionName = function() {};
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 6" ignore {
      // TODO Failing as no import node created
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |require(['./module'], function(module) {
          |  module.functionName();
          |});
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |module.exports.functionName = function() {};
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 7" ignore {
      // TODO Failing as no import node created
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import('./module.js').then(module => {
          |  module.functionName();
          |});
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |module.exports.functionName = function() {};
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 8" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from 'common/module';
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 9" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from 'common/module';
          |
          |""".stripMargin,
        "src/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 10" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { module } from 'common';
          |
          |""".stripMargin,
        "src/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/modules/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/util.js") shouldBe Set("src/common/modules/module.js")
    }

    "resolve import files case 11" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { module, helper } from 'common';
          |
          |""".stripMargin,
        "src/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/modules/module.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/helpers/helper.js"
      ).moreCode(
        """
            |export function functionName() {}
            |
            |""".stripMargin,
        "src/common/deprecateds/Deprecated.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/util.js") shouldBe Set(
        "src/common/modules/module.js",
        "src/common/helpers/helper.js"
      )
    }

    "resolve import files case 12" ignore {
      // TODO Failing as no import node created
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |const functionName = () => import(/* webpackChunkName: "module" */ 'common/module');
          |
          |""".stripMargin,
        "src/util.js"
      ).moreCode(
        """
          |export function functionName() {}
          |
          |""".stripMargin,
        "src/common/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/util.js") shouldBe Set("src/common/module.js")
    }

    "resolve import files case 13" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from '../module.js';
          |functionName();
          |
          |""".stripMargin,
        "src/common/util.js"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from module');
          |}
          |
          |""".stripMargin,
        "src/module.js"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.js") shouldBe Set("src/module.js")
    }
  }

  "File import mapping in TypeScript" should {
    "resolve import files case 1" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from './module';
          |functionName();
          |
          |""".stripMargin,
        "src/common/util.ts"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from module');
          |}
          |
          |""".stripMargin,
        "src/common/module.ts"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.ts") shouldBe Set("src/common/module.ts")
    }

    "resolve import files case 2" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import type { TypeName } from './module';
          |
          |""".stripMargin,
        "src/common/util.ts"
      ).moreCode(
        """
          |export type TypeName = { /* ... */ };
          |
          |""".stripMargin,
        "src/common/module.ts"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/common/util.ts") shouldBe Set("src/common/module.ts")
    }

    "resolve import files case 3" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from '@utils/module';
          |
          |""".stripMargin,
        "src/main.ts"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from aliased path');
          |}
          |""".stripMargin,
        "common/module.ts"
      ).moreCode(
        """
          |{
          |  "compilerOptions": {
          |    "baseUrl": "./",
          |    "paths": {
          |      "@utils/module": ["common/module"]
          |    }
          |  }
          |}
          |""".stripMargin,
        "tsconfig.json"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/main.ts") shouldBe Set("common/module.ts")
    }

    "resolve import files case 4" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import { functionName } from '@utils/module';
          |
          |""".stripMargin,
        "src/main.ts"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from aliased path');
          |}
          |""".stripMargin,
        "src/common/module.ts"
      ).moreCode(
        """
          |{
          |  "compilerOptions": {
          |    "baseUrl": "./",
          |    "paths": {
          |      "@utils/*": ["src/common/*"]
          |    }
          |  }
          |}
          |""".stripMargin,
        "tsconfig.json"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/main.ts") shouldBe Set("src/common/module.ts")
    }

    "resolve import files case 5" in {
      val fileLinkingMetadata = FileLinkingMetadata()
      val cpg = code(
        """
          |import * as module from '@utils/module';
          |
          |""".stripMargin,
        "src/main.ts"
      ).moreCode(
        """
          |export function functionName() {
          |  console.log('Function from aliased path');
          |}
          |""".stripMargin,
        "src/common/module.ts"
      ).moreCode(
        """
          |{
          |  "compilerOptions": {
          |    "baseUrl": "./",
          |    "paths": {
          |      "@utils/*": ["src/common/*"]
          |    }
          |  }
          |}
          |""".stripMargin,
        "tsconfig.json"
      ).withFileLinkingMetadata(fileLinkingMetadata)

      cpg.getFileLinkingData.getFileImportMap("src/main.ts") shouldBe Set("src/common/module.ts")
    }
  }

}
