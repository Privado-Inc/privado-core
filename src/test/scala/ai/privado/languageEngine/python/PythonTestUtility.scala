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
 *
 */

package ai.privado.languageEngine.python

import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.pysrc2cpg._
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

object PythonTestUtility {

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.py").write(code)
    val outputFile = File.newTemporaryFile()

    val cpgconfig = Py2CpgOnFileSystemConfig(File(".venv").path, true)
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
    new Py2CpgOnFileSystem()
      .createCpg(cpgconfig)
      .map { cpg =>
        X2Cpg.applyDefaultOverlays(cpg)
        new ImportsPass(cpg).createAndApply()
        new PythonInheritanceNamePass(cpg).createAndApply()
        new PythonTypeRecoveryPass(cpg).createAndApply()
        new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
        new NaiveCallLinker(cpg).createAndApply()
        new AstLinkerPass(cpg).createAndApply()
        new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
        cpg
      }
      .get
  }

}
