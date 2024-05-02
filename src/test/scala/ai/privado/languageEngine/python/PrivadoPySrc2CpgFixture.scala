package ai.privado.languageEngine.python

import ai.privado.cache.AppCache
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.passes.ExperimentalLambdaDataFlowSupportPass
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class PrivadoPySrc2CpgFixture extends Code2CpgFixture(() => new PrivadoPySrcTestCpg()) {

  implicit val resolver: ICallResolver = NoResolve
  implicit val context: EngineContext  = EngineContext()

}

class PrivadoPySrcTestCpg extends PySrcTestCpg {

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    new ImportsPass(this).createAndApply()
    new PythonImportResolverPass(this).createAndApply()
    new PythonInheritanceNamePass(this).createAndApply()
    new DynamicTypeHintFullNamePass(this).createAndApply()
    new PythonTypeRecoveryPassGenerator(this).generate().foreach(_.createAndApply())
    new PrivadoPythonTypeHintCallLinker(this).createAndApply()
    new NaiveCallLinker(this).createAndApply()

    // Some of passes above create new methods, so, we
    // need to run the ASTLinkerPass one more time
    new AstLinkerPass(this).createAndApply()

    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)

    new ExperimentalLambdaDataFlowSupportPass(this).createAndApply()
  }

}
