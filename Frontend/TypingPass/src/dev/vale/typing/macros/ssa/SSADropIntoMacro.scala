package dev.vale.typing.macros.ssa

import dev.vale.{Keywords, RangeS, StrI, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionDefinitionT, FunctionHeaderT, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import dev.vale.typing.env.{FunctionEnvironment, FunctionEnvironmentBox}
import dev.vale.typing.{ArrayCompiler, CompilerOutputs}
import dev.vale.typing.macros.IFunctionBodyMacro
import dev.vale.typing.types.CoordT
import dev.vale.typing.ast._
import dev.vale.typing.env.FunctionEnvironmentBox
import dev.vale.typing.ast
import dev.vale.typing.names.FunctionDefaultRegionNameT

class SSADropIntoMacro(keywords: Keywords, arrayCompiler: ArrayCompiler) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.vale_static_sized_array_drop_into

  def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: List[RangeS],
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  (FunctionHeaderT, ReferenceExpressionTE) = {
    val header =
      FunctionHeaderT(
        env.fullName,
        Vector.empty,
        Vector(vimpl()), // should we get these handed in
        paramCoords,
        maybeRetCoord.get,
        Some(env.templata))
    coutputs.declareFunctionReturnType(header.toSignature, header.returnType)
    val fate = FunctionEnvironmentBox(env)
    val body =
      BlockTE(
        ReturnTE(
          arrayCompiler.evaluateDestroyStaticSizedArrayIntoCallable(
            coutputs,
            fate,
            callRange,
            ArgLookupTE(0, paramCoords(0).tyype),
            ArgLookupTE(1, paramCoords(1).tyype))))
    (header, body)
  }
}
