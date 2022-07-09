package dev.vale.typing.macros

import dev.vale.{Keywords, RangeS, StrI, vimpl}
import dev.vale.typing.CompilerOutputs
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionHeaderT, FunctionT, IsSameInstanceTE, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.env.FunctionEnvironment
import dev.vale.typing.types.CoordT
import dev.vale.highertyping.FunctionA
import dev.vale.typing.ast
import dev.vale.typing.ast._
import dev.vale.typing.function.FunctionCompilerCore

class SameInstanceMacro(keywords: Keywords) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.vale_same_instance

  def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val header =
      FunctionHeaderT(env.fullName, Vector.empty, paramCoords, maybeRetCoord.get, originFunction)
    coutputs.declareFunctionReturnType(header.toSignature, header.returnType)
    coutputs.addFunction(
      FunctionT(
        header,
        vimpl(),
        BlockTE(
          ReturnTE(
            IsSameInstanceTE(
              ArgLookupTE(0, paramCoords(0).tyype), ArgLookupTE(1, paramCoords(1).tyype))))))
    header
  }
}
