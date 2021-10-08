package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.FunctionA
import net.verdagon.vale.templar.ast._
import net.verdagon.vale.templar.env.{FunctionEnvironment, FunctionEnvironmentBox}
import net.verdagon.vale.templar.types.CoordT
import net.verdagon.vale.templar.{ArrayTemplar, Temputs, ast}
import net.verdagon.vale.{IProfiler, RangeS}


class RSALenMacro() {
  def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val header =
      ast.FunctionHeaderT(env.fullName, Vector.empty, paramCoords, maybeRetCoord.get, originFunction)
    temputs.declareFunctionReturnType(header.toSignature, header.returnType)
    temputs.addFunction(
      ast.FunctionT(
        header,
        BlockTE(
          ReturnTE(
            ArrayLengthTE(
              ArgLookupTE(0, paramCoords(0).tyype))))))
    header
  }
}