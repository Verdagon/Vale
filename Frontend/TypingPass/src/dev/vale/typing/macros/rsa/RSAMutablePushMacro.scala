package dev.vale.typing.macros.rsa

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing._
import dev.vale.typing.CompilerOutputs
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionDefinitionT, FunctionHeaderT, LocationInFunctionEnvironment, ParameterT, PushRuntimeSizedArrayTE, ReturnTE}
import dev.vale.typing.env.{FunctionEnvironment, TemplataLookupContext}
import dev.vale.typing.macros.IFunctionBodyMacro
import dev.vale.typing.templata.{CoordTemplata, MutabilityTemplata}
import dev.vale.typing.types._
import dev.vale.{Interner, Keywords, Profiler, RangeS, StrI, vassertSome, vimpl}
import dev.vale.postparsing.CodeRuneS
import dev.vale.typing.ast._
import dev.vale.typing.env.TemplataLookupContext
import dev.vale.typing.types._
import dev.vale.typing.ast
import dev.vale.typing.names.DenizenDefaultRegionNameT


class RSAMutablePushMacro(interner: Interner, keywords: Keywords) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.vale_runtime_sized_array_push

  def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  (FunctionHeaderT, ReferenceExpressionTE) = {
    val header =
      FunctionHeaderT(
        env.id,
        Vector.empty,
//        Vector(RegionT(env.defaultRegion.localName, true)),
        paramCoords,
        maybeRetCoord.get,
        Some(env.templata))
    val body =
      BlockTE(
        ReturnTE(
          PushRuntimeSizedArrayTE(
            ArgLookupTE(0, paramCoords(0).tyype),
            ArgLookupTE(1, paramCoords(1).tyype))))
    (header, body)
  }
}