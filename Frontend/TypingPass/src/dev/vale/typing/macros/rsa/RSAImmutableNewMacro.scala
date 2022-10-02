package dev.vale.typing.macros.rsa

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing._
import dev.vale.typing.{ArrayCompiler, CompileErrorExceptionT, CompilerErrorHumanizer, CompilerOutputs, CouldntFindFunctionToCallT, OverloadResolver, ast}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionDefinitionT, FunctionHeaderT, LocationInFunctionEnvironment, NewImmRuntimeSizedArrayTE, ParameterT, ReturnTE}
import dev.vale.typing.env.{FunctionEnvironment, TemplataLookupContext}
import dev.vale.typing.macros.IFunctionBodyMacro
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, StrI, vassert, vassertSome, vfail, vimpl, vwat}
import dev.vale.postparsing.CodeRuneS
import dev.vale.typing.ast._
import dev.vale.typing.env.TemplataLookupContext
import dev.vale.typing.function.DestructorCompiler
import dev.vale.typing.names.FunctionDefaultRegionNameT
import dev.vale.typing.templata.PrototypeTemplata
import dev.vale.typing.types._


class RSAImmutableNewMacro(
  interner: Interner,
  keywords: Keywords,
  overloadResolver: OverloadResolver,
  arrayCompiler: ArrayCompiler,
  destructorCompiler: DestructorCompiler
) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.vale_runtime_sized_array_imm_new

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

    val CoordTemplata(elementType) =
      vassertSome(
        env.lookupNearestWithImpreciseName(
          interner.intern(RuneNameS(CodeRuneS(keywords.E))), Set(TemplataLookupContext)))

    val mutability =
      ITemplata.expectMutability(
        vassertSome(
          env.lookupNearestWithImpreciseName(
            interner.intern(RuneNameS(CodeRuneS(keywords.M))), Set(TemplataLookupContext))))

    val arrayTT = arrayCompiler.resolveRuntimeSizedArray(elementType, mutability)

    val generatorArgCoord =
      paramCoords(1).tyype match {
        case c @ CoordT(ShareT, _, _) => c
        case c @ CoordT(BorrowT, _, _) => c
        case CoordT(OwnT, _, _) => vwat() // shouldnt happen, signature takes in an &
      }

    val generatorPrototype =
      overloadResolver.findFunction(
        env,
        coutputs,
        callRange,
        interner.intern(CodeNameS(keywords.underscoresCall)),
        Vector(),
        Vector(),
        Vector(generatorArgCoord, CoordT(ShareT, vimpl(), IntT(32))),
        Vector(),
        false,
        true) match {
        case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, e))
        case Ok(x) => x
      }

    vassert(generatorPrototype.prototype.prototype.returnType.ownership == ShareT)

    val sizeTE = ArgLookupTE(0, paramCoords(0).tyype)
    val generatorTE = ArgLookupTE(1, paramCoords(1).tyype)

    val body =
      BlockTE(
        ReturnTE(
          NewImmRuntimeSizedArrayTE(
            arrayTT,
            vimpl(),
            sizeTE,
            generatorTE,
            generatorPrototype.prototype.prototype)))
    (header, body)
  }
}
