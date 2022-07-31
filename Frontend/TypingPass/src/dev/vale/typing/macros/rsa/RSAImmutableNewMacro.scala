package dev.vale.typing.macros.rsa

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing._
import dev.vale.typing.{CompileErrorExceptionT, CompilerErrorHumanizer, CompilerOutputs, CouldntFindFunctionToCallT, OverloadResolver, ast}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, NewImmRuntimeSizedArrayTE, ParameterT, ReturnTE}
import dev.vale.typing.env.{FunctionEnvironment, TemplataLookupContext}
import dev.vale.typing.macros.IFunctionBodyMacro
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, StrI, vassertSome, vfail, vimpl}
import dev.vale.postparsing.CodeRuneS
import dev.vale.typing.ast._
import dev.vale.typing.env.TemplataLookupContext
import dev.vale.typing.templata.PrototypeTemplata
import dev.vale.typing.types._


class RSAImmutableNewMacro(interner: Interner, keywords: Keywords, overloadResolver: OverloadResolver) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.vale_runtime_sized_array_imm_new

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
      FunctionHeaderT(
        env.fullName, Vector.empty, paramCoords, maybeRetCoord.get, originFunction)
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

//    val PrototypeTemplata(generatorRange, generatorFullName, generatorReturnCoord) =
//      vassertSome(
//        env.lookupNearestWithImpreciseName(
//          interner.intern(RuneNameS(CodeRuneS(keywords.F))), Set(TemplataLookupContext)))

    val variability =
      mutability match {
        case PlaceholderTemplata(fullNameT, tyype) => vimpl()
        case MutabilityTemplata(ImmutableT) => FinalT
        case MutabilityTemplata(MutableT) => VaryingT
      }

    val arrayTT = interner.intern(RuntimeSizedArrayTT(mutability, elementType))

    val generatorArgCoord =
      paramCoords(1).tyype match {
        case CoordT(ShareT, kind) => CoordT(ShareT, kind)
        case CoordT(OwnT, kind) => CoordT(BorrowT, kind)
      }

    val generatorPrototype =
      overloadResolver.findFunction(
        env,
        coutputs,
        callRange,
        interner.intern(CodeNameS(keywords.underscoresCall)),
        Vector(),
        Array(),
        Vector(generatorArgCoord, CoordT(ShareT, IntT(32))),
        Vector(),
        false,
        true) match {
        case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, e))
        case Ok(x) => x
      }

    coutputs.addFunction(
      FunctionT(
        header,
        BlockTE(
          ReturnTE(
            NewImmRuntimeSizedArrayTE(
              arrayTT,
              ArgLookupTE(0, paramCoords(0).tyype),
              ArgLookupTE(1, paramCoords(1).tyype),
              generatorPrototype.prototype)))))
    header
  }
}
