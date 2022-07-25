package dev.vale.typing.macros.ssa

import dev.vale.{Err, Interner, Keywords, Ok, RangeS, StrI, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.rules.{RuneParentEnvLookupSR, RuneUsage}
import dev.vale.postparsing.{CodeNameS, FunctorParamRuneNameS, FunctorPrototypeRuneNameS, FunctorReturnRuneNameS, IRuneS}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE, VoidLiteralTE}
import dev.vale.typing.env.{FunctionEnvironment, FunctionEnvironmentBox, TemplataEnvEntry}
import dev.vale.typing.{ArrayCompiler, CompileErrorExceptionT, Compiler, CompilerOutputs, CouldntEvaluateFunction, CouldntFindFunctionToCallT, OverloadResolver, RangedInternalErrorT, ast}
import dev.vale.typing.function.{DestructorCompiler, FunctionCompiler}
import dev.vale.typing.macros.IFunctionBodyMacro
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.types._
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess}
import dev.vale.typing.names.RuneNameT
import dev.vale.typing.templata.CoordTemplata

class SSAFreeMacro(
  interner: Interner,
  keywords: Keywords,
  arrayCompiler: ArrayCompiler,
  overloadResolver: OverloadResolver,
  destructorCompiler: DestructorCompiler
) extends IFunctionBodyMacro {

  val generatorId: StrI = keywords.vale_static_sized_array_free

  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction1: Option[FunctionA],
    params2: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val Vector(ssaCoord @ CoordT(ShareT, arrayTT @ StaticSizedArrayTT(_, _, _, elementCoord))) = params2.map(_.tyype)

    val ret = CoordT(ShareT, VoidT())
    val header = FunctionHeaderT(env.fullName, Vector.empty, params2, ret, originFunction1)

    coutputs.declareFunctionReturnType(header.toSignature, header.returnType)

    val dropFunction = destructorCompiler.getDropFunction(env, coutputs, callRange, elementCoord)

    val args = Vector(CoordT(ShareT, VoidT())) ++ dropFunction.prototype.paramTypes

    val newEnv =
      env.addEntries(
        interner,
        Vector(
//          interner.intern(RuneNameT(FunctorPrototypeRuneNameS())) -> TemplataEnvEntry(dropFunction),
          interner.intern(RuneNameT(FunctorParamRuneNameS(0))) -> TemplataEnvEntry(CoordTemplata(dropFunction.prototype.paramTypes.head)),
          interner.intern(RuneNameT(FunctorReturnRuneNameS())) -> TemplataEnvEntry(CoordTemplata(dropFunction.prototype.returnType))))
    val callName = interner.intern(CodeNameS(keywords.underscoresCall))
    val callRules =
      Vector(
        RuneParentEnvLookupSR(callRange, RuneUsage(callRange, FunctorParamRuneNameS(0))),
        RuneParentEnvLookupSR(callRange, RuneUsage(callRange, FunctorReturnRuneNameS())))//,
//        RuneParentEnvLookupSR(callRange, RuneUsage(callRange, FunctorPrototypeRuneNameS())))
    val callRunes = Array[IRuneS](FunctorParamRuneNameS(0), FunctorReturnRuneNameS())//, FunctorPrototypeRuneNameS())
    val consumerPrototype =
      overloadResolver.findFunction(
          newEnv, coutputs, callRange, callName, callRules, callRunes, args, Vector(), true) match {
        case Ok(prototype) => prototype.prototype
        case Err(fffr) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, fffr))
      }

    val expr =
      DestroyStaticSizedArrayIntoFunctionTE(
        ArgLookupTE(0, ssaCoord), arrayTT, VoidLiteralTE(), consumerPrototype)

    val function2 =
      FunctionT(header, BlockTE(Compiler.consecutive(Vector(expr, ReturnTE(VoidLiteralTE())))))
    coutputs.addFunction(function2)
    function2.header
  }
}
