package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.{FunctionA, ImmConcreteDestructorNameS, ImmDropImpreciseNameS}
import net.verdagon.vale.parser.{OwnP, ReadonlyP, ReadwriteP, ShareP}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvironment, IEnvironment}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.templata.{Conversions, FunctionHeaderT, ParameterT, PrototypeT}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, OverloadTemplar, Templar, TemplarOptions, Temputs, ast}
import net.verdagon.vale._
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, DestroyRuntimeSizedArrayTE, DestroyStaticSizedArrayIntoFunctionTE, FunctionCallTE, FunctionT, LocationInFunctionEnvironment, ReturnTE, StructToInterfaceUpcastTE, VoidLiteralTE}
import net.verdagon.vale.templar.names.CodeVarNameT

class ArrayDestructorMacro(
    opts: TemplarOptions,
    structTemplar: StructTemplar,
    overloadTemplar: OverloadTemplar) {

  def generateStaticSizedArrayDestructor(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    maybeOriginFunction1: Option[FunctionA],
    sequenceRefType2: CoordT,
    sequence: StaticSizedArrayTT):
  (FunctionHeaderT) = {
    opts.debugOut("turn this into just a regular destructor template function? dont see why its special.")

    val arrayOwnership = if (sequence.array.mutability == MutableT) OwnT else ShareT
    val arrayPermission = if (sequence.array.mutability == MutableT) ReadwriteT else ReadonlyT
    val arrayBorrowOwnership = if (sequence.array.mutability == MutableT) ConstraintT else ShareT
    val arrayRefType = CoordT(arrayOwnership, arrayPermission, sequence)

    val elementDropFunctionPrototype = getDropFunction(env, temputs, sequence.array.elementType)

    val (ifunction1InterfaceRef, elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype) =
      structTemplar.prototypeToAnonymousIFunctionSubstruct(
        env, temputs, life, RangeS.internal(-1203), elementDropFunctionPrototype)

    val ifunctionExpression =
      StructToInterfaceUpcastTE(
        FunctionCallTE(constructorPrototype, Vector.empty),
        ifunction1InterfaceRef)


    val consumerMethod2 =
      overloadTemplar.scoutExpectedFunctionForPrototype(
        env, temputs, RangeS.internal(-108),
        GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME),
        Vector.empty,
        Array.empty,
        Vector(ParamFilter(ifunctionExpression.resultRegister.reference, None), ParamFilter(sequence.array.elementType, None)),
        Vector.empty, true)

    val function2 =
      FunctionT(
        FunctionHeaderT(
          env.fullName,
          Vector.empty,
          Vector(ParameterT(CodeVarNameT("this"), None, arrayRefType)),
          CoordT(ShareT, ReadonlyT, VoidT()),
          maybeOriginFunction1),
        BlockTE(
          Templar.consecutive(
            Vector(
              DestroyStaticSizedArrayIntoFunctionTE(
                ArgLookupTE(0, arrayRefType),
                sequence,
                ifunctionExpression,
                consumerMethod2),
              ReturnTE(VoidLiteralTE())))))

    temputs.declareFunctionReturnType(function2.header.toSignature, function2.header.returnType)
    temputs.addFunction(function2)
    function2.header
  }

  def generateRuntimeSizedArrayDestructor(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    maybeOriginFunction1: Option[FunctionA],
    arrayRefType2: CoordT,
    array: RuntimeSizedArrayTT):
  (FunctionHeaderT) = {
    val arrayOwnership = if (array.array.mutability == MutableT) OwnT else ShareT
    val arrayBorrowOwnership = if (array.array.mutability == MutableT) ConstraintT else ShareT

    val elementDropFunctionPrototype = getDropFunction(env, temputs, array.array.elementType)

    val (ifunction1InterfaceRef, elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype) =
      structTemplar.prototypeToAnonymousIFunctionSubstruct(env, temputs, life, RangeS.internal(-1879), elementDropFunctionPrototype)

    val ifunctionExpression =
      StructToInterfaceUpcastTE(
        FunctionCallTE(constructorPrototype, Vector.empty),
        ifunction1InterfaceRef)

    val range = RangeS.internal(-108)
    val consumerMethod2 =
      overloadTemplar.scoutExpectedFunctionForPrototype(
        env, temputs, range,
        GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME),
        Vector.empty,
        Array.empty,
        Vector(ParamFilter(ifunctionExpression.resultRegister.reference, None), ParamFilter(array.array.elementType, None)),
        Vector.empty, true)

    val function2 =
      ast.FunctionT(
        FunctionHeaderT(
          env.fullName,
          Vector.empty,
          Vector(ParameterT(CodeVarNameT("this"), None, arrayRefType2)),
          CoordT(ShareT, ReadonlyT, VoidT()),
          maybeOriginFunction1),
        BlockTE(
          Templar.consecutive(
            Vector(
              DestroyRuntimeSizedArrayTE(
                ArgLookupTE(0, arrayRefType2),
                array,
                ifunctionExpression,
                consumerMethod2),
              ReturnTE(VoidLiteralTE())))))

    temputs.declareFunctionReturnType(function2.header.toSignature, function2.header.returnType)
    temputs.addFunction(function2)
    (function2.header)
  }

  // "Drop" is a general term that encompasses:
  // - Destruct. This means we take an owning reference and call its destructor.
  // - Unshare. This means we take a shared reference, and if it's the last one, unshare anything
  //   it's pointing at and deallocate.
  // - Unborrow. This is a no op.
  // This is quite useful for handing into array consumers.
  private def getDropFunction(env: IEnvironment, temputs: Temputs, type2: CoordT): PrototypeT = {
    overloadTemplar.scoutExpectedFunctionForPrototype(
      env,
      temputs,
      RangeS.internal(-1676),
      if (type2.ownership == ShareT) {
        ImmDropImpreciseNameS()
      } else {
        GlobalFunctionFamilyNameS(CallTemplar.MUT_DROP_FUNCTION_NAME)
      },
      Vector.empty,
      Array.empty,
      Vector(ParamFilter(type2, None)),
      Vector.empty,
      true)
  }

}
