package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.{FunctionA, ImmDropNameS}
import net.verdagon.vale.scout.{CodeRuneS, CodeTypeNameS, CodeVarNameS, FunctionNameS, GeneratedBodyS, ParameterS, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CoordComponentsSR, KindComponentsSR, LiteralSR, LookupSR, MutabilityLiteralSL, RuneUsage}
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, DestroyTE, FunctionT, LocationInFunctionEnvironment, ReturnTE, UnletTE, VoidLiteralTE}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, Templar, Temputs}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.CodeVarNameT
import net.verdagon.vale.templar.templata.{Conversions, CoordTemplata, FunctionHeaderT, ParameterT}
import net.verdagon.vale.templar.types.{AddressMemberTypeT, ConstraintT, CoordT, CoordTemplataType, FinalT, FunctionTemplataType, MutabilityT, OwnT, ReadonlyT, ReadwriteT, ReferenceMemberTypeT, ShareT, StructMemberT, StructTT, TemplateTemplataType, VoidT}
import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert}

object StructDropMacro {

  def addDrop(
    mutability: MutabilityT):
  (FunctionA, IFunctionGenerator) = {
    // Drop is a function that:
    // - If received an owning pointer, will call the destructor
    // - If received a share pointer, will decrement it and if was last, call its destructor
    // - If received a borrow, do nothing.
    // Conceptually it's "drop the reference", as opposed to destructor which is "drop the object"
    val unevaluatedFunctionA =
    FunctionA(
      RangeS.internal(-66),
      if (mutability == MutableT) {
        FunctionNameS(CallTemplar.MUT_DROP_FUNCTION_NAME, CodeLocationS.internal(-19))
      } else {
        ImmDropNameS(PackageCoordinate.internal)
      },
      Vector(UserFunctionS),
      TemplateTemplataType(Vector(CoordTemplataType), FunctionTemplataType),
      Vector(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))),
      Map(
        CodeRuneS("T") -> CoordTemplataType,
        CodeRuneS("V") -> CoordTemplataType,
        CodeRuneS("K") -> KindTemplataType,
        CodeRuneS("KM") -> MutabilityTemplataType,
        CodeRuneS("O") -> OwnershipTemplataType,
        CodeRuneS("P") -> PermissionTemplataType),
      Vector(
        ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(CodeVarNameS("x"))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))), None))),
      Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("V"))),
      Vector(
        CoordComponentsSR(RangeS.internal(-98),
          RuneUsage(RangeS.internal(-64002), CodeRuneS("T")), RuneUsage(RangeS.internal(-64002), CodeRuneS("O")), RuneUsage(RangeS.internal(-64002), CodeRuneS("P")), RuneUsage(RangeS.internal(-64002), CodeRuneS("K"))),
        KindComponentsSR(RangeS.internal(-99),
          RuneUsage(RangeS.internal(-64002), CodeRuneS("K")), RuneUsage(RangeS.internal(-64002), CodeRuneS("KM"))),
        LiteralSR(RangeS.internal(-167251),
          RuneUsage(RangeS.internal(-64002), CodeRuneS("KM")), MutabilityLiteralSL(Conversions.unevaluateMutability(mutability))),
        LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("V")),CodeTypeNameS("void"))),
      GeneratedBodyS("dropGenerator"))
    val generator =
      new IFunctionGenerator {
        override def generate(profiler: IProfiler,
          functionTemplarCore: FunctionTemplarCore,
          structTemplar: StructTemplar,
          destructorTemplar: DestructorTemplar,
          arrayTemplar: ArrayTemplar,
          namedEnv: FunctionEnvironment,
          temputs: Temputs,
          life: LocationInFunctionEnvironment,
          callRange: RangeS,
          maybeOriginFunction1: Option[FunctionA],
          params: Vector[ParameterT],
          maybeReturnType2: Option[CoordT]):
        (FunctionHeaderT) = {
          vassert(maybeReturnType2 == Some(CoordT(ShareT, ReadonlyT, VoidT())))
          val Vector(CoordTemplata(ref2)) = namedEnv.fullName.last.templateArgs
          val Vector(ParameterT(CodeVarNameT("x"), None, paramType2)) = params
          vassert(paramType2 == ref2)
          destructorTemplar.generateDropFunction(
            namedEnv, temputs, maybeOriginFunction1.get, ref2)
        }
      }
    (unevaluatedFunctionA, generator)
  }

  def generateStructDestructor(
    namedEnv: FunctionEnvironment,
    temputs: Temputs,
    originFunction1: FunctionA,
    params2: Vector[ParameterT],
    structTT: StructTT):
  (FunctionHeaderT) = {
    val destructorFullName = namedEnv.fullName

    val bodyEnv = FunctionEnvironmentBox(namedEnv)

    val structDef = temputs.lookupStruct(structTT)
    val structOwnership = if (structDef.mutability == MutableT) OwnT else ShareT
    val structPermission = if (structDef.mutability == MutableT) ReadwriteT else ReadonlyT
    val structBorrowOwnership = if (structDef.mutability == MutableT) ConstraintT else ShareT
    val structType = CoordT(structOwnership, structPermission, structDef.getRef)

    val header =
      FunctionHeaderT(
        destructorFullName,
        Vector.empty,
        params2,
        CoordT(ShareT, ReadonlyT, VoidT()),
        Some(originFunction1));

    temputs
      .declareFunctionReturnType(header.toSignature, header.returnType)

    val structArgument = ArgLookupTE(0, structType)
    val memberLocalVariables =
      structDef.members.flatMap({
        case StructMemberT(name, variability, ReferenceMemberTypeT(reference)) => {
          Vector(ReferenceLocalVariableT(destructorFullName.addStep(name), FinalT, reference))
        }
        case StructMemberT(name, variability, AddressMemberTypeT(reference)) => {
          // See Destructure2 and its handling of addressible members for why
          // we don't include these in the destination variables.
          Vector.empty
        }
      })

    val destroyedUnletStruct = DestroyTE(structArgument, structTT, memberLocalVariables)
    val destructMemberExprs =
      memberLocalVariables.map({
        case (variable) => {
          val destructMemberExpr = drop(bodyEnv, temputs, UnletTE(variable))
          destructMemberExpr
        }
      })

    val returnVoid = ReturnTE(VoidLiteralTE())

    val function2 =
      FunctionT(
        header,
        BlockTE(
          Templar.consecutive(
            Vector(destroyedUnletStruct) ++ destructMemberExprs :+ returnVoid)))
    temputs.addFunction(function2)
    (function2.header)
  }

}
