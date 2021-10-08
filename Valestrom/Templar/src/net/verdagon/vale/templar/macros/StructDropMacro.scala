package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.{DropNameS, FunctionA, StructA}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, CoordComponentsSR, EqualsSR, KindComponentsSR, LiteralSR, LookupSR, MutabilityLiteralSL, RuneUsage}
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, DestroyTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE, UnletTE, VoidLiteralTE}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, Templar, Temputs}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.{CodeVarNameT, FullNameT, INameT, NameTranslator}
import net.verdagon.vale.templar.templata.{Conversions, CoordTemplata}
import net.verdagon.vale.templar.types.{AddressMemberTypeT, ConstraintT, CoordT, FinalT, ImmutableT, MutabilityT, MutableT, OwnT, ReadonlyT, ReadwriteT, ReferenceMemberTypeT, ShareT, StructMemberT, StructTT, VoidT}
import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert, vimpl}

class StructDropMacro(
  destructorTemplar: DestructorTemplar
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  override def generatorId: String = "dropGenerator"

  override def onStructDefined(
    packageCoordinate: PackageCoordinate, namespace: Vector[INameT], structName: INameT, structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    val functionA =
      FunctionA(
        RangeS.internal(-66),
//        if (mutability == MutableT) {
//          FunctionNameS(CallTemplar.MUT_DROP_FUNCTION_NAME, CodeLocationS.internal(-19))
//        } else {
          DropNameS(PackageCoordinate.internal),
  //      },
        Vector(UserFunctionS),
        structA.tyype match {
          case KindTemplataType => FunctionTemplataType
          case TemplateTemplataType(paramTypes, KindTemplataType) => {
            TemplateTemplataType(paramTypes, FunctionTemplataType)
          }
        },
        structA.identifyingRunes,
        structA.identifyingRunes.map(_.rune)
          .zip(structA.identifyingRunes.map(_.rune).map(structA.runeToType)).toMap ++
        Map(
          CodeRuneS("__S") -> structA.tyype,
          CodeRuneS("__P1") -> CoordTemplataType,
          CodeRuneS("__V") -> CoordTemplataType,
//          CodeRuneS("K") -> KindTemplataType,
//          CodeRuneS("KM") -> MutabilityTemplataType,
//          CodeRuneS("O") -> OwnershipTemplataType,
//          CodeRuneS("P") -> PermissionTemplataType
          ),
        Vector(
          ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(CodeVarNameS("x"))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("__P1"))), None))),
        Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("__V"))),
        Vector(
          structA.tyype match {
            case KindTemplataType => {
              EqualsSR(
                RangeS.internal(-167215),
                RuneUsage(RangeS.internal(-64002), CodeRuneS("__P1")),
                RuneUsage(RangeS.internal(-64002), CodeRuneS("__S")))
            }
            case TemplateTemplataType(_, KindTemplataType) => {
              CallSR(
                RangeS.internal(-167215),
                RuneUsage(RangeS.internal(-64002), CodeRuneS("__P1")),
                RuneUsage(RangeS.internal(-64002), CodeRuneS("__S")),
                structA.identifyingRunes.map(_.rune).map(r => RuneUsage(RangeS.internal(-64002), r)).toArray)
            }
          },
//          CoordComponentsSR(RangeS.internal(-98),
//            RuneUsage(RangeS.internal(-64002), CodeRuneS("T")), RuneUsage(RangeS.internal(-64002), CodeRuneS("O")), RuneUsage(RangeS.internal(-64002), CodeRuneS("P")), RuneUsage(RangeS.internal(-64002), CodeRuneS("K"))),
//          KindComponentsSR(RangeS.internal(-99),
//            RuneUsage(RangeS.internal(-64002), CodeRuneS("K")), RuneUsage(RangeS.internal(-64002), CodeRuneS("KM"))),
//          LiteralSR(RangeS.internal(-167251),
//            RuneUsage(RangeS.internal(-64002), CodeRuneS("KM")), MutabilityLiteralSL(Conversions.unevaluateMutability(mutability))),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("__S")),CodeNameS(structA.name.name)),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("__V")),CodeNameS("void"))),
        GeneratedBodyS(generatorId))

    val fullName =
      FullNameT(packageCoordinate, namespace, NameTranslator.translateFunctionNameToTemplateName(functionA.name))
    Vector((fullName, FunctionEnvEntry(functionA)))
  }

//  def addDrop(
//    mutability: MutabilityT):
//  (FunctionA, IFunctionGenerator) = {
//    // Drop is a function that:
//    // - If received an owning pointer, will call the destructor
//    // - If received a share pointer, will decrement it and if was last, call its destructor
//    // - If received a borrow, do nothing.
//    // Conceptually it's "drop the reference", as opposed to destructor which is "drop the object"
//
//    val generator =
//      new IFunctionGenerator {
//        override def generate(profiler: IProfiler,
//          functionTemplarCore: FunctionTemplarCore,
//          structTemplar: StructTemplar,
//          destructorTemplar: DestructorTemplar,
//          arrayTemplar: ArrayTemplar,
//          namedEnv: FunctionEnvironment,
//          temputs: Temputs,
//          life: LocationInFunctionEnvironment,
//          callRange: RangeS,
//          maybeOriginFunction1: Option[FunctionA],
//          params: Vector[ParameterT],
//          maybeReturnType2: Option[CoordT]):
//        (FunctionHeaderT) = {
//          vassert(maybeReturnType2 == Some(CoordT(ShareT, ReadonlyT, VoidT())))
//          val Vector(CoordTemplata(ref2)) = namedEnv.fullName.last.templateArgs
//          val Vector(ParameterT(CodeVarNameT("x"), None, paramType2)) = params
//          vassert(paramType2 == ref2)
//          destructorTemplar.generateDropFunction(
//            namedEnv, temputs, maybeOriginFunction1.get, ref2)
//        }
//      }
//    (unevaluatedFunctionA, generator)
//  }

  override def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction1: Option[FunctionA],
    params2: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val destructorFullName = env.fullName

    val bodyEnv = FunctionEnvironmentBox(env)

    val structTT @ StructTT(_) = params2.head.tyype.kind
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
        originFunction1);

    temputs
      .declareFunctionReturnType(header.toSignature, header.returnType)

    val body =
      structDef.mutability match {
        case ImmutableT => {
          BlockTE(ReturnTE(VoidLiteralTE()))
        }
        case MutableT => {
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
                val destructMemberExpr = destructorTemplar.drop(bodyEnv, temputs, UnletTE(variable))
                destructMemberExpr
              }
            })

          val returnVoid = ReturnTE(VoidLiteralTE())
          BlockTE(
            Templar.consecutive(
              Vector(destroyedUnletStruct) ++ destructMemberExprs :+ returnVoid))
        }
      }
    val function2 =
      FunctionT(
        header,
        body)
    temputs.addFunction(function2)
    (function2.header)
  }
}
