package net.verdagon.vale.templar.function

//import net.verdagon.vale.astronomer.{AbstractAP, CallAR, CodeRuneS, CodeTypeNameS, CodeVarNameS, ComponentsAR, EqualsAR, FunctionA, FunctionNameS, GeneratedBodyS, ImmConcreteDestructorImpreciseNameS, ImmConcreteDestructorNameS, ImmDropImpreciseNameS, ImmDropNameS, ImmInterfaceDestructorImpreciseNameS, ImmInterfaceDestructorNameS, LocalS, MutabilityAR, NameSR, OrAR, OverrideAP, OwnershipAR, ParameterS, PermissionAR, RuneSR, TemplexAR, UserFunctionA}
import net.verdagon.vale.astronomer.{FunctionA, ImmConcreteDestructorImpreciseNameS, ImmConcreteDestructorNameS, ImmInterfaceDestructorImpreciseNameS, ImmInterfaceDestructorNameS}
import net.verdagon.vale.parser.{OwnP, ReadonlyP, ReadwriteP, ShareP}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, CaptureS, OverrideSP}
import net.verdagon.vale.scout.rules.{CallSR, CoordComponentsSR, EqualsSR, IsConcreteSR, IsInterfaceSR, IsStructSR, KindComponentsSR, LiteralSR, LookupSR, MutabilityLiteralSL, OneOfSR, OwnershipLiteralSL, PermissionLiteralSL, RuneUsage}
import net.verdagon.vale.scout.{CodeNameS, CodeRuneS, CodeVarNameS, FunctionNameS, GeneratedBodyS, GlobalFunctionFamilyNameS, LocalS, NotUsed, ParameterS, Used, UserFunctionS}
import net.verdagon.vale.templar.types.{CoordT, _}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.OverloadTemplar.FindFunctionFailure
import net.verdagon.vale.templar.{ast, _}
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, DiscardTE, FunctionCallTE, FunctionHeaderT, FunctionT, OverrideT, ParameterT, PrototypeT, ReferenceExpressionTE, ReturnTE, VoidLiteralTE}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.names.CodeVarNameT
import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert, vfail, vimpl}

import scala.collection.immutable.List

class DestructorTemplar(
    opts: TemplarOptions,
    structTemplar: StructTemplar,
    overloadTemplar: OverloadTemplar) {
  def getCitizenDestructor(
    env: IEnvironment,
    temputs: Temputs,
    type2: CoordT):
  (PrototypeT) = {
    type2.kind match {
      case PackTT(_, _) | StructTT(_) => { // | OrdinaryClosure2(_, _, _) | TemplatedClosure2(_, _, _) => {
        overloadTemplar.findFunction(
          env,
          temputs,
          RangeS.internal(-1663),
//          if (type2.ownership == ShareT) {
//            ImmConcreteDestructorImpreciseNameS()
//          } else {
            GlobalFunctionFamilyNameS(CallTemplar.DROP_FUNCTION_NAME),
//          },
          Vector.empty,
          Array.empty,
          Vector(ParamFilter(type2, None)),
          Vector(),
          true)
      }
      case InterfaceTT(fullName) => {
        overloadTemplar.findFunction(
          env,
          temputs,
          RangeS.internal(-1668),
//          if (type2.ownership == ShareT) {
          GlobalFunctionFamilyNameS(CallTemplar.DROP_FUNCTION_NAME),
//          } else {
//            GlobalFunctionFamilyNameS(CallTemplar.MUT_INTERFACE_DESTRUCTOR_NAME)
//          },
          Vector.empty,
          Array.empty,
          Vector(ParamFilter(type2, None)),
          Vector.empty,
          true)
      }
    }
  }

  def getArrayDestructor(
    env: IEnvironment,
    temputs: Temputs,
    type2: CoordT):
  (PrototypeT) = {
    type2.kind match {
      case StaticSizedArrayTT(_, _) | RuntimeSizedArrayTT(_) =>
    }
    overloadTemplar.findFunction(
      env,
      temputs,
      RangeS.internal(-16721),
      GlobalFunctionFamilyNameS(CallTemplar.DROP_FUNCTION_NAME),
//      if (type2.ownership == ShareT) {
//        ImmConcreteDestructorImpreciseNameS()
//      } else {
//        GlobalFunctionFamilyNameS(CallTemplar.MUT_DESTRUCTOR_NAME)
//      },
      Vector.empty,
      Array.empty,
      Vector(ParamFilter(type2, None)),
      Vector.empty,
      true)
  }

  def drop(
    fate: FunctionEnvironmentBox,
    temputs: Temputs,
    undestructedExpr2: ReferenceExpressionTE):
  (ReferenceExpressionTE) = {
    val resultExpr2 =
      undestructedExpr2.resultRegister.reference match {
        case r@CoordT(OwnT, ReadwriteT, kind) => {
          val destructorPrototype =
            kind match {
              case PackTT(_, understructTT) => {
                getCitizenDestructor(fate.snapshot, temputs, CoordT(OwnT, ReadwriteT, understructTT))
              }
              case StructTT(_) | InterfaceTT(_) => {
                getCitizenDestructor(fate.snapshot, temputs, r)
              }
              case StaticSizedArrayTT(_, _) | RuntimeSizedArrayTT(_) => {
                getArrayDestructor(fate.snapshot, temputs, r)
              }
            }
          FunctionCallTE(destructorPrototype, Vector(undestructedExpr2))
        }
        case CoordT(ConstraintT, _, _) => (DiscardTE(undestructedExpr2))
        case CoordT(WeakT, _, _) => (DiscardTE(undestructedExpr2))
        case CoordT(ShareT, ReadonlyT, _) => {
          val destroySharedCitizen =
            (temputs: Temputs, Coord: CoordT) => {
              val destructorHeader = getCitizenDestructor(fate.snapshot, temputs, Coord)
              // We just needed to ensure it's in the temputs, so that the backend can use it
              // for when reference counts drop to zero.
              // If/when we have a GC backend, we can skip generating share destructors.
              val _ = destructorHeader
              DiscardTE(undestructedExpr2)
            };
          val destroySharedArray =
            (temputs: Temputs, Coord: CoordT) => {
              val destructorHeader = getArrayDestructor(fate.snapshot, temputs, Coord)
              // We just needed to ensure it's in the temputs, so that the backend can use it
              // for when reference counts drop to zero.
              // If/when we have a GC backend, we can skip generating share destructors.
              val _ = destructorHeader
              DiscardTE(undestructedExpr2)
            };


          val unshareExpr2 =
            undestructedExpr2.resultRegister.reference.kind match {
              case NeverT() => undestructedExpr2
              case IntT(_) | StrT() | BoolT() | FloatT() | VoidT() => {
                DiscardTE(undestructedExpr2)
              }
              case as@StaticSizedArrayTT(_, _) => {
                val underarrayReference2 =
                  CoordT(
                    undestructedExpr2.resultRegister.reference.ownership,
                    undestructedExpr2.resultRegister.reference.permission,
                    as)
                destroySharedArray(temputs, underarrayReference2)
              }
              case as@RuntimeSizedArrayTT(_) => {
                val underarrayReference2 =
                  CoordT(
                    undestructedExpr2.resultRegister.reference.ownership,
                    undestructedExpr2.resultRegister.reference.permission,
                    as)
                destroySharedArray(temputs, underarrayReference2)
              }
              case OverloadSet(overloadSetEnv, name, voidStructRef) => {
                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(kind = voidStructRef)
                destroySharedCitizen(temputs, understructReference2)
              }
              case PackTT(_, understruct2) => {
                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(kind = understruct2)
                destroySharedCitizen(temputs, understructReference2)
              }
              case TupleTT(_, understruct2) => {
                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(kind = understruct2)
                destroySharedCitizen(temputs, understructReference2)
              }
              case StructTT(_) | InterfaceTT(_) => {
                destroySharedCitizen(temputs, undestructedExpr2.resultRegister.reference)
              }
            }
          unshareExpr2
        }
      }
    vassert(
      resultExpr2.resultRegister.reference == CoordT(ShareT, ReadonlyT, VoidT()) ||
        resultExpr2.resultRegister.reference == CoordT(ShareT, ReadonlyT, NeverT()))
    resultExpr2
  }

  def generateDropFunction(
    initialBodyEnv: FunctionEnvironment,
    temputs: Temputs,
    originFunction1: FunctionA,
    type2: CoordT):
  (FunctionHeaderT) = {
    val bodyEnv = FunctionEnvironmentBox(initialBodyEnv)
    val dropExpr2 = drop(bodyEnv, temputs, ArgLookupTE(0, type2))
    val header =
      ast.FunctionHeaderT(
        bodyEnv.fullName,
        Vector.empty,
        Vector(ParameterT(CodeVarNameT("x"), None, type2)),
        CoordT(ShareT, ReadonlyT, VoidT()),
        Some(originFunction1))

    val function2 = FunctionT(header, BlockTE(Templar.consecutive(Vector(dropExpr2, ReturnTE(VoidLiteralTE())))))
    temputs.declareFunctionReturnType(header.toSignature, CoordT(ShareT, ReadonlyT, VoidT()))
    temputs.addFunction(function2)
    vassert(temputs.getDeclaredSignatureOrigin(bodyEnv.fullName) == Some(originFunction1.range))
    header
  }

  def getImmInterfaceDestructorOverride(
    temputs: Temputs,
    env: IEnvironment,
    structTT: StructTT,
    implementedInterfaceRefT: InterfaceTT):
  PrototypeT = {
    vassert(Templar.getMutability(temputs, structTT) == ImmutableT)
    vassert(Templar.getMutability(temputs, implementedInterfaceRefT) == ImmutableT)

    overloadTemplar.findFunction(
      env,
      temputs,
      RangeS.internal(-1674),
      ImmInterfaceDestructorImpreciseNameS(),
      Vector.empty,
      Array.empty,
      Vector(ParamFilter(CoordT(ShareT, ReadonlyT, structTT), Some(OverrideT(implementedInterfaceRefT)))),
      Vector.empty,
      true)
  }
}
