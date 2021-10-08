package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.{DropNameS, FunctionA, ImmInterfaceDestructorImpreciseNameS, ImmInterfaceDestructorNameS}
import net.verdagon.vale.parser.{OwnP, ReadonlyP, ReadwriteP, ShareP}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CoordComponentsSR, IsInterfaceSR, KindComponentsSR, LiteralSR, LookupSR, MutabilityLiteralSL, OneOfSR, OwnershipLiteralSL, PermissionLiteralSL, RuneUsage}
import net.verdagon.vale.templar.ast.{FunctionHeaderT, LocationInFunctionEnvironment, ParameterT, PrototypeT}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, OverloadTemplar, Templar, Temputs}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvironment, IEnvironment}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.templata.Conversions
import net.verdagon.vale.templar.types.{CoordT, ImmutableT, InterfaceTT, MutabilityT, MutableT, ParamFilter, ReadonlyT, ShareT}
import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert, vfail, vimpl}

class InterfaceDropMacro(overloadTemplar: OverloadTemplar) {

  def addInterfaceDestructor(mutability: MutabilityT):
  (FunctionA, IFunctionGenerator) = {
    val unevaluatedFunctionA =
      FunctionA(
        RangeS.internal(-64),
        DropNameS(vimpl()),
//        if (mutability == MutableT) {
//          FunctionNameS(CallTemplar.MUT_INTERFACE_DESTRUCTOR_NAME, CodeLocationS.internal(-17))
//        } else {
//          ImmInterfaceDestructorNameS(PackageCoordinate.internal)
//        },
        Vector(UserFunctionS),
        TemplateTemplataType(Vector(CoordTemplataType), FunctionTemplataType),
        //        Set(CodeRuneS("V")),
        Vector(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))),
        //        Set(CodeRuneS("T"), CodeRuneS("XX"), CodeRuneS("V")),
        Map(
          CodeRuneS("T") -> CoordTemplataType,
          CodeRuneS("V") -> CoordTemplataType,
          CodeRuneS("KM") -> MutabilityTemplataType,
          CodeRuneS("O") -> OwnershipTemplataType,
          CodeRuneS("P") -> PermissionTemplataType,
          CodeRuneS("K") -> KindTemplataType),
        Vector(
          ParameterS(AtomSP(RangeS.internal(-1340), Some(CaptureS(CodeVarNameS("this"))), Some(AbstractSP), Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))), None))),
        Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("V"))),
        Vector(
          KindComponentsSR(RangeS.internal(-95), RuneUsage(RangeS.internal(-64002), CodeRuneS("K")), RuneUsage(RangeS.internal(-64002), CodeRuneS("KM"))),
          LiteralSR(RangeS.internal(-167216),RuneUsage(RangeS.internal(-64002), CodeRuneS("KM")),MutabilityLiteralSL(Conversions.unevaluateMutability(mutability))),
          CoordComponentsSR(
            RangeS.internal(-96),
            RuneUsage(RangeS.internal(-64002), CodeRuneS("T")),
            RuneUsage(RangeS.internal(-64002), CodeRuneS("O")),
            RuneUsage(RangeS.internal(-64002), CodeRuneS("P")),
            RuneUsage(RangeS.internal(-64002), CodeRuneS("K"))),
          OneOfSR(RangeS.internal(-167219), RuneUsage(RangeS.internal(-64002), CodeRuneS("O")), Array(OwnershipLiteralSL(OwnP), OwnershipLiteralSL(ShareP))),
          OneOfSR(RangeS.internal(-167222), RuneUsage(RangeS.internal(-64002), CodeRuneS("P")), Array(PermissionLiteralSL(ReadwriteP), PermissionLiteralSL(ReadonlyP))),
          IsInterfaceSR(RangeS.internal(-167225), RuneUsage(RangeS.internal(-64002), CodeRuneS("K"))),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("V")),CodeNameS("void"))),
        GeneratedBodyS("interfaceDestructorGenerator"))
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
          // Even though below we treat packs, closures, and structs the same, they're
          // still disambiguated by the template arguments.
          val Some(returnType2) = maybeReturnType2
          params.map(_.tyype) match {
            case Vector(CoordT(_, _, InterfaceTT(_))) => {
              functionTemplarCore.makeInterfaceFunction(
                namedEnv,
                temputs,
                maybeOriginFunction1,
                params,
                returnType2)
            }
            case _ => {
              vfail("wot")
            }
          }
        }
      }
    (unevaluatedFunctionA, generator)
  }

  def getImmInterfaceDestructor(
    temputs: Temputs,
    env: IEnvironment,
    interfaceTT: InterfaceTT):
  PrototypeT = {
    vassert(Templar.getMutability(temputs, interfaceTT) == ImmutableT)

    val prototype =
      overloadTemplar.scoutExpectedFunctionForPrototype(
        env,
        temputs,
        RangeS.internal(-1677),
        ImmInterfaceDestructorImpreciseNameS(),
        Vector.empty,
        Array.empty,
        Vector(ParamFilter(CoordT(ShareT, ReadonlyT, interfaceTT), None)),
        Vector.empty,
        true)
    prototype
  }

}
