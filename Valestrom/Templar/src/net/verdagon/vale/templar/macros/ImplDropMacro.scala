package net.verdagon.vale.templar.macros

import net.verdagon.vale.astronomer.{FunctionA, ImmInterfaceDestructorNameS}
import net.verdagon.vale.parser.{OwnP, ReadonlyP, ReadwriteP, ShareP}
import net.verdagon.vale.scout.{CodeRuneS, CodeTypeNameS, CodeVarNameS, FunctionNameS, GeneratedBodyS, ParameterS, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS, OverrideSP}
import net.verdagon.vale.scout.rules.{CoordComponentsSR, IsInterfaceSR, IsStructSR, KindComponentsSR, LookupSR, MutabilityLiteralSL, OneOfSR, OwnershipLiteralSL, PermissionLiteralSL, RuneUsage}
import net.verdagon.vale.templar.ast.LocationInFunctionEnvironment
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, Temputs}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.FunctionEnvironment
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.templata.{Conversions, CoordTemplata, FunctionHeaderT, KindTemplata, ParameterT}
import net.verdagon.vale.templar.types.{CoordT, CoordTemplataType, FunctionTemplataType, InterfaceTT, KindTemplataType, MutabilityT, MutableT, OwnT, ReadonlyT, ReadwriteT, ShareT, StructTT, TemplateTemplataType}
import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert, vfail}

object ImplDropMacro {

  def addImplDestructor(
    mutability: MutabilityT):
  (FunctionA, IFunctionGenerator) = {
    val unevaluatedFunctionA =
      FunctionA(
        RangeS.internal(-65),
        if (mutability == MutableT) {
          FunctionNameS(CallTemplar.MUT_INTERFACE_DESTRUCTOR_NAME, CodeLocationS.internal(-18))
        } else {
          ImmInterfaceDestructorNameS(PackageCoordinate.internal)
        },
        Vector(UserFunctionS),
        TemplateTemplataType(Vector(CoordTemplataType, KindTemplataType), FunctionTemplataType),
        Vector(RuneUsage(RangeS.internal(-65002), CodeRuneS("SC")), RuneUsage(RangeS.internal(-65002), CodeRuneS("I"))),
        Map(
          CodeRuneS("SC") -> CoordTemplataType,
          CodeRuneS("SO") -> OwnershipTemplataType,
          CodeRuneS("SP") -> PermissionTemplataType,
          CodeRuneS("SK") -> KindTemplataType,
          CodeRuneS("SKM") -> MutabilityTemplataType,
          CodeRuneS("I") -> KindTemplataType,
          CodeRuneS("V") -> CoordTemplataType),
        Vector(
          ParameterS(AtomSP(RangeS.internal(-1341), Some(CaptureS(CodeVarNameS("this"))), Some(OverrideSP(RangeS.internal(-1133), RuneUsage(RangeS.internal(-1133), CodeRuneS("I")))), Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("SC"))), None))),
        Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("V"))),
        Vector(
          CoordComponentsSR(RangeS.internal(-98), RuneUsage(RangeS.internal(-64002), CodeRuneS("SC")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SO")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SP")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK"))),
          KindComponentsSR(RangeS.internal(-97), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SKM"))),
          IsStructSR(RangeS.internal(-167241), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK"))),
          OneOfSR(RangeS.internal(-167232), RuneUsage(RangeS.internal(-64002), CodeRuneS("SKM")), Array(MutabilityLiteralSL(Conversions.unevaluateMutability(mutability)))),
          OneOfSR(RangeS.internal(-167235), RuneUsage(RangeS.internal(-64002), CodeRuneS("SO")), Array(OwnershipLiteralSL(OwnP), OwnershipLiteralSL(ShareP))),
          OneOfSR(RangeS.internal(-167235), RuneUsage(RangeS.internal(-64002), CodeRuneS("SP")), Array(PermissionLiteralSL(ReadwriteP), PermissionLiteralSL(ReadonlyP))),
          IsInterfaceSR(RangeS.internal(-167243),RuneUsage(RangeS.internal(-64002), CodeRuneS("I"))),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("V")),CodeTypeNameS("void"))),
        GeneratedBodyS("implDestructorGenerator"))
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
          // There are multiple idestructor overrides for a given struct, which can
          // confuse us.
          // They all override different interfaces, but that's not factored into the
          // overload templar.
          // However, the template arguments are, and idestructor's template argument
          // is the interface we're overriding.
          val Vector(
          CoordTemplata(CoordT(_, _, overridingstructRefTFromTemplateArg @ StructTT(_))),
          KindTemplata(implementedInterfaceRef2 @ InterfaceTT(_))) =
          namedEnv.fullName.last.templateArgs

          params.map(_.tyype) match {
            case Vector(CoordT(_, _, structTT @ StructTT(_))) => {
              vassert(overridingstructRefTFromTemplateArg == structTT)
              val structDefT = temputs.lookupStruct(structTT)
              val ownership = if (structDefT.mutability == MutableT) OwnT else ShareT
              val permission = if (structDefT.mutability == MutableT) ReadwriteT else ReadonlyT
              val structType2 = CoordT(ownership, permission, structTT)
              val structDestructor =
                destructorTemplar.getCitizenDestructor(namedEnv, temputs, structType2)
              functionTemplarCore.makeImplDestructor(
                namedEnv, temputs, maybeOriginFunction1, structDefT, implementedInterfaceRef2, structDestructor)
            }
            case _ => {
              vfail("wot")
            }
          }
        }
      }
    (unevaluatedFunctionA, generator)
  }

}
