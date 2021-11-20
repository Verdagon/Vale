package net.verdagon.vale.templar.macros.drop

import net.verdagon.vale.astronomer.{FunctionA, ImplA}
import net.verdagon.vale.parser.{LendConstraintP, MoveP, OwnP, ReadonlyP, ReadwriteP, ShareP}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS, OverrideSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.ast.{FunctionHeaderT, LocationInFunctionEnvironment, ParameterT}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment}
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.macros.{IOnImplDefinedMacro, IOnImplGeneratedMacro}
import net.verdagon.vale.templar.names.{FullNameT, FunctionTemplateNameT, INameT, NameTranslator}
import net.verdagon.vale.templar.templata.{CoordTemplata, KindTemplata}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, Temputs}
import net.verdagon.vale._
import net.verdagon.vale.templar.expression.CallTemplar

class ImplDropMacro() extends IOnImplDefinedMacro {
  override def getImplSiblingEntries(implName: FullNameT[INameT], implA: ImplA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    val dropFunctionA =
      FunctionA(
        implA.range,
        FunctionNameS(CallTemplar.VIRTUAL_DROP_FUNCTION_NAME, implA.range.begin),
        Vector(UserFunctionS),
        if (implA.isTemplate) {
          TemplateTemplataType(implA.identifyingRunes.map(_.rune).map(implA.runeToType), FunctionTemplataType)
        } else {
          FunctionTemplataType
        },
        implA.identifyingRunes,
        implA.runeToType + (ImplDropCoordRuneS() -> CoordTemplataType) + (ImplDropVoidRuneS() -> CoordTemplataType),
        Vector(
          ParameterS(
            AtomSP(
              RangeS.internal(-1340),
              Some(CaptureS(CodeVarNameS("this"))),
              Some(OverrideSP(RangeS.internal(-64002), RuneUsage(RangeS.internal(-64002), implA.interfaceKindRune.rune))),
              Some(RuneUsage(RangeS.internal(-64002), ImplDropCoordRuneS())), None))),
        Some(RuneUsage(RangeS.internal(-64002), ImplDropVoidRuneS())),
        implA.rules ++
        Vector(
          CoerceToCoordSR(
            RangeS.internal(-167213),
            RuneUsage(RangeS.internal(-167214), ImplDropCoordRuneS()),
            RuneUsage(RangeS.internal(-167215), implA.structKindRune.rune)),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), ImplDropVoidRuneS()),CodeNameS("void"))),
        CodeBodyS(
          BodySE(RangeS.internal(-167213),
            Vector(),
            BlockSE(RangeS.internal(-167213),
              Vector(LocalS(CodeVarNameS("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
              Vector(
                FunctionCallSE(RangeS.internal(-167213),
                  OutsideLoadSE(RangeS.internal(-167213),
                    Array(),
                    CallTemplar.DROP_FUNCTION_NAME,
                    None,
                    LendConstraintP(None)),
                  Vector(LocalLoadSE(RangeS.internal(-167213), CodeVarNameS("this"), MoveP))))))))
    Vector((
      implName.addStep(NameTranslator.translateFunctionNameToTemplateName(dropFunctionA.name)),
      FunctionEnvEntry(dropFunctionA)))
  }
//
//  def addImplDestructor(
//    //    mutability: MutabilityT
//  ):
//  (FunctionA, IFunctionGenerator) = {
//    val unevaluatedFunctionA =
//      FunctionA(
//        RangeS.internal(-65),
//        FunctionNameS(CallTemplar.DROP_FUNCTION_NAME, CodeLocationS.internal(-18)),
//        //        if (mutability == MutableT) {
//        //          FunctionNameS(CallTemplar.MUT_INTERFACE_DESTRUCTOR_NAME, CodeLocationS.internal(-18))
//        //        } else {
//        //          ImmInterfaceDestructorNameS(PackageCoordinate.internal)
//        //        },
//        Vector(UserFunctionS),
//        TemplateTemplataType(Vector(CoordTemplataType, KindTemplataType), FunctionTemplataType),
//        Vector(RuneUsage(RangeS.internal(-65002), ImplDropCoordRuneS()), RuneUsage(RangeS.internal(-65002), CodeRuneS("I"))),
//        Map(
//          ImplDropCoordRuneS() -> CoordTemplataType,
//          CodeRuneS("SO") -> OwnershipTemplataType,
//          CodeRuneS("SP") -> PermissionTemplataType,
//          CodeRuneS("SK") -> KindTemplataType,
//          CodeRuneS("SKM") -> MutabilityTemplataType,
//          CodeRuneS("I") -> KindTemplataType,
//          ImplDropVoidRuneS() -> CoordTemplataType),
//        Vector(
//          ParameterS(AtomSP(RangeS.internal(-1341), Some(CaptureS(CodeVarNameS("this"))), Some(OverrideSP(RangeS.internal(-1133), RuneUsage(RangeS.internal(-1133), CodeRuneS("I")))), Some(RuneUsage(RangeS.internal(-64002), ImplDropCoordRuneS())), None))),
//        Some(RuneUsage(RangeS.internal(-64002), ImplDropVoidRuneS())),
//        Vector(
//          CoordComponentsSR(RangeS.internal(-98), RuneUsage(RangeS.internal(-64002), ImplDropCoordRuneS()), RuneUsage(RangeS.internal(-64002), CodeRuneS("SO")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SP")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK"))),
//          //          KindComponentsSR(RangeS.internal(-97), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK")), RuneUsage(RangeS.internal(-64002), CodeRuneS("SKM"))),
//          IsStructSR(RangeS.internal(-167241), RuneUsage(RangeS.internal(-64002), CodeRuneS("SK"))),
//          //          OneOfSR(RangeS.internal(-167232), RuneUsage(RangeS.internal(-64002), CodeRuneS("SKM")), Array(MutabilityLiteralSL(Conversions.unevaluateMutability(mutability)))),
//          OneOfSR(RangeS.internal(-167235), RuneUsage(RangeS.internal(-64002), CodeRuneS("SO")), Array(OwnershipLiteralSL(OwnP), OwnershipLiteralSL(ShareP))),
//          OneOfSR(RangeS.internal(-167235), RuneUsage(RangeS.internal(-64002), CodeRuneS("SP")), Array(PermissionLiteralSL(ReadwriteP), PermissionLiteralSL(ReadonlyP))),
//          IsInterfaceSR(RangeS.internal(-167243), RuneUsage(RangeS.internal(-64002), CodeRuneS("I"))),
//          LookupSR(RangeS.internal(-167213), RuneUsage(RangeS.internal(-64002), ImplDropVoidRuneS()), CodeNameS("void"))),
//        GeneratedBodyS("implDestructorGenerator"))
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
//          // There are multiple idestructor overrides for a given struct, which can
//          // confuse us.
//          // They all override different interfaces, but that's not factored into the
//          // overload templar.
//          // However, the template arguments are, and idestructor's template argument
//          // is the interface we're overriding.
//          val Vector(
//          CoordTemplata(CoordT(_, _, overridingstructRefTFromTemplateArg@StructTT(_))),
//          KindTemplata(implementedInterfaceRef2@InterfaceTT(_))) =
//          namedEnv.fullName.last.templateArgs
//
//          params.map(_.tyype) match {
//            case Vector(CoordT(_, _, structTT@StructTT(_))) => {
//              vassert(overridingstructRefTFromTemplateArg == structTT)
//              val structDefT = temputs.lookupStruct(structTT)
//              val ownership = if (structDefT.mutability == MutableT) OwnT else ShareT
//              val permission = if (structDefT.mutability == MutableT) ReadwriteT else ReadonlyT
//              val structType2 = CoordT(ownership, permission, structTT)
//              val structDestructor =
//                destructorTemplar.getCitizenDestructor(namedEnv, temputs, structType2)
//              functionTemplarCore.makeImplDestructor(
//                namedEnv, temputs, maybeOriginFunction1, structDefT, implementedInterfaceRef2, structDestructor)
//            }
//            case _ => {
//              vfail("wot")
//            }
//          }
//        }
//      }
//    (unevaluatedFunctionA, generator)
//  }

}
