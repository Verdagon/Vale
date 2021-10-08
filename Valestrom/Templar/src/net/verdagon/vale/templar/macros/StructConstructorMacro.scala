package net.verdagon.vale.templar.macros

import net.verdagon.vale.{IProfiler, PackageCoordinate, RangeS, vassert}
import net.verdagon.vale.astronomer.{ConstructorNameS, FunctionA, StructA}
import net.verdagon.vale.scout.{CodeNameS, CodeVarNameS, CoordTemplataType, FunctionTemplataType, GeneratedBodyS, IRuneS, ITemplataType, KindTemplataType, ParameterS, ReturnRuneS, RuneNameS, StructNameRuneS, TemplateTemplataType, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, ConstructTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment, PackageEnvironment}
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.{CitizenTemplateNameT, FullNameT, IFunctionNameT, INameT, NameTranslator, PackageTopLevelNameT}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, TemplarOptions, Temputs}
import net.verdagon.vale.templar.types.{CoordT, InterfaceTT, MutableT, OwnT, ReadonlyT, ReadwriteT, ReferenceMemberTypeT, ShareT, StructDefinitionT, StructMemberT, StructTT}

import scala.collection.mutable

class StructConstructorMacro(
  opts: TemplarOptions,
  profiler: IProfiler
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  override def onStructDefined(
    packageCoordinate: PackageCoordinate, namespace: Vector[INameT], structName: INameT, structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    val functionA = defineConstructorFunction(structA)
    Vector(
      FullNameT(packageCoordinate, namespace, NameTranslator.translateNameStep(functionA.name)) ->
        FunctionEnvEntry(functionA))
  }

  private def defineConstructorFunction(structA: StructA):
  FunctionA = {
    profiler.newProfile("StructTemplarGetConstructor", structA.name.name, () => {
      val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
      runeToType ++= structA.runeToType

      val rules = mutable.ArrayBuffer[IRulexSR]()
      rules ++= structA.rules

      val retRune = RuneUsage(structA.name.range, ReturnRuneS())
      runeToType += (retRune.rune -> CoordTemplataType)
      if (structA.isTemplate) {
        val structNameRune = StructNameRuneS(structA.name)
        runeToType += (structNameRune -> structA.tyype)
        rules += LookupSR(structA.range, RuneUsage(structA.name.range, structNameRune), CodeNameS(structA.name.name))
        rules += CallSR(structA.range, retRune, RuneUsage(structA.range, structNameRune), structA.identifyingRunes.toArray)
      } else {
        rules += LookupSR(structA.range, retRune, CodeNameS(structA.name.name))
      }

      val params =
        structA.members.map(member => {
          val capture = CaptureS(CodeVarNameS(member.name))
          ParameterS(AtomSP(member.range, Some(capture), None, Some(member.typeRune), None))
        })

      val functionA =
        FunctionA(
          structA.range,
          ConstructorNameS(structA.name),
          Vector(UserFunctionS),
          structA.tyype match {
            case KindTemplataType => FunctionTemplataType
            case TemplateTemplataType(params, KindTemplataType) => TemplateTemplataType(params, FunctionTemplataType)
          },
          structA.identifyingRunes,
          runeToType.toMap,
          params,
          Some(retRune),
          rules.toVector,
          GeneratedBodyS(generatorId))
      functionA
    })
  }

  override def generatorId: String = "structConstructorGenerator"

  override def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val Some(CoordT(_, _, structTT @ StructTT(_))) = maybeRetCoord
    val structDef = temputs.lookupStruct(structTT)

    val constructorFullName = env.fullName
    vassert(constructorFullName.last.parameters.size == structDef.members.size)
    val constructorParams =
      structDef.members.map({
        case StructMemberT(name, _, ReferenceMemberTypeT(reference)) => {
          ParameterT(name, None, reference)
        }
      })
    val constructorReturnOwnership = if (structDef.mutability == MutableT) OwnT else ShareT
    val constructorReturnPermission = if (structDef.mutability == MutableT) ReadwriteT else ReadonlyT
    val constructorReturnType = CoordT(constructorReturnOwnership, constructorReturnPermission, structDef.getRef)
    // not virtual because how could a constructor be virtual
    val constructor2 =
      FunctionT(
        FunctionHeaderT(
          constructorFullName,
          Vector.empty,
          constructorParams,
          constructorReturnType,
          originFunction),
        BlockTE(
          ReturnTE(
            ConstructTE(
              structDef.getRef,
              constructorReturnType,
              constructorParams.zipWithIndex.map({ case (p, index) => ArgLookupTE(index, p.tyype) })))))

    // we cant make the destructor here because they might have a user defined one somewhere
    temputs.declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
    temputs.addFunction(constructor2);

    vassert(
      temputs.getDeclaredSignatureOrigin(
        constructor2.header.fullName).nonEmpty)

    (constructor2.header)
  }
}
