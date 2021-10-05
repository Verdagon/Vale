package net.verdagon.vale.templar.macros

import net.verdagon.vale.{IProfiler, RangeS, vassert}
import net.verdagon.vale.astronomer.{ConstructorNameS, FunctionA, StructA}
import net.verdagon.vale.scout.{CodeTypeNameS, CodeVarNameS, GeneratedBodyS, IRuneS, ParameterS, ReturnRuneS, RuneNameS, StructNameRuneS, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast.{ArgLookupTE, BlockTE, ConstructTE, FunctionT, LocationInFunctionEnvironment, ReturnTE}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.FunctionEnvironment
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.{FullNameT, IFunctionNameT}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, TemplarOptions, Temputs}
import net.verdagon.vale.templar.templata.{FunctionHeaderT, KindTemplata, ParameterT}
import net.verdagon.vale.templar.types.{CoordT, CoordTemplataType, FunctionTemplataType, ITemplataType, InterfaceTT, KindTemplataType, MutableT, OwnT, ReadonlyT, ReadwriteT, ReferenceMemberTypeT, ShareT, StructDefinitionT, StructMemberT, StructTT, TemplateTemplataType}

import scala.collection.mutable

class StructConstructorMacro(
  opts: TemplarOptions,
  profiler: IProfiler) {

  def getConstructor(structA: StructA): FunctionA = {
    profiler.newProfile("StructTemplarGetConstructor", structA.name.name, () => {
      opts.debugOut("todo: put all the members' rules up in the top of the struct")
      val params =
        structA.members.zipWithIndex.map({
          case (member, index) => {
            ParameterS(
              AtomSP(
                member.range,
                Some(CaptureS(CodeVarNameS(member.name))),
                //                Some(LocalS(CodeVarNameS(member.name), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
                None,
                Some(member.typeRune),
                None))
          }
        })

      val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
      runeToType ++= structA.runeToType

      val rules = mutable.ArrayBuffer[IRulexSR]()
      rules ++= structA.rules

      val retRune = RuneUsage(structA.name.range, ReturnRuneS())
      runeToType += (retRune.rune -> CoordTemplataType)
      if (structA.isTemplate) {
        val structNameRune = StructNameRuneS(structA.name)
        runeToType += (structNameRune -> structA.tyype)
        rules += LookupSR(structA.range, RuneUsage(structA.name.range, structNameRune), CodeTypeNameS(structA.name.name))
        rules += CallSR(structA.range, retRune, RuneUsage(structA.range, structNameRune), structA.identifyingRunes.toArray)
      } else {
        rules += LookupSR(structA.range, retRune, CodeTypeNameS(structA.name.name))
      }

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
        GeneratedBodyS("structConstructorGenerator"))
    })
  }

  def makeStructConstructor(
    temputs: Temputs,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    structDef: StructDefinitionT,
    constructorFullName: FullNameT[IFunctionNameT]):
  FunctionHeaderT = {
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
          maybeConstructorOriginFunctionA),
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

  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
      "structConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(profiler: IProfiler,
            functionTemplarCore: FunctionTemplarCore,
            structTemplar: StructTemplar,
            destructorTemplar: DestructorTemplar,
            arrayTemplar: ArrayTemplar,
            env: FunctionEnvironment,
            temputs: Temputs,
            life: LocationInFunctionEnvironment,
            callRange: RangeS,
            originFunction: Option[FunctionA],
            paramCoords: Vector[ParameterT],
            maybeRetCoord: Option[CoordT]):
          (FunctionHeaderT) = {
            val Some(CoordT(_, _, structTT @ StructTT(_))) = maybeRetCoord
            val structDefT = temputs.lookupStruct(structTT)
            makeStructConstructor(temputs, originFunction, structDefT, env.fullName)
          }
        })
  }
}
