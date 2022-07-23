package dev.vale.typing.macros

import dev.vale.highertyping.{FunctionA, StructA}
import dev.vale.postparsing.patterns.{AtomSP, CaptureS}
import dev.vale.postparsing.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import dev.vale.postparsing._
import dev.vale.typing.{ArrayCompiler, CompilerOutputs, TemplataCompiler, TypingPassOptions, ast}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, ConstructTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment}
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, ICitizenNameT, ICitizenTemplateNameT, IFunctionNameT, IFunctionTemplateNameT, INameT, ITemplateNameT, NameTranslator, PlaceholderNameT}
import dev.vale.{Interner, Keywords, PackageCoordinate, Profiler, RangeS, StrI, vassert, vcurious, vimpl}
import dev.vale.typing.types._
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.ConstructorNameS
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.postparsing.rules.CallSR
import dev.vale.typing.ast._
import dev.vale.typing.env.PackageEnvironment
import dev.vale.typing.function.FunctionCompilerCore
import dev.vale.typing.templata.ITemplata.expectMutability
import dev.vale.typing.templata.{CoordTemplata, ITemplata, KindTemplata, MutabilityTemplata, PlaceholderTemplata}
import dev.vale.typing.types.InterfaceTT

import scala.collection.mutable

class StructConstructorMacro(
  opts: TypingPassOptions,
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  val generatorId: StrI = keywords.structConstructorGenerator

  val macroName: StrI = keywords.DeriveStructConstructor

  override def getStructChildEntries(
    macroName: StrI,
    structName: FullNameT[INameT],
    structA: StructA,
    mutability: ITemplata[MutabilityTemplataType]):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    Vector()
  }

  override def getStructSiblingEntries(macroName: StrI, structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    if (structA.members.collect({ case VariadicStructMemberS(_, _, _) => }).nonEmpty) {
      // Dont generate constructors for variadic structs, not supported yet.
      // Only one we have right now is tuple, which has its own special syntax for constructing.
      return Vector()
    }
    val functionA = defineConstructorFunction(structA)
    Vector(
      structName.copy(last = nameTranslator.translateNameStep(functionA.name)) ->
        FunctionEnvEntry(functionA))
  }

  private def defineConstructorFunction(structA: StructA):
  FunctionA = {
    Profiler.frame(() => {
      val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
      runeToType ++= structA.headerRuneToType
      runeToType ++= structA.membersRuneToType

      val rules = mutable.ArrayBuffer[IRulexSR]()
      rules ++= structA.headerRules
      rules ++= structA.memberRules

      val retRune = RuneUsage(structA.name.range, ReturnRuneS())
      runeToType += (retRune.rune -> CoordTemplataType())
      val structNameRange = structA.name.range
      if (structA.isTemplate) {
        val structNameRune = StructNameRuneS(structA.name)
        runeToType += (structNameRune -> structA.tyype)
        rules += LookupSR(structNameRange, RuneUsage(structNameRange, structNameRune), structA.name.getImpreciseName(interner))
        rules += CallSR(structNameRange, retRune, RuneUsage(structNameRange, structNameRune), structA.genericParameters.map(_.rune).toArray)
      } else {
        rules += LookupSR(structNameRange, retRune, structA.name.getImpreciseName(interner))
      }

      val params =
        structA.members.zipWithIndex.flatMap({
          case (NormalStructMemberS(range, name, variability, typeRune), index) => {
            val capture = CaptureS(interner.intern(CodeVarNameS(name)))
            Vector(ParameterS(AtomSP(range, Some(capture), None, Some(typeRune), None)))
          }
          case (VariadicStructMemberS(range, variability, typeRune), index) => {
            Vector()
          }
        })

      val functionA =
        FunctionA(
          structA.range,
          interner.intern(ConstructorNameS(structA.name)),
          Vector(),
          structA.tyype match {
            case KindTemplataType() => FunctionTemplataType()
            case TemplateTemplataType(params, KindTemplataType()) => TemplateTemplataType(params, FunctionTemplataType())
          },
          structA.genericParameters,
          runeToType.toMap,
          params,
          Some(retRune),
          rules.toVector,
          GeneratedBodyS(generatorId))
      functionA
    })
  }


  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val Some(CoordT(_, structTT @ StructTT(_))) = maybeRetCoord
    val members = StructCompiler.getMembers(interner, coutputs, structTT)

    val placeholderSubstituter =
      TemplataCompiler.getPlaceholderSubstituter(interner, structTT)

    val constructorFullName = env.fullName
    vassert(constructorFullName.last.parameters.size == members.size)
    val constructorParams =
      members.map({
        case StructMemberT(name, _, ReferenceMemberTypeT(unsubstitutedReference)) => {
          ParameterT(name, None, placeholderSubstituter.substituteForCoord(unsubstitutedReference))
        }
      })
    val constructorReturnOwnership =
      StructCompiler.getMutability(interner, coutputs, structTT) match {
        case MutabilityTemplata(MutableT) => OwnT
        case MutabilityTemplata(ImmutableT) => ShareT
        case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => OwnT
      }
    val constructorReturnType = CoordT(constructorReturnOwnership, structTT)
    // not virtual because how could a constructor be virtual
    val constructor2 =
      FunctionT(
        ast.FunctionHeaderT(
          constructorFullName,
          Vector.empty,
          constructorParams,
          constructorReturnType,
          originFunction),
        BlockTE(
          ReturnTE(
            ConstructTE(
              structTT,
              constructorReturnType,
              constructorParams.zipWithIndex.map({ case (p, index) => ArgLookupTE(index, p.tyype) })))))

    // we cant make the destructor here because they might have a user defined one somewhere
    coutputs.declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
    coutputs.addFunction(constructor2);

    (constructor2.header)
  }
}
