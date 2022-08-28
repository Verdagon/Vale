package dev.vale.typing.macros

import dev.vale.highertyping.{FunctionA, StructA}
import dev.vale.postparsing.patterns.{AtomSP, CaptureS}
import dev.vale.postparsing.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import dev.vale.postparsing._
import dev.vale.typing.{ArrayCompiler, CompileErrorExceptionT, CompilerOutputs, CouldntFindFunctionToCallT, OverloadResolver, TemplataCompiler, TypingPassOptions, ast}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, ConstructTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment}
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, ICitizenNameT, ICitizenTemplateNameT, IFunctionNameT, IFunctionTemplateNameT, INameT, ITemplateNameT, NameTranslator, PlaceholderNameT}
import dev.vale.{Err, Interner, Keywords, Ok, PackageCoordinate, Profiler, RangeS, StrI, vassert, vcurious, vimpl}
import dev.vale.typing.types._
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.ConstructorNameS
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.postparsing.rules.CallSR
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast._
import dev.vale.typing.env.PackageEnvironment
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionSuccess
import dev.vale.typing.function.{DestructorCompiler, FunctionCompilerCore}
import dev.vale.typing.infer.CouldntFindFunction
import dev.vale.typing.templata.ITemplata.expectMutability
import dev.vale.typing.templata.{CoordTemplata, ITemplata, KindTemplata, MutabilityTemplata, PlaceholderTemplata}
import dev.vale.typing.types.InterfaceTT

import scala.collection.mutable

class StructConstructorMacro(
  opts: TypingPassOptions,
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator,
  destructorCompiler: DestructorCompiler,
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

  override def getStructSiblingEntries(structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    if (structA.members.collect({ case VariadicStructMemberS(_, _, _) => }).nonEmpty) {
      // Dont generate constructors for variadic structs, not supported yet.
      // Only one we have right now is tuple, which has its own special syntax for constructing.
      return Vector()
    }
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

    Vector(
      structName.copy(last = nameTranslator.translateNameStep(functionA.name)) ->
        FunctionEnvEntry(functionA))
  }


  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: List[RangeS],
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  (FunctionHeaderT, ReferenceExpressionTE) = {
    val Some(CoordT(_, structTT @ StructTT(_, _))) = maybeRetCoord
    val definition = coutputs.lookupStruct(structTT)
    val placeholderSubstituter =
      TemplataCompiler.getPlaceholderSubstituter(interner, keywords, structTT.fullName)
    val members =
      definition.members.map({
        case StructMemberT(name, _, ReferenceMemberTypeT(tyype)) => {
          (name, placeholderSubstituter.substituteForCoord(tyype))
        }
        case StructMemberT(name, variability, AddressMemberTypeT(tyype)) => {
          vcurious()
        }
      })

    val constructorFullName = env.fullName
    vassert(constructorFullName.last.parameters.size == members.size)
    val constructorParams =
      members.map({ case (name, coord) => ParameterT(name, None, coord) })
    val mutability = StructCompiler.getMutability(interner, keywords, coutputs, structTT)
    val constructorReturnOwnership =
      mutability match {
        case MutabilityTemplata(MutableT) => OwnT
        case MutabilityTemplata(ImmutableT) => ShareT
        case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => OwnT
      }
    val constructorReturnType = CoordT(constructorReturnOwnership, structTT)

    // Thisll still exist for mutable things, itll just contain a no-op.
    val freePrototype =
      destructorCompiler.getFreeFunction(coutputs, env, callRange, constructorReturnType)

    // not virtual because how could a constructor be virtual
    val header =
      ast.FunctionHeaderT(
        constructorFullName,
        Vector.empty,
        constructorParams,
        constructorReturnType,
        Some(env.templata))

    vassert(coutputs.getInstantiationBounds(freePrototype.function.prototype.fullName).nonEmpty)
    val body =
      BlockTE(
        ReturnTE(
          ConstructTE(
            structTT,
            constructorReturnType,
            constructorParams.zipWithIndex.map({ case (p, index) => ArgLookupTE(index, p.tyype) }),
            freePrototype.function.prototype)))
    (header, body)
  }
}
