package dev.vale.typing.macros.citizen

import dev.vale.highertyping.{FunctionA, StructA}
import dev.vale.postparsing.patterns.{AtomSP, CaptureS}
import dev.vale.postparsing.rules.{CallSR, CoerceToCoordSR, EqualsSR, LookupSR, RuneUsage}
import dev.vale.{Interner, Keywords, RangeS, StrI, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.typing.{Compiler, CompilerOutputs, OverloadResolver, TemplataCompiler, ast, env}
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, DestroyTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE, UnletTE, VoidLiteralTE}
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.DestructorCompiler
import dev.vale.typing.macros.{IFunctionBodyMacro, IOnStructDefinedMacro}
import dev.vale.typing.names.{FullNameT, INameT, NameTranslator}
import dev.vale.typing.types._
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.typing.ast._
import dev.vale.typing.macros.IOnStructDefinedMacro
import dev.vale.typing.names.INameT
import dev.vale.typing.types._
import dev.vale.typing.templata.{ITemplata, MutabilityTemplata, PlaceholderTemplata}

class StructFreeMacro(
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator,
  destructorCompiler: DestructorCompiler
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  val macroName: StrI = keywords.DeriveStructFree

  val freeGeneratorId: StrI = keywords.freeGenerator

  override def getStructSiblingEntries(structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    Vector()
  }

  override def getStructChildEntries(
    macroName: StrI, structName: FullNameT[INameT], structA: StructA, mutability: ITemplata[MutabilityTemplataType]):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    mutability match {
      case MutabilityTemplata(ImmutableT) => {
        val structNameS = structA.name
        val structType = structA.tyype
        val structIdentifyingRunes = structA.genericParameters
        val allRuneToType = structA.headerRuneToType ++ structA.membersRuneToType
        val structIdentifyingRuneToType =
          structIdentifyingRunes.map(_.rune.rune)
            .zip(structIdentifyingRunes.map(_.rune.rune).map(allRuneToType)).toMap

        val freeFunctionA =
          makeFunction(
            structNameS,
            structA.range,
            structType,
            structIdentifyingRunes.map(_.rune.rune),
            structIdentifyingRuneToType)
        val freeNameT = structName.addStep(nameTranslator.translateFunctionNameToTemplateName(freeFunctionA.name))
        Vector((freeNameT, FunctionEnvEntry(freeFunctionA)))
      }
      case MutabilityTemplata(MutableT) => Vector()
      case PlaceholderTemplata(fullNameT, tyype) => vimpl()
    }
  }

  def makeFunction(
    structNameS: ICitizenDeclarationNameS,
    structRange: RangeS,
    structType: ITemplataType,
    structIdentifyingRunes: Vector[IRuneS],
    structIdentifyingRuneToType: Map[IRuneS, ITemplataType]):
  FunctionA = {
    val nameS = interner.intern(FreeDeclarationNameS(structRange.begin))
    FunctionA(
      structRange,
      nameS,
      Vector(),
      structType match {
        case KindTemplataType() => FunctionTemplataType()
        case TemplateTemplataType(paramTypes, KindTemplataType()) => {
          TemplateTemplataType(paramTypes, FunctionTemplataType())
        }
      },
      structIdentifyingRunes
        .map(r => GenericParameterS(RuneUsage(RangeS.internal(interner, -64002), r), None)),
      structIdentifyingRuneToType ++
        (structType match {
          case KindTemplataType() => Map()
          case TemplateTemplataType(_, _) => Map(CodeRuneS(keywords.FreeStructTemplate) -> structType)
        }) ++
        Map(
          CodeRuneS(keywords.FreeStruct) -> structType,
          CodeRuneS(keywords.FreeP1) -> CoordTemplataType(),
          CodeRuneS(keywords.FreeV) -> CoordTemplataType()),
      Vector(
        ParameterS(AtomSP(RangeS.internal(interner, -1342), Some(CaptureS(interner.intern(CodeVarNameS(keywords.x)))), None, Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeP1))), None))),
      Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeV))),
      (structType match {
        case KindTemplataType() => {
          Vector(
            LookupSR(
              RangeS.internal(interner, -1672163),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStruct)),
              structNameS.getImpreciseName(interner)),
            CoerceToCoordSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeP1)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStruct))))
        }
        case TemplateTemplataType(_, KindTemplataType()) => {
          Vector(
            LookupSR(
              RangeS.internal(interner, -1672163),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStructTemplate)),
              structNameS.getImpreciseName(interner)),
            CallSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStruct)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStructTemplate)),
              structIdentifyingRunes.map(r => RuneUsage(RangeS.internal(interner, -64002), r)).toArray),
            CoerceToCoordSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeP1)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeStruct))))
        }
      }) ++
      Vector(
        LookupSR(RangeS.internal(interner, -1672164), RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeV)), interner.intern(CodeNameS(keywords.void)))),
      GeneratedBodyS(freeGeneratorId))
  }

  // Implicit drop is one made for closures, arrays, or anything else that's not explicitly
  // defined by the user.
  def makeImplicitFreeFunction(
    dropOrFreeFunctionNameS: IFunctionDeclarationNameS,
    structRange: RangeS):
  FunctionA = {
    FunctionA(
      structRange,
      dropOrFreeFunctionNameS,
      Vector(),
      FunctionTemplataType(),
      Vector(),
      Map(
        CodeRuneS(keywords.FreeP1) -> CoordTemplataType(),
        CodeRuneS(keywords.FreeV) -> CoordTemplataType()),
      Vector(
        ParameterS(
          AtomSP(
            RangeS.internal(interner, -1342),
            Some(CaptureS(interner.intern(CodeVarNameS(keywords.x)))),
            None,
            Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeP1))), None))),
      Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeV))),
      Vector(
        LookupSR(
          RangeS.internal(interner, -1672165),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeP1)),
          interner.intern(SelfNameS())),
        LookupSR(RangeS.internal(interner, -1672166), RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.FreeV)), interner.intern(CodeNameS(keywords.void)))),
      GeneratedBodyS(freeGeneratorId))
  }

  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: List[RangeS],
    originFunction1: Option[FunctionA],
    params2: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val bodyEnv = FunctionEnvironmentBox(env)

    val structTT =
      params2.head.tyype.kind match {
        case structTT @ StructTT(_) => structTT
        case other => vwat(other)
      }
    val structDef = coutputs.lookupStruct(structTT)
    val structOwnership =
      structDef.mutability match {
        case MutabilityTemplata(MutableT) => OwnT
        case MutabilityTemplata(ImmutableT) => ShareT
        case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => vimpl()
      }
    val structType = CoordT(structOwnership, structTT)

    val ret = CoordT(ShareT, VoidT())
    val header = ast.FunctionHeaderT(env.fullName, Vector.empty, params2, ret, originFunction1)

    coutputs.declareFunctionReturnType(header.toSignature, header.returnType)

    val substituter =
      TemplataCompiler.getPlaceholderSubstituter(interner, structTT.fullName)

    val memberLocalVariables =
      structDef.members.flatMap({
        case StructMemberT(name, _, ReferenceMemberTypeT(reference)) => {
          Vector(ReferenceLocalVariableT(env.fullName.addStep(name), FinalT, substituter.substituteForCoord(reference)))
        }
        case StructMemberT(_, _, AddressMemberTypeT(_)) => {
          // See Destructure2 and its handling of addressible members for why
          // we don't include these in the destination variables.
          Vector.empty
        }
      })
    val expr =
      structDef.mutability match {
        case PlaceholderTemplata(fullNameT, tyype) => vimpl()
        case MutabilityTemplata(ImmutableT) => {
          Compiler.consecutive(
            Vector(DestroyTE(ArgLookupTE(0, structType), structTT, memberLocalVariables)) ++
              memberLocalVariables.map(v => {
                destructorCompiler.drop(bodyEnv, coutputs, callRange, UnletTE(v))
              }))
        }
        case MutabilityTemplata(MutableT) => vwat() // Shouldnt be a free for mutables
      }

    val function2 = FunctionT(header, BlockTE(Compiler.consecutive(Vector(expr, ReturnTE(VoidLiteralTE())))))
    coutputs.addFunction(function2)
    function2.header
  }
}
