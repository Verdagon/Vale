package dev.vale.typing.macros.citizen

import dev.vale.highertyping.{FunctionA, StructA}
import dev.vale.postparsing.patterns.{AtomSP, CaptureS}
import dev.vale.postparsing.rules.{CallSR, CoerceToCoordSR, CoordComponentsSR, EqualsSR, LookupSR, RuneUsage}
import dev.vale.{Interner, Keywords, RangeS, StrI, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, DestroyTE, DiscardTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, ParameterT, ReturnTE, UnletTE, VoidLiteralTE}
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import dev.vale.typing.{Compiler, CompilerOutputs, ast, env}
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
import dev.vale.typing.OverloadResolver
import dev.vale.typing.templata.{ITemplata, MutabilityTemplata, PlaceholderTemplata}

class StructDropMacro(
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator,
  destructorCompiler: DestructorCompiler
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  val macroName: StrI = keywords.DeriveStructDrop

  val dropGeneratorId: StrI = keywords.dropGenerator

  override def getStructSiblingEntries(
    macroName: StrI, structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    Vector()
  }

  override def getStructChildEntries(
    macroName: StrI, structName: FullNameT[INameT], structA: StructA, mutability: ITemplata[MutabilityTemplataType]):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    val structNameS = structA.name
    val structType = structA.tyype
    val genericParams = structA.genericParameters
    val allRuneToType = structA.headerRuneToType ++ structA.membersRuneToType
    val structIdentifyingRuneToType =
      genericParams.map(_.rune.rune)
        .zip(genericParams.map(_.rune.rune).map(allRuneToType)).toMap

    val dropFunctionA =
      makeFunction(
        true,
        structNameS,
        structA.range,
        structType,
        genericParams.map(_.rune.rune),
        structIdentifyingRuneToType)
    val dropNameT = structName.addStep(nameTranslator.translateFunctionNameToTemplateName(dropFunctionA.name))
    Vector((dropNameT, FunctionEnvEntry(dropFunctionA)))
  }

  def makeFunction(
    isDrop: Boolean, // If false, generate the free() function
    structNameS: ICitizenDeclarationNameS,
    structRange: RangeS,
    structType: ITemplataType,
    structGenericParams: Vector[IRuneS],
    structIdentifyingRuneToType: Map[IRuneS, ITemplataType]):
  FunctionA = {
    val nameS =
      if (isDrop) {
        interner.intern(FunctionNameS(keywords.drop, structRange.begin))
      } else {
        interner.intern(FreeDeclarationNameS(structRange.begin))
      }
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
      structGenericParams
        .map(p => GenericParameterS(RuneUsage(RangeS.internal(interner, -64002), p), None)),
      structIdentifyingRuneToType ++
        (structType match {
          case KindTemplataType() => Map()
          case TemplateTemplataType(_, _) => Map(CodeRuneS(keywords.DropStructTemplate) -> structType)
        }) ++
        Map(
          CodeRuneS(keywords.DropStruct) -> KindTemplataType(),
          CodeRuneS(keywords.DropP1) -> CoordTemplataType(),
          CodeRuneS(keywords.DropV) -> CoordTemplataType()),
      Vector(
        ParameterS(AtomSP(RangeS.internal(interner, -1342), Some(CaptureS(interner.intern(CodeVarNameS(keywords.x)))), None, Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1))), None))),
      Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV))),
    (structType match {
        case KindTemplataType() => {
          Vector(
            LookupSR(
              RangeS.internal(interner, -1672159),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStruct)),
              structNameS.getImpreciseName(interner)),
            CoerceToCoordSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStruct))))
        }
        case TemplateTemplataType(_, KindTemplataType()) => {
          Vector(
            LookupSR(
              RangeS.internal(interner, -1672159),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStructTemplate)),
              structNameS.getImpreciseName(interner)),
            CallSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStruct)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStructTemplate)),
              structGenericParams.map(r => RuneUsage(RangeS.internal(interner, -64002), r)).toArray),
            CoerceToCoordSR(
              RangeS.internal(interner, -167215),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1)),
              RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropStruct))))
        }
      }) ++
      Vector(
        LookupSR(RangeS.internal(interner, -1672160), RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV)), interner.intern(CodeNameS(keywords.void)))),
      GeneratedBodyS(dropGeneratorId))
  }

  // Implicit drop is one made for closures, arrays, or anything else that's not explicitly
  // defined by the user.
  def makeImplicitDropFunction(
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
        CodeRuneS(keywords.DropP1) -> CoordTemplataType(),
        CodeRuneS(keywords.DropV) -> CoordTemplataType()),
      Vector(
        ParameterS(AtomSP(RangeS.internal(interner, -1342), Some(CaptureS(interner.intern(CodeVarNameS(keywords.x)))), None, Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1))), None))),
      Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV))),
      Vector(
        LookupSR(
          RangeS.internal(interner, -1672161),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1)),
          interner.intern(SelfNameS())),
        LookupSR(RangeS.internal(interner, -1672162), RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV)), interner.intern(CodeNameS(keywords.void)))),
      GeneratedBodyS(dropGeneratorId))
  }

  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
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

    val memberLocalVariables =
      structDef.members.flatMap({
        case StructMemberT(name, _, ReferenceMemberTypeT(reference)) => {
          Vector(ReferenceLocalVariableT(env.fullName.addStep(name), FinalT, reference))
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
        case MutabilityTemplata(ImmutableT) => DiscardTE(ArgLookupTE(0, structType))
        case MutabilityTemplata(MutableT) => {
          Compiler.consecutive(
            Vector(DestroyTE(ArgLookupTE(0, structType), structTT, memberLocalVariables)) ++
              memberLocalVariables.map(v => {
                destructorCompiler.drop(
                  bodyEnv,
                  coutputs,
                  originFunction1.map(_.range).getOrElse(callRange),
                  UnletTE(v))
              }))
        }
      }

    val function2 = FunctionT(header, BlockTE(Compiler.consecutive(Vector(expr, ReturnTE(VoidLiteralTE())))))
    coutputs.addFunction(function2)
    function2.header
  }
}
