package dev.vale.typing.macros.citizen

import dev.vale.highertyping._
import dev.vale.postparsing.patterns.{AbstractSP, AtomSP, CaptureS}
import dev.vale.postparsing.rules._
import dev.vale._
import dev.vale.postparsing._
import dev.vale.typing.ast.{ArgLookupTE, BlockTE, DestroyTE, DiscardTE, FunctionDefinitionT, FunctionHeaderT, LocationInFunctionEnvironment, ParameterT, ReturnTE, UnletTE, VoidLiteralTE}
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import dev.vale.typing.{Compiler, CompilerOutputs, InheritBoundsFromTypeItself, OverloadResolver, TemplataCompiler, ast, env}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.DestructorCompiler
import dev.vale.typing.macros.{IFunctionBodyMacro, IOnStructDefinedMacro}
import dev.vale.typing.names.{INameT, IdT, NameTranslator}
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.macros.IOnStructDefinedMacro
import dev.vale.typing.names.INameT
import dev.vale.typing.types._
import dev.vale.typing.templata.{ITemplata, MutabilityTemplata, PlaceholderTemplata}

import scala.collection.mutable

class StructDropMacro(
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator,
  destructorCompiler: DestructorCompiler
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  val macroName: StrI = keywords.DeriveStructDrop

  val dropGeneratorId: StrI = keywords.dropGenerator

  override def getStructSiblingEntries(
    structName: IdT[INameT], structA: StructA):
  Vector[(IdT[INameT], FunctionEnvEntry)] = {
    def range(n: Int) = RangeS.internal(interner, n)
    def use(n: Int, rune: IRuneS) = RuneUsage(range(n), rune)

    val rules = new Accumulator[IRulexSR]()
    // Use the same rules as the original struct, see MDSFONARFO.
    structA.headerRules.foreach(r => rules.add(r))
    val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
    // Use the same runes as the original struct, see MDSFONARFO.
    structA.headerRuneToType.foreach(runeToType += _)

    val defaultRegionRune = structA.regionRune

    val voidKindRune = MacroVoidKindRuneS()
    runeToType.put(voidKindRune, KindTemplataType())
    rules.add(
      MaybeCoercingLookupSR(
        range(-1672147), use(-64002, voidKindRune), use(-64002, defaultRegionRune), interner.intern(CodeNameS(keywords.void))))
    val voidCoordRune = MacroVoidCoordRuneS()
    runeToType.put(voidCoordRune, CoordTemplataType())
    rules.add(CoerceToCoordSR(range(-1672147),use(-64002, voidCoordRune),use(-64002, defaultRegionRune),use(-64002, voidKindRune)))

    val selfKindTemplateRune = SelfKindTemplateRuneS()
    runeToType += (selfKindTemplateRune -> structA.tyype)
    rules.add(
      MaybeCoercingLookupSR(
        structA.name.range,
        RuneUsage(structA.name.range, selfKindTemplateRune),
        use(-64002, defaultRegionRune),
        structA.name.getImpreciseName(interner)))

    val selfKindRune = SelfKindRuneS()
    runeToType += (selfKindRune -> KindTemplataType())
    rules.add(
      MaybeCoercingCallSR(
        structA.name.range,
        use(-64002, selfKindRune),
        use(-64002, defaultRegionRune),
        RuneUsage(structA.name.range, selfKindTemplateRune),
        structA.genericParameters.map(_.rune).toVector))

    val selfCoordRune = SelfCoordRuneS()
    runeToType += (selfCoordRune -> CoordTemplataType())
    rules.add(
      CoerceToCoordSR(
        structA.name.range,
        RuneUsage(structA.name.range, selfCoordRune),
        RuneUsage(structA.name.range, defaultRegionRune),
        RuneUsage(structA.name.range, selfKindRune)))


    // Use the same generic parameters as the struct
    val functionGenericParameters = structA.genericParameters

    val functionTemplataType =
      TemplateTemplataType(
        functionGenericParameters.map(_.rune.rune).map(runeToType),
        FunctionTemplataType())

    val nameS = interner.intern(FunctionNameS(keywords.drop, structA.range.begin))
    val dropFunctionA =
      FunctionA(
        structA.range,
        nameS,
        Vector(),
        functionTemplataType,
        functionGenericParameters,
        runeToType.toMap,
        Vector(
          ParameterS(
            AtomSP(
              range(-1340),
              Some(CaptureS(interner.intern(CodeVarNameS(keywords.thiss)))),
              None,
              Some(use(-64002, selfCoordRune)), None))),
        Some(use(-64002, voidCoordRune)),
        defaultRegionRune,
        rules.buildArray().toVector,
        GeneratedBodyS(dropGeneratorId))

    val dropNameT = structName.addStep(nameTranslator.translateGenericFunctionName(dropFunctionA.name))
    Vector((dropNameT, FunctionEnvEntry(dropFunctionA)))
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
      TemplateTemplataType(Vector(), FunctionTemplataType()),
      Vector(),
      Map(
        CodeRuneS(keywords.DropP1) -> CoordTemplataType(),
        CodeRuneS(keywords.DropR) -> RegionTemplataType(),
        CodeRuneS(keywords.DropP1K) -> KindTemplataType(),
        CodeRuneS(keywords.DropVK) -> KindTemplataType(),
        CodeRuneS(keywords.DropV) -> CoordTemplataType()),
      Vector(
        ParameterS(
          AtomSP(
            RangeS.internal(interner, -1342),
            Some(CaptureS(interner.intern(CodeVarNameS(keywords.x)))),
            None,
            Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1))), None))),
      Some(RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV))),
      CodeRuneS(keywords.DropR),
      Vector(
        MaybeCoercingLookupSR(
          RangeS.internal(interner, -1672161),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1K)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropR)),
          interner.intern(SelfNameS())),
        MaybeCoercingLookupSR(
          RangeS.internal(interner, -1672162),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropVK)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropR)),
          interner.intern(CodeNameS(keywords.void))),
        CoerceToCoordSR(
          RangeS.internal(interner, -1672162),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropV)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropR)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropVK))),
        CoerceToCoordSR(
          RangeS.internal(interner, -1672162),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropR)),
          RuneUsage(RangeS.internal(interner, -64002), CodeRuneS(keywords.DropP1K)))),
      GeneratedBodyS(dropGeneratorId))
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
  (FunctionHeaderT, ReferenceExpressionTE) = {
    val bodyEnv = FunctionEnvironmentBox(env)

    val structType = params2.head.tyype
    val structTT =
      params2.head.tyype.kind match {
        case structTT @ StructTT(_) => structTT
        case other => vwat(other)
      }
    val structDef = coutputs.lookupStruct(structTT.id)
//    val structOwnership =
//      structDef.mutability match {
//        case MutabilityTemplata(MutableT) => OwnT
//        case MutabilityTemplata(ImmutableT) => ShareT
//        case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => OwnT
//      }
//    val structType = CoordT(structOwnership, structTT)
    structType match {
      case CoordT(structOwnership, _, structTT) =>
      case _ => vwat()
    }

    val ret = vassertSome(maybeRetCoord)
    val header =
      ast.FunctionHeaderT(
        env.id,
        Vector.empty,
//        Vector(RegionT(env.defaultRegion.localName, true)),
        params2,
        ret,
        Some(env.templata))

    coutputs.declareFunctionReturnType(header.toSignature, header.returnType)

    val body =
      BlockTE(
        Compiler.consecutive(
          Vector(
            structDef.mutability match {
              case MutabilityTemplata(ImmutableT) => DiscardTE(ArgLookupTE(0, structType))
              case MutabilityTemplata(MutableT) | PlaceholderTemplata(_, _) => {
                val memberLocalVariables =
                  structDef.members.flatMap({
                    case NormalStructMemberT(name, _, ReferenceMemberTypeT(unsubstitutedReference)) => {
                      val substituter =
                        TemplataCompiler.getPlaceholderSubstituter(
                          interner, keywords, structTT.id,
//                          Vector((structDef.defaultRegion, env.defaultRegion)),
                          // We received an instance of this type, so we can use the bounds from it.
                          InheritBoundsFromTypeItself)
                      val reference = substituter.substituteForCoord(coutputs, unsubstitutedReference)
                      Vector(ReferenceLocalVariableT(name, FinalT, reference))
                    }
                    case NormalStructMemberT(_, _, AddressMemberTypeT(_)) => {
                      // See Destructure2 and its handling of addressible members for why
                      // we don't include these in the destination variables.
                      Vector.empty
                    }
                    case VariadicStructMemberT(name, tyype) => vimpl()
                  })

                Compiler.consecutive(
                  Vector(DestroyTE(ArgLookupTE(0, structType), structTT, memberLocalVariables)) ++
                    memberLocalVariables.map(v => {
                      destructorCompiler.drop(
                        bodyEnv,
                        coutputs,
                        originFunction1.map(_.range).toList ++ callRange,
                        env.defaultRegion,
                        UnletTE(v))
                    }))
              }
            },
            ReturnTE(VoidLiteralTE(ret.region)))))
    (header, body)
  }
}
