package net.verdagon.vale.templar.macros.citizen

import net.verdagon.vale.astronomer.{FunctionA, StructA}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, EqualsSR, LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast._
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, ReferenceLocalVariableT}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.DestructorTemplar
import net.verdagon.vale.templar.macros.{IFunctionBodyMacro, IOnStructDefinedMacro}
import net.verdagon.vale.templar.names.{FullNameT, INameT, NameTranslator}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.{OverloadTemplar, Templar, Temputs}
import net.verdagon.vale.{Interner, RangeS, vwat}

class StructFreeMacro(
  interner: Interner,
  nameTranslator: NameTranslator,
  destructorTemplar: DestructorTemplar
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  val macroName: String = "DeriveStructFree"

  val freeGeneratorId: String = "freeGenerator"

  override def getStructSiblingEntries(macroName: String, structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    Vector()
  }

  override def getStructChildEntries(
    macroName: String, structName: FullNameT[INameT], structA: StructA, mutability: MutabilityT):
  Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    if (mutability == ImmutableT) {
      val structNameS = structA.name
      val structType = structA.tyype
      val structIdentifyingRunes = structA.identifyingRunes
      val structIdentifyingRuneToType =
        structIdentifyingRunes.map(_.rune)
          .zip(structIdentifyingRunes.map(_.rune).map(structA.runeToType)).toMap

      val freeFunctionA =
        makeFunction(
          structNameS,
          structA.range,
          structType,
          structIdentifyingRunes.map(_.rune),
          structIdentifyingRuneToType)
      val freeNameT = structName.addStep(nameTranslator.translateFunctionNameToTemplateName(freeFunctionA.name))
      Vector((freeNameT, FunctionEnvEntry(freeFunctionA)))
    } else {
      Vector()
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
        case KindTemplataType => FunctionTemplataType
        case TemplateTemplataType(paramTypes, KindTemplataType) => {
          TemplateTemplataType(paramTypes, FunctionTemplataType)
        }
      },
      structIdentifyingRunes.map(r => RuneUsage(RangeS.internal(-64002), r)),
      structIdentifyingRuneToType ++
        Map(
          CodeRuneS("DropStruct") -> structType,
          CodeRuneS("DropP1") -> CoordTemplataType,
          CodeRuneS("DropV") -> CoordTemplataType),
      Vector(
        ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(interner.intern(CodeVarNameS("x")))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1"))), None))),
      Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV"))),
      Vector(
        structType match {
          case KindTemplataType => {
            EqualsSR(
              RangeS.internal(-167215),
              RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1")),
              RuneUsage(RangeS.internal(-64002), CodeRuneS("DropStruct")))
          }
          case TemplateTemplataType(_, KindTemplataType) => {
            CallSR(
              RangeS.internal(-167215),
              RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1")),
              RuneUsage(RangeS.internal(-64002), CodeRuneS("DropStruct")),
              structIdentifyingRunes.map(r => RuneUsage(RangeS.internal(-64002), r)).toArray)
          }
        },
        LookupSR(RangeS.internal(-1672163), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropStruct")), structNameS.getImpreciseName(interner)),
        LookupSR(RangeS.internal(-1672164), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV")), interner.intern(CodeNameS("void")))),
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
      FunctionTemplataType,
      Vector(),
      Map(
        CodeRuneS("DropP1") -> CoordTemplataType,
        CodeRuneS("DropV") -> CoordTemplataType),
      Vector(
        ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(interner.intern(CodeVarNameS("x")))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1"))), None))),
      Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV"))),
      Vector(
        LookupSR(
          RangeS.internal(-1672165),
          RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1")),
          interner.intern(SelfNameS())),
        LookupSR(RangeS.internal(-1672166), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV")), interner.intern(CodeNameS("void")))),
      GeneratedBodyS(freeGeneratorId))
  }

  override def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    generatorId: String,
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
    val structDef = temputs.lookupStruct(structTT)
    val structOwnership = if (structDef.mutability == MutableT) OwnT else ShareT
    val structType = CoordT(structOwnership, structDef.getRef)

    val ret = CoordT(ShareT, VoidT())
    val header = FunctionHeaderT(env.fullName, Vector.empty, params2, ret, originFunction1)

    temputs.declareFunctionReturnType(header.toSignature, header.returnType)

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
        case ImmutableT => {
          Templar.consecutive(
            Vector(DestroyTE(ArgLookupTE(0, structType), structTT, memberLocalVariables)) ++
              memberLocalVariables.map(v => {
                destructorTemplar.drop(bodyEnv, temputs, callRange, UnletTE(v))
              }))
        }
        case MutableT => vwat() // Shouldnt be a free for mutables
      }

    val function2 = FunctionT(header, BlockTE(Templar.consecutive(Vector(expr, ReturnTE(VoidLiteralTE())))))
    temputs.addFunction(function2)
    function2.header
  }
}
