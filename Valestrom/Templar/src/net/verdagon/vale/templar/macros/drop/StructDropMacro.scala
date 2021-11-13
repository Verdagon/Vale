package net.verdagon.vale.templar.macros.drop

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
import net.verdagon.vale.templar.{Templar, Temputs}
import net.verdagon.vale.{CodeLocationS, PackageCoordinate, RangeS}

class StructDropMacro(
  destructorTemplar: DestructorTemplar
) extends IOnStructDefinedMacro with IFunctionBodyMacro {

  override def generatorId: String = "dropGenerator"

  override def getStructSiblingEntries(structName: FullNameT[INameT], structA: StructA):
  Vector[(INameT, FunctionEnvEntry)] = {
    Vector()
  }

  override def getStructChildEntries(
    structName: FullNameT[INameT], structA: StructA):
  Vector[(INameT, FunctionEnvEntry)] = {
    val structNameS = CodeNameS(structA.name.name)
    val structType = structA.tyype
    val structIdentifyingRunes = structA.identifyingRunes
    val structIdentifyingRuneToType =
      structIdentifyingRunes.map(_.rune)
        .zip(structIdentifyingRunes.map(_.rune).map(structA.runeToType)).toMap

    val functionA =
      makeDropFunction(
        structNameS, structA.range, structType, structIdentifyingRunes.map(_.rune), structIdentifyingRuneToType)

    val nameT = NameTranslator.translateFunctionNameToTemplateName(functionA.name)
    Vector((nameT, FunctionEnvEntry(functionA)))

//    DropTemplateNameT() ->
//      Vector(FunctionEnvEntry(structRunesEnv.globalEnv.structDropMacro.makeImplicitDropFunction(SelfNameS()))),
//    fullName.last -> Vector(TemplataEnvEntry(KindTemplata(temporaryStructRef))),
//    SelfNameT() -> Vector(TemplataEnvEntry(KindTemplata(temporaryStructRef))))))
  }

//  def addDrop(
//    mutability: MutabilityT):
//  (FunctionA, IFunctionGenerator) = {
//    // Drop is a function that:
//    // - If received an owning pointer, will call the destructor
//    // - If received a share pointer, will decrement it and if was last, call its destructor
//    // - If received a borrow, do nothing.
//    // Conceptually it's "drop the reference", as opposed to destructor which is "drop the object"
//
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
//          vassert(maybeReturnType2 == Some(CoordT(ShareT, ReadonlyT, VoidT())))
//          val Vector(CoordTemplata(ref2)) = namedEnv.fullName.last.templateArgs
//          val Vector(ParameterT(CodeVarNameT("x"), None, paramType2)) = params
//          vassert(paramType2 == ref2)
//          destructorTemplar.generateDropFunction(
//            namedEnv, temputs, maybeOriginFunction1.get, ref2)
//        }
//      }
//    (unevaluatedFunctionA, generator)
//  }

  def makeDropFunction(structNameS: CodeNameS, structRange: RangeS, structType: ITemplataType, structIdentifyingRunes: Vector[IRuneS], structIdentifyingRuneToType: Map[IRuneS, ITemplataType]) = {
    FunctionA(
      structRange,
      FunctionNameS(CallTemplar.DROP_FUNCTION_NAME, structRange.begin),
      Vector(UserFunctionS),
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
        ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(CodeVarNameS("x"))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1"))), None))),
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
        LookupSR(RangeS.internal(-167213), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropStruct")), structNameS),
        LookupSR(RangeS.internal(-167213), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV")), CodeNameS("void"))),
      GeneratedBodyS(generatorId))
  }

  // Implicit drop is one made for closures, arrays, or anything else that's not explicitly
  // defined by the user.
  def makeImplicitDropFunction(selfName: INameS, range: RangeS):
  FunctionA = {
    FunctionA(
      range,
      FunctionNameS(CallTemplar.DROP_FUNCTION_NAME, range.begin),
      Vector(UserFunctionS),
      FunctionTemplataType,
      Vector(),
      Map(
        CodeRuneS("DropP1") -> CoordTemplataType,
        CodeRuneS("DropV") -> CoordTemplataType),
      Vector(
        ParameterS(AtomSP(RangeS.internal(-1342), Some(CaptureS(CodeVarNameS("x"))), None, Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1"))), None))),
      Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV"))),
      Vector(
        LookupSR(
          RangeS.internal(-167213),
          RuneUsage(RangeS.internal(-64002), CodeRuneS("DropP1")),
          selfName),
        LookupSR(RangeS.internal(-167213), RuneUsage(RangeS.internal(-64002), CodeRuneS("DropV")), CodeNameS("void"))),
      GeneratedBodyS(generatorId))
  }

  override def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction1: Option[FunctionA],
    params2: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT = {
    val destructorFullName = env.fullName

    val bodyEnv = FunctionEnvironmentBox(env)

    val structTT @ StructTT(_) = params2.head.tyype.kind
    val structDef = temputs.lookupStruct(structTT)
    val structOwnership = if (structDef.mutability == MutableT) OwnT else ShareT
    val structPermission = if (structDef.mutability == MutableT) ReadwriteT else ReadonlyT
    val structBorrowOwnership = if (structDef.mutability == MutableT) ConstraintT else ShareT
    val structType = CoordT(structOwnership, structPermission, structDef.getRef)

    val header =
      FunctionHeaderT(
        destructorFullName,
        Vector.empty,
        params2,
        CoordT(ShareT, ReadonlyT, VoidT()),
        originFunction1);

    temputs
      .declareFunctionReturnType(header.toSignature, header.returnType)

    val body =
      structDef.mutability match {
        case ImmutableT => {
          BlockTE(
            ConsecutorTE(
              Vector(
                DiscardTE(ArgLookupTE(0, structType)),
                ReturnTE(VoidLiteralTE()))))
        }
        case MutableT => {
          val structArgument = ArgLookupTE(0, structType)
          val memberLocalVariables =
            structDef.members.flatMap({
              case StructMemberT(name, variability, ReferenceMemberTypeT(reference)) => {
                Vector(ReferenceLocalVariableT(destructorFullName.addStep(name), FinalT, reference))
              }
              case StructMemberT(name, variability, AddressMemberTypeT(reference)) => {
                // See Destructure2 and its handling of addressible members for why
                // we don't include these in the destination variables.
                Vector.empty
              }
            })

          val destroyedUnletStruct = DestroyTE(structArgument, structTT, memberLocalVariables)
          val destructMemberExprs =
            memberLocalVariables.map({
              case (variable) => {
                val destructMemberExpr = destructorTemplar.drop(bodyEnv, temputs, UnletTE(variable))
                destructMemberExpr
              }
            })

          val returnVoid = ReturnTE(VoidLiteralTE())
          BlockTE(
            Templar.consecutive(
              Vector(destroyedUnletStruct) ++ destructMemberExprs :+ returnVoid))
        }
      }
    val function2 =
      FunctionT(
        header,
        body)
    temputs.addFunction(function2)
    (function2.header)
  }
}
