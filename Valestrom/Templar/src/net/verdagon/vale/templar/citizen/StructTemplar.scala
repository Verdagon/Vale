package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer}
import net.verdagon.vale._
import net.verdagon.vale.templar.OverloadTemplar.IScoutExpectedFunctionResult
import net.verdagon.vale.templar.citize.StructTemplarTemplateArgsLayer

import scala.collection.immutable.List
import scala.collection.mutable

case class WeakableImplingMismatch(structWeakable: Boolean, interfaceWeakable: Boolean) extends Throwable { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

trait IStructTemplarDelegate {
  def evaluateOrdinaryFunctionFromNonCallForHeader(
    temputs: Temputs,
    functionTemplata: FunctionTemplata):
  FunctionHeaderT

  def evaluateTemplatedFunctionFromNonCallForHeader(temputs: Temputs, functionTemplata: FunctionTemplata): FunctionHeaderT

  def scoutExpectedFunctionForPrototype(
    env: IEnvironment,
    temputs: Temputs,
    callRange: RangeS,
    functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[ParamFilter],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean):
  IScoutExpectedFunctionResult

  def makeImmConcreteDestructor(
    temputs: Temputs,
    env: IEnvironment,
    structTT: StructTT):
  PrototypeT

  def getImmInterfaceDestructorOverride(
    temputs: Temputs,
    env: IEnvironment,
    structTT: StructTT,
    implementedInterfaceRefT: InterfaceTT):
  PrototypeT

  def getImmInterfaceDestructor(
    temputs: Temputs,
    env: IEnvironment,
    interfaceTT: InterfaceTT):
  PrototypeT

  def getImmConcreteDestructor(
    temputs: Temputs,
    env: IEnvironment,
    structTT: StructTT):
  PrototypeT
}

class StructTemplar(
    opts: TemplarOptions,
    profiler: IProfiler,
    newTemplataStore: () => TemplatasStore,
    inferTemplar: InferTemplar,
    ancestorHelper: AncestorHelper,
    delegate: IStructTemplarDelegate) {
  val templateArgsLayer =
    new StructTemplarTemplateArgsLayer(
      opts, profiler, newTemplataStore, inferTemplar, ancestorHelper, delegate)

  def addBuiltInStructs(env: PackageEnvironment[INameT], temputs: Temputs): Unit = {
    templateArgsLayer.addBuiltInStructs(env, temputs)
  }

  private def makeStructConstructor(
    temputs: Temputs,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    structDef: StructDefinitionT,
    constructorFullName: FullNameT[IFunctionNameT]):
  FunctionHeaderT = {
    templateArgsLayer.makeStructConstructor(temputs, maybeConstructorOriginFunctionA, structDef, constructorFullName)
  }

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
        rules += CallSR(structA.range, retRune, true, RuneUsage(structA.range, structNameRune), structA.identifyingRunes.toArray)
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
  def getInterfaceConstructor(interfaceA: InterfaceA): FunctionA = {
    profiler.newProfile("StructTemplarGetInterfaceConstructor", interfaceA.name.name, () => {
      opts.debugOut("todo: put all the members' rules up in the top of the struct")
      val identifyingRunes = interfaceA.identifyingRunes
      val functorRunes = interfaceA.internalMethods.indices.map(i => (CodeRuneS("Functor" + i)))
      val params =
        interfaceA.internalMethods.zipWithIndex.map({ case (method, index) =>
          ParameterS(
            AtomSP(
              method.range,
              Some(CaptureS(AnonymousSubstructMemberNameS(index))),
              None,
              Some(RuneUsage(method.range, CodeRuneS("Functor" + index))),
              None))
        })

      val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
      runeToType ++= interfaceA.runeToType
      runeToType ++= functorRunes.map(functorRune => (functorRune -> CoordTemplataType)).toMap
      runeToType.put(AnonymousSubstructParentInterfaceRuneS(), KindTemplataType)

      val rules = mutable.ArrayBuffer[IRulexSR]()
      rules ++= interfaceA.rules

      // We stash the interface type in the env with this rune, so that when the interface constructor
      // generator runs, it can read this to know what interface it's making a subclass of.
      val substructRune = RuneUsage(interfaceA.name.range, AnonymousSubstructParentInterfaceRuneS())
      runeToType += (substructRune.rune -> KindTemplataType)
      if (interfaceA.isTemplate) {
        val structNameRune = RuneUsage(interfaceA.name.range, StructNameRuneS(interfaceA.name))
        runeToType += (structNameRune.rune -> interfaceA.tyype)
        rules += LookupSR(interfaceA.range, structNameRune, CodeTypeNameS(interfaceA.name.name))
        rules += CallSR(interfaceA.range, substructRune, true, structNameRune, interfaceA.identifyingRunes.toArray)
      } else {
        rules += LookupSR(interfaceA.range, substructRune, CodeTypeNameS(interfaceA.name.name))
      }

      val isTemplate = interfaceA.tyype != KindTemplataType

      val templateParams =
        (interfaceA.tyype match {
          case KindTemplataType => Vector.empty
          case TemplateTemplataType(params, KindTemplataType) => params
        }) ++
          interfaceA.internalMethods.map(meth => CoordTemplataType)
      val functionType =
        if (templateParams.isEmpty) FunctionTemplataType else TemplateTemplataType(templateParams, FunctionTemplataType)

      val TopLevelCitizenDeclarationNameS(name, range) = interfaceA.name
      FunctionA(
        interfaceA.range,
        FunctionNameS(name, range.begin),
        Vector(UserFunctionS),
        functionType,
//        interfaceA.knowableRunes ++ functorRunes ++ (if (isTemplate) Vector.empty else Vector(AnonymousSubstructParentInterfaceRuneS())),
        identifyingRunes,
//        interfaceA.localRunes ++ functorRunes ++ Vector(AnonymousSubstructParentInterfaceRuneS()),
        runeToType.toMap,
        params,
        None,
        rules.toVector,
        GeneratedBodyS("interfaceConstructorGenerator"))
    })
  }

  def getStructRef(
    temputs: Temputs,
    callRange: RangeS,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: Vector[ITemplata]):
  (StructTT) = {
    profiler.newProfile("StructTemplarGetStructRef", structTemplata.debugString + "<" + uncoercedTemplateArgs.mkString(", ") + ">", () => {
      templateArgsLayer.getStructRef(
        temputs, callRange, structTemplata, uncoercedTemplateArgs)
    })
  }

  def getInterfaceRef(
    temputs: Temputs,
    callRange: RangeS,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: Vector[ITemplata]):
  (InterfaceTT) = {
//    profiler.newProfile("StructTemplar-getInterfaceRef", interfaceTemplata.debugString + "<" + uncoercedTemplateArgs.mkString(", ") + ">", () => {
      templateArgsLayer.getInterfaceRef(
        temputs, callRange, interfaceTemplata, uncoercedTemplateArgs)
//    })
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: Temputs,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
//    profiler.newProfile("StructTemplar-makeClosureUnderstruct", name.codeLocation.toString, () => {
      templateArgsLayer.makeClosureUnderstruct(containingFunctionEnv, temputs, name, functionS, members)
//    })
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: PackageEnvironment[INameT], temputs: Temputs, memberTypes2: Vector[CoordT], name: ICitizenNameT):
  (StructTT, MutabilityT) = {
//    profiler.newProfile("StructTemplar-makeSeqOrPackUnderstruct", "[" + memberTypes2.map(_.toString).mkString(", ") + "]", () => {
      templateArgsLayer.makeSeqOrPackUnerstruct(env, temputs, memberTypes2, name)
//    })
  }

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    temputs: Temputs,
    range: RangeS,
    interfaceTT: InterfaceTT,
    members: Vector[CoordT]):
  StructTT = {
//    profiler.newProfile("StructTemplar-makeSeqOrPackUnderstruct", "[" + interfaceTT.toString + " " + members.map(_.toString).mkString(", ") + "]", () => {
      val anonymousSubstructName =
        interfaceTT.fullName.addStep(AnonymousSubstructNameT(members))

      temputs.structDeclared(anonymousSubstructName) match {
        case Some(s) => return s
        case None =>
      }

      val interfaceEnv = temputs.getEnvForInterfaceRef(interfaceTT)
      val (s, _) =
        templateArgsLayer.makeAnonymousSubstruct(
          interfaceEnv, temputs, range, interfaceTT, anonymousSubstructName)
      s
//    })
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  // This does NOT make a constructor, because its so easy to just Construct2 it.
  def prototypeToAnonymousStruct(
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    range: RangeS,
    prototype: PrototypeT):
  StructTT = {
//    profiler.newProfile("StructTemplar-prototypeToAnonymousStruct", prototype.toString, () => {
      val structFullName = prototype.fullName.addStep(AnonymousSubstructLambdaNameT(CodeLocationS.internal(-13)))

      temputs.structDeclared(structFullName) match {
        case Some(structTT) => return structTT
        case None =>
      }

      val outerEnv = temputs.getEnvForFunctionSignature(prototype.toSignature)
      templateArgsLayer.prototypeToAnonymousStruct(
        outerEnv, temputs, life, range, prototype, structFullName)
//    })
  }

  // This doesnt make a constructor, but its easy enough to make manually.
  def prototypeToAnonymousSubstruct(
      temputs: Temputs,
    life: LocationInFunctionEnvironment,
      range: RangeS,
      interfaceTT: InterfaceTT,
      prototype: PrototypeT):
  (StructTT, PrototypeT) = {
//    profiler.newProfile("StructTemplar-prototypeToAnonymousSubstruct", prototype.toString + " " + interfaceTT.toString, () => {
      val functionStructRef = prototypeToAnonymousStruct(temputs, life, range, prototype)
      val functionStructType = CoordT(ShareT, ReadonlyT, functionStructRef)

      val lambdas = Vector(functionStructType)

      val anonymousSubstructTT =
        makeAnonymousSubstruct(temputs, range, interfaceTT, lambdas)
      val anonymousSubstructType = CoordT(ShareT, ReadonlyT, anonymousSubstructTT)

      val constructorName =
        interfaceTT.fullName
          .addStep(AnonymousSubstructNameT(Vector(functionStructType)))
          .addStep(ConstructorNameT(Vector.empty))
      temputs.prototypeDeclared(constructorName) match {
        case Some(func) => return (anonymousSubstructTT, func)
        case None =>
      }

      // Now we make a function which constructs a functionStruct, then constructs a substruct.
      val constructor2 =
        FunctionT(
          FunctionHeaderT(
            constructorName,
            Vector.empty,
            Vector.empty,
            anonymousSubstructType,
            None),
          BlockTE(
              ReturnTE(
                ConstructTE(
                  anonymousSubstructTT,
                  anonymousSubstructType,
                  Vector(
                    ConstructTE(
                      functionStructRef,
                      CoordT(ShareT, ReadonlyT, functionStructRef),
                      Vector.empty))))))
      temputs.declareFunctionSignature(range, constructor2.header.toSignature, None)
      temputs.declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
      temputs.addFunction(constructor2);

      vassert(temputs.getDeclaredSignatureOrigin(constructor2.header.fullName) == Some(range))

      (anonymousSubstructTT, constructor2.header.toPrototype)
//    })
  }

//  // Makes a functor for the given prototype.
//  def functionToLambda(
//    outerEnv: IEnvironment,
//    temputs: Temputs,
//    header: FunctionHeader2):
//  structTT = {
//    templateArgsLayer.functionToLambda(outerEnv, temputs, header)
//  }

  def getMemberCoords(temputs: Temputs, structTT: StructTT): Vector[CoordT] = {
    temputs.getStructDefForRef(structTT).members.map(_.tyype).map({
      case ReferenceMemberTypeT(coord) => coord
      case AddressMemberTypeT(_) => {
        // At time of writing, the only one who calls this is the inferer, who wants to know so it
        // can match incoming arguments into a destructure. Can we even destructure things with
        // addressible members?
        vcurious()
      }
    })
  }

//  def headerToIFunctionSubclass(
//    env: IEnvironment,
//    temputs: Temputs,
//    header: FunctionHeader2):
//  structTT = {
//    val (paramType, returnType) =
//      header.toPrototype match {
//        case Prototype2(_, Vector(paramType), returnType) => (paramType, returnType)
//        case _ => vimpl("Only IFunction1 implemented")
//      }
//    val Some(InterfaceTemplata(ifunction1InterfaceEnv, ifunction1InterfaceA)) =
//      env.getNearestTemplataWithName("IFunction1", Set(TemplataLookupContext))
//
//    val lambdaStructRef = functionToLambda(env, temputs, header)
//
//    val ifunction1InterfaceRef =
//      getInterfaceRef(
//        ifunction1InterfaceEnv,
//        temputs,
//        ifunction1InterfaceA,
//        Vector(
//          MutabilityTemplata(Immutable),
//          CoordTemplata(paramType),
//          CoordTemplata(returnType)))
//
//    makeAnonymousSubstruct()
//  }

  def prototypeToAnonymousIFunctionSubstruct(
      env: IEnvironment,
      temputs: Temputs,
    life: LocationInFunctionEnvironment,
      range: RangeS,
      prototype: PrototypeT):
  (InterfaceTT, StructTT, PrototypeT) = {
//    profiler.newProfile("StructTemplar-prototypeToAnonymousIFunctionSubstruct", prototype.toString, () => {
      val returnType = prototype.returnType
      val Vector(paramType) = prototype.fullName.last.parameters

      val ifunction1Templata@InterfaceTemplata(_, _) =
        vassertOne(env.lookupWithImpreciseName(profiler, CodeTypeNameS("IFunction1"), Set(TemplataLookupContext), true))
      val ifunction1InterfaceRef =
        getInterfaceRef(
          temputs,
          range,
          ifunction1Templata,
          Vector(
            MutabilityTemplata(ImmutableT),
            CoordTemplata(paramType),
            CoordTemplata(returnType)))

      val (elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype) =
        prototypeToAnonymousSubstruct(
          temputs, life, range, ifunction1InterfaceRef, prototype)

      (ifunction1InterfaceRef, elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype)
//    })
  }
}

object StructTemplar {

  def getCompoundTypeMutability(memberTypes2: Vector[CoordT])
  : MutabilityT = {
    val membersOwnerships = memberTypes2.map(_.ownership)
    val allMembersImmutable = membersOwnerships.isEmpty || membersOwnerships.toSet == Set(ShareT)
    if (allMembersImmutable) ImmutableT else MutableT
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
            structTemplar.makeStructConstructor(temputs, originFunction, structDefT, env.fullName)
          }
        },
      "interfaceConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            profiler: IProfiler,
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
            // The interface should be in the "__Interface" rune of the function environment.
            val interfaceTT =
              env.lookupWithImpreciseName(profiler, RuneNameS(AnonymousSubstructParentInterfaceRuneS()), Set(TemplataLookupContext), true) match {
                case List(KindTemplata(ir @ InterfaceTT(_))) => ir
                case other => vwat(other)
              }

            val structTT =
              structTemplar.makeAnonymousSubstruct(
                temputs, callRange, interfaceTT, paramCoords.map(_.tyype))
            val structDef = temputs.lookupStruct(structTT)

            val constructorFullName = env.fullName
            val constructor =
              structTemplar.makeStructConstructor(
                temputs, originFunction, structDef, constructorFullName)

            constructor
          }
        })
  }
}