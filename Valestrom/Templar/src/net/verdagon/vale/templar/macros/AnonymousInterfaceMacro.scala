package net.verdagon.vale.templar.macros

import net.verdagon.vale.{CodeLocationS, IProfiler, RangeS, vassert, vassertOne, vwat}
import net.verdagon.vale.astronomer.{FunctionA, InterfaceA}
import net.verdagon.vale.scout.{AnonymousSubstructMemberNameS, AnonymousSubstructParentInterfaceRuneS, CodeRuneS, CodeTypeNameS, FunctionNameS, GeneratedBodyS, IRuneS, ParameterS, RuneNameS, StructNameRuneS, TopLevelCitizenDeclarationNameS, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast.{BlockTE, ConstructTE, FunctionT, LocationInFunctionEnvironment, ReturnTE}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvironment, IEnvironment, TemplataLookupContext}
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.{AnonymousSubstructLambdaNameT, AnonymousSubstructNameT, ConstructorNameT}
import net.verdagon.vale.templar.templata.{CoordTemplata, FunctionHeaderT, InterfaceTemplata, KindTemplata, MutabilityTemplata, ParameterT, PrototypeT}
import net.verdagon.vale.templar.{ArrayTemplar, IFunctionGenerator, Temputs}
import net.verdagon.vale.templar.types.{CoordT, CoordTemplataType, FunctionTemplataType, ITemplataType, ImmutableT, InterfaceTT, KindTemplataType, ReadonlyT, ShareT, StructTT, TemplateTemplataType}

import scala.collection.immutable.List
import scala.collection.mutable

class AnonymousInterfaceMacro {

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
        rules += CallSR(interfaceA.range, substructRune, structNameRune, interfaceA.identifyingRunes.toArray)
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


  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
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
