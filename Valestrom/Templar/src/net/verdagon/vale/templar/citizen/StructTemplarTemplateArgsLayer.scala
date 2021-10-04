package net.verdagon.vale.templar.citize

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.citizen.{AncestorHelper, IStructTemplarDelegate, StructTemplarMiddle}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.FunctionTemplar
import net.verdagon.vale.{IProfiler, RangeS, vassert, vfail, vimpl, vwat}

import scala.collection.immutable.List

class StructTemplarTemplateArgsLayer(
    opts: TemplarOptions,
    profiler: IProfiler,
    newTemplataStore: () => TemplatasStore,
    inferTemplar: InferTemplar,
    ancestorHelper: AncestorHelper,
    delegate: IStructTemplarDelegate) {
  val middle = new StructTemplarMiddle(opts, profiler, newTemplataStore, ancestorHelper, delegate)

  def addBuiltInStructs(env: PackageEnvironment[INameT], temputs: Temputs): Unit = {
    middle.addBuiltInStructs(env, temputs)
  }

  def makeStructConstructor(
    temputs: Temputs,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    structDef: StructDefinitionT,
    constructorFullName: FullNameT[IFunctionNameT]):
  FunctionHeaderT = {
    middle.makeStructConstructor(temputs, maybeConstructorOriginFunctionA, structDef, constructorFullName)
  }

  def getStructRef(
    temputs: Temputs,
    callRange: RangeS,
    structTemplata: StructTemplata,
    templateArgs: Vector[ITemplata]):
  (StructTT) = {
    profiler.newProfile("getStructRef", structTemplata.debugString + "<" + templateArgs.map(_.toString).mkString(", ") + ">", () => {
      val StructTemplata(env, structA) = structTemplata
      val TopLevelCitizenDeclarationNameS(humanName, codeLocation) = structA.name
      val structTemplateName = NameTranslator.translateCitizenName(structA.name)
      val structLastName = structTemplateName.makeCitizenName(templateArgs)
      val fullName = env.fullName.addStep(structLastName)

      temputs.structDeclared(fullName) match {
        case Some(structTT) => {
          (structTT)
        }
        case None => {
          // not sure if this is okay or not, do we allow this?
          if (templateArgs.size != structA.identifyingRunes.size) {
            vfail("wat?")
          }
          val temporaryStructRef = StructTT(fullName)
          temputs.declareStruct(temporaryStructRef)

          structA.maybePredictedMutability match {
            case None =>
            case Some(predictedMutability) => temputs.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(predictedMutability))
          }
          vassert(structA.identifyingRunes.size == templateArgs.size)
          val inferences =
            inferTemplar.solveExpectComplete(
              env,
              temputs,
              structA.rules,
              structA.runeToType,
              callRange,
              Map(),
              structA.identifyingRunes.map(_.rune).zip(templateArgs).toMap)

          structA.maybePredictedMutability match {
            case None => {
              val MutabilityTemplata(mutability) = inferences(structA.mutabilityRune.rune)
              temputs.declareStructMutability(temporaryStructRef, mutability)
            }
            case Some(_) =>
          }

          middle.getStructRef(env, temputs, callRange, structA, inferences)
        }
      }
    })
  }

  def getInterfaceRef(
    temputs: Temputs,
    callRange: RangeS,
    interfaceTemplata: InterfaceTemplata,
    templateArgs: Vector[ITemplata]):
  (InterfaceTT) = {
    profiler.newProfile("getInterfaceRef", interfaceTemplata.debugString + "<" + templateArgs.map(_.toString).mkString(", ") + ">", () => {
      val InterfaceTemplata(env, interfaceS) = interfaceTemplata
      val TopLevelCitizenDeclarationNameS(humanName, codeLocation) = interfaceS.name
      val interfaceTemplateName = NameTranslator.translateCitizenName(interfaceS.name)
      val interfaceLastName = interfaceTemplateName.makeCitizenName(templateArgs)
      val fullName = env.fullName.addStep(interfaceLastName)

      temputs.interfaceDeclared(fullName) match {
        case Some(interfaceTT) => {
          (interfaceTT)
        }
        case None => {
          // not sure if this is okay or not, do we allow this?
          if (templateArgs.size != interfaceS.identifyingRunes.size) {
            vfail("wat?")
          }
          val temporaryInterfaceRef = InterfaceTT(fullName)
          temputs.declareInterface(temporaryInterfaceRef)


          interfaceS.maybePredictedMutability match {
            case None =>
            case Some(predictedMutability) => temputs.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(predictedMutability))
          }
          vassert(interfaceS.identifyingRunes.size == templateArgs.size)

          val inferences =
            inferTemplar.solveExpectComplete(
              env,
              temputs,
              interfaceS.rules,
              interfaceS.runeToType,
              callRange,
              Map(),
              interfaceS.identifyingRunes.map(_.rune).zip(templateArgs).toMap)

          interfaceS.maybePredictedMutability match {
            case None => {
              val MutabilityTemplata(mutability) = inferences(interfaceS.mutabilityRune.rune)
              temputs.declareInterfaceMutability(temporaryInterfaceRef, mutability)
            }
            case Some(_) =>
          }

          middle.getInterfaceRef(env, temputs, callRange, interfaceS, inferences)
        }
      }
    })
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: Temputs,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
    middle.makeClosureUnderstruct(containingFunctionEnv, temputs, name, functionS, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnerstruct(env: PackageEnvironment[INameT], temputs: Temputs, memberTypes2: Vector[CoordT], name: ICitizenNameT):
  (StructTT, MutabilityT) = {
    middle.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, name)
  }

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    interfaceEnv: IEnvironment,
    temputs: Temputs,
    range: RangeS,
    interfaceTT: InterfaceTT,
    substructName: FullNameT[AnonymousSubstructNameT]):
  (StructTT, MutabilityT) = {
    middle.makeAnonymousSubstruct(
      interfaceEnv,
      temputs,
      range,
      interfaceTT,
      substructName)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  def prototypeToAnonymousStruct(
    outerEnv: IEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    range: RangeS,
    prototype: PrototypeT,
    structFullName: FullNameT[ICitizenNameT]):
  StructTT = {
    middle.prototypeToAnonymousStruct(
      outerEnv, temputs, life, range, prototype, structFullName)
  }
}
