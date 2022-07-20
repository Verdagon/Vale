package dev.vale.typing.citizen

import dev.vale.highertyping.ImplA
import dev.vale.postparsing.{IRuneS, ITemplataType, ImplImpreciseNameS}
import dev.vale.postparsing.rules.Equivalencies
import dev.vale.solver.SolverErrorHumanizer
import dev.vale.typing.OverloadResolver.InferFailure
import dev.vale.typing.env.{ExpressionLookupContext, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.{CantImplNonInterface, CompileErrorExceptionT, CompilerOutputs, InferCompiler, InitialKnown, TypingPassOptions}
import dev.vale.typing.names.{FullNameT, ImplTemplateNameT, ImplTemplateSubNameT, ImplTemplateSuperNameT, InterfaceTemplateNameT, NameTranslator}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, postparsing, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast.ImplT
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionFailure

import scala.collection.immutable.List

trait IAncestorHelperDelegate {
  def resolveInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  InterfaceTT
}

class AncestorHelper(
    opts: TypingPassOptions,

    interner: Interner,
    inferCompiler: InferCompiler,
    delegate: IAncestorHelperDelegate) {

  // We don't have an isAncestor call, see REMUIDDA.

  private def compileImplInner(
    coutputs: CompilerOutputs,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplTemplata):
  Map[IRuneS, ITemplata[ITemplataType]] = {
    val ImplTemplata(env, impl) = implTemplata
    val ImplA(range, name, impreciseName, identifyingRunes, rules, runeToType, structKindRune, interfaceKindRune) = impl

    val definitionRules = rules.filter(InferCompiler.includeRuleInDefinitionSolve)
    vcurious(rules == definitionRules)

    val result =
      inferCompiler.solveComplete(
        env, None, coutputs, definitionRules, runeToType, range, initialKnowns, Vector())
    val inferences =
      result match {
        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(range, e))
        case Ok(inferences) => inferences
      }
    inferences
  }

  private def compileImplGivenChildCitizen(
    coutputs: CompilerOutputs,
    subCitizenRef: ICitizenTT,
    implTemplata: ImplTemplata):
  InterfaceTT = {
    val initialKnowns =
      Vector(InitialKnown(implTemplata.impl.structKindRune, KindTemplata(subCitizenRef)))
    val inferences = compileImplInner(coutputs, initialKnowns, implTemplata)
    val interfaceTT =
      inferences(implTemplata.impl.interfaceKindRune.rune) match {
        case KindTemplata(interfaceTT @ InterfaceTT(_)) => interfaceTT
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case KindTemplata(other) => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
      }
    interfaceTT
  }

  private def compileImplGivenSuperInterface(
    coutputs: CompilerOutputs,
    superInterfaceRef: InterfaceTT,
    implTemplata: ImplTemplata):
  ICitizenTT = {
    val initialKnowns =
      Vector(InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(superInterfaceRef)))
    val inferences = compileImplInner(coutputs, initialKnowns, implTemplata)
    val citizenTT =
      inferences(implTemplata.impl.structKindRune.rune) match {
        case KindTemplata(cr : ICitizenTT) => cr
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case KindTemplata(other) => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
      }
    citizenTT
  }

  def compileParentImpls(
    coutputs: CompilerOutputs,
    childCitizenRef: ICitizenTT):
  Array[(ImplTemplata, InterfaceTT)] = {
    Profiler.frame(() => {
      val citizenTemplateFullName = TemplataCompiler.getCitizenTemplate(childCitizenRef.fullName)
      val citizenEnv = coutputs.getEnvForTemplate(citizenTemplateFullName)
      // See INSHN, the imprecise name for an impl is the wrapped imprecise name of its struct template.
      val needleImplTemplateFullName = ImplTemplateSubNameT(citizenTemplateFullName)
      val implTemplates =
        citizenEnv.lookupAllWithName(needleImplTemplateFullName, Set(TemplataLookupContext))
      val implsAndParentInterfaces =
        implTemplates.map({
          case impl @ ImplTemplata(_, _) => (impl, compileImplGivenChildCitizen(coutputs, childCitizenRef, impl))
          case other => vwat(other)
        })
      implsAndParentInterfaces.toArray
    })
  }

  def compileChildImpls(
    coutputs: CompilerOutputs,
    parentInterfaceRef: InterfaceTT):
  Array[(ImplTemplata, ICitizenTT)] = {
    Profiler.frame(() => {
      val parentInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(parentInterfaceRef.fullName)
      val parentInterfaceEnv = coutputs.getEnvForTemplate(parentInterfaceTemplateFullName)
      // See INSHN, the imprecise name for an impl is the wrapped imprecise name of its struct template.
      val needleImplTemplateFullName = ImplTemplateSuperNameT(parentInterfaceTemplateFullName)
      val implTemplates =
        parentInterfaceEnv.lookupAllWithName(needleImplTemplateFullName, Set(TemplataLookupContext))
      val implsAndParentInterfaces =
        implTemplates.map({
          case impl @ ImplTemplata(_, _) => (impl, compileImplGivenSuperInterface(coutputs, parentInterfaceRef, impl))
          case other => vwat(other)
        })
      implsAndParentInterfaces.toArray
    })
  }

  // Doesn't include self
  def compileGetAncestorInterfaces(
    coutputs: CompilerOutputs,
    descendantCitizenRef: ICitizenTT):
  (Map[InterfaceTT, ImplTemplateNameT]) = {
    Profiler.frame(() => {
      val parentInterfacesAndImpls =
        compileGetParentInterfaces(coutputs, descendantCitizenRef)

      // Make a map that contains all the parent interfaces, with distance 1
      val foundSoFar =
        parentInterfacesAndImpls.map({ case (interfaceRef, impl) => (interfaceRef, impl) }).toMap

      compileGetAncestorInterfacesInner(
        coutputs,
        foundSoFar,
        parentInterfacesAndImpls.toMap)
    })
  }

  private def compileGetAncestorInterfacesInner(
    coutputs: CompilerOutputs,
    // This is so we can know what we've already searched.
    nearestDistanceByInterfaceRef: Map[InterfaceTT, ImplTemplateNameT],
    // These are the interfaces that are *exactly* currentDistance away.
    // We will do our searching from here.
    interfacesAtCurrentDistance: Map[InterfaceTT, ImplTemplateNameT]):
  (Map[InterfaceTT, ImplTemplateNameT]) = {
    val interfacesAtNextDistance =
      interfacesAtCurrentDistance.foldLeft((Map[InterfaceTT, ImplTemplateNameT]()))({
        case ((previousAncestorInterfaceRefs), (parentInterfaceRef, parentImpl)) => {
          val parentAncestorInterfaceRefs =
            compileGetParentInterfaces(coutputs, parentInterfaceRef)
          (previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
        }
      })

    // Discard the ones that have already been found; they're actually at
    // a closer distance.
    val newlyFoundInterfaces =
    interfacesAtNextDistance.keySet
      .diff(nearestDistanceByInterfaceRef.keySet)
      .toVector
      .map(key => (key -> interfacesAtNextDistance(key)))
      .toMap

    if (newlyFoundInterfaces.isEmpty) {
      (nearestDistanceByInterfaceRef)
    } else {
      // Combine the previously found ones with the newly found ones.
      val newNearestDistanceByInterfaceRef =
        nearestDistanceByInterfaceRef ++ newlyFoundInterfaces.toMap

      compileGetAncestorInterfacesInner(
        coutputs,
        newNearestDistanceByInterfaceRef,
        newlyFoundInterfaces)
    }
  }

  def isParent(
    coutputs: CompilerOutputs,
    subCitizenRef: ICitizenTT,
    superInterfaceRef: InterfaceTT):
  Boolean = {
    val subCitizenDefinition = coutputs.lookupCitizen(subCitizenRef)
    val placeholderedTT = subCitizenDefinition.nameWithPlaceholders
    coutputs.getImplsForSubCitizenTemplate(placeholderedTT)

    TemplataCompiler.getInterfaceTemplate()
  }

}
