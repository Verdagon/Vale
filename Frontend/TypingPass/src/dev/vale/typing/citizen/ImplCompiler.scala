package dev.vale.typing.citizen

import dev.vale.highertyping.ImplA
import dev.vale.postparsing.{IRuneS, ITemplataType}
import dev.vale.postparsing.rules.{Equivalencies, RuleScout}
import dev.vale.solver.SolverErrorHumanizer
import dev.vale.typing.OverloadResolver.InferFailure
import dev.vale.typing.env.{ExpressionLookupContext, TemplataLookupContext, TemplatasStore}
import dev.vale.typing._
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, U, postparsing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast.{CitizenDefinitionT, ImplT, InterfaceDefinitionT}
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionFailure

import scala.collection.immutable.List

class ImplCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    structCompiler: StructCompiler,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler) {

  // We don't have an isAncestor call, see REMUIDDA.

  private def solveImpl(
    coutputs: CompilerOutputs,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplTemplata):
  Map[IRuneS, ITemplata[ITemplataType]] = {
    val ImplTemplata(parentEnv, impl) = implTemplata
    val ImplA(range, name, identifyingRunes, rules, runeToType, structKindRune, interfaceKindRune) = impl

    val implTemplateFullName =
      parentEnv.fullName.addStep(interner.intern(ImplTemplateDeclareNameT(range.begin)))

    val outerEnv =
      CitizenEnvironment(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateFullName,
        implTemplateFullName,
        TemplatasStore(implTemplateFullName, Map(), Map()))
    coutputs.declareTemplate(implTemplateFullName)
    coutputs.declareOuterEnvForTemplate(implTemplateFullName, outerEnv)

    // Remember, impls can have rules too, such as:
    //   impl<T> Opt<T> for Some<T> where func drop(T)void;
    // so we do need to filter them out when compiling.
    val definitionRules = rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    val result =
      inferCompiler.solveComplete(
        outerEnv, None, coutputs, definitionRules, runeToType, range, initialKnowns, Vector())
    val inferences =
      result match {
        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(range, e))
        case Ok(inferences) => inferences
      }
    inferences
  }

  private def compileImplGivenSubCitizen(
    coutputs: CompilerOutputs,
    placeholderedSubCitizenTT: ICitizenTT,
    implTemplata: ImplTemplata):
  Unit = {
    val subCitizenTemplateFullName =
      TemplataCompiler.getCitizenTemplate(placeholderedSubCitizenTT.fullName)

    val inferencesFromPlaceholderedSubCitizen =
      solveImpl(
        coutputs,
        Vector(InitialKnown(implTemplata.impl.structKindRune, KindTemplata(placeholderedSubCitizenTT))),
        implTemplata)
    val parentInterfaceFromPlaceholderedSubCitizen =
      inferencesFromPlaceholderedSubCitizen(implTemplata.impl.interfaceKindRune.rune) match {
        case KindTemplata(interfaceTT @ InterfaceTT(_)) => interfaceTT
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case other => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
      }

    val parentInterfaceTemplateFullName =
      TemplataCompiler.getInterfaceTemplate(parentInterfaceFromPlaceholderedSubCitizen.fullName)
    val parentInterfaceDefinition =
      coutputs.lookupInterface(parentInterfaceTemplateFullName)

    val inferencesFromPlaceholderedSuperInterface =
      solveImpl(
        coutputs,
        Vector(InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(parentInterfaceDefinition.placeholderedInterface))),
        implTemplata)
    val subCitizenFromPlaceholderedParentInterface =
      inferencesFromPlaceholderedSuperInterface(implTemplata.impl.structKindRune.rune) match {
        case KindTemplata(cit : ICitizenTT) => cit
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case other => throw CompileErrorExceptionT(NonCitizenCantImpl(implTemplata.impl.range, other))
      }

    val implT =
      interner.intern(
        ImplT(
          subCitizenTemplateFullName,
          parentInterfaceFromPlaceholderedSubCitizen,
          parentInterfaceTemplateFullName,
          subCitizenFromPlaceholderedParentInterface))
    // There may be a collision here but it's fine as this call will deduplicate. See CIFBD.
    coutputs.addImpl(implT)
  }

  private def compileImplGivenSuperInterface(
    coutputs: CompilerOutputs,
    placeholderedSuperInterfaceTT: InterfaceTT,
    implTemplata: ImplTemplata):
  Unit = {
    val parentInterfaceTemplateFullName =
      TemplataCompiler.getInterfaceTemplate(placeholderedSuperInterfaceTT.fullName)

    val inferencesFromPlaceholderedSuperInterface =
      solveImpl(
        coutputs,
        Vector(InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(placeholderedSuperInterfaceTT))),
        implTemplata)
    val subCitizenFromPlaceholderedParentInterface =
      inferencesFromPlaceholderedSuperInterface(implTemplata.impl.structKindRune.rune) match {
        case KindTemplata(cit : ICitizenTT) => cit
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case other => throw CompileErrorExceptionT(NonCitizenCantImpl(implTemplata.impl.range, other))
      }

    val subCitizenTemplateFullName =
      TemplataCompiler.getCitizenTemplate(subCitizenFromPlaceholderedParentInterface.fullName)
    val subCitizenDefinition =
      coutputs.lookupCitizen(subCitizenTemplateFullName)

    val inferencesFromPlaceholderedSubCitizen =
      solveImpl(
        coutputs,
        Vector(InitialKnown(implTemplata.impl.structKindRune, KindTemplata(subCitizenDefinition.placeholderedCitizen))),
        implTemplata)
    val parentInterfaceFromPlaceholderedSubCitizen =
      inferencesFromPlaceholderedSubCitizen(implTemplata.impl.interfaceKindRune.rune) match {
        case KindTemplata(interfaceTT @ InterfaceTT(_)) => interfaceTT
        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
        case other => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
      }

    val implT =
      interner.intern(
        ImplT(
          subCitizenTemplateFullName,
          parentInterfaceFromPlaceholderedSubCitizen,
          parentInterfaceTemplateFullName,
          subCitizenFromPlaceholderedParentInterface))
    // There may be a collision here but it's fine as this call will deduplicate. See CIFBD.
    coutputs.addImpl(implT)
  }

  def compileImpl(coutputs: CompilerOutputs, implTemplata: ImplTemplata): Unit = {
    val ImplTemplata(parentEnv, implA) = implTemplata

    val implTemplateFullName =
      parentEnv.fullName.addStep(interner.intern(ImplTemplateDeclareNameT(implA.range.begin)))

    val implOuterEnv =
      CitizenEnvironment(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateFullName,
        implTemplateFullName,
        TemplatasStore(implTemplateFullName, Map(), Map()))

    // First, figure out what citizen is implementing.
    val subCitizenImpreciseName = RuleScout.getRuneKindTemplate(implA.rules, implA.structKindRune.rune)
    val subCitizenTemplata =
      implOuterEnv.lookupNearestWithImpreciseName(subCitizenImpreciseName, Set(TemplataLookupContext)) match {
        case None => throw CompileErrorExceptionT(ImplSubCitizenNotFound(implA.range, subCitizenImpreciseName))
        case Some(it @ CitizenTemplata(_, _)) => it
        case Some(other) => throw CompileErrorExceptionT(NonCitizenCantImpl(implA.range, other))
      }
    val subCitizenTemplateFullName = templataCompiler.resolveCitizenTemplate(subCitizenTemplata)
    val subCitizenDefinition = coutputs.lookupCitizen(subCitizenTemplateFullName)
    val subCitizenPlaceholders =
      subCitizenDefinition.genericParamTypes.zipWithIndex.map({ case (tyype, index) =>
        templataCompiler.createPlaceholder(coutputs, implOuterEnv, implTemplateFullName, index, tyype)
      })
    val placeholderedSubCitizenTT =
      structCompiler.resolveCitizen(coutputs, implOuterEnv, implA.range, subCitizenTemplata, subCitizenPlaceholders)


    // Now, figure out what interface is being implemented.
    val superInterfaceImpreciseName = RuleScout.getRuneKindTemplate(implA.rules, implA.interfaceKindRune.rune)
    val superInterfaceTemplata =
      implOuterEnv.lookupNearestWithImpreciseName(superInterfaceImpreciseName, Set(TemplataLookupContext)) match {
        case None => throw CompileErrorExceptionT(ImplSuperInterfaceNotFound(implA.range, superInterfaceImpreciseName))
        case Some(it @ InterfaceTemplata(_, _)) => it
        case Some(other) => throw CompileErrorExceptionT(CantImplNonInterface(implA.range, other))
      }
    val superInterfaceTemplateFullName = templataCompiler.resolveCitizenTemplate(superInterfaceTemplata)
    val superInterfaceDefinition = coutputs.lookupCitizen(superInterfaceTemplateFullName)
    val superInterfacePlaceholders =
      superInterfaceDefinition.genericParamTypes.zipWithIndex.map({ case (tyype, index) =>
        val placeholderNameT = implTemplateFullName.addStep(PlaceholderNameT(PlaceholderTemplateNameT(index)))
        templataCompiler.createPlaceholder(coutputs, implOuterEnv, implTemplateFullName, index, tyype)
      })
    val placeholderedSuperInterfaceTT =
      structCompiler.resolveInterface(coutputs, implOuterEnv, implA.range, superInterfaceTemplata, superInterfacePlaceholders)

    // Now compile it from the sub citizen's perspective.
    compileImplGivenSubCitizen(coutputs, placeholderedSubCitizenTT, implTemplata)
    // Now compile it from the super interface's perspective.
    compileImplGivenSuperInterface(coutputs, placeholderedSuperInterfaceTT, implTemplata)
  }
//
//  def compileParentImplsForSubCitizen(
//    coutputs: CompilerOutputs,
//    subCitizenDefinition: CitizenDefinitionT):
//  Unit = {
//    Profiler.frame(() => {
//      val subCitizenTemplateFullName = subCitizenDefinition.templateName
//      val subCitizenEnv = coutputs.getEnvForTemplate(subCitizenTemplateFullName)
//      // See INSHN, the imprecise name for an impl is the wrapped imprecise name of its struct template.
//      val needleImplTemplateFullName = interner.intern(ImplTemplateSubNameT(subCitizenTemplateFullName))
//      val implTemplates =
//        subCitizenEnv.lookupAllWithName(needleImplTemplateFullName, Set(TemplataLookupContext))
//      implTemplates.foreach({
//        case it @ ImplTemplata(_, _) => {
//          compileImplGivenSubCitizen(coutputs, subCitizenDefinition, it)
//        }
//        case other => vwat(other)
//      })
//    })
//  }
//
//  def compileChildImplsForParentInterface(
//    coutputs: CompilerOutputs,
//    parentInterfaceDefinition: InterfaceDefinitionT):
//  Unit = {
//    Profiler.frame(() => {
//      val parentInterfaceTemplateFullName = parentInterfaceDefinition.templateName
//      val parentInterfaceEnv = coutputs.getEnvForTemplate(parentInterfaceTemplateFullName)
//      // See INSHN, the imprecise name for an impl is the wrapped imprecise name of its struct template.
//      val needleImplTemplateFullName = interner.intern(ImplTemplateSuperNameT(parentInterfaceTemplateFullName))
//      val implTemplates =
//        parentInterfaceEnv.lookupAllWithName(needleImplTemplateFullName, Set(TemplataLookupContext))
//      implTemplates.foreach({
//        case impl @ ImplTemplata(_, _) => {
//          compileImplGivenSuperInterface(coutputs, parentInterfaceDefinition, impl)
//        }
//        case other => vwat(other)
//      })
//    })
//  }

//  // Doesn't include self
//  def compileGetAncestorInterfaces(
//    coutputs: CompilerOutputs,
//    descendantCitizenRef: ICitizenTT):
//  (Map[InterfaceTT, ImplTemplateNameT]) = {
//    Profiler.frame(() => {
//      val parentInterfacesAndImpls =
//        compileGetParentInterfaces(coutputs, descendantCitizenRef)
//
//      // Make a map that contains all the parent interfaces, with distance 1
//      val foundSoFar =
//        parentInterfacesAndImpls.map({ case (interfaceRef, impl) => (interfaceRef, impl) }).toMap
//
//      compileGetAncestorInterfacesInner(
//        coutputs,
//        foundSoFar,
//        parentInterfacesAndImpls.toMap)
//    })
//  }
//
//  private def compileGetAncestorInterfacesInner(
//    coutputs: CompilerOutputs,
//    // This is so we can know what we've already searched.
//    nearestDistanceByInterfaceRef: Map[InterfaceTT, ImplTemplateNameT],
//    // These are the interfaces that are *exactly* currentDistance away.
//    // We will do our searching from here.
//    interfacesAtCurrentDistance: Map[InterfaceTT, ImplTemplateNameT]):
//  (Map[InterfaceTT, ImplTemplateNameT]) = {
//    val interfacesAtNextDistance =
//      interfacesAtCurrentDistance.foldLeft((Map[InterfaceTT, ImplTemplateNameT]()))({
//        case ((previousAncestorInterfaceRefs), (parentInterfaceRef, parentImpl)) => {
//          val parentAncestorInterfaceRefs =
//            compileGetParentInterfaces(coutputs, parentInterfaceRef)
//          (previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
//        }
//      })
//
//    // Discard the ones that have already been found; they're actually at
//    // a closer distance.
//    val newlyFoundInterfaces =
//    interfacesAtNextDistance.keySet
//      .diff(nearestDistanceByInterfaceRef.keySet)
//      .toVector
//      .map(key => (key -> interfacesAtNextDistance(key)))
//      .toMap
//
//    if (newlyFoundInterfaces.isEmpty) {
//      (nearestDistanceByInterfaceRef)
//    } else {
//      // Combine the previously found ones with the newly found ones.
//      val newNearestDistanceByInterfaceRef =
//        nearestDistanceByInterfaceRef ++ newlyFoundInterfaces.toMap
//
//      compileGetAncestorInterfacesInner(
//        coutputs,
//        newNearestDistanceByInterfaceRef,
//        newlyFoundInterfaces)
//    }
//  }

  def getParents(
    coutputs: CompilerOutputs,
    subCitizenTT: ICitizenTT):
  Array[InterfaceTT] = {
    val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName)
    coutputs
      .getParentImplsForSubCitizenTemplate(subCitizenTemplateFullName)
      .map({ case ImplT(_, parentInterfaceFromPlaceholderedSubCitizen, _, _) =>
        val substituter =
          TemplataCompiler.getPlaceholderSubstituter(interner, subCitizenTT.fullName)
        substituter.substituteForInterface(parentInterfaceFromPlaceholderedSubCitizen)
      }).toArray
  }

  def isParent(
    coutputs: CompilerOutputs,
    subCitizenTT: ICitizenTT,
    superInterfaceTT: InterfaceTT):
  Boolean = {
    val needleSubCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName)
    val needleSuperInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(superInterfaceTT.fullName)
    coutputs.getParentImplsForSubCitizenTemplate(needleSubCitizenTemplateFullName)
      .foreach({ case ImplT(_, _, superInterfaceTemplateName, subCitizenFromPlaceholderedParentInterface) =>
        if (superInterfaceTemplateName == needleSuperInterfaceTemplateFullName) {
          return true
        }
      })
    false
  }

  def isDescendant(
    coutputs: CompilerOutputs,
    subCitizenTT: ICitizenTT):
  Boolean = {
    val needleSubCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName)
    coutputs.getParentImplsForSubCitizenTemplate(needleSubCitizenTemplateFullName).nonEmpty
  }
}
