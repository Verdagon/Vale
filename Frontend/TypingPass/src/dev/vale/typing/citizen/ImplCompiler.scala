package dev.vale.typing.citizen

import dev.vale.highertyping.ImplA
import dev.vale.postparsing.{IRuneS, ITemplataType, ImplImpreciseNameS, ImplSubCitizenImpreciseNameS}
import dev.vale.postparsing.rules.{Equivalencies, IRulexSR, RuleScout}
import dev.vale.solver.{IIncompleteOrFailedSolve, SolverErrorHumanizer}
import dev.vale.typing.OverloadResolver.InferFailure
import dev.vale.typing.env.{ExpressionLookupContext, TemplataLookupContext, TemplatasStore}
import dev.vale.typing._
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, Result, U, postparsing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast.{CitizenDefinitionT, ImplT, InterfaceDefinitionT}
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionFailure
import dev.vale.typing.infer.ITypingPassSolverError

sealed trait IsParentResult
case class IsParent(
  conclusions: Map[IRuneS, ITemplata[ITemplataType]]
) extends IsParentResult
case class IsntParent(
  candidates: Vector[IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]]
) extends IsParentResult

class ImplCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    structCompiler: StructCompiler,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler) {

  // We don't have an isAncestor call, see REMUIDDA.

  private def solveImplForCall(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplTemplata,
    isRootSolve: Boolean):
  Result[
      Map[IRuneS, ITemplata[ITemplataType]],
      IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val ImplTemplata(parentEnv, impl) = implTemplata
    val ImplA(
      range,
      name,
      identifyingRunes,
      rules,
      runeToType,
      structKindRune,
      subCitizenImpreciseName,
      interfaceKindRune,
      superInterfaceImpreciseName
    ) = impl

    val implTemplateFullName =
      parentEnv.fullName.addStep(interner.intern(ImplTemplateDeclareNameT(range.begin)))

    val outerEnv =
      CitizenEnvironment(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateFullName,
        implTemplateFullName,
        TemplatasStore(implTemplateFullName, Map(), Map()))

    // Remember, impls can have rules too, such as:
    //   impl<T> Opt<T> for Some<T> where func drop(T)void;
    // so we do need to filter them out when compiling.
    val definitionRules = rules.filter(InferCompiler.includeRuleInCallSiteSolve)

    val result =
      inferCompiler.solveComplete(
        InferEnv(
          // This is callingEnv because we might be coming from an abstraction function that's trying
          // to evaluate an override.
          callingEnv,
          range :: parentRanges,
          outerEnv),
        coutputs, definitionRules, runeToType, range :: parentRanges, initialKnowns, Vector(), true, isRootSolve, false)
    //    val inferences =
    //      result match {
    //        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(range, e))
    //        case Ok(inferences) => inferences
    //      }
    //    inferences
    result
  }

  private def solveImplForDefine(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplTemplata,
    verifyConclusions: Boolean,
    isRootSolve: Boolean):
  Result[
    Map[IRuneS, ITemplata[ITemplataType]],
    IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val ImplTemplata(parentEnv, impl) = implTemplata
    val ImplA(
    range,
    name,
    identifyingRunes,
    rules,
    runeToType,
    structKindRune,
    subCitizenImpreciseName,
    interfaceKindRune,
    superInterfaceImpreciseName
    ) = impl

    val implTemplateFullName =
      parentEnv.fullName.addStep(interner.intern(ImplTemplateDeclareNameT(range.begin)))

    val outerEnv =
      CitizenEnvironment(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateFullName,
        implTemplateFullName,
        TemplatasStore(implTemplateFullName, Map(), Map()))

    // Remember, impls can have rules too, such as:
    //   impl<T> Opt<T> for Some<T> where func drop(T)void;
    // so we do need to filter them out when compiling.
    val definitionRules = rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    val result =
      inferCompiler.solveComplete(
        InferEnv(
          // This is callingEnv because we might be coming from an abstraction function that's trying
          // to evaluate an override.
          callingEnv,
          range :: parentRanges,
          outerEnv),
        coutputs, definitionRules, runeToType, range :: parentRanges, initialKnowns, Vector(), true, isRootSolve, false)
    //    val inferences =
    //      result match {
    //        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(range, e))
    //        case Ok(inferences) => inferences
    //      }
    //    inferences
    result
  }

  //  private def compileImplGivenSubCitizen(
  //    coutputs: CompilerOutputs,
  //    placeholderedSubCitizenTT: ICitizenTT,
  //    implTemplata: ImplTemplata):
  //  Unit = {
  //    val subCitizenTemplateFullName =
  //      TemplataCompiler.getCitizenTemplate(placeholderedSubCitizenTT.fullName)
  //
  //    val inferencesFromPlaceholderedSubCitizen =
  //      solveImpl(
  //        coutputs,
  //        Vector(InitialKnown(implTemplata.impl.subCitizenRune, KindTemplata(placeholderedSubCitizenTT))),
  //        implTemplata)
  //    val parentInterfaceFromPlaceholderedSubCitizen =
  //      inferencesFromPlaceholderedSubCitizen(implTemplata.impl.interfaceKindRune.rune) match {
  //        case KindTemplata(interfaceTT @ InterfaceTT(_)) => interfaceTT
  //        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
  //        case other => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
  //      }
  //    val parentInterfaceTemplateFullName =
  //      TemplataCompiler.getInterfaceTemplate(parentInterfaceFromPlaceholderedSubCitizen.fullName)
  ////    val parentInterfaceDefinition =
  ////      coutputs.lookupInterface(parentInterfaceTemplateFullName)
  ////    val inferencesFromPlaceholderedSuperInterface =
  ////      solveImpl(
  ////        coutputs,
  ////        Vector(InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(parentInterfaceDefinition.placeholderedInterface))),
  ////        implTemplata)
  ////    val subCitizenFromPlaceholderedParentInterface =
  ////      inferencesFromPlaceholderedSuperInterface(implTemplata.impl.subCitizenRune.rune) match {
  ////        case KindTemplata(cit : ICitizenTT) => cit
  ////        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
  ////        case other => throw CompileErrorExceptionT(NonCitizenCantImpl(implTemplata.impl.range, other))
  ////      }
  //
  //    val implT =
  //      interner.intern(
  //        ImplT(
  //          subCitizenTemplateFullName,
  ////          parentInterfaceFromPlaceholderedSubCitizen,
  //          parentInterfaceTemplateFullName))
  ////          subCitizenFromPlaceholderedParentInterface))
  //    // There may be a collision here but it's fine as this call will deduplicate. See CIFBD.
  //    coutputs.addImpl(implT)
  //  }
  //
  //  private def compileImplGivenSuperInterface(
  //    coutputs: CompilerOutputs,
  //    placeholderedSuperInterfaceTT: InterfaceTT,
  //    implTemplata: ImplTemplata):
  //  Unit = {
  //    val parentInterfaceTemplateFullName =
  //      TemplataCompiler.getInterfaceTemplate(placeholderedSuperInterfaceTT.fullName)
  //
  //    val inferencesFromPlaceholderedSuperInterface =
  //      solveImpl(
  //        coutputs,
  //        Vector(InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(placeholderedSuperInterfaceTT))),
  //        implTemplata)
  //    val subCitizenFromPlaceholderedParentInterface =
  //      inferencesFromPlaceholderedSuperInterface(implTemplata.impl.subCitizenRune.rune) match {
  //        case KindTemplata(cit : ICitizenTT) => cit
  //        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
  //        case other => throw CompileErrorExceptionT(NonCitizenCantImpl(implTemplata.impl.range, other))
  //      }
  //
  //    val subCitizenTemplateFullName =
  //      TemplataCompiler.getCitizenTemplate(subCitizenFromPlaceholderedParentInterface.fullName)
  //    val subCitizenDefinition =
  //      coutputs.lookupCitizen(subCitizenTemplateFullName)
  //
  //    val inferencesFromPlaceholderedSubCitizen =
  //      solveImpl(
  //        coutputs,
  //        Vector(InitialKnown(implTemplata.impl.subCitizenRune, KindTemplata(subCitizenDefinition.placeholderedCitizen))),
  //        implTemplata)
  //    val parentInterfaceFromPlaceholderedSubCitizen =
  //      inferencesFromPlaceholderedSubCitizen(implTemplata.impl.interfaceKindRune.rune) match {
  //        case KindTemplata(interfaceTT @ InterfaceTT(_)) => interfaceTT
  //        case InterfaceTemplata(_, _) => vcurious() // shouldnt the impl solver produce a kind? or do we have to coerce / resolveInterface?
  //        case other => throw CompileErrorExceptionT(CantImplNonInterface(implTemplata.impl.range, other))
  //      }
  //
  //    val implT =
  //      interner.intern(
  //        ImplT(
  //          subCitizenTemplateFullName,
  //          parentInterfaceFromPlaceholderedSubCitizen,
  //          parentInterfaceTemplateFullName,
  //          subCitizenFromPlaceholderedParentInterface))
  //    // There may be a collision here but it's fine as this call will deduplicate. See CIFBD.
  //    coutputs.addImpl(implT)
  //  }

  // This will just figure out the struct template and interface template,
  // so we can add it to the temputs.
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

    val implPlaceholders =
      implA.identifyingRunes.zipWithIndex.map({ case (rune, index) =>
        val placeholder =
          templataCompiler.createPlaceholder(
            coutputs, implOuterEnv, implTemplateFullName, rune, index, implA.runeToType, true)
        InitialKnown(rune.rune, placeholder)
      })

    val inferences =
      solveImplForDefine(coutputs, List(implA.range), implOuterEnv, implPlaceholders, implTemplata, true, true) match {
        case Ok(i) => i
        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(List(implA.range), e))
      }

    val subCitizen =
      inferences.get(implA.subCitizenRune.rune) match {
        case None => vwat()
        case Some(KindTemplata(s: ICitizenTT)) => s
        case _ => vwat()
      }
    val subCitizenTemplateFullName =
      TemplataCompiler.getCitizenTemplate(subCitizen.fullName)

    val superInterface =
      inferences.get(implA.interfaceKindRune.rune) match {
        case None => vwat()
        case Some(KindTemplata(i@InterfaceTT(_))) => i
        case Some(other) => throw CompileErrorExceptionT(CantImplNonInterface(List(implA.range), other))
      }
    val superInterfaceTemplateFullName =
      TemplataCompiler.getInterfaceTemplate(superInterface.fullName)


    val implT =
      interner.intern(
        ImplT(
          implTemplata,
          implOuterEnv,
          subCitizenTemplateFullName,
          subCitizen,
          superInterface,
          superInterfaceTemplateFullName))
    coutputs.declareType(implTemplateFullName)
    coutputs.declareTypeOuterEnv(implTemplateFullName, implOuterEnv)
    //          subCitizenFromPlaceholderedParentInterface))
    // There may be a collision here but it's fine as this call will deduplicate. See CIFBD.
    coutputs.addImpl(implT)
  }
  //    // First, figure out what citizen is implementing.
  //    val subCitizenImpreciseName = RuleScout.getRuneKindTemplate(implA.rules, implA.structKindRune.rune)
  //    val subCitizenTemplata =
  //      implOuterEnv.lookupNearestWithImpreciseName(subCitizenImpreciseName, Set(TemplataLookupContext)) match {
  //        case None => throw CompileErrorExceptionT(ImplSubCitizenNotFound(implA.range, subCitizenImpreciseName))
  //        case Some(it @ CitizenTemplata(_, _)) => it
  //        case Some(other) => throw CompileErrorExceptionT(NonCitizenCantImpl(implA.range, other))
  //      }
  //    val subCitizenTemplateFullName = templataCompiler.resolveCitizenTemplate(subCitizenTemplata)
  //    val subCitizenDefinition = coutputs.lookupCitizen(subCitizenTemplateFullName)
  //    val subCitizenPlaceholders =
  //      subCitizenDefinition.genericParamTypes.zipWithIndex.map({ case (tyype, index) =>
  //        templataCompiler.createPlaceholder(coutputs, implOuterEnv, implTemplateFullName, index, tyype)
  //      })
  //    val placeholderedSubCitizenTT =
  //      structCompiler.resolveCitizen(coutputs, implOuterEnv, implA.range, subCitizenTemplata, subCitizenPlaceholders)
  //
  //
  //    // Now, figure out what interface is being implemented.
  //    val superInterfaceImpreciseName = RuleScout.getRuneKindTemplate(implA.rules, implA.interfaceKindRune.rune)
  //    val superInterfaceTemplata =
  //      implOuterEnv.lookupNearestWithImpreciseName(superInterfaceImpreciseName, Set(TemplataLookupContext)) match {
  //        case None => throw CompileErrorExceptionT(ImplSuperInterfaceNotFound(implA.range, superInterfaceImpreciseName))
  //        case Some(it @ InterfaceTemplata(_, _)) => it
  //        case Some(other) => throw CompileErrorExceptionT(CantImplNonInterface(implA.range, other))
  //      }
  //    val superInterfaceTemplateFullName = templataCompiler.resolveCitizenTemplate(superInterfaceTemplata)
  //    val superInterfaceDefinition = coutputs.lookupCitizen(superInterfaceTemplateFullName)
  //    val superInterfacePlaceholders =
  //      superInterfaceDefinition.genericParamTypes.zipWithIndex.map({ case (tyype, index) =>
  //        val placeholderNameT = implTemplateFullName.addStep(PlaceholderNameT(PlaceholderTemplateNameT(index)))
  //        templataCompiler.createPlaceholder(coutputs, implOuterEnv, implTemplateFullName, index, tyype)
  //      })
  //    val placeholderedSuperInterfaceTT =
  //      structCompiler.resolveInterface(coutputs, implOuterEnv, implA.range, superInterfaceTemplata, superInterfacePlaceholders)
  //
  //    // Now compile it from the sub citizen's perspective.
  //    compileImplGivenSubCitizen(coutputs, placeholderedSubCitizenTT, implTemplata)
  //    // Now compile it from the super interface's perspective.
  //    compileImplGivenSuperInterface(coutputs, placeholderedSuperInterfaceTT, implTemplata)
  //  }
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
  //
  //  def getParents(
  //    coutputs: CompilerOutputs,
  //    subCitizenTT: ICitizenTT):
  //  Array[InterfaceTT] = {
  //    val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName)
  //    coutputs
  //      .getParentImplsForSubCitizenTemplate(subCitizenTemplateFullName)
  //      .map({ case ImplT(_, parentInterfaceFromPlaceholderedSubCitizen, _, _) =>
  //        val substituter =
  //          TemplataCompiler.getPlaceholderSubstituter(interner, subCitizenTT.fullName)
  //        substituter.substituteForInterface(parentInterfaceFromPlaceholderedSubCitizen)
  //      }).toArray
  //  }
  //

  def isDescendant(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    kind: KindT,
    verifyConclusions: Boolean):
  Boolean = {
    getParents(coutputs, parentRanges, callingEnv, kind, verifyConclusions).nonEmpty
  }

  def getImplDescendantGivenParent(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    implTemplata: ImplTemplata,
    parent: InterfaceTT,
    verifyConclusions: Boolean,
    isRootSolve: Boolean):
  Result[ICitizenTT, IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val initialKnowns =
      Vector(
        InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplata(parent)))
    val conclusions =
      solveImplForCall(coutputs, parentRanges, callingEnv, initialKnowns, implTemplata, isRootSolve) match {
        case Ok(c) => c
        case Err(e) => return Err(e)
      }
    val parentTT = conclusions.get(implTemplata.impl.subCitizenRune.rune)
    vassertSome(parentTT) match {
      case KindTemplata(i : ICitizenTT) => Ok(i)
      case _ => vwat()
    }
  }

  def getImplParentGivenSubCitizen(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    implTemplata: ImplTemplata,
    child: ICitizenTT,
    verifyConclusions: Boolean):
  Result[InterfaceTT, IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val initialKnowns =
      Vector(
        InitialKnown(implTemplata.impl.subCitizenRune, KindTemplata(child)))
    val childEnv =
      coutputs.getOuterEnvForType(
        TemplataCompiler.getCitizenTemplate(child.fullName))
    val conclusions =
      solveImplForCall(coutputs, parentRanges, callingEnv, initialKnowns, implTemplata, false) match {
        case Ok(c) => c
        case Err(e) => return Err(e)
      }
    val parentTT = conclusions.get(implTemplata.impl.interfaceKindRune.rune)
    vassertSome(parentTT) match {
      case KindTemplata(i @ InterfaceTT(_)) => Ok(i)
      case _ => vwat()
    }
  }

  def getParents(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callingEnv: IEnvironment,
    kind: KindT,
    verifyConclusions: Boolean):
  Array[InterfaceTT] = {
    val subCitizenTT =
      kind match {
        case c : ICitizenTT => c
        case _ => return Array()
      }

    val subCitizenImpreciseName =
      TemplatasStore.getImpreciseName(interner, subCitizenTT.fullName.last) match {
        case None => return Array()
        case Some(n) => n
      }
    val implImpreciseNameS =
      interner.intern(ImplSubCitizenImpreciseNameS(subCitizenImpreciseName))

    val subCitizenEnv =
      coutputs.getOuterEnvForType(TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName))

    val matching =
      subCitizenEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext))

    val implsWithDuplicates =
      matching.map({
        case it@ImplTemplata(_, _) => it
        case _ => vwat()
      })
    val impls =
      implsWithDuplicates.groupBy(_.impl.range).map(_._2.head)

    impls.flatMap(impl => {
      getImplParentGivenSubCitizen(coutputs, parentRanges, callingEnv, impl, subCitizenTT, verifyConclusions) match {
        case Ok(x) => List(x)
        case Err(_) => List()
      }
    }).toArray
  }

  def isParent(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    subCitizenTT: ICitizenTT,
    superInterfaceTT: InterfaceTT, verifyConclusions: Boolean):
  IsParentResult = {
    val superInterfaceImpreciseName =
      TemplatasStore.getImpreciseName(interner, superInterfaceTT.fullName.last) match {
        case None => return IsntParent(Vector())
        case Some(n) => n
      }
    val subCitizenImpreciseName =
      TemplatasStore.getImpreciseName(interner, subCitizenTT.fullName.last) match {
        case None => return IsntParent(Vector())
        case Some(n) => n
      }
    val implImpreciseNameS =
      interner.intern(ImplImpreciseNameS(superInterfaceImpreciseName, subCitizenImpreciseName))

    val subCitizenEnv =
      coutputs.getOuterEnvForType(TemplataCompiler.getCitizenTemplate(subCitizenTT.fullName))
    val superInterfaceEnv =
      coutputs.getOuterEnvForType(TemplataCompiler.getInterfaceTemplate(superInterfaceTT.fullName))

    val matching =
      subCitizenEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext)) ++
        superInterfaceEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext))

    val implsWithDuplicates =
      matching.map({
        case it@ImplTemplata(_, _) => it
        case _ => vwat()
      })
    val impls =
      implsWithDuplicates.groupBy(_.impl.range).map(_._2.head)

    val results =
      impls.map(impl => {
        val initialKnowns =
          Vector(
            InitialKnown(impl.impl.subCitizenRune, KindTemplata(subCitizenTT)),
            InitialKnown(impl.impl.interfaceKindRune, KindTemplata(superInterfaceTT)))
        solveImplForCall(coutputs, parentRanges, superInterfaceEnv, initialKnowns, impl, false)
      })
    val (oks, errs) = Result.split(results)
    vcurious(oks.size <= 1)
    oks.headOption match {
      case Some(ok) => IsParent(ok)
      case None => IsntParent(errs.toVector)
    }
  }
}
