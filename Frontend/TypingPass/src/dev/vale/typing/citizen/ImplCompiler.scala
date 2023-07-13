package dev.vale.typing.citizen

import dev.vale.highertyping.ImplA
import dev.vale.postparsing.{IRuneS, ITemplataType, ImplImpreciseNameS, ImplSubCitizenImpreciseNameS, ImplTemplataType, LocationInDenizen}
import dev.vale.postparsing.rules.{Equivalencies, IRulexSR, RuleScout}
import dev.vale.solver.{IIncompleteOrFailedSolve, SolverErrorHumanizer}
import dev.vale.typing.OverloadResolver.InferFailure
import dev.vale.typing.env.{ExpressionLookupContext, TemplataLookupContext, TemplatasStore}
import dev.vale.typing._
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Accumulator, Err, Interner, Ok, Profiler, RangeS, Result, U, postparsing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast.{CitizenDefinitionT, ImplT, InterfaceDefinitionT}
import dev.vale.typing.env._
import dev.vale.typing.function._
import dev.vale.typing.infer.ITypingPassSolverError

import scala.collection.immutable.Set

sealed trait IsParentResult
case class IsParent(
  templata: ITemplataT[ImplTemplataType],
  conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
  implId: IdT[IImplNameT]
) extends IsParentResult
case class IsntParent(
  candidates: Vector[IIncompleteOrFailedCompilerSolve]
) extends IsParentResult

class ImplCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    nameTranslator: NameTranslator,
    structCompiler: StructCompiler,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler) {

  // We don't have an isAncestor call, see REMUIDDA.

  def solveImplForCall(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplDefinitionTemplataT,
    isRootSolve: Boolean,
    verifyConclusions: Boolean):
  ICompilerSolverOutcome = {
    val ImplDefinitionTemplataT(parentEnv, impl) = implTemplata
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

    val implTemplateId =
      parentEnv.id.addStep(nameTranslator.translateImplName(name))

    val outerEnv =
      CitizenEnvironmentT(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateId,
        implTemplateId,
        TemplatasStore(implTemplateId, Map(), Map()))

    // Remember, impls can have rules too, such as:
    //   impl<T> Opt<T> for Some<T> where func drop(T)void;
    // so we do need to filter them out when compiling.
    val definitionRules = rules.filter(InferCompiler.includeRuleInCallSiteSolve)

    val envs =
      InferEnv(
        // This is callingEnv because we might be coming from an abstract function that's trying
        // to evaluate an override.
        callingEnv,
        range :: parentRanges,
        callLocation,
        outerEnv,
        vimpl())
    val solver =
      inferCompiler.makeSolver(
        envs, coutputs, definitionRules, runeToType, range :: parentRanges, initialKnowns, Vector())

    inferCompiler.continue(envs, coutputs, solver) match {
      case Ok(()) =>
      case Err(e) => return e
    }

    val result =
      inferCompiler.interpretResults(
        envs,
        coutputs,
        range :: parentRanges,
        callLocation,
        runeToType,
        definitionRules,
        verifyConclusions,
        isRootSolve,
        // We include the reachable bounds for the struct rune. Those are bounds that this impl will
        // have to satisfy when it calls the interface.
        Vector(structKindRune.rune),
        solver)
    result
  }

  private def solveImplForDefine(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    initialKnowns: Vector[InitialKnown],
    implTemplata: ImplDefinitionTemplataT,
    verifyConclusions: Boolean,
    isRootSolve: Boolean):
  Result[CompleteCompilerSolve, IIncompleteOrFailedCompilerSolve] = {
    val ImplDefinitionTemplataT(parentEnv, impl) = implTemplata
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

    val implTemplateId =
      parentEnv.id.addStep(nameTranslator.translateImplName(name))

    val outerEnv =
      CitizenEnvironmentT(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateId,
        implTemplateId,
        TemplatasStore(implTemplateId, Map(), Map()))

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
          callLocation,
          outerEnv,
          vimpl()),
        coutputs,
        definitionRules,
        runeToType,
        range :: parentRanges,
        callLocation,
        initialKnowns,
        Vector(),
        true,
        isRootSolve,
        // We include reachable bounds for the struct so we don't have to re-specify all its bounds in the impl.
        Vector(structKindRune.rune))
    result
  }

  // This will just figure out the struct template and interface template,
  // so we can add it to the temputs.
  def compileImpl(coutputs: CompilerOutputs, callLocation: LocationInDenizen, implTemplata: ImplDefinitionTemplataT): Unit = {
    val ImplDefinitionTemplataT(parentEnv, implA) = implTemplata

    val implTemplateId =
      parentEnv.id.addStep(
        nameTranslator.translateImplName(implA.name))
    val defaultRegion = vimpl()

    val implOuterEnv =
      CitizenEnvironmentT(
        parentEnv.globalEnv,
        parentEnv,
        implTemplateId,
        implTemplateId,
        TemplatasStore(implTemplateId, Map(), Map()))
    coutputs.declareType(implTemplateId)
    coutputs.declareTypeOuterEnv(implTemplateId, implOuterEnv)

    // We might one day need to incrementally solve and add placeholders here like we do for
    // functions and structs, see IRAGP.
    val implPlaceholders =
      implA.genericParams.zipWithIndex.map({ case (rune, index) =>
        val placeholder =
          templataCompiler.createPlaceholder(
            coutputs,
            implOuterEnv,
            implTemplateId,
            rune,
            index,
            implA.runeToType,
            vimpl(),
            true)
        InitialKnown(rune.rune, placeholder)
      })

    val CompleteCompilerSolve(_, inferences, runeToFunctionBound1, reachableBoundsFromSubCitizen) =
      solveImplForDefine(coutputs, List(implA.range), callLocation, implOuterEnv, implPlaceholders, implTemplata, true, true) match {
        case Ok(i) => i
        case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(List(implA.range), e))
      }

    val subCitizen =
      inferences.get(implA.subCitizenRune.rune) match {
        case None => vwat()
        case Some(KindTemplataT(s: ICitizenTT)) => s
        case _ => vwat()
      }
    val subCitizenTemplateId =
      TemplataCompiler.getCitizenTemplate(subCitizen.id)

    val superInterface =
      inferences.get(implA.interfaceKindRune.rune) match {
        case None => vwat()
        case Some(KindTemplataT(i@InterfaceTT(_))) => i
        case Some(other) => throw CompileErrorExceptionT(CantImplNonInterface(List(implA.range), other))
      }
    val superInterfaceTemplateId =
      TemplataCompiler.getInterfaceTemplate(superInterface.id)


    val templateArgs = implA.genericParams.map(_.rune.rune).map(inferences)
    val instantiatedId = assembleImplName(implTemplateId, templateArgs, subCitizen)

    val implInnerEnv =
      GeneralEnvironmentT.childOf(
        interner,
        implOuterEnv,
        instantiatedId,
        newEntriesList = reachableBoundsFromSubCitizen.zipWithIndex.map({ case (templata, index) =>
          interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(templata)
        }).toVector ++
        inferences.map({ case (nameS, templata) =>
          interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
        }).toVector)
    val runeToNeededFunctionBound = TemplataCompiler.assembleRuneToFunctionBound(implInnerEnv.templatas)
    val runeToNeededImplBound = TemplataCompiler.assembleRuneToImplBound(implInnerEnv.templatas)
//    vcurious(runeToFunctionBound1 == runeToNeededFunctionBound) // which do we want?

    val runeIndexToIndependence =
      calculateRunesIndependence(coutputs, callLocation, implTemplata, implOuterEnv, superInterface)

    val implT =
      interner.intern(
        ImplT(
          implTemplata,
          implOuterEnv,
          instantiatedId,
          implTemplateId,
          subCitizenTemplateId,
          subCitizen,
          superInterface,
          superInterfaceTemplateId,
          runeToNeededFunctionBound,
          runeToNeededImplBound,
          runeIndexToIndependence.toVector,
          reachableBoundsFromSubCitizen.map(_.prototype)))
    coutputs.declareType(implTemplateId)
    coutputs.declareTypeOuterEnv(implTemplateId, implOuterEnv)
    coutputs.declareTypeInnerEnv(implTemplateId, implInnerEnv)
    coutputs.addImpl(implT)
  }

  def calculateRunesIndependence(
    coutputs: CompilerOutputs,
    callLocation: LocationInDenizen,
    implTemplata: ImplDefinitionTemplataT,
    implOuterEnv: IInDenizenEnvironmentT,
    interface: InterfaceTT,
  ): Vector[Boolean] = {

    // Now we're going to figure out the <ZZ> for the eg Milano case.
    val (partialCaseConclusionsFromSuperInterface, _) =
      solveImplForCall(
        coutputs,
        List(implTemplata.impl.range),
        callLocation,
        implOuterEnv,
        Vector(
          InitialKnown(
            implTemplata.impl.interfaceKindRune,
            // We may be feeding in something interesting like IObserver<Opt<T>> here should be fine,
            // the impl will receive it and match it to its own unknown runes appropriately.
            KindTemplataT(interface))),
        implTemplata,
        false,
        // Don't verify conclusions, because this will likely be a partial solve, which means we
        // might not even be able to solve the struct, which means we can't pull in any declared
        // function bounds that come from them. We'll check them later.
        false) match {
        case CompleteCompilerSolve(_, conclusions, _, reachableBoundsFromSubCitizen) => (conclusions, reachableBoundsFromSubCitizen)
        case IncompleteCompilerSolve(_, _, _, incompleteConclusions) => (incompleteConclusions, Vector[ITemplataT[ITemplataType]]())
        case fcs @ FailedCompilerSolve(_, _, _) => {
          throw CompileErrorExceptionT(CouldntEvaluatImpl(List(implTemplata.impl.range), fcs))
        }
      }
    // These will be anything that wasn't already determined by the incoming interface.
    // These are the "independent" generic params, like the <ZZ> in Milano.
    // No particular reason they're ordered, it just feels appropriate to keep them in the same
    // order they appeared in the impl.
    val runeToIndependence =
      implTemplata.impl.genericParams.map(_.rune.rune)
        .map(rune => !partialCaseConclusionsFromSuperInterface.contains(rune))

    runeToIndependence
  }

  def assembleImplName(
    templateName: IdT[IImplTemplateNameT],
    templateArgs: Vector[ITemplataT[ITemplataType]],
    subCitizen: ICitizenTT):
  IdT[IImplNameT] = {
    templateName.copy(
      localName = templateName.localName.makeImplName(interner, templateArgs, subCitizen))
  }

  def isDescendant(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    kind: ISubKindTT,
    verifyConclusions: Boolean):
  Boolean = {
    getParents(coutputs, parentRanges, callLocation, callingEnv, kind, verifyConclusions).nonEmpty
  }

  def getImplDescendantGivenParent(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
      callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    implTemplata: ImplDefinitionTemplataT,
    parent: InterfaceTT,
    verifyConclusions: Boolean,
    isRootSolve: Boolean):
  Result[ICitizenTT, IIncompleteOrFailedCompilerSolve] = {
    val initialKnowns =
      Vector(
        InitialKnown(implTemplata.impl.interfaceKindRune, KindTemplataT(parent)))
    val CompleteCompilerSolve(_, conclusions, _, _) =
      solveImplForCall(coutputs, parentRanges, callLocation, callingEnv, initialKnowns, implTemplata, isRootSolve, true) match {
        case ccs @ CompleteCompilerSolve(_, _, _, _) => ccs
        case x : IIncompleteOrFailedCompilerSolve => return Err(x)
      }
    val parentTT = conclusions.get(implTemplata.impl.subCitizenRune.rune)
    vassertSome(parentTT) match {
      case KindTemplataT(i : ICitizenTT) => Ok(i)
      case _ => vwat()
    }
  }

  def getImplParentGivenSubCitizen(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
      callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    implTemplata: ImplDefinitionTemplataT,
    child: ICitizenTT,
    verifyConclusions: Boolean):
  Result[InterfaceTT, IIncompleteOrFailedCompilerSolve] = {
    val initialKnowns =
      Vector(
        InitialKnown(implTemplata.impl.subCitizenRune, KindTemplataT(child)))
    val childEnv =
      coutputs.getOuterEnvForType(
        parentRanges,
        TemplataCompiler.getCitizenTemplate(child.id))
    val CompleteCompilerSolve(_, conclusions, _, _) =
      solveImplForCall(coutputs, parentRanges, callLocation, callingEnv, initialKnowns, implTemplata, false, true) match {
        case ccs @ CompleteCompilerSolve(_, _, _, _) => ccs
        case x : IIncompleteOrFailedCompilerSolve => return Err(x)
      }
    val parentTT = conclusions.get(implTemplata.impl.interfaceKindRune.rune)
    vassertSome(parentTT) match {
      case KindTemplataT(i @ InterfaceTT(_)) => Ok(i)
      case _ => vwat()
    }
  }

  def getParents(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    callingEnv: IInDenizenEnvironmentT,
    subKind: ISubKindTT,
    verifyConclusions: Boolean):
  Vector[ISuperKindTT] = {
    val subKindId = subKind.id
    val subKindTemplateName = TemplataCompiler.getSubKindTemplate(subKindId)
    val subKindEnv = coutputs.getOuterEnvForType(parentRanges, subKindTemplateName)
    val subKindImpreciseName =
      TemplatasStore.getImpreciseName(interner, subKindId.localName) match {
        case None => return Vector()
        case Some(n) => n
      }
    val implImpreciseNameS =
      interner.intern(ImplSubCitizenImpreciseNameS(subKindImpreciseName))

    val matching =
      subKindEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext)) ++
      callingEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext))

    val implDefsWithDuplicates = new Accumulator[ImplDefinitionTemplataT]()
    val implTemplatasWithDuplicates = new Accumulator[IsaTemplataT]()

    matching.foreach({
      case it@ImplDefinitionTemplataT(_, _) => implDefsWithDuplicates.add(it)
      case it@IsaTemplataT(_, _, _, _) => implTemplatasWithDuplicates.add(it)
      case _ => vwat()
    })

    val implDefs =
      implDefsWithDuplicates.buildArray().groupBy(_.impl.range).map(_._2.head)
    val parentsFromImplDefs =
      implDefs.flatMap(impl => {
        subKind match {
          case subCitizen : ICitizenTT => {
            getImplParentGivenSubCitizen(coutputs, parentRanges, callLocation, callingEnv, impl, subCitizen, verifyConclusions) match {
              case Ok(x) => List(x)
              case Err(_) => {
                opts.debugOut("Throwing away error! TODO: Use an index or something instead.")
                List()
              }
            }
          }
          case _ => List()
        }
      }).toVector

    val parentsFromImplTemplatas =
      implTemplatasWithDuplicates
        .buildArray()
        .filter(_.subKind == subKind)
        .map(_.superKind)
        .collect({ case x : ISuperKindTT => x })
        .distinct

    parentsFromImplDefs ++ parentsFromImplTemplatas
  }

  def isParent(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    subKindTT: ISubKindTT,
    superKindTT: ISuperKindTT):
  IsParentResult = {
    val superKindImpreciseName =
      TemplatasStore.getImpreciseName(interner, superKindTT.id.localName) match {
        case None => return IsntParent(Vector())
        case Some(n) => n
      }
    val subKindImpreciseName =
      TemplatasStore.getImpreciseName(interner, subKindTT.id.localName) match {
        case None => return IsntParent(Vector())
        case Some(n) => n
      }
    val implImpreciseNameS =
      interner.intern(ImplImpreciseNameS(subKindImpreciseName, superKindImpreciseName))

    val subKindEnv =
      coutputs.getOuterEnvForType(
        parentRanges, TemplataCompiler.getSubKindTemplate(subKindTT.id))
    val superKindEnv =
      coutputs.getOuterEnvForType(
        parentRanges, TemplataCompiler.getSuperKindTemplate(superKindTT.id))

    val matching =
      callingEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext)) ++
      subKindEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext)) ++
      superKindEnv.lookupAllWithImpreciseName(implImpreciseNameS, Set(TemplataLookupContext))

    val implsDefsWithDuplicates = new Accumulator[ImplDefinitionTemplataT]()
    val implTemplatasWithDuplicatesAcc = new Accumulator[IsaTemplataT]()
    matching.foreach({
      case it@ImplDefinitionTemplataT(_, _) => implsDefsWithDuplicates.add(it)
      case it@IsaTemplataT(_, _, _, _) => implTemplatasWithDuplicatesAcc.add(it)
      case _ => vwat()
    })
    val implTemplatasWithDuplicates = implTemplatasWithDuplicatesAcc.buildArray()

    implTemplatasWithDuplicates.find(i => i.subKind == subKindTT && i.superKind == superKindTT) match {
      case Some(impl) => {
        coutputs.addInstantiationBounds(impl.implName, InstantiationBoundArgumentsT(Map(), Map()))
        return IsParent(impl, Map(), impl.implName)
      }
      case None =>
    }

    val impls =
      implsDefsWithDuplicates.buildArray().groupBy(_.impl.range).map(_._2.head)
    val results =
      impls.map(impl => {
        val initialKnowns =
          Vector(
            InitialKnown(impl.impl.subCitizenRune, KindTemplataT(subKindTT)),
            InitialKnown(impl.impl.interfaceKindRune, KindTemplataT(superKindTT)))
        solveImplForCall(coutputs, parentRanges, callLocation, callingEnv, initialKnowns, impl, false, true) match {
          case ccs @ CompleteCompilerSolve(_, _, _, _) => Ok((impl, ccs))
          case x : IIncompleteOrFailedCompilerSolve => Err(x)
        }
      })
    val (oks, errs) = Result.split(results)
    vcurious(oks.size <= 1)
    oks.headOption match {
      case Some((implTemplata, CompleteCompilerSolve(_, conclusions, runeToSuppliedFunction, reachableBoundsFromSubCitizen))) => {
        // Dont need this for anything yet
        val _ = reachableBoundsFromSubCitizen

        val templateArgs =
          implTemplata.impl.genericParams.map(_.rune.rune).map(conclusions)
        val implTemplateId =
          implTemplata.env.id.addStep(
            nameTranslator.translateImplName(implTemplata.impl.name))
        val instantiatedId = assembleImplName(implTemplateId, templateArgs, subKindTT.expectCitizen())
        coutputs.addInstantiationBounds(instantiatedId, runeToSuppliedFunction)
        IsParent(implTemplata, conclusions, instantiatedId)
      }
      case None => IsntParent(errs.toVector)
    }
  }
}
