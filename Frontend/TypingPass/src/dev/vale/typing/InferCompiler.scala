package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale._
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast._
import dev.vale.typing.citizen._
import dev.vale.typing.env._
import dev.vale.typing.function._
import dev.vale.typing.infer._
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplataT._
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable._

//ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]

sealed trait IResolveSolveOutcome {
  def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]]
}
case class CompleteResolveSolve(
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
    runeToBound: InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT],
    reachableBounds: Vector[PrototypeTemplataT[IFunctionNameT]]
) extends IResolveSolveOutcome {
  override def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]] = conclusions
}
case class CompleteDefineSolve(
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
    runeToBound: InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT],
    reachableBounds: Vector[PrototypeTemplataT[IFunctionNameT]]
) {
  // override def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]] = conclusions
}

sealed trait IIncompleteOrFailedCompilerSolve extends IResolveSolveOutcome {
  def unsolvedRules: Vector[IRulexSR]
  def unsolvedRunes: Vector[IRuneS]
  def steps: Stream[Step[IRulexSR, IRuneS, ITemplataT[ITemplataType]]]
}
case class IncompleteCompilerSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplataT[ITemplataType]]],
  unsolvedRules: Vector[IRulexSR],
  unknownRunes: Set[IRuneS],
  incompleteConclusions: Map[IRuneS, ITemplataT[ITemplataType]]
) extends IIncompleteOrFailedCompilerSolve {
  vassert(unknownRunes.nonEmpty)
  override def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]] = vfail()
  override def unsolvedRunes: Vector[IRuneS] = unknownRunes.toVector
}

case class FailedCompilerSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplataT[ITemplataType]]],
  unsolvedRules: Vector[IRulexSR],
  error: ISolverError[IRuneS, ITemplataT[ITemplataType], ITypingPassSolverError]
) extends IIncompleteOrFailedCompilerSolve {
  override def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]] = vfail()
  override def unsolvedRunes: Vector[IRuneS] = Vector()
}

sealed trait IConclusionResolveError
case class CouldntFindImplForConclusionResolve(range: List[RangeS], fail: IsntParent) extends IConclusionResolveError
case class CouldntFindKindForConclusionResolve(inner: ResolveFailure[KindT]) extends IConclusionResolveError
case class CouldntFindFunctionForConclusionResolve(range: List[RangeS], fff: FindFunctionFailure) extends IConclusionResolveError
case class ReturnTypeConflictInConclusionResolve(range: List[RangeS], expectedReturnType: CoordT, actual: PrototypeT[IFunctionNameT]) extends IConclusionResolveError

sealed trait IResolvingError
case class ResolvingSolveFailedOrIncomplete(inner: IIncompleteOrFailedCompilerSolve) extends IResolvingError
case class ResolvingResolveConclusionError(inner: IConclusionResolveError) extends IResolvingError

sealed trait IDefiningError
case class DefiningSolveFailedOrIncomplete(inner: IIncompleteOrFailedCompilerSolve) extends IDefiningError
case class DefiningResolveConclusionError(inner: IConclusionResolveError) extends IDefiningError

case class InferEnv(
  // This is the only one that matters when checking template instantiations.
  // This is also the one that the placeholders come from.
  originalCallingEnv: IInDenizenEnvironmentT,

  parentRanges: List[RangeS],
  callLocation: LocationInDenizen,

  // We look in this for everything else, such as type names like "int" etc.
  selfEnv: IEnvironmentT,


  // Sometimes these can be all equal.

  contextRegion: RegionT
)

case class InitialSend(
  senderRune: RuneUsage,
  receiverRune: RuneUsage,
  sendTemplata: ITemplataT[ITemplataType])

case class InitialKnown(
  rune: RuneUsage,
  templata: ITemplataT[ITemplataType])

trait IInferCompilerDelegate {
  def resolveStruct(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    templata: StructDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]],
    verifyConclusions: Boolean):
  IResolveOutcome[StructTT]

  def resolveInterface(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    templata: InterfaceDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]],
    verifyConclusions: Boolean):
  IResolveOutcome[InterfaceTT]

  def resolveStaticSizedArrayKind(
    coutputs: CompilerOutputs,
    mutability: ITemplataT[MutabilityTemplataType],
    variability: ITemplataT[VariabilityTemplataType],
    size: ITemplataT[IntegerTemplataType],
    element: CoordT,
    region: RegionT):
  StaticSizedArrayTT

  def resolveRuntimeSizedArrayKind(
    coutputs: CompilerOutputs,
    type2: CoordT,
    arrayMutability: ITemplataT[MutabilityTemplataType],
    region: RegionT):
  RuntimeSizedArrayTT

  def resolveFunction(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    name: StrI,
    coords: Vector[CoordT],
    contextRegion: RegionT,
    verifyConclusions: Boolean):
  Result[StampFunctionSuccess, FindFunctionFailure]

  def resolveImpl(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    subKind: ISubKindTT,
    superKind: ISuperKindTT):
  IsParentResult
}

class InferCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    infererDelegate: IInfererDelegate,
    delegate: IInferCompilerDelegate) {
  val compilerSolver = new CompilerSolver(opts.globalOptions, interner, infererDelegate)

  // The difference between solveForDefining and solveForResolving is whether we declare the function bounds that the
  // rules mention, see DBDAR.
  def solveForDefining(
    envs: InferEnv, // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: List[RangeS],
    callLocation: LocationInDenizen,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend],
    includeReachableBoundsForRunes: Vector[IRuneS]):
  Result[CompleteDefineSolve, IDefiningError] = {
    val solver =
      makeSolver(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)
    continue(envs, coutputs, solver) match {
      case Ok(()) =>
      case Err(e) => return Err(DefiningSolveFailedOrIncomplete(e))
    }
    val conclusions =
      interpretResults(runeToType, solver) match {
        case Ok(conclusions) => conclusions
        case Err(f) => return Err(DefiningSolveFailedOrIncomplete(f))
      }
    checkDefiningConclusionsAndResolve(
      envs, coutputs, invocationRange, callLocation, rules, includeReachableBoundsForRunes, conclusions) match {
      case Ok(x) => Ok(x)
      case Err(x) => Err(DefiningResolveConclusionError(x))
    }
  }

  // The difference between solveForDefining and solveForResolving is whether we declare the function bounds that the
  // rules mention, see DBDAR.
  def solveForResolving(
      envs: InferEnv, // See CSSNCE
      coutputs: CompilerOutputs,
      rules: Vector[IRulexSR],
      runeToType: Map[IRuneS, ITemplataType],
      invocationRange: List[RangeS],
      callLocation: LocationInDenizen,
      initialKnowns: Vector[InitialKnown],
      initialSends: Vector[InitialSend],
      includeReachableBoundsForRunes: Vector[IRuneS]):
  Result[CompleteResolveSolve, IResolvingError] = {
    val solver =
      makeSolver(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)
    continue(envs, coutputs, solver) match {
      case Ok(()) =>
      case Err(e) => return Err(ResolvingSolveFailedOrIncomplete(e))
    }
    checkResolvingConclusionsAndResolve(
      envs, coutputs, invocationRange, callLocation, runeToType, rules, includeReachableBoundsForRunes, solver)
  }

  def partialSolve(
      envs: InferEnv, // See CSSNCE
      coutputs: CompilerOutputs,
      rules: Vector[IRulexSR],
      runeToType: Map[IRuneS, ITemplataType],
      invocationRange: List[RangeS],
      initialKnowns: Vector[InitialKnown],
      initialSends: Vector[InitialSend]):
  Result[Map[IRuneS, ITemplataT[ITemplataType]], FailedCompilerSolve] = {
    val solver =
      makeSolver(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)
    continue(envs, coutputs, solver) match {
      case Ok(()) =>
      case Err(e) => return Err(e)
    }
    Ok(solver.userifyConclusions().toMap)
  }


  def makeSolver(
    envs: InferEnv, // See CSSNCE
    state: CompilerOutputs,
    initialRules: Vector[IRulexSR],
    initialRuneToType: Map[IRuneS, ITemplataType],
    invocationRange: List[RangeS],
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]):
  Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType],
    ITypingPassSolverError] = {
    Profiler.frame(() => {
      val runeToType =
        initialRuneToType ++
          initialSends.map({ case InitialSend(senderRune, _, _) =>
            senderRune.rune -> CoordTemplataType()
          })
      val rules =
        initialRules ++
          initialSends.map({ case InitialSend(senderRune, receiverRune, _) =>
            CoordSendSR(receiverRune.range, senderRune, receiverRune)
          })
      val alreadyKnown =
        initialKnowns.map({ case InitialKnown(rune, templata) =>
          if (opts.globalOptions.sanityCheck) {
            infererDelegate.sanityCheckConclusion(envs, state, rune.rune, templata)
          }
          rune.rune -> templata
        }).toMap ++
          initialSends.map({ case InitialSend(senderRune, _, senderTemplata) =>
            if (opts.globalOptions.sanityCheck) {
              infererDelegate.sanityCheckConclusion(envs, state, senderRune.rune, senderTemplata)
            }
            (senderRune.rune -> senderTemplata)
          })

      val solver =
        compilerSolver.makeSolver(invocationRange, envs, state, rules, runeToType, alreadyKnown)
      solver
    })
  }

  def continue(
    envs: InferEnv, // See CSSNCE
    state: CompilerOutputs,
    solver: Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError]):
  Result[Unit, FailedCompilerSolve] = {
    compilerSolver.continue(envs, state, solver) match {
      case Ok(()) => Ok(())
      case Err(FailedSolve(steps, unsolvedRules, error)) => Err(FailedCompilerSolve(steps, unsolvedRules, error))
    }
  }

  def checkResolvingConclusionsAndResolve(
      envs: InferEnv, // See CSSNCE
      state: CompilerOutputs,
      ranges: List[RangeS],
      callLocation: LocationInDenizen,
      runeToType: Map[IRuneS, ITemplataType],
      rules: Vector[IRulexSR],
      includeReachableBoundsForRunes: Vector[IRuneS],
      solver: Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError]):
  Result[CompleteResolveSolve, IResolvingError] = {
    val (steps, conclusions) =
      compilerSolver.interpretResults(runeToType, solver) match {
        case CompleteSolve(steps, conclusions) => (steps, conclusions)
        case IncompleteSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions) => {
          return Err(ResolvingSolveFailedOrIncomplete(IncompleteCompilerSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions)))
        }
        case FailedSolve(steps, unsolvedRules, error) => {
          return Err(ResolvingSolveFailedOrIncomplete(FailedCompilerSolve(steps, unsolvedRules, error)))
        }
      }
    // rules.collect({
    //   case r@CallSR(_, RuneUsage(_, callerResolveResultRune), _, _) => {
    //     val inferences =
    //       resolveTemplateCallConclusion(envs.originalCallingEnv, state, ranges, callLocation, r, conclusions) match {
    //         case Ok(i) => i
    //         case Err(e) => return Err(FailedCompilerSolve(steps, Vector(), RuleError(CouldntResolveKind(e))))
    //       }
    //     val _ = inferences // We don't care, we just did the resolve so that we could instantiate it and add its
    //   }
    // })
    val reachableBounds =
      includeReachableBoundsForRunes
          .map(conclusions)
          .flatMap(conc => TemplataCompiler.getReachableBounds(interner, keywords, state, conc))
    val envWithConclusions = importReachableBounds(envs.originalCallingEnv, reachableBounds)
    // Check all template calls
    rules.collect({
      case r@CallSR(_, RuneUsage(_, callerResolveResultRune), _, _) => {
        val inferences =
          resolveTemplateCallConclusion(envWithConclusions, state, ranges, callLocation, r, conclusions) match {
            case Ok(i) => i
            case Err(e) => return Err(ResolvingSolveFailedOrIncomplete(FailedCompilerSolve(steps, Vector(), RuleError(CouldntResolveKind(e)))))
          }
        val _ = inferences // We don't care, we just did the resolve so that we could instantiate it and add its
      }
    })

    val runesAndPrototypes =
      rules.collect({
        case r@ResolveSR(_, _, _, _, _) => {
          resolveFunctionCallConclusion(envWithConclusions, state, ranges, callLocation, r, conclusions, envs.contextRegion) match {
            case Ok(x) => x
            case Err(e) => return Err(ResolvingResolveConclusionError(e))
          }
        }
      })
    val runeToPrototype = runesAndPrototypes.toMap
    if (runeToPrototype.size < runesAndPrototypes.size) {
      // checkFunctionCall returns None if it was an incomplete solve and we didn't have some
      // param types so it didn't attempt to resolve them.
      // If that happened at all, return None for the entire time.
      vwat()
    }

    val runesAndImpls =
      rules.collect({
        case r@CallSiteCoordIsaSR(_, _, _, _) => {
          resolveImplConclusion(envWithConclusions, state, ranges, callLocation, r, conclusions) match {
            case Ok(maybeRuneAndPrototype) => vassertSome(maybeRuneAndPrototype)
            case Err(e) => return Err(ResolvingResolveConclusionError(e))
          }
        }
      })
    val runeToImpl = runesAndImpls.toMap
    if (runeToImpl.size < runesAndImpls.size) {
      // checkFunctionCall returns None if it was an incomplete solve and we didn't have some
      // param types so it didn't attempt to resolve them.
      // If that happened at all, return None for the entire time.
      vwat()
    }

    val instantiationBoundArgs =
      InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT](
        runeToPrototype,
        runeToImpl)

    Ok(CompleteResolveSolve(conclusions, instantiationBoundArgs, reachableBounds))
  }

  def interpretResults(
      runeToType: Map[IRuneS, ITemplataType],
      solver: Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError]):
  Result[Map[IRuneS, ITemplataT[ITemplataType]], IIncompleteOrFailedCompilerSolve] = {
    compilerSolver.interpretResults(runeToType, solver) match {
      case CompleteSolve(steps, conclusions) => Ok(conclusions)
      case IncompleteSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions) => {
        Err(IncompleteCompilerSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions))
      }
      case FailedSolve(steps, unsolvedRules, error) => {
        Err(FailedCompilerSolve(steps, unsolvedRules, error))
      }
    }
  }

  // DO NOT SUBMIT this both takes in and returns conclusions lol
  def checkDefiningConclusionsAndResolve(
      envs: InferEnv, // See CSSNCE
      state: CompilerOutputs,
      invocationRange: List[RangeS],
      callLocation: LocationInDenizen,
      initialRules: Vector[IRulexSR],
      includeReachableBoundsForRunes: Vector[IRuneS],
      conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[CompleteDefineSolve, IConclusionResolveError] = {
    val reachableBounds =
      includeReachableBoundsForRunes
          .map(conclusions)
          .flatMap(conc => TemplataCompiler.getReachableBounds(interner, keywords, state, conc))
    val environmentForFinalizing =
      importConclusionsAndReachableBounds(envs.originalCallingEnv, conclusions, reachableBounds)
    val instantiationBoundArgs =
      resolveConclusions(
        environmentForFinalizing, state, invocationRange, callLocation, envs.contextRegion, initialRules, conclusions) match {
          case Ok(c) => c
          case Err(e) => return Err(e)
        }
    Ok(CompleteDefineSolve(conclusions, instantiationBoundArgs, reachableBounds))
  }

  def importReachableBounds(
      originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
      reachableBounds: Vector[PrototypeTemplataT[IFunctionNameT]]):
  GeneralEnvironmentT[INameT] = {
    GeneralEnvironmentT.childOf(
      interner,
      originalCallingEnv,
      originalCallingEnv.id,
      // These are the bounds we pulled in from the parameters, return type, impl sub citizen, etc.
      reachableBounds.zipWithIndex.map({ case (reachableBound, index) =>
        interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(reachableBound)
      }))
  }

  // This includes putting newly defined bound functions in.
  def importConclusionsAndReachableBounds(
      originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
      conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
      reachableBounds: Vector[PrototypeTemplataT[IFunctionNameT]]):
  GeneralEnvironmentT[INameT] = {
    // If this is the original calling env, in other words, if we're the original caller for
    // this particular solve, then lets add all of our templatas to the environment.
    GeneralEnvironmentT.childOf(
      interner,
      originalCallingEnv,
      originalCallingEnv.id,
      conclusions
          .map({ case (nameS, templata) =>
            interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
          }).toVector ++
          // These are the bounds we pulled in from the parameters, return type, impl sub citizen, etc.
          reachableBounds.zipWithIndex.map({ case (reachableBound, index) =>
            interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(reachableBound)
          }))
  }

  private def resolveConclusions(
    env: IInDenizenEnvironmentT, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    callLocation: LocationInDenizen,
    contextRegion: RegionT,
    rules: Vector[IRulexSR],
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT], IConclusionResolveError] = {
    // Check all template calls
    rules.foreach({
      case r@CallSR(_, _, _, _) => {
        resolveTemplateCallConclusion(env, state, ranges, callLocation, r, conclusions) match {
          case Ok(i) =>
          case Err(e) => return Err(CouldntFindKindForConclusionResolve(e))
        }
      }
      case _ =>
    })

    val runesAndPrototypes =
      rules.collect({
        case r@ResolveSR(_, _, _, _, _) => {
          resolveFunctionCallConclusion(env, state, ranges, callLocation, r, conclusions, contextRegion) match {
            case Ok(maybeRuneAndPrototype) => maybeRuneAndPrototype
            case Err(e) => return Err(e)
          }
        }
      })
    val runeToPrototype = runesAndPrototypes.toMap
    if (runeToPrototype.size < runesAndPrototypes.size) {
      // checkFunctionCall returns None if it was an incomplete solve and we didn't have some
      // param types so it didn't attempt to resolve them.
      // If that happened at all, return None for the entire time.
      vwat()
    }

    val maybeRunesAndImpls =
      rules.collect({
        case r@CallSiteCoordIsaSR(_, _, _, _) => {
          resolveImplConclusion(env, state, ranges, callLocation, r, conclusions) match {
            case Ok(maybeRuneAndPrototype) => maybeRuneAndPrototype
            case Err(e) => return Err(e)
          }
        }
      })
    val runeToImpl = maybeRunesAndImpls.flatten.toMap
    if (runeToImpl.size < maybeRunesAndImpls.size) {
      // checkFunctionCall returns None if it was an incomplete solve and we didn't have some
      // param types so it didn't attempt to resolve them.
      // If that happened at all, return None for the entire time.
      vwat()
    }

    Ok(InstantiationBoundArgumentsT(runeToPrototype, runeToImpl))
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  def resolveFunctionCallConclusion(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    ranges: List[RangeS],
    callLocation: LocationInDenizen,
    c: ResolveSR,
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
    contextRegion: RegionT):
  Result[(IRuneS, PrototypeT[IFunctionNameT]), IConclusionResolveError] = {
    val ResolveSR(range, resultRune, name, paramsListRune, returnRune) = c

    // If it was an incomplete solve, then just skip.
    val returnCoord =
      conclusions.get(returnRune.rune) match {
        case Some(CoordTemplataT(t)) => t
        case None => vwat()
      }
    val paramCoords =
      conclusions.get(paramsListRune.rune) match {
        case None => vwat()
        case Some(CoordListTemplataT(paramList)) => paramList
      }

    val funcSuccess =
      delegate.resolveFunction(callingEnv, state, range :: ranges, callLocation, name, paramCoords, contextRegion, true) match {
        case Err(e) => return Err(CouldntFindFunctionForConclusionResolve(range :: ranges, e))
        case Ok(x) => x
      }

    if (funcSuccess.prototype.prototype.returnType != returnCoord) {
      return Err(ReturnTypeConflictInConclusionResolve(range :: ranges, returnCoord, funcSuccess.prototype.prototype))
    }

    Ok((resultRune.rune, funcSuccess.prototype.prototype))
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  def resolveImplConclusion(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    ranges: List[RangeS],
    callLocation: LocationInDenizen,
    c: CallSiteCoordIsaSR,
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[Option[(IRuneS, IdT[IImplNameT])], IConclusionResolveError] = {
    val CallSiteCoordIsaSR(range, resultRune, subRune, superRune) = c

    // If it was an incomplete solve, then just skip.
    val subCoord =
      conclusions.get(subRune.rune) match {
        case Some(CoordTemplataT(t)) => t
        case None => return Ok(None)
      }
    val subKind = subCoord.kind match { case x : ISubKindTT => x case other => vwat(other) }

    val superCoord =
      conclusions.get(superRune.rune) match {
        case Some(CoordTemplataT(t)) => t
        case None => return Ok(None)
      }
    val superKind = superCoord.kind match { case x : ISuperKindTT => x case other => vwat(other) }

    val implSuccess =
      delegate.resolveImpl(callingEnv, state, range :: ranges, callLocation, subKind, superKind) match {
        case x @ IsntParent(_) => return Err(CouldntFindImplForConclusionResolve(range :: ranges, x))
        case x @ IsParent(_, _, _) => x
      }

    Ok(Some((vassertSome(resultRune).rune, implSuccess.implId)))
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  def resolveTemplateCallConclusion(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    ranges: List[RangeS],
      callLocation: LocationInDenizen,
    c: CallSR,
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[Unit, ResolveFailure[KindT]] = {
//  Result[Option[(IRuneS, PrototypeTemplata)], ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val CallSR(range, resultRune, templateRune, argRunes) = c

    // If it was an incomplete solve, then just skip.
    val template =
      conclusions.get(templateRune.rune) match {
        case Some(t) => t
        case None =>  return Ok(None)
      }
    val args =
      argRunes.map(argRune => {
        conclusions.get(argRune.rune) match {
          case Some(t) => t
          case None =>  return Ok(None)
        }
      })

    template match {
      case RuntimeSizedArrayTemplateTemplataT() => {
        val Vector(m, CoordTemplataT(coord)) = args
        val mutability = ITemplataT.expectMutability(m)
        val contextRegion = RegionT()
        delegate.resolveRuntimeSizedArrayKind(state, coord, mutability, contextRegion)
        Ok(())
      }
      case StaticSizedArrayTemplateTemplataT() => {
        val Vector(s, m, v, CoordTemplataT(coord)) = args
        val size = ITemplataT.expectInteger(s)
        val mutability = ITemplataT.expectMutability(m)
        val variability = ITemplataT.expectVariability(v)
        val contextRegion = RegionT()
        delegate.resolveStaticSizedArrayKind(state, mutability, variability, size, coord, contextRegion)
        Ok(())
      }
      case it @ StructDefinitionTemplataT(_, _) => {
        delegate.resolveStruct(callingEnv, state, range :: ranges, callLocation, it, args.toVector, true) match {
          case ResolveSuccess(kind) => kind
          case rf @ ResolveFailure(_, _) => return Err(rf)
        }
        Ok(())
      }
      case it @ InterfaceDefinitionTemplataT(_, _) => {
        delegate.resolveInterface(callingEnv, state, range :: ranges, callLocation, it, args.toVector, true) match {
          case ResolveSuccess(kind) => kind
          case rf @ ResolveFailure(_, _) => return Err(rf)
        }
        Ok(())
      }
      case kt @ KindTemplataT(_) => {
        Ok(())
      }
      case other => vimpl(other)
    }
  }

  def incrementallySolve(
    envs: InferEnv,
    coutputs: CompilerOutputs,
    solver: Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError],
    onIncompleteSolve: (Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError]) => Boolean):
  Result[Boolean, FailedCompilerSolve] = {
    // See IRAGP for why we have this incremental solving/placeholdering.
    while ( {
      continue(envs, coutputs, solver) match {
        case Ok(()) =>
        case Err(f) => return Err(f)
      }

      // During the solve, we postponed resolving structs and interfaces, see SFWPRL.
      // Caller should remember to do that!
      if (!solver.isComplete()) {
        val continue = onIncompleteSolve(solver)
        if (!continue) {
          return Ok(false)
        }
        true
      } else {
        return Ok(true)
      }
    }) {}

    vfail() // Shouldnt get here
  }
}

object InferCompiler {
  // Some rules should be excluded from the call site, see SROACSD.
  def includeRuleInCallSiteSolve(rule: IRulexSR): Boolean = {
    rule match {
      case DefinitionFuncSR(_, _, _, _, _) => false
      case DefinitionCoordIsaSR(_, _, _, _) => false
      case _ => true
    }
  }

  // Some rules should be excluded from the call site, see SROACSD.
  def includeRuleInDefinitionSolve(rule: IRulexSR): Boolean = {
    rule match {
      case CallSiteCoordIsaSR(_, _, _, _) => false
      case CallSiteFuncSR(_, _, _, _, _) => false
      case ResolveSR(_, _, _, _, _) => false
      case _ => true
    }
  }
}