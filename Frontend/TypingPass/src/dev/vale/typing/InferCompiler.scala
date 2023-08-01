package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale._
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast.{PrototypeT, PrototypeTemplataCalleeCandidate}
import dev.vale.typing.citizen.{IResolveOutcome, IsParent, IsParentResult, IsntParent, ResolveFailure, ResolveSuccess}
import dev.vale.typing.env.{CitizenEnvironmentT, EnvironmentHelper, GeneralEnvironmentT, GlobalEnvironment, IEnvEntry, IEnvironmentT, IInDenizenEnvironmentT, ILookupContext, IVariableT, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.function._
import dev.vale.typing.infer.{CompilerSolver, CouldntFindFunction, CouldntFindImpl, CouldntResolveKind, IInfererDelegate, ITypingPassSolverError, ReturnTypeConflict}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FunctionBoundNameT, IFunctionNameT, IImplNameT, INameT, ITemplateNameT, IdT, ImplNameT, NameTranslator, ReachablePrototypeNameT, ResolvingEnvNameT, RuneNameT}
import dev.vale.typing.templata.ITemplataT.{expectCoordTemplata, expectMutability, expectPrototypeTemplata}
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.{List, Set}

//ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]

sealed trait IResolveSolveOutcome {
  def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]]
}
case class CompleteResolveSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplataT[ITemplataType]]],
  conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
  instantiationBoundArgs: InstantiationBoundArgumentsT
) extends IResolveSolveOutcome {
  override def getOrDie(): Map[IRuneS, ITemplataT[ITemplataType]] = conclusions
}

case class CompleteDefineSolve(
  conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
  runeToBound: InstantiationBoundArgumentsT,
  declaredBounds: Vector[PrototypeT[FunctionBoundNameT]],
  reachableBounds: Vector[PrototypeT[FunctionBoundNameT]] // DO NOT SUBMIT document. is this for the call site?
)

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
  // Note from later: This seems to be the things' outer environment, basically the declaring environment plus the name
  // of the denizen we're solving.
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
    interpretResults(runeToType, solver) match {
      case Ok(conclusions) => {
        checkDefiningConclusionsAndResolve(
          envs.originalCallingEnv, coutputs, invocationRange, callLocation, envs.contextRegion, rules, includeReachableBoundsForRunes, conclusions) match {
          case Ok(x) => Ok(x)
          case Err(x) => Err(DefiningResolveConclusionError(x))
        }
      }
      case Err(e) => Err(DefiningSolveFailedOrIncomplete(e))
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
      invocationRange: List[RangeS],
      callLocation: LocationInDenizen,
      runeToType: Map[IRuneS, ITemplataType],
      initialRules: Vector[IRulexSR],
      // We'll want to get these reachable bounds so that we can add them to the instantiation bound args.
      includeReachableBoundsForRunes: Vector[IRuneS],
      solver: Solver[IRulexSR, IRuneS, InferEnv, CompilerOutputs, ITemplataT[ITemplataType], ITypingPassSolverError]):
  Result[CompleteResolveSolve, IResolvingError] = {
    compilerSolver.interpretResults(runeToType, solver) match {
      case CompleteSolve(steps, conclusions) => {
        val citizenToReachableBounds =
          includeReachableBoundsForRunes
              .flatMap(rune => {
                vassertSome(conclusions.get(rune)) match {
                  case KindTemplataT(c@ICitizenTT(_)) => {
                    List(rune -> TemplataCompiler.getReachableBounds(interner, keywords, envs.originalCallingEnv.denizenTemplateId, false, state, c))
                  }
                  case CoordTemplataT(CoordT(_, _, c@ICitizenTT(_))) => {
                    List(rune -> TemplataCompiler.getReachableBounds(interner, keywords, envs.originalCallingEnv.denizenTemplateId, false, state, c))
                  }
                  case _ => List()
                }
              }).toMap
        val envWithConclusions = importReachableBounds(state, envs.originalCallingEnv, citizenToReachableBounds)
        // right here we need to DO NOT SUBMIT
        val instantiationBoundArgs =
          resolveConclusionsForResolve(
            envWithConclusions, state, invocationRange, callLocation, envs.contextRegion, initialRules, conclusions, citizenToReachableBounds) match {
            case Ok(c) => c
            case Err(e) => return Err(ResolvingResolveConclusionError(e))
          }
        Ok(CompleteResolveSolve(steps, conclusions, instantiationBoundArgs))
      }
      case i @ IncompleteSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions) => {
        Err(ResolvingSolveFailedOrIncomplete(IncompleteCompilerSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions)))
      }
      case FailedSolve(steps, unsolvedRules, error) => {
        Err(ResolvingSolveFailedOrIncomplete(FailedCompilerSolve(steps, unsolvedRules, error)))
      }
    }
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

  def checkDefiningConclusionsAndResolve(
      originalCallingEnv: IInDenizenEnvironmentT,
      state: CompilerOutputs,
      invocationRange: List[RangeS],
      callLocation: LocationInDenizen,
      contextRegion: RegionT,
      initialRules: Vector[IRulexSR],
      // We'll want these reachable bounds so that we can add them to our environment to call them.
      includeReachableBoundsForRunes: Vector[IRuneS],
      conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[CompleteDefineSolve, IConclusionResolveError] = {
    // DO NOT SUBMIT might be slow.
    val declaredBounds =
      initialRules
          .collect({ case DefinitionFuncSR(_, resultRune, _, _, _) => resultRune.rune })
          .map(conclusions)
          .map({
            case PrototypeTemplataT(_, PrototypeT(IdT(packageCoord, initSteps, n @ FunctionBoundNameT(_, _, _)), returnType)) => {
              PrototypeT(IdT(packageCoord, initSteps, n), returnType)
            }
            case other => vwat(other)
          })
    // We'll need to look at these and figure out what we're supplying to fulfill these callee-expected bounds.
    val callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions =
      includeReachableBoundsForRunes
          .map(rune => rune -> vassertSome(conclusions.get(rune)))
          .flatMap({
            case (rune, KindTemplataT(c@ICitizenTT(_))) => List(rune -> c)
            case (rune, CoordTemplataT(CoordT(_, _, c@ICitizenTT(_)))) => List(rune -> c)
            case _ => List()
          })
          .map({ case (rune, citizen) =>
            rune -> TemplataCompiler.getReachableBounds(interner, keywords, originalCallingEnv.denizenTemplateId, true, state, citizen)
          })
          .toMap
    val environmentForFinalizing =
      importConclusionsAndReachableBounds(state, originalCallingEnv, conclusions, declaredBounds, callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions)
    val callerPlaceholderedReachableBoundFunction =
      callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions.values.flatMap(_.values)
    val instantiationBoundArgs =
      resolveConclusionsForDefine(
        environmentForFinalizing, state, invocationRange, callLocation, contextRegion, initialRules, conclusions) match {
          case Ok(c) => c
          case Err(e) => return Err(e)
        }
    Ok(CompleteDefineSolve(conclusions, instantiationBoundArgs, declaredBounds, callerPlaceholderedReachableBoundFunction.toVector)) // DO NOT SUBMIT seems redundant with instantiationBoundArgs now
  }

  def importReachableBounds(
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    reachableBounds: Map[IRuneS, Map[IRuneS, PrototypeT[FunctionBoundNameT]]]):
  GeneralEnvironmentT[INameT] = {

    // DO NOT SUBMIT document. we did a perspective shift, bringing these prototypes into our own perspective and making it take *our* placeholders. we'll
    // need these in the overload index so that we can resolve them later when we want to call them.
    reachableBounds.values.flatMap(_.values).foreach(prototype => {
      // DO NOT SUBMIT move from TemplatasStore
      TemplatasStore.getImpreciseName(interner, prototype.id.localName) match {
        case None => println("Skipping adding bound " + prototype.id.localName) // DO NOT SUBMIT
        case Some(impreciseName) => {
          coutputs.addOverload(
            opts.globalOptions.useOverloadIndex,
            impreciseName,
            prototype.id.localName.parameters.map(x => Some(x)),
            PrototypeTemplataCalleeCandidate(prototype.id.localName.template.range, prototype))
        }
      }
    })

    GeneralEnvironmentT.childOf(
      interner,
      originalCallingEnv,
      originalCallingEnv.denizenTemplateId,
      originalCallingEnv.id,
      // These are the bounds we pulled in from the parameters, return type, impl sub citizen, etc.
      reachableBounds.values.flatMap(_.values).toVector.zipWithIndex.map({ case (reachableBound, index) =>
        interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(PrototypeTemplataT(reachableBound.id.localName.template.range, reachableBound))
      }))
  }

  // This includes putting newly defined bound functions in.
  def importConclusionsAndReachableBounds(
      coutputs: CompilerOutputs,
      originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
      conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
      declaredBounds: Vector[PrototypeT[FunctionBoundNameT]],
      callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions: Map[IRuneS, Map[IRuneS, PrototypeT[FunctionBoundNameT]]]):
  GeneralEnvironmentT[INameT] = {
    // If this is the original calling env, in other words, if we're the original caller for
    // this particular solve, then lets add all of our templatas to the environment.

    (declaredBounds ++ callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions.values.flatMap(_.values)).foreach({ case prototype =>
      // DO NOT SUBMIT move from TemplatasStore
      TemplatasStore.getImpreciseName(interner, prototype.id.localName) match {
        case None => println("Skipping adding bound " + prototype.id.localName) // DO NOT SUBMIT
        case Some(impreciseName) => {
          coutputs.addOverload(
            opts.globalOptions.useOverloadIndex,
            impreciseName,
            prototype.id.localName.parameters.map(x => Some(x)),
            PrototypeTemplataCalleeCandidate(prototype.id.localName.template.range, prototype))
        }
      }
    })

    GeneralEnvironmentT.childOf(
      interner,
      originalCallingEnv,
      originalCallingEnv.denizenTemplateId,
      originalCallingEnv.id,
      conclusions
          .map({ case (nameS, templata) =>
            interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
          }).toVector ++
          // These are the bounds we pulled in from the parameters, return type, impl sub citizen, etc.
          callerRuneToCalleeRuneToCallerPlaceholderedExpectedBoundFunctions.values.flatMap(_.values).zipWithIndex.map({ case (reachableBound, index) =>
            interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(PrototypeTemplataT(reachableBound.id.localName.template.range, reachableBound))
          }))
  }

  private def resolveConclusionsForResolve( // DO NOT SUBMIT rename or fold in
    env: IInDenizenEnvironmentT, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    callLocation: LocationInDenizen,
    contextRegion: RegionT,
    rules: Vector[IRulexSR],
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
    // I think we'll need to include all these in the instantiation bounds DO NOT SUBMIT
    // These are caller-placeholdered versions of the bounds that these citizens expect to exist.
    // Let's try to resolve them here? fuck how do we even do that...
    // i think we dont actually. hm. we wait until the instantiator
    citizenToReachableBounds: Map[IRuneS, Map[IRuneS, PrototypeT[FunctionBoundNameT]]]):
  Result[InstantiationBoundArgumentsT, IConclusionResolveError] = {
    val callerRuneToInstantiationReachableBoundArgFuncsFromKindResolves =
      rules.collect({
        case r@CallSR(_, RuneUsage(_, callerResolveResultRune), _, _) => {
          val inferences =
            resolveTemplateCallConclusion(env, state, ranges, callLocation, r, conclusions) match {
              case Ok(i) => i
              case Err(e) => return Err(CouldntFindKindForConclusionResolve(e))
            }
          val callerPlaceholderedCalleeBoundFunctionToCallerSuppliedBoundFunctionArg =
            citizenToReachableBounds.get(callerResolveResultRune) match {
              case None => {
                // There are plenty of things that we resolve which we don't want to pull in reachable instantiation
                // bounds for. For example, function return types.
                Map[PrototypeT[FunctionBoundNameT], PrototypeT[IFunctionNameT]]()
              }
              case Some(calleeRuneToCallerPlaceholderedCalleeBoundFunction) => {
                calleeRuneToCallerPlaceholderedCalleeBoundFunction
                    .toVector
                    .map({ case (calleeRune, callerPlaceholderedCalleeBoundFunction) =>
                      vassertSome(inferences.get(calleeRune)) match {
                        case PrototypeTemplataT(_, callerSuppliedBoundFunctionArg) => {
                          (callerPlaceholderedCalleeBoundFunction -> callerSuppliedBoundFunctionArg)
                        }
                        case other => vwat(other)
                      }
                    })
                    .toMap
              }
            }
          val instantiationReachableBoundArguments =
            InstantiationReachableBoundArgumentsT(
              callerPlaceholderedCalleeBoundFunctionToCallerSuppliedBoundFunctionArg)
          callerResolveResultRune -> instantiationReachableBoundArguments
        }
      })

    val funcResolveResults =
      rules
          .collect({
            case r@ResolveSR(_, RuneUsage(_, callerResolveResultRune), _, _, _) => {
              val (prototype, inferences) =
                resolveFunctionCallConclusion(env, state, ranges, callLocation, r, conclusions, contextRegion) match {
                  case Ok(x) => x
                  case Err(e) => return Err(e)
                }
              val callerPlaceholderedCalleeBoundFunctionToCallerSuppliedBoundFunctionArg =
                citizenToReachableBounds.get(callerResolveResultRune) match {
                  case None => {
                    // There are plenty of things that we resolve which we don't want to pull in reachable instantiation
                    // bounds for. For example, function return types.
                    Map[PrototypeT[FunctionBoundNameT], PrototypeT[IFunctionNameT]]()
                  }
                  case Some(calleeRuneToCallerPlaceholderedCalleeBoundFunction) => {
                    calleeRuneToCallerPlaceholderedCalleeBoundFunction
                        .toVector
                        .map({ case (calleeRune, callerPlaceholderedCalleeBoundFunction) =>
                          vassertSome(inferences.get(calleeRune)) match {
                            case PrototypeTemplataT(_, callerSuppliedBoundFunctionArg) => {
                              (callerPlaceholderedCalleeBoundFunction -> callerSuppliedBoundFunctionArg)
                            }
                            case other => vwat(other)
                          }
                        })
                        .toMap
                  }
                }
              val instantiationReachableBoundArguments =
                InstantiationReachableBoundArgumentsT(
                  callerPlaceholderedCalleeBoundFunctionToCallerSuppliedBoundFunctionArg)
              callerResolveResultRune -> (prototype, instantiationReachableBoundArguments)
            }
          })
          .toMap
    val runeToPrototype = funcResolveResults.mapValues(_._1)
    val callerRuneToInstantiationReachableBoundArgFuncsFromFuncResolves = funcResolveResults.mapValues(_._2)

    val callerRuneToInstantiationReachableBoundArgFuncs =
      (callerRuneToInstantiationReachableBoundArgFuncsFromKindResolves ++
      callerRuneToInstantiationReachableBoundArgFuncsFromFuncResolves).toMap


    // // we have to decide whether its okay or not to be satisfying multiple types' bounds for this call, or whether we want
    // // to import them somehow.
    // // my gut tells me it might be better to just include all these bounds even though theyre heterogenous. we won't have
    // // any bounds in the calling function to really attach them to. it's not like the calling function assigns them into
    // // its own bounds or something.
    // // so we should probably handle this downstream.
    // runeToPrototype.values.map(_.id.steps.init).toVector.distinct match {
    //   case Vector() => // Fine, not all things have bounds
    //   case Vector(_) => // Good, only one.
    //   case other => vwat(other)
    // }
    // // DO NOT SUBMIT document VERY THOROUGHLY that we could have heterogenous bounds.

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
      vwat()
    }

    Ok(InstantiationBoundArgumentsT(runeToPrototype, callerRuneToInstantiationReachableBoundArgFuncs, runeToImpl))
  }

  private def resolveConclusionsForDefine( // DO NOT SUBMIT rename or fold in
      env: IInDenizenEnvironmentT, // See CSSNCE
      state: CompilerOutputs,
      ranges: List[RangeS],
      callLocation: LocationInDenizen,
      contextRegion: RegionT,
      rules: Vector[IRulexSR],
      conclusions: Map[IRuneS, ITemplataT[ITemplataType]]):
  Result[InstantiationBoundArgumentsT, IConclusionResolveError] = {
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

    // DO NOT SUBMIT can we get these instead from the DefinitionFuncSR? then they cant accidentally get it from
    // somewhere else
    val runeToPrototype =
      rules
          .collect({
            case r@ResolveSR(_, RuneUsage(_, resultRune), _, _, _) => {
              resolveFunctionCallConclusion(env, state, ranges, callLocation, r, conclusions, contextRegion) match {
                case Ok((prototype, _)) => (resultRune -> prototype)
                case Err(e) => return Err(e)
              }
            }
          })
          .toMap

    // // we have to decide whether its okay or not to be satisfying multiple types' bounds for this call, or whether we want
    // // to import them somehow.
    // // my gut tells me it might be better to just include all these bounds even though theyre heterogenous. we won't have
    // // any bounds in the calling function to really attach them to. it's not like the calling function assigns them into
    // // its own bounds or something.
    // // so we should probably handle this downstream.
    // runeToPrototype.values.map(_.id.steps.init).toVector.distinct match {
    //   case Vector() => // Fine, not all things have bounds
    //   case Vector(_) => // Good, only one.
    //   case other => vwat(other)
    // }
    // // DO NOT SUBMIT document VERY THOROUGHLY that we could have heterogenous bounds.

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
      vwat()
    }

    Ok(InstantiationBoundArgumentsT(runeToPrototype, Map(), runeToImpl))
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  // Otherwise returns a Some containing:
  // - The rune in the caller that we're making the bound for? DO NOT SUBMIT            not The callee bound function declaration which needs to be filled. Callee-placeholdered.
  // - The function the caller is supplying, the "bound arg function". Caller-placeholdered.
  def resolveFunctionCallConclusion(
    callingEnv: IInDenizenEnvironmentT,
    state: CompilerOutputs,
    ranges: List[RangeS],
    callLocation: LocationInDenizen,
    c: ResolveSR,
    conclusions: Map[IRuneS, ITemplataT[ITemplataType]],
    contextRegion: RegionT):
  Result[(PrototypeT[IFunctionNameT], Map[IRuneS, ITemplataT[ITemplataType]]), IConclusionResolveError] = {
    val ResolveSR(range, _, name, paramsListRune, returnRune) = c

    // If it was an incomplete solve, then just skip.
    val returnCoord =
      expectCoordTemplata(vassertSome(conclusions.get(returnRune.rune))).coord
    val paramCoords =
      vassertSome(conclusions.get(paramsListRune.rune)) match {
        case CoordListTemplataT(paramList) => paramList
        case _ => vwat()
      }
    val StampFunctionSuccess(prototype, inferences) =
      delegate.resolveFunction(callingEnv, state, range :: ranges, callLocation, name, paramCoords, contextRegion, true) match {
        case Err(e) => return Err(CouldntFindFunctionForConclusionResolve(range :: ranges, e))
        case Ok(x) => x
      }

    if (prototype.prototype.returnType != returnCoord) {
      return Err(ReturnTypeConflictInConclusionResolve(range :: ranges, returnCoord, prototype.prototype))
    }

    Ok((prototype.prototype, inferences))
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
  Result[Map[IRuneS, ITemplataT[ITemplataType]], ResolveFailure[KindT]] = {
//  Result[Option[(IRuneS, PrototypeTemplata)], ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val CallSR(range, resultRune, templateRune, argRunes) = c

    // If it was an incomplete solve, then just skip.
    val template = vassertSome(conclusions.get(templateRune.rune))
    val args = argRunes.map(argRune => vassertSome(conclusions.get(argRune.rune)))

    template match {
      case RuntimeSizedArrayTemplateTemplataT() => {
        val Vector(m, CoordTemplataT(coord)) = args
        val mutability = ITemplataT.expectMutability(m)
        val contextRegion = RegionT()
        delegate.resolveRuntimeSizedArrayKind(state, coord, mutability, contextRegion)
        Ok(Map())
      }
      case StaticSizedArrayTemplateTemplataT() => {
        val Vector(s, m, v, CoordTemplataT(coord)) = args
        val size = ITemplataT.expectInteger(s)
        val mutability = ITemplataT.expectMutability(m)
        val variability = ITemplataT.expectVariability(v)
        val contextRegion = RegionT()
        delegate.resolveStaticSizedArrayKind(state, mutability, variability, size, coord, contextRegion)
        Ok(Map())
      }
      case it @ StructDefinitionTemplataT(_, _) => {
        delegate.resolveStruct(callingEnv, state, range :: ranges, callLocation, it, args.toVector, true) match {
          case ResolveSuccess(kind, inferences) => Ok(inferences)
          case rf @ ResolveFailure(_, _) => return Err(rf)
        }
      }
      case it @ InterfaceDefinitionTemplataT(_, _) => {
        delegate.resolveInterface(callingEnv, state, range :: ranges, callLocation, it, args.toVector, true) match {
          case ResolveSuccess(kind, inferences) => Ok(inferences)
          case rf @ ResolveFailure(_, _) => return Err(rf)
        }
      }
      case kt @ KindTemplataT(_) => {
        vcurious()
        Ok(Map())
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

// DO NOT SUBMIT could we cache the bounds of a certain struct instantiation so we dont have to keep recalculating them?
// do we already do that with instantiation bounds?
// im not sure we cache placeholders, does that interfere? can we get around that?