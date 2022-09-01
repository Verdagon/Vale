package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, Result, StrI, typing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast.PrototypeT
import dev.vale.typing.citizen.ResolveSuccess
import dev.vale.typing.env.{CitizenEnvironment, EnvironmentHelper, GeneralEnvironment, GlobalEnvironment, IEnvEntry, IEnvironment, ILookupContext, IVariableT, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionSuccess
import dev.vale.typing.infer.{CompilerSolver, CouldntFindFunction, IInfererDelegate, ITypingPassSolverError}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FullNameT, INameT, ITemplateNameT, NameTranslator, ReachablePrototypeNameT, ResolvingEnvNameT, RuneNameT}
import dev.vale.typing.templata.{CoordListTemplata, CoordTemplata, ITemplata, InterfaceDefinitionTemplata, KindTemplata, PrototypeTemplata, RuntimeSizedArrayTemplateTemplata, StructDefinitionTemplata}
import dev.vale.typing.types.{CoordT, ICitizenTT, InterfaceTT, RuntimeSizedArrayTT, StaticSizedArrayTT, StructTT}

import scala.collection.immutable.{List, Set}

//ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]

sealed trait ICompilerSolverOutcome {
  def getOrDie(): Map[IRuneS, ITemplata[ITemplataType]]
}
sealed trait IIncompleteOrFailedCompilerSolve extends ICompilerSolverOutcome {
  def unsolvedRules: Vector[IRulexSR]
  def unsolvedRunes: Vector[IRuneS]
  def steps: Stream[Step[IRulexSR, IRuneS, ITemplata[ITemplataType]]]
}
case class CompleteCompilerSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplata[ITemplataType]]],
  conclusions: Map[IRuneS, ITemplata[ITemplataType]],
  runeToFunctionBound: Map[IRuneS, PrototypeT]
) extends ICompilerSolverOutcome {
  override def getOrDie(): Map[IRuneS, ITemplata[ITemplataType]] = conclusions
}
case class IncompleteCompilerSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplata[ITemplataType]]],
  unsolvedRules: Vector[IRulexSR],
  unknownRunes: Set[IRuneS],
  incompleteConclusions: Map[IRuneS, ITemplata[ITemplataType]]
) extends IIncompleteOrFailedCompilerSolve {
  vassert(unknownRunes.nonEmpty)
  override def getOrDie(): Map[IRuneS, ITemplata[ITemplataType]] = vfail()
  override def unsolvedRunes: Vector[IRuneS] = unknownRunes.toVector
}

case class FailedCompilerSolve(
  steps: Stream[Step[IRulexSR, IRuneS, ITemplata[ITemplataType]]],
  unsolvedRules: Vector[IRulexSR],
  error: ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]
) extends IIncompleteOrFailedCompilerSolve {
  override def getOrDie(): Map[IRuneS, ITemplata[ITemplataType]] = vfail()
  override def unsolvedRunes: Vector[IRuneS] = Vector()
}

case class InferEnv(
  // This is the only one that matters when checking template instantiations.
  // This is also the one that the placeholders come from.
  originalCallingEnv: IEnvironment,

  parentRanges: List[RangeS],

//
//  // We look in this for declared functions, see CSSNCE.
//  // DO NOT SUBMIT we no longer need this because of SFWPRL
//  directlyCallingEnv: IEnvironment,

  // We look in this for everything else, such as type names like "int" etc.
  selfEnv: IEnvironment,


  // Sometimes these can be all equal.
)

case class InitialSend(
  senderRune: RuneUsage,
  receiverRune: RuneUsage,
  sendTemplata: ITemplata[ITemplataType])

case class InitialKnown(
  rune: RuneUsage,
  templata: ITemplata[ITemplataType])

trait IInferCompilerDelegate {
  def resolveStruct(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    callRange: List[RangeS],
    templata: StructDefinitionTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]],
    verifyConclusions: Boolean):
  ResolveSuccess[StructTT]

  def resolveInterface(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    callRange: List[RangeS],
    templata: InterfaceDefinitionTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]],
    verifyConclusions: Boolean):
  ResolveSuccess[InterfaceTT]

  def resolveStaticSizedArrayKind(
    coutputs: CompilerOutputs,
    mutability: ITemplata[MutabilityTemplataType],
    variability: ITemplata[VariabilityTemplataType],
    size: ITemplata[IntegerTemplataType],
    element: CoordT):
  StaticSizedArrayTT

  def resolveRuntimeSizedArrayKind(
    coutputs: CompilerOutputs,
    type2: CoordT,
    arrayMutability: ITemplata[MutabilityTemplataType]):
  RuntimeSizedArrayTT

  def resolveFunction(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    range: List[RangeS],
    name: StrI,
    coords: Vector[CoordT],
    verifyConclusions: Boolean):
  Result[EvaluateFunctionSuccess, FindFunctionFailure]
}

class InferCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    infererDelegate: IInfererDelegate,
    delegate: IInferCompilerDelegate) {
  def solveComplete(
    envs: InferEnv, // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: List[RangeS],
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend],
    verifyConclusions: Boolean,
    isRootSolve: Boolean,
    includeReachableBoundsForRunes: Set[IRuneS]):
  Result[CompleteCompilerSolve, IIncompleteOrFailedCompilerSolve] = {
    solve(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends, verifyConclusions, isRootSolve, includeReachableBoundsForRunes) match {
      case f @ FailedCompilerSolve(_, _, _) => Err(f)
      case i @ IncompleteCompilerSolve(_, _, _, _) => Err(i)
      case c @ CompleteCompilerSolve(_, _, _) => Ok(c)
    }
  }

  def solveExpectComplete(
    envs: InferEnv, // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: List[RangeS],
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend],
    verifyConclusions: Boolean,
    isRootSolve: Boolean,
    includeReachableBoundsForRunes: Set[IRuneS]):
  CompleteCompilerSolve = {
    solve(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends, verifyConclusions, isRootSolve, includeReachableBoundsForRunes) match {
      case f @ FailedCompilerSolve(_, _, err) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, f))
      }
      case i @ IncompleteCompilerSolve(_, _, _, _) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, i))
      }
      case c @ CompleteCompilerSolve(_, conclusions, _) => c
    }
  }


  def solve(
    envs: InferEnv, // See CSSNCE
    state: CompilerOutputs,
    initialRules: Vector[IRulexSR],
    initialRuneToType: Map[IRuneS, ITemplataType],
    invocationRange: List[RangeS],
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend],
    verifyConclusions: Boolean,
    isRootSolve: Boolean,
    includeReachableBoundsForRunes: Set[IRuneS]):
  ICompilerSolverOutcome = {
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

      new CompilerSolver(opts.globalOptions, interner, infererDelegate)
        .solve(
          invocationRange,
          envs,
          state,
          rules,
          runeToType,
          alreadyKnown) match {
        case CompleteSolve(steps, conclusionsWithoutReachableBounds) => {
          val conclusionsMaybeWithReachableBounds =
            conclusionsWithoutReachableBounds ++
              includeReachableBoundsForRunes
                .map(conclusionsWithoutReachableBounds)
                .flatMap(conc => TemplataCompiler.getReachableBounds(interner, keywords, state, conc))
                .zipWithIndex
                .map({ case (templata, num) => ReachablePrototypeRuneS(num) -> templata })
          val runeToFunctionBound =
            if (verifyConclusions) {
              checkTemplateInstantiations(envs, state, invocationRange, rules.toArray, conclusionsMaybeWithReachableBounds, isRootSolve) match {
                case Ok(c) => vassertSome(c)
                case Err(e) => return FailedCompilerSolve(steps, Vector(), e)
              }
            } else {
              Map[IRuneS, PrototypeT]()
            }
          CompleteCompilerSolve(steps, conclusionsMaybeWithReachableBounds, runeToFunctionBound)
        }
        case IncompleteSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions) => {
          if (verifyConclusions) {
            checkTemplateInstantiations(envs, state, invocationRange, rules.toArray, incompleteConclusions, isRootSolve) match {
              case Ok(c) =>
              case Err(e) => return FailedCompilerSolve(steps, unsolvedRules, e)
            }
          }
          IncompleteCompilerSolve(steps, unsolvedRules, unknownRunes, incompleteConclusions)
        }
        case FailedSolve(steps, unsolvedRules, error) => FailedCompilerSolve(steps, unsolvedRules, error)
      }
    })
  }

  def checkTemplateInstantiations(
    envs: InferEnv, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]],
    isRootSolve: Boolean):
  Result[Option[Map[IRuneS, PrototypeT]], ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    // This is a temporary env which contains all of our conclusions.
    // This is important if we want to resolve some sort of existing type, like how
    //   impl<T> Opt<T> for Some<T> where func drop(T)void;
    // will want to resolve that Some<T> and want it to see that there's a drop(T).
    //
    // However, we *dont* want to use this temporary env when imposing conditions on the caller.
    // If we have:
    //   func moo(x Bork<int>) { }
    //   struct Bork<T> where func drop(T)void { }
    // then when we're compiling moo's Bork<int>, we *dont* want the conclusions we just figured
    // out, because we'd see the temporary func drop(T) void that the CallSiteSR just conjured up.
    //
    // So, if we're invoking a template (like CallSR) then we want to use the temporary env...
    // ...but if we want to impose a restriction on above, we don't.
//    val callingEnv =
//      maybeCallingEnv match {
//        case None => return Ok(())
//        case Some(x) => x
//      }
//    val name = callingEnv.fullName.addStep(ResolvingEnvNameT())

    if (isRootSolve) {
      // If this is the original calling env, in other words, if we're the original caller for
      // this particular solve, then lets add all of our templatas to the environment.
      val originalCallingEnvWithUnverifiedConclusions =
        GeneralEnvironment.childOf(
          interner,
          envs.originalCallingEnv,
          envs.originalCallingEnv.fullName,
          conclusions
            .map({ case (nameS, templata) =>
              interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
            }).toVector)

      checkTemplateInstantiationsForEnv(
          originalCallingEnvWithUnverifiedConclusions, state, ranges, rules, conclusions)
    } else {
      checkTemplateInstantiationsForEnv(
          envs.originalCallingEnv, state, ranges, rules, conclusions)
    }
  }

  private def checkTemplateInstantiationsForEnv(
    env: IEnvironment, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Result[Option[Map[IRuneS, PrototypeT]], ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    // Check all template calls
    rules.foreach({
      case r@CallSR(_, _, _, _) => {
        checkTemplateCall(env, state, ranges, r, conclusions) //match {
//          case Ok(maybeRuneAndPrototype) => maybeRuneAndPrototype
//          case Err(e) => return Err(e)
//        }
      }
      case _ =>
    })

    // Check all function calls
    val maybeRunesAndPrototypes =
      rules.collect({
        case r@ResolveSR(_, _, _, _, _) => {
          checkFunctionCall(env, state, ranges, r, conclusions) match {
            case Ok(maybeRuneAndPrototype) => maybeRuneAndPrototype
            case Err(e) => return Err(e)
          }
        }
      })

    val runeToPrototype = maybeRunesAndPrototypes.flatten.toMap
    if (runeToPrototype.size < maybeRunesAndPrototypes.size) {
      // checkFunctionCall returns None if it was an incomplete solve and we didn't have some
      // param types so it didn't attempt to resolve them.
      // If that happened at all, return None for the entire time.
      Ok(None)
    } else {
      Ok(Some(runeToPrototype))
    }
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  def checkFunctionCall(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    ranges: List[RangeS],
    c: ResolveSR,
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Result[Option[(IRuneS, PrototypeT)], ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val ResolveSR(range, resultRune, name, paramsListRune, returnRune) = c

    // If it was an incomplete solve, then just skip.
    val returnCoord =
      conclusions.get(returnRune.rune) match {
        case Some(CoordTemplata(t)) => t
        case None => return Ok(None)
      }
    val paramCoords =
      conclusions.get(paramsListRune.rune) match {
        case None => return Ok(None)
        case Some(CoordListTemplata(paramList)) => paramList
      }

    val funcSuccess =
      delegate.resolveFunction(callingEnv, state, range :: ranges, name, paramCoords, true) match {
        case Err(e) => return Err(RuleError(CouldntFindFunction(range :: ranges, e)))
        case Ok(x) => x
      }

    if (funcSuccess.function.prototype.returnType != returnCoord) {
      throw CompileErrorExceptionT(RangedInternalErrorT(range :: ranges, "Return type conflict"))
    }

    Ok(Some((resultRune.rune, funcSuccess.function.prototype)))
  }

  // Returns None for any call that we don't even have params for,
  // like in the case of an incomplete solve.
  def checkTemplateCall(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    ranges: List[RangeS],
    c: CallSR,
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Unit = {
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
      case RuntimeSizedArrayTemplateTemplata() => {
        val Array(m, CoordTemplata(coord)) = args
        val mutability = ITemplata.expectMutability(m)
        delegate.resolveRuntimeSizedArrayKind(state, coord, mutability)
      }
      case it @ StructDefinitionTemplata(_, _) => {
        delegate.resolveStruct(callingEnv, state, range :: ranges, it, args.toVector, true).kind
      }
      case it @ InterfaceDefinitionTemplata(_, _) => {
        delegate.resolveInterface(callingEnv, state, range :: ranges, it, args.toVector, true).kind
      }
      case kt @ KindTemplata(_) => {
        Ok(kt)
      }
      case other => vimpl(other)
    }
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