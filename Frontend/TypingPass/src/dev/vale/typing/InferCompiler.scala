package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, Result, StrI, typing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.env.{CitizenEnvironment, EnvironmentHelper, GeneralEnvironment, GlobalEnvironment, IEnvEntry, IEnvironment, ILookupContext, IVariableT, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.infer.{CompilerSolver, CouldntFindFunction, IInfererDelegate, ITypingPassSolverError}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FullNameT, INameT, ITemplateNameT, NameTranslator, ResolvingEnvNameT, RuneNameT}
import dev.vale.typing.templata.{CoordListTemplata, CoordTemplata, ITemplata, InterfaceTemplata, KindTemplata, PrototypeTemplata, RuntimeSizedArrayTemplateTemplata, StructTemplata}
import dev.vale.typing.types.{CoordT, InterfaceTT, RuntimeSizedArrayTT, StaticSizedArrayTT, StructTT}

import scala.collection.immutable.{List, Set}

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
    templata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]],
    verifyConclusions: Boolean):
  StructTT

  def resolveInterface(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    callRange: List[RangeS],
    templata: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]],
    verifyConclusions: Boolean):
  InterfaceTT

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
  Result[PrototypeTemplata, FindFunctionFailure]
}

class InferCompiler(
    opts: TypingPassOptions,
    interner: Interner,
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
    isRootSolve: Boolean):
  Result[Map[IRuneS, ITemplata[ITemplataType]], IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    solve(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends, verifyConclusions, isRootSolve) match {
      case f @ FailedSolve(_, _, _) => Err(f)
      case i @ IncompleteSolve(_, _, _, _) => Err(i)
      case CompleteSolve(_, conclusions) => Ok(conclusions)
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
    isRootSolve: Boolean):
  Map[IRuneS, ITemplata[ITemplataType]] = {
    solve(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends, verifyConclusions, isRootSolve) match {
      case f @ FailedSolve(_, _, err) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, f))
      }
      case i @ IncompleteSolve(_, _, _, _) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, i))
      }
      case CompleteSolve(_, conclusions) => conclusions
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
    isRootSolve: Boolean):
  ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError] = {
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

      val outcome =
        new CompilerSolver(opts.globalOptions, interner, infererDelegate)
          .solve(
            invocationRange,
            envs,
            state,
            rules,
            runeToType,
            alreadyKnown)
      if (verifyConclusions) {
        outcome match {
          case CompleteSolve(steps, conclusions) => {
            checkTemplateInstantiations(envs, state, invocationRange, rules.toArray, conclusions, isRootSolve) match {
              case Ok(c) =>
              case Err(e) => return FailedSolve(steps, Vector(), e)
            }
          }
          case IncompleteSolve(steps, unsolvedRules, _, incompleteConclusions) => {
            checkTemplateInstantiations(envs, state, invocationRange, rules.toArray, incompleteConclusions, isRootSolve) match {
              case Ok(c) =>
              case Err(e) => return FailedSolve(steps, unsolvedRules, e)
            }
          }
          case FailedSolve(_, _, _) =>
        }
      }
      outcome
    })
  }

  def checkTemplateInstantiations(
    envs: InferEnv, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]],
    isRootSolve: Boolean):
  Result[Unit, ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
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
          conclusions.map({ case (nameS, templata) =>
            interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
          }).toVector)

      strt here
      // this is where we should look at all the citizens we solved for, grab the prototypes from them,
      // and merge them in, see NBIFP and ONBIFS.

      checkTemplateInstantiationsForEnv(
          originalCallingEnvWithUnverifiedConclusions, state, ranges, rules, conclusions) match {
        case Err(e) => return Err(e)
        case Ok(()) =>
      }
    } else {
      checkTemplateInstantiationsForEnv(
          envs.originalCallingEnv, state, ranges, rules, conclusions) match {
        case Err(e) => return Err(e)
        case Ok(()) =>
      }
    }

    Ok(())
  }

  private def checkTemplateInstantiationsForEnv(
    env: IEnvironment, // See CSSNCE
    state: CompilerOutputs,
    ranges: List[RangeS],
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Result[Unit, ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    rules.foreach({
      case r@CallSR(_, _, _, _) => checkTemplateCall(env, state, ranges, r, conclusions)
      case r@ResolveSR(_, _, _, _, _) => {
        checkFunctionCall(env, state, ranges, r, conclusions) match {
          case Ok(()) =>
          case Err(e) => return Err(e)
        }
      }
      case _ =>
    })
    Ok(())
  }

  def checkFunctionCall(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    ranges: List[RangeS],
    c: ResolveSR,
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Result[Unit, ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val ResolveSR(range, resultRune, name, paramsListRune, returnRune) = c

    // If it was an incomplete solve, then just skip.
    val returnCoord =
      conclusions.get(returnRune.rune) match {
        case Some(CoordTemplata(t)) => t
        case None => return Ok(())
      }
    val paramCoords =
      conclusions.get(paramsListRune.rune) match {
        case None => return Ok(())
        case Some(CoordListTemplata(paramList)) => paramList
      }

    val prototypeTemplata =
      delegate.resolveFunction(callingEnv, state, range :: ranges, name, paramCoords, true) match {
        case Err(e) => return Err(RuleError(CouldntFindFunction(range :: ranges, e)))
        case Ok(x) => x
      }

    if (prototypeTemplata.prototype.returnType != returnCoord) {
      throw CompileErrorExceptionT(RangedInternalErrorT(range :: ranges, "Return type conflict"))
    }
    Ok(())
  }

  def checkTemplateCall(
    callingEnv: IEnvironment,
    state: CompilerOutputs,
    ranges: List[RangeS],
    c: CallSR,
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Unit = {
    val CallSR(range, resultRune, templateRune, argRunes) = c

    // If it was an incomplete solve, then just skip.
    val template =
      conclusions.get(templateRune.rune) match {
        case Some(t) => t
        case None => return
      }
    val args =
      argRunes.map(argRune => {
        conclusions.get(argRune.rune) match {
          case Some(t) => t
          case None => return
        }
      })

    template match {
      case RuntimeSizedArrayTemplateTemplata() => {
        val Array(m, CoordTemplata(coord)) = args
        val mutability = ITemplata.expectMutability(m)
        delegate.resolveRuntimeSizedArrayKind(state, coord, mutability)
      }
      case it @ StructTemplata(_, _) => {
        delegate.resolveStruct(callingEnv, state, range :: ranges, it, args.toVector, true)
      }
      case it @ InterfaceTemplata(_, _) => {
        delegate.resolveInterface(callingEnv, state, range :: ranges, it, args.toVector, true)
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
      case _ => true
    }
  }

  // Some rules should be excluded from the call site, see SROACSD.
  def includeRuleInDefinitionSolve(rule: IRulexSR): Boolean = {
    rule match {
      case CallSiteFuncSR(_, _, _, _, _) => false
      case ResolveSR(_, _, _, _, _) => false
      case _ => true
    }
  }
}