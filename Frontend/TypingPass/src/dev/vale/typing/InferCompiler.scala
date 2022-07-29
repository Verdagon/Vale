package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, Result, typing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.env.{CitizenEnvironment, EnvironmentHelper, GeneralEnvironment, GlobalEnvironment, IEnvEntry, IEnvironment, ILookupContext, IVariableT, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.infer.{CompilerSolver, CouldntFindFunction, IInfererDelegate, ITypingPassSolverError}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FullNameT, INameT, ITemplateNameT, NameTranslator, ResolvingEnvNameT, RuneNameT}
import dev.vale.typing.templata.{CoordListTemplata, CoordTemplata, ITemplata, InterfaceTemplata, KindTemplata, RuntimeSizedArrayTemplateTemplata, StructTemplata}

import scala.collection.immutable.Set

case class InferEnv(
  // We look in this for declared functions, see CSSNCE.
  // DO NOT SUBMIT we no longer need this because of SFWPRL
  callingEnv: Option[IEnvironment],
  // We look in this for everything else, such as type names like "int" etc.
  declaringEnv: IEnvironment,
)

case class InitialSend(
  senderRune: RuneUsage,
  receiverRune: RuneUsage,
  sendTemplata: ITemplata[ITemplataType])

case class InitialKnown(
  rune: RuneUsage,
  templata: ITemplata[ITemplataType])

class InferCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    nameTranslator: NameTranslator,
    delegate: IInfererDelegate[InferEnv, CompilerOutputs]) {
  def solveComplete(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]):
  Result[Map[IRuneS, ITemplata[ITemplataType]], IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    solve(declaringEnv, callingEnv, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends) match {
      case f @ FailedSolve(_, _, _) => Err(f)
      case i @ IncompleteSolve(_, _, _, _) => Err(i)
      case CompleteSolve(conclusions) => Ok(conclusions)
    }
  }

  def solveExpectComplete(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]):
  Map[IRuneS, ITemplata[ITemplataType]] = {
    solve(declaringEnv, callingEnv, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends) match {
      case f @ FailedSolve(_, _, err) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, f))
      }
      case i @ IncompleteSolve(_, _, _, _) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, i))
      }
      case CompleteSolve(conclusions) => conclusions
    }
  }


  def solve(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    state: CompilerOutputs,
    initialRules: Vector[IRulexSR],
    initialRuneToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]
  ): ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError] = {
    Profiler.frame(() => {
      val envs = InferEnv(callingEnv, declaringEnv)

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
            delegate.sanityCheckConclusion(envs, state, rune.rune, templata)
          }
          rune.rune -> templata
        }).toMap ++
        initialSends.map({ case InitialSend(senderRune, _, senderTemplata) =>
          if (opts.globalOptions.sanityCheck) {
            delegate.sanityCheckConclusion(envs, state, senderRune.rune, senderTemplata)
          }
          (senderRune.rune -> senderTemplata)
        })

      val outcome =
        new CompilerSolver[InferEnv, CompilerOutputs](opts.globalOptions, interner, delegate)
          .solve(
            invocationRange,
            envs,
            state,
            rules,
            runeToType,
            alreadyKnown)
      val conclusions: Map[IRuneS, ITemplata[ITemplataType]] =
        outcome match {
          case CompleteSolve(conclusions) => conclusions
          case IncompleteSolve(_, _, _, incompleteConclusions) => incompleteConclusions
          case FailedSolve(_, _, _) => Map()
        }

      // Now we need to actually resolve all the functions and stuff that we said existed in there, see SFWPRL.
      checkTemplateInstantiations(declaringEnv, callingEnv, state, rules.toArray, conclusions) match {
        case Ok(()) =>
        case Err(e) => return FailedSolve(Vector(), Vector(), e) // DO NOT SUBMIT
      }

      outcome
    })
  }

  def checkTemplateInstantiations(
    declaringEnv: IEnvironment,
    maybeCallingEnv: Option[IEnvironment], // See CSSNCE
    state: CompilerOutputs,
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
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
    val callingEnv =
      maybeCallingEnv match {
        case None => return Ok(())
        case Some(x) => x
      }
    val name = callingEnv.fullName.addStep(ResolvingEnvNameT())

//    val temporaryEnv =
//      GeneralEnvironment.childOf(
//        interner,
//        // "Caller" called us, now we're calling someone ("callee").
//        // We don't want our callee to see our caller. We want them to only see us.
//        // So, this temporary environment's parent is our own environment, not the caller's env.
//        // See OSDCE for more and an example.
//        // (or maybe this should just not have a parent?)
//        // or maybe we should not do things deeply.
//        declaringEnv,
//        name,
//        conclusions.map({case (nameS, templata) =>
//          interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
//        }).toVector)

    rules.foreach({
      case r @ CallSR(_, _, _, _) => {
        checkTemplateCall(InferEnv(maybeCallingEnv, declaringEnv), state, r, conclusions)
      }
      case r @ ResolveSR(_, _, _, _, _) => {
        checkFunctionCall(InferEnv(maybeCallingEnv, declaringEnv), state, r, conclusions) match {
          case Ok(_) =>
          case Err(e) => {
            return Err(e)
          }
        }
      }
      case _ =>
    })
    Ok(())
  }

  def checkFunctionCall(env: InferEnv, state: CompilerOutputs, c: ResolveSR, conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
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
      delegate.resolveFunction(env, state, range, name, paramCoords) match {
        case Err(e) => {
          return Err(RuleError(CouldntFindFunction(range, e)))
        }
        case Ok(x) => x
      }

    if (prototypeTemplata.prototype.returnType != returnCoord) {
      return Err(SolverConflict(returnRune.rune, CoordTemplata(returnCoord), CoordTemplata(prototypeTemplata.prototype.returnType)))
    }
    Ok(())
  }

  def checkTemplateCall(env: InferEnv, state: CompilerOutputs, c: CallSR, conclusions: Map[IRuneS, ITemplata[ITemplataType]]): Unit = {
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
        delegate.getRuntimeSizedArrayKind(env, state, coord, mutability)
      }
      case it @ StructTemplata(_, _) => {
        delegate.resolveStruct(env, state, range, it, args.toVector)
      }
      case it @ InterfaceTemplata(_, _) => {
        delegate.resolveInterface(env, state, range, it, args.toVector)
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