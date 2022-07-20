package dev.vale.typing

import dev.vale.{Err, Interner, Ok, Profiler, RangeS, Result, typing, vassert, vassertSome, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.infer.{CompilerSolver, IInfererDelegate, ITypingPassSolverError}
import dev.vale.typing.templata.ITemplata

case class InferEnv(
  // We look in this for declared functions, see CSSNCE.
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
        initialKnowns.map({ case InitialKnown(rune, templata) => rune.rune -> templata }).toMap ++
        initialSends.map({ case InitialSend(senderRune, _, senderTemplata) =>
          (senderRune.rune -> senderTemplata)
        })

      new CompilerSolver[InferEnv, CompilerOutputs](opts.globalOptions, interner, delegate)
        .solve(
          invocationRange,
          InferEnv(callingEnv, declaringEnv),
          state,
          rules,
          runeToType,
          alreadyKnown)
    })
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
      case ResolveSR(_, _, _, _) => false
      case _ => true
    }
  }
}