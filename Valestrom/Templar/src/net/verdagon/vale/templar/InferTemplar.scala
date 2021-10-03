package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules.IRulexSR
import net.verdagon.vale.scout.IRuneS
import net.verdagon.vale.solver.{CompleteSolve, FailedSolve, IIncompleteOrFailedSolve, ISolverOutcome, IncompleteSolve, RuleError, SolverConflict}
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.vale.templar.citizen.{AncestorHelper, StructTemplar}
import net.verdagon.vale.templar.env.{IEnvironment, ILookupContext, TemplataLookupContext}
import net.verdagon.vale.templar.infer.{IInfererDelegate, _}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{Err, IProfiler, Ok, RangeS, Result, vassert, vassertSome, vfail, vimpl, vwat}

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class InferTemplar(
    opts: TemplarOptions,
    profiler: IProfiler,
    delegate: IInfererDelegate[IEnvironment, Temputs]) {
  def solveComplete(
    env: IEnvironment,
    temputs: Temputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    alreadyKnown: Map[IRuneS, ITemplata]):
  Result[Map[IRuneS, ITemplata], IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata, ITemplarSolverError]] = {
    solve(env, temputs, rules, runeToType, invocationRange, alreadyKnown) match {
      case f @ FailedSolve(_, _, _) => Err(f)
      case i @ IncompleteSolve(_, _, _) => Err(i)
      case CompleteSolve(conclusions) => Ok(conclusions)
    }
  }

  def solveExpectComplete(
    env: IEnvironment,
    temputs: Temputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    alreadyKnown: Map[IRuneS, ITemplata]):
  Map[IRuneS, ITemplata] = {
    solve(env, temputs, rules, runeToType, invocationRange, alreadyKnown) match {
      case f @ FailedSolve(_, _, err) => {
        throw CompileErrorExceptionT(TemplarSolverError(invocationRange, f))
      }
      case i @ IncompleteSolve(_, _, _) => {
        throw CompileErrorExceptionT(TemplarSolverError(invocationRange, i))
      }
      case CompleteSolve(conclusions) => conclusions
    }
  }

  def solve(
    env: IEnvironment,
    state: Temputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    alreadyKnown: Map[IRuneS, ITemplata]
  ): ISolverOutcome[IRulexSR, IRuneS, ITemplata, ITemplarSolverError] = {
    profiler.newProfile("infer", "", () => {
//      val output = ConstructingRuneWorldTR(ArrayBuffer(), mutable.HashMap(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer())
//      rules.map(translateRule(output, _))
//      val rulesTR = output.build()

      new TemplarSolver[IEnvironment, Temputs](delegate).solve(
          invocationRange,
          env,
          state,
          rules,
          runeToType,
          alreadyKnown)
    })
  }
}
