package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl, vpass}

sealed trait ISolverOutcome[Rule, RuneID, Conclusion, ErrType] {
  def getOrDie(): Map[RuneID, Conclusion]
}
sealed trait IIncompleteOrFailedSolve[Rule, RuneID, Conclusion, ErrType] extends ISolverOutcome[Rule, RuneID, Conclusion, ErrType] {
  def unsolvedRules: Vector[Rule]
  def incompleteConclusions: Map[RuneID, Conclusion]
}
case class CompleteSolve[Rule, RuneID, Conclusion, ErrType](
  conclusions: Map[RuneID, Conclusion]
) extends ISolverOutcome[Rule, RuneID, Conclusion, ErrType] {
  override def getOrDie(): Map[RuneID, Conclusion] = conclusions
}
case class IncompleteSolve[Rule, RuneID, Conclusion, ErrType](
  incompleteConclusions: Map[RuneID, Conclusion],
  unsolvedRules: Vector[Rule],
  unknownRunes: Set[RuneID]
) extends IIncompleteOrFailedSolve[Rule, RuneID, Conclusion, ErrType] {
  vassert(unknownRunes.nonEmpty)
  vpass()
  override def getOrDie(): Map[RuneID, Conclusion] = vfail()
}

case class FailedSolve[Rule, RuneID, Conclusion, ErrType](
  incompleteConclusions: Map[RuneID, Conclusion],
  unsolvedRules: Vector[Rule],
  error: ISolverError[RuneID, Conclusion, ErrType]
) extends IIncompleteOrFailedSolve[Rule, RuneID, Conclusion, ErrType] {
  override def getOrDie(): Map[RuneID, Conclusion] = vfail()
  vpass()
}

sealed trait ISolverError[RuneID, Conclusion, ErrType]
case class SolverConflict[RuneID, Conclusion, ErrType](
  rule: Int,
  rune: RuneID,
  previousConclusion: Conclusion,
  newConclusion: Conclusion
) extends ISolverError[RuneID, Conclusion, ErrType] {
  vpass()
}
case class RuleError[RuneID, Conclusion, ErrType](
  ruleIndex: Int,
  err: ErrType
) extends ISolverError[RuneID, Conclusion, ErrType]

// Given enough user specified template params and param inputs, we should be able to
// infer everything.
// This class's purpose is to take those things, and see if it can figure out as many
// inferences as possible.

trait ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType] {
  def solve(
    state: State,
    env: Env,
    ruleIndex: Int,
    rule: Rule,
    solverState: ISolverStateForRule[Rule, RuneID, Conclusion]):
  Result[Map[RuneID, Conclusion], ErrType]
}

//object Solver {
//  def solve[Rule, RuneID, Env, State, Conclusion, ErrType](
//    state: State,
//    env: Env,
//    numCanonicalRunes: Int,
//    rules: IndexedSeq[Rule],
//    ruleExecutionOrder: Iterable[Int],
//    ruleToRunes: Rule => Iterable[RuneID],
//    userRuneToCanonicalRune: RuneID => Int,
//    allUserRunes: Iterable[RuneID],
//    initiallyKnownRunes: Map[RuneID, Conclusion],
//    solveRule: ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType]
//  ): Result[Stream[(RuneID, Conclusion)], FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {
//  }
//}
