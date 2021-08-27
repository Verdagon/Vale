package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl}

sealed trait ISolverOutcome[RuneID, Conclusion, ErrType] {
  def getOrDie(): Map[RuneID, Conclusion]
}
case class CompleteSolve[RuneID, Conclusion, ErrType](
  conclusions: Map[RuneID, Conclusion]
) extends ISolverOutcome[RuneID, Conclusion, ErrType] {
  override def getOrDie(): Map[RuneID, Conclusion] = conclusions
}
case class IncompleteSolve[RuneID, Conclusion, ErrType](
  conclusions: Map[RuneID, Conclusion]
) extends ISolverOutcome[RuneID, Conclusion, ErrType] {
  override def getOrDie(): Map[RuneID, Conclusion] = vfail()
}

case class FailedSolve[RuneID, Conclusion, ErrType](
  error: ISolverError[RuneID, Conclusion, ErrType],
  conclusions: Map[RuneID, Conclusion]
) extends ISolverOutcome[RuneID, Conclusion, ErrType] {
  override def getOrDie(): Map[RuneID, Conclusion] = vfail()
}

sealed trait ISolverError[RuneID, Conclusion, ErrType]
case class SolverConflict[RuneID, Conclusion, ErrType](
  rule: Int,
  rune: RuneID,
  previousConclusion: Conclusion,
  newConclusion: Conclusion
) extends ISolverError[RuneID, Conclusion, ErrType]
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
    getConclusion: RuneID => Option[Conclusion],
    concludeRune: (RuneID, Conclusion) => Unit):
  Result[Unit, ErrType]
}

object Solver {
  def solve[Rule, RuneID, Env, State, Conclusion, ErrType](
    state: State,
    env: Env,
    numCanonicalRunes: Int,
    rules: IndexedSeq[Rule],
    ruleExecutionOrder: Iterable[Int],
    ruleToRunes: Rule => Iterable[RuneID],
    userRuneToCanonicalRune: RuneID => Int,
    allUserRunes: Iterable[RuneID],
    solveRule: ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType]
  ): Result[Stream[(RuneID, Conclusion)], FailedSolve[RuneID, Conclusion, ErrType]] = {
    val conclusions: Array[Option[Conclusion]] = (0 until numCanonicalRunes).map(_ => None).toArray
    def userifyConclusions(conclusions: Array[Option[Conclusion]]): Stream[(RuneID, Conclusion)] = {
      allUserRunes.toStream.flatMap(userRune => {
        conclusions(userRuneToCanonicalRune(userRune)).map(userRune -> _)
      })
    }

    ruleExecutionOrder.map(rules).zipWithIndex.foreach({ case (solvingRule, solvingRuleIndex) =>
      val ruleCanonicalRunes = ruleToRunes(solvingRule).map(userRuneToCanonicalRune).toArray

      val getConclusion =
        (requestedUserRune: RuneID) => {
          val requestedCanonicalRune = userRuneToCanonicalRune(requestedUserRune)
          vassert(ruleCanonicalRunes.contains(requestedCanonicalRune))
          conclusions(requestedCanonicalRune)
        }
      val concludeRune =
        (newlySolvedRune: RuneID, newConclusion: Conclusion) => {
          val newlySolvedCanonicalRune = userRuneToCanonicalRune(newlySolvedRune)
          vassert(ruleCanonicalRunes.contains(newlySolvedCanonicalRune))
          conclusions(newlySolvedCanonicalRune) match {
            case None => conclusions(newlySolvedCanonicalRune) = Some(newConclusion)
            case Some(existingConclusion) => {
              if (existingConclusion != newConclusion) {
                return Err(FailedSolve(SolverConflict(solvingRuleIndex, newlySolvedRune, existingConclusion, newConclusion), userifyConclusions(conclusions).toMap))
              }
            }
          }
          ()
        }
      solveRule.solve(state, env, solvingRuleIndex, solvingRule, getConclusion, concludeRune) match {
        case Ok(_) =>
        case Err(e) => return Err(FailedSolve(RuleError(solvingRuleIndex, e), userifyConclusions(conclusions).toMap))
      }
    })

    Ok(userifyConclusions(conclusions))
  }
}
