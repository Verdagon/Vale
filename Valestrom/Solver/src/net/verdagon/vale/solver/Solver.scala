package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl, vpass}

import scala.collection.immutable.Map

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
  rune: RuneID,
  previousConclusion: Conclusion,
  newConclusion: Conclusion
) extends ISolverError[RuneID, Conclusion, ErrType] {
  vpass()
}
case class RuleError[RuneID, Conclusion, ErrType](
//  ruleIndex: Int,
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

  // Called when we can't do any regular solves, we don't have enough
  // runes. This is where we do more interesting rules, like SMCMST.
  // See CSALR for more.
  def complexSolve(
    solverState: ISolverStateForRule[Rule, RuneID, Conclusion]
  ): Result[(Array[Int], Map[RuneID, Conclusion]), ErrType]
}

object Solver {
  def solve[Rule, RuneID, Env, State, Conclusion, ErrType](
    state: State,
    env: Env,
    solverState: SolverState[Rule, RuneID, Conclusion],
    solveRule: ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType]
  ): Result[Stream[(RuneID, Conclusion)], FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {

    while (solverState.hasNextSolvable()) {
      solveNextSolvableRule(env, state, solverState, solveRule) match {
        case Err(toReturn) => return Err(toReturn)
        case Ok(()) =>
      }
    }

    if (solverState.getUnsolvedRules().nonEmpty) {
      solveRule.complexSolve(makeSolverStateForRule(solverState)) match {
        case Ok((solvingRuleIndices, conclusions)) => {
          solverState.markRulesSolved[ErrType](solvingRuleIndices, conclusions) match {
            case Ok(0) => // Do nothing, we're done
            case Ok(_) => solve(state, env, solverState, solveRule)
            case Err(e) => return Err(e)
          }
        }
        case Err(e) => return Err(
          FailedSolve(
            solverState.userifyConclusions().toMap,
            solverState.getUnsolvedRules(),
            RuleError(e)))
      }
    }

    Ok(solverState.userifyConclusions())
  }

  private def makeSolverStateForRule[ErrType, Conclusion, State, Env, RuneID, Rule](
    solverState: SolverState[Rule, RuneID, Conclusion]
  ): ISolverStateForRule[Rule, RuneID, Conclusion] = {
    new ISolverStateForRule[Rule, RuneID, Conclusion] {
      override def getConclusion(requestedUserRune: RuneID): Option[Conclusion] = {
        val requestedCanonicalRune = solverState.getCanonicalRune(requestedUserRune)
        solverState.getConclusion(requestedCanonicalRune)
      }

      override def addPuzzle(ruleIndex: Int, runes: Array[RuneID]): Unit = {
        solverState.addPuzzle(ruleIndex, runes.map(solverState.getCanonicalRune))
      }

      override def addRule(rule: Rule, runes: Array[RuneID]): Int = {
        solverState.addRule(rule, runes.map(solverState.getCanonicalRune))
      }

      override def getUnsolvedRules(): Vector[Rule] = {
        solverState.getUnsolvedRules()
      }
    }
  }

  private def solveNextSolvableRule[ErrType, Conclusion, State, Env, RuneID, Rule](
      env: Env,
      state: State,
      solverState: SolverState[Rule, RuneID, Conclusion],
      solveRule: ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType]):
  Result[Unit, FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {
    val (solvingRuleIndex, solvingPuzzleIndex, ruleRunes) = solverState.getNextSolvable()

    val rule = solverState.getRule(solvingRuleIndex)
    val newConclusions =
      solveRule.solve(state, env, solvingRuleIndex, rule, makeSolverStateForRule(solverState)) match {
        case Ok(c) => c
        case Err(e) => return Err(
          FailedSolve(
            solverState.userifyConclusions().toMap,
            solverState.getUnsolvedRules(),
            RuleError(e)))
      }

    solverState.markRulesSolved[ErrType](Array(solvingRuleIndex), newConclusions) match {
      case Ok(_) =>
      case Err(e) => return Err(e)
    }

    solverState.sanityCheck()
    Ok(())
  }

  def makeInitialSolverState[Rule, RuneID, Conclusion](
    initialRules: IndexedSeq[Rule],
    ruleToRunes: Rule => Iterable[RuneID],
    ruleToPuzzles: Rule => Array[Array[RuneID]],
    initiallyKnownRunes: Map[RuneID, Conclusion]):
  SolverState[Rule, RuneID, Conclusion] = {
    val solverState = SolverState[Rule, RuneID, Conclusion]()

    initiallyKnownRunes.foreach({ case (rune, conclusion) =>
      solverState.concludeRune(solverState.getCanonicalRune(rune), conclusion)
    })

    initialRules.foreach(rule => {
      val ruleRunes = ruleToRunes(rule)
      val ruleCanonicalRunes =
        ruleRunes.map(rune => {
          solverState.getCanonicalRune(rune)
        })
      val ruleIndex = solverState.addRule(rule, ruleCanonicalRunes.toArray.distinct)
      ruleToPuzzles(rule).foreach(puzzleRunes => {
        solverState.addPuzzle(ruleIndex, puzzleRunes.map(solverState.getCanonicalRune).distinct)
      })
    })

    solverState.sanityCheck()
    solverState
  }
}
