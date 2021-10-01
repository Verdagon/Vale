package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl, vpass}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Planner {
//  def plan[Rule, RuneID, RuleID, Literal, Lookup, Conclusion](
//    rules: Iterable[Rule],
//    additionalRunes: Iterable[RuneID],
//    ruleToRunes: Rule => Array[RuneID],
//    ruleToPuzzles: Rule => Array[Array[RuneID]],
//    // Sometimes, we already know a rune up-front, such as if we inherit it from a containing interface,
//    // or if we're finally calling a generic function.
//    initiallyKnownRunes: Set[RuneID],
//    isEquals: PartialFunction[Rule, (RuneID, RuneID)]):
//  (Int, Map[RuneID, Int], Array[Int], Array[Boolean]) = {
//
////    val world =
////      analyze(
////        rules,
////        (rule: Rule) => !isEquals.isDefinedAt(rule),
////        numCanonicalRunes,
////        (rule: Rule) => {
////          vpass()
////          ruleToRunes(rule).map(userRuneToCanonicalRune).distinct
////        },
////        (rule: Rule) => ruleToPuzzles(rule).map(_.map(userRuneToCanonicalRune).distinct))
////
////    val initiallyKnownCanonicalRunes = mutable.Set[Int]()
////    initiallyKnownRunes.foreach(knownRune => {
////      initiallyKnownCanonicalRunes += userRuneToCanonicalRune(knownRune)
////    })
////
////    val solverState = makeSolverState(world)
//
//      solve(solverState, initiallyKnownRunes)
//
//  }

//  private[solver] def canonicalizeRunes[RuneID](allUserRunes: Iterable[RuneID], equalRunes: Iterable[(RuneID, RuneID)]):
//  (Int, Map[RuneID, Int]) = {
//    val userRuneToOrder = mutable.HashMap[RuneID, Int]()
//    allUserRunes.zipWithIndex.foreach({ case (userRune, index) =>
//      userRuneToOrder.put(userRune, index)
//    })
//
//    val userRuneToEarlierEqualUserRune = mutable.HashMap[RuneID, RuneID]()
//    equalRunes.foreach({ case (leftRune, rightRune) =>
//      if (leftRune != rightRune) {
//        if (userRuneToOrder(leftRune) < userRuneToOrder(rightRune)) {
//          userRuneToEarlierEqualUserRune.put(rightRune, leftRune)
//        } else {
//          userRuneToEarlierEqualUserRune.put(leftRune, rightRune)
//        }
//      }
//    })
//
//    val userRuneToEarliestEqualUserRune = mutable.HashMap[RuneID, RuneID]()
//    // The below two are inverses of each other
//    val earliestEqualUserRuneToIndex = mutable.HashMap[RuneID, Int]()
//    val earliestEqualUserRunes = mutable.ArrayBuffer[RuneID]()
//
//    // This function will recursively dive through the userRuneToEarlierEqualUserRune map to,
//    // for a given rune, find an equal earlier rune, and find an equal earlier rune to *that*,
//    // and so on, until it finds the earliest one.
//    // The AndUpdate is because this updates the userRuneToEarliestEqualUserRune map.
//    def getAndUpdateEarliestEqualUserRune(rune: RuneID): RuneID = {
//      userRuneToEarliestEqualUserRune.get(rune) match {
//        case Some(earliest) => earliest
//        case None => {
//          val earliest =
//            userRuneToEarlierEqualUserRune.get(rune) match {
//              case None => {
//                // We've never seen this before, and theres nothing earlier equal to it.
//                // It's an earliest! Let's add it to the list.
//                val earliestRuneIndex = earliestEqualUserRunes.size
//                earliestEqualUserRunes += rune
//                earliestEqualUserRuneToIndex.put(rune, earliestRuneIndex)
//                rune
//              }
//              case Some(equalPreexistingRune) => getAndUpdateEarliestEqualUserRune(equalPreexistingRune)
//            }
//          userRuneToEarliestEqualUserRune.put(rune, earliest)
//          earliest
//        }
//      }
//    }
//    allUserRunes.foreach(getAndUpdateEarliestEqualUserRune)
//
//    (earliestEqualUserRuneToIndex.size, userRuneToEarliestEqualUserRune.mapValues(earliestEqualUserRuneToIndex).toMap)
//  }

//  private[solver] def analyze[Rule, RuneID, RuleID, Literal, Lookup](
//    rules: Iterable[Rule],
//    considerRule: Rule => Boolean,
//    numCanonicalRunes: Int,
//    getRuleRunes: Rule => Array[Int],
//    getRulePuzzles: Rule => Array[Array[Int]]):
//  Analysis = {
//    val noopRules =
//      rules.zipWithIndex.filter({ case (rule, _) => !considerRule(rule) }).map(_._2).toArray
//
//    val ruleIndexToPuzzles =
//      rules.map(rule => {
//        if (considerRule(rule)) {
//          // Skip this rule (e.g. it's an Equals rule)
//          getRulePuzzles(rule)
//        } else {
//          Array[Array[Int]]()
//        }
//      })
//
//    // We cant just merge all the puzzles' runes because some runes are never part of a puzzle, for example
//    // in literal rules or lookup rules, the result rune is never part of a puzzle.
//    val ruleToRunes =
//      rules.map(rule => {
//        if (considerRule(rule)) {
//          // Skip this rule (e.g. it's an Equals rule)
//          getRuleRunes(rule)
//        } else {
//          Array[Int]()
//        }
//      })
//
//    val puzzlesToRuleAndUnknownRunes = ArrayBuffer[(Int, Array[Int])]()
//    val ruleToPuzzles = rules.map(_ => ArrayBuffer[Int]()).toArray
//    val runeToPuzzles = (0 until numCanonicalRunes).map(_ => ArrayBuffer[Int]()).toArray
//    ruleIndexToPuzzles.zipWithIndex.foreach({ case (puzzlesForRule, ruleIndex) =>
//      puzzlesForRule.foreach(puzzleUnknownRunes => {
//        val puzzle = puzzlesToRuleAndUnknownRunes.size
//
//        val thing = (ruleIndex, puzzleUnknownRunes)
//        puzzlesToRuleAndUnknownRunes += thing
//        ruleToPuzzles(ruleIndex) += puzzle
//        puzzleUnknownRunes.foreach(unknownRune => {
//          runeToPuzzles(unknownRune) += puzzle
//        })
//      })
//    })
//
//    val puzzleToRule = puzzlesToRuleAndUnknownRunes.map(_._1)
//    val puzzleToUnknownRunes = puzzlesToRuleAndUnknownRunes.map(_._2)
//
//    puzzleToUnknownRunes.foreach(unknownRunes => {
//      vassert(unknownRunes.length == unknownRunes.distinct.length)
//    })
//
//    Analysis(
//      ruleToRunes.toArray,
//      puzzleToRule.toArray,
//      puzzleToUnknownRunes.map(_.clone()).toArray,
//      ruleToPuzzles.map(_.toArray),
//      runeToPuzzles.map(_.toArray),
//      noopRules)
//  }


//  private[solver] def makeSolverState[RuleID, Literal, Lookup](
//    world: Analysis):
//  SolverState = {
//    val numPuzzles = solverState.puzzleToRunes.length
//
//    val puzzleToIndexInNumUnknowns: Array[Int] = solverState.puzzleToRule.indices.map(_ => -1).toArray
//    val numUnknownsToPuzzlesBuilders =
//      Array(
//        ArrayBuffer[Int](),
//        ArrayBuffer[Int](),
//        ArrayBuffer[Int](),
//        ArrayBuffer[Int](),
//        ArrayBuffer[Int]())
//    solverState.puzzleToRunes.zipWithIndex.foreach({ case (runesForPuzzle, puzzleIndex) =>
//      val numUnknownRunes = runesForPuzzle.length
//      val indexInNumUnknowns = numUnknownsToPuzzlesBuilders(numUnknownRunes).size
//      numUnknownsToPuzzlesBuilders(numUnknownRunes) += puzzleIndex
//      puzzleToIndexInNumUnknowns(puzzleIndex) = indexInNumUnknowns
//    })
//    val numUnknownsToNumPuzzles = numUnknownsToPuzzlesBuilders.map(_.length)
//    val numUnknownsToPuzzles =
//      numUnknownsToPuzzlesBuilders.map(puzzles => {
//        // Fill in the rest with -1s
//        puzzles ++= (0 until (numPuzzles - puzzles.length)).map(_ => -1)
//        vassert(puzzles.length == numPuzzles)
//        puzzles.toArray.clone()
//      })
//
//    val runeToIsSolved = solverState.runeToPuzzles.map(_ => false)
//
//    val puzzleToSatisfied = solverState.puzzleToRule.indices.map(_ => false).toArray
//    val state =
//      SolverState(
//        world,
//        puzzleToSatisfied.clone(),
//        solverState.puzzleToRunes.map(_.length),
//        solverState.puzzleToRunes.map(_.clone()),
//        puzzleToIndexInNumUnknowns,
//        numUnknownsToNumPuzzles,
//        numUnknownsToPuzzles,
//        runeToIsSolved.clone())
//    solverState.noopRules.foreach(noopRule => {
//      state.markRuleSolved(noopRule)
//    })
//    state
//  }

//  def solveAndReorder[RuneID, RuleID, Literal, Lookup](
//    world: Analysis,
//    suppliedCanonicalRunes: Iterable[Int]):
//  (Array[Int], Array[Boolean]) = {
//    val solverState = makeSolverState(world)
//    suppliedCanonicalRunes.foreach(suppliedCanonicalRune => solverState.markRuneKnown(suppliedCanonicalRune))
//    val (ruleExecutionOrder, canonicalRuneToIsSolved) = solve(solverState)
//    (ruleExecutionOrder, canonicalRuneToIsSolved)
//  }

  def solve[Rule, RuneID, Env, State, Conclusion, ErrType](
    state: State,
    env: Env,
    solverState: SolverState[Rule, RuneID, Conclusion],
    solveRule: ISolveRule[Rule, RuneID, Env, State, Conclusion, ErrType]
  ): Result[Stream[(RuneID, Conclusion)], FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {
    //    var numRulesExecuted = 0
//    val orderedRules: Array[Int] = solverState.ruleToRunes.map(_ => -1)

    while (solverState.hasNextSolvable()) {
      val (solvingRuleIndex, solvingPuzzleIndex, ruleRunes) = solverState.getNextSolvable()

      val solverStateForRule =
        new ISolverStateForRule[Rule, RuneID, Conclusion] {
          override def getConclusion(requestedUserRune: RuneID): Option[Conclusion] = {
            val requestedCanonicalRune = solverState.getCanonicalRune(requestedUserRune)
            vassert(ruleRunes.contains(requestedCanonicalRune))
            solverState.getConclusion(requestedCanonicalRune)
          }
          override def addPuzzle(ruleIndex: Int, runes: Array[RuneID]): Unit = {
            solverState.addPuzzle(ruleIndex, runes.map(solverState.getCanonicalRune))
          }
          override def addRule(rule: Rule, runes: Array[RuneID]): Int = {
            solverState.addRule(rule, runes.map(solverState.getCanonicalRune))
          }
        }
      val newConclusions =
        solveRule.solve(state, env, solvingRuleIndex, solverState.getRule(solvingRuleIndex), solverStateForRule) match {
          case Ok(c) => c
          case Err(e) => return Err(
            FailedSolve(
              solverState.userifyConclusions().toMap,
              solverState.getUnsolvedRules(),
              RuleError(solvingRuleIndex, e)))
        }

      newConclusions.foreach({ case (newlySolvedRune, newConclusion) =>
        val newlySolvedCanonicalRune = solverState.getCanonicalRune(newlySolvedRune)
        vassert(ruleRunes.contains(newlySolvedCanonicalRune))
        solverState.getConclusion(newlySolvedCanonicalRune) match {
          case None => solverState.concludeRune(newlySolvedCanonicalRune, newConclusion)
          case Some(existingConclusion) => {
            if (existingConclusion != newConclusion) {
              return Err(
                FailedSolve(
                  solverState.userifyConclusions().toMap,
                  solverState.getUnsolvedRules(),
                  SolverConflict(solvingRuleIndex, newlySolvedRune, existingConclusion, newConclusion)))
            }
          }
        }
      })

      solverState.markRuleSolved(
        solvingRuleIndex,
        newConclusions.map({ case (userRune, conclusion) => (solverState.getCanonicalRune(userRune), conclusion) }))

      solverState.sanityCheck()
    }

    Ok(solverState.userifyConclusions())

//    // We're just doublechecking here, we could disable all this
//    val complete =
//      solverState.puzzleToRunes.indices.forall({ case puzzleIndex =>
//        val numUnknownRunes = solverState.puzzleToNumUnknownRunes(puzzleIndex);
//        vassert(numUnknownRunes != 0) // Should have been attempted in the main solver loop
//        if (solverState.puzzleToNumUnknownRunes(puzzleIndex) == -1) {
//          vassert(solverState.puzzleToExecuted(puzzleIndex))
//          true
//        } else {
//          // An incomplete solve
//          false
//        }
//      })
//
//    if (complete) {
//      CompletePlan(runeToIsSolved, orderedRules.slice(0, numRulesExecuted))
//    } else {
//      IncompletePlan(runeToIsSolved, orderedRules.slice(0, numRulesExecuted))
//    }
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

//case class SolverState(
//  world: Analysis, // immutable
//
//  // For each rule, whether it's been actually executed or not
//  puzzleToExecuted: Array[Boolean],
//
//  // Together, these basically form a Array[Vector[Int]]
//  puzzleToNumUnknownRunes: Array[Int],
//  puzzleToUnknownRunes: Array[Array[Int]],
//  // This is the puzzle's index in the below numUnknownsToPuzzle map.
//  puzzleToIndexInNumUnknowns: Array[Int],
//
//  // Together, these basically form a Array[Vector[Int]]
//  // which will have five elements: 0, 1, 2, 3, 4
//  // At slot 4 is all the puzzles that have 4 unknowns left
//  // At slot 3 is all the puzzles that have 3 unknowns left
//  // At slot 2 is all the puzzles that have 2 unknowns left
//  // At slot 1 is all the puzzles that have 1 unknowns left
//  // At slot 0 is all the puzzles that have 0 unknowns left
//  // We will:
//  // - Move a puzzle from one set to the next set if we solve one of its runes
//  // - Solve any puzzle that has 0 unknowns left
//  numUnknownsToNumPuzzles: Array[Int],
//  numUnknownsToPuzzles: Array[Array[Int]],
//
//  // For each rune, whether it's solved already
//  runeToIsSolved: Array[Boolean]
//) {
//}
