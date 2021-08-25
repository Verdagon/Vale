package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl}

import scala.collection.mutable.ArrayBuffer

object Planner {
  private[solver] def makePlannerState[RuleID, Literal, Lookup](
    world: World[Int, RuleID, Literal, Lookup]):
  PlannerState[RuleID, Literal, Lookup] = {
    val numPuzzles = world.puzzleToRunes.length

    val puzzleToIndexInNumUnknowns: Array[Int] = world.puzzleToRule.indices.map(_ => -1).toArray
    val numUnknownsToPuzzlesBuilders =
      Array(
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int]())
    world.puzzleToRunes.zipWithIndex.foreach({ case (runesForPuzzle, puzzleIndex) =>
      val numUnknownRunes = runesForPuzzle.length
      val indexInNumUnknowns = numUnknownsToPuzzlesBuilders(numUnknownRunes).size
      numUnknownsToPuzzlesBuilders(numUnknownRunes) += puzzleIndex
      puzzleToIndexInNumUnknowns(puzzleIndex) = indexInNumUnknowns
    })
    val numUnknownsToNumPuzzles = numUnknownsToPuzzlesBuilders.map(_.length)
    val numUnknownsToPuzzles =
      numUnknownsToPuzzlesBuilders.map(puzzles => {
        // Fill in the rest with -1s
        puzzles ++= (0 until (numPuzzles - puzzles.length)).map(_ => -1)
        vassert(puzzles.length == numPuzzles)
        puzzles.toArray.clone()
      })

    val runeToIsSolved = world.runeToPuzzles.map(_ => false)

    val puzzleToSatisfied = world.puzzleToRule.indices.map(_ => false).toArray
    PlannerState[RuleID, Literal, Lookup](
      world,
      puzzleToSatisfied.clone(),
      world.puzzleToRunes.map(_.length),
      world.puzzleToRunes.map(_.clone()),
      puzzleToIndexInNumUnknowns,
      numUnknownsToNumPuzzles,
      numUnknownsToPuzzles,
      runeToIsSolved.clone())
  }

  def solveAndReorder[RuneID, RuleID, Literal, Lookup](
    world: World[Int, RuleID, Literal, Lookup],
    suppliedCanonicalRunes: Iterable[Int]):
  (Array[IRulexAR[Int, RuleID, Literal, Lookup]], Array[Boolean]) = {
    val plannerState = makePlannerState(world)
    suppliedCanonicalRunes.foreach(suppliedCanonicalRune => plannerState.markRuneKnown(suppliedCanonicalRune))
    val (ruleExecutionOrder, canonicalRuneToIsSolved) = solve(plannerState)
    val orderedRules = ruleExecutionOrder.map(ruleIndex => world.rules(ruleIndex))
    (orderedRules, canonicalRuneToIsSolved)
  }

  def solve[RuleID, Literal, Lookup](
    originalPlannerState: PlannerState[RuleID, Literal, Lookup]
  ): (Array[Int], Array[Boolean]) = {
    val world = originalPlannerState.world
    val plannerState = originalPlannerState.deepClone()

    plannerState.sanityCheck()

    var numRulesExecuted = 0
    val orderedRules: Array[Int] = plannerState.world.rules.map(_ => -1)

    while (plannerState.numUnknownsToNumPuzzles(0) > 0) {
      vassert(plannerState.numUnknownsToPuzzles(0)(0) >= 0)

      val numSolvableRules = plannerState.numUnknownsToNumPuzzles(0)

      val solvingPuzzle = plannerState.numUnknownsToPuzzles(0)(numSolvableRules - 1)
      vassert(solvingPuzzle >= 0)
      vassert(plannerState.puzzleToIndexInNumUnknowns(solvingPuzzle) == numSolvableRules - 1)

      val solvingRule = world.puzzleToRule(solvingPuzzle)
      val ruleRunes = world.ruleToRunes(solvingRule)

      world.ruleToPuzzles(solvingRule).foreach(rulePuzzle => {
        vassert(!plannerState.puzzleToExecuted(rulePuzzle))
      })

      orderedRules(numRulesExecuted) = solvingRule
      numRulesExecuted = numRulesExecuted + 1

      world.ruleToPuzzles(solvingRule).foreach(rulePuzzle => {
        plannerState.puzzleToExecuted(rulePuzzle) = true
      })

      ruleRunes.foreach({ case newlySolvedRune =>
        if (!plannerState.runeToIsSolved(newlySolvedRune)) {
          plannerState.runeToIsSolved(newlySolvedRune) = true
          plannerState.markRuneKnown(newlySolvedRune)
        }
      })

      markRuleSolved(plannerState, solvingRule)

      plannerState.sanityCheck()
    }

    (orderedRules.slice(0, numRulesExecuted), plannerState.runeToIsSolved)

//    // We're just doublechecking here, we could disable all this
//    val complete =
//      plannerState.world.puzzleToRunes.indices.forall({ case puzzleIndex =>
//        val numUnknownRunes = plannerState.puzzleToNumUnknownRunes(puzzleIndex);
//        vassert(numUnknownRunes != 0) // Should have been attempted in the main solver loop
//        if (plannerState.puzzleToNumUnknownRunes(puzzleIndex) == -1) {
//          vassert(plannerState.puzzleToExecuted(puzzleIndex))
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

  def markRuleSolved[RuleID, Literal, Lookup](
    plannerState: PlannerState[RuleID, Literal, Lookup],
    rule: Int) = {
    val puzzlesForRule = plannerState.world.ruleToPuzzles(rule)
    puzzlesForRule.foreach(puzzle => {
      val numUnknowns = plannerState.puzzleToNumUnknownRunes(puzzle)
      vassert(numUnknowns == 0)
      plannerState.puzzleToNumUnknownRunes(puzzle) = -1
      val indexInNumUnknowns = plannerState.puzzleToIndexInNumUnknowns(puzzle)

      val oldNumPuzzlesInNumUnknownsBucket = plannerState.numUnknownsToNumPuzzles(0)
      val lastSlotInNumUnknownsBucket = oldNumPuzzlesInNumUnknownsBucket - 1

      // Swap the last one into this spot
      val newPuzzleForThisSpot = plannerState.numUnknownsToPuzzles(0)(lastSlotInNumUnknownsBucket)
      plannerState.numUnknownsToPuzzles(0)(indexInNumUnknowns) = newPuzzleForThisSpot

      // We just moved something in the numUnknownsToPuzzle, so we have to update that thing's knowledge of
      // where it is in the list.
      plannerState.puzzleToIndexInNumUnknowns(newPuzzleForThisSpot) = indexInNumUnknowns

      // Mark our position as -1
      plannerState.puzzleToIndexInNumUnknowns(puzzle) = -1

      // Clear the last slot to -1
      plannerState.numUnknownsToPuzzles(0)(lastSlotInNumUnknownsBucket) = -1

      // Reduce the number of puzzles in that bucket by 1
      val newNumPuzzlesInNumUnknownsBucket = oldNumPuzzlesInNumUnknownsBucket - 1
      plannerState.numUnknownsToNumPuzzles(0) = newNumPuzzlesInNumUnknownsBucket
    })
  }
}

case class PlannerState[RuleID, Literal, Lookup](
  world: World[Int, RuleID, Literal, Lookup], // immutable

  // For each rule, whether it's been actually executed or not
  puzzleToExecuted: Array[Boolean],

  // Together, these basically form a Array[Vector[Int]]
  puzzleToNumUnknownRunes: Array[Int],
  puzzleToUnknownRunes: Array[Array[Int]],
  // This is the puzzle's index in the below numUnknownsToPuzzle map.
  puzzleToIndexInNumUnknowns: Array[Int],

  // Together, these basically form a Array[Vector[Int]]
  // which will have five elements: 0, 1, 2, 3, 4
  // At slot 4 is all the puzzles that have 4 unknowns left
  // At slot 3 is all the puzzles that have 3 unknowns left
  // At slot 2 is all the puzzles that have 2 unknowns left
  // At slot 1 is all the puzzles that have 1 unknowns left
  // At slot 0 is all the puzzles that have 0 unknowns left
  // We will:
  // - Move a puzzle from one set to the next set if we solve one of its runes
  // - Solve any puzzle that has 0 unknowns left
  numUnknownsToNumPuzzles: Array[Int],
  numUnknownsToPuzzles: Array[Array[Int]],

  // For each rune, whether it's solved already
  runeToIsSolved: Array[Boolean]
) {
  override def hashCode(): Int = vfail() // is mutable, should never be hashed

  def deepClone(): PlannerState[RuleID, Literal, Lookup] = {
    PlannerState(
      world,
      puzzleToExecuted.clone(),
      puzzleToNumUnknownRunes.clone(),
      puzzleToUnknownRunes.map(_.clone()).clone(),
      puzzleToIndexInNumUnknowns.clone(),
      numUnknownsToNumPuzzles.clone(),
      numUnknownsToPuzzles.map(_.clone()).clone(),
      runeToIsSolved.clone())
  }

  def markRuneKnown(newlySolvedRune: Int) = {
    val puzzlesWithNewlySolvedRune = world.runeToPuzzles(newlySolvedRune)

    puzzlesWithNewlySolvedRune.foreach(puzzle => {
      val puzzleRunes = world.puzzleToRunes(puzzle)
      vassert(puzzleRunes.contains(newlySolvedRune))

      val oldNumUnknownRunes = puzzleToNumUnknownRunes(puzzle)
      val newNumUnknownRunes = oldNumUnknownRunes - 1
      // == newNumUnknownRunes because we already registered it as a conclusion
      vassert(puzzleRunes.count(!runeToIsSolved(_)) == newNumUnknownRunes)
      puzzleToNumUnknownRunes(puzzle) = newNumUnknownRunes

      val puzzleUnknownRunes = puzzleToUnknownRunes(puzzle)

      // Should be O(5), no rule has more than 5 unknowns
      val indexOfNewlySolvedRune = puzzleUnknownRunes.indexOf(newlySolvedRune)
      vassert(indexOfNewlySolvedRune >= 0)
      // Swap the last thing into this one's place
      puzzleUnknownRunes(indexOfNewlySolvedRune) = puzzleUnknownRunes(newNumUnknownRunes)
      // This is unnecessary, but might make debugging easier
      puzzleUnknownRunes(newNumUnknownRunes) = -1

      vassert(
        puzzleUnknownRunes.slice(0, newNumUnknownRunes).distinct.sorted sameElements
          puzzleRunes.filter(!runeToIsSolved(_)).distinct.sorted)

      val oldNumUnknownsBucket = numUnknownsToPuzzles(oldNumUnknownRunes)

      val oldNumUnknownsBucketOldSize = numUnknownsToNumPuzzles(oldNumUnknownRunes)
      vassert(oldNumUnknownsBucketOldSize == oldNumUnknownsBucket.count(_ >= 0))
      val oldNumUnknownsBucketNewSize = oldNumUnknownsBucketOldSize - 1
      numUnknownsToNumPuzzles(oldNumUnknownRunes) = oldNumUnknownsBucketNewSize

      val indexOfPuzzleInOldNumUnknownsBucket = puzzleToIndexInNumUnknowns(puzzle)
      vassert(indexOfPuzzleInOldNumUnknownsBucket == oldNumUnknownsBucket.indexOf(puzzle))

      // Swap the last thing into this one's place
      val newPuzzleForThisSpotInOldNumUnknownsBucket = oldNumUnknownsBucket(oldNumUnknownsBucketNewSize)
      vassert(puzzleToIndexInNumUnknowns(newPuzzleForThisSpotInOldNumUnknownsBucket) == oldNumUnknownsBucketNewSize)
      oldNumUnknownsBucket(indexOfPuzzleInOldNumUnknownsBucket) = newPuzzleForThisSpotInOldNumUnknownsBucket
      puzzleToIndexInNumUnknowns(newPuzzleForThisSpotInOldNumUnknownsBucket) = indexOfPuzzleInOldNumUnknownsBucket
      // This is unnecessary, but might make debugging easier
      oldNumUnknownsBucket(oldNumUnknownsBucketNewSize) = -1

      val newNumUnknownsBucketOldSize = numUnknownsToNumPuzzles(newNumUnknownRunes)
      val newNumUnknownsBucketNewSize = newNumUnknownsBucketOldSize + 1
      numUnknownsToNumPuzzles(newNumUnknownRunes) = newNumUnknownsBucketNewSize

      val newNumUnknownsBucket = numUnknownsToPuzzles(newNumUnknownRunes)
      vassert(newNumUnknownsBucket(newNumUnknownsBucketOldSize) == -1)
      val indexOfPuzzleInNewNumUnknownsBucket = newNumUnknownsBucketOldSize
      newNumUnknownsBucket(indexOfPuzzleInNewNumUnknownsBucket) = puzzle

      puzzleToIndexInNumUnknowns(puzzle) = indexOfPuzzleInNewNumUnknownsBucket
    })
  }

  def sanityCheck() = {
    puzzleToExecuted.zipWithIndex.foreach({ case (executed, puzzle) =>
      if (executed) {
        vassert(puzzleToIndexInNumUnknowns(puzzle) == -1)
        vassert(puzzleToNumUnknownRunes(puzzle) == -1)
        world.puzzleToRunes(puzzle).foreach(rune => vassert(runeToIsSolved(rune)))
        puzzleToUnknownRunes(puzzle).foreach(unknownRune => vassert(unknownRune == -1))
        numUnknownsToPuzzles.foreach(_.foreach(p => vassert(p != puzzle)))
      } else {
        // An un-executed puzzle might have all known runes. It just means that it hasn't been
        // executed yet, it'll probably be executed very soon.

        vassert(puzzleToIndexInNumUnknowns(puzzle) != -1)
        vassert(puzzleToNumUnknownRunes(puzzle) != -1)

        // Make sure it only appears in one place in numUnknownsToPuzzles
        vassert(numUnknownsToPuzzles.flatMap(_.map(p => if (p == puzzle) 1 else 0)).sum == 1)
      }
    })

    puzzleToNumUnknownRunes.zipWithIndex.foreach({ case (numUnknownRunes, puzzle) =>
      if (numUnknownRunes == -1) {
        // If numUnknownRunes is -1, then it's been marked solved, and it should appear nowhere.
        vassert(puzzleToUnknownRunes(puzzle).forall(_ == -1))
        vassert(!numUnknownsToPuzzles.exists(_.contains(puzzle)))
        vassert(puzzleToIndexInNumUnknowns(puzzle) == -1)
      } else {
        vassert(puzzleToUnknownRunes(puzzle).count(_ != -1) == numUnknownRunes)
        vassert(numUnknownsToPuzzles(numUnknownRunes).count(_ == puzzle) == 1)
        vassert(puzzleToIndexInNumUnknowns(puzzle) == numUnknownsToPuzzles(numUnknownRunes).indexOf(puzzle))
      }
      vassert((numUnknownRunes == -1) == puzzleToExecuted(puzzle))
    })

    puzzleToUnknownRunes.zipWithIndex.foreach({ case (unknownRunesWithNegs, puzzle) =>
      val unknownRunes = unknownRunesWithNegs.filter(_ != -1)
      val numUnknownRunes = unknownRunes.length
      if (puzzleToExecuted(puzzle)) {
        vassert(puzzleToNumUnknownRunes(puzzle) == -1)
      } else {
        if (numUnknownRunes == 0) {
          vassert(
            puzzleToNumUnknownRunes(puzzle) == 0 ||
              puzzleToNumUnknownRunes(puzzle) == -1)
        } else {
          vassert(puzzleToNumUnknownRunes(puzzle) == numUnknownRunes)
        }
      }
      unknownRunes.foreach(rune => vassert(!runeToIsSolved(rune)))
    })

    numUnknownsToNumPuzzles.zipWithIndex.foreach({ case (numPuzzles, numUnknowns) =>
      vassert(puzzleToNumUnknownRunes.count(_ == numUnknowns) == numPuzzles)
    })

    numUnknownsToPuzzles.zipWithIndex.foreach({ case (puzzlesWithNegs, numUnknowns) =>
      val puzzles = puzzlesWithNegs.filter(_ != -1)
      puzzles.foreach(puzzle => {
        vassert(puzzleToNumUnknownRunes(puzzle) == numUnknowns)
      })
    })
  }
}
