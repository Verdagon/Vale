package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vassertSome, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object SimpleSolverState {
  def apply[Rule, RuneID, Conclusion](): SimpleSolverState[Rule, RuneID, Conclusion] = {
    SimpleSolverState[Rule, RuneID, Conclusion](
      Map[RuneID, Int](),
      Map[Int, RuneID](),
      Vector[Rule](),
      Map[Int, Array[Array[Int]]](),
      Map[Int, Conclusion]())
  }
}

case class SimpleSolverState[Rule, RuneID, Conclusion](
  private var userRuneToCanonicalRune: Map[RuneID, Int],
  private var canonicalRuneToUserRune: Map[Int, RuneID],

  private var rules: Vector[Rule],

  private var openRuleToPuzzleToRunes: Map[Int, Array[Array[Int]]],

  private var canonicalRuneToConclusion: Map[Int, Conclusion]
) extends ISolverState[Rule, RuneID, Conclusion] {

  override def hashCode(): Int = vfail() // is mutable, should never be hashed

  def deepClone(): SimpleSolverState[Rule, RuneID, Conclusion] = {
    SimpleSolverState(
      userRuneToCanonicalRune,
      canonicalRuneToUserRune,
      rules,
      openRuleToPuzzleToRunes,
      canonicalRuneToConclusion)
  }

  override def sanityCheck(): Unit = {
  }

  override def getRule(ruleIndex: Int): Rule = rules(ruleIndex)

  override def getConclusion(rune: Int): Option[Conclusion] = canonicalRuneToConclusion.get(rune)

  override def userifyRune(rune: Int): RuneID = canonicalRuneToUserRune(rune)

  override def getConclusions(): Stream[(Int, Conclusion)] = {
    canonicalRuneToConclusion.toStream
  }

  override def userifyConclusions(): Stream[(RuneID, Conclusion)] = {
    canonicalRuneToConclusion
      .toStream
      .map({ case (canonicalRune, conclusion) => (canonicalRuneToUserRune(canonicalRune), conclusion) })
  }

  override def getAllRunes(): Set[Int] = {
    openRuleToPuzzleToRunes.values.flatten.flatten.toSet
  }

  override def addRune(rune: RuneID): Int = {
    vassert(!userRuneToCanonicalRune.contains(rune))
    val newCanonicalRune = userRuneToCanonicalRune.size
    userRuneToCanonicalRune = userRuneToCanonicalRune + (rune -> newCanonicalRune)
    canonicalRuneToUserRune = canonicalRuneToUserRune + (newCanonicalRune -> rune)
    newCanonicalRune
  }

  override def getAllRules(): Set[Int] = {
    openRuleToPuzzleToRunes.keySet
  }

  override def addRule(rule: Rule): Int = {
    vassert(!rules.contains(rule))
    val newCanonicalRule = rules.size
    rules = rules :+ rule
//    canonicalRuleToUserRule = canonicalRuleToUserRule + (newCanonicalRule -> rule)
    newCanonicalRule
  }

  override def getCanonicalRune(rune: RuneID): Int = {
    vassertSome(userRuneToCanonicalRune.get(rune))
  }

  override def addPuzzle(ruleIndex: Int, runes: Array[Int]): Unit = {
    val thisRulePuzzleToRunes = openRuleToPuzzleToRunes.getOrElse(ruleIndex, Array())
    openRuleToPuzzleToRunes = openRuleToPuzzleToRunes + (ruleIndex -> (thisRulePuzzleToRunes :+ runes))
  }

  override def getNextSolvable(): Option[Int] = {
    openRuleToPuzzleToRunes
      .filter({ case (_, puzzleToRunes) =>
        puzzleToRunes.exists(runes => {
          runes.forall(rune => canonicalRuneToConclusion.contains(rune))
        })
      })
      // Get rule with lowest ID, keep it deterministic
      .keySet
      .headOption
  }
//
//  def getConclusion(rune: Int): Option[Conclusion] = {
//    runeToConclusion(rune)
//  }
//
//  def getConclusions(): Array[Option[Conclusion]] = {
//    runeToConclusion.toArray
//  }
//
//  def userifyConclusions(): Stream[(RuneID, Conclusion)] = {
//    userRuneToCanonicalRune.toStream.flatMap({ case (userRune, canonicalRune) =>
//      runeToConclusion(canonicalRune).map(userRune -> _)
//    })
//  }

  override def getUnsolvedRules(): Vector[Rule] = {
    openRuleToPuzzleToRunes.keySet.toVector.map(rules)
  }

  // Returns whether it's a new conclusion
  override def concludeRune[ErrType](newlySolvedRune: Int, newConclusion: Conclusion):
  Result[Boolean, FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {
    val isNew =
      canonicalRuneToConclusion.get(newlySolvedRune) match {
        case Some(existingConclusion) => {
          if (existingConclusion != newConclusion) {
            return Err(
              FailedSolve(
                userifyConclusions().toMap,
                getUnsolvedRules(),
                SolverConflict(
                  canonicalRuneToUserRune(newlySolvedRune),
                  existingConclusion,
                  newConclusion)))
          }
          false
        }
        case None => true
      }
    canonicalRuneToConclusion = canonicalRuneToConclusion + (newlySolvedRune -> newConclusion)
    Ok(isNew)
  }

  // Success returns number of new conclusions
  override def markRulesSolved[ErrType](ruleIndices: Array[Int], newConclusions: Map[Int, Conclusion]):
  Result[Int, FailedSolve[Rule, RuneID, Conclusion, ErrType]] = {
    val numNewConclusions =
      newConclusions.map({ case (newlySolvedRune, newConclusion) =>
        concludeRune[ErrType](newlySolvedRune, newConclusion) match {
          case Err(e) => return Err(e)
          case Ok(isNew) => isNew
        }
      }).count(_ == true)

    ruleIndices.foreach(removeRule)

    Ok(numNewConclusions)
  }

  private def removeRule(ruleIndex: Int) = {
    openRuleToPuzzleToRunes = openRuleToPuzzleToRunes - ruleIndex
  }
}
