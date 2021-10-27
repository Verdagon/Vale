package net.verdagon.vale.solver

import net.verdagon.vale.{Err, Ok, Result, vassert, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ISolverStateForRule[Rule, RuneID, Conclusion] {
  def getConclusion(rune: RuneID): Option[Conclusion]
//  def concludeRune(rune: RuneID, conclusion: Conclusion)
  def addRule(rule: Rule, runes: Array[RuneID]): Int
  def addPuzzle(ruleIndex: Int, runes: Array[RuneID])
  def getUnsolvedRules(): Vector[Rule]
}

trait ISolverState[Rule, RuneID, Conclusion] {
  def deepClone(): ISolverState[Rule, RuneID, Conclusion]
  def getCanonicalRune(rune: RuneID): Int
  def getRule(ruleIndex: Int): Rule
  def getConclusion(rune: Int): Option[Conclusion]
  def getConclusions(): Stream[(Int, Conclusion)]
  def userifyConclusions(): Stream[(RuneID, Conclusion)]
  def userifyRune(rune: Int): RuneID
  def getUnsolvedRules(): Vector[Rule]
  def getNextSolvable(): Option[Int]

  def addRule(rule: Rule): Int
  def addRune(rune: RuneID): Int

  def getAllRunes(): Set[Int]
  def getAllRules(): Set[Int]

  def addPuzzle(ruleIndex: Int, runes: Array[Int]): Unit

  def concludeRune[ErrType](newlySolvedRune: Int, conclusion: Conclusion):
  Result[Boolean, FailedSolve[Rule, RuneID, Conclusion, ErrType]]

  def sanityCheck(): Unit

  // Success returns number of new conclusions
  def markRulesSolved[ErrType](ruleIndices: Array[Int], newConclusions: Map[Int, Conclusion]):
  Result[Int, FailedSolve[Rule, RuneID, Conclusion, ErrType]]
}
