package net.verdagon.vale.solver

import net.verdagon.vale.{Collector, Err, Ok, Result, vassert, vassertSome, vfail, vimpl, vwat}
import org.scalatest.{FunSuite, Matchers}

sealed trait ISimpleRule {
  def allRunes: Array[Long]
  def allPuzzles: Array[Array[Long]]
}
case class SimpleLiteral(rune: Long, value: String) extends ISimpleRule {
  override def allRunes: Array[Long] = Array(rune)
  override def allPuzzles: Array[Array[Long]] = Array(Array())
}
case class Equals(leftRune: Long, rightRune: Long) extends ISimpleRule {
  override def allRunes: Array[Long] = Array(leftRune, rightRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(leftRune), Array(rightRune))
}
case class CoordComponents(coordRune: Long, ownershipRune: Long, kindRune: Long) extends ISimpleRule {
  override def allRunes: Array[Long] = Array(coordRune, ownershipRune, kindRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(coordRune), Array(ownershipRune, kindRune))
}
case class OneOf(coordRune: Long, possibleValues: Array[String]) extends ISimpleRule {
  override def allRunes: Array[Long] = Array(coordRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(coordRune))
}
case class Call(resultRune: Long, name: String, argRune: Long) extends ISimpleRule {
  override def allRunes: Array[Long] = Array(resultRune, argRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(resultRune), Array(argRune))
}

class SolverTests extends FunSuite with Matchers with Collector {
  val complexRuleSet =
    Array(
      SimpleLiteral(-3L, "1448"),
      CoordComponents(-6L, -5L, -5L),
      SimpleLiteral(-2L, "1337"),
      Equals(-4L, -2L),
      OneOf(-4L, Array("1337", "73")),
      Equals(-1L, -5L),
      CoordComponents(-1L, -2L, -3L),
      Equals(-6L, -7L))
  val complexRuleSetEqualsRules = Array(3, 5, 7)

  //  def makePuzzler() = {
  //    new IRunePuzzler[Unit, SimpleLiteral, SimpleLookup] {
  //      override def getPuzzles(rulexAR: IRulexAR[Int, Unit, SimpleLiteral, SimpleLookup]): Array[Array[Int]] = {
  //        TemplarPuzzler.apply(rulexAR)
  //      }
  //    }
  //  }

  object RuleSolver extends ISolveRule[ISimpleRule, Long, Unit, Unit, String, String] {
    override def solve(
      state: Unit,
      env: Unit,
      ruleIndex: Int,
      rule: ISimpleRule,
      getConclusion: Long => Option[String],
      concludeRune: (Long, String) => Unit):
    Result[Unit, String] = {
      rule match {
        case Equals(rune, right) => {
          // We should never be asked to solve an Equals, those should be filtered out
          // by the planner
          vfail()
        }
        case SimpleLiteral(rune, literal) => {
          concludeRune(rune, literal)
          Ok(())
        }
        case OneOf(rune, literals) => {
          val literal = getConclusion(rune).get
          if (!literals.contains(literal)) {
            return Err("conflict!")
          }
          Ok(())
        }
        case CoordComponents(coordRune, ownershipRune, kindRune) => {
          getConclusion(coordRune) match {
            case Some(combined) => {
              val Array(ownership, kind) = combined.split("/")
              concludeRune(ownershipRune, ownership)
              concludeRune(kindRune, kind)
            }
            case None => {
              (getConclusion(ownershipRune), getConclusion(kindRune)) match {
                case (Some(ownership), Some(kind)) => {
                  concludeRune(coordRune, (ownership + "/" + kind))
                }
                case _ => vfail()
              }
            }
          }
          Ok(())
        }
      }
    }
  }

//  def solve(rulesSR: Vector[IRulexSR]): Map[IRuneA, String] = {
//    solveAndGetState(rulesSR)._1
//  }
//
//  def solveAndGetState(rulesSR: Vector[IRulexSR]): (Map[IRuneA, String], PlannerState) = {
//    val solver = makeSolver()
//    val (runeToIndex, runeToType, plannerState) = RuleFlattener.flattenAndCompileRules(rulesSR)
//    val rawConclusions =
//      solver.solve((), (), plannerState, tr).getOrDie()
//    val conclusions = runeToIndex.mapValues(i => vassertSome(rawConclusions(i)))
//    (conclusions, plannerState)
//  }

  test("Simple int rule") {
    val rules =
      Array(
        SimpleLiteral(-1L, "1337"))
    getConclusions(rules, true) shouldEqual Map(-1L -> "1337")
  }

  test("Equals transitive") {
    val rules =
      Array(
        Equals(-2L, -1L),
        SimpleLiteral(-1L, "1337"))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337", -2L -> "1337")
  }

  test("Incomplete solve") {
    val rules =
      Array(
        OneOf(-1L, Array("1448", "1337")))
    getConclusions(rules, false) shouldEqual Map()
  }

  test("Half-complete solve") {
    // Note how these two rules aren't connected to each other at all
    val rules =
      Array(
        OneOf(-1L, Array("1448", "1337")),
        SimpleLiteral(-2L, "1337"))
    getConclusions(rules, false) shouldEqual Map(-2L -> "1337")
  }

  test("OneOf") {
    val rules =
      Array(
        OneOf(-1L, Array("1448", "1337")),
        SimpleLiteral(-1L, "1337"))
    getConclusions(rules, true) shouldEqual Map(-1L -> "1337")
  }

  test("Solves a components rule") {
    val rules =
      Array(
        CoordComponents(-1L, -2L, -3L),
        SimpleLiteral(-2L, "1337"),
        SimpleLiteral(-3L, "1448"))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337/1448", -2L -> "1337", -3L -> "1448")
  }

  test("Reverse-solve a components rule") {
    val rules =
      Array(
        CoordComponents(-1L, -2L, -3L),
        SimpleLiteral(-1L, "1337/1448"))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337/1448", -2L -> "1337", -3L -> "1448")
  }

  test("Complex rule set") {
    val conclusions = getConclusions(complexRuleSet, true)
    conclusions.get(-7L) shouldEqual Some("1337/1448/1337/1448")
  }

  test("Optimize") {
    // A possible slowdown in the solver is all the mapping that goes on, to translate between
    // user rune and canonical rune.
    // So, we need to be able to "optimize" the rules, to have them directly think in terms
    // of the canonical rune.
    // We take the Planner's own mapping (userRuneToCanonicalRune) and actually apply it to
    // the *rules*. Then, everyone's thinking in terms of canonical runes, and the mapping is
    // just the identity function.

    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        complexRuleSet.toIterable,
        (rule: ISimpleRule) => rule.allRunes,
        (rule: ISimpleRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[ISimpleRule, (Long, Long)])
    // There's 7 runes up there, and 3 equals rules, so 3 of them are redundant,
    // we should end up with 4.
    vassert(numCanonicalRunes == 4)
    // These are the indices of the equals rules, they should be in the execution plan.
    complexRuleSetEqualsRules.foreach(r => vassert(!ruleExecutionOrder.contains(3)))
    // The other 5 should be though.
    vassert(ruleExecutionOrder.length == 5)

    val urtcr = userRuneToCanonicalRune
    // These are rules that know exactly what index their runes are at. This is nice because
    // there's no constant mapping between e.g. IRuneS and Int.
    val optimizedRules =
      ruleExecutionOrder.map(complexRuleSet).map({
        case Equals(_, _) => vfail() // Shouldnt execute these at all
        case SimpleLiteral(unoptimizedRune, value) => SimpleLiteral(urtcr(unoptimizedRune), value)
        case CoordComponents(unoptimizedCoordRune, unoptimizedOwnershipRune, unoptimizedKindRune) => {
          CoordComponents(urtcr(unoptimizedCoordRune), urtcr(unoptimizedOwnershipRune), urtcr(unoptimizedKindRune))
        }
        case OneOf(unoptimizedRune, possibleValues) => {
          OneOf(urtcr(unoptimizedRune), possibleValues)
        }
      })

    val conclusions =
      Solver.solve[ISimpleRule, Long, Unit, Unit, String, String](
        (), (), numCanonicalRunes, optimizedRules, optimizedRules.indices,
        (rule: ISimpleRule) => rule.allRunes.toVector,
        l => l.toInt,
        (0 until numCanonicalRunes).map(_.toLong).toVector,
        RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    conclusions.toMap.get(userRuneToCanonicalRune(-7L)) shouldEqual Some("1337/1448/1337/1448")
  }

  test("Dive for template") {
    // For things like:
    //   impl<T> Firefly<T> for ISpaceship<T>;
    // we need to index by struct's generic type (e.g. Firefly) and also by
    // interface's generic type (e.g. ISpaceship).
    //
    // Another example:
    //   fn equip(ship &ISpaceship<T>, n int)
    // we need to index by first argument's generic type (e.g. ISpaceship)
    //
    // In both cases, we need to dive into the rules.

    val rules =
      Vector(
        Call(-2, "Firefly", -1), // X = Firefly<A, B>
        Call(-3, "ISpaceship", -1)) // Y = ISpaceship<A, B>
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules,
        (rule: ISimpleRule) => rule.allRunes,
        (rule: ISimpleRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[ISimpleRule, (Long, Long)])

    val structRune = -2
    val structCanonicalRune = userRuneToCanonicalRune(structRune)
    val foundGenericNames =
      rules.collect({
        case Call(resultRune, name, _)
          if userRuneToCanonicalRune(resultRune) == structCanonicalRune => name
      })
    foundGenericNames.size shouldEqual 1
    val Vector(foundGenericName) = foundGenericNames
    foundGenericName shouldEqual "Firefly"
  }

  test("Test conflict") {
    val rules =
      Array(
        SimpleLiteral(-1L, "1448"),
        SimpleLiteral(-1L, "1337"))
    expectSolveFailure(rules) match {
      case FailedSolve(SolverConflict(_, _, conclusionA, conclusionB), _) => {
        Vector(conclusionA, conclusionB).sorted shouldEqual Vector("1337", "1448").sorted
      }
    }
  }

  private def expectSolveFailure(rules: IndexedSeq[ISimpleRule]):
  FailedSolve[Long, String, String] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules.toIterable,
        (rule: ISimpleRule) => rule.allRunes,
        (rule: ISimpleRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[ISimpleRule, (Long, Long)])
    Solver.solve(
      (), (), numCanonicalRunes, rules, ruleExecutionOrder,
      (rule: ISimpleRule) => rule.allRunes.toVector,
      userRuneToCanonicalRune,
      userRuneToCanonicalRune.keys,
      RuleSolver.solve) match {
      case Ok(c) => vfail(c)
      case Err(e) => e
    }
  }

  private def getConclusions(
    rules: IndexedSeq[ISimpleRule],
    expectCompleteSolve: Boolean):
  Map[Long, String] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules.toIterable,
        (rule: ISimpleRule) => rule.allRunes,
        (rule: ISimpleRule) => rule.allPuzzles,
        Set(),
        { case Equals(a, b) => (a, b)}: PartialFunction[ISimpleRule, (Long, Long)])
    val conclusions =
      Solver.solve(
        (), (), numCanonicalRunes, rules, ruleExecutionOrder,
        (rule: ISimpleRule) => rule.allRunes.toVector,
        userRuneToCanonicalRune,
        userRuneToCanonicalRune.keys,
        RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    val conclusionsMap = conclusions.toMap
    vassert(expectCompleteSolve == (conclusionsMap.keySet == userRuneToCanonicalRune.keySet))
    conclusionsMap
  }
}
