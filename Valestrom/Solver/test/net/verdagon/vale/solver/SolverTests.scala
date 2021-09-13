package net.verdagon.vale.solver

import net.verdagon.vale.{Collector, Err, Ok, Result, vassert, vassertSome, vfail, vimpl, vwat}
import org.scalatest.{FunSuite, Matchers}

sealed trait IRule {
  def allRunes: Array[Long]
  def allPuzzles: Array[Array[Long]]
}
case class Lookup(rune: Long, name: String) extends IRule {
  override def allRunes: Array[Long] = Array(rune)
  override def allPuzzles: Array[Array[Long]] = Array(Array())
}
case class Literal(rune: Long, value: String) extends IRule {
  override def allRunes: Array[Long] = Array(rune)
  override def allPuzzles: Array[Array[Long]] = Array(Array())
}
case class Equals(leftRune: Long, rightRune: Long) extends IRule {
  override def allRunes: Array[Long] = Array(leftRune, rightRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(leftRune), Array(rightRune))
}
case class CoordComponents(coordRune: Long, ownershipRune: Long, kindRune: Long) extends IRule {
  override def allRunes: Array[Long] = Array(coordRune, ownershipRune, kindRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(coordRune), Array(ownershipRune, kindRune))
}
case class OneOf(coordRune: Long, possibleValues: Array[String]) extends IRule {
  override def allRunes: Array[Long] = Array(coordRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(coordRune))
}
case class Call(resultRune: Long, nameRune: Long, argRune: Long) extends IRule {
  override def allRunes: Array[Long] = Array(resultRune, nameRune, argRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(resultRune, nameRune), Array(nameRune, argRune))
}

class SolverTests extends FunSuite with Matchers with Collector {
  val complexRuleSet =
    Array(
      Literal(-3L, "1448"),
      CoordComponents(-6L, -5L, -5L),
      Literal(-2L, "1337"),
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

  object RuleSolver extends ISolveRule[IRule, Long, Unit, Unit, String, String] {
    override def solve(
      state: Unit,
      env: Unit,
      ruleIndex: Int,
      rule: IRule,
      getConclusion: Long => Option[String],
      concludeRune: (Long, String) => Unit):
    Result[Unit, String] = {
      rule match {
        case Equals(rune, right) => {
          // We should never be asked to solve an Equals, those should be filtered out
          // by the planner
          vfail()
        }
        case Lookup(rune, name) => {
          val value = name
          concludeRune(rune, value)
          Ok(())
        }
        case Literal(rune, literal) => {
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
        case Call(resultRune, nameRune, argRune) => {
          val maybeResult = getConclusion(resultRune)
          val maybeName = getConclusion(nameRune)
          val maybeArg = getConclusion(argRune)
          (maybeResult, maybeName, maybeArg) match {
            case (Some(result), Some("Firefly"), _) => concludeRune(argRune, result.slice("Firefly:".length, result.length))
            case (_, Some("Firefly"), Some(arg)) => concludeRune(resultRune, "Firefly:" + arg)
            case other => vwat(other)
          }
          Ok(())
        }
      }
    }
  }

//  def solve(rulesSR: Vector[IRulexSR]): Map[IRuneS, String] = {
//    solveAndGetState(rulesSR)._1
//  }
//
//  def solveAndGetState(rulesSR: Vector[IRulexSR]): (Map[IRuneS, String], PlannerState) = {
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
        Literal(-1L, "1337"))
    getConclusions(rules, true) shouldEqual Map(-1L -> "1337")
  }

  test("Equals transitive") {
    val rules =
      Array(
        Equals(-2L, -1L),
        Literal(-1L, "1337"))
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
        Literal(-2L, "1337"))
    getConclusions(rules, false) shouldEqual Map(-2L -> "1337")
  }

  test("OneOf") {
    val rules =
      Array(
        OneOf(-1L, Array("1448", "1337")),
        Literal(-1L, "1337"))
    getConclusions(rules, true) shouldEqual Map(-1L -> "1337")
  }

  test("Solves a components rule") {
    val rules =
      Array(
        CoordComponents(-1L, -2L, -3L),
        Literal(-2L, "1337"),
        Literal(-3L, "1448"))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337/1448", -2L -> "1337", -3L -> "1448")
  }

  test("Reverse-solve a components rule") {
    val rules =
      Array(
        CoordComponents(-1L, -2L, -3L),
        Literal(-1L, "1337/1448"))
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
        Set(),
        (rule: IRule) => rule.allRunes,
        (rule: IRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[IRule, (Long, Long)])
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
        case Literal(unoptimizedRune, value) => Literal(urtcr(unoptimizedRune), value)
        case CoordComponents(unoptimizedCoordRune, unoptimizedOwnershipRune, unoptimizedKindRune) => {
          CoordComponents(urtcr(unoptimizedCoordRune), urtcr(unoptimizedOwnershipRune), urtcr(unoptimizedKindRune))
        }
        case OneOf(unoptimizedRune, possibleValues) => {
          OneOf(urtcr(unoptimizedRune), possibleValues)
        }
      })

    val conclusions =
      Solver.solve[IRule, Long, Unit, Unit, String, String](
        (), (), numCanonicalRunes, optimizedRules, optimizedRules.indices,
        (rule: IRule) => rule.allRunes.toVector,
        l => l.toInt,
        (0 until numCanonicalRunes).map(_.toLong).toVector,
        Map(),
        RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    conclusions.toMap.get(userRuneToCanonicalRune(-7L)) shouldEqual Some("1337/1448/1337/1448")
  }

  test("Predicting") {
    // "Predicting" is when the rules arent completely solvable, but we can still run some of them
    // to figure out what we can.
    // For example, in:
    //   #2 = 1337
    //   #3 = #1<#2>
    // we can figure out that #2 is 1337, even if we don't know #1 yet.
    // This is useful for recursive types.
    // See: Recursive Types Must Have Types Predicted (RTMHTP)

    def solveWithPuzzler(puzzler: IRule => Array[Array[Long]]) = {
      // Below, we're reporting that Lookup has no puzzles that can solve it.
      val rules =
        Vector(
          Lookup(-1, "Firefly"),
          Literal(-2, "1337"),
          Call(-3, -1, -2)) // X = Firefly<A>
      val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
        Planner.plan(
          rules,
          Set(),
          (rule: IRule) => rule.allRunes,
          puzzler,
          Set(), {
            case Equals(a, b) => (a, b)
          }: PartialFunction[IRule, (Long, Long)])
      val conclusions =
        Solver.solve(
          (), (), numCanonicalRunes, rules, ruleExecutionOrder,
          (rule: IRule) => rule.allRunes.toVector,
          userRuneToCanonicalRune,
          userRuneToCanonicalRune.keys,
          Map(),
          RuleSolver.solve) match {
          case Ok(c) => c.toMap
          case Err(e) => vfail(e)
        }
      (ruleExecutionOrder, conclusions.toMap)
    }

    val (predictionRuleExecutionOrder, predictions) =
      solveWithPuzzler({
        // This Array() makes it unsolvable
        case Lookup(rune, name) => Array()
        case rule => rule.allPuzzles
      })
    vassert(predictionRuleExecutionOrder sameElements Array(1))
    vassert(predictions.size == 1)
    vassert(predictions(-2) == "1337")

    val (ruleExecutionOrder, conclusions) = solveWithPuzzler(_.allPuzzles)
    vassert(ruleExecutionOrder.length == 3)
    conclusions shouldEqual Map(-1L -> "Firefly", -2L -> "1337", -3L -> "Firefly:1337")
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
    // This test case shows how it can be done.

    val rules =
      Vector(
        Literal(-4, "Firefly"),
        Literal(-5, "ISpaceship"),
        Call(-2, -4, -1), // X = Firefly<A, B>
        Call(-3, -5, -1)) // Y = ISpaceship<A, B>
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules,
        Set(),
        (rule: IRule) => rule.allRunes,
        (rule: IRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[IRule, (Long, Long)])

    val structRune = -2
    val foundGenericNames =
      rules.flatMap({
        case Call(resultRune, nameRune, _) if userRuneToCanonicalRune(resultRune) == userRuneToCanonicalRune(structRune) => {
          rules.flatMap({
            case Literal(nr, name) if userRuneToCanonicalRune(nr) == userRuneToCanonicalRune(nameRune) => {
              List(name)
            }
            case _ => List()
          })
        }
        case _ => List()
      })
    foundGenericNames.size shouldEqual 1
    val Vector(foundGenericName) = foundGenericNames
    foundGenericName shouldEqual "Firefly"
  }

  test("Test conflict") {
    val rules =
      Array(
        Literal(-1L, "1448"),
        Literal(-1L, "1337"))
    expectSolveFailure(rules) match {
      case FailedSolve(SolverConflict(_, _, conclusionA, conclusionB), _) => {
        Vector(conclusionA, conclusionB).sorted shouldEqual Vector("1337", "1448").sorted
      }
    }
  }

  private def expectSolveFailure(rules: IndexedSeq[IRule]):
  FailedSolve[Long, String, String] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules.toIterable,
        List(),
        (rule: IRule) => rule.allRunes,
        (rule: IRule) => rule.allPuzzles,
        Set(), {
          case Equals(a, b) => (a, b)
        }: PartialFunction[IRule, (Long, Long)])
    Solver.solve(
      (), (), numCanonicalRunes, rules, ruleExecutionOrder,
      (rule: IRule) => rule.allRunes.toVector,
      userRuneToCanonicalRune,
      userRuneToCanonicalRune.keys,
      Map(),
      RuleSolver.solve) match {
      case Ok(c) => vfail(c)
      case Err(e) => e
    }
  }

  private def getConclusions(
    rules: IndexedSeq[IRule],
    expectCompleteSolve: Boolean,
    initiallyKnownRunes: Map[Long, String] = Map()):
  Map[Long, String] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules.toIterable,
        List(),
        (rule: IRule) => rule.allRunes,
        (rule: IRule) => rule.allPuzzles,
        initiallyKnownRunes.keySet,
        { case Equals(a, b) => (a, b)}: PartialFunction[IRule, (Long, Long)])
    val conclusions =
      Solver.solve(
        (), (), numCanonicalRunes, rules, ruleExecutionOrder,
        (rule: IRule) => rule.allRunes.toVector,
        userRuneToCanonicalRune,
        userRuneToCanonicalRune.keys,
        initiallyKnownRunes,
        RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    val conclusionsMap = conclusions.toMap
    vassert(expectCompleteSolve == (conclusionsMap.keySet == userRuneToCanonicalRune.keySet))
    conclusionsMap
  }
}
