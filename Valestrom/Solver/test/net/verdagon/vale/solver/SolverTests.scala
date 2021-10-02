package net.verdagon.vale.solver

import net.verdagon.vale.{Collector, Err, Ok, Result, vassert, vassertSome, vfail, vimpl, vwat}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Map

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
// See IRFU and SRCAMP for what this rule is doing
case class Receive(receiverRune: Long, senderRune: Long) extends IRule {
  override def allRunes: Array[Long] = Array(receiverRune, senderRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(receiverRune))
}
case class Implements(subRune: Long, superRune: Long) extends IRule {
  override def allRunes: Array[Long] = Array(subRune, superRune)
  override def allPuzzles: Array[Array[Long]] = Array(Array(subRune, superRune))
}
case class Pack(resultRune: Long, memberRunes: Array[Long]) extends IRule {
  override def allRunes: Array[Long] = Array(resultRune) ++ memberRunes
  override def allPuzzles: Array[Array[Long]] = {
    if (memberRunes.isEmpty) {
      Array(Array(resultRune))
    } else {
      Array(Array(resultRune), memberRunes)
    }
  }
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
  //      override def getPuzzles(rulexAR: IRulexSR[Int, Unit, SimpleLiteral, SimpleLookup]): Array[Array[Int]] = {
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
      solverState: ISolverStateForRule[IRule, Long, String]):
    Result[Map[Long, String], String] = {
      rule match {
        case Equals(leftRune, rightRune) => {
          solverState.getConclusion(leftRune) match {
            case Some(left) => Ok(Map(rightRune -> left))
            case None => Ok(Map(leftRune -> vassertSome(solverState.getConclusion(rightRune))))
          }
        }
        case Lookup(rune, name) => {
          val value = name
          Ok(Map(rune -> value))
        }
        case Literal(rune, literal) => {
          Ok(Map(rune -> literal))
        }
        case OneOf(rune, literals) => {
          val literal = solverState.getConclusion(rune).get
          if (!literals.contains(literal)) {
            return Err("conflict!")
          }
          Ok(Map())
        }
        case CoordComponents(coordRune, ownershipRune, kindRune) => {
          solverState.getConclusion(coordRune) match {
            case Some(combined) => {
              val Array(ownership, kind) = combined.split("/")
              Ok(Map(ownershipRune -> ownership, kindRune -> kind))
            }
            case None => {
              (solverState.getConclusion(ownershipRune), solverState.getConclusion(kindRune)) match {
                case (Some(ownership), Some(kind)) => {
                  Ok(Map(coordRune -> (ownership + "/" + kind)))
                }
                case _ => vfail()
              }
            }
          }
        }
        case Pack(resultRune, memberRunes) => {
          solverState.getConclusion(resultRune) match {
            case Some(result) => {
              val parts = result.split(",")
              Ok(memberRunes.zip(parts).toMap)
            }
            case None => {
              val result = memberRunes.map(solverState.getConclusion).map(_.get).mkString(",")
              Ok(Map(resultRune -> result))
            }
          }
        }
        case Call(resultRune, nameRune, argRune) => {
          val maybeResult = solverState.getConclusion(resultRune)
          val maybeName = solverState.getConclusion(nameRune)
          val maybeArg = solverState.getConclusion(argRune)
          (maybeResult, maybeName, maybeArg) match {
            case (Some(result), Some("Firefly"), _) => Ok(Map(argRune -> result.slice("Firefly:".length, result.length)))
            case (_, Some("Firefly"), Some(arg)) => Ok(Map(resultRune -> ("Firefly:" + arg)))
            case other => vwat(other)
          }
        }
        case Receive(receiverRune, senderRune) => {
          val receiver = vassertSome(solverState.getConclusion(receiverRune))
          if (receiver == "ISpaceship") {
            val ruleIndex =
              solverState.addRule(Implements(senderRune, receiverRune), Array(senderRune, receiverRune))
            solverState.addPuzzle(ruleIndex, Array(senderRune, receiverRune))
            Ok(Map())
          } else {
            // Not receiving into an interface, so sender must be the same
            Ok(Map(senderRune -> receiver))
          }
        }
        case Implements(subRune, superRune) => {
          val sub = vassertSome(solverState.getConclusion(subRune))
          val suuper = vassertSome(solverState.getConclusion(superRune))
          if (sub == suuper) {
            Ok(Map())
          } else if (sub == "Firefly" && suuper == "ISpaceship") {
            Ok(Map())
          } else {
            Err("bork")
          }
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
//      Planner.solve((), (), plannerState, tr).getOrDie()
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

  test("Test infer Pack") {
    val rules =
      Array(
        Literal(-1L, "1337"),
        Literal(-2L, "1448"),
        Pack(-3L, Array(-1L, -2L)))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337", -2L -> "1448", -3L -> "1337,1448")
  }

  test("Test infer Pack from result") {
    val rules =
      Array(
        Literal(-3L, "1337,1448"),
        Pack(-3L, Array(-1L, -2L)))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "1337", -2L -> "1448", -3L -> "1337,1448")
  }

  test("Test infer Pack from empty result") {
    val rules =
      Array(
        Literal(-3L, ""),
        Pack(-3L, Array()))
    getConclusions(rules, true) shouldEqual
      Map(-3L -> "")
  }

  test("Test cant solve empty Pack") {
    val rules =
      Array(
        Pack(-3L, Array()))
    getConclusions(rules, false) shouldEqual Map()
  }

  test("Complex rule set") {
    val conclusions = getConclusions(complexRuleSet, true)
    conclusions.get(-7L) shouldEqual Some("1337/1448/1337/1448")
  }

  test("Test receiving struct to struct") {
    val rules =
      Array(
        Literal(-1L, "Firefly"),
        Receive(-1L, -2L))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "Firefly", -2L -> "Firefly")
  }

  test("Test receive struct from sent interface") {
    val rules =
      Array(
        Literal(-1L, "Firefly"),
        Literal(-2L, "ISpaceship"),
        Receive(-1L, -2L))
    expectSolveFailure(rules) match {
      case FailedSolve(conclusions, unsolvedRules, err) => {
        conclusions shouldEqual Map(-1 -> "Firefly", -2 -> "ISpaceship")
        unsolvedRules shouldEqual Vector()
        err match {
          case SolverConflict(
            2,
            -2,
            // Already concluded this
            "ISpaceship",
            // But now we're concluding that it should have been a Firefly
            "Firefly") =>
        }
      }
    }
  }

  test("Test receive interface from sent struct") {
    val rules =
      Array(
        Literal(-1L, "ISpaceship"),
        Literal(-2L, "Firefly"),
        Receive(-1L, -2L))
    getConclusions(rules, true) shouldEqual
      Map(-1L -> "ISpaceship", -2L -> "Firefly")
  }

  test("Partial Solve") {
    // It'll be nice to partially solve some rules, for example before we put them in the overload index.

    // Note how these two rules aren't connected to each other at all
    val rules =
      Vector(
        Literal(-2, "A"),
        Call(-3, -1, -2)) // We dont know the template, -1, yet


    val solverState =
      Solver.makeInitialSolverState[IRule, Long, String](
        rules,
        (rule: IRule) => rule.allRunes.toVector,
        (rule: IRule) => rule.allPuzzles,
        Map())
    val firstConclusions =
      Solver.solve((), (), solverState, RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    firstConclusions.toMap shouldEqual Map(-2 -> "A")
    solverState.concludeRune(solverState.getCanonicalRune(-1), "Firefly")

    val secondConclusions =
      Solver.solve((), (), solverState, RuleSolver.solve) match {
        case Ok(c) => c
        case Err(e) => vfail(e)
      }
    secondConclusions.toMap shouldEqual
      Map(-1 -> "Firefly", -2 -> "A", -3 -> "Firefly:A")
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
//      val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
//        Planner.plan(
//          rules,
//          Set(),
//          (rule: IRule) => rule.allRunes,
//          puzzler,
//          Set(), {
//            case Equals(a, b) => (a, b)
//          }: PartialFunction[IRule, (Long, Long)])

      val solverState =
        Solver.makeInitialSolverState[IRule, Long, String](
          rules,
          (rule: IRule) => rule.allRunes.toVector,
          puzzler,
          Map())
      val conclusions =
        Solver.solve((), (), solverState, RuleSolver.solve) match {
          case Ok(c) => c.toMap
          case Err(e) => vfail(e)
        }
      conclusions
    }

    val predictions =
      solveWithPuzzler({
        // This Array() makes it unsolvable
        case Lookup(rune, name) => Array()
        case rule => rule.allPuzzles
      })
//    vassert(predictionRuleExecutionOrder sameElements Array(1))
    vassert(predictions.size == 1)
    vassert(predictions(-2) == "1337")

    val conclusions = solveWithPuzzler(_.allPuzzles)
//    vassert(ruleExecutionOrder.length == 3)
    conclusions shouldEqual Map(-1L -> "Firefly", -2L -> "1337", -3L -> "Firefly:1337")
  }

  test("Test conflict") {
    val rules =
      Array(
        Literal(-1L, "1448"),
        Literal(-1L, "1337"))
    expectSolveFailure(rules) match {
      case FailedSolve(_, _, SolverConflict(_, _, conclusionA, conclusionB)) => {
        Vector(conclusionA, conclusionB).sorted shouldEqual Vector("1337", "1448").sorted
      }
    }
  }

  private def expectSolveFailure(rules: IndexedSeq[IRule]):
  FailedSolve[IRule, Long, String, String] = {
//    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
////      Solv.plan(
//        rules.toIterable,
//        List(),
//        (rule: IRule) => rule.allRunes,
//        (rule: IRule) => rule.allPuzzles,
//        Set(), {
//          case Equals(a, b) => (a, b)
//        }//: PartialFunction[IRule, (Long, Long)])

    val solverState =
      Solver.makeInitialSolverState[IRule, Long, String](
        rules,
        (rule: IRule) => rule.allRunes.toVector,
        (rule: IRule) => rule.allPuzzles,
        Map())
    Solver.solve((), (), solverState, RuleSolver.solve) match {
      case Ok(c) => vfail(c)
      case Err(e) => e
    }
  }

  private def getConclusions(
    rules: IndexedSeq[IRule],
    expectCompleteSolve: Boolean,
    initiallyKnownRunes: Map[Long, String] = Map()):
  Map[Long, String] = {
    val solverState =
      Solver.makeInitialSolverState[IRule, Long, String](
        rules,
        (rule: IRule) => rule.allRunes.toVector,
        (rule: IRule) => rule.allPuzzles,
        initiallyKnownRunes)
    val conclusions =
      Solver.solve((), (), solverState, RuleSolver.solve) match {
          case Ok(c) => c
          case Err(e) => vfail(e)
        }
    val conclusionsMap = conclusions.toMap
    vassert(expectCompleteSolve == (conclusionsMap.keySet == rules.flatMap(_.allRunes).toSet))
    conclusionsMap
  }
}
