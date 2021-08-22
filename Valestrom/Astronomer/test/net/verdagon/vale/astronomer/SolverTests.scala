package net.verdagon.vale.astronomer

import net.verdagon.vale.{Collector, Ok, Result, vassert, vassertSome, vfail, vimpl}
import net.verdagon.vale.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator}
import net.verdagon.vale.scout.{CodeRuneS, StringSR, RangeS, RuneSR}
import net.verdagon.vale.scout.rules.{ComponentsSR, CoordTypeSR, EqualsSR, IRulexSR, TypedSR}
import org.scalatest.{FunSuite, Matchers}

class RuleTyperTests extends FunSuite with Matchers with Collector {
  def makeSolver() = {
    new RuleTyperEvaluator[Unit, Unit, String, Unit](
      new IRuleTyperEvaluatorDelegate[Unit, Unit, String, Unit] {
        override def solve(state: Unit, env: Unit, range: RangeS, rule: IRulexAR, runes: Map[Int, String]): Result[Map[Int, String], Unit] = {
          rule match {
            case StringAR(_, resultRune, value) => {
              Ok(Map(resultRune -> value))
            }
            case CoordComponentsAR(_, coordRune, ownershipRune, permissionRune, kindRune) => {
              runes.get(coordRune) match {
                case Some(combined) => {
                  val Array(ownership, permission, kind) = combined.split("/")
                  Ok(Map(ownershipRune -> ownership, permissionRune -> permission, kindRune -> kind))
                }
                case None => {
                  (runes.get(ownershipRune), runes.get(permissionRune), runes.get(kindRune)) match {
                    case (Some(ownership), Some(permission), Some(kind)) => {
                      Ok(Map(coordRune -> (ownership + "/" + permission + "/" + kind)))
                    }
                    case _ => vfail()
                  }
                }
              }
            }
          }
        }
      })
  }

  def solve(rulesSR: Vector[IRulexSR]): Map[IRuneA, String] = {
    solveAndGetState(rulesSR)._1
  }

  def solveAndGetState(rulesSR: Vector[IRulexSR]): (Map[IRuneA, String], RuneWorldSolverState) = {
    val solver = makeSolver()
    val (runeToIndex, runeToType, solverState) = RuleFlattener.flattenAndCompileRules(rulesSR)
    val rawConclusions =
      solver.solve((), (), solverState, RangeS.testZero).getOrDie()
    val conclusions = runeToIndex.mapValues(i => vassertSome(rawConclusions(i)))
    (conclusions, solverState)
  }

  test("Simple int rule") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("A")), RuneSR(RangeS.testZero, CodeRuneS("B"))),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("B")), StringSR(RangeS.testZero, "1337")))
    solve(rules) shouldEqual Map(
      CodeRuneA("A") -> "1337",
      CodeRuneA("B") -> "1337")
  }


  test("Equals are optimized out") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("A")), RuneSR(RangeS.testZero, CodeRuneS("B"))),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("B")), StringSR(RangeS.testZero, "1337")))
    val (conclusions, solverState) = solveAndGetState(rules)
    solverState.runeWorld.rules.size shouldEqual 1
    solverState.runeWorld.rules(0) match {
      case StringAR(_, _, "1337") =>
    }
    solverState.runeWorld.runeToPuzzles.size shouldEqual 1
    conclusions shouldEqual Map(
      CodeRuneA("A") -> "1337",
      CodeRuneA("B") -> "1337")
  }

  test("Solves a components rule") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("X")), StringSR(RangeS.testZero, "turquoise")),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("Y")), StringSR(RangeS.testZero, "bicycle")),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("Z")), StringSR(RangeS.testZero, "shoe")),
        ComponentsSR(RangeS.testZero,
          TypedSR(RangeS.testZero, CodeRuneS("C"), CoordTypeSR),
          Vector(
          RuneSR(RangeS.testZero, CodeRuneS("X")),
          RuneSR(RangeS.testZero, CodeRuneS("Y")),
          RuneSR(RangeS.testZero, CodeRuneS("Z")))))
    val (conclusions, solverState) = solveAndGetState(rules)
    conclusions shouldEqual Map(
      CodeRuneA("X") -> "turquoise",
      CodeRuneA("Y") -> "bicycle",
      CodeRuneA("Z") -> "shoe",
      CodeRuneA("C") -> "turquoise/bicycle/shoe")
  }

  test("Reverse-solves a components rule") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("C")), StringSR(RangeS.testZero, "turquoise/bicycle/shoe")),
        ComponentsSR(RangeS.testZero,
          TypedSR(RangeS.testZero, CodeRuneS("C"), CoordTypeSR),
          Vector(
            RuneSR(RangeS.testZero, CodeRuneS("X")),
            RuneSR(RangeS.testZero, CodeRuneS("Y")),
            RuneSR(RangeS.testZero, CodeRuneS("Z")))))
    val (conclusions, solverState) = solveAndGetState(rules)
    conclusions shouldEqual Map(
      CodeRuneA("X") -> "turquoise",
      CodeRuneA("Y") -> "bicycle",
      CodeRuneA("Z") -> "shoe",
      CodeRuneA("C") -> "turquoise/bicycle/shoe")
  }
}
