package net.verdagon.vale.astronomer

import net.verdagon.vale.{Collector, Ok, Result, vassert, vassertSome, vfail, vimpl}
import net.verdagon.vale.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator}
import net.verdagon.vale.scout.{CodeRuneS, IntSR, RangeS, RuneSR, StringSR}
import net.verdagon.vale.scout.rules.{ComponentsSR, CoordTypeSR, EqualsSR, IRulexSR, TypedSR}
import org.scalatest.{FunSuite, Matchers}

class RuleTyperTests extends FunSuite with Matchers with Collector {
  def makeSolver() = {
    new RuleTyperEvaluator[Unit, Unit, Long, Unit](
      new IRuleTyperEvaluatorDelegate[Unit, Unit, Long, Unit] {
        override def solve(state: Unit, env: Unit, range: RangeS, rule: IRulexAR, runes: Map[Int, Long]): Result[Map[Int, Long], Unit] = {
          rule match {
            case IntAR(_, resultRune, value) => {
              Ok(Map(resultRune -> value))
            }
            case CoordComponentsAR(_, coordRune, ownershipRune, permissionRune, kindRune) => {
              runes.get(coordRune) match {
                case Some(x) => vimpl()
                case None => {
                  (runes.get(ownershipRune), runes.get(permissionRune), runes.get(kindRune)) match {
                    case (Some(ownership), Some(permission), Some(kind)) => {
                      Ok(Map(coordRune -> (ownership + permission + kind)))
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

  def solve(rulesSR: Vector[IRulexSR]): Map[IRuneA, Long] = {
    solveAndGetState(rulesSR)._1
  }

  def solveAndGetState(rulesSR: Vector[IRulexSR]): (Map[IRuneA, Long], RuneWorldSolverState) = {
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
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("B")), IntSR(RangeS.testZero, 1337)))
    solve(rules) shouldEqual Map(
      CodeRuneA("A") -> 1337,
      CodeRuneA("B") -> 1337)
  }


  test("Equals are optimized out") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("A")), RuneSR(RangeS.testZero, CodeRuneS("B"))),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("B")), IntSR(RangeS.testZero, 1337)))
    val (conclusions, solverState) = solveAndGetState(rules)
    solverState.runeWorld.rules.size shouldEqual 1
    solverState.runeWorld.rules(0) match {
      case IntAR(_, _, 1337) =>
    }
    solverState.runeWorld.runeToPuzzles.size shouldEqual 1
    conclusions shouldEqual Map(
      CodeRuneA("A") -> 1337,
      CodeRuneA("B") -> 1337)
  }

  test("Solves a components rule") {
    val rules =
      Vector(
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("X")), IntSR(RangeS.testZero, 10)),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("Y")), IntSR(RangeS.testZero, 20)),
        EqualsSR(RangeS.testZero, RuneSR(RangeS.testZero, CodeRuneS("Z")), IntSR(RangeS.testZero, 40)),
        ComponentsSR(RangeS.testZero,
          TypedSR(RangeS.testZero, CodeRuneS("C"), CoordTypeSR),
          Vector(
          RuneSR(RangeS.testZero, CodeRuneS("X")),
          RuneSR(RangeS.testZero, CodeRuneS("Y")),
          RuneSR(RangeS.testZero, CodeRuneS("Z")))))
    val (conclusions, solverState) = solveAndGetState(rules)
    conclusions shouldEqual Map(
      CodeRuneA("X") -> 10,
      CodeRuneA("Y") -> 20,
      CodeRuneA("Z") -> 40,
      CodeRuneA("C") -> 70)
  }
}
