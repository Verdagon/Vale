package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.RangeS.{testZero => tr}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout._
import net.verdagon.vale._
import net.verdagon.vale.solver.{CoordComponentsAR, ISolverDelegate, IRulexAR, Solver}
import org.scalatest.{FunSuite, Matchers}

class SolverTests extends FunSuite with Matchers with Collector {
  def makeSolver() = {
    new Solver[Unit, Unit, String, Unit](
      new ISolverDelegate[Unit, Unit, String, Unit] {
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
      solver.solve((), (), solverState, tr).getOrDie()
    val conclusions = runeToIndex.mapValues(i => vassertSome(rawConclusions(i)))
    (conclusions, solverState)
  }

  test("Simple int rule") {
    val rules =
      Vector(
        EqualsSR(tr, RuneSR(tr, CodeRuneS("A")), RuneSR(tr, CodeRuneS("B"))),
        EqualsSR(tr, RuneSR(tr, CodeRuneS("B")), StringSR(tr, "1337")))
    solve(rules) shouldEqual Map(
      CodeRuneA("A") -> "1337",
      CodeRuneA("B") -> "1337")
  }


  test("Equals are optimized out") {
    val rules =
      Vector(
        EqualsSR(tr, RuneSR(tr, CodeRuneS("A")), RuneSR(tr, CodeRuneS("B"))),
        EqualsSR(tr, RuneSR(tr, CodeRuneS("B")), StringSR(tr, "1337")))
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
        EqualsSR(tr, RuneSR(tr, CodeRuneS("X")), StringSR(tr, "turquoise")),
        EqualsSR(tr, RuneSR(tr, CodeRuneS("Y")), StringSR(tr, "bicycle")),
        EqualsSR(tr, RuneSR(tr, CodeRuneS("Z")), StringSR(tr, "shoe")),
        ComponentsSR(tr,
          TypedSR(tr, CodeRuneS("C"), CoordTypeSR),
          Vector(RuneSR(tr, CodeRuneS("X")), RuneSR(tr, CodeRuneS("Y")), RuneSR(tr, CodeRuneS("Z")))))
    solve(rules) shouldEqual Map(
      CodeRuneA("X") -> "turquoise",
      CodeRuneA("Y") -> "bicycle",
      CodeRuneA("Z") -> "shoe",
      CodeRuneA("C") -> "turquoise/bicycle/shoe")
  }

  test("Reverse-solves a components rule") {
    val rules =
      Vector(
        EqualsSR(tr, RuneSR(tr, CodeRuneS("C")), StringSR(tr, "turquoise/bicycle/shoe")),
        ComponentsSR(tr,
          TypedSR(tr, CodeRuneS("C"), CoordTypeSR),
          Vector(RuneSR(tr, CodeRuneS("X")), RuneSR(tr, CodeRuneS("Y")), RuneSR(tr, CodeRuneS("Z")))))
    solve(rules) shouldEqual Map(
      CodeRuneA("X") -> "turquoise",
      CodeRuneA("Y") -> "bicycle",
      CodeRuneA("Z") -> "shoe",
      CodeRuneA("C") -> "turquoise/bicycle/shoe")
  }
}
