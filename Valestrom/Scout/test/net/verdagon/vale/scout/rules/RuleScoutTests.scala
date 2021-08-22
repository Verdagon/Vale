package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.{Err, FileCoordinate, Ok, vassert, vfail}
import org.scalatest.{FunSuite, Matchers}

class RuleScoutTests extends FunSuite with Matchers {
  private def compile(code: String): Vector[IRulexSR] = {
    Parser.runParser(code) match {
      case ParseFailure(err) => fail(err.toString)
      case ParseSuccess(program0) => {
        val programS =
          Scout.scoutProgram(FileCoordinate.test, program0) match {
            case Err(e) => vfail(e.toString)
            case Ok(t) => t
          }
        programS.lookupFunction("main").templateRules
      }
    }
  }

  test("A") {
    val expectedRulesS =
      Vector(
        EqualsSR(RangeS.testZero,
          TypedSR(RangeS.testZero,CodeRuneS("B"),CoordTypeSR),
          CallSR(RangeS.testZero,NameSR(RangeS.testZero, CodeTypeNameS("List")),Vector(RuneSR(RangeS.testZero,CodeRuneS("A"))))),
        EqualsSR(RangeS.testZero,
          TypedSR(RangeS.testZero,CodeRuneS("C"),CoordTypeSR),
          OrSR(RangeS.testZero,Vector(RuneSR(RangeS.testZero,CodeRuneS("B")), RuneSR(RangeS.testZero,CodeRuneS("A")), NameSR(RangeS.testZero, CodeTypeNameS("int"))))),
        TypedSR(RangeS.testZero,CodeRuneS("A"),CoordTypeSR))
    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual
      Vector(CodeRuneS("B"), CodeRuneS("A"), CodeRuneS("C"))

    val results =
      compile(
        """fn main<A>(a A) infer-ret
          |rules(
          |  B Ref = List<A>,
          |  C Ref = B | A | int)
          |{ }
          |""".stripMargin)
    results match {
      case Vector(
        EqualsSR(_,
          TypedSR(_,br1 @ CodeRuneS("B"),CoordTypeSR),
          CallSR(_,NameSR(_, CodeTypeNameS("List")),Vector(RuneSR(_,ar1 @ CodeRuneS("A"))))),
        EqualsSR(_,
          TypedSR(_,CodeRuneS("C"),CoordTypeSR),
          OrSR(_,Vector(RuneSR(_,br2), RuneSR(_,ar3), NameSR(_, CodeTypeNameS("int"))))),
        TypedSR(_,ar2,CoordTypeSR)) => {
        vassert(br1 == br2)
        vassert(ar1 == ar2)
        vassert(ar1 == ar3)
      }
    }
  }

  test("B") {
    val rulesS = compile("fn main() infer-ret rules(B Ref = List<A>, A Ref, C Ref = B | A | Int) {}")
    RuleSUtils.getDistinctOrderedRunesForRulexes(rulesS) match {
      case Vector(
        CodeRuneS("B"),
        CodeRuneS("A"),
        CodeRuneS("C")) =>
    }
  }
}
