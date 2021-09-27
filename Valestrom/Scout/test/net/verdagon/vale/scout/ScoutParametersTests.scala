package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.{Collector, Err, FileCoordinate, Ok, vassert, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

class ScoutParametersTests extends FunSuite with Matchers with Collector {

  private def compile(code: String): ProgramS = {
    Parser.runParser(code) match {
      case ParseFailure(err) => fail(err.toString)
      case ParseSuccess(program0) => {
        Scout.scoutProgram(FileCoordinate.test, program0) match {
          case Err(e) => vfail(e.toString)
          case Ok(t) => t
        }
      }
    }
  }

  test("Simple rune rule") {
    val program1 = compile("""fn main<T>(moo T) infer-ret { }""")
    val main = program1.lookupFunction("main")

    vassert(main.runeToExplicitType.size == 1)

    vassert(main.identifyingRunes == List(CodeRuneS("T")))
  }

  test("Returned rune") {
    val program1 = compile("""fn main<T>(moo T) T { moo }""")
    val main = program1.lookupFunction("main")

    vassert(main.runeToExplicitType.size == 1)

    vassert(main.identifyingRunes == List(CodeRuneS("T")))
    vassert(main.maybeRetCoordRune == Some(CodeRuneS("T")))
  }

  test("Borrowed rune") {
    val program1 = compile("""fn main<T>(moo &T) infer-ret { }""")
    val main = program1.lookupFunction("main")
    val Vector(param) = main.params

    val tCoordRuneFromParams =
      param match {
        case ParameterS(
          AtomSP(_,
            Some(CaptureS(CodeVarNameS("moo"))),
            None,
            RuneUsage(_, tcr @ ImplicitRuneS(_)),
            None)) => tcr
      }

    val tCoordRuneFromRules =
      main.rules shouldHave {
        case AugmentSR(_, tcr, Vector(OwnershipLiteralSL(ConstraintP),PermissionLiteralSL(ReadonlyP)), RuneUsage(_, CodeRuneS("T"))) => tcr
      }

    tCoordRuneFromParams shouldEqual tCoordRuneFromRules
  }

  test("Anonymous typed param") {
    val program1 = compile("""fn main(_ int) infer-ret { }""")
    val main = program1.lookupFunction("main")
    val Vector(param) = main.params
    val paramRune =
      param match {
        case ParameterS(
          AtomSP(_,
          None,
            None,
            RuneUsage(_, pr @ ImplicitRuneS(_)),
            None)) => pr
      }

    main.rules shouldHave {
      case LookupSR(_, pr, CodeTypeNameS("int")) => vassert(pr == paramRune)
    }
  }

  test("Anonymous untyped param") {
    val program1 = compile("""fn main(_) infer-ret { }""")
    val main = program1.lookupFunction("main")
    val Vector(param) = main.params
    param match {
      case ParameterS(
       AtomSP(_,
        None,
        None,
        RuneUsage(_, pr @ ImplicitRuneS(_)),
        None)) => pr
    }
  }

  test("Rune destructure") {
    // This is an ambiguous case but we decided it should destructure a struct or sequence, see CSTODTS in docs.
    val program1 = compile("""fn main<T>(moo T(a int)) infer-ret { }""")
    val main = program1.lookupFunction("main")

    val Vector(param) = main.params

    val (aRune, tRune) =
      param match {
        case ParameterS(
          AtomSP(_,
            Some(CaptureS(CodeVarNameS("moo"))),
            None,
            tr,
            Some(
              Vector(
                AtomSP(_,
                  Some(CaptureS(CodeVarNameS("a"))),
                  None,
                  RuneUsage(_, ar @ ImplicitRuneS(_)),
                None))))) => (ar, tr)
      }

    main.rules shouldHave {
      case LookupSR(_, air, CodeTypeNameS("int")) => vassert(air == aRune)
    }

    // See CCAUIR.
    main.identifyingRunes shouldEqual Vector(tRune)
  }

  test("Regioned pure function") {
    val bork = compile("fn main<'r ro>(ship 'r &Spaceship) pure 't { }")

    val main = bork.lookupFunction("main")
    // We dont support regions yet, so scout should filter them out.
    main.identifyingRunes.size shouldEqual 0
  }

  test("Test param-less lambda identifying runes") {
    val bork = compile(
      """
        |fn main() int export {do({ 3 })}
        |""".stripMargin)

    val main = bork.lookupFunction("main")
    // We dont support regions yet, so scout should filter them out.
    main.identifyingRunes.size shouldEqual 0
    val lambda = Collector.onlyOf(main.body, classOf[FunctionSE])
    lambda.function.identifyingRunes.size shouldEqual 0
  }

  test("Test one-param lambda identifying runes") {
    val bork = compile(
      """
        |fn main() int export {do({ _ })}
        |""".stripMargin)

    val main = bork.lookupFunction("main")
    // We dont support regions yet, so scout should filter them out.
    main.identifyingRunes.size shouldEqual 0
    val lambda = Collector.onlyOf(main.body, classOf[FunctionSE])
    lambda.function.identifyingRunes.size shouldEqual 1
  }

}
