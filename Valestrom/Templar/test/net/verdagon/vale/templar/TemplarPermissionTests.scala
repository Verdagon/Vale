package net.verdagon.vale.templar

import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale._
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, WrongNumberOfArguments}
import org.scalatest.{FunSuite, Matchers, _}

import scala.collection.immutable.List
import scala.io.Source

class TemplarPermissionTests extends FunSuite with Matchers {
  // TODO: pull all of the templar specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }


  test("Templex readonly") {
    val compile = TemplarTestCompilation.test(
      """struct Bork {}
        |fn main(a &Bork) int {
        |  = 7;
        |}
        |""".stripMargin)
    val temputs = compile.expectTemputs()

    val main = temputs.lookupFunction("main")
    Collector.only(main, {
      case FunctionHeaderT(simpleName("main"),Vector(UserFunctionT),Vector(ParameterT(_, _, CoordT(ConstraintT, ReadonlyT, StructTT(_)))), _, _) => true
    })
  }

  test("Templex readwrite") {
    val compile = TemplarTestCompilation.test(
      """struct Bork {}
        |fn main(a &!Bork) int {
        |  = 7;
        |}
        |""".stripMargin)
    val temputs = compile.expectTemputs()

    val main = temputs.lookupFunction("main")
    Collector.only(main, {
      case FunctionHeaderT(simpleName("main"),Vector(UserFunctionT),Vector(ParameterT(_, _, CoordT(ConstraintT, ReadwriteT, StructTT(_)))), _, _) => true
    })
  }

  test("Borrow readwrite member from a readonly container") {
    val compile = TemplarTestCompilation.test(
      """
        |struct Engine {}
        |struct Bork {
        |  engine Engine;
        |}
        |fn main(a &Bork) infer-ret {
        |  a.engine
        |}
        |""".stripMargin)
    val temputs = compile.expectTemputs()

    val main = temputs.lookupFunction("main")
    main.header.returnType match {
      case CoordT(ConstraintT, ReadonlyT, _) =>
    }
  }

  test("Borrow-method-call on readwrite member") {
    val compile = TemplarTestCompilation.test(
      """
        |struct Engine { }
        |struct Bork {
        |  engine Engine;
        |}
        |fn getFuel(engine &Engine) int { 42 }
        |fn main() infer-ret {
        |  bork = Bork(Engine());
        |  ret bork.engine.getFuel();
        |}
        |""".stripMargin)
    val temputs = compile.expectTemputs()
  }

}

