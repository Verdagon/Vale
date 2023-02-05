package dev.vale

import dev.vale.simplifying.VonHammer
import dev.vale.finalast.YonderH
import dev.vale.typing._
import dev.vale.typing.types.StrT
import dev.vale.testvm.StructInstanceV
import dev.vale.von.VonInt
import dev.vale.{finalast => m}
import org.scalatest.{FunSuite, Matchers}

class PureTests extends FunSuite with Matchers {
  test("Simple pure function") {
    val compile =
      RunCompilation.test(
        """
          |struct Engine {
          |  fuel int;
          |}
          |struct Spaceship {
          |  engine Engine;
          |}
          |pure func pfunc(s &Spaceship) int {
          |  return s.engine.fuel;
          |}
          |exported func main() int {
          |  s = Spaceship(Engine(10));
          |  return pfunc(&s);
          |}
          |""".stripMargin)
    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

  test("Simple pure block") {
    val compile =
      RunCompilation.test(
        """
          |struct Engine { fuel int; }
          |struct Spaceship { engine Engine; }
          |exported func main() int {
          |  s = Spaceship(Engine(10));
          |  pure block { s.engine.fuel }
          |}
          |""".stripMargin, false)
    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }
}
