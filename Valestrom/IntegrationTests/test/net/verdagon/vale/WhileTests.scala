package net.verdagon.vale

import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}
import net.verdagon.vale.driver.Compilation

class WhileTests extends FunSuite with Matchers {
  test("Simple while loop that doesnt execute") {
    val compile = Compilation(
      """
        |fn main() int export {
        |  while (false) {}
        |  = 5;
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Test a for-ish while loop") {
    val compile = Compilation(
      """
        |fn main() int export {
        |  i! = 0;
        |  while (i < 4) {
        |    mut i = i + 1;
        |  }
        |  = i;
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  }

  test("Tests a while loop with a complex condition") {
    val compile = Compilation(
      """
        |fn main() int export {
        |  key! = 0;
        |  while (mut key = __getch(); = key < 96;) {
        |    print(key);
        |  }
        |  = key;
        |}
      """.stripMargin +
        Samples.get("libraries/castutils.vale") +
        Samples.get("libraries/printutils.vale"))

    compile.evalForReferend(Vector(), List("A", "B", "c")) shouldEqual VonInt(99)
  }

  test("Tests a while loop with a != in it") {
    val compile = Compilation(
      """
        |fn main() int export {
        |  key! = 0;
        |  while (mut key = __getch(); = key != 99;) {
        |    print(key);
        |  }
        |  = key;
        |}
      """.stripMargin +
        Samples.get("libraries/castutils.vale") +
        Samples.get("libraries/printutils.vale") +
        Samples.get("libraries/utils.vale"))

    compile.evalForReferend(Vector(), List("A", "B", "c")) shouldEqual VonInt(99)
  }

  test("Return from infinite while loop") {
    val compile = Compilation(
      """
        |fn main() int export {
        |  while (true) {
        |    ret 9;
        |  }
        |  = __panic();
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }
//
//  test("Tests a while loop with a move in it") {
//    val compile = Compilation(
//      """
//        |fn doThings(m: Marine) { }
//        |struct Marine { hp: int; }
//        |fn main() int export {
//        |  m = Marine(7);
//        |  while (true) {
//        |    doThings(m);
//        |  }
//        |  = 4;
//        |}
//      """.stripMargin)
//
//    // should fail
//
//    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
//  }
}
