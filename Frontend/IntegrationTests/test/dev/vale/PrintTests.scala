package dev.vale

import org.scalatest.funsuite._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PrintTests extends AnyFunSuite with Matchers {
  test("Println'ing an int") {
    val compile = RunCompilation.test(
      """
        |import printutils.*;
        |exported func main() {
        |  println(6);
        |}
      """.stripMargin)

    compile.evalForStdout(Vector()) shouldEqual "6\n"
  }

  test("Println'ing a bool") {
    val compile = RunCompilation.test(
      """
        |import printutils.*;
        |exported func main() {
        |  println(true);
        |}
      """.stripMargin)

    compile.evalForStdout(Vector()) shouldEqual "true\n"
  }
}
