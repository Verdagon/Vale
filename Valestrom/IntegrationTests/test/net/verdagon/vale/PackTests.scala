package net.verdagon.vale

import net.verdagon.vale.templar.{FullNameT, PackTE, StaticArrayFromValuesTE, TupleNameT}
import net.verdagon.vale.templar.types.{CoordT, IntT, PackTT, StructTT}
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class PackTests extends FunSuite with Matchers {
  test("Extract seq") {
    val compile = RunCompilation.test(
      """
        |fn main() int export {
        |  (x, y, z) = [5, 6, 7];
        |  = x;
        |}
      """.stripMargin)

    val temputs = compile.expectTemputs()
    val main = temputs.lookupFunction("main")
    main.all({ case StructTT(FullNameT(_, _, TupleNameT(List(_, _, _)))) => }).size shouldEqual 1

    compile.evalForKind(Vector()) shouldEqual VonInt(5)
  }

  test("Nested seqs") {
    val compile = RunCompilation.test(
      """
        |fn main() int export {
        |  (x, (y, z)) = [[4, 5], [6, 7]];
        |  = y;
        |}
      """.stripMargin)

    val temputs = compile.expectTemputs()
    val main = temputs.lookupFunction("main")
    main.all({
      case StructTT(FullNameT(_, _, TupleNameT(
        List(
          CoordT(_, _, StructTT(FullNameT(_, _, TupleNameT(List(_, _))))),
          CoordT(_, _, StructTT(FullNameT(_, _, TupleNameT(List(_, _))))))))) =>
    }).size shouldEqual 1

    compile.evalForKind(Vector()) shouldEqual VonInt(6)
  }

  test("Nested tuples") {
    val compile = RunCompilation.test(
      """
        |fn main() int export {
        |  (x, (y, z)) = [5, [6, false]];
        |  = x;
        |}
      """.stripMargin)

    val temputs = compile.expectTemputs()
    val main = temputs.lookupFunction("main")
    main .all({
      case StructTT(FullNameT(_, _, TupleNameT(
        List(
          CoordT(_, _, IntT(_)),
          CoordT(_, _, StructTT(FullNameT(_, _, TupleNameT(List(_, _))))))))) =>
    }).size shouldEqual 1

    compile.evalForKind(Vector()) shouldEqual VonInt(5)
  }

}
