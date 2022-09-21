package dev.vale

import dev.vale.finalast._
import dev.vale.highertyping.{ICompileErrorA, ProgramA}
import dev.vale.lexing.{FailedParse, RangeL}
import dev.vale.options.GlobalOptions
import dev.vale.parsing.ast.FileP
import dev.vale.passmanager.{FullCompilation, FullCompilationOptions}
import dev.vale.postparsing.{ICompileErrorS, _}
import dev.vale.testvm._
import dev.vale.typing.ast._
import dev.vale.typing.names.{FunctionNameT, FunctionTemplateNameT}
import dev.vale.typing.types.IntT
import dev.vale.typing.{Hinputs, ICompileErrorT}
import dev.vale.von.{IVonData, VonBool, VonFloat, VonInt}
import org.scalatest.{FunSuite, Matchers}


class AfterRegionsIntegrationTests extends FunSuite with Matchers {

  test("Test returning empty seq") {
    val compile = RunCompilation.test(
      """
        |export () as Tup0;
        |exported func main() () {
        |  return ();
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()

    compile.run(Vector())
  }

  test("Map function") {
    val compile = RunCompilation.test(
      Tests.loadExpected("programs/genericvirtuals/mapFunc.vale"))
    compile.expectCompilerOutputs()

    compile.evalForKind(Vector()) match { case VonBool(true) => }
  }

  test("imm tuple access") {
    vfail() // these tuples are actually mutable
    val compile = RunCompilation.test(Tests.loadExpected("programs/tuples/immtupleaccess.vale"))
    compile.evalForKind(Vector()) match { case VonInt(42) => }
  }

  test("Test overload set") {
    val compile =
      RunCompilation.test(
        """
          |import array.each.*;
          |func myfunc(i int) { }
          |exported func main() int {
          |  mylist = [#][1, 3, 3, 7];
          |  mylist.each(myfunc);
          |  42
          |}
          |""".stripMargin)
    compile.evalForKind(Vector()) match { case VonInt(42) => }
  }

  test("Simple tuple with one int") {
    val compile = RunCompilation.test( "exported func main() int { return (9,).0; }")

    val coutputs = compile.expectCompilerOutputs()
    coutputs.lookupFunction("main").header.returnType.kind shouldEqual IntT.i32
    // Funny story, theres no such thing as a one element tuple! It becomes a one element array.
    Collector.only(coutputs.lookupFunction("main"), { case TupleTE(_, _) => })

    compile.evalForKind(Vector()) match { case VonInt(9) => }
  }

  test("Upcasting in a generic function") {
    // This is testing two things:
    //  - Upcasting inside a generic function
    //  - The return type's ownership is actually calculated from the parameter. This will
    //    fail as long as we still have CoordT(Ownership, ITemplata[KindTemplataType])
    //    because that ownership isn't a templata. The call site will correctly have that
    //    ownership as borrow, but the definition will think it's an own, *not* a placeholder
    //    or variable-thing or anything like that. So, when it gets to the monomorphizer, it
    //    will actually make the wrong return type. I think the solution will be to make CoordT
    //    contain a placeholder, and move O to be a generic param.
    val compile = RunCompilation.test(
      """
        |func upcast<SuperKind Kind, SubType Ref>(left SubType) SuperType
        |where O Ownership,
        |  SubKind Kind,
        |  SuperType Ref = Ref[O, SuperKind],
        |  SubType Ref = Ref[O, SubKind],
        |  implements(SubType, SuperType)
        |{
        |  left
        |}
        |
        |sealed interface IShip  {}
        |struct Serenity {}
        |impl IShip for Serenity;
        |
        |exported func main() {
        |  ship &IShip = upcast<IShip>(&Serenity());
        |}
        |
        |""".stripMargin)

    compile.evalForKind(Vector())
  }

}
