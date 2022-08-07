package dev.vale.typing

import dev.vale.typing.ast.{AsSubtypeTE, SignatureT}
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.CoordTemplata
import dev.vale.typing.types._
import dev.vale.{Collector, StrI, vassert}
import dev.vale.typing.types.InterfaceTT
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Set

class CompilerVirtualTests extends FunSuite with Matchers {

  test("Regular interface and struct") {
    val compile = CompilerTestCompilation.test(
      """
        |sealed interface Opt { }
        |
        |struct Some { x int; }
        |impl Opt for Some;
      """.stripMargin)
    val interner = compile.interner
    val coutputs = compile.expectCompilerOutputs()

    // Make sure there's two drop functions
    val dropFuncNames =
      coutputs.functions.map(_.header.fullName).collect({
        case f @ FullNameT(_, _, FunctionNameT(FunctionTemplateNameT(StrI("drop"), _), _, _)) => f
      })
    dropFuncNames.size shouldEqual 2
  }

  test("Implementing two interfaces causes no vdrop conflict") {
    // See NIIRII
    val compile = CompilerTestCompilation.test(
      """
        |struct MyStruct {}
        |
        |interface IA {}
        |impl IA for MyStruct;
        |
        |interface IB {}
        |impl IB for MyStruct;
        |
        |func bork(a IA) {}
        |func zork(b IB) {}
        |exported func main() {
        |  bork(MyStruct());
        |  zork(MyStruct());
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Basic interface anonymous subclass") {
    val compile = CompilerTestCompilation.test(
      """
        |interface Bork {
        |  func bork(virtual self &Bork) int;
        |}
        |
        |exported func main() int {
        |  f = Bork({ 7 });
        |  return f.bork();
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Basic IFunction1 anonymous subclass") {
    val compile = CompilerTestCompilation.test(
      """
        |
        |import ifunction.ifunction1.*;
        |
        |exported func main() int {
        |  f = IFunction1<mut, int, int>({_});
        |  return (f)(7);
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Upcast") {
    val compile = CompilerTestCompilation.test(
      """
        |
        |interface IShip {}
        |struct Raza { fuel int; }
        |impl IShip for Raza;
        |
        |exported func main() {
        |  ship IShip = Raza(42);
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Downcast with as") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.as.*;
        |import v.builtins.drop.*;
        |
        |interface IShip {}
        |
        |struct Raza { fuel int; }
        |impl IShip for Raza;
        |
        |exported func main() {
        |  ship IShip = Raza(42);
        |  ship.as<Raza>();
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()

    Collector.only(coutputs.lookupFunction("as"), {
      case as @ AsSubtypeTE(sourceExpr, targetSubtype, resultOptType, okConstructor, errConstructor) => {
        sourceExpr.result.reference match {
          case CoordT(BorrowT,InterfaceTT(FullNameT(_, Vector(),InterfaceNameT(InterfaceTemplateNameT(StrI("IShip")),Vector())))) =>
        }
        targetSubtype match {
          case StructTT(FullNameT(_, Vector(),StructNameT(StructTemplateNameT(StrI("Raza")),Vector()))) =>
        }
        val (firstGenericArg, secondGenericArg) =
          resultOptType match {
            case CoordT(
              OwnT,
              InterfaceTT(
                FullNameT(
                  _, Vector(),
                  InterfaceNameT(
                    InterfaceTemplateNameT(StrI("Result")),
                    Vector(firstGenericArg, secondGenericArg))))) => (firstGenericArg, secondGenericArg)
          }
        // They should both be pointers, since we dont really do borrows in structs yet
        firstGenericArg match {
          case CoordTemplata(
            CoordT(
              BorrowT,
              StructTT(FullNameT(_, Vector(),StructNameT(StructTemplateNameT(StrI("Raza")),Vector()))))) =>
        }
        secondGenericArg match {
          case CoordTemplata(
            CoordT(
              BorrowT,
              InterfaceTT(FullNameT(_, Vector(),InterfaceNameT(InterfaceTemplateNameT(StrI("IShip")),Vector()))))) =>
        }
        vassert(okConstructor.paramTypes.head.kind == targetSubtype)
        vassert(errConstructor.paramTypes.head.kind == sourceExpr.result.reference.kind)
        as
      }
    })
  }

  test("Virtual with body") {
    CompilerTestCompilation.test(
      """
        |interface IBork { }
        |struct Bork { }
        |impl IBork for Bork;
        |
        |func rebork(virtual result *IBork) bool { true }
        |exported func main() {
        |  rebork(&Bork());
        |}
        |""".stripMargin)
  }

  test("Templated interface and struct") {
    val compile = CompilerTestCompilation.test(
      """
        |sealed interface Opt<T Ref>
        |where func drop(T)void
        |{ }
        |
        |struct Some<T>
        |where func drop(T)void
        |{ x T; }
        |
        |impl<T> Opt<T> for Some<T>
        |where func drop(T)void;
        |""".stripMargin)
    val interner = compile.interner
    val coutputs = compile.expectCompilerOutputs()
    val dropFuncNames =
      coutputs.functions.map(_.header.fullName).collect({
        case f @ FullNameT(_, _, FunctionNameT(FunctionTemplateNameT(StrI("drop"), _), _, _)) => f
      })
    dropFuncNames.size shouldEqual 2
  }

  test("Custom drop with concept function") {
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveInterfaceDrop
        |sealed interface Opt<T Ref> { }
        |
        |abstract func drop<T>(virtual opt Opt<T>)
        |where func drop(T)void;
        |
        |#!DeriveStructDrop
        |struct Some<T> { x T; }
        |impl<T> Opt<T> for Some<T>;
        |
        |func drop<T>(opt Some<T>)
        |where func drop(T)void
        |{
        |  [x] = opt;
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

}
