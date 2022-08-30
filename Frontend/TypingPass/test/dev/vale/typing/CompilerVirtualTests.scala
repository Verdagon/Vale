package dev.vale.typing

import dev.vale.typing.ast.{AsSubtypeTE, FunctionHeaderT, SignatureT}
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FreeNameT, FreeTemplateNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.CoordTemplata
import dev.vale.typing.types._
import dev.vale.{Collector, StrI, Tests, vassert}
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
    vassert(dropFuncNames.size == 2)

    val interface = coutputs.lookupInterface("Opt")
    interface.internalMethods
  }

  test("Regular open interface and struct, no anonymous interface") {
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveAnonymousSubstruct
        |interface Opt { }
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

    val interface = coutputs.lookupInterface("Opt")
    interface.internalMethods.collect({
      case FunctionHeaderT(FullNameT(_, _, FreeNameT(FreeTemplateNameT(_), _, coord)), _, _, _, _) => {
        vassert(coord.kind == interface.ref)
      }
    }).size shouldEqual 1
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

  test("Test complex interface") {
    val compile = CompilerTestCompilation.test(
      Tests.loadExpected("programs/genericvirtuals/templatedinterface.vale"))
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Test specializing interface") {
    val compile = CompilerTestCompilation.test(
      Tests.loadExpected("programs/genericvirtuals/specializeinterface.vale"))
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Use bound from struct") {
    // See NBIFPR.
    // Without it, when it tries to compile (1), at (2) it tries to resolve BorkForwarder
    // and fails bound (3) because (1) has no such bound.
    // NBIFPR says we should first get that knowledge from (2).
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveStructDrop
        |struct BorkForwarder<Lam>
        |where func __call(&Lam)int // 3
        |{
        |  lam Lam;
        |}
        |
        |
        |func bork<Lam>( // 1
        |  self &BorkForwarder<Lam> // 2
        |) int {
        |  return (self.lam)();
        |}
        |
        |exported func main() {
        |  b = BorkForwarder({ 7 });
        |  b.bork();
        |  [_] = b;
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Basic interface forwarder") {
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveInterfaceDrop
        |sealed interface Bork {
        |  func bork(virtual self &Bork) int;
        |}
        |
        |#!DeriveStructDrop
        |struct BorkForwarder<Lam>
        |where func drop(Lam)void, func __call(&Lam)int {
        |  lam Lam;
        |}
        |
        |impl<Lam> Bork for BorkForwarder<Lam>
        |where func drop(Lam)void, func __call(&Lam)int;
        |
        |func bork<Lam>(self &BorkForwarder<Lam>) int {
        |  return (self.lam)();
        |}
        |
        |exported func main() int {
        |  f = BorkForwarder({ 7 });
        |  z = f.bork();
        |  [_] = f;
        |  return z;
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

}
