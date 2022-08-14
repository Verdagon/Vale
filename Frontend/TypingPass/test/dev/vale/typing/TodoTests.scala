package dev.vale.typing

import dev.vale.Collector.ProgramWithExpect
import dev.vale.postparsing._
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.solver.{FailedSolve, RuleError, Step}
import dev.vale.typing.OverloadResolver.{FindFunctionFailure, SpecificParamDoesntSend, WrongNumberOfArguments}
import dev.vale.typing.ast.{ConstantIntTE, DestroyTE, DiscardTE, FunctionCallTE, FunctionHeaderT, FunctionT, KindExportT, LetAndLendTE, LetNormalTE, LocalLookupTE, ParameterT, PrototypeT, ReferenceMemberLookupTE, ReturnTE, SignatureT, SoftLoadTE, StructToInterfaceUpcastTE, referenceExprResultKind, referenceExprResultStructName, _}
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.infer.KindIsNotConcrete
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{CodeLocationS, Collector, Err, FileCoordinateMap, Ok, PackageCoordinate, RangeS, Tests, vassert, vassertOne, vpass, vwat, _}
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List
import scala.io.Source

class TodoTests extends FunSuite with Matchers {
  // TODO: pull all of the typingpass specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }

  test("Tests overload set and concept function") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.print.*;
        |import v.builtins.drop.*;
        |import v.builtins.str.*;
        |
        |func moo<X, F>(x X, f F)
        |where func(&F, &X)void, func drop(X)void, func drop(F)void {
        |  f(&x);
        |}
        |exported func main() {
        |  moo("hello", print);
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Prints bread crumb trail") {
    val compile = CompilerTestCompilation.test(
      """
        |import printutils.*;
        |import v.builtins.panic.*;
        |
        |#!DeriveInterfaceDrop
        |sealed interface Opt<T> where T Ref { }
        |#!DeriveStructDrop
        |struct Some<T> where T Ref { value T; }
        |#!DeriveImplDrop
        |impl<T> Opt<T> for Some<T>;
        |#!DeriveStructDrop
        |struct None<T> where T Ref { }
        |#!DeriveImplDrop
        |impl<T> Opt<T> for None<T>;
        |
        |abstract func drop<T>(virtual opt Opt<T>)
        |where func drop(T)void;
        |
        |func drop<T>(opt Some<T>)
        |where func drop(T)void
        |{
        |  [x] = opt;
        |}
        |
        |func drop<T>(opt None<T>) {
        |  [ ] = opt;
        |}
        |
        |abstract func isEmpty<T>(virtual opt &Opt<T>) bool;
        |func isEmpty<T>(opt &None<T>) bool { return true; }
        |func isEmpty<T>(opt &Some<T>) bool { return false; }
        |
        |abstract func isEmpty<T>(virtual opt Opt<T>) bool;
        |func isEmpty<T>(opt None<T>) bool { return true; }
        |func isEmpty<T>(opt Some<T>) bool
        |where func drop(T)void
        |{ return false; }
        |
        |abstract func get<T>(virtual opt Opt<T>) T;
        |func get<T>(opt None<T>) T { panic("Called get() on a None!"); }
        |func get<T>(opt Some<T>) T {
        |  [value] = opt;
        |  return value;
        |}
        |
        |abstract func get<T>(virtual opt &Opt<T>) &T;
        |func get<T>(opt &None<T>) &T { panic("Called get() on a None!"); }
        |func get<T>(opt &Some<T>) &T { return &opt.value; }
        |
        |
        |#!DeriveStructDrop
        |struct MyList<T Ref> {
        |  value T;
        |  next Opt<MyList<T>>;
        |}
        |
        |func drop<T>(this MyList<T>)
        |where func drop(T)void {
        |  [value, next] = this;
        |}
        |
        |func printValues(list &MyList<int>) void {
        |  print(list.value);
        |  printNextValue(list.next);
        |}
        |
        |func printNextValue(virtual opt &Opt<MyList<int>>) void { }
        |func printNextValue(opt &None<MyList<int>>) void { }
        |func printNextValue(opt &Some<MyList<int>>) void {
        |  printValues(opt.value);
        |}
        |
        |
        |exported func main() int {
        |  list = MyList<int>(10, Some<MyList<int>>(MyList<int>(20, Some<MyList<int>>(MyList<int>(30, None<MyList<int>>())))));
        |  printValues(&list);
        |  return 0;
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    // Ensure it properly prints out that the original error is from isEmpty
    // Also prune it down a bit
    vimpl()
  }

  // Depends on free
  test("Can mutate an element in a runtime-sized array") {
    val compile = CompilerTestCompilation.test(
      """
        |
        |import v.builtins.arrays.*;
        |import v.builtins.drop.*;
        |exported func main() int {
        |  arr = Array<mut, int>(3);
        |  arr.push(0);
        |  arr.push(1);
        |  arr.push(2);
        |  set arr[1] = 10;
        |  return 73;
        |}
        |""".stripMargin)
    compile.expectCompilerOutputs()
  }

  // DO NOT SUBMIT fails anonymous subclass
  test("Test MakeArray") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.arith.*;
        |import array.make.*;
        |import v.builtins.arrays.*;
        |import v.builtins.drop.*;
        |import ifunction.ifunction1.*;
        |
        |exported func main() int {
        |  a = MakeArray(11, {_});
        |  return len(&a);
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  // Depends on Basic interface anonymous subclass
  test("Reports error") {
    // https://github.com/ValeLang/Vale/issues/548

    val compile = CompilerTestCompilation.test(
      """
        |interface A {
        |	func foo(virtual a &A) int;
        |}
        |
        |struct B imm { val int; }
        |impl A for B;
        |
        |func foo(b &B) int { return b.val; }
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()

    vimpl()
  }

  // Interface bounds, downcasting
  test("Downcast with as") {
    vimpl() // can we solve this by putting an impl in the environment for that placeholder?

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

  // DO NOT SUBMIT fails lambda in template
  test("Test array push, pop, len, capacity, drop") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.arrays.*;
        |import v.builtins.drop.*;
        |
        |exported func main() void {
        |  arr = Array<mut, int>(9);
        |  arr.push(420);
        |  arr.push(421);
        |  arr.push(422);
        |  arr.len();
        |  arr.capacity();
        |  // implicit drop with pops
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  // right now there is no collision because they have different template names.
  test("Reports when two functions with same signature") {
    val compile = CompilerTestCompilation.test(
      """
        |exported func moo() int { return 1337; }
        |exported func moo() int { return 1448; }
        |""".stripMargin)
    compile.getCompilerOutputs() match {
      case Err(FunctionAlreadyExists(_, _, SignatureT(FullNameT(_, Vector(), FunctionNameT(FunctionTemplateNameT(StrI("moo"), _), Vector(), Vector()))))) =>
    }
  }

  // Interface bounds, downcasting
  test("Report when downcasting to interface") {
    vimpl() // can we solve this by putting an impl in the environment for that placeholder?

    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.as.*;
        |import panicutils.*;
        |
        |interface ISuper { }
        |interface ISub { }
        |impl ISuper for ISub;
        |
        |exported func main() {
        |  ship = __pretend<ISuper>();
        |  ship.as<ISub>();
        |}
        |""".stripMargin)
    compile.getCompilerOutputs() match {
      case Err(CantDowncastToInterface(_, _)) =>
    }
  }
}
