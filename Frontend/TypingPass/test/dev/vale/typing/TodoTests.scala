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

  test("Generic interface anonymous subclass") {
    val compile = CompilerTestCompilation.test(
      """
        |interface Bork<T Ref> {
        |  func bork(virtual self &Bork<T>, x T) int;
        |}
        |
        |exported func main() int {
        |  f = Bork((x) => { 7 });
        |  return f.bork();
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  // Depends on Generic interface anonymous subclass
  test("Lambda is incompatible anonymous interface") {
    val compile = CompilerTestCompilation.test(
      """
        |interface AFunction1<P Ref> {
        |  func __call(virtual this &AFunction1<P>, a P) int;
        |}
        |exported func main() {
        |  arr = AFunction1<int>((_) => { true });
        |}
        |""".stripMargin)

    compile.getCompilerOutputs() match {
      case Err(BodyResultDoesntMatch(_, _, _, _)) =>
      case Err(other) => vwat(CompilerErrorHumanizer.humanize(true, compile.getCodeMap().getOrDie(), other))
      case Ok(wat) => vwat(wat)
    }
  }

  // Depends on IFunction1, and maybe Generic interface anonymous subclass
  test("Basic IFunction1 anonymous subclass") {
    val compile = CompilerTestCompilation.test(
      """
        |import ifunction.ifunction1.*;
        |
        |exported func main() int {
        |  f = IFunction1<mut, int, int>({_});
        |  return (f)(7);
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }
}
