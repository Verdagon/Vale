package net.verdagon.vale.templar

import net.verdagon.vale._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.rules.{CoordComponentsSR, KindComponentsSR, RuneUsage}
import net.verdagon.vale.solver.{FailedSolve, IncompleteSolve, RuleError, Step}
import net.verdagon.vale.templar.OverloadTemplar.{FindFunctionFailure, WrongNumberOfArguments}
import net.verdagon.vale.templar.ast.{ConstantIntTE, FunctionCallTE, KindExportT, PrototypeT, SignatureT, StructToInterfaceUpcastTE}
import net.verdagon.vale.templar.env.ReferenceLocalVariableT
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.infer.KindIsNotConcrete
import net.verdagon.vale.templar.names.{CitizenNameT, FullNameT, FunctionNameT}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
//import net.verdagon.vale.templar.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class TemplarSolverTests extends FunSuite with Matchers {
  // TODO: pull all of the templar specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }


  test("Humanize errors") {
    val fireflyKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Firefly", Vector())))
    val fireflyCoord = CoordT(OwnT,ReadwriteT,fireflyKind)
    val serenityKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Serenity", Vector())))
    val serenityCoord = CoordT(OwnT,ReadwriteT,serenityKind)
    val ispaceshipKind = InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ISpaceship", Vector())))
    val ispaceshipCoord = CoordT(OwnT,ReadwriteT,ispaceshipKind)
    val unrelatedKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Spoon", Vector())))
    val unrelatedCoord = CoordT(OwnT,ReadwriteT,unrelatedKind)
    val fireflySignature = SignatureT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), FunctionNameT("myFunc", Vector(), Vector(fireflyCoord))))
    val fireflyExport = KindExportT(RangeS.testZero, fireflyKind, PackageCoordinate.TEST_TLD, "Firefly");
    val serenityExport = KindExportT(RangeS.testZero, fireflyKind, PackageCoordinate.TEST_TLD, "Serenity");

    val codeStr = "Hello I am A large piece Of code [that has An error]"
    val filenamesAndSources = FileCoordinateMap.test(codeStr)
    def makeLoc(pos: Int) = CodeLocationS(FileCoordinate.test, pos)
    def makeRange(begin: Int, end: Int) = RangeS(makeLoc(begin), makeLoc(end))

    val unsolvedRules =
      Vector(
        CoordComponentsSR(
          makeRange(0, codeStr.length),
          RuneUsage(makeRange(6, 7), CodeRuneS("I")),
          RuneUsage(makeRange(11, 12), CodeRuneS("A")),
          RuneUsage(makeRange(25, 27), CodeRuneS("Of")),
          RuneUsage(makeRange(33, 52), ImplicitRuneS(LocationInDenizen(Vector(7))))),
        KindComponentsSR(
          makeRange(33, 52),
          RuneUsage(makeRange(33, 52), ImplicitRuneS(LocationInDenizen(Vector(7)))),
          RuneUsage(makeRange(43, 45), CodeRuneS("An"))))

    vassert(TemplarErrorHumanizer.humanize(false, filenamesAndSources,
      TemplarSolverError(
        RangeS.testZero,
        FailedSolve(
          Vector(
            Step(
              false,
              Vector(),
              Vector(),
              Map(
                CodeRuneS("A") -> OwnershipTemplata(OwnT)))),
          unsolvedRules,
          RuleError(KindIsNotConcrete(ispaceshipKind)))))
      .nonEmpty)

    val errorText =
      TemplarErrorHumanizer.humanize(false, filenamesAndSources,
        TemplarSolverError(
          RangeS.testZero,
          IncompleteSolve(
            Vector(
              Step(
                false,
                Vector(),
                Vector(),
                Map(
                  CodeRuneS("A") -> OwnershipTemplata(OwnT)))),
            unsolvedRules,
            Set(
              CodeRuneS("I"),
              CodeRuneS("Of"),
              CodeRuneS("An"),
              ImplicitRuneS(LocationInDenizen(Vector(7)))))))
    println(errorText)
    vassert(errorText.nonEmpty)
    vassert(errorText.contains("\n  A:       ^ own"))
    vassert(errorText.contains("\n  I:  ^ (unknown)"))
    vassert(errorText.contains("\n  _7:                            ^^^^^^^^^^^^^^^^^^^ (unknown)"))
  }

  test("Simple int rule") {
    val compile = TemplarTestCompilation.test(
      """
        |fn main() rules(N int = 3) int export {
        |  N
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
    Collector.only(temputs.lookupFunction("main"), { case ConstantIntTE(3, 32) => })
  }

  test("Equals transitive") {
    val compile = TemplarTestCompilation.test(
      """
        |fn main() rules(N int = 3, M int = N) int export {
        |  M
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
    Collector.only(temputs.lookupFunction("main"), { case ConstantIntTE(3, 32) => })
  }

  test("OneOf") {
    val compile = TemplarTestCompilation.test(
      """
        |fn main() rules(N int = 2 | 3 | 4, N = 3) int export {
        |  N
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
    Collector.only(temputs.lookupFunction("main"), { case ConstantIntTE(3, 32) => })
  }

  test("Components") {
    val compile = TemplarTestCompilation.test(
      """
        |struct MyStruct export { }
        |fn main()
        |rules(
        |  MyStruct = T Ref(O Ownership, P Permission, K Kind),
        |  X Ref(borrow, ro, K))
        |X export {
        |  &MyStruct()
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
    temputs.lookupFunction("main").header.returnType match {
      case CoordT(ConstraintT, ReadonlyT, StructTT(_)) =>
    }
  }

  test("Prototype rule") {
    val compile = TemplarTestCompilation.test(
      """
        |fn moo(i int, b bool) str { "hello" }
        |fn main()
        |rules(
        |  mooFunc Prot("moo", (int, bool), _))
        |str export {
        |  (mooFunc)(5, true)
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
    Collector.only(temputs.lookupFunction("main"), {
      case FunctionCallTE(PrototypeT(simpleName("moo"), _), _) =>
    })
  }

  test("Send struct to struct") {
    val compile = TemplarTestCompilation.test(
      """
        |struct MyStruct {}
        |fn moo(m MyStruct) { }
        |fn main() export {
        |  moo(MyStruct())
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
  }

  test("Send struct to interface") {
    val compile = TemplarTestCompilation.test(
      """
        |struct MyStruct {}
        |interface MyInterface {}
        |impl MyInterface for MyStruct;
        |fn moo(m MyInterface) { }
        |fn main() export {
        |  moo(MyStruct())
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
  }

  test("Assume most specific generic param") {
    val compile = TemplarTestCompilation.test(
      """
        |struct MyStruct {}
        |interface MyInterface {}
        |impl MyInterface for MyStruct;
        |fn moo<T>(m T) { }
        |fn main() export {
        |  moo(MyStruct())
        |}
        |""".stripMargin
    )

    val temputs = compile.expectTemputs()
    temputs.lookupFunction("moo").header.params.head.tyype match {
      case CoordT(_, _, StructTT(_)) =>
    }
  }

  test("Assume most specific common ancestor") {
    val compile = TemplarTestCompilation.test(
      """
        |interface IShip {}
        |struct Firefly {}
        |impl IShip for Firefly;
        |struct Serenity {}
        |impl IShip for Serenity;
        |fn moo<T>(a T, b T) { }
        |fn main() export {
        |  moo(Firefly(), Serenity())
        |}
        |""".stripMargin
    )

    val temputs = compile.expectTemputs()
    val moo = temputs.lookupFunction("moo")
    moo.header.params.head.tyype match {
      case CoordT(_, _, InterfaceTT(_)) =>
    }
    val main = temputs.lookupFunction("main")
    Collector.all(main, {
      case StructToInterfaceUpcastTE(_, _) =>
    }).size shouldEqual 2
  }

  test("Descendant satisfying call") {
    val compile = TemplarTestCompilation.test(
      """
        |interface IShip<T> rules(T Ref) {}
        |struct Firefly<T> rules(T Ref) {}
        |impl<T> IShip<T> for Firefly<T>;
        |fn moo<T>(a IShip<T>) { }
        |fn main() export {
        |  moo(Firefly<int>())
        |}
        |""".stripMargin
    )

    val temputs = compile.expectTemputs()
    val moo = temputs.lookupFunction("moo")
    moo.header.params.head.tyype match {
      case CoordT(_, _, InterfaceTT(FullNameT(_, _, CitizenNameT(_, Vector(CoordTemplata(CoordT(_, _, IntT(_)))))))) =>
    }
  }

  test("Reports incomplete solve") {
    val compile = TemplarTestCompilation.test(
      """
        |fn main() rules(N int) int export {
        |  M
        |}
        |""".stripMargin
    )
    compile.getTemputs() match {
      case Err(TemplarSolverError(_,IncompleteSolve(_,Vector(),unsolved))) => {
        unsolved shouldEqual Set(CodeRuneS("N"))
      }
    }
  }


  test("Stamps an interface template via a function return") {
    val compile = TemplarTestCompilation.test(
      """
        |interface MyInterface<X> rules(X Ref) { }
        |
        |struct SomeStruct<X> rules(X Ref) { x X; }
        |impl<X> MyInterface<X> for SomeStruct<X>;
        |
        |fn doAThing<T>(t T) SomeStruct<T> {
        |  SomeStruct<T>(t)
        |}
        |
        |fn main() export {
        |  doAThing(4);
        |}
        |""".stripMargin
    )
    val temputs = compile.expectTemputs()
  }
}
