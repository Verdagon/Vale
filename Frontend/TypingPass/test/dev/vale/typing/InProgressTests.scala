package dev.vale.typing

import dev.vale.postparsing.CodeNameS
import dev.vale.typing.OverloadResolver.SpecificParamDoesntSend
import dev.vale.typing.ast.{DestroyTE, DiscardTE, FunctionCallTE, FunctionT, PrototypeT, ReferenceMemberTypeT, SignatureT, referenceExprResultKind, referenceExprResultStructName}
import dev.vale.typing.names.{FreeNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.{CoordTemplata, IntegerTemplata, MutabilityTemplata, functionName, simpleName}
import dev.vale.typing.types.{BoolT, CoordT, ImmutableT, IntT, InterfaceTT, OwnT, RuntimeSizedArrayTT, ShareT, StructTT}
import dev.vale.{Collector, Err, Ok, StrI, Tests, vassert, vassertOne, vimpl, vwat}
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class InProgressTests extends FunSuite with Matchers {
  // TODO: pull all of the typingpass specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }

  test("Test complex interface") {
    val compile = CompilerTestCompilation.test(
      Tests.loadExpected("programs/genericvirtuals/templatedinterface.vale"))
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Test return from inside if destroys locals") {
    val compile = CompilerTestCompilation.test(
      """
        |struct Marine { hp int; }
        |exported func main() int {
        |  m = Marine(5);
        |  x =
        |    if (true) {
        |      return 7;
        |    } else {
        |      m.hp
        |    };
        |  return x;
        |}
        |""".stripMargin)// +
    //        Tests.loadExpected("castutils/castutils.vale") +
    //        Tests.loadExpected("printutils/printutils.vale"))
    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("main")
    val destructorCalls =
      Collector.all(main, {
        case fpc @ FunctionCallTE(PrototypeT(FullNameT(_,Vector(StructNameT(StructTemplateNameT(StrI("Marine")),Vector())),FunctionNameT(FunctionTemplateNameT(StrI("drop"), _),Vector(),Vector(CoordT(_,StructTT(simpleName("Marine")))))),_),_) => fpc
      })
    destructorCalls.size shouldEqual 2
  }

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

  test("Test array push, pop, len, capacity, drop") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.arith.*;
        |import array.make.*;
        |import v.builtins.arrays.*;
        |import v.builtins.drop.*;
        |import ifunction.ifunction1.*;
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

  test("Test imm array") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.panic.*;
        |import v.builtins.drop.*;
        |import v.builtins.arrays.*;
        |import v.builtins.functor1.*;
        |export #[]int as ImmArrInt;
        |exported func main(arr #[]int) {
        |  __vbi_panic();
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("main")
    main.header.params.head.tyype.kind match { case RuntimeSizedArrayTT(MutabilityTemplata(ImmutableT), _) => }
  }

  test("DO NOT SUBMIT") {
    vimpl() // this is a reminder to put a DO NOT SUBMIT presubmit check in

    vimpl() // OSDCE might be obsolete

    vimpl() // add a test for a lambda that is used twice
  }

  test("Test interface default generic argument in type") {
    val compile = CompilerTestCompilation.test(
      """
        |sealed interface MyHashSet<K Ref, H Int = 5> { }
        |struct MyInterface {
        |  x MyHashSet<bool>();
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val moo = coutputs.lookupInterface("MyInterface")
    val tyype = Collector.only(moo, { case ReferenceMemberTypeT(c) => c.unsubstitutedCoord })
    tyype match {
      case CoordT(
      OwnT,
      InterfaceTT(
      FullNameT(_,_,
      InterfaceNameT(
      InterfaceTemplateNameT(StrI("MyHashSet")),
      Vector(
      CoordTemplata(CoordT(ShareT,BoolT())),
      IntegerTemplata(5)))))) =>
    }
  }

  test("Test struct default generic argument in type") {
    val compile = CompilerTestCompilation.test(
      """
        |struct MyHashSet<K Ref, H Int = 5> { }
        |struct MyStruct {
        |  x MyHashSet<bool>();
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val moo = coutputs.lookupStruct("MyStruct")
    val tyype = Collector.only(moo, { case ReferenceMemberTypeT(c) => c.unsubstitutedCoord })
    tyype match {
      case CoordT(
        OwnT,
        StructTT(
          FullNameT(_,_,
            StructNameT(
              StructTemplateNameT(StrI("MyHashSet")),
              Vector(
                CoordTemplata(CoordT(ShareT,BoolT())),
                IntegerTemplata(5)))))) =>
    }
  }

  test("Tests calling an abstract function") {
    val compile = CompilerTestCompilation.test(
      Tests.loadExpected("programs/genericvirtuals/callingAbstract.vale"))
    val coutputs = compile.expectCompilerOutputs()

    coutputs.functions.collectFirst({
      case FunctionT(header @ functionName("doThing"), _) if header.getAbstractInterface != None => true
    }).get
  }

  // See DSDCTD
  test("Tests destructuring shared doesnt compile to destroy") {
    val compile = CompilerTestCompilation.test(
      """
        |
        |struct Vec3i imm {
        |  x int;
        |  y int;
        |  z int;
        |}
        |
        |exported func main() int {
        |	 Vec3i[x, y, z] = Vec3i(3, 4, 5);
        |  return y;
        |}
      """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()

    Collector.all(coutputs.lookupFunction("main"), {
      case DestroyTE(_, _, _) =>
    }).size shouldEqual 0

    // Make sure there's a destroy in its destructor though.
    val destructor =
      vassertOne(
        coutputs.functions.collect({
          case f if (f.header.fullName.last match { case FreeNameT(_, _, _) => true case _ => false }) => f
        }))

    Collector.only(destructor, { case DestroyTE(referenceExprResultStructName(StrI("Vec3i")), _, _) => })
    Collector.all(destructor, { case DiscardTE(referenceExprResultKind(IntT(_))) => }).size shouldEqual 3
  }

  test("Reports when ownership doesnt match") {
    val compile = CompilerTestCompilation.test(
      """
        |
        |struct Firefly {}
        |func getFuel(self &Firefly) int { return 7; }
        |
        |exported func main() int {
        |  f = Firefly();
        |  return (f).getFuel();
        |}
        |""".stripMargin
    )
    compile.getCompilerOutputs() match {
      case Err(CouldntFindFunctionToCallT(range, fff)) => {
        fff.name match { case CodeNameS(StrI("getFuel")) => }
        fff.rejectedCalleeToReason.size shouldEqual 1
        val reason = fff.rejectedCalleeToReason.head._2
        reason match { case SpecificParamDoesntSend(0, _, _) => }
      }
    }
  }

  test("Report when num elements mismatch") {
    val compile = CompilerTestCompilation.test(
      """
        |struct Spaceship imm {
        |  name! str;
        |  numWings int;
        |}
        |exported func main() bool {
        |  arr = [#4][true, false, false];
        |  return arr.0;
        |}
        |""".stripMargin)
    compile.getCompilerOutputs() match {
      case Err(InitializedWrongNumberOfElements(_, 4, 3)) =>
    }
  }

  test("Report when downcasting to interface") {
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

  test("Reports when exported SSA depends on non-exported element") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.arrays.*;
        |import v.builtins.functor1.*;
        |export [#5]<imm>Raza as RazaArray;
        |struct Raza imm { }
        |""".stripMargin)
    compile.getCompilerOutputs() match {
      case Err(ExportedImmutableKindDependedOnNonExportedKind(_, _, _, _)) =>
    }
  }
  test("Reports when exported RSA depends on non-exported element") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.arrays.*;
        |import v.builtins.functor1.*;
        |export []<imm>Raza as RazaArray;
        |struct Raza imm { }
        |""".stripMargin)
    compile.getCompilerOutputs() match {
      case Err(ExportedImmutableKindDependedOnNonExportedKind(_, _, _, _)) =>
    }
  }

}
