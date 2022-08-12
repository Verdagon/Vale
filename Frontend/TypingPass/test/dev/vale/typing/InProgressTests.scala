package dev.vale.typing

import dev.vale.postparsing.{CodeNameS, TopLevelStructDeclarationNameS}
import dev.vale.solver.{FailedSolve, RuleError}
import dev.vale.typing.OverloadResolver.{InferFailure, SpecificParamDoesntSend}
import dev.vale.typing.ast.{DestroyTE, DiscardTE, FunctionCallTE, FunctionT, PrototypeT, ReferenceMemberTypeT, SignatureT, referenceExprResultKind, referenceExprResultStructName}
import dev.vale.typing.infer.OwnershipDidntMatch
import dev.vale.typing.names.{FreeNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.{CoordTemplata, IntegerTemplata, MutabilityTemplata, functionName, simpleName}
import dev.vale.typing.types.{BoolT, BorrowT, CoordT, ImmutableT, IntT, InterfaceTT, OwnT, RuntimeSizedArrayTT, ShareT, StructTT}
import dev.vale.{Collector, Err, Ok, StrI, Tests, vassert, vassertOne, vfail, vimpl, vwat}
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class InProgressTests extends FunSuite with Matchers {
  // TODO: pull all of the typingpass specific stuff out, the unit test-y stuff

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

}
