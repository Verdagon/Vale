package dev.vale.typing

import dev.vale.postparsing.{CodeNameS, TopLevelStructDeclarationNameS}
import dev.vale.solver.{FailedSolve, RuleError}
import dev.vale.typing.OverloadResolver.{InferFailure, SpecificParamDoesntSend}
import dev.vale.typing.ast.{DestroyTE, DiscardTE, FunctionCallTE, FunctionT, PrototypeT, ReferenceMemberTypeT, SignatureT, UnletTE, referenceExprResultKind, referenceExprResultStructName}
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.infer.OwnershipDidntMatch
import dev.vale.typing.names.{FreeNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.{CoordTemplata, IntegerTemplata, MutabilityTemplata, functionName, simpleName}
import dev.vale.typing.types.{BoolT, BorrowT, CoordT, ImmutableT, IntT, InterfaceTT, OwnT, RuntimeSizedArrayTT, ShareT, StructTT, VoidT}
import dev.vale.{Collector, Err, Ok, StrI, Tests, vassert, vassertOne, vfail, vimpl, vwat}
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class InProgressTests extends FunSuite with Matchers {
  // TODO: pull all of the typingpass specific stuff out, the unit test-y stuff

  // DO NOT SUBMIT fails lambda inside template
  test("Lambda inside template") {
    // This originally didn't work because both helperFunc<int> and helperFunc<Str>
    // made a closure struct called helperFunc:lam1, which collided.
    // This is what spurred paackage support.

    val compile = CompilerTestCompilation.test(
      """
        |import printutils.*;
        |
        |func helperFunc<T>(x T) {
        |  { print(x); }();
        |}
        |exported func main() {
        |  helperFunc(4);
        |  helperFunc("bork");
        |}
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  // DO NOT SUBMIT fails: imm generics
  test("Report imm mut mismatch for generic type") {
    val compile = CompilerTestCompilation.test(
      """
        |struct MyImmContainer<T Ref imm> imm { value T; }
        |struct MyMutStruct { }
        |exported func main() { x = MyImmContainer<MyMutStruct>(MyMutStruct()); }
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("DO NOT SUBMIT") {
    vimpl() // this is a reminder to put a DO NOT SUBMIT presubmit check in

    vimpl() // OSDCE might be obsolete

    vimpl() // add a test for a lambda that is used twice
  }
}
