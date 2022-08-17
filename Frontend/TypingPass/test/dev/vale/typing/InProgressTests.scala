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

  test("Basic interface forwarder") {
    val compile = CompilerTestCompilation.test(
      """
        |sealed interface Bork {
        |  func bork(virtual self &Bork) int;
        |}
        |
        |struct BorkForwarder<Lam>
        |where func drop(Lam)void, func __call(&Lam)int {
        |  lam Lam;
        |}
        |
        |impl<Lam> Bork for BorkForwarder<Lam>
        |where func drop(Lam)void, func __call(&Lam)int;
        |func bork<Lam>(self &BorkForwarder<Lam>) int
        |where func drop(Lam)void, func __call(&Lam)int {
        |  return (&self.lam)();
        |}
        |
        |exported func main() int {
        |  f = BorkForwarder({ 7 });
        |  return f.bork();
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

  // Depends on Basic interface anonymous subclass
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

  // Depends on anonymous interfaces
  test("Lambda is incompatible anonymous interface") {
    val compile = CompilerTestCompilation.test(
      """
        |interface AFunction1<P> where P Ref {
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

  test("DO NOT SUBMIT") {
    vimpl() // this is a reminder to put a DO NOT SUBMIT presubmit check in

    vimpl() // OSDCE might be obsolete

    vimpl() // add a test for a lambda that is used twice

    vimpl() // add test for callsite Cons<Cons<int>> rune collision
  }
}
