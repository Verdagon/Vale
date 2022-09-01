package dev.vale.typing

import dev.vale.postparsing.{CodeNameS, TopLevelStructDeclarationNameS}
import dev.vale.solver.{FailedSolve, RuleError}
import dev.vale.typing.OverloadResolver.{InferFailure, SpecificParamDoesntSend}
import dev.vale.typing.ast.{AsSubtypeTE, DestroyTE, DiscardTE, FunctionCallTE, FunctionT, LocalLookupTE, PrototypeT, ReferenceMemberTypeT, SignatureT, SoftLoadTE, UnletTE, UpcastTE, referenceExprResultKind, referenceExprResultStructName}
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.infer.OwnershipDidntMatch
import dev.vale.typing.names.{FreeNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, PlaceholderNameT, PlaceholderTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.{CoordTemplata, IntegerTemplata, KindTemplata, MutabilityTemplata, functionName, simpleName}
import dev.vale.typing.types.{BoolT, BorrowT, CoordT, ImmutableT, IntT, InterfaceTT, OwnT, PlaceholderT, RuntimeSizedArrayTT, ShareT, StructTT, VoidT}
import dev.vale.{Collector, Err, Ok, PackageCoordinate, StrI, Tests, vassert, vassertOne, vfail, vimpl, vwat}
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class InProgressTests extends FunSuite with Matchers {

  test("DO NOT SUBMIT") {
    vimpl() // search for all DO NOT SUBMITs in the codebase and add them here, there are some

    vimpl() // this is a reminder to put a DO NOT SUBMIT presubmit check in

    vimpl() // OSDCE might be obsolete

    vimpl() // add test for callsite Cons<Cons<int>> rune collision

    vimpl() // make sure that we dont satisfy a function bound from the caller's environment unless using another function bound

    // is something like this possible?
    // and would it mean that we have to detect reentrant evaluation of that lambda template function?
    """
      |lam = (f, z) => {
      |  f(f, z)
      |}
      |lam(lam);
      |""".stripMargin
  }

}
