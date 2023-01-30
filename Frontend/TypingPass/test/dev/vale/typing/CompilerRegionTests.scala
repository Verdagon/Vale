package dev.vale.typing

import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.infer.{KindIsNotConcrete, OwnershipDidntMatch}
import dev.vale.{CodeLocationS, Collector, Err, FileCoordinateMap, Ok, PackageCoordinate, RangeS, Tests, vassert, vassertOne, vpass, vwat, _}
import dev.vale.parsing.ParseErrorHumanizer
import dev.vale.postparsing.PostParser
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.highertyping.{FunctionA, HigherTypingCompilation}
import dev.vale.solver.RuleError
import OverloadResolver.{FindFunctionFailure, InferFailure, SpecificParamDoesntSend, WrongNumberOfArguments}
import dev.vale.Collector.ProgramWithExpect
import dev.vale.postparsing._
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.solver.{FailedSolve, RuleError, Step}
import dev.vale.typing.ast.{ConstantIntTE, DestroyTE, DiscardTE, FunctionCallTE, FunctionDefinitionT, FunctionHeaderT, KindExportT, LetAndLendTE, LetNormalTE, LocalLookupTE, ParameterT, PrototypeT, ReferenceMemberLookupTE, ReturnTE, SignatureT, SoftLoadTE, UserFunctionT, referenceExprResultKind, referenceExprResultStructName}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, CitizenNameT, CitizenTemplateNameT, CodeVarNameT, DenizenDefaultRegionNameT, FunctionNameT, FunctionTemplateNameT, IdT, InterfaceNameT, InterfaceTemplateNameT, KindPlaceholderNameT, KindPlaceholderTemplateNameT, NonKindPlaceholderNameT, RawArrayNameT, RuntimeSizedArrayNameT, RuntimeSizedArrayTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.typing.ast._
//import dev.vale.typingpass.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers, _}

import scala.collection.immutable.List
import scala.io.Source

class CompilerRegionTests extends FunSuite with Matchers {
  def readCodeFromResource(resourceFilename: String): String = {
    val is =
      Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }

  test("Test caller and callee regions") {
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveStructDrop
        |struct MyStruct { }
        |func myFunc(x &MyStruct) { }
        |exported func main() {
        |  a = MyStruct();
        |  myFunc(&a);
        |  [] = a;
        |}
      """.stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("main")
    Collector.only(main, {
      case FunctionCallTE(
        PrototypeT(
          IdT(_,Vector(),FunctionNameT(FunctionTemplateNameT(StrI("myFunc"),_),Vector(),Vector(CoordT(BorrowT,PlaceholderTemplata(IdT(_,Vector(FunctionTemplateNameT(StrI("main"),_)),KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, DefaultRegionRuneS()))), RegionTemplataType()),StructTT(IdT(_,Vector(),StructNameT(StructTemplateNameT(StrI("MyStruct")),Vector()))))))),
          CoordT(ShareT, PlaceholderTemplata(IdT(_,Vector(FunctionTemplateNameT(StrI("main"),_)),KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, DefaultRegionRuneS()))), RegionTemplataType()),VoidT())),
        Vector(arg)) => {
        arg.result.coord.region match {
          case PlaceholderTemplata(IdT(_,Vector(FunctionTemplateNameT(StrI("main"),_)),KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, DefaultRegionRuneS()))), RegionTemplataType()) =>
        }
      }
    })

    val myFunc = coutputs.lookupFunction("myFunc")
    myFunc.header.params.head.tyype.region match {
      case PlaceholderTemplata(IdT(_,Vector(FunctionTemplateNameT(StrI("myFunc"),_)),KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, DefaultRegionRuneS()))), RegionTemplataType()) =>
    }
  }

  test("Test region'd type") {
    val compile = CompilerTestCompilation.test(
      """
        |func CellularAutomata<r' imm>(rand_seed r'int) { }
      """.stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("main")
  }

  test("Test deeply region'd type") {
    val compile = CompilerTestCompilation.test(
      """
        |func CellularAutomata<r' imm>(rand_seed &r'[]int) { }
      """.stripMargin)

    object regionR {
      def unapply(templata: ITemplata[ITemplataType]): Option[Unit] = {
        templata match {
          case PlaceholderTemplata(
            IdT(_,
              Vector(FunctionTemplateNameT(StrI("CellularAutomata"),_)),
              NonKindPlaceholderNameT(0,CodeRuneS(StrI("r")))),
            RegionTemplataType()) => Some(Unit)
          case _ => None
        }
      }
    }

    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("CellularAutomata")
    main.header.params.head.tyype match {
      case CoordT(
        BorrowT,
        regionR(_),
        RuntimeSizedArrayTT(
          IdT(_,
            Vector(),
            RuntimeSizedArrayNameT(
              RuntimeSizedArrayTemplateNameT(),
              RawArrayNameT(
                MutabilityTemplata(MutableT),
                CoordT(ShareT, regionR(_), IntT(32)),
              regionR(_)))))) =>
    }
  }

  test("Function with two regions") {
    val compile = CompilerTestCompilation.test(
      """
        |func CellularAutomata<r' imm, x'>(incoming_int r'int) { }
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val main = coutputs.lookupFunction("CellularAutomata")
  }

  test("Function with region param still has a default region") {
    val compile = CompilerTestCompilation.test(
      """
        |func CellularAutomata<r' imm>(incoming_int r'int) {
        |  a = 7;
        |}
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val func = coutputs.lookupFunction("CellularAutomata")
    val intType = Collector.only(func, { case LetNormalTE(ReferenceLocalVariableT(IdT(_, _, CodeVarNameT(StrI("a"))), _, c), _) => c })
    intType.region match {
      case PlaceholderTemplata(
        IdT(
          _,
          Vector(FunctionTemplateNameT(StrI("CellularAutomata"),_)),
          NonKindPlaceholderNameT(1,DefaultRegionRuneS())),RegionTemplataType()) =>
    }
  }

  test("Explicit default region") {
    val compile = CompilerTestCompilation.test(
      """
        |func CellularAutomata<r' imm, x'>(incoming_int r'int) x'{
        |  a = 7;
        |}
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val func = coutputs.lookupFunction("CellularAutomata")
    val intType = Collector.only(func, { case LetNormalTE(ReferenceLocalVariableT(IdT(_, _, CodeVarNameT(StrI("a"))), _, c), _) => c })
    intType.region match {
      case PlaceholderTemplata(
        IdT(
          _,
          Vector(FunctionTemplateNameT(StrI("CellularAutomata"),_)),
          NonKindPlaceholderNameT(1,CodeRuneS(StrI("x")))),
        RegionTemplataType()) =>
    }
  }

  test("Call function with callee param explicit region") {
    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.runtime_sized_array_mut_new.*;
        |
        |func Display<r' imm>(map &r'[][]bool) { }
        |
        |exported func main() {
        |  board_0 = [][]bool(20);
        |  Display(&board_0);
        |  [] = board_0;
        |}
        |
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
    val func = coutputs.lookupFunction("main")
  }
}
