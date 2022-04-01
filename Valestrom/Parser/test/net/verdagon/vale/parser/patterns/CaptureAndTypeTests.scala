package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser.ast.Patterns.{capturedWithType, capturedWithTypeRune}
import net.verdagon.vale.parser._
import net.verdagon.vale.parser.ast._
import net.verdagon.vale.{Collector, vfail}
import org.scalatest.{FunSuite, Matchers}

class CaptureAndTypeTests extends FunSuite with Matchers with Collector with TestParseUtils {
//  private def compile[T](parser: CombinatorParsers.Parser[T], code: String): T = {
//    CombinatorParsers.parse(parser, code.toCharArray()) match {
//      case CombinatorParsers.NoSuccess(msg, input) => {
//        fail();
//      }
//      case CombinatorParsers.Success(expr, rest) => {
//        if (!rest.atEnd) {
//          vfail(rest.pos.longString)
//        }
//        expr
//      }
//    }
//  }
  private def compile[T](code: String): PatternPP = {
    compile(new PatternParser().parsePattern(_), code)
  }

  test("No capture, with type") {
    compile("_ int") shouldHave {
      case PatternPP(_,None, _, Some(NameOrRunePT(NameP(_, "int"))), None, None) =>
    }
  }
  test("Capture with type") {
    compile("a int") shouldHave {
      case capturedWithType("a", NameOrRunePT(NameP(_, "int"))) =>
    }
  }
  test("Simple capture with tame") {
    compile("a T") shouldHave {
      case capturedWithTypeRune("a","T") =>
    }
  }
  test("Capture with borrow tame") {
    compile("arr &R") shouldHave {
      case PatternPP(_,_,
      Some(LocalNameDeclarationP(NameP(_, "arr"))),
      Some(InterpretedPT(_,BorrowP, NameOrRunePT(NameP(_, "R")))),
      None,
      None) =>
    }
  }
  test("Capture with self. in front") {
    compile("self.arr &&R") shouldHave {
      case PatternPP(_,_,
      Some(ConstructingMemberNameDeclarationP(NameP(_, "arr"))),
      Some(InterpretedPT(_,WeakP, NameOrRunePT(NameP(_, "R")))),
      None,
      None) =>
    }
  }
}
