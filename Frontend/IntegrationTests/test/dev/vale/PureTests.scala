package dev.vale

import dev.vale.simplifying.VonHammer
import dev.vale.finalast.YonderH
import dev.vale.instantiating.ast._
import dev.vale.typing._
import dev.vale.typing.types._
import dev.vale.testvm.StructInstanceV
import dev.vale.typing.ast.{LetNormalTE, LocalLookupTE, ReferenceMemberLookupTE, StaticSizedArrayLookupTE}
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.names.{CodeVarNameT, IdT, RawArrayNameT, RuntimeSizedArrayNameT, RuntimeSizedArrayTemplateNameT, StaticSizedArrayNameT, StaticSizedArrayTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata._
import dev.vale.von.VonInt
import dev.vale.{finalast => m}
import org.scalatest.{FunSuite, Matchers}

class PureTests extends FunSuite with Matchers {
  test("Simple pure block") {
    // Taking in a &Spaceship so we don't call the constructors, that's covered by another test.

    val compile =
      RunCompilation.test(
        """
          |struct Engine { fuel int; }
          |struct Spaceship { engine Engine; }
          |exported func main(s &Spaceship) int {
          |  pure block {
          |    x = s.engine;
          |    y = x.fuel;
          |    y
          |  }
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")
    val rml =
      Collector.only(main, {
        case rml @ ReferenceMemberLookupIE(_, _, CodeVarNameI(StrI("engine")), _, _) => rml
      })
    rml.memberReference match {
      // See RMLRMO for why this is OwnI
      case CoordI(OwnI,StructIT(IdI(_,_,StructNameI(StructTemplateNameI(StrI("Engine")),Vector(RegionTemplataI(7777)))))) =>
    }

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _) => coord
      })
    xType match {
      case CoordI(null,StructIT(IdI(_,_,StructNameI(StructTemplateNameI(StrI("Engine")),Vector(RegionTemplataI(7777)))))) =>
    }

    val yType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("y")), _, coord), _) => coord
      })
    yType match {
      case CoordI(MutableShareI,IntIT(32)) =>
    }

    // We don't evaluate the program, its main takes in a struct which is impossible
    compile.getHamuts()
  }

//  test("Pure block accessing arrays") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |exported func main() int {
//          |  s = [#]([#](10, 20), [#](30, 40));
//          |  res =
//          |    pure block {
//          |      x = s[0];
//          |      y = x[0];
//          |      y
//          |    };
//          |  [[a1, a2], [a3, a4]] = s;
//          |  res
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//    val ssal =
//      vassertSome(
//        Collector.all(main, {
//          case ssal @ StaticSizedArrayLookupTE(_, _, _, _, _) => ssal
//        }).headOption)
//    ssal.elementType match {
//      // See RMLRMO for why this is OwnI
//      case CoordI(OwnI,StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(_,IntegerTemplataT(2),_,RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))) =>
//    }
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(BorrowT,StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(_,IntegerTemplataT(2),_,RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))) =>
//    }
//
//    val yType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("y")), _, coord), _) => coord
//      })
//    yType match {
//      case CoordI(ShareT,IntT(32)) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(10) => }
//  }
//
//  test("Pure block returning an array") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |exported func main() int {
//          |  x = pure block { [#]([#](10, 20), [#](30, 40)) };
//          |  [[a1, a2], [a3, a4]] = x;
//          |  a1
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))),RegionTemplataI(true)))))) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(10) => }
//  }
//
//  test("Pure function returning a static sized array") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |pure func makeArr() [#2][#2]int {
//          |  return [#]([#](10, 20), [#](30, 40));
//          |}
//          |exported func main() int {
//          |  x = makeArr();
//          |  [[a1, a2], [a3, a4]] = x;
//          |  a1
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))),RegionTemplataI(true)))))) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(10) => }
//  }
//
//  test("Pure function taking in a static sized array") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |pure func Display<r'>(arr &r'[#2][#2]int) {
//          |  // do nothing
//          |}
//          |exported func main() int {
//          |  x = [#]([#](10, 20), [#](30, 40));
//          |  Display(&x);
//          |  [[a1, a2], [a3, a4]] = x;
//          |  a1
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(OwnI,RegionTemplataI(true),StaticSizedArrayTT(IdT(_,_,StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(),IntegerTemplataT(2),VariabilityTemplataT(FinalT),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))),RegionTemplataI(true)))))) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(10) => }
//  }
//
//  test("Pure function returning a runtime sized array") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |import v.builtins.runtime_sized_array_mut_new.*;
//          |pure func makeArr() [][]int {
//          |  return [][]int(0);
//          |}
//          |exported func main() int {
//          |  x = makeArr();
//          |  [] = x;
//          |  42
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(OwnI,RegionTemplataI(true),RuntimeSizedArrayTT(IdT(_,_,RuntimeSizedArrayNameT(RuntimeSizedArrayTemplateNameT(),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(OwnI,RegionTemplataI(true),RuntimeSizedArrayTT(IdT(_,Vector(),RuntimeSizedArrayNameT(RuntimeSizedArrayTemplateNameT(),RawArrayNameT(MutabilityTemplataT(MutableT),CoordI(ShareT,RegionTemplataI(true),IntT(32)),RegionTemplataI(true)))))),RegionTemplataI(true)))))) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(42) => }
//  }
//
//  test("Pure function returning struct") {
//    // In other words, calling a constructor. All the default constructors are pure functions.
//
//    val compile =
//      RunCompilation.test(
//        """
//          |struct Engine { fuel int; }
//          |struct Spaceship { engine Engine; }
//          |pure func makeSpaceship() Spaceship {
//          |  Spaceship(Engine(10))
//          |}
//          |exported func main() int {
//          |  s = makeSpaceship();
//          |  pure block {
//          |    x = s.engine;
//          |    y = x.fuel;
//          |    y
//          |  }
//          |}
//          |""".stripMargin, false)
//    val main = compile.getMonouts().lookupFunction("main")
//    val rml =
//      Collector.only(main, {
//        case rml @ ReferenceMemberLookupTE(_, _, CodeVarNameT(StrI("engine")), _, _) => rml
//      })
//    rml.memberReference match {
//      // See RMLRMO for why this is OwnI
//      case CoordI(OwnI,StructIT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplataI(true)))))) =>
//    }
//
//    val xType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
//      })
//    xType match {
//      case CoordI(ImmutableBorrowI,StructIT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplataI(true)))))) =>
//    }
//
//    val yType =
//      Collector.only(main, {
//        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("y")), _, coord), _) => coord
//      })
//    yType match {
//      case CoordI(ImmutableShareI,IntT(32)) =>
//    }
//
//    compile.evalForKind(Vector()) match { case VonInt(10) => }
//  }
}
