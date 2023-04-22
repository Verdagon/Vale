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
import dev.vale.von.{VonBool, VonInt}
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
      case CoordI(OwnI,StructIT(IdI(_,_,StructNameI(StructTemplateNameI(StrI("Engine")),Vector(RegionTemplataI(0)))))) =>
    }

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _, _) => coord
      })
    xType match {
      // The ImmutableBorrowI is the important part here
      case CoordI(ImmutableBorrowI,StructIT(IdI(_,_,StructNameI(StructTemplateNameI(StrI("Engine")),Vector(RegionTemplataI(0)))))) =>
    }

    val yType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("y")), _, coord), _, _) => coord
      })
    yType match {
      case CoordI(MutableShareI,IntIT(32)) =>
    }

    // We don't evaluate the program, its main takes in a struct which is impossible
    compile.getHamuts()
  }

  test("Pure block accessing arrays") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |exported func main() int {
          |  s = [#]([#](10, 20), [#](30, 40));
          |  res =
          |    pure block {
          |      x = s[0];
          |      y = x[0];
          |      y
          |    };
          |  [[a1, a2], [a3, a4]] = s;
          |  res
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")
    val ssal =
      vassertSome(
        Collector.all(main, {
          case ssal @ StaticSizedArrayLookupIE(_, _, _, _, _) => ssal
        }).headOption)
    ssal.elementType match {
      // See RMLRMO for why this is OwnI
      case CoordI(OwnI,StaticSizedArrayIT(IdI(_,_,StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(),2,FinalI,RawArrayNameI(MutableI,CoordI(MutableShareI,IntIT(32)),RegionTemplataI(0)))))) =>
    }

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _, _) => coord
      })
    xType match {
      case CoordI(ImmutableBorrowI,StaticSizedArrayIT(IdI(_,_,_))) =>
    }

    val yType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("y")), _, coord), _, _) => coord
      })
    yType match {
      case CoordI(MutableShareI,IntIT(32)) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

  test("Pure block returning an array") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |exported func main() int {
          |  x = pure block { [#]([#](10, 20), [#](30, 40)) };
          |  [[a1, a2], [a3, a4]] = x;
          |  a1
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _, _) => coord
      })
    xType match {
      case CoordI(OwnI,StaticSizedArrayIT(IdI(_,_,StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(),2,FinalI,RawArrayNameI(MutableI,CoordI(OwnI,StaticSizedArrayIT(IdI(_,_,StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(),2,FinalI,RawArrayNameI(MutableI,CoordI(MutableShareI,IntIT(32)),RegionTemplataI(0)))))),RegionTemplataI(0)))))) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

  test("Pure function returning a static sized array") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |pure func makeArr() [#2][#2]int {
          |  return [#]([#](10, 20), [#](30, 40));
          |}
          |exported func main() int {
          |  x = makeArr();
          |  [[a1, a2], [a3, a4]] = x;
          |  a1
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _, _) => coord
      })
    xType match {
      case CoordI(OwnI,StaticSizedArrayIT(IdI(_,_,StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(),2,FinalI,RawArrayNameI(MutableI,CoordI(OwnI,StaticSizedArrayIT(IdI(_,_,StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(),2,FinalI,RawArrayNameI(MutableI,CoordI(MutableShareI,IntIT(32)),RegionTemplataI(0)))))),RegionTemplataI(0)))))) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

  test("Pure function taking in a static sized array") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |pure func Display<r'>(arr &r'[#2][#2]int) {
          |  // do nothing
          |}
          |exported func main() int {
          |  x = [#]([#](10, 20), [#](30, 40));
          |  Display(&x);
          |  [[a1, a2], [a3, a4]] = x;
          |  a1
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    val callArg =
      Collector.only(main, {
        case FunctionCallIE(calleePrototype, Vector(arg), _) => arg.result
      })

    val display = compile.getMonouts().lookupFunction("Display")
    display.header.id.localName match {
      case FunctionNameIX(
        FunctionTemplateNameI(StrI("Display"),_),
        Vector(RegionTemplataI(-1), RegionTemplataI(0)),
        Vector(_)) =>
    }
    val funcParam = display.header.id.localName.parameters.head
    funcParam match {
      case CoordI(
        ImmutableBorrowI,
        StaticSizedArrayIT(
          IdI(_,_,
            StaticSizedArrayNameI(
              StaticSizedArrayTemplateNameI(),2,FinalI,
              RawArrayNameI(
                MutableI,
                CoordI(
                  OwnI,
                  StaticSizedArrayIT(
                    IdI(_,_,
                      StaticSizedArrayNameI(
                        StaticSizedArrayTemplateNameI(),2,FinalI,
                        RawArrayNameI(
                          MutableI,
                          CoordI(MutableShareI,IntIT(32)),
                          RegionTemplataI(0)))))),
                RegionTemplataI(0)))))) =>
    }

    vassert(callArg == funcParam)

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

  test("Pure function returning a runtime sized array") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |import v.builtins.runtime_sized_array_mut_new.*;
          |pure func makeArr() [][]int {
          |  return [][]int(0);
          |}
          |exported func main() int {
          |  x = makeArr();
          |  [] = x;
          |  42
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    val xType =
      Collector.only(main, {
        case LetNormalIE(ReferenceLocalVariableI(CodeVarNameI(StrI("x")), _, coord), _, _) => coord
      })
    xType match {
      case CoordI(OwnI,RuntimeSizedArrayIT(IdI(_,_,RuntimeSizedArrayNameI(_,RawArrayNameI(MutableI,CoordI(OwnI,RuntimeSizedArrayIT(IdI(_,_,RuntimeSizedArrayNameI(RuntimeSizedArrayTemplateNameI(),RawArrayNameI(MutableI,CoordI(MutableShareI,IntIT(32)),RegionTemplataI(0)))))),RegionTemplataI(0)))))) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(42) => }
  }

  test("Pure function returning struct") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |struct Engine { fuel int; }
          |struct Spaceship { engine Engine; }
          |pure func makeSpaceship() Spaceship {
          |  Spaceship(Engine(10))
          |}
          |exported func main() int {
          |  s = makeSpaceship();
          |  pure block {
          |    x = s.engine;
          |    y = x.fuel;
          |    y
          |  }
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }


  test("Pure function with immediate value") {
    val compile =
      RunCompilation.test(
        """
          |pure func pureFunc<r'>(s r'str) bool {
          |  true
          |}
          |exported func main() bool {
          |  pureFunc("abc")
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    compile.evalForKind(Vector()) match { case VonBool(true) => }
  }

  test("Pure function reading from local") {
    val compile =
      RunCompilation.test(
        """
          |pure func pureFunc<r'>(s r'str) bool {
          |  true
          |}
          |exported func main() bool {
          |  s = "abc";
          |  return pureFunc(s);
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    compile.evalForKind(Vector()) match {
      case VonBool(true) =>
    }
  }

  test("Readonly function call inside pure block") {
    val compile =
      RunCompilation.test(
        """
          |func rofunc<r'>(s r'str) bool {
          |  true
          |}
          |exported func main() bool {
          |  s = "abc";
          |  return pure block { rofunc(s) };
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    compile.evalForKind(Vector()) match {
      case VonBool(true) =>
    }
  }

  test("Extern function with different regions") {
    val compile =
      RunCompilation.test(
        """import v.builtins.streq.*;
          |
          |pure func pureFunc<r'>(s r'str) bool {
          |  streq(s, 0, 3, "def", 0, 3)
          |}
          |
          |exported func main() bool {
          |  s = "abc";
          |  pureFunc(s)
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")

    compile.evalForKind(Vector()) match { case VonBool(false) => }
  }


}
