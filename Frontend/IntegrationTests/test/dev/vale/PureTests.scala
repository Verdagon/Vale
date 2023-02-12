package dev.vale

import dev.vale.simplifying.VonHammer
import dev.vale.finalast.YonderH
import dev.vale.typing._
import dev.vale.typing.types.{CoordT, ImmutableBorrowT, ImmutableShareT, IntT, OwnT, StrT, StructTT}
import dev.vale.testvm.StructInstanceV
import dev.vale.typing.ast.{LetNormalTE, LocalLookupTE, ReferenceMemberLookupTE}
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.names.{CodeVarNameT, IdT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.RegionTemplata
import dev.vale.von.VonInt
import dev.vale.{finalast => m}
import org.scalatest.{FunSuite, Matchers}

class PureTests extends FunSuite with Matchers {
  test("Simple pure function") {
    val compile =
      RunCompilation.test(
        """
          |struct Engine {
          |  fuel int;
          |}
          |struct Spaceship {
          |  engine Engine;
          |}
          |pure func pfunc(s &Spaceship) int {
          |  return s.engine.fuel;
          |}
          |exported func main() int {
          |  s = Spaceship(Engine(10));
          |  return pfunc(&s);
          |}
          |""".stripMargin)
    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }

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
        case rml @ ReferenceMemberLookupTE(_, _, CodeVarNameT(StrI("engine")), _, _) => rml
      })
    rml.memberReference match {
      // See RMLRMO for why this is OwnT
      case CoordT(OwnT,RegionTemplata(false),StructTT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplata(true)))))) =>
    }

    val xType =
      Collector.only(main, {
        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("x")), _, coord), _) => coord
      })
    xType match {
      case CoordT(ImmutableBorrowT,RegionTemplata(false),StructTT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplata(true)))))) =>
    }

    val yType =
      Collector.only(main, {
        case LetNormalTE(ReferenceLocalVariableT(CodeVarNameT(StrI("y")), _, coord), _) => coord
      })
    yType match {
      case CoordT(ImmutableShareT,RegionTemplata(false),IntT(32)) =>
    }

    // We don't evaluate the program, its main takes in a struct which is impossible
  }

  test("Pure function returning struct") {
    // In other words, calling a constructor. All the default constructors are pure functions.

    val compile =
      RunCompilation.test(
        """
          |struct Engine { fuel int; }
          |struct Spaceship { engine Engine; }
          |exported func main() int {
          |  s = Spaceship(Engine(10));
          |  s.engine.fuel
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")
    val mainEngineCoord =
      Collector.only(main, {
        case ReferenceMemberLookupTE(_, _, CodeVarNameT(StrI("engine")), coord, _) => coord
      })
    mainEngineCoord match {
      case CoordT(ImmutableBorrowT,RegionTemplata(false),StructTT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplata(true)))))) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }
}
