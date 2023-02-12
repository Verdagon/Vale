package dev.vale

import dev.vale.simplifying.VonHammer
import dev.vale.finalast.YonderH
import dev.vale.typing._
import dev.vale.typing.types.{CoordT, ImmutableBorrowT, StrT, StructTT}
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
    val compile =
      RunCompilation.test(
        """
          |struct Engine { fuel int; }
          |struct Spaceship { engine Engine; }
          |exported func main() int {
          |  s = Spaceship(Engine(10));
          |  pure block { s.engine.fuel }
          |}
          |""".stripMargin, false)
    val main = compile.getMonouts().lookupFunction("main")
    val mainEngineCoord =
      Collector.only(main, {
        case ReferenceMemberLookupTE(_, _, IdT(_, _, CodeVarNameT(StrI("engine"))), coord, _) => coord
      })
    mainEngineCoord match {
      case CoordT(ImmutableBorrowT,RegionTemplata(false),StructTT(IdT(_,_,StructNameT(StructTemplateNameT(StrI("Engine")),Vector(RegionTemplata(true)))))) =>
    }

    compile.evalForKind(Vector()) match { case VonInt(10) => }
  }
}
