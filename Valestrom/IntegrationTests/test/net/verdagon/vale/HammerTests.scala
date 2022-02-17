package net.verdagon.vale

import net.verdagon.vale.driver.FullCompilationOptions
import net.verdagon.vale.hammer._
import net.verdagon.vale.metal.{BlockH, CallH, ConsecutorH, ConstantIntH, Final, FullNameH, InlineH, IntH, Local, NeverH, PrototypeH, ReadonlyH, ReferenceH, ShareH, StackifyH, VariableIdH, VoidH}
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.templar.types.ShareT
import net.verdagon.vale.vivem.PanicException
import org.scalatest.{FunSuite, Matchers}
import net.verdagon.von.VonInt

import scala.collection.immutable.List

class HammerTests extends FunSuite with Matchers {
  // Hammer tests only test the general structure of things, not the generated nodes.
  // The generated nodes will be tested by end-to-end tests.

  test("Simple main") {
    val compile = RunCompilation.test(
      "exported func main() int { ret 3; }")
    val hamuts = compile.getHamuts()

    val testPackage = hamuts.lookupPackage(PackageCoordinate.TEST_TLD)
    vassert(testPackage.getAllUserFunctions.size == 1)
    testPackage.getAllUserFunctions.head.prototype.fullName.toFullString() shouldEqual """test::F("main")"""
  }

//  // Make sure a ListNode struct made it out
//  test("Templated struct makes it into hamuts") {
//    val compile = RunCompilation.test(
//      """
//        |struct ListNode<T> imm where T: Ref {
//        |  tail: *ListNode<T>;
//        |}
//        |func main(a: *ListNode:int) {}
//      """.stripMargin)
//    val hamuts = compile.getHamuts()
//    hamuts.structs.find(_.fullName.parts.last.humanName == "ListNode").get;
//  }

  test("Two templated structs make it into hamuts") {
    val compile = RunCompilation.test(
      """
        |interface MyOption<T> imm where T Ref { }
        |struct MyNone<T> imm where T Ref { }
        |impl<T> MyOption<T> for MyNone<T>;
        |struct MySome<T> imm where T Ref { value T; }
        |impl<T> MyOption<T> for MySome<T>;
        |
        |func main(a @MySome<int>, b @MyNone<int>) {}
      """.stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    packageH.interfaces.find(interface => {
      interface.fullName.toFullString() == """test::C(CT("MyOption"),[TR(R(@,<,#,i(32)))])"""
    }).get;

    val mySome = packageH.structs.find(_.fullName.toFullString() == """test::C(CT("MySome"),[TR(R(@,<,#,i(32)))])""").get;
    vassert(mySome.members.size == 1);
    vassert(mySome.members.head.tyype == ReferenceH[IntH](m.ShareH, InlineH, ReadonlyH, IntH.i32))

    val myNone = packageH.structs.find(_.fullName.toFullString() == """test::C(CT("MyNone"),[TR(R(@,<,#,i(32)))])""").get;
    vassert(myNone.members.isEmpty);
  }

  // Known failure 2020-08-20
  // Maybe we can turn off tree shaking?
  // Maybe this just violates requirements?
//  test("Virtual and override functions make it into hamuts") {
//    val compile = RunCompilation.test(
//      """
//        |interface Blark imm { }
//        |func wot(virtual b *Blark) int abstract;
//        |struct MyStruct export imm {}
//        |impl Blark for MyStruct;
//        |func wot(b *MyStruct impl Blark) int { ret 9; }
//      """.stripMargin)
//    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
//    packageH.nonExternFunctions.find(f => f.prototype.fullName.toFullString().startsWith("""F("wot"""")).get;
//    packageH.nonExternFunctions.find(f => f.prototype.fullName.toFullString() == """F("MyStruct")""").get;
//    vassert(packageH.abstractFunctions.size == 2)
//    vassert(packageH.getAllUserImplementedFunctions.size == 1)
//    vassert(packageH.getAllUserFunctions.size == 1)
//  }

  test("Tests stripping things after panic") {
    val compile = RunCompilation.test(
      """
        |exported func main() int {
        |  __vbi_panic();
        |  a = 42;
        |  ret a;
        |}
      """.stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    val main = packageH.lookupFunction("main")
    main.body match {
      case BlockH(CallH(PrototypeH(fullNameH, Vector(), ReferenceH(_, _, ReadonlyH, NeverH(_))), Vector())) => {
        vassert(fullNameH.toFullString().contains("__vbi_panic"))
      }
    }
  }

  test("panic in expr") {
    val compile = RunCompilation.test(
      """
        |import intrange.*;
        |
        |exported func main() int {
        |  ret 3 + __vbi_panic();
        |}
        |""".stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    val main = packageH.lookupFunction("main")
    val intExpr =
      main.body match {
        case BlockH(
          ConsecutorH(Vector(
            intExpr,
            CallH(
              PrototypeH(_,Vector(),ReferenceH(_,_,ReadonlyH,NeverH(_))),
              Vector())))) => {
          intExpr
        }
      }
    Collector.only(intExpr, {
      case ConstantIntH(3, 32) =>
    })
  }

  test("Tests export function") {
    val compile = RunCompilation.test(
      """
        |exported func moo() int { ret 42; }
        |""".stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    val moo = packageH.lookupFunction("moo")
    packageH.exportNameToFunction("moo") shouldEqual moo.prototype
  }

  test("Tests export struct") {
    val compile = RunCompilation.test(
      """
        |exported struct Moo { }
        |""".stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    val moo = packageH.lookupStruct("Moo")
    packageH.exportNameToKind("Moo") shouldEqual moo.getRef
  }

  test("Tests export interface") {
    val compile = RunCompilation.test(
      """
        |exported interface Moo { }
        |""".stripMargin)
    val packageH = compile.getHamuts().lookupPackage(PackageCoordinate.TEST_TLD)
    val moo = packageH.lookupInterface("Moo")
    packageH.exportNameToKind("Moo") shouldEqual moo.getRef
  }

  test("Tests exports from two modules, different names") {
    val compile =
      new RunCompilation(
        Vector(PackageCoordinate.BUILTIN, PackageCoordinate("moduleA", Vector.empty), PackageCoordinate("moduleB", Vector.empty)),
        Builtins.getCodeMap()
          .or(
            FileCoordinateMap(Map())
              .add("moduleA", Vector.empty, "StructA.vale", "exported struct StructA { a int; }")
              .add("moduleB", Vector.empty, "StructB.vale", "exported struct StructB { a int; }"))
          .or(Tests.getPackageToResourceResolver),
        FullCompilationOptions())
    val hamuts = compile.getHamuts()

    val packageA = hamuts.lookupPackage(PackageCoordinate("moduleA", Vector.empty))
    val fullNameA = vassertSome(packageA.exportNameToKind.get("StructA"))

    val packageB = hamuts.lookupPackage(PackageCoordinate("moduleB", Vector.empty))
    val fullNameB = vassertSome(packageB.exportNameToKind.get("StructB"))

    vassert(fullNameA != fullNameB)
  }

  // Intentional known failure, need to separate things internally inside Hammer
//  test("Tests exports from two modules, same name") {
//    val compile =
//      new RunCompilation(
//        Vector(PackageCoordinate.BUILTIN, PackageCoordinate("moduleA", Vector.empty), PackageCoordinate("moduleB", Vector.empty)),
//        Builtins.getCodeMap()
//          .or(
//            FileCoordinateMap(Map())
//              .add("moduleA", Vector.empty, "MyStruct.vale", "struct MyStruct export { a int; }")
//              .add("moduleB", Vector.empty, "MyStruct.vale", "struct MyStruct export { a int; }"))
//          .or(Tests.getPackageToResourceResolver),
//        FullCompilationOptions())
//    val hamuts = compile.getHamuts()
//
//    val packageA = hamuts.lookupPackage(PackageCoordinate("moduleA", Vector.empty))
//    val fullNameA = vassertSome(packageA.exportNameToKind.get("StructA"))
//
//    val packageB = hamuts.lookupPackage(PackageCoordinate("moduleB", Vector.empty))
//    val fullNameB = vassertSome(packageB.exportNameToKind.get("StructA"))
//
//    vassert(fullNameA != fullNameB)
//  }
}
