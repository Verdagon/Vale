package dev.vale.typing

import dev.vale._
import OverloadResolver.FindFunctionFailure
import dev.vale.postparsing.CodeNameS
import dev.vale.typing.ast.RestackifyTE
import dev.vale.typing.env.ReferenceLocalVariableT
import dev.vale.typing.names.CodeVarNameT
import dev.vale.vassert
import dev.vale.typing.templata._
import dev.vale.typing.types._
import org.scalatest.funsuite._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.immutable.List
import scala.io.Source

class CompilerGenericsTests extends AnyFunSuite with Matchers {
  // TODO: pull all of the typingpass specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }


  test("Upcasting with generic bounds") {

    val compile = CompilerTestCompilation.test(
      """
        |import v.builtins.panic.*;
        |import v.builtins.drop.*;
        |
        |#!DeriveInterfaceDrop
        |sealed interface XOpt<T Ref> where func drop(T)void {
        |  func harvest(virtual opt XOpt<T>) &T;
        |}
        |
        |#!DeriveStructDrop
        |struct XNone<T Ref> where func drop(T)void  { }
        |
        |impl<T> XOpt<T> for XNone<T>;
        |
        |func harvest<T>(opt XNone<T>) &T {
        |  __vbi_panic();
        |}
        |
        |exported func main() int {
        |  m XOpt<int> = XNone<int>();
        |  return (m).harvest();
        |}
        |
        |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
  }

  test("Lambda registering instantiation bounds with root denizen's placeholders") {
    // We had a assertion that was a bit too strict.
    // When we were compiling inside clone's lambda's drop method,
    //   clone<$clone.E>(&Box<$clone.E>).lam.drop<>(^clone<$clone.E>(&Box<$clone.E>).lam)
    // we were registering some instantiation bounds for the Box<$clone.E> struct.
    // However, it didn't like that we were inside clone.lam.drop but we were seeing a placeholder for clone.
    // It didn't like that we were seeing another function's placeholder.
    // However, that's just how lambdas work with their parents' placeholders, see LAGT.
    val compile = CompilerTestCompilation.test(
      """
        |#!DeriveStructDrop
        |struct Box<E> { x E; }
        |
        |func bork<F>(x &F) where func(&F)void { }
        |
        |func clone<E>(list &Box<E>) where func zork(&E)void {
        |  bork(&{ list.x.zork(); });
        |}
      """.stripMargin)

    val coutputs = compile.expectCompilerOutputs()
  }

  // DO NOT SUBMIT infinite loop
//  test("Mutual structs") {
//    val compile = CompilerTestCompilation.test(
//      """
//        |struct DropBox<T> where func drop(T)void { x T; }
//        |struct A<T> where func drop(T)void, func drop(B<T>)void { y DropBox<B<T>>; }
//        |struct B<T> where func drop(T)void, func drop(A<T>)void { y DropBox<A<T>>; }
//        |""".stripMargin)
//
//    val coutputs = compile.expectCompilerOutputs()
//  }

  test("2x nested structs with bounds") {
    // Here, a struct is guaranteed to ask another struct for some information before it's done compiling.
    // It's guaranteed because they depend on each other (which might be invalid someday, but until then it's good for
    // testing).
    //
    // When we're compiling a struct's members, we need to check instantiation bounds.
    // For example, if we contain a DropBox<A<T>>, we need to make sure that A<T> satisfies DropBox's bounds.
    // Of course, to know DropBox's bounds, we need to have already seen it to gather its bounds.
    // This implies that we need some sort of pre-processing stage where we gather the bounds.
    //
    // DO NOT SUBMIT talk about this in a
    // # Preprocessing Phase In Typing Phase (PPPITP)
    // centralize docs on preprocessing, and mention that we gather bounds in it, and why.
    val compile = CompilerTestCompilation.test(
      """
        |struct DropBox<T> where func drop(T)void { x T; }
        |struct A<T> where func drop(T)void { y DropBox<T>; }
        |struct B<T> where func drop(T)void { y DropBox<A<T>>; }
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
  }

  test("Struct depending on another struct with bounds DO NOT SUBMIT") {
    // We had a bug where we weren't creating a struct's after-header env correctly (we weren't adding the bounds to it)
    // so when we tried to resolve this DropBox<T> it was saying we didn't have the functions to satisfy DropBox<T>'s
    // bounds.
    val compile = CompilerTestCompilation.test(
      """
        |struct DropBox<T> where func drop(T)void { x T; }
        |struct A<T> where func drop(T)void { y DropBox<T>; }
        |""".stripMargin)

    val coutputs = compile.expectCompilerOutputs()
  }


  test("Recursive call") {
    vimpl() // The below causes a stack overflow

    val compile = CompilerTestCompilation.test(
      """
        |func clone<E>(obj &E) E where func clone(&E)E {
        |  return { clone(_) }(obj);
        |}
        |
  """.stripMargin)

    val coutputs = compile.expectCompilerOutputs()
  }

}
