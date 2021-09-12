package net.verdagon.vale.astronomer

import net.verdagon.vale.parser.{FileP, ParseFailure, ParseSuccess, Parser}
import net.verdagon.vale.scout.{ProgramS, Scout}
import net.verdagon.vale._
import org.scalatest.{FunSuite, Matchers}

class AstronomerTests extends FunSuite with Matchers  {
  def compileProgramForError(compilation: AstronomerCompilation): ICompileErrorA = {
    compilation.getAstrouts() match {
      case Ok(result) => vfail("Expected error, but actually parsed invalid program:\n" + result)
      case Err(err) => err
    }
  }

  test("Type simple main function") {
    val compilation =
      AstronomerTestCompilation.test(
      """fn main() export {
        |}
        |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Type simple generic function") {
    val compilation =
      AstronomerTestCompilation.test(
        """fn moo<T>() rules(T Ref) export {
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Infer coord type from parameters") {
    val compilation =
      AstronomerTestCompilation.test(
        """fn moo<T>(x T) export {
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
    val program = vassertSome(astrouts.get(PackageCoordinate.TEST_TLD))
    val main = program.lookupFunction("moo")
    main.typeByRune(CodeRuneA("T")) shouldEqual CoordTemplataType
  }

  test("Type simple struct") {
    val compilation =
      AstronomerTestCompilation.test(
        """struct Moo {
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Type simple generic struct") {
    val compilation =
      AstronomerTestCompilation.test(
        """struct Moo<T> {
          |  bork T;
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Type simple interface") {
    val compilation =
      AstronomerTestCompilation.test(
        """interface Moo {
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Type simple generic interface") {
    val compilation =
      AstronomerTestCompilation.test(
        """interface Moo<T> {
          |  fn bork(x T);
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }

  test("Infer generic type through param type template call") {
    val compilation =
      AstronomerTestCompilation.test(
        """struct List<T> {
          |  moo T;
          |}
          |fn moo<T>(x List<T>) export {
          |}
          |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
    val program = vassertSome(astrouts.get(PackageCoordinate.TEST_TLD))
    val main = program.lookupFunction("moo")
    main.typeByRune(CodeRuneA("T")) shouldEqual CoordTemplataType
  }
}
