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

  test("Type basic main function") {
    val compilation =
      AstronomerTestCompilation.test(
      """fn main() export {
        |}
        |""".stripMargin)
    val astrouts = compilation.getAstrouts().getOrDie()
  }
}
