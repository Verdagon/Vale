package dev.vale.instantiating

import dev.vale.options.GlobalOptions
import dev.vale.{Builtins, FileCoordinateMap, Interner, Keywords, PackageCoordinate, Tests}
import org.scalatest.funsuite._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object InstantiatingCompilation {
  def test(code: String*): InstantiatedCompilation = {
    val interner = new Interner()
    val keywords = new Keywords(interner)
    new InstantiatedCompilation(
      interner,
      keywords,
      Vector(
        PackageCoordinate.TEST_TLD(interner, keywords)),
      Builtins.getCodeMap(interner, keywords)
        .or(FileCoordinateMap.test(interner, code.toVector))
        .or(Tests.getPackageToResourceResolver),
      InstantiatorCompilationOptions(
        GlobalOptions(true, true, true, true, true)))
  }
}

class InstantiatedTests extends AnyFunSuite with Matchers {

  test("Test templates") {
    val compile = InstantiatingCompilation.test(
      """
        |func drop(x int) { }
        |func bork<T>(a T) void where func drop(T)void {
        |  // implicitly calls drop
        |}
        |exported func main() {
        |  bork(3);
        |}
      """.stripMargin)
    compile.getMonouts()
  }

}
