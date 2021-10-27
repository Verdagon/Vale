package net.verdagon.vale.astronomer

import net.verdagon.vale._
import net.verdagon.vale.options.GlobalOptions

object AstronomerTestCompilation {
  def test(code: String*): AstronomerCompilation = {
    new AstronomerCompilation(
      new GlobalOptions(true, true, true),
      Vector(PackageCoordinate.TEST_TLD),
      FileCoordinateMap.test(code.toVector)
        .or(Tests.getPackageToResourceResolver))
  }
}
