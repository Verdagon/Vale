package net.verdagon.vale.astronomer

import net.verdagon.vale._

object AstronomerTestCompilation {
  def test(code: String*): AstronomerCompilation = {
    new AstronomerCompilation(
      Vector(PackageCoordinate.TEST_TLD),
      FileCoordinateMap.test(code.toVector)
        .or(Tests.getPackageToResourceResolver))
  }
}
