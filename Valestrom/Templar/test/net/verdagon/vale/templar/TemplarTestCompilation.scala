package net.verdagon.vale.templar

import net.verdagon.vale._
import net.verdagon.vale.astronomer._
import net.verdagon.vale.options.GlobalOptions
import net.verdagon.vale.parser.FileP

import scala.collection.immutable.{List, ListMap, Map, Set}
import scala.collection.mutable

object TemplarTestCompilation {
  def test(code: String*): TemplarCompilation = {
    new TemplarCompilation(
      GlobalOptions(true, true, true),
      Vector(PackageCoordinate.TEST_TLD),
      Builtins.getModulizedCodeMap()
        .or(FileCoordinateMap.test(code.toVector))
        .or(Tests.getPackageToResourceResolver),
      TemplarCompilationOptions())
  }
}
