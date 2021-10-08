package net.verdagon.vale.astronomer

import net.verdagon.vale.parser.{CaptureP, VariabilityP}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.vale.scout._
import net.verdagon.vale.{vcurious, vimpl}

import scala.collection.immutable.List

object PatternSUtils {
  def getRuneTypesFromPattern(pattern: AtomSP): Iterable[(IRuneS, ITemplataType)] = {
    val runesFromVirtuality =
      pattern.virtuality match {
        case None => Vector.empty
        case Some(AbstractSP) => Vector.empty
        case Some(OverrideSP(range, kindRune)) => Vector((kindRune.rune -> KindTemplataType))
      }
    val runesFromDestructures =
      pattern.destructure.toVector.flatten.flatMap(getRuneTypesFromPattern)
    (runesFromVirtuality ++ runesFromDestructures ++ pattern.coordRune.map(_.rune -> CoordTemplataType)).distinct
  }

//  def getDistinctOrderedRunesForPattern(pattern: AtomSP): Vector[IRuneS] = {
//    val runesFromVirtuality =
//      pattern.virtuality match {
//        case None => Vector.empty
//        case Some(AbstractAP) => Vector.empty
//        case Some(OverrideAP(range, kindRune)) => Vector(kindRune)
//      }
//    val runesFromDestructures =
//      pattern.destructure.toVector.flatten.flatMap(getDistinctOrderedRunesForPattern)
//    (runesFromVirtuality ++ runesFromDestructures :+ pattern.coordRune).distinct
//  }

}