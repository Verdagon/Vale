package net.verdagon.vale.astronomer

import net.verdagon.vale.parser.{CaptureP, VariabilityP}
import net.verdagon.vale.scout.{LocalS, RangeS}
import net.verdagon.vale.{vcurious, vimpl}

import scala.collection.immutable.List

//case class AtomSP(
//  range: RangeS,
//  // This is an Option so Templar can destroy incoming ignored vars immediately, see DIPRA.
//  capture: Option[LocalS],
//  virtuality: Option[VirtualityAP],
//  coordRune: IRuneA,
//  destructure: Option[Vector[AtomSP]]) { override def hashCode(): Int = vcurious() }

//sealed trait VirtualityAP
//case object AbstractAP extends VirtualityAP
//case class OverrideAP(range: RangeS, kindRune: IRuneS) extends VirtualityAP { override def hashCode(): Int = vcurious() }

//object PatternSUtils {
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
//
//}