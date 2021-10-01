package net.verdagon.vale.scout.patterns

import net.verdagon.vale.parser.{CaptureP, VariabilityP}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.rules.RuneUsage
import net.verdagon.vale.{vcurious, vimpl}

import scala.collection.immutable.List

case class CaptureS(
  name: IVarNameS) {
  override def hashCode(): Int = vcurious()
}

case class AtomSP(
  range: RangeS,
  // This is an option because in PatternTemplar, if it's None, we'll explode the
  // expression into the destructure or throw the incoming thing away right now (see DIPRA),
  // and if it's Some, we'll make this variable an owning ref.
  // This is a CaptureS instead of a LocalS, which is slightly annoying for Templar, since it has to
  // remember the LocalSs in scope. But it'd be even more difficult for Scout to know the Used/NotUsed
  // etc up-front to include in the pattern.
  name: Option[CaptureS],
  virtuality: Option[VirtualitySP],
  coordRune: Option[RuneUsage],
  destructure: Option[Vector[AtomSP]]) {
  override def hashCode(): Int = vcurious()
}

sealed trait VirtualitySP
case object AbstractSP extends VirtualitySP
case class OverrideSP(range: RangeS, kindRune: RuneUsage) extends VirtualitySP {
  override def hashCode(): Int = vcurious()
}

//object PatternSUtils {
//  def getDistinctOrderedRunesForPattern(pattern: AtomSP): Vector[IRuneS] = {
//    val runesFromVirtuality =
//      pattern.virtuality match {
//        case None => Vector.empty
//        case Some(AbstractSP) => Vector.empty
//        case Some(OverrideSP(range, kindRune)) => Vector(kindRune)
//      }
//    val runesFromDestructures =
//      pattern.destructure.toVector.flatten.flatMap(getDistinctOrderedRunesForPattern)
//    (runesFromVirtuality ++ runesFromDestructures :+ pattern.coordRune.rune).distinct
//  }
//}