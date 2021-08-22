package net.verdagon.vale.astronomer

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{CodeLocationS, RangeS}
import net.verdagon.vale.{vassert, vcurious, vimpl, vwat}

import scala.collection.immutable.List

// These are different from IRulexA because those use IRuneA, not IRuneT which
// has more possibilities.
// See PVSBUFI
sealed trait IRulexAR {
  def range: RangeS
}
case class OrAR(range: RangeS, possibilities: Array[RuneWorld]) extends IRulexAR {
  override def hashCode(): Int = vcurious()
  vassert(possibilities.nonEmpty)
}
case class CoordComponentsAR(
  range: RangeS,
  coordRune: Int,
  ownershipRune: Int,
  permissionRune: Int,
  kindRune: Int,
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class KindComponentsAR(
  range: RangeS,
  kindRune: Int,
  mutabilityRune: Int,
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
// This is for built-in parser functions, such as exists() or isBaseOf() etc.
case class BuiltinCallAR(
  range: RangeS,
  resultRune: Int,
  name: String,
  args: Array[Int]
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class IsaAR(
  range: RangeS,
  subRule: Int,
  interfaceRule: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class IntAR(range: RangeS, resultRune: Int, value: Long) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class StringAR(range: RangeS, resultRune: Int, value: String) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class BoolAR(range: RangeS, resultRune: Int, value: Boolean) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class MutabilityAR(range: RangeS, resultRune: Int, mutability: MutabilityP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class PermissionAR(range: RangeS, resultRune: Int, permission: PermissionP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class LocationAR(range: RangeS, resultRune: Int, location: LocationP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class OwnershipAR(range: RangeS, resultRune: Int, ownership: OwnershipP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class VariabilityAR(range: RangeS, resultRune: Int, variability: VariabilityP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class NameAR(
  range: RangeS,
  resultRune: Int,
  name: IImpreciseNameStepA
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
  //  println("hi")
}

case class AbsoluteNameAR(
  range: RangeS,
  resultRune: Int,
  name: INameA
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
  //  println("hi")
}

// InterpretedAR will overwrite inner's permission and ownership to the given ones.
case class InterpretedAR(
  range: RangeS,
  resultRune: Int,
  ownership: OwnershipP,
  permission: PermissionP,
  inner: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

//case class NullableAR(
//  range: RangeS,
//  resultRune: Int,
//  inner: Int) extends IRulexAR {
//  override def hashCode(): Int = vcurious()
//}

case class CallAR(
  range: RangeS,
  resultRune: Int,
  template: Int,
  args: Array[Int]
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

//case class FunctionAR(
//  mutability: Option[IRulexAR],
//  parameters: Array[Option[IRulexAR]],
//  returnType: Option[IRulexAR]
//) extends IRulexAR {
// override def hashCode(): Int = vcurious()}

case class PrototypeAR(
  range: RangeS,
  resultRune: Int,
  name: String,
  parameters: Array[Int],
  returnType: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class RepeaterSequenceAR(
  range: RangeS,
  resultRune: Int,
  mutability: Int,
  variability: Int,
  size: Int,
  element: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class ManualSequenceAR(
  range: RangeS,
  resultRune: Int,
  elements: Array[Int]
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class CoordListAR(
  range: RangeS,
  resultRune: Int,
  elements: Array[Int]
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
