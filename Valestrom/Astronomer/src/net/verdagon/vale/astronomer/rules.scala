package net.verdagon.vale.astronomer

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{CodeLocationS, RangeS}
import net.verdagon.vale.{vassert, vcurious, vimpl, vwat}

import scala.collection.immutable.List

case class RuneWorld(
  rules: Array[IRulexAR],

  // For example, if rule 7 says:
  //   1 = Ref(2, 3, 4, 5)
  // then 2, 3, 4, 5 together could solve the rule, or 1 could solve the rule.
  // In other words, the two sets of runes that could solve the rule are:
  // - [1]
  // - [2, 3, 4, 5]
  // Here we have two "puzzles". The runes in a puzzle are called "pieces".
  // Puzzles are identified up-front by Astronomer.

  puzzleToPiecesAndRule: Array[Array[Int]],
  puzzleToRule: Array[Int],

  pieceToPuzzles: Array[Array[Int]])

// These are different from IRulexA because those use IRuneA, not IRuneT which
// has more possibilities.
// See PVSBUFI
sealed trait IRulexAR {
  def range: RangeS
}
case class OrAR(range: RangeS, possibilities: Vector[RuneWorld]) extends IRulexAR {
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
  rune: Int,
  name: String,
  args: Vector[Int],
  resultType: ITemplataType
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

case class IntAR(range: RangeS, rune: Int, value: Long) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class StringAR(range: RangeS, rune: Int, value: String) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class BoolAR(range: RangeS, rune: Int, value: Boolean) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class MutabilityAR(range: RangeS, rune: Int, mutability: MutabilityP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class PermissionAR(range: RangeS, rune: Int, permission: PermissionP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class LocationAR(range: RangeS, rune: Int, location: LocationP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class OwnershipAR(range: RangeS, rune: Int, ownership: OwnershipP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
case class VariabilityAR(range: RangeS, rune: Int, variability: VariabilityP) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class NameAR(
  range: RangeS,
  rune: Int,
  name: IImpreciseNameStepA,
  resultType: ITemplataType
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
  //  println("hi")
}

case class AbsoluteNameAR(
  range: RangeS,
  rune: Int,
  name: INameA,
  resultType: ITemplataType
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
  //  println("hi")
}

// InterpretedAR will overwrite inner's permission and ownership to the given ones.
case class InterpretedAR(
  range: RangeS,
  rune: Int,
  ownership: OwnershipP,
  permission: PermissionP,
  inner: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class NullableAR(
  range: RangeS,
  rune: Int,
  inner: Int) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class CallAR(
  range: RangeS,
  rune: Int,
  template: Int,
  args: Vector[Int],
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

//case class FunctionAR(
//  mutability: Option[IRulexAR],
//  parameters: Vector[Option[IRulexAR]],
//  returnType: Option[IRulexAR]
//) extends IRulexAR {
// override def hashCode(): Int = vcurious()}

case class PrototypeAR(
  range: RangeS,
  rune: Int,
  name: String,
  parameters: Vector[Int],
  returnType: Int
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class RepeaterSequenceAR(
  range: RangeS,
  rune: Int,
  mutability: Int,
  variability: Int,
  size: Int,
  element: Int,
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class ManualSequenceAR(
  range: RangeS,
  rune: Int,
  elements: Vector[Int],
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}

case class CoordListAR(
  range: RangeS,
  rune: Int,
  elements: Vector[Int]
) extends IRulexAR {
  override def hashCode(): Int = vcurious()
}
