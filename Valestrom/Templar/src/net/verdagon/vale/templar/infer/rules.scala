package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.RangeS
import net.verdagon.vale.templar.{INameT, IRuneT}
import net.verdagon.vale.{vassert, vcurious, vimpl, vwat}

import scala.collection.immutable.List

case class RuneWorld(
  rules: Array[IRulexTR],

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
sealed trait IRulexTR {
  def range: RangeS
}
case class OrTR(range: RangeS, possibilities: Vector[RuneWorld]) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  vassert(possibilities.nonEmpty)
}
case class CoordComponentsTR(
  range: RangeS,
  coordRune: Int,
  ownershipRune: Int,
  permissionRune: Int,
  kindRune: Int,
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class KindComponentsTR(
  range: RangeS,
  kindRune: Int,
  mutabilityRune: Int,
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
// This is for built-in parser functions, such as exists() or isBaseOf() etc.
case class CallTR(
  range: RangeS,
  rune: Int,
  name: String,
  args: Vector[Int],
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class IsaTR(
  range: RangeS,
  subRule: Int,
  interfaceRule: Int
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class IntTT(range: RangeS, rune: Int, value: Long) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class StringTT(range: RangeS, rune: Int, value: String) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class BoolTT(range: RangeS, rune: Int, value: Boolean) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class MutabilityTT(range: RangeS, rune: Int, mutability: MutabilityP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class PermissionTT(range: RangeS, rune: Int, permission: PermissionP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class LocationTT(range: RangeS, rune: Int, location: LocationP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class OwnershipTT(range: RangeS, rune: Int, ownership: OwnershipP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
case class VariabilityTT(range: RangeS, rune: Int, variability: VariabilityP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class NameTT(
  range: RangeS,
  rune: Int,
  name: IImpreciseNameStepA,
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
//  println("hi")
}

case class AbsoluteNameTT(
  range: RangeS,
  rune: Int,
  name: INameA,
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
//  println("hi")
}

// InterpretedTT will overwrite inner's permission and ownership to the given ones.
case class InterpretedTT(
  range: RangeS,
  rune: Int,
  ownership: OwnershipP,
  permission: PermissionP,
  inner: Int
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class NullableTT(
  range: RangeS,
  rune: Int,
  inner: Int) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class CallTT(
  range: RangeS,
  rune: Int,
  template: Int,
  args: Vector[Int],
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

//case class FunctionTT(
//  mutability: Option[IRulexTR],
//  parameters: Vector[Option[IRulexTR]],
//  returnType: Option[IRulexTR]
//) extends IRulexTR {
// override def hashCode(): Int = vcurious()}

case class PrototypeTT(
  range: RangeS,
  rune: Int,
  name: String,
  parameters: Vector[Int],
  returnType: Int
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class RepeaterSequenceTT(
  range: RangeS,
  rune: Int,
  mutability: Int,
  variability: Int,
  size: Int,
  element: Int,
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class ManualSequenceTT(
  range: RangeS,
  rune: Int,
  elements: Vector[Int],
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class CoordListTT(
  range: RangeS,
  rune: Int,
  elements: Vector[Int]
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}
