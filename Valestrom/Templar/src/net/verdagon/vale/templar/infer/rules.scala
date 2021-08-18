package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.RangeS
import net.verdagon.vale.templar.{INameT, IRuneT}
import net.verdagon.vale.{vassert, vcurious, vimpl, vwat}

import scala.collection.immutable.List

// These are different from IRulexA because those use IRuneA, not IRune2 which
// has more possibilities.
// See PVSBUFI
sealed trait IRulexTR {
  def resultType: ITemplataType
  def range: RangeS
}
case class EqualsTR(range: RangeS, left: IRulexTR, right: IRulexTR) extends IRulexTR {
  override def hashCode(): Int = vcurious()

  override def resultType: ITemplataType = left.resultType
}
case class OrTR(range: RangeS, possibilities: Vector[IRulexTR]) extends IRulexTR {
  override def hashCode(): Int = vcurious()

  vassert(possibilities.nonEmpty)
  override def resultType: ITemplataType = possibilities.head.resultType
}
case class ComponentsTR(
  range: RangeS,
  tyype: ITemplataType,
  components: Vector[IRulexTR]
) extends IRulexTR {
  override def hashCode(): Int = vcurious()

  override def resultType: ITemplataType = tyype
}
// This is for built-in parser functions, such as exists() or isBaseOf() etc.
case class CallTR(
  range: RangeS,
  name: String,
  args: Vector[IRulexTR],
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  }

case class IsaTR(
  range: RangeS,
  subRule: IRulexTR,
  interfaceRule: IRulexTR
) extends IRulexTR {
  override def hashCode(): Int = vcurious()

  override def resultType: ITemplataType = subRule.resultType
}

case class IntTT(range: RangeS, value: Long) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = IntegerTemplataType
}
case class StringTT(range: RangeS, value: String) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = StringTemplataType
}
case class BoolTT(range: RangeS, value: Boolean) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = BooleanTemplataType
}
case class MutabilityTT(range: RangeS, mutability: MutabilityP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = MutabilityTemplataType
}
case class PermissionTT(range: RangeS, permission: PermissionP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = PermissionTemplataType
}
case class LocationTT(range: RangeS, location: LocationP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = LocationTemplataType
}
case class OwnershipTT(range: RangeS, ownership: OwnershipP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = OwnershipTemplataType
}
case class VariabilityTT(range: RangeS, variability: VariabilityP) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = VariabilityTemplataType
}

case class NameTT(
  range: RangeS,
  name: IImpreciseNameStepA,
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
//  println("hi")
}

case class AbsoluteNameTT(
  range: RangeS,
  name: INameA,
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
//  println("hi")
}

// We have both NameAT and RuneAT even though theyre syntactically identical
// because in the template engine, when we try to match an incoming type
// against a NameAT/RuneAT, we do different things. For NameAT, we take the thing
// from the environment and make sure it matches. For RuneAT, we might put
// something into the environment.
case class RuneTT(
  range: RangeS,
  rune: IRuneT,
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

// InterpretedTT will overwrite inner's permission and ownership to the given ones.
case class InterpretedTT(
  range: RangeS,
  ownership: OwnershipP,
  permission: PermissionP,
  inner: IRulexTR
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  vassert(inner.resultType == CoordTemplataType)
  override def resultType: ITemplataType = CoordTemplataType
}

case class NullableTT(
  range: RangeS,
  inner: IRulexTR) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = KindTemplataType
}

case class CallTT(
  range: RangeS,
  template: IRulexTR,
  args: Vector[IRulexTR],
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
  name: String,
  parameters: Vector[IRulexTR],
  returnType: IRulexTR
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = vimpl()
}

//case class PackTT(
//  members: Vector[IRulexTR],
//  // This is here because we might want to coerce the result. We do this for
//  // calls, packs, etc.
//  resultType: ITemplataType
//) extends IRulexTR {
// override def hashCode(): Int = vcurious()}

case class RepeaterSequenceTT(
  range: RangeS,
  mutability: IRulexTR,
  variability: IRulexTR,
  size: IRulexTR,
  element: IRulexTR,
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class ManualSequenceTT(
  range: RangeS,
  elements: Vector[IRulexTR],
  // This is here because we might want to coerce the result. We do this for
  // calls, packs, etc.
  resultType: ITemplataType
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
}

case class CoordListTT(
  range: RangeS,
  elements: Vector[IRulexTR]
) extends IRulexTR {
  override def hashCode(): Int = vcurious()
  override def resultType: ITemplataType = PackTemplataType(CoordTemplataType)
}
