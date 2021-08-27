package net.verdagon.vale.scout.rules

import net.verdagon.vale.{vassert, vcurious, vimpl, vpass, vwat}
import net.verdagon.vale.parser.{LocationP, MutabilityP, OwnershipP, PermissionP, VariabilityP}
import net.verdagon.vale.scout._

import scala.collection.immutable.List

// See PVSBUFI
trait IRulexSR {
  def range: RangeS
}

case class EqualsSR(range: RangeS, left: IRuneS, right: IRuneS) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class IsaSR(range: RangeS, sub: IRuneS, suuper: IRuneS) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class KindComponentsSR(
  range: RangeS,
  resultRune: IRuneS,
  mutabilityRune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class CoordComponentsSR(
  range: RangeS,
  resultRune: IRuneS,
  ownershipRune: IRuneS,
  permissionRune: IRuneS,
  kindRune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class OneOfSR(
  range: RangeS,
  rune: IRuneS,
  literals: Array[IValueSR]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  vassert(literals.nonEmpty)
}

case class IsConcreteSR(
  range: RangeS,
  rune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class IsInterfaceSR(
  range: RangeS,
  rune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class IsStructSR(
  range: RangeS,
  rune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class CoerceToCoord(
  range: RangeS,
  coordRune: IRuneS,
  kindRune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class LiteralSR(
  range: RangeS,
  rune: IRuneS,
  literal: IValueSR
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class LookupSR(
  range: RangeS,
  rune: IRuneS,
  name: INameSR
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

// InterpretedAR will overwrite inner's permission and ownership to the given ones.
// We turned InterpretedAR into this
case class AugmentSR(
  range: RangeS,
  resultRune: IRuneS,
  // Lets try and figure out a way to only have one thing here instead of a Vector
  literal: Vector[IValueSR],
  innerRune: IRuneS
) extends IRulexSR {
  vpass()
  override def hashCode(): Int = vcurious()
}

//case class NullableSR(
//  range: RangeS,
//  resultRune: IRuneS,
//  inner: Int) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}

case class CallSR(
  range: RangeS,
  resultRune: IRuneS,
  templateRune: IRuneS,
  args: Array[IRuneS]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

//case class CommonMutabilitySR(
//  range: RangeS,
//  resultRune: IRuneS,
//  args: Array[IRuneS]
//) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}

//case class FunctionSR(
//  mutability: Option[IRulexAR],
//  parameters: Array[Option[IRulexAR]],
//  returnType: Option[IRulexAR]
//) extends IRulexSR {
// override def hashCode(): Int = vcurious()}

case class PrototypeSR(
  range: RangeS,
  resultRune: IRuneS,
  name: String,
  parameters: Array[IRuneS],
  returnTypeRune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class PackSR(
  range: RangeS,
  resultRune: IRuneS,
  members: Array[IRuneS]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class RepeaterSequenceSR(
  range: RangeS,
  resultRune: IRuneS,
  mutabilityRune: IRuneS,
  variabilityRune: IRuneS,
  sizeRune: IRuneS,
  elementRune: IRuneS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class ManualSequenceSR(
  range: RangeS,
  resultRune: IRuneS,
  elements: Array[IRuneS]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

case class CoordListSR(
  range: RangeS,
  resultRune: IRuneS,
  elements: Array[IRuneS]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
}

//case class RuneLeafSR(
//  range: RangeS,
//  rune: IRuneS
//) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}
//
//case class ValueLeafSR(
//  range: RangeS,
//  resultRune: IRuneS,
//  value: IValueSR
//) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}


sealed trait IValueSR {
  def getType(): ITypeSR
}

case class IntLiteralSR(value: Long) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = IntTypeSR
}
case class StringLiteralSR(value: String) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = StringTypeSR
}
case class BoolLiteralSR(value: Boolean) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = BoolTypeSR
}
case class MutabilityLiteralSR(mutability: MutabilityP) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = MutabilityTypeSR
}
case class PermissionLiteralSR(permission: PermissionP) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = PermissionTypeSR
}
case class LocationLiteralSR(location: LocationP) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = LocationTypeSR
}
case class OwnershipLiteralSR(ownership: OwnershipP) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = OwnershipTypeSR
}
case class VariabilityLiteralSR(variability: VariabilityP) extends IValueSR {
  override def hashCode(): Int = vcurious()
  override def getType(): ITypeSR = VariabilityTypeSR
}

//// This is a rune that we know is defined, which came from the parent env
//case class EnvRuneLookupSR(
//  rune: IRuneS
//) extends IValueSR {
//  override def hashCode(): Int = vcurious()
//}

sealed trait INameSR

case class NameSR(
  name: IImpreciseNameStepS
) extends INameSR {
  override def hashCode(): Int = vcurious()
}

case class AbsoluteNameSR(
  name: INameS
) extends INameSR {
  override def hashCode(): Int = vcurious()
}



sealed trait ITypeSR
case object IntTypeSR extends ITypeSR
case object StringTypeSR extends ITypeSR
case object PrototypeTypeSR extends ITypeSR
case object BoolTypeSR extends ITypeSR
case object OwnershipTypeSR extends ITypeSR
case object MutabilityTypeSR extends ITypeSR
case object PermissionTypeSR extends ITypeSR
case object LocationTypeSR extends ITypeSR
case object CoordTypeSR extends ITypeSR
case object KindTypeSR extends ITypeSR
case object FunctionTypeSR extends ITypeSR
case class TemplateTypeSR(params: Vector[ITypeSR], result: ITypeSR) extends ITypeSR {
  override def hashCode(): Int = vcurious()
}
case object VariabilityTypeSR extends ITypeSR
//case object StructTypeSR extends ITypeSR
//case object SequenceTypeSR extends ITypeSR
// We need PackTypeSR because we have a built-in templated destructor whose rules
// only match packs... PackTypeSR is how it does that.
//case object PackTypeSR extends ITypeSR
//case object ArrayTypeSR extends ITypeSR
//case object CallableTypeSR extends ITypeSR
//case object InterfaceTypeSR extends ITypeSR


//object RuleSUtils {
//
//  def getDistinctOrderedRunesForRulex(rulex: IRulexSR): Vector[IRuneS] = {
//    rulex match {
////      case PackSR(elements) => getDistinctOrderedRunesForRulexes(elements)
//      case EqualsSR(range, left, right) => Vector(left, right).distinct
//      case IsaSR(range, left, right) => Vector(left, right).distinct
//      case OrSR(range, rune, possibilities) => Vector(rune)
//      case KindComponentsSR(_, kindRune, mutabilityRune) => {
//        getDistinctOrderedRunesForRulex(container) ++ components.flatMap(getDistinctOrderedRunesForRulex).toSet
//      }
//      case TypedSR(_, rune, tyype) => Vector(rune)
//      case NameSR(_, name) => Vector()
////      case templex => TemplexSUtils.getDistinctOrderedRunesForTemplex(templex)
//      case CallSR(_, name, args) => args.flatMap(getDistinctOrderedRunesForRulex).distinct
//    }
//  }
//
//  def getDistinctOrderedRunesForRulexes(rulexes: Vector[IRulexSR]): Vector[IRuneS] = {
//    rulexes.flatMap(getDistinctOrderedRunesForRulex).distinct
//  }
//}
