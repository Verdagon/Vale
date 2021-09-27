package net.verdagon.vale.scout.rules

import net.verdagon.vale.{vassert, vcurious, vimpl, vpass, vwat}
import net.verdagon.vale.parser.{LocationP, MutabilityP, OwnershipP, PermissionP, VariabilityP}
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.types._

import scala.collection.immutable.List

case class RuneUsage(range: RangeS, rune: IRuneS)

// This isn't generic over e.g.  because we shouldnt reuse
// this between layers. The generics solver doesn't even know about IRulexSR, doesn't
// need to, it relies on delegates to do any rule-specific things.
// Different stages will likely need different kinds of rules, so best not prematurely
// combine them.
trait IRulexSR {
  def range: RangeS
  def runeUsages: Array[RuneUsage]
}

case class EqualsSR(range: RangeS, left: RuneUsage, right: RuneUsage) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(left, right)
}

case class IsaSR(range: RangeS, sub: RuneUsage, suuper: RuneUsage) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(sub, suuper)
}

case class KindComponentsSR(
  range: RangeS,
  kindRune: RuneUsage,
  mutabilityRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(kindRune, mutabilityRune)
}

case class CoordComponentsSR(
  range: RangeS,
  resultRune: RuneUsage,
  ownershipRune: RuneUsage,
  permissionRune: RuneUsage,
  kindRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, ownershipRune, permissionRune, kindRune)
}

case class PrototypeComponentsSR(
  range: RangeS,
  resultRune: RuneUsage,
  nameRune: RuneUsage,
  paramsListRune: RuneUsage,
  returnRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, nameRune, paramsListRune, returnRune)
}

// See Possible Values Shouldnt Be Used For Inference (PVSBUFI)
case class OneOfSR(
  range: RangeS,
  rune: RuneUsage,
  literals: Array[ILiteralSL]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  vassert(literals.nonEmpty)
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

case class IsConcreteSR(
  range: RangeS,
  rune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

case class IsInterfaceSR(
  range: RangeS,
  rune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

case class IsStructSR(
  range: RangeS,
  rune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(rune)
4}

case class CoerceToCoord(
  range: RangeS,
  coordRune: RuneUsage,
  kindRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(coordRune, kindRune)
}

case class LiteralSR(
  range: RangeS,
  rune: RuneUsage,
  literal: ILiteralSL
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

case class LookupSR(
  range: RangeS,
  rune: RuneUsage,
  name: INameS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  vpass()
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

case class KindLookupSR(
  range: RangeS,
  rune: RuneUsage,
  name: INameS
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  vpass()
  override def runeUsages: Array[RuneUsage] = Array(rune)
}

// InterpretedAR will overwrite inner's permission and ownership to the given ones.
// We turned InterpretedAR into this
case class AugmentSR(
  range: RangeS,
  resultRune: RuneUsage,
  // Lets try and figure out a way to only have one thing here instead of a Vector
  literal: Vector[ILiteralSL],
  innerRune: RuneUsage
) extends IRulexSR {
  vpass()
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, innerRune)
}

//case class NullableSR(
//  range: RangeS,
//  resultRune: RuneUsage,
//  inner: Int) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}

case class CallSR(
  range: RangeS,
  resultRune: RuneUsage,
  templateRune: RuneUsage,
  args: Array[RuneUsage]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, templateRune) ++ args
}

//case class CommonMutabilitySR(
//  range: RangeS,
//  resultRune: RuneUsage,
//  args: Array[RuneUsage]
//) extends IRulexSR {
//  override def hashCode(): Int = vcurious()
//}

//case class FunctionSR(
//  mutability: Option[IRulexSR],
//  parameters: Array[Option[IRulexSR]],
//  returnType: Option[IRulexSR]
//) extends IRulexSR {
// override def hashCode(): Int = vcurious()}

case class PrototypeSR(
  range: RangeS,
  resultRune: RuneUsage,
  name: String,
  parameters: Array[RuneUsage],
  returnTypeRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, returnTypeRune) ++ parameters
}

case class PackSR(
  range: RangeS,
  resultRune: RuneUsage,
  members: Array[RuneUsage]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune) ++ members
}

case class RepeaterSequenceSR(
  range: RangeS,
  resultRune: RuneUsage,
  mutabilityRune: RuneUsage,
  variabilityRune: RuneUsage,
  sizeRune: RuneUsage,
  elementRune: RuneUsage
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune)
}

case class ManualSequenceSR(
  range: RangeS,
  resultRune: RuneUsage,
  elements: Array[RuneUsage]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune) ++ elements
}

case class CoordListSR(
  range: RangeS,
  resultRune: RuneUsage,
  elements: Array[RuneUsage]
) extends IRulexSR {
  override def hashCode(): Int = vcurious()
  override def runeUsages: Array[RuneUsage] = Array(resultRune) ++ elements
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


sealed trait ILiteralSL {
  def getType(): ITemplataType
}

case class IntLiteralSL(value: Long) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = IntegerTemplataType
}
case class StringLiteralSL(value: String) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = StringTemplataType
}
case class BoolLiteralSL(value: Boolean) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = BooleanTemplataType
}
case class MutabilityLiteralSL(mutability: MutabilityP) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = MutabilityTemplataType
}
case class PermissionLiteralSL(permission: PermissionP) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = PermissionTemplataType
}
case class LocationLiteralSL(location: LocationP) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = LocationTemplataType
}
case class OwnershipLiteralSL(ownership: OwnershipP) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = OwnershipTemplataType
}
case class VariabilityLiteralSL(variability: VariabilityP) extends ILiteralSL {
  override def hashCode(): Int = vcurious()
  override def getType(): ITemplataType = VariabilityTemplataType
}

//// This is a rune that we know is defined, which came from the parent env
//case class EnvRuneLookupSR(
//  rune: IRuneS
//) extends IValueSR {
//  override def hashCode(): Int = vcurious()
//}



//sealed trait ITemplataType
//case object IntegerTemplataType extends ITemplataType
//case object StringTemplataType extends ITemplataType
//case object PrototypeTemplataType extends ITemplataType
//case object BooleanTemplataType extends ITemplataType
//case object OwnershipTemplataType extends ITemplataType
//case object MutabilityTemplataType extends ITemplataType
//case object PermissionTemplataType extends ITemplataType
//case object LocationTemplataType extends ITemplataType
//case object CoordTemplataType extends ITemplataType
//case object KindTemplataType extends ITemplataType
//case object FunctionTemplataType extends ITemplataType
//case class TemplateTemplataType(params: Vector[ITemplataType], result: ITemplataType) extends ITemplataType {
//  override def hashCode(): Int = vcurious()
//}
//case object VariabilityTemplataType extends ITemplataType

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
