package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules.IRulexSR
import net.verdagon.vale.{PackageCoordinate, vassert, vcheck, vcurious, vimpl, vpass, vwat}

import scala.collection.immutable.List
import scala.runtime
import scala.runtime.ScalaRunTime
import scala.util.hashing.MurmurHash3

// We paackage runes with a full name so we don't have to worry about collisions
// between, for example, two ImplicitRune(0)s.

// We have this INameS stuff so we don't have to have prefixes and names like
// __magic_0 __magic_1 __Closure etc.

sealed trait INameS
sealed trait IVarNameS extends INameS
sealed trait IFunctionDeclarationNameS extends INameS {
  def packageCoordinate: PackageCoordinate
}
case class LambdaNameS(
//  parentName: INameS,
  codeLocation: CodeLocationS
) extends IFunctionDeclarationNameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  override def packageCoordinate: PackageCoordinate = codeLocation.file.packageCoordinate
}
case class FunctionNameS(name: String, codeLocation: CodeLocationS) extends IFunctionDeclarationNameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  override def packageCoordinate: PackageCoordinate = codeLocation.file.packageCoordinate
}
case class TopLevelCitizenDeclarationNameS(name: String, codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class LambdaStructNameS(lambdaName: LambdaNameS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ImplNameS(subCitizenHumanName: String, codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ExportAsNameS(codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class LetNameS(codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
//case class UnnamedLocalNameS(codeLocation: CodeLocationS) extends IVarNameS {  override def hashCode(): Int = vcurious() }
case class ClosureParamNameS() extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MagicParamNameS(codeLocation: CodeLocationS) extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class CodeVarNameS(name: String) extends IVarNameS {
  vcheck(name != "set", "Can't name a variable 'set'")
  vcheck(name != "mut", "Can't name a variable 'mut'")
}
case class ConstructingMemberNameS(name: String) extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// We differentiate rune names from regular names, we scout out what's actually
// a rune so we can inform the templar. The templar wants to know so it can know
// how to handle this thing; if it's a name, we expect it to exist in the
// environment already, but if it's a rune we can assign something into it.
// Also, we might refer to a rune that was defined in our container's container's
// container, so asking "is this thing here a rune" involves looking at all our
// containers. That's much easier for the scout, so thats a nice bonus.
// We have all these subclasses instead of a string so we don't have to have
// prefixes and names like __implicit_0, __paramRune_0, etc.
trait IRuneS
case class CodeRuneS(name: String) extends IRuneS {
  vpass()
}
case class ImplicitRuneS(lid: LocationInDenizen) extends IRuneS {
  vpass()
}
case class LetImplicitRuneS(lid: LocationInDenizen) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MagicParamRuneS(lid: LocationInDenizen) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MemberRuneS(memberIndex: Int) extends IRuneS {
   val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
// Used to type the templex handed to the size part of the static sized array expressions
case class ArraySizeImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// Used to type the templex handed to the mutability part of the static sized array expressions
case class ArrayMutabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// Used to type the templex handed to the variability part of the static sized array expressions
case class ArrayVariabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ReturnRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// These are only made by the templar
case class ExplicitTemplateArgRuneS(index: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

trait IImpreciseNameStepS
case class CodeTypeNameS(name: String) extends IImpreciseNameStepS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// When we're calling a function, we're addressing an overload set, not a specific function.
// If we want a specific function, we use TopLevelDeclarationNameS.
case class GlobalFunctionFamilyNameS(name: String) extends IImpreciseNameStepS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
case class ImpreciseCodeVarNameS(name: String) extends IImpreciseNameStepS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }


// See PVSBUFI
////case class IntSR(range: RangeS, value: Long) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class StringSR(range: RangeS, value: String) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class MutabilitySR(range: RangeS, mutability: MutabilityP) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class PermissionSR(range: RangeS, permission: PermissionP) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class LocationSR(range: RangeS, location: LocationP) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class OwnershipSR(range: RangeS, ownership: OwnershipP) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class VariabilitySR(range: RangeS, variability: VariabilityP) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class BoolSR(range: RangeS, value: Boolean) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class AbsoluteNameSR(range: RangeS, name: INameS) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class NameSR(range: RangeS, name: CodeTypeNameS) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class RuneSR(range: RangeS, rune: IRuneS) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class InterpretedSR(range: RangeS, ownership: OwnershipP, permission: PermissionP, inner: IRulexSR) extends IRulexSR { override def hashCode(): Int = vcurious() }
//////case class PermissionedST(range: RangeS, permission: PermissionP, inner: ITemplexS) extends ITemplexS {  override def hashCode(): Int = vcurious() }
//////case class NullableSR(range: RangeS, inner: IRulexSR) extends IRulexSR { override def hashCode(): Int = vcurious() }
////case class CallSR(range: RangeS,
////    template: IRulexSR,
////    args: Vector[IRulexSR]) extends IRulexSR {
////}
////case class FunctionST(
////  mutability: Option[ITemplexS],
////  parameters: Vector[Option[ITemplexS]],
////  returnType: Option[ITemplexS]
////) extends ITemplexS {  override def hashCode(): Int = vcurious() }
//case class PrototypeSR(
//  range: RangeS,
//  name: String,
//  parameters: Vector[IRulexSR],
//  returnType: IRulexSR
//) extends IRulexSR { override def hashCode(): Int = vcurious() }
//case class PackSR(
//  range: RangeS,
//  members: Vector[IRulexSR]
//) extends IRulexSR { override def hashCode(): Int = vcurious() }
//case class BorrowSR(
//  range: RangeS,
//  inner: IRulexSR
//) extends IRulexSR { override def hashCode(): Int = vcurious() }
//case class RepeaterSequenceSR(
//  range: RangeS,
//  mutability: IRulexSR,
//  variability: IRulexSR,
//  size: IRulexSR,
//  element: IRulexSR
//) extends IRulexSR { override def hashCode(): Int = vcurious() }
//case class ManualSequenceSR(
//  range: RangeS,
//  elements: Vector[IRulexSR]
//) extends IRulexSR { override def hashCode(): Int = vcurious() }
//
//object TemplexSUtils {
//  def getDistinctOrderedRunesForTemplex(templex: IRulexSR): Vector[IRuneS] = {
//    templex match {
//      case StringSR(_, _) => Vector.empty
//      case IntSR(_, _) => Vector.empty
//      case MutabilitySR(_, _) => Vector.empty
//      case PermissionSR(_, _) => Vector.empty
//      case LocationSR(_, _) => Vector.empty
//      case OwnershipSR(_, _) => Vector.empty
//      case VariabilitySR(_, _) => Vector.empty
//      case BoolSR(_, _) => Vector.empty
//      case NameSR(_, _) => Vector.empty
//      case AbsoluteNameSR(_, _) => Vector.empty
//      case RuneSR(_, rune) => Vector(rune)
//      case InterpretedSR(_, _, _, inner) => getDistinctOrderedRunesForTemplex(inner)
//      case BorrowSR(_, inner) => getDistinctOrderedRunesForTemplex(inner)
//      case CallSR(_, template, args) => {
//        (Vector(template) ++ args).flatMap(getDistinctOrderedRunesForTemplex).distinct
//      }
//      case PrototypeSR(_, name, parameters, returnType) => {
//        (parameters :+ returnType).flatMap(getDistinctOrderedRunesForTemplex).distinct
//      }
//      case PackSR(_, members) => {
//        members.flatMap(getDistinctOrderedRunesForTemplex).distinct
//      }
//      case RepeaterSequenceSR(_, mutability, variability, size, element) => {
//        Vector(mutability, variability, size, element).flatMap(getDistinctOrderedRunesForTemplex).distinct
//      }
//      case ManualSequenceSR(_, elements) => {
//        elements.flatMap(getDistinctOrderedRunesForTemplex).distinct
//      }
//    }
//  }
//}
