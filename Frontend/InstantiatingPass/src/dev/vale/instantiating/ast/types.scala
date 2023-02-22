package dev.vale.instantiating.ast

import dev.vale._
import dev.vale.postparsing.IImpreciseNameS
import dev.vale.typing.ast._
import dev.vale.typing.env.IInDenizenEnvironment
import dev.vale.typing.names._
import dev.vale.highertyping._
import dev.vale.postparsing._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.List

sealed trait OwnershipI {
}
// Instantiator turns BorrowI into MutableBorrowI and ImmutableBorrowI, see HRALII
case object ImmutableShareI extends OwnershipI {
  override def toString: String = "immshare"
}
// Instantiator turns ShareI into MutableShareI and ImmutableShareI, see HRALII
// Ironic because shared things are immutable, this is rather referring to the refcount.
case object MutableShareI extends OwnershipI {
  override def toString: String = "mutshare"
}
case object OwnI extends OwnershipI {
  override def toString: String = "own"
}
case object WeakI extends OwnershipI {
  override def toString: String = "weak"
}
// Instantiator turns BorrowI into MutableBorrowI and ImmutableBorrowI, see HRALII
case object ImmutableBorrowI extends OwnershipI {
  override def toString: String = "immborrow"
}
// Instantiator turns BorrowI into MutableBorrowI and ImmutableBorrowI, see HRALII
case object MutableBorrowI extends OwnershipI {
  override def toString: String = "mutborrow"
}

sealed trait MutabilityI  {
}
case object MutableI extends MutabilityI {
  override def toString: String = "mut"
}
case object ImmutableI extends MutabilityI {
  override def toString: String = "imm"
}

sealed trait VariabilityI  {
}
case object FinalI extends VariabilityI {
  override def toString: String = "final"
}
case object VaryingI extends VariabilityI {
  override def toString: String = "vary"
}

sealed trait LocationI  {
}
case object InlineI extends LocationI {
  override def toString: String = "inl"
}
case object YonderI extends LocationI {
  override def toString: String = "heap"
}


case class CoordI(
  ownership: OwnershipI,
  kind: KindIT)  {

  vpass()

  kind match {
    case IntIT(_) | BoolIT() | StrIT() | FloatIT() | VoidIT() | NeverIT(_) => {
      vassert(ownership == MutableShareI || ownership == ImmutableShareI)
    }
    case RuntimeSizedArrayIT(IdI(_, _, RuntimeSizedArrayNameI(_, RawArrayNameI(_, _, arrRegion)))) =>
    case StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, _, _, RawArrayNameI(_, _, arrRegion)))) =>
    case StructIT(IdI(_, _, localName)) =>
    case InterfaceIT(IdI(_, _, localName)) =>
    case _ =>
  }
  if (ownership == OwnI) {
    // See CSHROOR for why we don't assert this.
    // vassert(permission == Readwrite)
  }
}

sealed trait KindIT {
  // Note, we don't have a mutability: Mutability in here because this Kind
  // should be enough to uniquely identify a type, and no more.
  // We can always get the mutability for a struct from the coutputs.

  def expectCitizen(): ICitizenIT = {
    this match {
      case c : ICitizenIT => c
      case _ => vfail()
    }
  }

  def expectInterface(): InterfaceIT = {
    this match {
      case c @ InterfaceIT(_) => c
      case _ => vfail()
    }
  }

  def expectStruct(): StructIT = {
    this match {
      case c @ StructIT(_) => c
      case _ => vfail()
    }
  }
}

// like Scala's Nothing. No instance of this can ever happen.
case class NeverIT(
  // True if this Never came from a break.
  // While will have to know about this; if IT's a Never from a ret, IT should
  // propagate IT, but if its body is a break never, the while produces a void.
  // See BRCOBS.
  fromBreak: Boolean
) extends KindIT {

}

// Mostly for interoperability with extern functions
case class VoidIT() extends KindIT {

}

object IntIT {
  val i32: IntIT = IntIT(32)
  val i64: IntIT = IntIT(64)
}
case class IntIT(bits: Int) extends KindIT {
}

case class BoolIT() extends KindIT {

}

case class StrIT() extends KindIT {

}

case class FloatIT() extends KindIT {

}

object contentsStaticSizedArrayIT {
  def unapply(ssa: StaticSizedArrayIT):
  Option[(ITemplataI[IntegerTemplataType], ITemplataI[MutabilityTemplataType], ITemplataI[VariabilityTemplataType], CoordI, ITemplataI[RegionTemplataType])] = {
    val IdI(_, _, StaticSizedArrayNameI(_, size, variability, RawArrayNameI(mutability, coord, selfRegion))) = ssa.name
    Some((size, mutability, variability, coord, selfRegion))
  }
}

case class StaticSizedArrayIT(
  name: IdI[StaticSizedArrayNameI]
) extends KindIT {
  vassert(name.initSteps.isEmpty)
  def mutability: ITemplataI[MutabilityTemplataType] = name.localName.arr.mutability
  def elementType = name.localName.arr.elementType
  def size = name.localName.size
  def variability = name.localName.variability
}

object contentsRuntimeSizedArrayIT {
  def unapply(rsa: RuntimeSizedArrayIT):
  Option[(ITemplataI[MutabilityTemplataType], CoordI, ITemplataI[RegionTemplataType])] = {
    val IdI(_, _, RuntimeSizedArrayNameI(_, RawArrayNameI(mutability, coord, selfRegion))) = rsa.name
    Some((mutability, coord, selfRegion))
  }
}
case class RuntimeSizedArrayIT(
  name: IdI[RuntimeSizedArrayNameI]
) extends KindIT {
  def mutability = name.localName.arr.mutability
  def elementType = name.localName.arr.elementType

//  name.localName.arr.selfRegion match {
//    case RegionTemplata(false) => vwat()
//    case _ =>
//  }
}

object ICitizenIT {
  def unapply(self: ICitizenIT): Option[IdI[ICitizenNameI]] = {
    Some(self.id)
  }
}

// Structs, interfaces, and placeholders
sealed trait ISubKindIT extends KindIT {
  def id: IdI[ISubKindNameI]
}
// Interfaces and placeholders
sealed trait ISuperKindIT extends KindIT {
  def id: IdI[ISuperKindNameI]
}

sealed trait ICitizenIT extends ISubKindIT {
  def id: IdI[ICitizenNameI]
}

// These should only be made by StructCompiler, which puts the definition and bounds into coutputs at the same time
case class StructIT(id: IdI[IStructNameI]) extends ICitizenIT {
  (id.initSteps.lastOption, id.localName) match {
    case (Some(StructTemplateNameI(_)), StructNameI(_, _)) => vfail()
    case _ =>
  }
}

case class InterfaceIT(id: IdI[IInterfaceNameI]) extends ICitizenIT with ISuperKindIT {
  (id.initSteps.lastOption, id.localName) match {
    case (Some(InterfaceTemplateNameI(_)), InterfaceNameI(_, _)) => vfail()
    case _ =>
  }
}

// Represents a bunch of functions that have the same name.
// See ROS.
// Lowers to an empty struct.
case class OverloadSeIT(
  env: IInDenizenEnvironment,
  // The name to look for in the environment.
  name: IImpreciseNameS
) extends KindIT {
  vpass()

}

// At some point IT'd be nice to make Coord.kind into a templata so we can directly have a
// placeholder templata instead of needing this special kind.
case class KindPlaceholderIT(id: IdI[KindPlaceholderNameI]) extends ISubKindIT with ISuperKindIT
