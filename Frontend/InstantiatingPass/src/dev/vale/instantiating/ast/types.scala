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

sealed trait MutabilityI {
}
case object MutableI extends MutabilityI {
  override def toString: String = "mut"
}
case object ImmutableI extends MutabilityI {
  override def toString: String = "imm"
}

sealed trait VariabilityI {
}
case object FinalI extends VariabilityI {
  override def toString: String = "final"
}
case object VaryingI extends VariabilityI {
  override def toString: String = "vary"
}

sealed trait LocationI {
}
case object InlineI extends LocationI {
  override def toString: String = "inl"
}
case object YonderI extends LocationI {
  override def toString: String = "heap"
}

sealed trait IRegionsModeI
case class sI() extends IRegionsModeI
case class cI() extends IRegionsModeI

object CoordI {
  def void[R <: IRegionsModeI]: CoordI[R] = CoordI[R](MutableShareI, VoidIT())
}

case class CoordI[R <: IRegionsModeI](
  ownership: OwnershipI,
  kind: KindIT[R])  {

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

sealed trait KindIT[R <: IRegionsModeI] {
  // Note, we don't have a mutability: Mutability in here because this Kind
  // should be enough to uniquely identify a type, and no more.
  // We can always get the mutability for a struct from the coutputs.

  def expectCitizen(): ICitizenIT[R] = {
    this match {
      case c : ICitizenIT[R] => c
      case _ => vfail()
    }
  }

  def expectInterface(): InterfaceIT[R] = {
    this match {
      case c @ InterfaceIT(_) => c
      case _ => vfail()
    }
  }

  def expectStruct(): StructIT[R] = {
    this match {
      case c @ StructIT(_) => c
      case _ => vfail()
    }
  }
}

// like Scala's Nothing. No instance of this can ever happen.
case class NeverIT[R <: IRegionsModeI](
  // True if this Never came from a break.
  // While will have to know about this; if IT's a Never from a ret, IT should
  // propagate IT, but if its body is a break never, the while produces a void.
  // See BRCOBS.
  fromBreak: Boolean
) extends KindIT[R] {

}

// Mostly for interoperability with extern functions
case class VoidIT[R <: IRegionsModeI]() extends KindIT[R] {

}

case class IntIT[R <: IRegionsModeI](bits: Int) extends KindIT[R] {
}

case class BoolIT[R <: IRegionsModeI]() extends KindIT[R] {

}

case class StrIT[R <: IRegionsModeI]() extends KindIT[R] {

}

case class FloatIT[R <: IRegionsModeI]() extends KindIT[R] {

}

object contentsStaticSizedArrayIT {
  def unapply[R <: IRegionsModeI](ssa: StaticSizedArrayIT[R]):
  Option[(Long, MutabilityI, VariabilityI, CoordI[R], RegionTemplataI[R])] = {
    val IdI(_, _, StaticSizedArrayNameI(_, size, variability, RawArrayNameI(mutability, coord, selfRegion))) = ssa.name
    Some((size, mutability, variability, coord, selfRegion))
  }
}

case class StaticSizedArrayIT[R <: IRegionsModeI](
  name: IdI[R, StaticSizedArrayNameI[R]]
) extends KindIT[R] {
  vassert(name.initSteps.isEmpty)
  def mutability: MutabilityI = name.localName.arr.mutability
  def elementType = name.localName.arr.elementType
  def size = name.localName.size
  def variability = name.localName.variability
}

object contentsRuntimeSizedArrayIT {
  def unapply[R <: IRegionsModeI](rsa: RuntimeSizedArrayIT[R]):
  Option[(MutabilityI, CoordI[R], RegionTemplataI[R])] = {
    val IdI(_, _, RuntimeSizedArrayNameI(_, RawArrayNameI(mutability, coord, selfRegion))) = rsa.name
    Some((mutability, coord, selfRegion))
  }
}
case class RuntimeSizedArrayIT[R <: IRegionsModeI](
  name: IdI[R, RuntimeSizedArrayNameI[R]]
) extends KindIT[R] {
  def mutability = name.localName.arr.mutability
  def elementType = name.localName.arr.elementType

//  name.localName.arr.selfRegion match {
//    case RegionTemplata(false) => vwat()
//    case _ =>
//  }
}

object ICitizenIT {
  def unapply[R <: IRegionsModeI](self: ICitizenIT[R]): Option[IdI[R, ICitizenNameI[R]]] = {
    Some(self.id)
  }
}

// Structs, interfaces, and placeholders
sealed trait ISubKindIT[R <: IRegionsModeI] extends KindIT[R] {
  def id: IdI[R, ISubKindNameI[R]]
}
// Interfaces and placeholders
sealed trait ISuperKindIT[R <: IRegionsModeI] extends KindIT[R] {
  def id: IdI[R, ISuperKindNameI[R]]
}

sealed trait ICitizenIT[R <: IRegionsModeI] extends ISubKindIT[R] {
  def id: IdI[R, ICitizenNameI[R]]
}

// These should only be made by StructCompiler, which puts the definition and bounds into coutputs at the same time
case class StructIT[R <: IRegionsModeI](id: IdI[R, IStructNameI[R]]) extends ICitizenIT[R] {
  (id.initSteps.lastOption, id.localName) match {
    case (Some(StructTemplateNameI(_)), StructNameI(_, _)) => vfail()
    case _ =>
  }
}

case class InterfaceIT[R <: IRegionsModeI](id: IdI[R, IInterfaceNameI[R]]) extends ICitizenIT[R] with ISuperKindIT[R] {
  (id.initSteps.lastOption, id.localName) match {
    case (Some(InterfaceTemplateNameI(_)), InterfaceNameI(_, _)) => vfail()
    case _ =>
  }
}

// Represents a bunch of functions that have the same name.
// See ROS.
// Lowers to an empty struct.
case class OverloadSeIT[R <: IRegionsModeI](
  env: IInDenizenEnvironment,
  // The name to look for in the environment.
  name: IImpreciseNameS
) extends KindIT[R] {
  vpass()

}
