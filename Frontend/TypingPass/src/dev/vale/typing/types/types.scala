package dev.vale.typing.types

import dev.vale.{CodeLocationS, IInterning, Interner, Keywords, PackageCoordinate, vassert, vcurious, vfail, vpass}
import dev.vale.postparsing.IImpreciseNameS
import dev.vale.typing.ast.{AbstractT, FunctionHeaderT, ICitizenAttributeT}
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.names.{CitizenNameT, FullNameT, ICitizenNameT, IVarNameT, RawArrayNameT, RuntimeSizedArrayNameT, StaticSizedArrayNameT}
import dev.vale.highertyping._
import dev.vale.postparsing._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.names.AnonymousSubstructNameT
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.List

sealed trait OwnershipT  {
}
case object ShareT extends OwnershipT {
  override def toString: String = "share"
}
case object OwnT extends OwnershipT {
  override def toString: String = "own"
}
case object BorrowT extends OwnershipT {
  override def toString: String = "borrow"
}
case object WeakT extends OwnershipT {
  override def toString: String = "weak"
}

sealed trait MutabilityT  {
}
case object MutableT extends MutabilityT {
  override def toString: String = "mut"
}
case object ImmutableT extends MutabilityT {
  override def toString: String = "imm"
}

sealed trait VariabilityT  {
}
case object FinalT extends VariabilityT {
  override def toString: String = "final"
}
case object VaryingT extends VariabilityT {
  override def toString: String = "vary"
}

sealed trait LocationT  {
}
case object InlineT extends LocationT {
  override def toString: String = "inl"
}
case object YonderT extends LocationT {
  override def toString: String = "heap"
}


case class CoordT(ownership: OwnershipT, kind: KindT)  {
  vpass()

  kind match {
    case IntT(_) | BoolT() | StrT() | FloatT() | VoidT() | NeverT(_) => {
      vassert(ownership == ShareT)
    }
    case _ =>
  }
  if (ownership == OwnT) {
    // See CSHROOR for why we don't assert this.
    // vassert(permission == Readwrite)
  }
}

sealed trait KindT {

  // Note, we don't have a mutability: Mutability in here because this Kind
  // should be enough to uniquely identify a type, and no more.
  // We can always get the mutability for a struct from the coutputs.
}

// like Scala's Nothing. No instance of this can ever happen.
case class NeverT(
  // True if this Never came from a break.
  // While will have to know about this; if it's a Never from a ret, it should
  // propagate it, but if its body is a break never, the while produces a void.
  // See BRCOBS.
  fromBreak: Boolean
) extends KindT {

}

// Mostly for interoperability with extern functions
case class VoidT() extends KindT {

}

object IntT {
  val i32: IntT = IntT(32)
  val i64: IntT = IntT(64)
}
case class IntT(bits: Int) extends KindT {
}

case class BoolT() extends KindT {

}

case class StrT() extends KindT {

}

case class FloatT() extends KindT {

}

case class StaticSizedArrayTT(
  size: Int,
  mutability: MutabilityT,
  variability: VariabilityT,
  elementType: CoordT
) extends KindT with IInterning  {
  def getName(interner: Interner, keywords: Keywords): FullNameT[StaticSizedArrayNameT] = FullNameT(PackageCoordinate.BUILTIN(interner, keywords), Vector.empty, interner.intern(StaticSizedArrayNameT(size, interner.intern(RawArrayNameT(mutability, elementType)))))
}

case class RuntimeSizedArrayTT(
  mutability: MutabilityT,
  elementType: CoordT
) extends KindT with IInterning {


  def getName(interner: Interner, keywords: Keywords): FullNameT[RuntimeSizedArrayNameT] = FullNameT(PackageCoordinate.BUILTIN(interner, keywords), Vector.empty, interner.intern(RuntimeSizedArrayNameT(interner.intern(RawArrayNameT(mutability, elementType)))))
}

trait CitizenRefT extends KindT with IInterning {
  def fullName: FullNameT[ICitizenNameT]
}

// These should only be made by struct typingpass, which puts the definition into coutputs at the same time
case class StructTT(fullName: FullNameT[ICitizenNameT]) extends CitizenRefT {

}

case class InterfaceTT(
  fullName: FullNameT[ICitizenNameT]
) extends CitizenRefT  {


}

// Represents a bunch of functions that have the same name.
// See ROS.
// Lowers to an empty struct.
case class OverloadSetT(
  env: IEnvironment,
  // The name to look for in the environment.
  name: IImpreciseNameS
) extends KindT with IInterning {


}

case class PlaceholderT(identifyingRuneIndex: Int) extends KindT

// This is what we use to search for overloads.
case class ParamFilter(
    tyype: CoordT,
    virtuality: Option[AbstractT]) {
   override def equals(obj: Any): Boolean = vcurious();

  def debugString: String = {
    tyype.toString + virtuality.map(x => " abstract").getOrElse("")
  }
}
