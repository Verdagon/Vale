package dev.vale.typing.ast

import dev.vale.typing.names.{CitizenNameT, FullNameT, ICitizenNameT, IVarNameT}
import dev.vale.typing.types._
import dev.vale.{vcurious, vfail, vpass}

// A "citizen" is a struct or an interface.
trait CitizenDefinitionT {
  def getRef: CitizenRefT;
}


// We include templateArgTypes to aid in looking this up... same reason we have name
case class StructDefinitionT(
  fullName: FullNameT[ICitizenNameT],
  ref: StructTT,
  attributes: Vector[ICitizenAttributeT],
  weakable: Boolean,
  mutability: MutabilityT,
  members: Vector[StructMemberT],
  isClosure: Boolean
) extends CitizenDefinitionT {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  override def getRef: StructTT = ref



  def getMember(memberName: String): StructMemberT = {
    members.find(p => p.name.equals(memberName)) match {
      case None => vfail("Couldn't find member " + memberName)
      case Some(member) => member
    }
  }

  private def getIndex(memberName: IVarNameT): Int = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => vfail("wat")
      case Some((member, index)) => index
    }
  }

  def getMemberAndIndex(memberName: IVarNameT): Option[(StructMemberT, Int)] = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName))
  }
}

case class StructMemberT(
  name: IVarNameT,
  // In the case of address members, this refers to the variability of the pointee variable.
  variability: VariabilityT,
  tyype: IMemberTypeT
)  {

  vpass()
}

sealed trait IMemberTypeT  {
  def reference: CoordT

  def expectReferenceMember(): ReferenceMemberTypeT = {
    this match {
      case r @ ReferenceMemberTypeT(_) => r
      case a @ AddressMemberTypeT(_) => vfail("Expected reference member, was address member!")
    }
  }
  def expectAddressMember(): AddressMemberTypeT = {
    this match {
      case r @ ReferenceMemberTypeT(_) => vfail("Expected reference member, was address member!")
      case a @ AddressMemberTypeT(_) => a
    }
  }
}
case class AddressMemberTypeT(reference: CoordT) extends IMemberTypeT
case class ReferenceMemberTypeT(reference: CoordT) extends IMemberTypeT

case class InterfaceDefinitionT(
  fullName: FullNameT[CitizenNameT],
  ref: InterfaceTT,
  attributes: Vector[ICitizenAttributeT],
  weakable: Boolean,
  mutability: MutabilityT,
  // This does not include abstract functions declared outside the interface.
  // See IMRFDI for why we need to remember only the internal methods here.
  internalMethods: Vector[FunctionHeaderT]
) extends CitizenDefinitionT  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def getRef = ref
}
