package dev.vale.typing.ast

import dev.vale.postparsing.{IRuneS, ITemplataType, MutabilityTemplataType}
import dev.vale.typing.TemplataCompiler
import dev.vale.typing.names.{CitizenNameT, CodeVarNameT, FullNameT, FunctionBoundNameT, ICitizenNameT, ICitizenTemplateNameT, IInterfaceNameT, IInterfaceTemplateNameT, IStructNameT, IStructTemplateNameT, IVarNameT, StructNameT}
import dev.vale.typing.templata.ITemplata
import dev.vale.typing.types._
import dev.vale.{StrI, vcurious, vfail, vpass}

import scala.collection.immutable.Map

// A "citizen" is a struct or an interface.
trait CitizenDefinitionT {
  def templateName: FullNameT[ICitizenTemplateNameT]
  def genericParamTypes: Vector[ITemplataType]
  def instantiatedCitizen: ICitizenTT
}

case class StructDefinitionT(
  templateName: FullNameT[IStructTemplateNameT],
  // In typing pass, this will have placeholders. Monomorphizing will give it a real name.
  instantiatedCitizen: StructTT,
  attributes: Vector[ICitizenAttributeT],
  weakable: Boolean,
  mutability: ITemplata[MutabilityTemplataType],
  members: Vector[StructMemberT],
  isClosure: Boolean,
  runeToFunctionBound: Map[IRuneS, FullNameT[FunctionBoundNameT]],
) extends CitizenDefinitionT {
  override def genericParamTypes: Vector[ITemplataType] = {
    instantiatedCitizen.fullName.last.templateArgs.map(_.tyype)
  }

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

//  override def getRef: StructTT = ref

  def getMember(memberName: StrI): StructMemberT = {
    members.find(p => p.name.equals(CodeVarNameT(memberName))) match {
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
  def reference: UnsubstitutedCoordT

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
// DO NOT SUBMIT document
case class UnsubstitutedCoordT(unsubstitutedCoord: CoordT)
case class AddressMemberTypeT(reference: UnsubstitutedCoordT) extends IMemberTypeT
case class ReferenceMemberTypeT(reference: UnsubstitutedCoordT) extends IMemberTypeT

case class InterfaceDefinitionT(
  templateName: FullNameT[IInterfaceTemplateNameT],
  instantiatedInterface: InterfaceTT,
  ref: InterfaceTT,
  attributes: Vector[ICitizenAttributeT],
  weakable: Boolean,
  mutability: ITemplata[MutabilityTemplataType],
  runeToFunctionBound: Map[IRuneS, FullNameT[FunctionBoundNameT]],
  // This does not include abstract functions declared outside the interface.
  // See IMRFDI for why we need to remember only the internal methods here.
  internalMethods: Vector[FunctionHeaderT]
) extends CitizenDefinitionT  {
  override def genericParamTypes: Vector[ITemplataType] = {
    instantiatedCitizen.fullName.last.templateArgs.map(_.tyype)
  }

  override def instantiatedCitizen: ICitizenTT = instantiatedInterface
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
//  override def getRef = ref
}
