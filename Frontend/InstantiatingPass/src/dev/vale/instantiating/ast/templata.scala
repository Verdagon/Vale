package dev.vale.instantiating.ast

import dev.vale.postparsing._
import dev.vale.typing.env.IInDenizenEnvironment
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FunctionNameT, IFunctionNameT, IImplNameT, INameT, IPlaceholderNameT, IRegionNameT, IdT, InterfaceTemplateNameT, KindPlaceholderNameT, RegionPlaceholderNameT}
import dev.vale.typing.types._
import dev.vale.{RangeS, StrI, vassert, vfail, vimpl, vpass, vwat}
import dev.vale.highertyping._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.types._

import scala.collection.immutable.List


object ITemplataI {
  def expectCoord(templata: ITemplataI): ITemplataI = {
    templata match {
      case t @ CoordTemplataI(_) => t
      case other => vfail(other)
    }
  }

  def expectCoordTemplata(templata: ITemplataI): CoordTemplataI = {
    templata match {
      case t @ CoordTemplataI(_) => t
      case other => vfail(other)
    }
  }

  def expectIntegerTemplata(templata: ITemplataI): IntegerTemplataI = {
    templata match {
      case t @ IntegerTemplataI(_) => t
      case _ => vfail()
    }
  }

  def expectMutabilityTemplata(templata: ITemplataI): MutabilityTemplataI = {
    templata match {
      case t @ MutabilityTemplataI(_) => t
      case _ => vfail()
    }
  }

  def expectVariabilityTemplata(templata: ITemplataI): VariabilityTemplataI = {
    templata match {
      case t @ VariabilityTemplataI(_) => t
      case _ => vfail()
    }
  }

  def expectKind(templata: ITemplataI): ITemplataI = {
    templata match {
      case t @ KindTemplataI(_) => t
      case _ => vfail()
    }
  }

  def expectKindTemplata(templata: ITemplataI): KindTemplataI = {
    templata match {
      case t @ KindTemplataI(_) => t
      case _ => vfail()
    }
  }

  def expectRegionTemplata(templata: ITemplataI): RegionTemplataI = {
    templata match {
      case t @ RegionTemplataI(_) => t
      case _ => vfail()
    }
  }

}

sealed trait ITemplataI

//// The typing phase never makes one of these, they're purely abstract and conceptual in the
//// typing phase. The monomorphizer is the one that actually makes these templatas.
//case class RegionTemplataI(pureHeight: Int) extends ITemplataI {
//  vpass()
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//
//}

case class CoordTemplataI(coord: CoordI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;


  vpass()
}
//case class PlaceholderTemplataI[+T <: ITemplataType](
//  fullNameT: IdI[IPlaceholderNameI],
//  tyype: T
//) extends ITemplataI {
//  tyype match {
//    case CoordTemplataType() => vwat()
//    case KindTemplataType() => vwat()
//    case _ =>
//  }
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//}
case class KindTemplataI(kind: KindIT) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class RuntimeSizedArrayTemplateTemplataI() extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class StaticSizedArrayTemplateTemplataI() extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}



case class FunctionTemplataI(
  envId: IdI[FunctionTemplateNameI]
) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;



  def getTemplateName(): IdI[INameI] = vimpl()
}

case class StructDefinitionTemplataI(
  envId: IdI[StructTemplateNameI],
  tyype: TemplateTemplataType
) extends CitizenDefinitionTemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}

sealed trait CitizenDefinitionTemplataI extends ITemplataI

case class InterfaceDefinitionTemplataI(
  envId: IdI[InterfaceTemplateNameI],
  tyype: TemplateTemplataType
) extends CitizenDefinitionTemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}

case class ImplDefinitionTemplataI(
  envId: IdI[INameI]
//  // The paackage this interface was declared in.
//  // See TMRE for more on these environments.
//  env: IEnvironment,
////
////  // The containers are the structs/interfaces/impls/functions that this thing is inside.
////  // E.g. if LinkedList has a Node substruct, then the Node's templata will have one
////  // container, the LinkedList.
////  // See NTKPRR for why we have these parents.
////  containers: Vector[IContainer],
//
//  // This is the impl that the interface came from originally. It has all the parent
//  // structs and interfaces. See NTKPRR for more.
//  impl: ImplA
) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}

case class OwnershipTemplataI(ownership: OwnershipI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class VariabilityTemplataI(variability: VariabilityI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class MutabilityTemplataI(mutability: MutabilityI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class LocationTemplataI(location: LocationI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}

case class BooleanTemplataI(value: Boolean) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class IntegerTemplataI(value: Long) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class StringTemplataI(value: String) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class PrototypeTemplataI(declarationRange: RangeS, prototype: PrototypeI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class IsaTemplataI(declarationRange: RangeS, implName: IdI[IImplNameI], subKind: KindT, superKind: KindIT) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class CoordListTemplataI(coords: Vector[CoordI]) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  vpass()
}
case class RegionTemplataI(pureHeight: Int) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}

// ExternFunction/ImplTemplata are here because for example when we create an anonymous interface
// substruct, we want to add its forwarding functions and its impl to the environment, but it's
// very difficult to add the ImplA and FunctionA for those. So, we allow having coutputs like
// these directly in the environment.
// These should probably be renamed from Extern to something else... they could be supplied
// by plugins, but theyre also used internally.

case class ExternFunctionTemplataI(header: FunctionHeaderI) extends ITemplataI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
