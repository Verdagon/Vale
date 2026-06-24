use crate::instantiating::ast::types::{CoordI, KindIT, OwnershipI, MutabilityI, VariabilityI, LocationI};
use crate::instantiating::ast::ast::{FunctionHeaderI, PrototypeI};
use crate::instantiating::ast::names::{IdI, IImplNameI};
use crate::interner::StrI;
use crate::utils::range::RangeS;
use crate::postparsing::itemplatatype::TemplateTemplataType;
use crate::typing::types::types::KindT;
use std::marker::PhantomData;

/*
package dev.vale.instantiating.ast

import dev.vale.postparsing._
import dev.vale.typing.env._
import dev.vale.typing.names._
import dev.vale.typing.types._
import dev.vale.{RangeS, StrI, vassert, vfail, vimpl, vpass, vwat}
import dev.vale.highertyping._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.types._

import scala.collection.immutable.List


object ITemplataI {
*/
pub fn expect_coord<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> ITemplataI<'s, 'i, R> { panic!("Unimplemented: expect_coord"); }
/*
  def expectCoord[R <: IRegionsModeI](templata: ITemplataI[R]): ITemplataI[R] = {
    templata match {
      case t @ CoordTemplataI(_, _) => t
      case other => vfail(other)
    }
  }
*/
pub fn expect_coord_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> CoordTemplataI<'s, 'i, R> {
    match templata {
        ITemplataI::Coord(t) => t,
        _ => panic!("expect_coord_templata: not a CoordTemplataI"),
    }
}
/*
  def expectCoordTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): CoordTemplataI[R] = {
    templata match {
      case t @ CoordTemplataI(_, _) => t
      case other => vfail(other)
    }
  }
*/
pub fn expect_integer_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> IntegerTemplataI<R> {
    match templata {
        ITemplataI::Integer(t) => t,
        _ => panic!("vfail"),
    }
}
/*
  def expectIntegerTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): IntegerTemplataI[R] = {
    templata match {
      case t @ IntegerTemplataI(_) => t
      case _ => vfail()
    }
  }
*/
pub fn expect_mutability_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> MutabilityTemplataI<R> {
    match templata {
        ITemplataI::Mutability(m) => m,
        _ => panic!("expect_mutability_templata: not a MutabilityTemplataI"),
    }
}
/*
  def expectMutabilityTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): MutabilityTemplataI[R] = {
    templata match {
      case t @ MutabilityTemplataI(_) => t
      case _ => vfail()
    }
  }
*/
pub fn expect_variability_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> VariabilityTemplataI<R> {
    match templata {
        ITemplataI::Variability(t) => t,
        _ => panic!("expect_variability_templata: not a VariabilityTemplataI"),
    }
}
/*
  def expectVariabilityTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): VariabilityTemplataI[R] = {
    templata match {
      case t @ VariabilityTemplataI(_) => t
      case _ => vfail()
    }
  }
*/
pub fn expect_kind<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> ITemplataI<'s, 'i, R> { panic!("Unimplemented: expect_kind"); }
/*
  def expectKind[R <: IRegionsModeI](templata: ITemplataI[R]): ITemplataI[R] = {
    templata match {
      case t @ KindTemplataI(_) => t
      case _ => vfail()
    }
  }
*/
pub fn expect_kind_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> KindTemplataI<'s, 'i, R> { panic!("Unimplemented: expect_kind_templata"); }
/*
  def expectKindTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): KindTemplataI[R] = {
    templata match {
      case t @ KindTemplataI(_) => t
      case _ => vfail()
    }
  }
*/
pub fn expect_region_templata<'s, 'i, R>(templata: ITemplataI<'s, 'i, R>) -> RegionTemplataI<R> { panic!("Unimplemented: expect_region_templata"); }
/*
  def expectRegionTemplata[R <: IRegionsModeI](templata: ITemplataI[R]): RegionTemplataI[R] = {
    templata match {
      case t @ RegionTemplataI(_) => t
      case _ => vfail()
    }
  }

}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum ITemplataI<'s, 'i, R> {
  Coord(CoordTemplataI<'s, 'i, R>),
  Kind(KindTemplataI<'s, 'i, R>),
  RuntimeSizedArrayTemplate(RuntimeSizedArrayTemplateTemplataI<R>),
  StaticSizedArrayTemplate(StaticSizedArrayTemplateTemplataI<R>),
  Function(FunctionTemplataI<'s, 'i, R>),
  StructDefinition(StructDefinitionTemplataI<'s, 'i, R>),
  InterfaceDefinition(InterfaceDefinitionTemplataI<'s, 'i, R>),
  ImplDefinition(ImplDefinitionTemplataI<'s, 'i, R>),
  Ownership(OwnershipTemplataI<R>),
  Variability(VariabilityTemplataI<R>),
  Mutability(MutabilityTemplataI<R>),
  Location(LocationTemplataI<R>),
  Boolean(BooleanTemplataI<R>),
  Integer(IntegerTemplataI<R>),
  String(StringTemplataI<'s, R>),
  Prototype(PrototypeTemplataI<'s, 'i, R>),
  Isa(IsaTemplataI<'s, 'i, R>),
  CoordList(CoordListTemplataI<'s, 'i, R>),
  Region(RegionTemplataI<R>),
  ExternFunction(ExternFunctionTemplataI<'s, 'i, R>),
}
/*
sealed trait ITemplataI[+R <: IRegionsModeI] {
*/
impl<'s, 'i, R> ITemplataI<'s, 'i, R> {
  pub fn expect_coord_templata(&self) -> CoordTemplataI<'s, 'i, R> { panic!("Unimplemented: expect_coord_templata"); }
/*
  def expectCoordTemplata(): CoordTemplataI[R] = {
    this match {
      case c@CoordTemplataI(_, _) => c
      case other => vwat(other)
    }
  }
*/
  pub fn expect_region_templata(&self) -> RegionTemplataI<R> { panic!("Unimplemented: expect_region_templata"); }
}
/*
  def expectRegionTemplata(): RegionTemplataI[R] = {
    this match {
      case c@RegionTemplataI(_) => c
      case other => vwat(other)
    }
  }
}

//// The typing phase never makes one of these, they're purely abstract and conceptual in the
//// typing phase. The monomorphizer is the one that actually makes these templatas.
//case class RegionTemplataI[+R <: IRegionsModeI](pureHeight: Int) extends ITemplataI[R] {
//  vpass()
//  val hash = runtime.ScalaRunTime._hashCode(this);
//override def hashCode(): Int = hash;
//
//}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct CoordTemplataI<'s, 'i, R> {
  pub region: RegionTemplataI<R>,
  pub coord: CoordI<'s, 'i, R>,
}
/*
case class CoordTemplataI[+R <: IRegionsModeI](
    region: RegionTemplataI[R],
    coord: CoordI[R]
) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

  this match {
    case CoordTemplataI(RegionTemplataI(-1), CoordI(ImmutableShareI, StrIT())) => {
      vpass()
    }
    case _ =>
  }

  vpass()
}
//case class PlaceholderTemplataI[+T <: ITemplataType](
//  fullNameT: IdI[R, IPlaceholderNameI],
//  tyype: T
//) extends ITemplataI[R] {
//  tyype match {
//    case CoordTemplataType() => vwat()
//    case KindTemplataType() => vwat()
//    case _ =>
//  }
//  val hash = runtime.ScalaRunTime._hashCode(this);
//override def hashCode(): Int = hash;
//}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct KindTemplataI<'s, 'i, R> {
  pub kind: KindIT<'s, 'i, R>,
}
/*
case class KindTemplataI[+R <: IRegionsModeI](kind: KindIT[R]) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct RuntimeSizedArrayTemplateTemplataI<R> {
  pub _marker: PhantomData<R>,
}
/*
case class RuntimeSizedArrayTemplateTemplataI[+R <: IRegionsModeI]() extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct StaticSizedArrayTemplateTemplataI<R> {
  pub _marker: PhantomData<R>,
}
/*
case class StaticSizedArrayTemplateTemplataI[+R <: IRegionsModeI]() extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}



*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct FunctionTemplataI<'s, 'i, R> {
  pub env_id: IdI<'s, 'i, R>,
}
/*
case class FunctionTemplataI[+R <: IRegionsModeI](
  envId: IdI[R, FunctionTemplateNameI[R]]
) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;


*/
impl<'s, 'i, R> FunctionTemplataI<'s, 'i, R> {
  pub fn get_template_name(&self) -> IdI<'s, 'i, R> { panic!("Unimplemented: get_template_name"); }
}
/*
  def getTemplateName(): IdI[R, INameI[R]] = vimpl()
}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct StructDefinitionTemplataI<'s, 'i, R> {
  pub env_id: IdI<'s, 'i, R>,
  pub tyype: TemplateTemplataType<'s>,
}
/*
case class StructDefinitionTemplataI[+R <: IRegionsModeI](
  envId: IdI[R, StructTemplateNameI[R]],
  tyype: TemplateTemplataType
) extends CitizenDefinitionTemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;
}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum CitizenDefinitionTemplataI<'s, 'i, R> {
  Struct(StructDefinitionTemplataI<'s, 'i, R>),
  Interface(InterfaceDefinitionTemplataI<'s, 'i, R>),
}
/*
sealed trait CitizenDefinitionTemplataI[+R <: IRegionsModeI] extends ITemplataI[R]

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct InterfaceDefinitionTemplataI<'s, 'i, R> {
  pub env_id: IdI<'s, 'i, R>,
  pub tyype: TemplateTemplataType<'s>,
}
/*
case class InterfaceDefinitionTemplataI[+R <: IRegionsModeI](
  envId: IdI[R, InterfaceTemplateNameI[R]],
  tyype: TemplateTemplataType
) extends CitizenDefinitionTemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;
}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct ImplDefinitionTemplataI<'s, 'i, R> {
  pub env_id: IdI<'s, 'i, R>,
}
/*
case class ImplDefinitionTemplataI[+R <: IRegionsModeI](
  envId: IdI[R, INameI[R]]
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
) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct OwnershipTemplataI<R> {
  pub ownership: OwnershipI,
  pub _marker: PhantomData<R>,
}
/*
case class OwnershipTemplataI[+R <: IRegionsModeI](ownership: OwnershipI) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct VariabilityTemplataI<R> {
  pub variability: VariabilityI,
  pub _marker: PhantomData<R>,
}
/*
case class VariabilityTemplataI[+R <: IRegionsModeI](variability: VariabilityI) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct MutabilityTemplataI<R> {
  pub mutability: MutabilityI,
  pub _marker: PhantomData<R>,
}
/*
case class MutabilityTemplataI[+R <: IRegionsModeI](mutability: MutabilityI) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct LocationTemplataI<R> {
  pub location: LocationI,
  pub _marker: PhantomData<R>,
}
/*
case class LocationTemplataI[+R <: IRegionsModeI](location: LocationI) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}

*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct BooleanTemplataI<R> {
  pub value: bool,
  pub _marker: PhantomData<R>,
}
/*
case class BooleanTemplataI[+R <: IRegionsModeI](value: Boolean) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct IntegerTemplataI<R> {
  pub value: i64,
  pub _marker: PhantomData<R>,
}
/*
case class IntegerTemplataI[+R <: IRegionsModeI](value: Long) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct StringTemplataI<'s, R> {
  pub value: StrI<'s>,
  pub _marker: PhantomData<R>,
}
/*
case class StringTemplataI[+R <: IRegionsModeI](value: String) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct PrototypeTemplataI<'s, 'i, R> {
  pub declaration_range: RangeS<'s>,
  pub prototype: &'i PrototypeI<'s, 'i, R>,
}
/*
case class PrototypeTemplataI[+R <: IRegionsModeI](declarationRange: RangeS, prototype: PrototypeI[R]) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IsaTemplataI<'s, 'i, R> {
  pub declaration_range: RangeS<'s>,
  pub impl_name: IdI<'s, 'i, R>,
  pub sub_kind: KindIT<'s, 'i, R>,
  pub super_kind: KindIT<'s, 'i, R>,
}
/*
case class IsaTemplataI[+R <: IRegionsModeI](declarationRange: RangeS, implName: IdI[R, IImplNameI[R]], subKind: KindIT[R], superKind: KindIT[R]) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct CoordListTemplataI<'s, 'i, R> {
  pub coords: &'i[CoordI<'s, 'i, R>],
}
/*
case class CoordListTemplataI[+R <: IRegionsModeI](coords: Vector[CoordI[R]]) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

  vpass()
}
*/
/// Polyvalue
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct RegionTemplataI<R> {
  pub pure_height: i32,
  pub _marker: PhantomData<R>,
}
/*
case class RegionTemplataI[+R <: IRegionsModeI](pureHeight: Int) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}

// ExternFunction/ImplTemplata are here because for example when we create an anonymous interface
// substruct, we want to add its forwarding functions and its impl to the environment, but it's
// very difficult to add the ImplA and FunctionA for those. So, we allow having coutputs like
// these directly in the environment.
// These should probably be renamed from Extern to something else... they could be supplied
// by plugins, but theyre also used internally.

*/
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ExternFunctionTemplataI<'s, 'i, R> {
  pub header: &'i FunctionHeaderI<'s, 'i>,
  pub _marker: PhantomData<R>,
}
/*
case class ExternFunctionTemplataI[+R <: IRegionsModeI](header: FunctionHeaderI) extends ITemplataI[R] {
  val hash = runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = hash;

}
*/
