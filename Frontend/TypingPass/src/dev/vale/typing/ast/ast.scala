package dev.vale.typing.ast

import dev.vale.highertyping.FunctionA
import dev.vale.typing.names.{CitizenTemplateNameT, FullNameT, ICitizenTemplateNameT, IFunctionNameT, IFunctionTemplateNameT, IInterfaceTemplateNameT, IStructTemplateNameT, IVarNameT, InterfaceTemplateNameT}
import dev.vale.typing.templata.FunctionTemplata
import dev.vale.{PackageCoordinate, RangeS, vassert, vcurious, vfail}
import dev.vale.typing.types._
import dev.vale._
import dev.vale.postparsing.{IRuneS, ITemplataType}
import dev.vale.typing._
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable._

// We won't always have a return type for a banner... it might have not specified its return
// type, so we're currently evaluating the entire body for it right now.
// If we ever find ourselves wanting the return type for a banner, we need to:
// - Check if it's in the returnTypesByBanner map. If so, good.
// - If not, then check if the banner is in declaredBanners. If so, then we're currently in
//   the process of evaluating the entire body. In this case, throw an error because we're
//   about to infinite loop. Hopefully this is a user error, they need to specify a return
//   type to avoid a cyclical definition.
// - If not in declared banners, then tell FunctionCompiler to start evaluating it.

case class ImplT(
  // These are ICitizenTT and InterfaceTT which likely have placeholder templatas in them.
  // We do this because a struct might implement an interface in multiple ways, see SCIIMT.
  // We have the template names as well as the placeholders for better searching, see MLUIBTN.

  templata: ImplTemplata,

  subCitizenTemplateName: FullNameT[ICitizenTemplateNameT],
//  // Starting from a placeholdered sub citizen, this is the interface that would result.
//  // We get this by solving the impl, given a placeholdered sub citizen.
//  parentInterfaceFromPlaceholderedSubCitizen: InterfaceTT,

  superInterfaceTemplateName: FullNameT[IInterfaceTemplateNameT],
//  // Starting from a placeholdered super interface, this is the interface that would result.
//  // We get this by solving the impl, given a placeholdered sub citizen.
//  subCitizenFromPlaceholderedParentInterface: ICitizenTT,
) extends IInterning

case class KindExportT(
  range: RangeS,
  tyype: KindT,
  packageCoordinate: PackageCoordinate,
  exportedName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class FunctionExportT(
  range: RangeS,
  prototype: PrototypeT,
  packageCoordinate: PackageCoordinate,
  exportedName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class KindExternT(
  tyype: KindT,
  packageCoordinate: PackageCoordinate,
  externName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class FunctionExternT(
  range: RangeS,
  prototype: PrototypeT,
  packageCoordinate: PackageCoordinate,
  externName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class InterfaceEdgeBlueprint(
  interface: FullNameT[IInterfaceTemplateNameT],
  superFamilyRootHeaders: Vector[FunctionHeaderT]) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

case class EdgeT(
    struct: FullNameT[ICitizenTemplateNameT],
    interface: FullNameT[IInterfaceTemplateNameT],
    methods: Vector[PrototypeT]) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  override def equals(obj: Any): Boolean = {
    obj match {
      case EdgeT(thatStruct, thatInterface, _) => {
        struct == thatStruct && interface == thatInterface
      }
    }
  }
}

object ProgramT {
//  val emptyTupleTT =
//    StructTT(FullNameT(PackageCoordinate.BUILTIN, Vector(), CitizenNameT(CitizenTemplateNameT(tupleHumanName), Vector(CoordListTemplata(Vector())))))

  val intType = CoordT(ShareT, IntT.i32)
  val boolType = CoordT(ShareT, BoolT())
}

case class FunctionT(
  header: FunctionHeaderT,
  body: ReferenceExpressionTE)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  // We always end a function with a ret, whose result is a Never.
  vassert(body.result.kind == NeverT(false))
}

object getFunctionLastName {
  def unapply(f: FunctionT): Option[IFunctionTemplateNameT] = Some(f.header.fullName.last)
}

// A unique location in a function. Environment is in the name so it spells LIFE!
case class LocationInFunctionEnvironment(path: Vector[Int]) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  def +(subLocation: Int): LocationInFunctionEnvironment = {
    LocationInFunctionEnvironment(path :+ subLocation)
  }

  override def toString: String = path.mkString(".")
}

case class AbstractT()

case class ParameterT(
  name: IVarNameT,
  virtuality: Option[AbstractT],
  tyype: CoordT)  {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  // Use same instead, see EHCFBD for why we dont like equals.
  override def equals(obj: Any): Boolean = vcurious();

  def same(that: ParameterT): Boolean = {
    name == that.name &&
      virtuality == that.virtuality &&
      tyype == that.tyype
  }
}

sealed trait ICalleeCandidate

case class FunctionCalleeCandidate(ft: FunctionTemplata) extends ICalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
case class HeaderCalleeCandidate(header: FunctionHeaderT, templateArgs: Vector[ITemplata[ITemplataType]]) extends ICalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
case class PrototypeTemplataCalleeCandidate(range: RangeS, prototypeT: PrototypeT) extends ICalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}

sealed trait IValidCalleeCandidate {
  def range: Option[RangeS]
  def paramTypes: Array[CoordT]
}
case class ValidHeaderCalleeCandidate(
  header: FunctionHeaderT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IValidCalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

  override def range: Option[RangeS] = header.maybeOriginFunction.map(_.range)
  override def paramTypes: Array[CoordT] = header.paramTypes.toArray
}
case class ValidPrototypeTemplataCalleeCandidate(
  prototype: PrototypeTemplata
) extends IValidCalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

  override def range: Option[RangeS] = Some(prototype.declarationRange)
  override def paramTypes: Array[CoordT] = prototype.prototype.fullName.last.parameters.toArray
}
case class ValidCalleeCandidate(
  banner: FunctionHeaderT,
  templateArgs: Vector[ITemplata[ITemplataType]],
  function: FunctionTemplata
) extends IValidCalleeCandidate {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

  override def range: Option[RangeS] = banner.maybeOriginFunction.map(_.range)
  override def paramTypes: Array[CoordT] = banner.paramTypes.toArray
}

// A "signature" is just the things required for overload resolution, IOW function name and arg types.

// An autograph could be a super signature; a signature plus attributes like virtual and mutable.
// If we ever need it, a "schema" could be something.

// A FunctionBanner2 is everything in a FunctionHeader2 minus the return type.
// These are only made by the FunctionCompiler, to signal that it's currently being
// evaluated or it's already been evaluated.
// It's easy to see all possible function banners, but not easy to see all possible
// function headers, because functions don't have to specify their return types and
// it takes a complete typingpass evaluate to deduce a function's return type.

case class SignatureT(fullName: FullNameT[IFunctionNameT]) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def paramTypes: Vector[CoordT] = fullName.last.parameters
}

case class FunctionBannerT(
  originFunction: Option[FunctionA],
  templateName: FullNameT[IFunctionTemplateNameT])   {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  // Use same instead, see EHCFBD for why we dont like equals.
  override def equals(obj: Any): Boolean = vcurious();

  def same(that: FunctionBannerT): Boolean = {
    originFunction == that.originFunction &&
      templateName == that.templateName
  }



//  def unapply(arg: FunctionBannerT):
//  Option[(FullNameT[IFunctionNameT], Vector[ParameterT])] =
//    Some(templateName, params)

  override def toString: String = {
    // # is to signal that we override this
    "FunctionBanner2#(" + templateName + ")"
    //    "FunctionBanner2#(" + templateName + ", " + params + ")"
  }
}

sealed trait IFunctionAttributeT
sealed trait ICitizenAttributeT
case class ExternT(packageCoord: PackageCoordinate) extends IFunctionAttributeT with ICitizenAttributeT { // For optimization later
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
// There's no Export2 here, we use separate KindExport and FunctionExport constructs.
//case class Export2(packageCoord: PackageCoordinate) extends IFunctionAttribute2 with ICitizenAttribute2
case object PureT extends IFunctionAttributeT
case object SealedT extends ICitizenAttributeT
case object UserFunctionT extends IFunctionAttributeT // Whether it was written by a human. Mostly for tests right now.

case class FunctionHeaderT(
  fullName: FullNameT[IFunctionTemplateNameT],
  attributes: Vector[IFunctionAttributeT],
  params: Vector[ParameterT],
  returnType: CoordT,
  maybeOriginFunction: Option[FunctionA])  {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  override def equals(obj: Any): Boolean = {
    obj match {
      case FunctionHeaderT(thatName, _, _, _, _) => {
        fullName == thatName
      }
      case _ => false
    }
  }

  // Make sure there's no duplicate names
  vassert(params.map(_.name).toSet.size == params.size);

//  vassert(fullName.last.parameters == paramTypes)

  def isExtern = attributes.exists({ case ExternT(_) => true case _ => false })
  //  def isExport = attributes.exists({ case Export2(_) => true case _ => false })
  def isUserFunction = attributes.contains(UserFunctionT)
//  def getAbstractInterface: Option[InterfaceTT] = toBanner.getAbstractInterface
////  def getOverride: Option[(StructTT, InterfaceTT)] = toBanner.getOverride
//  def getVirtualIndex: Option[Int] = toBanner.getVirtualIndex

//  def toSignature(interner: Interner, keywords: Keywords): SignatureT = {
//    val newLastStep = templateName.last.makeFunctionName(interner, keywords, templateArgs, params)
//    val fullName = FullNameT(templateName.packageCoord, name.initSteps, newLastStep)
//
//    SignatureT(fullName)
//
//  }
//  def paramTypes: Vector[CoordT] = params.map(_.tyype)

  def getAbstractInterface: Option[InterfaceTT] = {
    val abstractInterfaces =
      params.collect({
        case ParameterT(_, Some(AbstractT()), CoordT(_, ir @ InterfaceTT(_))) => ir
      })
    vassert(abstractInterfaces.size <= 1)
    abstractInterfaces.headOption
  }

  def getVirtualIndex: Option[Int] = {
    val indices =
      params.zipWithIndex.collect({
        case (ParameterT(_, Some(AbstractT()), _), index) => index
      })
    vassert(indices.size <= 1)
    indices.headOption
  }

//  maybeOriginFunction.foreach(originFunction => {
//    if (originFunction.genericParameters.size != fullName.last.templateArgs.size) {
//      vfail("wtf m8")
//    }
//  })

  def toBanner: FunctionBannerT = FunctionBannerT(maybeOriginFunction, fullName)
  def toPrototype(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]]): PrototypeT = {
    val substituter = TemplataCompiler.getPlaceholderSubstituter(interner, fullName, templateArgs)
    val paramTypes = params.map(_.tyype).map(substituter.substituteForCoord)
    val newLastStep = fullName.last.makeFunctionName(interner, keywords, templateArgs, paramTypes)
    val newName = FullNameT(fullName.packageCoord, fullName.initSteps, newLastStep)
    PrototypeT(newName, returnType)
  }
  def toSignature(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]]): SignatureT = {
    toPrototype(interner, keywords, templateArgs).toSignature
  }

  def paramTypes: Vector[CoordT] = params.map(_.tyype)

  def unapply(arg: FunctionHeaderT): Option[(FullNameT[IFunctionTemplateNameT], Vector[ParameterT], CoordT)] = {
    Some(fullName, params, returnType)
  }
}

case class PrototypeT(
    fullName: FullNameT[IFunctionNameT],
    returnType: CoordT)  {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def paramTypes: Vector[CoordT] = fullName.last.parameters
  def toSignature: SignatureT = SignatureT(fullName)
}
