package dev.vale.instantiating.ast

import dev.vale._
import dev.vale.postparsing._

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

case class ImplI(
  // These are ICitizenTI and InterfaceIT which likely have placeholder templatas in them.
  // We do this because a struct might implement an interface in multiple ways, see SCIIMT.
  // We have the template names as well as the placeholders for better searching, see MLUIBTN.

  templata: ImplDefinitionTemplataI,

  instantiatedId: IdI[IImplNameI],
  templateId: IdI[IImplTemplateNameI],

  subCitizenTemplateId: IdI[ICitizenTemplateNameI],
  subCitizen: ICitizenIT,

  superInterface: InterfaceIT,
  superInterfaceTemplateId: IdI[IInterfaceTemplateNameI],

  // This is similar to FunctionT.runeToFuncBound
  runeToFuncBound: Map[IRuneS, IdI[FunctionBoundNameI]],
  runeToImplBound: Map[IRuneS, IdI[ImplBoundNameI]],

  runeIndexToIndependence: Vector[Boolean],

  // A function will inherit bounds from its parameters' kinds. Same with an impl from its sub
  // citizen, and a case block from its receiving kind.
  // We'll need to remember those, so the instantiator can do its thing.
  // See TIBANFC for more.
  reachableBoundsFromSubCitizen: Vector[PrototypeI]

//  // Starting from a placeholdered super interface, this is the interface that would result.
//  // We get this by solving the impl, given a placeholdered sub citizen.
//  subCitizenFromPlaceholderedParentInterface: ICitizenIT,
)

case class KindExportI(
  range: RangeS,
  tyype: KindIT,
  // Good for knowing the package of this export for later prefixing the exportedName, also good
  // for getting its region.
  id: IdI[ExportNameI],
  exportedName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class FunctionExportI(
  range: RangeS,
  prototype: PrototypeI,
  exportId: IdI[ExportNameI],
  exportedName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vpass()
}

case class KindExternI(
  tyype: KindIT,
  packageCoordinate: PackageCoordinate,
  externName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class FunctionExternI(
  range: RangeS,
  prototype: PrototypeI,
  packageCoordinate: PackageCoordinate,
  externName: StrI
)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

}

case class InterfaceEdgeBlueprintI(
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  interface: IdI[IInterfaceNameI],
  superFamilyRootHeaders: Vector[(PrototypeI, Int)]) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

case class OverrideI(
  // it seems right here we'll need some sort of mapping of abstract func placeholder to the
  // override impl case placeholders, and perhaps also the existence of the <T>s for the case?
  // we need to instantiate the override, so its going to need some values for it... i guess
  // its from the impl, so the impl has it i think. so maybe a map from the impl rune to it



  // This is the name of the conceptual function called by the abstract function.
  // It has enough information to do simple dispatches, but not all cases, it can't handle
  // the Milano case, see OMCNAGP.
  // This will have some placeholders from the abstract function; this is the abstract function
  // calling the dispatcher.
  // This is like:
  //   abstract func send<T>(self &IObserver<T>, event I) void
  // calling:
  //   func add<int>(self &IObserver<int>, event int) void
  // or a more complex case:
  //   func add<Opt<int>>(self &IObserver<Opt<int>>, event Opt<int>) void
  // as you can see there may be some interesting templatas in there like that Opt<int>, they
  // might not be simple placeholders
  dispatcherCallId: IdI[OverrideDispatcherNameI],

  implPlaceholderToDispatcherPlaceholder: Vector[(IdI[IPlaceholderNameI], ITemplataI)],
  implPlaceholderToCasePlaceholder: Vector[(IdI[IPlaceholderNameI], ITemplataI)],

  // This is needed for bringing in the impl's bound args for the override dispatcher's case, see
  // TIBANFC.
  implSubCitizenReachableBoundsToCaseSubCitizenReachableBounds: Map[IdI[FunctionBoundNameI], IdI[FunctionBoundNameI]],

  // Any FunctionI has a runeToFunctionBound, which is a map of the function's rune to its required
  // bounds. This is the one for our conceptual dispatcher function.
  dispatcherRuneToFunctionBound: Map[IRuneS, IdI[FunctionBoundNameI]],
  dispatcherRuneToImplBound: Map[IRuneS, IdI[ImplBoundNameI]],

  // This is the name of the conceptual case that's calling the override prototype. It'll have
  // template args inherited from the dispatcher function and template args inherited from the
  // impl. After typing pass these will be placeholders, and after instantiator these will be
  // actual real templatas.
  // This will have some placeholders from the impl; this is the impl calling the case, kind of.
  caseId: IdI[OverrideDispatcherCaseNameI],

  // The override function we're calling.
  // Conceptually, this is being called from the case's environment. It might even have some complex stuff
  // in the template args.
  overridePrototype: PrototypeI
)

case class EdgeI(
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  edgeId: IdI[IImplNameI],
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  subCitizen: ICitizenIT,
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  superInterface: IdI[IInterfaceNameI],
  // This is similar to FunctionT.runeToFuncBound
  runeToFuncBound: Map[IRuneS, IdI[FunctionBoundNameI]],
  runeToImplBound: Map[IRuneS, IdI[ImplBoundNameI]],
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  abstractFuncToOverrideFunc: Map[IdI[IFunctionNameI], OverrideI]
) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  override def equals(obj: Any): Boolean = {
    obj match {
      case EdgeI(thatEdgeFullName, thatStruct, thatInterface, _, _, _) => {
        val isSame = subCitizen == thatStruct && superInterface == thatInterface
        if (isSame) {
          vassert(edgeId == thatEdgeFullName)
        }
        isSame
      }
    }
  }
}

case class FunctionDefinitionI(
  header: FunctionHeaderI,
  runeToFuncBound: Map[IRuneS, IdI[FunctionBoundNameI]],
  runeToImplBound: Map[IRuneS, IdI[ImplBoundNameI]],
  body: ReferenceExpressionIE)  {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  // We always end a function with a ret, whose result is a Never.
  vassert(body.result.kind == NeverIT(false))

  def isPure: Boolean = header.isPure
}

object getFunctionLastName {
  def unapply(f: FunctionDefinitionI): Option[IFunctionNameI] = Some(f.header.id.localName)
}

// A unique location in a function. Environment is in the name so it spells LIFE!
case class LocationInFunctionEnvironment(path: Vector[Int]) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  def +(subLocation: Int): LocationInFunctionEnvironment = {
    LocationInFunctionEnvironment(path :+ subLocation)
  }

  override def toString: String = path.mkString(".")
}

case class AbstractI()

case class ParameterI(
  name: IVarNameI,
  virtuality: Option[AbstractI],
  tyype: CoordI)  {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  // Use same instead, see EHCFBD for why we dont like equals.
  override def equals(obj: Any): Boolean = vcurious();

  def same(that: ParameterI): Boolean = {
    name == that.name &&
      virtuality == that.virtuality &&
      tyype == that.tyype
  }
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

case class SignatureI(id: IdI[IFunctionNameI]) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def paramTypes: Vector[CoordI] = id.localName.parameters
}

sealed trait IFunctionAttributeI
sealed trait ICitizenAttributeI
case class ExternI(packageCoord: PackageCoordinate) extends IFunctionAttributeI with ICitizenAttributeI { // For optimization later
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
// There's no Export2 here, we use separate KindExport and FunctionExport constructs.
//case class Export2(packageCoord: PackageCoordinate) extends IFunctionAttribute2 with ICitizenAttribute2
case object PureI extends IFunctionAttributeI
case object SealedI extends ICitizenAttributeI
case object UserFunctionI extends IFunctionAttributeI // Whether it was written by a human. Mostly for tests right now.

case class RegionI(
  name: IRegionNameI,
  mutable: Boolean)

case class FunctionHeaderI(
  // This one little name field can illuminate much of how the compiler works, see UINIT.
  id: IdI[IFunctionNameI],
  attributes: Vector[IFunctionAttributeI],
//  regions: Vector[RegionI],
  params: Vector[ParameterI],
  returnType: CoordI) {

  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

//  val perspectiveRegion =
//    id.localName.templateArgs.last match {
//      case PlaceholderTemplata(IdI(packageCoord, initSteps, r @ RegionPlaceholderNameI(_, _, _, _, _)), RegionTemplataType()) => {
//        IdI(packageCoord, initSteps, r)
//      }
//      case _ => vwat()
//    }
//  if (attributes.contains(PureI)) {
//    // Instantiator relies on this assumption so that it knows when certain things are pure.
//    vassert(perspectiveRegion.localName.originalMaybeNearestPureLocation == Some(LocationInDenizen(Vector())))
//  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case FunctionHeaderI(thatName, _, _, _) => {
        id == thatName
      }
      case _ => false
    }
  }

  // Make sure there's no duplicate names
  vassert(params.map(_.name).toSet.size == params.size);

  vassert(id.localName.parameters == paramTypes)

  def isExtern = attributes.exists({ case ExternI(_) => true case _ => false })
  //  def isExport = attributes.exists({ case Export2(_) => true case _ => false })
  def isUserFunction = attributes.contains(UserFunctionI)
//  def getAbstractInterface: Option[InterfaceIT] = toBanner.getAbstractInterface
////  def getOverride: Option[(StructIT, InterfaceIT)] = toBanner.getOverride
//  def getVirtualIndex: Option[Int] = toBanner.getVirtualIndex

//  def toSignature(interner: Interner, keywords: Keywords): SignatureI = {
//    val newLastStep = templateName.last.makeFunctionName(interner, keywords, templateArgs, params)
//    val fullName = FullNameI(templateName.packageCoord, name.initSteps, newLastStep)
//
//    SignatureI(fullName)
//
//  }
//  def paramTypes: Vector[CoordI] = params.map(_.tyype)

  def getAbstractInterface: Option[InterfaceIT] = {
    val abstractInterfaces =
      params.collect({
        case ParameterI(_, Some(AbstractI()), CoordI(_, ir @ InterfaceIT(_))) => ir
      })
    vassert(abstractInterfaces.size <= 1)
    abstractInterfaces.headOption
  }

  def getVirtualIndex: Option[Int] = {
    val indices =
      params.zipWithIndex.collect({
        case (ParameterI(_, Some(AbstractI()), _), index) => index
      })
    vassert(indices.size <= 1)
    indices.headOption
  }

//  maybeOriginFunction.foreach(originFunction => {
//    if (originFunction.genericParameters.size != fullName.last.templateArgs.size) {
//      vfail("wtf m8")
//    }
//  })

  def toPrototype: PrototypeI = {
//    val substituter = TemplataCompiler.getPlaceholderSubstituter(interner, fullName, templateArgs)
//    val paramTypes = params.map(_.tyype).map(substituter.substituteForCoord)
//    val newLastStep = fullName.last.makeFunctionName(interner, keywords, templateArgs, paramTypes)
//    val newName = FullNameI(fullName.packageCoord, fullName.initSteps, newLastStep)
    PrototypeI(id, returnType)
  }
  def toSignature: SignatureI = {
    toPrototype.toSignature
  }

  def paramTypes: Vector[CoordI] = id.localName.parameters

  def unapply(arg: FunctionHeaderI): Option[(IdI[IFunctionNameI], Vector[ParameterI], CoordI)] = {
    Some(id, params, returnType)
  }

  def isPure: Boolean = {
    attributes.collectFirst({ case PureI => }).nonEmpty
  }
}

case class PrototypeI(
    id: IdI[IFunctionNameI],
    returnType: CoordI) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def paramTypes: Vector[CoordI] = id.localName.parameters
  def toSignature: SignatureI = SignatureI(id)
}


sealed trait IVariableI  {
  def name: IVarNameI
  def variability: VariabilityI
  def coord: CoordI
}
sealed trait ILocalVariableI extends IVariableI {
  def name: IVarNameI
  def coord: CoordI
}
// Why the difference between reference and addressible:
// If we mutate/move a variable from inside a closure, we need to put
// the local's address into the struct. But, if the closures don't
// mutate/move, then we could just put a regular reference in the struct.
// Lucky for us, the parser figured out if any of our child closures did
// any mutates/moves/borrows.
case class AddressibleLocalVariableI(
  name: IVarNameI,
  variability: VariabilityI,
  coord: CoordI
) extends ILocalVariableI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

}
case class ReferenceLocalVariableI(
  name: IVarNameI,
  variability: VariabilityI,
  coord: CoordI
) extends ILocalVariableI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();
  vpass()
}
case class AddressibleClosureVariableI(
  name: IVarNameI,
  closuredVarsStructType: StructIT,
  variability: VariabilityI,
  coord: CoordI
) extends IVariableI {
  vpass()
}
case class ReferenceClosureVariableI(
  name: IVarNameI,
  closuredVarsStructType: StructIT,
  variability: VariabilityI,
  coord: CoordI
) extends IVariableI {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

}
