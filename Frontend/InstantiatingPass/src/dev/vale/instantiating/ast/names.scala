package dev.vale.instantiating.ast

import dev.vale._
import dev.vale.instantiating.ast.ITemplataI._
import dev.vale.postparsing._

// Scout's/Astronomer's name parts correspond to where they are in the source code,
// but Compiler's correspond more to what packages and stamped functions / structs
// they're in. See TNAD.

case class IdI[+I <: INameI](
  packageCoord: PackageCoordinate,
  initSteps: Vector[INameI],
  localName: I
)  {

  // Placeholders should only be the last name, getPlaceholdersInKind assumes it
  initSteps.foreach({
    case KindPlaceholderNameI(_) => vfail()
    case KindPlaceholderTemplateNameI(_, _) => vfail()
    case _ =>
  })
  // Placeholders are under the template name.
  // There's really no other way; we make the placeholders before knowing the function's
  // instantated name.
  localName match {
    case KindPlaceholderNameI(_) => {
      initSteps.last match {
        case _ : ITemplateNameI =>
        case OverrideDispatcherNameI(_, _, _) => {
          initSteps.init.last match {
            case _ : ITemplateNameI =>
            case other => vfail(other)
          }
        }
        case other => vfail(other)
      }
    }
    case _ =>
  }

  // PackageTopLevelName2 is just here because names have to have a last step.
  vassert(initSteps.collectFirst({ case PackageTopLevelNameI() => }).isEmpty)

  vcurious(initSteps.distinct == initSteps)

  override def equals(obj: Any): Boolean = {
    obj match {
      case IdI(thatPackageCoord, thatInitSteps, thatLast) => {
        packageCoord == thatPackageCoord && initSteps == thatInitSteps && localName == thatLast
      }
      case _ => false
    }
  }

  def packageFullName: IdI[PackageTopLevelNameI] = {
    IdI(packageCoord, Vector(), PackageTopLevelNameI())
  }

  def initFullName: IdI[INameI] = {
    if (initSteps.isEmpty) {
      IdI(packageCoord, Vector(), PackageTopLevelNameI())
    } else {
      IdI(packageCoord, initSteps.init, initSteps.last)
    }
  }

  def initNonPackageFullName(): Option[IdI[INameI]] = {
    if (initSteps.isEmpty) {
      None
    } else {
      Some(IdI(packageCoord, initSteps.init, initSteps.last))
    }
  }

  def steps: Vector[INameI] = {
    localName match {
      case PackageTopLevelNameI() => initSteps
      case _ => initSteps :+ localName
    }
  }
  def addStep[Y <: INameI](newLast: Y): IdI[Y] = {
    IdI[Y](packageCoord, steps, newLast)
  }

  def mapLocal[Z <: INameI](func: I => Z): IdI[Z] = {
    IdI[Z](packageCoord, steps, func(localName))
  }
}

sealed trait INameI
sealed trait ITemplateNameI extends INameI
sealed trait IFunctionTemplateNameI extends ITemplateNameI {
//  def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI
}
sealed trait IInstantiationNameI extends INameI {
  def template: ITemplateNameI
  def templateArgs: Vector[ITemplataI]
}
sealed trait IFunctionNameI extends IInstantiationNameI {
  def template: IFunctionTemplateNameI
  def templateArgs: Vector[ITemplataI]
  def parameters: Vector[CoordI]
}
sealed trait ISuperKindTemplateNameI extends ITemplateNameI
sealed trait ISubKindTemplateNameI extends ITemplateNameI
sealed trait ICitizenTemplateNameI extends ISubKindTemplateNameI {
//  def makeCitizenName(templateArgs: Vector[ITemplataI]): ICitizenNameI
}
sealed trait IStructTemplateNameI extends ICitizenTemplateNameI {
//  def makeStructName(templateArgs: Vector[ITemplataI]): IStructNameI
//  override def makeCitizenName(templateArgs: Vector[ITemplataI]):
//  ICitizenNameI = {
//    makeStructName(templateArgs)
//  }
}
sealed trait IInterfaceTemplateNameI extends ICitizenTemplateNameI with ISuperKindTemplateNameI {
//  def makeInterfaceName(templateArgs: Vector[ITemplataI]): IInterfaceNameI
}
sealed trait ISuperKindNameI extends IInstantiationNameI {
  def template: ISuperKindTemplateNameI
  def templateArgs: Vector[ITemplataI]
}
sealed trait ISubKindNameI extends IInstantiationNameI {
  def template: ISubKindTemplateNameI
  def templateArgs: Vector[ITemplataI]
}
sealed trait ICitizenNameI extends ISubKindNameI {
  def template: ICitizenTemplateNameI
  def templateArgs: Vector[ITemplataI]
}
sealed trait IStructNameI extends ICitizenNameI with ISubKindNameI {
  override def template: IStructTemplateNameI
  override def templateArgs: Vector[ITemplataI]
}
sealed trait IInterfaceNameI extends ICitizenNameI with ISubKindNameI with ISuperKindNameI {
  override def template: InterfaceTemplateNameI
  override def templateArgs: Vector[ITemplataI]
}
sealed trait IImplTemplateNameI extends ITemplateNameI {
//  def makeImplName(templateArgs: Vector[ITemplataI], subCitizen: ICitizenIT): IImplNameI
}
sealed trait IImplNameI extends IInstantiationNameI {
  def template: IImplTemplateNameI
}

sealed trait IRegionNameI extends INameI

case class RegionNameI(rune: IRuneS) extends IRegionNameI
case class DenizenDefaultRegionNameI() extends IRegionNameI

case class ExportTemplateNameI(codeLoc: CodeLocationS) extends ITemplateNameI
case class ExportNameI(template: ExportTemplateNameI, region: ITemplataI) extends IInstantiationNameI {
  override def templateArgs: Vector[ITemplataI] = Vector(region)
}

case class ImplTemplateNameI(codeLocationS: CodeLocationS) extends IImplTemplateNameI {
  vpass()
//  override def makeImplName(templateArgs: Vector[ITemplataI], subCitizen: ICitizenIT): ImplNameI = {
//    ImplNameI(this, templateArgs, subCitizen)
//  }
}
case class ImplNameI(
  template: ImplTemplateNameI,
  templateArgs: Vector[ITemplataI],
  // The instantiator wants this so it can know the struct type up-front before monomorphizing the
  // whole impl, so it can hoist some bounds out of the struct, like NBIFP.
  subCitizen: ICitizenIT
) extends IImplNameI {
  vpass()
}

case class ImplBoundTemplateNameI(codeLocationS: CodeLocationS) extends IImplTemplateNameI {
//  override def makeImplName(templateArgs: Vector[ITemplataI], subCitizen: ICitizenIT): ImplBoundNameI = {
//    ImplBoundNameI(this, templateArgs)
//  }
}
case class ImplBoundNameI(
  template: ImplBoundTemplateNameI,
  templateArgs: Vector[ITemplataI]
) extends IImplNameI {

}

//// The name of an impl that is subclassing some interface. To find all impls subclassing an interface,
//// look for this name.
//case class ImplImplementingSuperInterfaceNameI(superInterface: FullNameI[IInterfaceTemplateNameI]) extends IImplTemplateNameI
//// The name of an impl that is augmenting some sub citizen. To find all impls subclassing an interface,
//// look for this name.
//case class ImplAugmentingSubCitizenNameI(subCitizen: FullNameI[ICitizenTemplateNameI]) extends IImplTemplateNameI

case class LetNameI(codeLocation: CodeLocationS) extends INameI
case class ExportAsNameI(codeLocation: CodeLocationS) extends INameI

case class RawArrayNameI(
  mutability: MutabilityI,
  elementType: CoordI,
  selfRegion: RegionTemplataI
) extends INameI

case class ReachablePrototypeNameI(num: Int) extends INameI

case class StaticSizedArrayTemplateNameI() extends ICitizenTemplateNameI {
//  override def makeCitizenName(templateArgs: Vector[ITemplataI]): ICitizenNameI = {
//    vassert(templateArgs.size == 5)
//    val size = expectIntegerTemplata(templateArgs(0)).value
//    val mutability = expectMutabilityTemplata(templateArgs(1)).mutability
//    val variability = expectVariabilityTemplata(templateArgs(2)).variability
//    val elementType = expectCoordTemplata(templateArgs(3)).coord
//    val selfRegion = expectRegionTemplata(templateArgs(4))
//    StaticSizedArrayNameI(this, size, variability, RawArrayNameI(mutability, elementType, selfRegion))
//  }
}

case class StaticSizedArrayNameI(
  template: StaticSizedArrayTemplateNameI,
  size: Long,
  variability: VariabilityI,
  arr: RawArrayNameI) extends ICitizenNameI {

  override def templateArgs: Vector[ITemplataI] = {
    Vector(
      IntegerTemplataI(size),
      MutabilityTemplataI(arr.mutability),
      VariabilityTemplataI(variability),
      CoordTemplataI(arr.elementType))
  }
}

case class RuntimeSizedArrayTemplateNameI() extends ICitizenTemplateNameI {
//  override def makeCitizenName(templateArgs: Vector[ITemplataI]): ICitizenNameI = {
//    vassert(templateArgs.size == 3)
//    val mutability = expectMutabilityTemplata(templateArgs(0)).mutability
//    val elementType = expectCoordTemplata(templateArgs(1)).coord
//    val region = expectRegionTemplata(templateArgs(2))
//    RuntimeSizedArrayNameI(this, RawArrayNameI(mutability, elementType, region))
//  }
}

case class RuntimeSizedArrayNameI(template: RuntimeSizedArrayTemplateNameI, arr: RawArrayNameI) extends ICitizenNameI {
  override def templateArgs: Vector[ITemplataI] = {
    Vector(MutabilityTemplataI(arr.mutability), CoordTemplataI(arr.elementType))
  }
}

sealed trait IPlaceholderNameI extends INameI {
  def index: Int
  def rune: IRuneS
}

// This exists because PlaceholderI is a kind, and all kinds need environments to assist
// in call/overload resolution. Environments are associated with templates, so it makes
// some sense to have a "placeholder template" notion.
case class KindPlaceholderTemplateNameI(index: Int, rune: IRuneS) extends ISubKindTemplateNameI with ISuperKindTemplateNameI
case class KindPlaceholderNameI(template: KindPlaceholderTemplateNameI) extends IPlaceholderNameI with ISubKindNameI with ISuperKindNameI {
  override def templateArgs: Vector[ITemplataI] = Vector()
  override def rune: IRuneS = template.rune
  override def index: Int = template.index
}

// DO NOI SUBMII figure out if kind/non-kind is the best distinction here.
// This exists because we need a different way to refer to a coord generic param's other components,
// see MNRFGC.
case class NonKindNonRegionPlaceholderNameI(index: Int, rune: IRuneS) extends IPlaceholderNameI

case class RegionPlaceholderNameI(
  index: Int,
  rune: IRuneS,
//  // Used for TTTDRM to determine other regions' mutabilities from this region's perspective.
//  originalMaybeNearestPureLocation: Option[LocationInDenizen],
//  originallyIntroducedLocation: LocationInDenizen,
//  originallyMutable: Boolean
  pureHeight: Int
) extends IRegionNameI with IPlaceholderNameI

// See NNSPAFOC.
case class OverrideDispatcherTemplateNameI(
  implId: IdI[IImplTemplateNameI]
) extends IFunctionTemplateNameI {
//  override def makeFunctionName(
//    interner: Interner,
//    keywords: Keywords,
//    templateArgs: Vector[ITemplataI],
//    params: Vector[CoordI]):
//  OverrideDispatcherNameI = {
//    interner.intern(OverrideDispatcherNameI(this, templateArgs, params))
//  }
}

case class OverrideDispatcherNameI(
  template: OverrideDispatcherTemplateNameI,
  // This will have placeholders in it after the typing pass.
  templateArgs: Vector[ITemplataI],
  parameters: Vector[CoordI]
) extends IFunctionNameI {
  vpass()
}

case class OverrideDispatcherCaseNameI(
  // These are the templatas for the independent runes from the impl, like the <ZZ> for Milano, see
  // OMCNAGP.
  independentImplTemplateArgs: Vector[ITemplataI]
) extends ITemplateNameI with IInstantiationNameI {
  override def template: ITemplateNameI = this
  override def templateArgs: Vector[ITemplataI] = independentImplTemplateArgs
}

sealed trait IVarNameI extends INameI
case class TypingPassBlockResultVarNameI(life: LocationInFunctionEnvironmentI) extends IVarNameI
case class TypingPassFunctionResultVarNameI() extends IVarNameI
case class TypingPassTemporaryVarNameI(life: LocationInFunctionEnvironmentI) extends IVarNameI
case class TypingPassPatternMemberNameI(life: LocationInFunctionEnvironmentI) extends IVarNameI
case class TypingIgnoredParamNameI(num: Int) extends IVarNameI
case class TypingPassPatternDestructureeNameI(life: LocationInFunctionEnvironmentI) extends IVarNameI
case class UnnamedLocalNameI(codeLocation: CodeLocationS) extends IVarNameI
case class ClosureParamNameI(codeLocation: CodeLocationS) extends IVarNameI
case class ConstructingMemberNameI(name: StrI) extends IVarNameI
case class WhileCondResultNameI(range: RangeS) extends IVarNameI
case class IterableNameI(range: RangeS) extends IVarNameI {  }
case class IteratorNameI(range: RangeS) extends IVarNameI {  }
case class IterationOptionNameI(range: RangeS) extends IVarNameI {  }
case class MagicParamNameI(codeLocation2: CodeLocationS) extends IVarNameI
case class CodeVarNameI(name: StrI) extends IVarNameI
// We dont use CodeVarName2(0), CodeVarName2(1) etc because we dont want the user to address these members directly.
case class AnonymousSubstructMemberNameI(index: Int) extends IVarNameI
case class PrimitiveNameI(humanName: StrI) extends INameI
// Only made in typingpass
case class PackageTopLevelNameI() extends INameI
case class ProjectNameI(name: StrI) extends INameI
case class PackageNameI(name: StrI) extends INameI
case class RuneNameI(rune: IRuneS) extends INameI

// This is the name of a function that we're still figuring out in the function typingpass.
// We have its closured variables, but are still figuring out its template args and params.
case class BuildingFunctionNameWithClosuredsI(
  templateName: IFunctionTemplateNameI,
) extends INameI {



}

case class ExternFunctionNameI(
  humanName: StrI,
  parameters: Vector[CoordI]
) extends IFunctionNameI with IFunctionTemplateNameI {
  override def template: IFunctionTemplateNameI = this

//  override def makeFunctionName(
//    interner: Interner,
//    keywords: Keywords,
//    templateArgs: Vector[ITemplataI],
//    params: Vector[CoordI]):
//  IFunctionNameI = this

  override def templateArgs: Vector[ITemplataI] = Vector.empty
}

case class FunctionNameIX(
  template: FunctionTemplateNameI,
  templateArgs: Vector[ITemplataI],
  parameters: Vector[CoordI]
) extends IFunctionNameI

case class ForwarderFunctionNameI(
  template: ForwarderFunctionTemplateNameI,
  inner: IFunctionNameI
) extends IFunctionNameI {
  override def templateArgs: Vector[ITemplataI] = inner.templateArgs
  override def parameters: Vector[CoordI] = inner.parameters
}

case class FunctionBoundTemplateNameI(
  humanName: StrI,
  codeLocation: CodeLocationS
) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): FunctionBoundNameI = {
//    interner.intern(FunctionBoundNameI(this, templateArgs, params))
//  }
}

case class FunctionBoundNameI(
  template: FunctionBoundTemplateNameI,
  templateArgs: Vector[ITemplataI],
  parameters: Vector[CoordI]
) extends IFunctionNameI

case class FunctionTemplateNameI(
    humanName: StrI,
    codeLocation: CodeLocationS
) extends INameI with IFunctionTemplateNameI {
  vpass()
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(FunctionNameI(this, templateArgs, params))
//  }
}

case class LambdaCallFunctionTemplateNameI(
  codeLocation: CodeLocationS,
  paramTypes: Vector[CoordI]
) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI = {
//    // Post instantiator, the params will be real, but our template paramTypes will still be placeholders
//    // vassert(params == paramTypes)
//    interner.intern(LambdaCallFunctionNameI(this, templateArgs, params))
//  }
}

case class LambdaCallFunctionNameI(
  template: LambdaCallFunctionTemplateNameI,
  templateArgs: Vector[ITemplataI],
  parameters: Vector[CoordI]
) extends IFunctionNameI

case class ForwarderFunctionTemplateNameI(
  inner: IFunctionTemplateNameI,
  index: Int
) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(ForwarderFunctionNameI(this, inner.makeFunctionName(keywords, templateArgs, params)))//, index))
//  }
}


//case class AbstractVirtualDropFunctionTemplateNameI(
//  implName: INameI
//) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(
//      AbstractVirtualDropFunctionNameI(implName, templateArgs, params))
//  }
//}

//case class AbstractVirtualDropFunctionNameI(
//  implName: INameI,
//  templateArgs: Vector[ITemplata[ITemplataType]],
//  parameters: Vector[CoordI]
//) extends INameI with IFunctionNameI

//case class OverrideVirtualDropFunctionTemplateNameI(
//  implName: INameI
//) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(
//      OverrideVirtualDropFunctionNameI(implName, templateArgs, params))
//  }
//}

//case class OverrideVirtualDropFunctionNameI(
//  implName: INameI,
//  templateArgs: Vector[ITemplata[ITemplataType]],
//  parameters: Vector[CoordI]
//) extends INameI with IFunctionNameI

//case class LambdaTemplateNameI(
//  codeLocation: CodeLocationS
//) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(FunctionNameI(interner.intern(FunctionTemplateNameI(keywords.underscoresCall, codeLocation)), templateArgs, params))
//  }
//}
case class ConstructorTemplateNameI(
  codeLocation: CodeLocationS
) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI = vimpl()
}

//case class FreeTemplateNameI(codeLoc: CodeLocationS) extends INameI with IFunctionTemplateNameI {
//  vpass()
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    params match {
//      case Vector(coord) => {
//        interner.intern(FreeNameI(this, templateArgs, coord))
//      }
//      case other => vwat(other)
//    }
//  }
//}
//case class FreeNameI(
//  template: FreeTemplateNameI,
//  templateArgs: Vector[ITemplata[ITemplataType]],
//  coordT: CoordI
//) extends IFunctionNameI {
//  override def parameters: Vector[CoordI] = Vector(coordI)
//}

//// See NSIDN for why we have these virtual names
//case class AbstractVirtualFreeTemplateNameI(codeLoc: CodeLocationS) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    val Vector(CoordI(ShareI, kind)) = params
//    interner.intern(AbstractVirtualFreeNameI(templateArgs, kind))
//  }
//}
//// See NSIDN for why we have these virtual names
//case class AbstractVirtualFreeNameI(templateArgs: Vector[ITemplata[ITemplataType]], param: KindI) extends IFunctionNameI {
//  override def parameters: Vector[CoordI] = Vector(CoordI(ShareI, param))
//}
//
//// See NSIDN for why we have these virtual names
//case class OverrideVirtualFreeTemplateNameI(codeLoc: CodeLocationS) extends INameI with IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordI]): IFunctionNameI = {
//    val Vector(CoordI(ShareI, kind)) = params
//    interner.intern(OverrideVirtualFreeNameI(templateArgs, kind))
//  }
//}
//// See NSIDN for why we have these virtual names
//case class OverrideVirtualFreeNameI(templateArgs: Vector[ITemplata[ITemplataType]], param: KindI) extends IFunctionNameI {
//  override def parameters: Vector[CoordI] = Vector(CoordI(ShareI, param))
//}

// Vale has no Self, its just a convenient first name parameter.
// See also SelfNameS.
case class SelfNameI() extends IVarNameI
case class ArbitraryNameI() extends INameI

sealed trait CitizenNameI extends ICitizenNameI {
  def template: ICitizenTemplateNameI
  def templateArgs: Vector[ITemplataI]
}

object CitizenNameI {
  def unapply(c: CitizenNameI): Option[(ICitizenTemplateNameI, Vector[ITemplataI])] = {
    c match {
      case StructNameI(template, templateArgs) => Some((template, templateArgs))
      case InterfaceNameI(template, templateArgs) => Some((template, templateArgs))
    }
  }
}

case class StructNameI(
  template: IStructTemplateNameI,
  templateArgs: Vector[ITemplataI]
) extends IStructNameI with CitizenNameI {
  vpass()
}

case class InterfaceNameI(
  template: InterfaceTemplateNameI,
  templateArgs: Vector[ITemplataI]
) extends IInterfaceNameI with CitizenNameI {
  vpass()
}

case class LambdaCitizenTemplateNameI(
  codeLocation: CodeLocationS
) extends IStructTemplateNameI {
//  override def makeStructName(templateArgs: Vector[ITemplataI]): IStructNameI = {
//    vassert(templateArgs.isEmpty)
//    interner.intern(LambdaCitizenNameI(this))
//  }
}

case class LambdaCitizenNameI(
  template: LambdaCitizenTemplateNameI
) extends IStructNameI {
  def templateArgs: Vector[ITemplataI] = Vector.empty
  vpass()
}

sealed trait CitizenTemplateNameI extends ICitizenTemplateNameI {
  def humanName: StrI
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the CitizenTemplateNameI from a CitizenNameI which doesn't
  //   remember its code location.
  //codeLocation: CodeLocationS

//  override def makeCitizenName(templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameI = {
//    interner.intern(CitizenNameI(this, templateArgs))
//  }
}

case class StructTemplateNameI(
  humanName: StrI,
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the StructTemplateNameI from a StructNameI which doesn't
  //   remember its code location.
  //   (note from later: not sure this is true anymore, since StructNameI contains a StructTemplateNameI)
  //codeLocation: CodeLocationS
) extends IStructTemplateNameI with CitizenTemplateNameI {
  vpass()

//  override def makeStructName(templateArgs: Vector[ITemplataI]): IStructNameI = {
//    interner.intern(StructNameI(this, templateArgs))
//  }
}
case class InterfaceTemplateNameI(
  humanNamee: StrI,
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the InterfaceTemplateNameI from a InterfaceNameI which doesn't
  //   remember its code location.
  //codeLocation: CodeLocationS
) extends IInterfaceTemplateNameI with CitizenTemplateNameI with ICitizenTemplateNameI {
  override def humanName = humanNamee
//  override def makeInterfaceName(templateArgs: Vector[ITemplataI]): IInterfaceNameI = {
//    interner.intern(InterfaceNameI(this, templateArgs))
//  }
//  override def makeCitizenName(templateArgs: Vector[ITemplataI]): ICitizenNameI = {
//    makeInterfaceName(templateArgs)
//  }
}

case class AnonymousSubstructImplTemplateNameI(
  interface: IInterfaceTemplateNameI
) extends IImplTemplateNameI {
//  override def makeImplName(templateArgs: Vector[ITemplataI], subCitizen: ICitizenIT): IImplNameI = {
//    AnonymousSubstructImplNameI(this, templateArgs, subCitizen)
//  }
}
case class AnonymousSubstructImplNameI(
  template: AnonymousSubstructImplTemplateNameI,
  templateArgs: Vector[ITemplataI],
  subCitizen: ICitizenIT
) extends IImplNameI


case class AnonymousSubstructTemplateNameI(
  // This happens to be the same thing that appears before this AnonymousSubstructNameI in a FullNameT.
  // This is really only here to help us calculate the imprecise name for this thing.
  interface: IInterfaceTemplateNameI
) extends IStructTemplateNameI {
//  override def makeStructName(templateArgs: Vector[ITemplataI]): IStructNameI = {
//    interner.intern(AnonymousSubstructNameI(this, templateArgs))
//  }
}
case class AnonymousSubstructConstructorTemplateNameI(
  substruct: ICitizenTemplateNameI
) extends IFunctionTemplateNameI {
//  override def makeFunctionName(keywords: Keywords, templateArgs: Vector[ITemplataI], params: Vector[CoordI]): IFunctionNameI = {
//    interner.intern(AnonymousSubstructConstructorNameI(this, templateArgs, params))
//  }
}

case class AnonymousSubstructConstructorNameI(
  template: AnonymousSubstructConstructorTemplateNameI,
  templateArgs: Vector[ITemplataI],
  parameters: Vector[CoordI]
) extends IFunctionNameI

case class AnonymousSubstructNameI(
  // This happens to be the same thing that appears before this AnonymousSubstructNameI in a FullNameT.
  // This is really only here to help us calculate the imprecise name for this thing.
  template: AnonymousSubstructTemplateNameI,
  templateArgs: Vector[ITemplataI]
) extends IStructNameI {

}
//case class AnonymousSubstructImplNameI() extends INameI {
//
//}

case class ResolvingEnvNameI() extends INameI {
  vpass()
}

case class CallEnvNameI() extends INameI {
  vpass()
}
