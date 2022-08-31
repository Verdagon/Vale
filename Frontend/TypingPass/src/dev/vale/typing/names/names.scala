package dev.vale.typing.names

import dev.vale.postparsing.{IRuneS, ITemplataType, IntegerTemplataType, MutabilityTemplataType}
import dev.vale.typing.ast.LocationInFunctionEnvironment
import dev.vale.typing.expression.CallCompiler
import dev.vale.{CodeLocationS, IInterning, Interner, Keywords, PackageCoordinate, RangeS, vassert, vcurious, vimpl, vpass, vwat, _}
import dev.vale.typing.templata.ITemplata
import dev.vale.typing.types._
import dev.vale.typing.templata.CoordTemplata
import dev.vale.typing.types._

// Scout's/Astronomer's name parts correspond to where they are in the source code,
// but Compiler's correspond more to what packages and stamped functions / structs
// they're in. See TNAD.

case class FullNameT[+T <: INameT](
  packageCoord: PackageCoordinate,
  initSteps: Vector[INameT],
  last: T
)  {

  // Placeholders should only be the last name, getPlaceholdersInKind assumes it
  initSteps.foreach({
    case PlaceholderNameT(_) => vfail()
    case PlaceholderTemplateNameT(_) => vfail()
    case _ =>
  })
  // Placeholders are under the template name.
  // There's really no other way; we make the placeholders before knowing the function's
  // instantated name.
  last match {
    case PlaceholderNameT(_) => {
      initSteps.last match {
        case FunctionTemplateNameT(_, _) =>
        case StructTemplateNameT(_) =>
        case InterfaceTemplateNameT(_) =>
        case ImplTemplateDeclareNameT(_) =>
        case AnonymousSubstructTemplateNameT(_) =>
        case AnonymousSubstructConstructorTemplateNameT(_) =>
        case ForwarderFunctionTemplateNameT(_,_) =>
        case FreeTemplateNameT(_) =>
        case other => vfail(other)
      }
    }
    case _ =>
  }

  // PackageTopLevelName2 is just here because names have to have a last step.
  vassert(initSteps.collectFirst({ case PackageTopLevelNameT() => }).isEmpty)

  vcurious(initSteps.distinct == initSteps)

  override def equals(obj: Any): Boolean = {
    obj match {
      case FullNameT(thatPackageCoord, thatInitSteps, thatLast) => {
        packageCoord == thatPackageCoord && initSteps == thatInitSteps && last == thatLast
      }
      case _ => false
    }
  }

  def packageFullName(interner: Interner): FullNameT[PackageTopLevelNameT] = {
    FullNameT(packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
  }

  def initFullName(interner: Interner): FullNameT[INameT] = {
    if (initSteps.isEmpty) {
      FullNameT(packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
    } else {
      FullNameT(packageCoord, initSteps.init, initSteps.last)
    }
  }

  def steps: Vector[INameT] = {
    last match {
      case PackageTopLevelNameT() => initSteps
      case _ => initSteps :+ last
    }
  }
  def addStep[Y <: INameT](newLast: Y): FullNameT[Y] = {
    FullNameT[Y](packageCoord, steps, newLast)
  }
//  def init: FullNameT[INameT] = {
//    if (initSteps.isEmpty) {
//      last match {
//        case PackageTopLevelNameT() => vimpl()
//        case _ => {
//          FullNameT(packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
//        }
//      }
//    } else {
//      FullNameT(packageCoord, initSteps.init, initSteps.last)
//    }
//  }

//  def parent: Option[FullNameT[INameT]] = {
//    if (initSteps.isEmpty) {
//      packageCoord.parent match {
//        case None => None
//        case Some(parentPackage) => Some(FullNameT(parentPackage, Vector(), interner.intern(PackageTopLevelNameT())))
//      }
//    } else {
//      Some(FullNameT(packageCoord, initSteps.init, initSteps.last))
//    }
//  }
//
//  def selfAndParents: List[FullNameT[INameT]] = {
//    parent match {
//      case None => List(this)
//      case Some(parent) => this :: parent.selfAndParents
//    }
//  }
//
//  def parents: List[FullNameT[INameT]] = {
//    parent match {
//      case None => List()
//      case Some(parent) => parent.selfAndParents
//    }
//  }
}

sealed trait INameT extends IInterning
sealed trait ITemplateNameT extends INameT
sealed trait IFunctionTemplateNameT extends ITemplateNameT {
  def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT
}
sealed trait IInstantiationNameT extends INameT {
  def template: ITemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait IFunctionNameT extends IInstantiationNameT {
  def template: IFunctionTemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
  def parameters: Vector[CoordT]
}
sealed trait ISuperKindTemplateNameT extends ITemplateNameT
sealed trait ISubKindTemplateNameT extends ITemplateNameT
sealed trait ICitizenTemplateNameT extends ISubKindTemplateNameT {
  def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameT
}
sealed trait IStructTemplateNameT extends ICitizenTemplateNameT {
  def makeStructName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IStructNameT
  override def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]):
  ICitizenNameT = {
    makeStructName(interner, templateArgs)
  }
}
sealed trait IInterfaceTemplateNameT extends ICitizenTemplateNameT with ISuperKindTemplateNameT {
  def makeInterfaceName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IInterfaceNameT
}
sealed trait ISuperKindNameT extends IInstantiationNameT {
  def template: ISuperKindTemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait ISubKindNameT extends IInstantiationNameT {
  def template: ISubKindTemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait ICitizenNameT extends ISubKindNameT {
  def template: ICitizenTemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait IStructNameT extends ICitizenNameT with ISubKindNameT {
  override def template: IStructTemplateNameT
  override def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait IInterfaceNameT extends ICitizenNameT with ISubKindNameT with ISuperKindNameT {
  override def template: InterfaceTemplateNameT
  override def templateArgs: Vector[ITemplata[ITemplataType]]
}
sealed trait IImplTemplateNameT extends ITemplateNameT {
  def makeImplName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IImplNameT
}
sealed trait IImplNameT extends IInstantiationNameT {
  def template: IImplTemplateNameT
}

case class ExportTemplateNameT(codeLoc: CodeLocationS) extends ITemplateNameT
case class ExportNameT(template: ExportTemplateNameT) extends IInstantiationNameT {
  override def templateArgs: Vector[ITemplata[ITemplataType]] = Vector()
}

case class ImplTemplateDeclareNameT(codeLocationS: CodeLocationS) extends IImplTemplateNameT {
  override def makeImplName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ImplDeclareNameT = {
    interner.intern(ImplDeclareNameT(this, templateArgs))
  }
}
case class ImplDeclareNameT(
  template: ImplTemplateDeclareNameT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IImplNameT {

}

//// The name of an impl that is subclassing some interface. To find all impls subclassing an interface,
//// look for this name.
//case class ImplImplementingSuperInterfaceNameT(superInterface: FullNameT[IInterfaceTemplateNameT]) extends IImplTemplateNameT
//// The name of an impl that is augmenting some sub citizen. To find all impls subclassing an interface,
//// look for this name.
//case class ImplAugmentingSubCitizenNameT(subCitizen: FullNameT[ICitizenTemplateNameT]) extends IImplTemplateNameT

case class LetNameT(codeLocation: CodeLocationS) extends INameT
case class ExportAsNameT(codeLocation: CodeLocationS) extends INameT

case class RawArrayNameT(mutability: ITemplata[MutabilityTemplataType], elementType: CoordT) extends INameT

case class ReachablePrototypeNameT(num: Int) extends INameT

case class StaticSizedArrayTemplateNameT() extends ICitizenTemplateNameT {
  override def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameT = {
    vimpl()
  }
}
case class StaticSizedArrayNameT(size: ITemplata[IntegerTemplataType], arr: RawArrayNameT) extends INameT
case class RuntimeSizedArrayTemplateNameT() extends ICitizenTemplateNameT {
  override def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameT = {
    vimpl()
  }
}
case class RuntimeSizedArrayNameT(arr: RawArrayNameT) extends INameT

// This exists because PlaceholderT is a kind, and all kinds need environments to assist
// in call/overload resolution. Environments are associated with templates, so it makes
// some sense to have a "placeholder template" notion.
case class PlaceholderTemplateNameT(index: Int) extends ISubKindTemplateNameT with ISuperKindTemplateNameT
case class PlaceholderNameT(template: PlaceholderTemplateNameT) extends ISubKindNameT with ISuperKindNameT {
  override def templateArgs: Vector[ITemplata[ITemplataType]] = Vector()
}

sealed trait IVarNameT extends INameT
case class TypingPassBlockResultVarNameT(life: LocationInFunctionEnvironment) extends IVarNameT
case class TypingPassFunctionResultVarNameT() extends IVarNameT
case class TypingPassTemporaryVarNameT(life: LocationInFunctionEnvironment) extends IVarNameT
case class TypingPassPatternMemberNameT(life: LocationInFunctionEnvironment) extends IVarNameT
case class TypingIgnoredParamNameT(num: Int) extends IVarNameT
case class TypingPassPatternDestructureeNameT(life: LocationInFunctionEnvironment) extends IVarNameT
case class UnnamedLocalNameT(codeLocation: CodeLocationS) extends IVarNameT
case class ClosureParamNameT() extends IVarNameT
case class ConstructingMemberNameT(name: StrI) extends IVarNameT
case class WhileCondResultNameT(range: RangeS) extends IVarNameT
case class IterableNameT(range: RangeS) extends IVarNameT {  }
case class IteratorNameT(range: RangeS) extends IVarNameT {  }
case class IterationOptionNameT(range: RangeS) extends IVarNameT {  }
case class MagicParamNameT(codeLocation2: CodeLocationS) extends IVarNameT
case class CodeVarNameT(name: StrI) extends IVarNameT
// We dont use CodeVarName2(0), CodeVarName2(1) etc because we dont want the user to address these members directly.
case class AnonymousSubstructMemberNameT(index: Int) extends IVarNameT
case class PrimitiveNameT(humanName: StrI) extends INameT
// Only made in typingpass
case class PackageTopLevelNameT() extends INameT
case class ProjectNameT(name: StrI) extends INameT
case class PackageNameT(name: StrI) extends INameT
case class RuneNameT(rune: IRuneS) extends INameT

// This is the name of a function that we're still figuring out in the function typingpass.
// We have its closured variables, but are still figuring out its template args and params.
case class BuildingFunctionNameWithClosuredsT(
  templateName: IFunctionTemplateNameT,
) extends INameT {



}

case class ExternFunctionNameT(
  humanName: StrI,
  parameters: Vector[CoordT]
) extends IFunctionNameT with IFunctionTemplateNameT {
  override def template: IFunctionTemplateNameT = this

  override def makeFunctionName(
    interner: Interner,
    keywords: Keywords,
    templateArgs: Vector[ITemplata[ITemplataType]],
    params: Vector[CoordT]):
  IFunctionNameT = this

  override def templateArgs: Vector[ITemplata[ITemplataType]] = Vector.empty
}

case class FunctionNameT(
  template: FunctionTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]],
  parameters: Vector[CoordT]
) extends IFunctionNameT

case class ForwarderFunctionNameT(
  template: ForwarderFunctionTemplateNameT,
  inner: IFunctionNameT
) extends IFunctionNameT {
  override def templateArgs: Vector[ITemplata[ITemplataType]] = inner.templateArgs
  override def parameters: Vector[CoordT] = inner.parameters
}

case class FunctionBoundTemplateNameT(
  humanName: StrI,
  codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  this match {
    case FunctionBoundTemplateNameT(StrI("drop"),CodeLocationS(FileCoordinate(_,"opt.vale"), 67)) => {
      vpass()
    }
    case _ =>
  }
  vpass()
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    interner.intern(FunctionBoundNameT(this, templateArgs, params))
  }
}

case class FunctionBoundNameT(
  template: FunctionBoundTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]],
  parameters: Vector[CoordT]
) extends IFunctionNameT

case class FunctionTemplateNameT(
    humanName: StrI,
    codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  this match {
    case FunctionTemplateNameT(StrI("drop"),CodeLocationS(FileCoordinate(_,"opt.vale"), 67)) => {
      vpass()
    }
    case _ =>
  }
  vpass()
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    interner.intern(FunctionNameT(this, templateArgs, params))
  }
}

case class ForwarderFunctionTemplateNameT(
  inner: IFunctionTemplateNameT,
  index: Int
) extends INameT with IFunctionTemplateNameT {
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    interner.intern(ForwarderFunctionNameT(this, inner.makeFunctionName(interner, keywords, templateArgs, params)))//, index))
  }
}


//case class AbstractVirtualDropFunctionTemplateNameT(
//  implName: INameT
//) extends INameT with IFunctionTemplateNameT {
//  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
//    interner.intern(
//      AbstractVirtualDropFunctionNameT(implName, templateArgs, params))
//  }
//}

//case class AbstractVirtualDropFunctionNameT(
//  implName: INameT,
//  templateArgs: Vector[ITemplata[ITemplataType]],
//  parameters: Vector[CoordT]
//) extends INameT with IFunctionNameT

//case class OverrideVirtualDropFunctionTemplateNameT(
//  implName: INameT
//) extends INameT with IFunctionTemplateNameT {
//  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
//    interner.intern(
//      OverrideVirtualDropFunctionNameT(implName, templateArgs, params))
//  }
//}

//case class OverrideVirtualDropFunctionNameT(
//  implName: INameT,
//  templateArgs: Vector[ITemplata[ITemplataType]],
//  parameters: Vector[CoordT]
//) extends INameT with IFunctionNameT

case class LambdaTemplateNameT(
  codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    interner.intern(FunctionNameT(interner.intern(FunctionTemplateNameT(keywords.underscoresCall, codeLocation)), templateArgs, params))
  }
}
case class ConstructorTemplateNameT(
  codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = vimpl()
}

case class FreeTemplateNameT(codeLoc: CodeLocationS) extends INameT with IFunctionTemplateNameT {
  vpass()
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    params match {
      case Vector(coord) => {
        interner.intern(FreeNameT(this, templateArgs, coord))
      }
      case other => vwat(other)
    }
  }
}
case class FreeNameT(
  template: FreeTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]],
  coordT: CoordT
) extends IFunctionNameT {
  override def parameters: Vector[CoordT] = Vector(coordT)
}

//// See NSIDN for why we have these virtual names
//case class AbstractVirtualFreeTemplateNameT(codeLoc: CodeLocationS) extends INameT with IFunctionTemplateNameT {
//  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
//    val Vector(CoordT(ShareT, kind)) = params
//    interner.intern(AbstractVirtualFreeNameT(templateArgs, kind))
//  }
//}
//// See NSIDN for why we have these virtual names
//case class AbstractVirtualFreeNameT(templateArgs: Vector[ITemplata[ITemplataType]], param: KindT) extends IFunctionNameT {
//  override def parameters: Vector[CoordT] = Vector(CoordT(ShareT, param))
//}
//
//// See NSIDN for why we have these virtual names
//case class OverrideVirtualFreeTemplateNameT(codeLoc: CodeLocationS) extends INameT with IFunctionTemplateNameT {
//  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
//    val Vector(CoordT(ShareT, kind)) = params
//    interner.intern(OverrideVirtualFreeNameT(templateArgs, kind))
//  }
//}
//// See NSIDN for why we have these virtual names
//case class OverrideVirtualFreeNameT(templateArgs: Vector[ITemplata[ITemplataType]], param: KindT) extends IFunctionNameT {
//  override def parameters: Vector[CoordT] = Vector(CoordT(ShareT, param))
//}

// Vale has no Self, its just a convenient first name parameter.
// See also SelfNameS.
case class SelfNameT() extends IVarNameT
case class ArbitraryNameT() extends INameT

sealed trait CitizenNameT extends ICitizenNameT {
  def template: ICitizenTemplateNameT
  def templateArgs: Vector[ITemplata[ITemplataType]]
}

object CitizenNameT {
  def unapply(c: CitizenNameT): Option[(ICitizenTemplateNameT, Vector[ITemplata[ITemplataType]])] = {
    c match {
      case StructNameT(template, templateArgs) => Some((template, templateArgs))
      case InterfaceNameT(template, templateArgs) => Some((template, templateArgs))
    }
  }
}

case class StructNameT(
  template: IStructTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IStructNameT with CitizenNameT {
  vpass()
}

case class InterfaceNameT(
  template: InterfaceTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IInterfaceNameT with CitizenNameT {
  vpass()
}

case class LambdaCitizenTemplateNameT(
  codeLocation: CodeLocationS
) extends IStructTemplateNameT {
  def templateArgs: Vector[ITemplata[ITemplataType]] = Vector.empty
  override def makeStructName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IStructNameT = {
    vassert(templateArgs.isEmpty)
    interner.intern(LambdaCitizenNameT(this))
  }
}

case class LambdaCitizenNameT(
  template: LambdaCitizenTemplateNameT
) extends IStructNameT {
  def templateArgs: Vector[ITemplata[ITemplataType]] = Vector.empty
  vpass()
}

sealed trait CitizenTemplateNameT extends ICitizenTemplateNameT {
  def humanName: StrI
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the CitizenTemplateNameT from a CitizenNameT which doesn't
  //   remember its code location.
  //codeLocation: CodeLocationS

//  override def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameT = {
//    interner.intern(CitizenNameT(this, templateArgs))
//  }
}

case class StructTemplateNameT(
  humanName: StrI,
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the StructTemplateNameT from a StructNameT which doesn't
  //   remember its code location.
  //   (note from later: not sure this is true anymore, since StructNameT contains a StructTemplateNameT)
  //codeLocation: CodeLocationS
) extends IStructTemplateNameT with CitizenTemplateNameT {
  vpass()

  override def makeStructName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IStructNameT = {
    interner.intern(StructNameT(this, templateArgs))
  }
}
case class InterfaceTemplateNameT(
  humanNamee: StrI,
  // We don't include a CodeLocation here because:
  // - There's no struct overloading, so there should only ever be one, we don't have to disambiguate
  //   with code locations
  // - It makes it easier to determine the InterfaceTemplateNameT from a InterfaceNameT which doesn't
  //   remember its code location.
  //codeLocation: CodeLocationS
) extends IInterfaceTemplateNameT with CitizenTemplateNameT with ICitizenTemplateNameT {
  override def humanName = humanNamee
  override def makeInterfaceName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IInterfaceNameT = {
    interner.intern(InterfaceNameT(this, templateArgs))
  }
  override def makeCitizenName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): ICitizenNameT = {
    makeInterfaceName(interner, templateArgs)
  }
}

case class AnonymousSubstructImplTemplateNameT(
  interface: IInterfaceTemplateNameT
) extends IImplTemplateNameT {
  override def makeImplName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IImplNameT = {
    interner.intern(AnonymousSubstructImplNameT(this, templateArgs))
  }
}
case class AnonymousSubstructImplNameT(
  template: AnonymousSubstructImplTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IImplNameT


case class AnonymousSubstructTemplateNameT(
  // This happens to be the same thing that appears before this AnonymousSubstructNameT in a FullNameT.
  // This is really only here to help us calculate the imprecise name for this thing.
  interface: IInterfaceTemplateNameT
) extends IStructTemplateNameT {
  override def makeStructName(interner: Interner, templateArgs: Vector[ITemplata[ITemplataType]]): IStructNameT = {
    interner.intern(AnonymousSubstructNameT(this, templateArgs))
  }
}
case class AnonymousSubstructConstructorTemplateNameT(
  substruct: ICitizenTemplateNameT
) extends IFunctionTemplateNameT {
  override def makeFunctionName(interner: Interner, keywords: Keywords, templateArgs: Vector[ITemplata[ITemplataType]], params: Vector[CoordT]): IFunctionNameT = {
    interner.intern(AnonymousSubstructConstructorNameT(this, templateArgs, params))
  }
}

case class AnonymousSubstructConstructorNameT(
  template: AnonymousSubstructConstructorTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]],
  parameters: Vector[CoordT]
) extends IFunctionNameT

case class AnonymousSubstructNameT(
  // This happens to be the same thing that appears before this AnonymousSubstructNameT in a FullNameT.
  // This is really only here to help us calculate the imprecise name for this thing.
  template: AnonymousSubstructTemplateNameT,
  templateArgs: Vector[ITemplata[ITemplataType]]
) extends IStructNameT {

}
//case class AnonymousSubstructImplNameT() extends INameT {
//
//}

case class ResolvingEnvNameT() extends INameT {
  vpass()
}

case class CallEnvNameT() extends INameT {
  vpass()
}
