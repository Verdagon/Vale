package dev.vale.typing

import dev.vale.{RangeS, vassert, vassertOne, vfail, vimpl}
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.postparsing._
import dev.vale.typing.env.{IEnvironment, TemplataLookupContext}
import dev.vale.typing.names.{AnonymousSubstructNameT, CitizenNameT, FullNameT, ICitizenNameT, ICitizenTemplateNameT, IFunctionNameT, IFunctionTemplateNameT, IInterfaceNameT, IInterfaceTemplateNameT, INameT, IStructNameT, IStructTemplateNameT, InterfaceNameT, NameTranslator, PlaceholderNameT, StructNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.postparsing._
import dev.vale.typing._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.env.TemplataLookupContext
import dev.vale.typing.templata.ITemplata.expectMutability
import dev.vale.typing.types._
import dev.vale.typing.templata._

import scala.collection.immutable.{List, Map, Set}

trait ITemplataCompilerDelegate {

  def isParent(
    coutputs: CompilerOutputs,
    descendantCitizenRef: ICitizenTT,
    ancestorInterfaceRef: InterfaceTT):
  Boolean

  def resolveStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  StructTT

  def resolveInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  InterfaceTT

  def getStaticSizedArrayKind(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    mutability: ITemplata[MutabilityTemplataType],
    variability: ITemplata[VariabilityTemplataType],
    size: ITemplata[IntegerTemplataType],
    type2: CoordT):
  StaticSizedArrayTT

  def getRuntimeSizedArrayKind(env: IEnvironment, state: CompilerOutputs, element: CoordT, arrayMutability: ITemplata[MutabilityTemplataType]): RuntimeSizedArrayTT
}

object TemplataCompiler {

  def getFunctionTemplate(fullName: FullNameT[IFunctionNameT]): FullNameT[IFunctionTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def getCitizenTemplate(fullName: FullNameT[ICitizenNameT]): FullNameT[ICitizenTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def getStructTemplate(fullName: FullNameT[IStructNameT]): FullNameT[IStructTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def getInterfaceTemplate(fullName: FullNameT[IInterfaceNameT]): FullNameT[IInterfaceTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def substituteTemplatasInCoord(
    coord: CoordT,
    substitutions: Array[(PlaceholderTemplata[ITemplataType], ITemplata[ITemplataType])]):
  CoordT = {
    val CoordT(ownership, kind) = coord
    CoordT(ownership, substituteTemplatasInKind(kind, substitutions))
  }

  def substituteTemplatasInKind(
    kind: KindT,
    substitutions: Array[(PlaceholderTemplata[ITemplataType], ITemplata[ITemplataType])]):
  KindT = {
    kind match {
      case IntT(bits) => kind
      case BoolT() => kind
      case StrT() => kind
      case FloatT() => kind
      case VoidT() => kind
      case NeverT(_) => kind
      case s @ StructTT(_) => substituteTemplatasInStruct(s, substitutions)
      case s @ InterfaceTT(_) => substituteTemplatasInInterface(s, substitutions)
    }
  }

  def substituteTemplatasInStruct(
    structTT: StructTT,
    substitutions: Array[(PlaceholderTemplata[ITemplataType], ITemplata[ITemplataType])]):
  StructTT = {
    val StructTT(FullNameT(packageCoord, initSteps, last)) = structTT
    StructTT(
      FullNameT(
        packageCoord,
        initSteps,
        last match {
          case StructNameT(template, templateArgs) => StructNameT(template, templateArgs.map(substituteTemplatasInTemplata(_, substitutions)))
        }))
  }

  def substituteTemplatasInInterface(
    interfaceTT: InterfaceTT,
    substitutions: Array[(PlaceholderTemplata[ITemplataType], ITemplata[ITemplataType])]):
  InterfaceTT = {
    val InterfaceTT(FullNameT(packageCoord, initSteps, last)) = interfaceTT
    InterfaceTT(
      FullNameT(
        packageCoord,
        initSteps,
        last match {
          case InterfaceNameT(template, templateArgs) => InterfaceNameT(template, templateArgs.map(substituteTemplatasInTemplata(_, substitutions)))
        }))
  }

  def substituteTemplatasInTemplata(
    templata: ITemplata[ITemplataType],
    substitutions: Array[(PlaceholderTemplata[ITemplataType], ITemplata[ITemplataType])]):
  ITemplata[ITemplataType] = {
    templata match {
      case CoordTemplata(c) => CoordTemplata(substituteTemplatasInCoord(c, substitutions))
      case KindTemplata(k) => KindTemplata(substituteTemplatasInKind(k, substitutions))
      case p @ PlaceholderTemplata(FullNameT(_, _, PlaceholderNameT(index)), _)
        if index < substitutions.length && p == substitutions(index)._1 => {
        substitutions(index)._2
      }
      case MutabilityTemplata(_) => templata
      case VariabilityTemplata(_) => templata
      case IntegerTemplata(_) => templata
      case BooleanTemplata(_) => templata
      case other => vimpl(other)
    }
  }
}

class TemplataCompiler(
  opts: TypingPassOptions,

  nameTranslator: NameTranslator,
  delegate: ITemplataCompilerDelegate) {

  def isTypeConvertible(
    coutputs: CompilerOutputs,
    sourcePointerType: CoordT,
    targetPointerType: CoordT):
  Boolean = {

    val CoordT(targetOwnership, targetType) = targetPointerType;
    val CoordT(sourceOwnership, sourceType) = sourcePointerType;

    // Note the Never case will short-circuit a true, regardless of the other checks (ownership)

    (sourceType, targetType) match {
      case (NeverT(_), _) => return true
      case (a, b) if a == b =>
      case (VoidT() | IntT(_) | BoolT() | StrT() | FloatT() | RuntimeSizedArrayTT(_, _) | StaticSizedArrayTT(_, _, _, _), _) => return false
      case (_, VoidT() | IntT(_) | BoolT() | StrT() | FloatT() | RuntimeSizedArrayTT(_, _) | StaticSizedArrayTT(_, _, _, _)) => return false
      case (_, StructTT(_)) => return false
      case (a @ StructTT(_), b @ InterfaceTT(_)) => {
        if (!delegate.isParent(coutputs, a, b)) {
          return false
        }
      }
      case (a @ InterfaceTT(_), b @ InterfaceTT(_)) => {
        if (!delegate.isParent(coutputs, a, b)) {
          return false
        }
      }
      case _ => {
        vfail("Dont know if we can convert from " + sourceType + " to " + targetType)
      }
    }

    (sourceOwnership, targetOwnership) match {
      case (a, b) if a == b =>
      // At some point maybe we should automatically convert borrow to pointer and vice versa
      // and perhaps automatically promote borrow or pointer to weak?
      case (OwnT, BorrowT) => return false
      case (OwnT, WeakT) => return false
      case (OwnT, ShareT) => return false
      case (BorrowT, OwnT) => return false
      case (BorrowT, WeakT) => return false
      case (BorrowT, ShareT) => return false
      case (WeakT, OwnT) => return false
      case (WeakT, BorrowT) => return false
      case (WeakT, ShareT) => return false
      case (ShareT, BorrowT) => return false
      case (ShareT, WeakT) => return false
      case (ShareT, OwnT) => return false
    }

    true
  }

  def pointifyKind(coutputs: CompilerOutputs, kind: KindT, ownershipIfMutable: OwnershipT): CoordT = {
    val mutability = Compiler.getMutability(coutputs, kind)
    val ownership =
      mutability match {
        case PlaceholderTemplata(fullNameT, tyype) => vimpl()
        case MutabilityTemplata(MutableT) => ownershipIfMutable
        case MutabilityTemplata(ImmutableT) => ShareT
      }
    kind match {
      case a @ RuntimeSizedArrayTT(_, _) => {
        CoordT(ownership, a)
      }
      case a @ StaticSizedArrayTT(_, _, _, _) => {
        CoordT(ownership, a)
      }
      case s @ StructTT(_) => {
        CoordT(ownership, s)
      }
      case i @ InterfaceTT(_) => {
        CoordT(ownership, i)
      }
      case VoidT() => {
        CoordT(ShareT, VoidT())
      }
      case i @ IntT(_) => {
        CoordT(ShareT, i)
      }
      case FloatT() => {
        CoordT(ShareT, FloatT())
      }
      case BoolT() => {
        CoordT(ShareT, BoolT())
      }
      case StrT() => {
        CoordT(ShareT, StrT())
      }
    }
  }

//  def evaluateStructTemplata(
//    coutputs: CompilerOutputs,
//    callRange: RangeS,
//    template: StructTemplata,
//    templateArgs: Vector[ITemplata[ITemplataType]],
//    expectedType: ITemplataType):
//  (ITemplata[ITemplataType]) = {
//    val uncoercedTemplata =
//      delegate.resolveStruct(coutputs, callRange, template, templateArgs)
//    val templata =
//      coerce(coutputs, callRange, KindTemplata(uncoercedTemplata), expectedType)
//    (templata)
//  }

  def evaluateInterfaceTemplata(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    template: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]],
    expectedType: ITemplataType):
  (ITemplata[ITemplataType]) = {
    val uncoercedTemplata =
      delegate.resolveInterface(coutputs, callingEnv, callRange, template, templateArgs)
    val templata =
      coerce(coutputs, callRange, KindTemplata(uncoercedTemplata), expectedType)
    (templata)
  }

//  def evaluateBuiltinTemplateTemplata(
//    env: IEnvironment,
//    coutputs: CompilerOutputs,
//    range: RangeS,
//    template: RuntimeSizedArrayTemplateTemplata,
//    templateArgs: Vector[ITemplata[ITemplataType]],
//    expectedType: ITemplataType):
//  (ITemplata[ITemplataType]) = {
//    val Vector(m, CoordTemplata(elementType)) = templateArgs
//    val mutability = ITemplata.expectMutability(m)
//    val arrayKindTemplata = delegate.getRuntimeSizedArrayKind(env, coutputs, elementType, mutability)
//    val templata =
//      coerce(coutputs, callingEnv, range, KindTemplata(arrayKindTemplata), expectedType)
//    (templata)
//  }

//  def getStaticSizedArrayKind(
//    env: IEnvironment,
//    coutputs: CompilerOutputs,
//    callRange: RangeS,
//    mutability: ITemplata[MutabilityTemplataType],
//    variability: ITemplata[VariabilityTemplataType],
//    size: ITemplata[IntegerTemplataType],
//    element: CoordT,
//    expectedType: ITemplataType):
//  (ITemplata[ITemplataType]) = {
//    val uncoercedTemplata =
//      delegate.getStaticSizedArrayKind(env, coutputs, mutability, variability, size, element)
//    val templata =
//      coerce(coutputs, callingEnv, callRange, KindTemplata(uncoercedTemplata), expectedType)
//    (templata)
//  }

  def lookupTemplata(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    range: RangeS,
    name: INameT):
  (ITemplata[ITemplataType]) = {
    // Changed this from AnythingLookupContext to TemplataLookupContext
    // because this is called from StructCompiler to figure out its members.
    // We could instead pipe a lookup context through, if this proves problematic.
    vassertOne(env.lookupNearestWithName(name, Set(TemplataLookupContext)))
  }

  def lookupTemplata(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    range: RangeS,
    name: IImpreciseNameS):
  Option[ITemplata[ITemplataType]] = {
    // Changed this from AnythingLookupContext to TemplataLookupContext
    // because this is called from StructCompiler to figure out its members.
    // We could instead pipe a lookup context through, if this proves problematic.
    val results = env.lookupNearestWithImpreciseName(name, Set(TemplataLookupContext))
    if (results.size > 1) {
      vfail()
    }
    results.headOption
  }

  def coerceKindToCoord(coutputs: CompilerOutputs, kind: KindT):
  CoordT = {
    val mutability = Compiler.getMutability(coutputs, kind)
    CoordT(
      mutability match {
        case MutabilityTemplata(MutableT) => OwnT
        case MutabilityTemplata(ImmutableT) => ShareT
        case PlaceholderTemplata(fullNameT, tyype) => OwnT
      },
      kind)
  }

  def coerce(
    coutputs: CompilerOutputs,
    range: RangeS,
    templata: ITemplata[ITemplataType],
    tyype: ITemplataType):
  (ITemplata[ITemplataType]) = {
    if (templata.tyype == tyype) {
      templata
    } else {
      (templata, tyype) match {
        case (KindTemplata(kind), CoordTemplataType()) => {
          CoordTemplata(coerceKindToCoord(coutputs, kind))
        }
        case (st@StructTemplata(declaringEnv, structA), KindTemplataType()) => {
          if (structA.isTemplate) {
            vfail("Can't coerce " + structA.name + " to be a kind, is a template!")
          }
          val kind =
            delegate.resolveStruct(coutputs, declaringEnv, range, st, Vector.empty)
          (KindTemplata(kind))
        }
        case (it@InterfaceTemplata(declaringEnv, interfaceA), KindTemplataType()) => {
          if (interfaceA.isTemplate) {
            vfail("Can't coerce " + interfaceA.name + " to be a kind, is a template!")
          }
          val kind =
            delegate.resolveInterface(coutputs, declaringEnv, range, it, Vector.empty)
          (KindTemplata(kind))
        }
        case (st@StructTemplata(declaringEnv, structA), CoordTemplataType()) => {
          if (structA.isTemplate) {
            vfail("Can't coerce " + structA.name + " to be a coord, is a template!")
          }
          val kind =
            delegate.resolveStruct(coutputs, declaringEnv, range, st, Vector.empty)
          val mutability = Compiler.getMutability(coutputs, kind)

          // Default ownership is own for mutables, share for imms
          val ownership =
            mutability match {
              case MutabilityTemplata(MutableT) => OwnT
              case MutabilityTemplata(ImmutableT) => ShareT
              case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => vimpl()
            }
          val coerced = CoordTemplata(CoordT(ownership, kind))
          (coerced)
        }
        case (it@InterfaceTemplata(declaringEnv, interfaceA), CoordTemplataType()) => {
          if (interfaceA.isTemplate) {
            vfail("Can't coerce " + interfaceA.name + " to be a coord, is a template!")
          }
          val kind =
            delegate.resolveInterface(coutputs, declaringEnv, range, it, Vector.empty)
          val mutability = Compiler.getMutability(coutputs, kind)
          val coerced =
            CoordTemplata(
              CoordT(
                mutability match {
                  case MutabilityTemplata(MutableT) => OwnT
                  case MutabilityTemplata(ImmutableT) => ShareT
                  case PlaceholderTemplata(fullNameT, MutabilityTemplataType()) => vimpl()
                },
                kind))
          (coerced)
        }
        case _ => {
          vfail("Can't coerce a " + templata.tyype + " to be a " + tyype)
        }
      }
    }
  }

  def citizenIsFromTemplate(actualCitizenRef: ICitizenTT, expectedCitizenTemplata: ITemplata[ITemplataType]): Boolean = {
    val citizenTemplateFullName =
      expectedCitizenTemplata match {
        case StructTemplata(env, originStruct) => {
          env.fullName.addStep(nameTranslator.translateCitizenName(originStruct.name))
        }
        case InterfaceTemplata(env, originInterface) => {
          env.fullName.addStep(nameTranslator.translateCitizenName(originInterface.name))
        }
        case KindTemplata(expectedKind) => return actualCitizenRef == expectedKind
        case CoordTemplata(CoordT(OwnT | ShareT, actualKind)) => return actualCitizenRef == actualKind
        case _ => return false
      }
    if (actualCitizenRef.fullName.initSteps != citizenTemplateFullName.initSteps) {
      // Packages dont match, bail
      return false
    }
    citizenTemplateFullName.last == actualCitizenRef.fullName.last.template
  }
}
