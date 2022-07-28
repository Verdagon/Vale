package dev.vale.typing

import dev.vale.{Interner, RangeS, vassert, vassertOne, vfail, vimpl, vwat}
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.postparsing._
import dev.vale.typing.env.{GeneralEnvironment, IEnvironment, TemplataLookupContext}
import dev.vale.typing.names.{AnonymousSubstructNameT, CitizenNameT, FullNameT, ICitizenNameT, ICitizenTemplateNameT, IFunctionNameT, IFunctionTemplateNameT, IInstantiationNameT, IInterfaceNameT, IInterfaceTemplateNameT, INameT, IStructNameT, IStructTemplateNameT, ITemplateNameT, InterfaceNameT, LambdaCitizenNameT, NameTranslator, PlaceholderNameT, PlaceholderTemplateNameT, StructNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.postparsing._
import dev.vale.typing.TemplataCompiler.getCitizenTemplate
import dev.vale.typing._
import dev.vale.typing.ast.UnsubstitutedCoordT
import dev.vale.typing.citizen.{ImplCompiler, IsParent, IsParentResult, IsntParent}
import dev.vale.typing.templata.ITemplata.{expectInteger, expectMutability, expectVariability}
import dev.vale.typing.types._
import dev.vale.typing.templata._

import scala.collection.immutable.{List, Map, Set}

trait ITemplataCompilerDelegate {

  def isParent(
    coutputs: CompilerOutputs,
    descendantCitizenRef: ICitizenTT,
    ancestorInterfaceRef: InterfaceTT):
  IsParentResult

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

  def resolveStaticSizedArrayKind(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    mutability: ITemplata[MutabilityTemplataType],
    variability: ITemplata[VariabilityTemplataType],
    size: ITemplata[IntegerTemplataType],
    type2: CoordT):
  StaticSizedArrayTT

  def resolveRuntimeSizedArrayKind(env: IEnvironment, state: CompilerOutputs, element: CoordT, arrayMutability: ITemplata[MutabilityTemplataType]): RuntimeSizedArrayTT
}

object TemplataCompiler {
  // See SFWPRL
  def assemblePredictRules(genericParameters: Vector[GenericParameterS], numExplicitTemplateArgs: Int): Vector[IRulexSR] = {
    genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
      if (index >= numExplicitTemplateArgs) {
        genericParam.default match {
          case Some(x) => x.rules
          case None => Vector()
        }
      } else {
        Vector()
      }
    })
  }

  def assembleCallSiteRules(rules: Vector[IRulexSR], genericParameters: Vector[GenericParameterS], numExplicitTemplateArgs: Int): Vector[IRulexSR] = {
    rules.filter(InferCompiler.includeRuleInCallSiteSolve) ++
      (genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
        if (index >= numExplicitTemplateArgs) {
          genericParam.default match {
            case Some(x) => x.rules
            case None => Vector()
          }
        } else {
          Vector()
        }
      }))
  }

  def getFunctionTemplate(fullName: FullNameT[IFunctionNameT]): FullNameT[IFunctionTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def getCitizenTemplate(fullName: FullNameT[ICitizenNameT]): FullNameT[ICitizenTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.template)
  }

  def getTemplate(fullName: FullNameT[IInstantiationNameT]): FullNameT[ITemplateNameT] = {
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

  def getPlaceholderTemplate(fullName: FullNameT[PlaceholderNameT]): FullNameT[PlaceholderTemplateNameT] = {
    val FullNameT(packageCoord, initSteps, last) = fullName
    FullNameT(packageCoord, initSteps, last.templateName)
  }

  def substituteTemplatasInCoord(
    interner: Interner,
    coord: CoordT,
    substitutions: Array[(FullNameT[PlaceholderNameT], ITemplata[ITemplataType])]):
  CoordT = {
    val CoordT(ownership, kind) = coord
    substituteTemplatasInKind(interner, kind, substitutions) match {
      case KindTemplata(kind) => CoordT(ownership, kind)
      case CoordTemplata(CoordT(innerOwnership, kind)) => {
        val resultOwnership =
          (ownership, innerOwnership) match {
            case (ShareT, _) => ShareT
            case (_, ShareT) => ShareT
            case (OwnT, OwnT) => OwnT
            case (OwnT, BorrowT) => BorrowT
            case (BorrowT, OwnT) => BorrowT
            case (BorrowT, BorrowT) => BorrowT
            case _ => vimpl()
          }
        CoordT(resultOwnership, kind)
      }
    }

  }

  // This returns an ITemplata because...
  // Let's say we have a parameter that's a Coord(own, $_0).
  // $_0 is a PlaceholderT(0), which means it's a standing for whatever the first template arg is.
  // Let's say the first template arg is a CoordTemplata containing &Ship.
  // We're in the weird position of turning a PlaceholderT kind into a &Ship coord!
  // That's why we have to return an ITemplata, because it could be a coord or a kind.
  def substituteTemplatasInKind(
    interner: Interner,
    kind: KindT,
    substitutions: Array[(FullNameT[PlaceholderNameT], ITemplata[ITemplataType])]):
  ITemplata[ITemplataType] = {
    kind match {
      case IntT(bits) => KindTemplata(kind)
      case BoolT() => KindTemplata(kind)
      case StrT() => KindTemplata(kind)
      case FloatT() => KindTemplata(kind)
      case VoidT() => KindTemplata(kind)
      case NeverT(_) => KindTemplata(kind)
      case RuntimeSizedArrayTT(mutability, elementType) => {
        KindTemplata(
          RuntimeSizedArrayTT(
            expectMutability(substituteTemplatasInTemplata(interner, mutability, substitutions)),
            substituteTemplatasInCoord(interner, elementType, substitutions)))
      }
      case StaticSizedArrayTT(size, mutability, variability, elementType) => {
        KindTemplata(
          StaticSizedArrayTT(
            expectInteger(substituteTemplatasInTemplata(interner, mutability, substitutions)),
            expectMutability(substituteTemplatasInTemplata(interner, mutability, substitutions)),
            expectVariability(substituteTemplatasInTemplata(interner, variability, substitutions)),
            substituteTemplatasInCoord(interner, elementType, substitutions)))
      }
      case PlaceholderT(hayName @ FullNameT(_, _, PlaceholderNameT(PlaceholderTemplateNameT(index))))
          if index < substitutions.length && hayName == substitutions(index)._1 => {
        substitutions(index)._2
      }
      case PlaceholderT(_) => KindTemplata(kind)
      case s @ StructTT(_) => KindTemplata(substituteTemplatasInStruct(interner, s, substitutions))
      case s @ InterfaceTT(_) => KindTemplata(substituteTemplatasInInterface(interner, s, substitutions))
    }
  }

  def substituteTemplatasInStruct(
    interner: Interner,
    structTT: StructTT,
    substitutions: Array[(FullNameT[PlaceholderNameT], ITemplata[ITemplataType])]):
  StructTT = {
    val StructTT(FullNameT(packageCoord, initSteps, last)) = structTT
    interner.intern(
      StructTT(
        FullNameT(
          packageCoord,
          initSteps,
          last match {
            case StructNameT(template, templateArgs) => interner.intern(StructNameT(template, templateArgs.map(substituteTemplatasInTemplata(interner, _, substitutions))))
            case LambdaCitizenNameT(template) => interner.intern(LambdaCitizenNameT(template))
          })))
  }

  def substituteTemplatasInInterface(
    interner: Interner,
    interfaceTT: InterfaceTT,
    substitutions: Array[(FullNameT[PlaceholderNameT], ITemplata[ITemplataType])]):
  InterfaceTT = {
    val InterfaceTT(FullNameT(packageCoord, initSteps, last)) = interfaceTT
    interner.intern(
      InterfaceTT(
        FullNameT(
          packageCoord,
          initSteps,
          last match {
            case InterfaceNameT(template, templateArgs) => interner.intern(InterfaceNameT(template, templateArgs.map(substituteTemplatasInTemplata(interner, _, substitutions))))
          })))
  }

  def substituteTemplatasInTemplata(
    interner: Interner,
    templata: ITemplata[ITemplataType],
    substitutions: Array[(FullNameT[PlaceholderNameT], ITemplata[ITemplataType])]):
  ITemplata[ITemplataType] = {
    templata match {
      case CoordTemplata(c) => CoordTemplata(substituteTemplatasInCoord(interner, c, substitutions))
      case KindTemplata(k) => substituteTemplatasInKind(interner, k, substitutions)
      case p @ PlaceholderTemplata(FullNameT(_, _, PlaceholderNameT(PlaceholderTemplateNameT(index))), _)
        if index < substitutions.length && p.fullNameT == substitutions(index)._1 => {
        substitutions(index)._2
      }
      case MutabilityTemplata(_) => templata
      case VariabilityTemplata(_) => templata
      case IntegerTemplata(_) => templata
      case BooleanTemplata(_) => templata
      case other => vimpl(other)
    }
  }

  trait IPlaceholderSubstituter {
    def substituteForCoord(coordT: UnsubstitutedCoordT): CoordT = {
      substituteForCoord(coordT.unsubstitutedCoord)
    }
    def substituteForCoord(coordT: CoordT): CoordT
    def substituteForInterface(interfaceTT: InterfaceTT): InterfaceTT
    def substituteForTemplata(coordT: ITemplata[ITemplataType]): ITemplata[ITemplataType]
  }
  // Let's say you have the line:
  //   myShip.engine
  // You need to somehow combine these two bits of knowledge:
  // - You have a Ship<WarpFuel>
  // - Ship<T> contains an Engine<T>.
  // To get back an Engine<WarpFuel>. This is the function that does that.
  def getPlaceholderSubstituter(
    interner: Interner,
    // This is the Ship<WarpFuel>.
    instantiationFullName: FullNameT[IInstantiationNameT]
    // The Engine<T> is given later to the IPlaceholderSubstituter
  ):
  IPlaceholderSubstituter = {
    val template = TemplataCompiler.getTemplate(instantiationFullName)
    val substitutions =
      instantiationFullName.last.templateArgs.zipWithIndex.map({ case (arg, index) =>
        val placeholderFullName = template.addStep(interner.intern(PlaceholderNameT(interner.intern(PlaceholderTemplateNameT(index)))))
        placeholderFullName -> arg
      }).toArray
    new IPlaceholderSubstituter {
      override def substituteForCoord(coordT: CoordT): CoordT = {
        TemplataCompiler.substituteTemplatasInCoord(interner, coordT, substitutions)
      }
      override def substituteForInterface(interfaceTT: InterfaceTT): InterfaceTT = {
        TemplataCompiler.substituteTemplatasInInterface(interner, interfaceTT, substitutions)
      }
      override def substituteForTemplata(templata: ITemplata[ITemplataType]): ITemplata[ITemplataType] = {
        TemplataCompiler.substituteTemplatasInTemplata(interner, templata, substitutions)
      }
    }
  }

//  // If you have a type (citizenTT) and it contains something (like a member) then
//  // you can use this function to figure out what the member looks like to you, the outsider.
//  // It will take out all the internal placeholders internal to the citizen, and replace them
//  // with what was given in citizenTT's template args.
//  def getTemplataTransformer(interner: Interner, coutputs: CompilerOutputs, citizenTT: ICitizenTT):
//  (ITemplata[ITemplataType]) => ITemplata[ITemplataType] = {
//    val citizenTemplateFullName = TemplataCompiler.getCitizenTemplate(citizenTT.fullName)
//    val citizenTemplateDefinition = coutputs.lookupCitizen(citizenTemplateFullName)
//    vassert(
//      citizenTT.fullName.last.templateArgs.size ==
//        citizenTemplateDefinition.placeholderedCitizen.fullName.last.templateArgs.size)
//    val substitutions =
//      citizenTT.fullName.last.templateArgs
//        .zip(citizenTemplateDefinition.placeholderedCitizen.fullName.last.templateArgs)
//        .flatMap({
//          case (arg, p @ PlaceholderTemplata(_, _)) => Some((p, arg))
//          case _ => None
//        }).toArray
//    (templataToTransform: ITemplata[ITemplataType]) => {
//      TemplataCompiler.substituteTemplatasInTemplata(interner, templataToTransform, substitutions)
//    }
//  }
}

class TemplataCompiler(
  interner: Interner,
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
        delegate.isParent(coutputs, a, b) match {
          case IsParent(conclusions) =>
          case IsntParent(_) => return false
        }
      }
      case (a @ InterfaceTT(_), b @ InterfaceTT(_)) => {
        delegate.isParent(coutputs, a, b) match {
          case IsParent(conclusions) =>
          case IsntParent(_) => return false
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

  def resolveStructTemplate(structTemplata: StructTemplata): FullNameT[IStructTemplateNameT] = {
    val StructTemplata(declaringEnv, structA) = structTemplata
    declaringEnv.fullName.addStep(nameTranslator.translateStructName(structA.name))
  }

  def resolveInterfaceTemplate(interfaceTemplata: InterfaceTemplata): FullNameT[IInterfaceTemplateNameT] = {
    val InterfaceTemplata(declaringEnv, interfaceA) = interfaceTemplata
    declaringEnv.fullName.addStep(nameTranslator.translateInterfaceName(interfaceA.name))
  }

  def resolveCitizenTemplate(citizenTemplata: CitizenTemplata): FullNameT[ICitizenTemplateNameT] = {
    citizenTemplata match {
      case st @ StructTemplata(_, _) => resolveStructTemplate(st)
      case it @ InterfaceTemplata(_, _) => resolveInterfaceTemplate(it)
    }
  }

  def citizenIsFromTemplate(actualCitizenRef: ICitizenTT, expectedCitizenTemplata: ITemplata[ITemplataType]): Boolean = {
    val citizenTemplateFullName =
      expectedCitizenTemplata match {
        case st @ StructTemplata(_, _) => resolveStructTemplate(st)
        case it @ InterfaceTemplata(_, _) => resolveInterfaceTemplate(it)
        case KindTemplata(c : ICitizenTT) => getCitizenTemplate(c.fullName)
        case CoordTemplata(CoordT(OwnT | ShareT, c : ICitizenTT)) => getCitizenTemplate(c.fullName)
        case _ => return false
      }
    TemplataCompiler.getCitizenTemplate(actualCitizenRef.fullName) == citizenTemplateFullName
  }

  def createPlaceholder(
      coutputs: CompilerOutputs,
      env: IEnvironment,
      namePrefix: FullNameT[INameT],
      index: Int,
      tyype: ITemplataType):
  ITemplata[ITemplataType] = {
    val placeholderFullName =
      namePrefix.addStep(
        interner.intern(PlaceholderNameT(
          interner.intern(PlaceholderTemplateNameT(index)))))
    val placeholderTemplateFullName =
      TemplataCompiler.getPlaceholderTemplate(placeholderFullName)
    tyype match {
      case KindTemplataType() => {
        val placeholderKindT = PlaceholderT(placeholderFullName)
        coutputs.declareTemplate(placeholderTemplateFullName)
        val placeholderEnv = GeneralEnvironment.childOf(interner, env, placeholderTemplateFullName)
        coutputs.declareOuterEnvForTemplate(placeholderTemplateFullName, placeholderEnv)
        coutputs.declareInnerEnvForTemplate(placeholderTemplateFullName, placeholderEnv)
        KindTemplata(placeholderKindT)
      }
      // TODO: Not sure what to put here when we do regions. We might need to
      // flood the nearest region annotation downward, and then apply it if it's
      // a coord or something. Remembering that in every templex would be bothersome
      // though.
      // For now, we can manually add them.
      // So, I guess we could just assume the function's default region here then.
      case CoordTemplataType() => {
        val placeholderKindT = PlaceholderT(placeholderFullName)
        coutputs.declareTemplate(placeholderTemplateFullName)
        val placeholderEnv = GeneralEnvironment.childOf(interner, env, placeholderTemplateFullName)
        coutputs.declareOuterEnvForTemplate(placeholderTemplateFullName, placeholderEnv)
        coutputs.declareInnerEnvForTemplate(placeholderTemplateFullName, placeholderEnv)
        CoordTemplata(CoordT(OwnT, placeholderKindT))
      }
      case _ => PlaceholderTemplata(placeholderFullName, tyype)
    }
  }
}
