package dev.vale.typing

import dev.vale.postparsing.{IRuneS, ITemplataType}
import dev.vale.typing.ast._
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{PackageCoordinate, StrI, vassert, vassertOne, vassertSome, vcurious, vfail, vimpl}
import dev.vale.typing.ast._
import dev.vale.typing.names._
import dev.vale.typing.types._

import scala.collection.mutable

// R is ReachableFunctionNameT when we're defining, or IFunctionNameT when defining
// We reuse the same class for these differences because what the caller does should generally line up with what the
// callee does.
case class InstantiationReachableBoundArgumentsT[R <: IFunctionNameT](
  // callerPlaceholderedCitizen: ICitizenTT, // DO NOT SUBMIT

  // Let's say this struct is for main's Fub call's first argument in this:
  //   #!DeriveStructDrop struct StructWBounds<T> where func drop(T)void { ... }
  //   func IFunc<T>(b Bork<T>, x T) { drop(x) }
  //   func OFunc<T>() where func drop(T) {
  //     b = Bork<T>();
  //     Fub(b, 5);
  //   }
  // These prototypes are the ones from inside the struct, but phrased in terms of OFunc's placeholders.
  //   func Swib.bound:drop(OFunc$T)void
  // so the full instance here might be:
  //   (Bork<OFunc$T>, func Swib.bound:drop(OFunc$T)void -> func Swib.bound:drop(OFunc$T)void)
  // Later on, the instantiator will use these to supply the right instantiated functions to the callee for these calls.
  // callerPlaceholderedCalleeBoundFunctionToCallerBoundArgFunction: Map[PrototypeT[FunctionBoundNameT], PrototypeT[IFunctionNameT]] DO NOT SUBMIT

  citizenAndRuneAndReachablePrototypes: Map[IRuneS, PrototypeT[R]]
)

// R and B are ReachableFunctionNameT and FunctionBoundNameT when we're defining, or IFunctionNameT when defining
// We reuse the same class for these differences because what the caller does should generally line up with what the
// callee does.
case class InstantiationBoundArgumentsT[BF <: IFunctionNameT, RF <: IFunctionNameT, BI <: IImplNameT](
  // these will also include any bounds that are reachable from any parameters.
  // we can't reach into the instantiated type in the instantiator stage to get the PrototypeI's but in the typing
  // phase the caller refers to them phrased in terms of its own placeholders.
  // this will help us map from those caller-placeholdered functions to the ultimate ones in PrototypeI.
  // the key here is the callee function declaration which we'll fill.
  // the value here is the caller-placeholdered function
  // DO NOT SUBMIT
  calleeRuneToCallerBoundArgFunction: Map[IRuneS, PrototypeT[BF]],

  // Actually no, they'll be in here DO NOT SUBMIT
  // these are how the caller's body will refer to those bound functions inside the citizen
  // ...these are never used by the instantiator, so maybe they shouldnt be here
  // These are not us supplying things to the thing we're instantiating, this is us harvesting things from the thing
  // we're instantiating. Instantiating denizens should *pull* from the callee to introduce things into its own scope.
  callerKindRuneToReachableBoundArguments: Map[IRuneS, InstantiationReachableBoundArgumentsT[RF]],

  calleeRuneToCallerBoundArgImpl: Map[IRuneS, IdT[BI]]) {

  def getCallerRunePlaceholderedReachableFunctions(): Array[PrototypeT[RF]] = {
    callerKindRuneToReachableBoundArguments
        .values
        .flatMap(_.citizenAndRuneAndReachablePrototypes.map(_._2))
        .toArray
  }
}

case class HinputsT(
  interfaces: Vector[InterfaceDefinitionT],
  structs: Vector[StructDefinitionT],
//  emptyPackStructRef: StructTT,
  functions: Vector[FunctionDefinitionT],
//  immKindToDestructor: Map[KindT, PrototypeT],

  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  interfaceToEdgeBlueprints: Map[IdT[IInterfaceNameT], InterfaceEdgeBlueprintT],
  // The typing pass keys this by placeholdered name, and the instantiator keys this by non-placeholdered names
  interfaceToSubCitizenToEdge: Map[IdT[IInterfaceNameT], Map[IdT[ICitizenNameT], EdgeT]],

  instantiationNameToInstantiationBounds: Map[IdT[IInstantiationNameT], InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT]],

  kindExports: Vector[KindExportT],
  functionExports: Vector[FunctionExportT],
  kindExterns: Vector[KindExternT],
  functionExterns: Vector[FunctionExternT],
) {

  private val subCitizenToInterfaceToEdgeMutable = mutable.HashMap[IdT[ICitizenNameT], mutable.HashMap[IdT[IInterfaceNameT], EdgeT]]()
  interfaceToSubCitizenToEdge.foreach({ case (interface, subCitizenToEdge) =>
    subCitizenToEdge.foreach({ case (subCitizen, edge) =>
      subCitizenToInterfaceToEdgeMutable
        .getOrElseUpdate(subCitizen, mutable.HashMap[IdT[IInterfaceNameT], EdgeT]())
        .put(interface, edge)
    })
  })
  val subCitizenToInterfaceToEdge: Map[IdT[ICitizenNameT], Map[IdT[IInterfaceNameT], EdgeT]] =
    subCitizenToInterfaceToEdgeMutable.mapValues(_.toMap).toMap

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vfail() // Would need a really good reason to hash something this big

  // We override this because it was massively slowing down debugging in intelliJ which eagerly calls toString on
  // everything in scope.
  override def toString: String = "Hinputs#"

  def lookupStruct(structId: IdT[IStructNameT]): StructDefinitionT = {
    vassertSome(structs.find(_.instantiatedCitizen.id == structId))
  }

  def lookupStructByTemplate(structTemplateName: IStructTemplateNameT): StructDefinitionT = {
    vassertSome(structs.find(_.instantiatedCitizen.id.localName.template == structTemplateName))
  }

  def lookupInterfaceByTemplate(interfaceTemplateName: IInterfaceTemplateNameT): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.instantiatedCitizen.id.localName.template == interfaceTemplateName))
  }

  def lookupImplByTemplate(implTemplateName: IImplTemplateNameT): EdgeT = {
    vassertSome(interfaceToSubCitizenToEdge.flatMap(_._2.values).find(_.edgeId.localName.template == implTemplateName))
  }

  def lookupInterface(interfaceId: IdT[IInterfaceNameT]): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.instantiatedCitizen.id == interfaceId))
  }

  def lookupEdge(implId: IdT[IImplNameT]): EdgeT = {
    vassertOne(interfaceToSubCitizenToEdge.flatMap(_._2.values).find(_.edgeId == implId))
  }

  def getInstantiationBoundArgs(instantiationName: IdT[IInstantiationNameT]): InstantiationBoundArgumentsT[IFunctionNameT, IFunctionNameT, IImplNameT] = {
    vassertSome(instantiationNameToInstantiationBounds.get(instantiationName))
  }

  def lookupStructByTemplateId(structTemplateId: IdT[IStructTemplateNameT]): StructDefinitionT = {
    vassertSome(structs.find(_.templateName == structTemplateId))
  }

  def lookupInterfaceByTemplateId(interfaceTemplateId: IdT[IInterfaceTemplateNameT]): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.templateName == interfaceTemplateId))
  }

  def lookupCitizenByTemplateId(interfaceTemplateId: IdT[ICitizenTemplateNameT]): CitizenDefinitionT = {
    interfaceTemplateId match {
      case IdT(packageCoord, initSteps, t: IStructTemplateNameT) => {
        lookupStructByTemplateId(IdT(packageCoord, initSteps, t))
      }
      case IdT(packageCoord, initSteps, t: IInterfaceTemplateNameT) => {
        lookupInterfaceByTemplateId(IdT(packageCoord, initSteps, t))
      }
    }
  }

  def lookupStructByTemplateName(structTemplateName: StructTemplateNameT): StructDefinitionT = {
    vassertOne(structs.filter(_.templateName.localName == structTemplateName))
  }

  def lookupInterfaceByTemplateName(interfaceTemplateName: InterfaceTemplateNameT): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.templateName.localName == interfaceTemplateName))
  }

  def lookupFunction(signature2: SignatureT): Option[FunctionDefinitionT] = {
    functions.find(_.header.toSignature == signature2).headOption
  }

  def lookupFunction(funcTemplateName: IFunctionTemplateNameT): Option[FunctionDefinitionT] = {
    functions.find(_.header.id.localName.template == funcTemplateName).headOption
  }

  def lookupFunction(humanName: String): FunctionDefinitionT = {
    val matches = functions.filter(f => {
      f.header.id.localName match {
        case FunctionNameT(n, _, _) if n.humanName.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Function \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupStruct(humanName: String): StructDefinitionT = {
    val matches = structs.filter(s => {
      s.templateName.localName match {
        case StructTemplateNameT(n) if n.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Struct \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupImpl(
    subCitizenTT: IdT[ICitizenNameT],
    interfaceTT: IdT[IInterfaceNameT]):
  EdgeT = {
    vassertSome(
      vassertSome(interfaceToSubCitizenToEdge.get(interfaceTT))
        .get(subCitizenTT))
  }

  def lookupInterface(humanName: String): InterfaceDefinitionT = {
    val matches = interfaces.filter(s => {
      s.templateName.localName match {
        case InterfaceTemplateNameT(n) if n.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Interface \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupUserFunction(humanName: String): FunctionDefinitionT = {
    val matches =
      functions
        .filter(function => simpleNameT.unapply(function.header.id).contains(humanName))
        .filter(_.header.isUserFunction)
    if (matches.size == 0) {
      vfail("Not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def nameIsLambdaIn(name: IdT[IFunctionNameT], needleFunctionHumanName: String): Boolean = {
    val first = name.steps.head
    val lastTwo = name.steps.slice(name.steps.size - 2, name.steps.size)
    (first, lastTwo) match {
      case (
        FunctionNameT(FunctionTemplateNameT(StrI(hayFunctionHumanName), _), _, _),
        Vector(
          LambdaCitizenTemplateNameT(_),
          LambdaCallFunctionNameT(LambdaCallFunctionTemplateNameT(_, _), _, _)))
        if hayFunctionHumanName == needleFunctionHumanName => true
      case _ => false
    }
  }

  def lookupLambdasIn(needleFunctionHumanName: String): Vector[FunctionDefinitionT] = {
    functions.filter(f => nameIsLambdaIn(f.header.id, needleFunctionHumanName)).toVector
  }

  def lookupLambdaIn(needleFunctionHumanName: String): FunctionDefinitionT = {
    vassertOne(lookupLambdasIn(needleFunctionHumanName))
  }

  // def getAllNonExternFunctions: Iterable[FunctionDefinitionT] = {
  //   functions.filter(!_.header.isExtern)
  // }

  def getAllUserFunctions: Iterable[FunctionDefinitionT] = {
    functions.filter(_.header.isUserFunction)
  }
}
