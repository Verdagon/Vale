package net.verdagon.vale.templar.macros

import net.verdagon.vale.{PackageCoordinate, RangeS}
import net.verdagon.vale.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.vale.templar.Temputs
import net.verdagon.vale.templar.ast.{FunctionHeaderT, LocationInFunctionEnvironment, ParameterT}
import net.verdagon.vale.templar.env.{IEnvEntry, FunctionEnvironment}
import net.verdagon.vale.templar.names.{CitizenTemplateNameT, FullNameT, INameT}
import net.verdagon.vale.templar.types.{CoordT, InterfaceTT, StructTT}

trait IOnFunctionDefinedMacro {
  def onFunctionDefined()
}

trait IOnFunctionGeneratedMacro {
  def onFunctionDefined()
}

trait IFunctionBodyMacro {
  def generatorId: String

  def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT
}

trait IOnStructDefinedMacro {
  def macroName: String

  def getStructSiblingEntries(
    structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], IEnvEntry)]

  def getStructChildEntries(
    structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnImplicitStructGeneratedMacro {
  def onImplicitStructGenerated(struct: StructTT):
  Vector[(FullNameT[INameT], IEnvEntry)]
}
//
//trait IOnStructGeneratedMacro {
//// make this one:
////  // If it's immutable, make sure there's a zero-arg destructor.
////  if (mutability == ImmutableT) {
////    temputs.addDestructor(
////      structDefT.getRef,
////      delegate.makeImmConcreteDestructor(temputs, structInnerEnv, structDefT.getRef))
////  }
//
//  def onStructGenerated(ref: StructTT)
//}

//trait IOnInterfaceGeneratedMacro {
//
//  // add this:
////  // If it's immutable, make sure there's a zero-arg destructor.
////  if (mutability == ImmutableT) {
////    temputs.addDestructor(
////      interfaceDef2.getRef,
////      delegate.getImmInterfaceDestructor(temputs, interfaceInnerEnv, interfaceDef2.getRef))
////  }
//  def onInterfaceGenerated(interface: InterfaceTT)
//}

trait IOnInterfaceDefinedMacro {
  def macroName: String

  def getInterfaceSiblingEntries(
    interfaceName: FullNameT[INameT], interfaceA: InterfaceA):
  Vector[(FullNameT[INameT], IEnvEntry)]

  def getInterfaceChildEntries(
    interfaceName: FullNameT[INameT], interfaceA: InterfaceA):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnImplDefinedMacro {
  def getImplSiblingEntries(implName: FullNameT[INameT], implA: ImplA):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

//trait IOnImplGeneratedMacro {
//
////  implementedInterfaceRefs2.foreach({
////    case (implementedInterfaceRefT) => {
////      structDefT.mutability match {
////        case MutableT => {
////          delegate.scoutExpectedFunctionForPrototype(
////            structInnerEnv,
////            temputs,
////            structA.range,
////            GlobalFunctionFamilyNameS(CallTemplar.MUT_INTERFACE_DESTRUCTOR_NAME),
////            Vector.empty,
////            Array.empty,
////            Vector(ParamFilter(CoordT(OwnT,ReadwriteT, structDefT.getRef), Some(OverrideT(implementedInterfaceRefT)))),
////            Vector.empty,
////            true)
////        }
////        case ImmutableT => {
////          // If it's immutable, make sure there's a zero-arg destructor.
////          delegate.getImmInterfaceDestructorOverride(temputs, structInnerEnv, structDefT.getRef, implementedInterfaceRefT)
////        }
////      }
////    }
////  })
//
//  def onImplGenerated(struct: StructTT, interface: InterfaceTT)
//}