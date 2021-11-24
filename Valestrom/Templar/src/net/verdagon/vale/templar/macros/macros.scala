package net.verdagon.vale.templar.macros

import net.verdagon.vale.{PackageCoordinate, RangeS}
import net.verdagon.vale.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.vale.templar.Temputs
import net.verdagon.vale.templar.ast.{FunctionHeaderT, LocationInFunctionEnvironment, ParameterT}
import net.verdagon.vale.templar.env.{FunctionEnvironment, IEnvEntry}
import net.verdagon.vale.templar.names.{CitizenTemplateNameT, FullNameT, INameT}
import net.verdagon.vale.templar.types.{CoordT, InterfaceTT, MutabilityT, RuntimeSizedArrayTT, StructTT}

trait IOnFunctionDefinedMacro {
  def onFunctionDefined()
}

trait IOnFunctionGeneratedMacro {
  def onFunctionDefined()
}

trait IFunctionBodyMacro {
//  def generatorId: String

  def generateFunctionBody(
    env: FunctionEnvironment,
    temputs: Temputs,
    generatorId: String,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  FunctionHeaderT
}

trait IOnRuntimeSizedArrayDefinedMacro {
//  def getRSASiblingEntries(
//    macroName: String, structName: FullNameT[INameT], structA: RSAA):
//  Vector[(FullNameT[INameT], IEnvEntry)]

  def getRuntimeSizedArrayChildEntries(
    rsaName: FullNameT[INameT], rsa: RuntimeSizedArrayTT, mutability: MutabilityT):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnStructDefinedMacro {
  def getStructSiblingEntries(
    macroName: String, structName: FullNameT[INameT], structA: StructA):
  Vector[(FullNameT[INameT], IEnvEntry)]

  def getStructChildEntries(
    macroName: String, structName: FullNameT[INameT], structA: StructA, mutability: MutabilityT):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnImplicitStructGeneratedMacro {
  def onImplicitStructGenerated(struct: StructTT):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnInterfaceDefinedMacro {
  def getInterfaceSiblingEntries(
    interfaceName: FullNameT[INameT], interfaceA: InterfaceA):
  Vector[(FullNameT[INameT], IEnvEntry)]

  def getInterfaceChildEntries(
    interfaceName: FullNameT[INameT], interfaceA: InterfaceA, mutability: MutabilityT):
  Vector[(FullNameT[INameT], IEnvEntry)]
}

trait IOnImplDefinedMacro {
  def getImplSiblingEntries(implName: FullNameT[INameT], implA: ImplA):
  Vector[(FullNameT[INameT], IEnvEntry)]
}
