package net.verdagon.vale.hammer

import net.verdagon.vale.metal._
import net.verdagon.vale.templar.{FullName2, IName2}
import net.verdagon.vale.templar.templata.Prototype2
import net.verdagon.vale.templar.types.{InterfaceRef2, PackT2, StructRef2}
import net.verdagon.vale.{PackageCoordinate, vassert, vfail, vimpl}
import net.verdagon.von.IVonData


case class HamutsBox(var inner: Hamuts) {

  def packageCoordToExportNameToFullName: Map[PackageCoordinate, Map[String, FullNameH]] = {
    inner.packageCoordToExportNameToFullName
  }
  def structRefsByRef2: Map[StructRef2, StructRefH] = inner.structRefsByRef2
  def structDefsByRef2: Map[StructRef2, StructDefinitionH] = inner.structDefsByRef2
  def structDefs: List[StructDefinitionH] = inner.structDefs
  def interfaceRefs: Map[InterfaceRef2, InterfaceRefH] = inner.interfaceRefs
  def interfaceDefs: Map[InterfaceRef2, InterfaceDefinitionH] = inner.interfaceDefs
  def functionRefs: Map[Prototype2, FunctionRefH] = inner.functionRefs
  def functionDefs: Map[Prototype2, FunctionH] = inner.functionDefs
  def staticSizedArrays: List[StaticSizedArrayDefinitionTH] = inner.staticSizedArrays
  def runtimeSizedArrays: List[RuntimeSizedArrayDefinitionTH] = inner.runtimeSizedArrays

  def forwardDeclareStruct(structRef2: StructRef2, structRefH: StructRefH): Unit = {
    inner = inner.forwardDeclareStruct(structRef2, structRefH)
  }

  def addStructOriginatingFromTemplar(structRef2: StructRef2, structDefH: StructDefinitionH): Unit = {
    inner = inner.addStructOriginatingFromTemplar(structRef2, structDefH)
  }

  def addStructOriginatingFromHammer(structDefH: StructDefinitionH): Unit = {
    inner = inner.addStructOriginatingFromHammer(structDefH)
  }

  def forwardDeclareInterface(interfaceRef2: InterfaceRef2, interfaceRefH: InterfaceRefH): Unit = {
    inner = inner.forwardDeclareInterface(interfaceRef2, interfaceRefH)
  }

  def addInterface(interfaceRef2: InterfaceRef2, interfaceDefH: InterfaceDefinitionH): Unit = {
    inner = inner.addInterface(interfaceRef2, interfaceDefH)
  }

  def addStaticSizedArray(staticSizedArrayDefinitionTH: StaticSizedArrayDefinitionTH): Unit = {
    inner = inner.addStaticSizedArray(staticSizedArrayDefinitionTH)
  }

  def addRuntimeSizedArray(runtimeSizedArrayDefinitionTH: RuntimeSizedArrayDefinitionTH): Unit = {
    inner = inner.addRuntimeSizedArray(runtimeSizedArrayDefinitionTH)
  }

  def forwardDeclareFunction(functionRef2: Prototype2, functionRefH: FunctionRefH): Unit = {
    inner = inner.forwardDeclareFunction(functionRef2, functionRefH)
  }

  def addFunction(functionRef2: Prototype2, functionDefH: FunctionH): Unit = {
    inner = inner.addFunction(functionRef2, functionDefH)
  }

  def addExport(fullNameH: FullNameH, packageCoordinate: PackageCoordinate, exportedName: String): Unit = {
    inner = inner.addExport(fullNameH, packageCoordinate, exportedName)
  }

  def getNameId(readableName: String, packageCoordinate: PackageCoordinate, parts: List[IVonData]): Int = {
    val (newInner, id) = inner.getNameId(readableName, packageCoordinate, parts)
    inner = newInner
    id
  }

  def getStaticSizedArray(staticSizedArrayTH: StaticSizedArrayTH): StaticSizedArrayDefinitionTH = {
    inner.getStaticSizedArray(staticSizedArrayTH)
  }
  def getRuntimeSizedArray(runtimeSizedArrayTH: RuntimeSizedArrayTH): RuntimeSizedArrayDefinitionTH = {
    inner.getRuntimeSizedArray(runtimeSizedArrayTH)
  }
}

case class Hamuts(
    idByFullNameByHumanName: Map[String, Map[String, Int]],
    packageCoordToExportNameToFullName: Map[PackageCoordinate, Map[String, FullNameH]],
    structRefsByRef2: Map[StructRef2, StructRefH],
    structDefsByRef2: Map[StructRef2, StructDefinitionH],
    structDefs: List[StructDefinitionH],
    staticSizedArrays: List[StaticSizedArrayDefinitionTH],
    runtimeSizedArrays: List[RuntimeSizedArrayDefinitionTH],
    interfaceRefs: Map[InterfaceRef2, InterfaceRefH],
    interfaceDefs: Map[InterfaceRef2, InterfaceDefinitionH],
    functionRefs: Map[Prototype2, FunctionRefH],
    functionDefs: Map[Prototype2, FunctionH]) {
  def forwardDeclareStruct(structRef2: StructRef2, structRefH: StructRefH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2 + (structRef2 -> structRefH),
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromTemplar(structRef2: StructRef2, structDefH: StructDefinitionH): Hamuts = {
    vassert(structRefsByRef2.contains(structRef2))
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2 + (structRef2 -> structDefH),
      structDefs :+ structDefH,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromHammer(structDefH: StructDefinitionH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs :+ structDefH,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareInterface(interfaceRef2: InterfaceRef2, interfaceRefH: InterfaceRefH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs + (interfaceRef2 -> interfaceRefH),
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addInterface(interfaceRef2: InterfaceRef2, interfaceDefH: InterfaceDefinitionH): Hamuts = {
    vassert(interfaceRefs.contains(interfaceRef2))
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs + (interfaceRef2 -> interfaceDefH),
      functionRefs,
      functionDefs)
  }

  def forwardDeclareFunction(functionRef2: Prototype2, functionRefH: FunctionRefH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs + (functionRef2 -> functionRefH),
      functionDefs)
  }

  def addFunction(functionRef2: Prototype2, functionDefH: FunctionH): Hamuts = {
    vassert(functionRefs.contains(functionRef2))

    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs + (functionRef2 -> functionDefH))
  }

  def addExport(fullNameH: FullNameH, packageCoordinate: PackageCoordinate, exportedName: String): Hamuts = {
    val newExportNameToFullName =
      packageCoordToExportNameToFullName.get(packageCoordinate) match {
        case None => {
          packageCoordToExportNameToFullName + (packageCoordinate -> Map(exportedName -> fullNameH))
        }
        case Some(exportNameToFullName) => {
          exportNameToFullName.get(exportedName) match {
            case None => {
              packageCoordToExportNameToFullName + (packageCoordinate -> (exportNameToFullName + (exportedName -> fullNameH)))
            }
            case Some(existingFullName) => {
              vfail("Already exported a `" + exportedName + "` from package `" + packageCoordinate + " : " + existingFullName)
            }
          }
        }
      }

    Hamuts(
      idByFullNameByHumanName,
      newExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStaticSizedArray(staticSizedArrayDefinitionTH: StaticSizedArrayDefinitionTH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays :+ staticSizedArrayDefinitionTH,
      runtimeSizedArrays,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addRuntimeSizedArray(runtimeSizedArrayDefinitionTH: RuntimeSizedArrayDefinitionTH): Hamuts = {
    Hamuts(
      idByFullNameByHumanName,
      packageCoordToExportNameToFullName,
      structRefsByRef2,
      structDefsByRef2,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays :+ runtimeSizedArrayDefinitionTH,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  // This returns a unique ID for that specific human name.
  // Two things with two different human names could result in the same ID here.
  // This ID is meant to be concatenated onto the human name.
  def getNameId(readableName: String, packageCoordinate: PackageCoordinate, parts: List[IVonData]): (Hamuts, Int) = {
    val namePartsString = FullNameH.namePartsToString(packageCoordinate, parts)
    val idByFullNameForHumanName =
      idByFullNameByHumanName.get(readableName) match {
        case None => Map[String, Int]()
        case Some(x) => x
      }
    val id =
      idByFullNameForHumanName.get(namePartsString) match {
        case None => idByFullNameForHumanName.size
        case Some(i) => i
      }
    val idByFullNameForHumanNameNew = idByFullNameForHumanName + (namePartsString -> id)
    val idByFullNameByHumanNameNew = idByFullNameByHumanName + (readableName -> idByFullNameForHumanNameNew)
    val newHamuts =
      Hamuts(
        idByFullNameByHumanNameNew,
        packageCoordToExportNameToFullName,
        structRefsByRef2,
        structDefsByRef2,
        structDefs,
        staticSizedArrays,
        runtimeSizedArrays,
        interfaceRefs,
        interfaceDefs,
        functionRefs,
        functionDefs)
    (newHamuts, id)
  }

  def getStaticSizedArray(staticSizedArrayTH: StaticSizedArrayTH): StaticSizedArrayDefinitionTH = {
    staticSizedArrays.find(_.name == staticSizedArrayTH.name).get
  }
  def getRuntimeSizedArray(runtimeSizedArrayTH: RuntimeSizedArrayTH): RuntimeSizedArrayDefinitionTH = {
    runtimeSizedArrays.find(_.name == runtimeSizedArrayTH.name).get
  }

}
