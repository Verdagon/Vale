// From Frontend/SimplifyingPass/src/dev/vale/simplifying/Hamuts.scala
/*
package dev.vale.simplifying

import dev.vale.{PackageCoordinate, StrI, vassert, vcurious, vfail, vimpl}
import dev.vale.finalast._
import dev.vale.instantiating.ast._
import dev.vale.von.IVonData

*/
// mig: struct HamutsBoxH
/// Temporary state
pub struct HamutsBoxH<'h> {
    pub inner: HamutsH<'h>,
}
// mig: impl HamutsBoxH
/*
case class HamutsBox(var inner: Hamuts) {
*/
// mig: fn eq (realized-by-impl PartialEq)
// (Realized by `impl PartialEq for HamutsBoxH` below.)
/*
  override def equals(obj: Any): Boolean = vcurious();
*/
// mig: fn hash_code (realized-by-impl Hash)
// (Realized by `impl Hash for HamutsBoxH` below.)
/*
override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

*/
// mig: fn package_coord_to_export_name_to_function
impl<'h> HamutsBoxH<'h> {
    pub fn package_coord_to_export_name_to_function(&self) -> &ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, PrototypeH<'h>>> {
        panic!("Unimplemented: package_coord_to_export_name_to_function");
    }
}
/*
  def packageCoordToExportNameToFunction: Map[PackageCoordinate, Map[StrI, PrototypeH]] = inner.packageCoordToExportNameToFunction
*/
// mig: fn package_coord_to_export_name_to_kind
impl<'h> HamutsBoxH<'h> {
    pub fn package_coord_to_export_name_to_kind(&self) -> &ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, KindHT<'h>>> {
        panic!("Unimplemented: package_coord_to_export_name_to_kind");
    }
}
/*
  def packageCoordToExportNameToKind: Map[PackageCoordinate, Map[StrI, KindHT]] = inner.packageCoordToExportNameToKind
*/
// mig: fn package_coord_to_extern_name_to_function
impl<'h> HamutsBoxH<'h> {
    pub fn package_coord_to_extern_name_to_function(&self) -> &ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, PrototypeH<'h>>> {
        panic!("Unimplemented: package_coord_to_extern_name_to_function");
    }
}
/*
  def packageCoordToExternNameToFunction: Map[PackageCoordinate, Map[StrI, PrototypeH]] = inner.packageCoordToExternNameToFunction
*/
// mig: fn package_coord_to_extern_name_to_kind
impl<'h> HamutsBoxH<'h> {
    pub fn package_coord_to_extern_name_to_kind(&self) -> &ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, KindHT<'h>>> {
        panic!("Unimplemented: package_coord_to_extern_name_to_kind");
    }
}
/*
  def packageCoordToExternNameToKind: Map[PackageCoordinate, Map[StrI, KindHT]] = inner.packageCoordToExternNameToKind
*/
// mig: fn struct_t_to_struct_h
impl<'h> HamutsBoxH<'h> {
    pub fn struct_t_to_struct_h(&self) -> &ArenaIndexMap<'h, StructIT<'h>, StructHT<'h>> {
        panic!("Unimplemented: struct_t_to_struct_h");
    }
}
/*
  def structTToStructH: Map[StructIT[cI], StructHT] = inner.structTToStructH
*/
// mig: fn struct_t_to_struct_def_h
impl<'h> HamutsBoxH<'h> {
    pub fn struct_t_to_struct_def_h(&self) -> &ArenaIndexMap<'h, StructIT<'h>, StructDefinitionH<'h>> {
        panic!("Unimplemented: struct_t_to_struct_def_h");
    }
}
/*
  def structTToStructDefH: Map[StructIT[cI], StructDefinitionH] = inner.structTToStructDefH
*/
// mig: fn struct_defs
impl<'h> HamutsBoxH<'h> {
    pub fn struct_defs(&self) -> &'h [StructDefinitionH<'h>] {
        panic!("Unimplemented: struct_defs");
    }
}
/*
  def structDefs: Vector[StructDefinitionH] = inner.structDefs
*/
// mig: fn interface_t_to_interface_h
impl<'h> HamutsBoxH<'h> {
    pub fn interface_t_to_interface_h(&self) -> &ArenaIndexMap<'h, InterfaceIT<'h>, InterfaceHT<'h>> {
        panic!("Unimplemented: interface_t_to_interface_h");
    }
}
/*
  def interfaceTToInterfaceH: Map[InterfaceIT[cI], InterfaceHT] = inner.interfaceTToInterfaceH
*/
// mig: fn interface_t_to_interface_def_h
impl<'h> HamutsBoxH<'h> {
    pub fn interface_t_to_interface_def_h(&self) -> &ArenaIndexMap<'h, InterfaceIT<'h>, InterfaceDefinitionH<'h>> {
        panic!("Unimplemented: interface_t_to_interface_def_h");
    }
}
/*
  def interfaceTToInterfaceDefH: Map[InterfaceIT[cI], InterfaceDefinitionH] = inner.interfaceTToInterfaceDefH
*/
// mig: fn function_refs
impl<'h> HamutsBoxH<'h> {
    pub fn function_refs(&self) -> &ArenaIndexMap<'h, PrototypeI<'h>, FunctionRefH<'h>> {
        panic!("Unimplemented: function_refs");
    }
}
/*
  def functionRefs: Map[PrototypeI[cI], FunctionRefH] = inner.functionRefs
*/
// mig: fn function_defs
impl<'h> HamutsBoxH<'h> {
    pub fn function_defs(&self) -> &ArenaIndexMap<'h, PrototypeI<'h>, FunctionH<'h>> {
        panic!("Unimplemented: function_defs");
    }
}
/*
  def functionDefs: Map[PrototypeI[cI], FunctionH] = inner.functionDefs
*/
// mig: fn static_sized_arrays
impl<'h> HamutsBoxH<'h> {
    pub fn static_sized_arrays(&self) -> &ArenaIndexMap<'h, StaticSizedArrayIT<'h>, StaticSizedArrayDefinitionHT<'h>> {
        panic!("Unimplemented: static_sized_arrays");
    }
}
/*
  def staticSizedArrays: Map[StaticSizedArrayIT[cI], StaticSizedArrayDefinitionHT] = inner.staticSizedArrays
*/
// mig: fn runtime_sized_arrays
impl<'h> HamutsBoxH<'h> {
    pub fn runtime_sized_arrays(&self) -> &ArenaIndexMap<'h, RuntimeSizedArrayIT<'h>, RuntimeSizedArrayDefinitionHT<'h>> {
        panic!("Unimplemented: runtime_sized_arrays");
    }
}
/*
  def runtimeSizedArrays: Map[RuntimeSizedArrayIT[cI], RuntimeSizedArrayDefinitionHT] = inner.runtimeSizedArrays

*/
// mig: fn forward_declare_struct
impl<'h> HamutsBoxH<'h> {
    pub fn forward_declare_struct(&mut self, struct_it: StructIT<'h>, struct_ref_h: StructHT<'h>) -> () {
        panic!("Unimplemented: forward_declare_struct");
    }
}
/*
  def forwardDeclareStruct(structIT: StructIT[cI], structRefH: StructHT): Unit = {
    inner = inner.forwardDeclareStruct(structIT, structRefH)
  }

*/
// mig: fn add_struct_originating_from_typing_pass
impl<'h> HamutsBoxH<'h> {
    pub fn add_struct_originating_from_typing_pass(&mut self, struct_it: StructIT<'h>, struct_def_h: StructDefinitionH<'h>) -> () {
        panic!("Unimplemented: add_struct_originating_from_typing_pass");
    }
}
/*
  def addStructOriginatingFromTypingPass(structIT: StructIT[cI], structDefH: StructDefinitionH): Unit = {
    inner = inner.addStructOriginatingFromTypingPass(structIT, structDefH)
  }

*/
// mig: fn add_struct_originating_from_hammer
impl<'h> HamutsBoxH<'h> {
    pub fn add_struct_originating_from_hammer(&mut self, struct_def_h: StructDefinitionH<'h>) -> () {
        panic!("Unimplemented: add_struct_originating_from_hammer");
    }
}
/*
  def addStructOriginatingFromHammer(structDefH: StructDefinitionH): Unit = {
    inner = inner.addStructOriginatingFromHammer(structDefH)
  }

*/
// mig: fn forward_declare_interface
impl<'h> HamutsBoxH<'h> {
    pub fn forward_declare_interface(&mut self, interface_it: InterfaceIT<'h>, interface_ref_h: InterfaceHT<'h>) -> () {
        panic!("Unimplemented: forward_declare_interface");
    }
}
/*
  def forwardDeclareInterface(interfaceIT: InterfaceIT[cI], interfaceRefH: InterfaceHT): Unit = {
    inner = inner.forwardDeclareInterface(interfaceIT, interfaceRefH)
  }

*/
// mig: fn add_interface
impl<'h> HamutsBoxH<'h> {
    pub fn add_interface(&mut self, interface_it: InterfaceIT<'h>, interface_def_h: InterfaceDefinitionH<'h>) -> () {
        panic!("Unimplemented: add_interface");
    }
}
/*
  def addInterface(interfaceIT: InterfaceIT[cI], interfaceDefH: InterfaceDefinitionH): Unit = {
    inner = inner.addInterface(interfaceIT, interfaceDefH)
  }

*/
// mig: fn add_static_sized_array
impl<'h> HamutsBoxH<'h> {
    pub fn add_static_sized_array(&mut self, ssa_it: StaticSizedArrayIT<'h>, static_sized_array_definition_ht: StaticSizedArrayDefinitionHT<'h>) -> () {
        panic!("Unimplemented: add_static_sized_array");
    }
}
/*
  def addStaticSizedArray(ssaIT: StaticSizedArrayIT[cI], staticSizedArrayDefinitionTH: StaticSizedArrayDefinitionHT): Unit = {
    inner = inner.addStaticSizedArray(ssaIT, staticSizedArrayDefinitionTH)
  }

*/
// mig: fn add_runtime_sized_array
impl<'h> HamutsBoxH<'h> {
    pub fn add_runtime_sized_array(&mut self, rsa_it: RuntimeSizedArrayIT<'h>, runtime_sized_array_definition_ht: RuntimeSizedArrayDefinitionHT<'h>) -> () {
        panic!("Unimplemented: add_runtime_sized_array");
    }
}
/*
  def addRuntimeSizedArray(rsaIT: RuntimeSizedArrayIT[cI], runtimeSizedArrayDefinitionTH: RuntimeSizedArrayDefinitionHT): Unit = {
    inner = inner.addRuntimeSizedArray(rsaIT, runtimeSizedArrayDefinitionTH)
  }

*/
// mig: fn forward_declare_function
impl<'h> HamutsBoxH<'h> {
    pub fn forward_declare_function(&mut self, function_ref: PrototypeI<'h>, function_ref_h: FunctionRefH<'h>) -> () {
        panic!("Unimplemented: forward_declare_function");
    }
}
/*
  def forwardDeclareFunction(functionRef2: PrototypeI[cI], functionRefH: FunctionRefH): Unit = {
    inner = inner.forwardDeclareFunction(functionRef2, functionRefH)
  }

*/
// mig: fn add_function
impl<'h> HamutsBoxH<'h> {
    pub fn add_function(&mut self, function_ref: PrototypeI<'h>, function_def_h: FunctionH<'h>) -> () {
        panic!("Unimplemented: add_function");
    }
}
/*
  def addFunction(functionRef2: PrototypeI[cI], functionDefH: FunctionH): Unit = {
    inner = inner.addFunction(functionRef2, functionDefH)
  }

*/
// mig: fn add_kind_export
impl<'h> HamutsBoxH<'h> {
    pub fn add_kind_export(&mut self, kind: KindHT<'h>, package_coordinate: PackageCoordinate, exported_name: StrI<'h>) -> () {
        panic!("Unimplemented: add_kind_export");
    }
}
/*
  def addKindExport(kind: KindHT, packageCoordinate: PackageCoordinate, exportedName: StrI): Unit = {
    inner = inner.addKindExport(kind, packageCoordinate, exportedName)
  }

//  def addKindExtern(kind: KindHT, packageCoordinate: PackageCoordinate, exportedName: StrI): Unit = {
//    inner = inner.addKindExtern(kind, packageCoordinate, exportedName)
//  }

*/
// mig: fn add_function_export
impl<'h> HamutsBoxH<'h> {
    pub fn add_function_export(&mut self, prototype: PrototypeH<'h>, package_coordinate: PackageCoordinate, exported_name: StrI<'h>) -> () {
        panic!("Unimplemented: add_function_export");
    }
}
/*
  def addFunctionExport(prototype: PrototypeH, packageCoordinate: PackageCoordinate, exportedName: StrI): Unit = {
    inner = inner.addFunctionExport(prototype, packageCoordinate, exportedName)
  }

*/
// mig: fn add_function_extern
impl<'h> HamutsBoxH<'h> {
    pub fn add_function_extern(hamuts: &'h HamutsH<'h>, prototype: PrototypeH<'h>, exported_name: StrI<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_function_extern");
    }
}
/*
  def addFunctionExtern(prototype: PrototypeH, exportedName: StrI): Unit = {
    inner = inner.addFunctionExtern(prototype, exportedName)
  }

//  def getNameId(readableName: String, packageCoordinate: PackageCoordinate, parts: Vector[IVonData]): Int = {
//    val (newInner, id) = inner.getNameId(readableName, packageCoordinate, parts)
//    inner = newInner
//    id
//  }

*/
// mig: fn get_static_sized_array
/*
  def getStaticSizedArray(staticSizedArrayTH: StaticSizedArrayHT): StaticSizedArrayDefinitionHT = {
    inner.getStaticSizedArray(staticSizedArrayTH)
  }
*/
// mig: fn get_runtime_sized_array
/*
  def getRuntimeSizedArray(runtimeSizedArrayTH: RuntimeSizedArrayHT): RuntimeSizedArrayDefinitionHT = {
    inner.getRuntimeSizedArray(runtimeSizedArrayTH)
  }
}

*/
// mig: struct HamutsH
/// Arena-allocated
#[derive(PartialEq, Eq, Hash)]
pub struct HamutsH<'h> {
    pub human_name_to_full_name_to_id: ArenaIndexMap<'h, String, ArenaIndexMap<'h, String, i32>>,
    pub struct_t_to_struct_h: ArenaIndexMap<'h, StructIT<'h>, StructHT<'h>>,
    pub struct_t_to_struct_def_h: ArenaIndexMap<'h, StructIT<'h>, StructDefinitionH<'h>>,
    pub struct_defs: &'h [StructDefinitionH<'h>],
    pub static_sized_arrays: ArenaIndexMap<'h, StaticSizedArrayIT<'h>, StaticSizedArrayDefinitionHT<'h>>,
    pub runtime_sized_arrays: ArenaIndexMap<'h, RuntimeSizedArrayIT<'h>, RuntimeSizedArrayDefinitionHT<'h>>,
    pub interface_t_to_interface_h: ArenaIndexMap<'h, InterfaceIT<'h>, InterfaceHT<'h>>,
    pub interface_t_to_interface_def_h: ArenaIndexMap<'h, InterfaceIT<'h>, InterfaceDefinitionH<'h>>,
    pub function_refs: ArenaIndexMap<'h, PrototypeI<'h>, FunctionRefH<'h>>,
    pub function_defs: ArenaIndexMap<'h, PrototypeI<'h>, FunctionH<'h>>,
    pub package_coord_to_export_name_to_function: ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, PrototypeH<'h>>>,
    pub package_coord_to_export_name_to_kind: ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, KindHT<'h>>>,
    pub package_coord_to_extern_name_to_function: ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, PrototypeH<'h>>>,
    pub package_coord_to_extern_name_to_kind: ArenaIndexMap<'h, PackageCoordinate, ArenaIndexMap<'h, StrI<'h>, KindHT<'h>>>,
    pub _must_intern: MustIntern,
}
// mig: impl HamutsH
/*
case class Hamuts(
    humanNameToFullNameToId: Map[String, Map[String, Int]],
    structTToStructH: Map[StructIT[cI], StructHT],
    structTToStructDefH: Map[StructIT[cI], StructDefinitionH],
    structDefs: Vector[StructDefinitionH],
    staticSizedArrays: Map[StaticSizedArrayIT[cI], StaticSizedArrayDefinitionHT],
    runtimeSizedArrays: Map[RuntimeSizedArrayIT[cI], RuntimeSizedArrayDefinitionHT],
    interfaceTToInterfaceH: Map[InterfaceIT[cI], InterfaceHT],
    interfaceTToInterfaceDefH: Map[InterfaceIT[cI], InterfaceDefinitionH],
    functionRefs: Map[PrototypeI[cI], FunctionRefH],
    functionDefs: Map[PrototypeI[cI], FunctionH],
    packageCoordToExportNameToFunction: Map[PackageCoordinate, Map[StrI, PrototypeH]],
    packageCoordToExportNameToKind: Map[PackageCoordinate, Map[StrI, KindHT]],
    packageCoordToExternNameToFunction: Map[PackageCoordinate, Map[StrI, PrototypeH]],
    packageCoordToExternNameToKind: Map[PackageCoordinate, Map[StrI, KindHT]]) {
*/
// mig: fn eq (realized-by-impl PartialEq)
// (Realized by `impl PartialEq for HamutsH` below.)
/*
  override def equals(obj: Any): Boolean = vcurious();
*/
// mig: fn hash_code (realized-by-impl Hash)
// (Realized by `impl Hash for HamutsH` below.)
/*
override def hashCode(): Int = vfail() // Would need a really good reason to hash something this big

  vassert(functionDefs.values.map(_.id).toVector.distinct.size == functionDefs.values.size)
  vassert(structDefs.map(_.id).distinct.size == structDefs.size)
  vassert(runtimeSizedArrays.values.map(_.name).toVector.distinct.size == runtimeSizedArrays.size)

*/
// mig: fn forward_declare_struct
impl<'h> HamutsH<'h> {
    pub fn forward_declare_struct(&'h self, struct_it: StructIT<'h>, struct_ref_h: StructHT<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: forward_declare_struct");
    }
}
/*
  def forwardDeclareStruct(structIT: StructIT[cI], structRefH: StructHT): Hamuts = {
    Hamuts(
      humanNameToFullNameToId,
      structTToStructH + (structIT -> structRefH),
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_struct_originating_from_typing_pass
impl<'h> HamutsH<'h> {
    pub fn add_struct_originating_from_typing_pass(&'h self, struct_tt: StructIT<'h>, struct_def_h: StructDefinitionH<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_struct_originating_from_typing_pass");
    }
}
/*
  def addStructOriginatingFromTypingPass(structTT: StructIT[cI], structDefH: StructDefinitionH): Hamuts = {
    vassert(structTToStructH.contains(structTT))
    // structTToStructDefH.get(structTT) match {
    //   case Some(existingDef) => {
    //     // Added all this to help VmdSiteGen. Apparently it calls this method twice with the same structs sometimes?
    //     vassert(existingDef.id == structDefH.id)
    //     vassert(existingDef.members.map(_.name) == structDefH.members.map(_.name))
    //     vassert(existingDef.members.map(_.tyype) == structDefH.members.map(_.tyype))
    //     vassert(structDefs.exists(_.id == structDefH.id))
    //     this
    //   }
    //   case None => {
        Hamuts(
          humanNameToFullNameToId,
          structTToStructH,
          structTToStructDefH + (structTT -> structDefH),
          structDefs :+ structDefH,
          staticSizedArrays,
          runtimeSizedArrays,
          interfaceTToInterfaceH,
          interfaceTToInterfaceDefH,
          functionRefs,
          functionDefs,
          packageCoordToExportNameToFunction,
          packageCoordToExportNameToKind,
          packageCoordToExternNameToFunction,
          packageCoordToExternNameToKind)
      // }
    // }
  }

*/
// mig: fn add_struct_originating_from_hammer
impl<'h> HamutsH<'h> {
    pub fn add_struct_originating_from_hammer(&'h self, struct_def_h: StructDefinitionH<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_struct_originating_from_hammer");
    }
}
/*
  def addStructOriginatingFromHammer(structDefH: StructDefinitionH): Hamuts = {
    vassert(!structDefs.exists(_.id == structDefH.id))

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs :+ structDefH,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn forward_declare_interface
impl<'h> HamutsH<'h> {
    pub fn forward_declare_interface(&'h self, interface_it: InterfaceIT<'h>, interface_ref_h: InterfaceHT<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: forward_declare_interface");
    }
}
/*
  def forwardDeclareInterface(interfaceIT: InterfaceIT[cI], interfaceRefH: InterfaceHT): Hamuts = {
    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH + (interfaceIT -> interfaceRefH),
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_interface
impl<'h> HamutsH<'h> {
    pub fn add_interface(&'h self, interface_it: InterfaceIT<'h>, interface_def_h: InterfaceDefinitionH<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_interface");
    }
}
/*
  def addInterface(interfaceIT: InterfaceIT[cI], interfaceDefH: InterfaceDefinitionH): Hamuts = {
    vassert(interfaceTToInterfaceH.contains(interfaceIT))
    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH + (interfaceIT -> interfaceDefH),
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn forward_declare_function
impl<'h> HamutsH<'h> {
    pub fn forward_declare_function(&'h self, function_ref: PrototypeI<'h>, function_ref_h: FunctionRefH<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: forward_declare_function");
    }
}
/*
  def forwardDeclareFunction(functionRef2: PrototypeI[cI], functionRefH: FunctionRefH): Hamuts = {
    vassert(!functionRefs.contains(functionRef2))

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs + (functionRef2 -> functionRefH),
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_function
impl<'h> HamutsH<'h> {
    pub fn add_function(&'h self, function_ref: PrototypeI<'h>, function_def_h: FunctionH<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_function");
    }
}
/*
  def addFunction(functionRef2: PrototypeI[cI], functionDefH: FunctionH): Hamuts = {
    vassert(functionRefs.contains(functionRef2))
    functionDefs.find(_._2.id == functionDefH.id) match {
      case None =>
      case Some(existing) => {
        vfail("Internal error: Can't add function:\n" + functionRef2 + "\nbecause there's already a function with same hammer name:\b" + existing._1 + "\nHammer name:\n" + functionDefH.id)
      }
    }

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs + (functionRef2 -> functionDefH),
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_kind_export
impl<'h> HamutsH<'h> {
    pub fn add_kind_export(&'h self, kind: KindHT<'h>, package_coordinate: PackageCoordinate, exported_name: StrI<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_kind_export");
    }
}
/*
  def addKindExport(kind: KindHT, packageCoordinate: PackageCoordinate, exportedName: StrI): Hamuts = {
    val newPackageCoordToExportNameToKind =
      packageCoordToExportNameToKind.get(packageCoordinate) match {
        case None => {
          packageCoordToExportNameToKind + (packageCoordinate -> Map(exportedName -> kind))
        }
        case Some(exportNameToFullName) => {
          exportNameToFullName.get(exportedName) match {
            case None => {
              packageCoordToExportNameToKind + (packageCoordinate -> (exportNameToFullName + (exportedName -> kind)))
            }
            case Some(existingFullName) => {
              vfail("Already exported a `" + exportedName + "` from package `" + packageCoordinate + " : " + existingFullName)
            }
          }
        }
      }

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      newPackageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_function_export
impl<'h> HamutsH<'h> {
    pub fn add_function_export(&'h self, function: PrototypeH<'h>, package_coordinate: PackageCoordinate, exported_name: StrI<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_function_export");
    }
}
/*
  def addFunctionExport(function: PrototypeH, packageCoordinate: PackageCoordinate, exportedName: StrI): Hamuts = {
    val newPackageCoordToExportNameToFunction =
      packageCoordToExportNameToFunction.get(packageCoordinate) match {
        case None => {
          packageCoordToExportNameToFunction + (packageCoordinate -> Map(exportedName -> function))
        }
        case Some(exportNameToFullName) => {
          exportNameToFullName.get(exportedName) match {
            case None => {
              packageCoordToExportNameToFunction + (packageCoordinate -> (exportNameToFullName + (exportedName -> function)))
            }
            case Some(existingFullName) => {
              vfail("Already exported a `" + exportedName + "` from package `" + packageCoordinate + " : " + existingFullName)
            }
          }
        }
      }

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      newPackageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

//  def addKindExtern(kind: KindHT, packageCoordinate: PackageCoordinate, exportedName: StrI): Hamuts = {
//    val newPackageCoordToExternNameToKind =
//      packageCoordToExternNameToKind.get(packageCoordinate) match {
//        case None => {
//          packageCoordToExternNameToKind + (packageCoordinate -> Map(exportedName -> kind))
//        }
//        case Some(exportNameToFullName) => {
//          exportNameToFullName.get(exportedName) match {
//            case None => {
//              packageCoordToExternNameToKind + (packageCoordinate -> (exportNameToFullName + (exportedName -> kind)))
//            }
//            case Some(existingFullName) => {
//              vfail("Already exported a `" + exportedName + "` from package `" + packageCoordinate + " : " + existingFullName)
//            }
//          }
//        }
//      }
//
//    Hamuts(
//      humanNameToFullNameToId,
//      structTToStructH,
//      structTToStructDefH,
//      structDefs,
//      staticSizedArrays,
//      runtimeSizedArrays,
//      interfaceTToInterfaceH,
//      interfaceTToInterfaceDefH,
//      functionRefs,
//      functionDefs,
//      packageCoordToExportNameToFunction,
//      packageCoordToExportNameToKind,
//      packageCoordToExternNameToFunction,
//      newPackageCoordToExternNameToKind)
//  }

*/
// mig: fn add_function_extern
impl<'h> HamutsH<'h> {
    pub fn add_function_extern(&'h self, function: PrototypeH<'h>, exported_name: StrI<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_function_extern");
    }
}
/*
  def addFunctionExtern(function: PrototypeH, exportedName: StrI): Hamuts = {
    val packageCoordinate = function.id.packageCoordinate
    val newPackageCoordToExternNameToFunction =
      packageCoordToExternNameToFunction.get(packageCoordinate) match {
        case None => {
          packageCoordToExternNameToFunction + (packageCoordinate -> Map(exportedName -> function))
        }
        case Some(exportNameToFullName) => {
          exportNameToFullName.get(exportedName) match {
            case None => {
              packageCoordToExternNameToFunction + (packageCoordinate -> (exportNameToFullName + (exportedName -> function)))
            }
            case Some(existingFullName) => {
              vfail("Already exported a `" + exportedName + "` from package `" + packageCoordinate + " : " + existingFullName)
            }
          }
        }
      }

    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      newPackageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_static_sized_array
impl<'h> HamutsH<'h> {
    pub fn add_static_sized_array(&'h self, ssa_it: StaticSizedArrayIT<'h>, static_sized_array_definition_ht: StaticSizedArrayDefinitionHT<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_static_sized_array");
    }
}
/*
  def addStaticSizedArray(
    ssaIT: StaticSizedArrayIT[cI],
    staticSizedArrayDefinitionHT: StaticSizedArrayDefinitionHT
  ): Hamuts = {
    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays + (ssaIT -> staticSizedArrayDefinitionHT),
      runtimeSizedArrays,
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

*/
// mig: fn add_runtime_sized_array
impl<'h> HamutsH<'h> {
    pub fn add_runtime_sized_array(&'h self, rsa_it: RuntimeSizedArrayIT<'h>, runtime_sized_array_definition_ht: RuntimeSizedArrayDefinitionHT<'h>) -> &'h HamutsH<'h> {
        panic!("Unimplemented: add_runtime_sized_array");
    }
}
/*
  def addRuntimeSizedArray(
    rsaIT: RuntimeSizedArrayIT[cI],
    runtimeSizedArrayDefinitionHT: RuntimeSizedArrayDefinitionHT
  ): Hamuts = {
    Hamuts(
      humanNameToFullNameToId,
      structTToStructH,
      structTToStructDefH,
      structDefs,
      staticSizedArrays,
      runtimeSizedArrays + (rsaIT -> runtimeSizedArrayDefinitionHT),
      interfaceTToInterfaceH,
      interfaceTToInterfaceDefH,
      functionRefs,
      functionDefs,
      packageCoordToExportNameToFunction,
      packageCoordToExportNameToKind,
      packageCoordToExternNameToFunction,
      packageCoordToExternNameToKind)
  }

//  // This returns a unique ID for that specific human name.
//  // Two things with two different human names could result in the same ID here.
//  // This ID is meant to be concatenated onto the human name.
//  def getNameId(readableName: String, packageCoordinate: PackageCoordinate, parts: Vector[IVonData]): (Hamuts, Int) = {
//    val namePartsString = IdH.namePartsToString(packageCoordinate, parts)
//    val idByFullNameForHumanName =
//      humanNameToFullNameToId.get(readableName) match {
//        case None => Map[String, Int]()
//        case Some(x) => x
//      }
//    val id =
//      idByFullNameForHumanName.get(namePartsString) match {
//        case None => idByFullNameForHumanName.size
//        case Some(i) => i
//      }
//    val idByFullNameForHumanNameNew = idByFullNameForHumanName + (namePartsString -> id)
//    val idByFullNameByHumanNameNew = humanNameToFullNameToId + (readableName -> idByFullNameForHumanNameNew)
//    val newHamuts =
//      Hamuts(
//        idByFullNameByHumanNameNew,
//        structTToStructH,
//        structTToStructDefH,
//        structDefs,
//        staticSizedArrays,
//        runtimeSizedArrays,
//        interfaceTToInterfaceH,
//        interfaceTToInterfaceDefH,
//        functionRefs,
//        functionDefs,
//        packageCoordToExportNameToFunction,
//        packageCoordToExportNameToKind,
//        packageCoordToExternNameToFunction,
//        packageCoordToExternNameToKind)
//    (newHamuts, id)
//  }

*/
// mig: fn get_static_sized_array
impl<'h> HamutsH<'h> {
    pub fn get_static_sized_array(&self, static_sized_array_ht: StaticSizedArrayHT<'h>) -> &'h StaticSizedArrayDefinitionHT<'h> {
        panic!("Unimplemented: get_static_sized_array");
    }
}
/*
  def getStaticSizedArray(staticSizedArrayHT: StaticSizedArrayHT): StaticSizedArrayDefinitionHT = {
    staticSizedArrays.values.find(_.kind == staticSizedArrayHT).get
  }
*/
// mig: fn get_runtime_sized_array
impl<'h> HamutsH<'h> {
    pub fn get_runtime_sized_array(&self, runtime_sized_array_ht: RuntimeSizedArrayHT<'h>) -> &'h RuntimeSizedArrayDefinitionHT<'h> {
        panic!("Unimplemented: get_runtime_sized_array");
    }
}
/*
  def getRuntimeSizedArray(runtimeSizedArrayTH: RuntimeSizedArrayHT): RuntimeSizedArrayDefinitionHT = {
    runtimeSizedArrays.values.find(_.kind == runtimeSizedArrayTH).get
  }
}
*/
