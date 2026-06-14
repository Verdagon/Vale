// From Frontend/SimplifyingPass/src/dev/vale/simplifying/VonHammer.scala
//
// Per typing-pass `Compiler` precedent, `VonHammer` is not a Rust struct.
// Methods become `impl Hammer { ... }` blocks colocated here.

use crate::final_ast::ast::{
    EdgeH, FunctionH, FunctionRefH, IdH, InterfaceDefinitionH, InterfaceMethodH, PackageH,
    ProgramH, PrototypeH, RegionH, StructDefinitionH, StructMemberH,
};
use crate::final_ast::instructions::{ExpressionH, Local, VariableIdH};
use crate::final_ast::types::{
    CodeLocation, CoordH, InterfaceHT, KindHT, LocationH, Mutability, OwnershipH,
    RuntimeSizedArrayDefinitionHT, SimpleId, SimpleIdStep, StaticSizedArrayDefinitionHT, StructHT,
    Variability,
};
use crate::utils::code_hierarchy::PackageCoordinate;
use crate::utils::range::{CodeLocationS, RangeS};
use crate::von::ast::{IVonData, VonMember};
use crate::simplifying::hammer::Hammer;
use crate::final_ast::instructions::ArgumentH;
use crate::final_ast::instructions::ArrayCapacityH;
use crate::final_ast::instructions::ArrayLengthH;
use crate::final_ast::instructions::AsSubtypeH;
use crate::final_ast::instructions::BlockH;
use crate::final_ast::instructions::BorrowToWeakH;
use crate::final_ast::instructions::CallH;
use crate::final_ast::instructions::ConsecutorH;
use crate::final_ast::instructions::ConstantBoolH;
use crate::final_ast::instructions::ConstantF64H;
use crate::final_ast::instructions::ConstantIntH;
use crate::final_ast::instructions::ConstantStrH;
use crate::final_ast::instructions::DestroyH;
use crate::final_ast::instructions::DestroyMutRuntimeSizedArrayH;
use crate::final_ast::instructions::DestroyStaticSizedArrayIntoFunctionH;
use crate::final_ast::instructions::DestroyStaticSizedArrayIntoLocalsH;
use crate::final_ast::instructions::DiscardH;
use crate::final_ast::instructions::ExternCallH;
use crate::final_ast::instructions::IfH;
use crate::final_ast::instructions::InterfaceCallH;
use crate::final_ast::instructions::IsSameInstanceH;
use crate::final_ast::instructions::LocalLoadH;
use crate::final_ast::instructions::LocalStoreH;
use crate::final_ast::instructions::LockWeakH;
use crate::final_ast::instructions::MemberLoadH;
use crate::final_ast::instructions::MemberStoreH;
use crate::final_ast::instructions::NewArrayFromValuesH;
use crate::final_ast::instructions::NewImmRuntimeSizedArrayH;
use crate::final_ast::instructions::NewMutRuntimeSizedArrayH;
use crate::final_ast::instructions::NewStructH;
use crate::final_ast::instructions::PopRuntimeSizedArrayH;
use crate::final_ast::instructions::PushRuntimeSizedArrayH;
use crate::final_ast::instructions::RestackifyH;
use crate::final_ast::instructions::ReturnH;
use crate::final_ast::instructions::RuntimeSizedArrayLoadH;
use crate::final_ast::instructions::RuntimeSizedArrayStoreH;
use crate::final_ast::instructions::StackifyH;
use crate::final_ast::instructions::StaticArrayFromCallableH;
use crate::final_ast::instructions::StaticSizedArrayLoadH;
use crate::final_ast::instructions::StructToInterfaceUpcastH;
use crate::final_ast::instructions::UnstackifyH;
use crate::final_ast::instructions::WhileH;
use crate::final_ast::types::HamutsFunctionExtern;
use crate::final_ast::types::HamutsKindExtern;
use crate::final_ast::types::IntHT;
use crate::final_ast::types::StaticSizedArrayHT;
use crate::simplifying::name_hammer::translate_package_coordinate;
use crate::von::ast::VonArray;
use crate::von::ast::VonBool;
use crate::von::ast::VonFloat;
use crate::von::ast::VonInt;
use crate::von::ast::VonObject;
use crate::von::ast::VonStr;



// mig: fn vonify_program
impl<'s, 'i, 'h, 'ctx> Hammer<'s, 'i, 'h, 'ctx>
where 's: 'h, 's: 'i, 'i: 'h,
{
    pub fn vonify_program(&self, program: &ProgramH<'s, 'h>) -> IVonData {
        let ProgramH { packages } = program;
        IVonData::Object(VonObject {
            tyype: "Program".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "packages".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: packages.package_coord_to_contents.iter().map(|(package_coord, paackage)| {
                            IVonData::Object(VonObject {
                                tyype: "Entry".to_string(),
                                id: None,
                                members: vec![
                                    VonMember {
                                        field_name: "packageCoordinate".to_string(),
                                        value: IVonData::Object(translate_package_coordinate(*package_coord)),
                                    },
                                    VonMember {
                                        field_name: "package".to_string(),
                                        value: self.vonify_package(**package_coord, paackage),
                                    },
                                ],
                            })
                        }).collect(),
                    }),
                },
            ],
        })
    }


// mig: fn vonify_simple_id
    pub fn vonify_simple_id(&self, simple_id: SimpleId<'s, 'h>) -> IVonData {
        IVonData::object(
            "Id".to_string(),
            vec![
                VonMember::new(
                    "steps".to_string(),
                    IVonData::array(
                        simple_id.steps.iter().map(|step| self.vonify_simple_id_step(*step)).collect(),
                    ),
                ),
            ],
        )
    }


// mig: fn vonify_simple_id_step
    pub fn vonify_simple_id_step(&self, step: SimpleIdStep<'s, 'h>) -> IVonData {
        let SimpleIdStep { name, template_args } = step;
        IVonData::object(
            "IdStep".to_string(),
            vec![
                VonMember::new("name".to_string(), IVonData::str(name.0.to_string())),
                VonMember::new(
                    "templateArgs".to_string(),
                    IVonData::array(
                        template_args.iter().map(|a| self.vonify_simple_id(*a)).collect(),
                    ),
                ),
            ],
        )
    }


// mig: fn vonify_package
    pub fn vonify_package(
        &self,
        package_coord: PackageCoordinate<'s>,
        paackage: &PackageH<'s, 'h>,
    ) -> IVonData {
        let PackageH {
            interfaces, structs, functions, static_sized_arrays, runtime_sized_arrays,
            export_name_to_function, export_name_to_kind, prototype_to_extern, kind_to_extern,
        } = paackage;
        IVonData::Object(VonObject {
            tyype: "Package".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "packageCoordinate".to_string(),
                    value: IVonData::Object(translate_package_coordinate(&package_coord)),
                },
                VonMember {
                    field_name: "interfaces".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: interfaces.iter().map(|i| self.vonify_interface_def(i)).collect(),
                    }),
                },
                VonMember {
                    field_name: "structs".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: structs.iter().map(|s| self.vonify_struct(s)).collect(),
                    }),
                },
                VonMember {
                    field_name: "functions".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: functions.iter().map(|f| self.vonify_function(f)).collect(),
                    }),
                },
                VonMember {
                    field_name: "staticSizedArrays".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: static_sized_arrays.iter().map(|a| self.vonify_static_sized_array_definition(a)).collect(),
                    }),
                },
                VonMember {
                    field_name: "runtimeSizedArrays".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: runtime_sized_arrays.iter().map(|a| self.vonify_runtime_sized_array_definition(a)).collect(),
                    }),
                },
                VonMember {
                    field_name: "exportNameToFunction".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: export_name_to_function.iter().map(|(export_name, prototype)| {
                            IVonData::Object(VonObject {
                                tyype: "Entry".to_string(),
                                id: None,
                                members: vec![
                                    VonMember {
                                        field_name: "exportName".to_string(),
                                        value: IVonData::Str(VonStr { value: export_name.0.to_string() }),
                                    },
                                    VonMember {
                                        field_name: "prototype".to_string(),
                                        value: self.vonify_prototype(prototype),
                                    },
                                ],
                            })
                        }).collect(),
                    }),
                },
                VonMember {
                    field_name: "exportNameToKind".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: export_name_to_kind.iter().map(|(export_name, kind)| {
                            IVonData::Object(VonObject {
                                tyype: "Entry".to_string(),
                                id: None,
                                members: vec![
                                    VonMember {
                                        field_name: "exportName".to_string(),
                                        value: IVonData::Str(VonStr { value: export_name.0.to_string() }),
                                    },
                                    VonMember {
                                        field_name: "kind".to_string(),
                                        value: self.vonify_kind(*kind),
                                    },
                                ],
                            })
                        }).collect(),
                    }),
                },
                VonMember {
                    field_name: "prototypeToExtern".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: prototype_to_extern.iter().map(|(_, extern_)| {
                            let HamutsFunctionExtern { maybe_extern_name, prototype, simple_id } = extern_;
                            IVonData::Object(VonObject {
                                tyype: "ExternFunction".to_string(),
                                id: None,
                                members: vec![
                                    VonMember {
                                        field_name: "mangledName".to_string(),
                                        value: IVonData::Str(VonStr { value: maybe_extern_name.0.to_string() }),
                                    },
                                    VonMember {
                                        field_name: "id".to_string(),
                                        value: self.vonify_simple_id(*simple_id),
                                    },
                                    VonMember {
                                        field_name: "prototype".to_string(),
                                        value: self.vonify_prototype(prototype),
                                    },
                                ],
                            })
                        }).collect(),
                    }),
                },
                VonMember {
                    field_name: "kindToExtern".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: kind_to_extern.iter().map(|(_, extern_)| {
                            let HamutsKindExtern { maybe_extern_name, kind, simple_id } = extern_;
                            IVonData::Object(VonObject {
                                tyype: "ExternKind".to_string(),
                                id: None,
                                members: vec![
                                    VonMember {
                                        field_name: "mangledName".to_string(),
                                        value: IVonData::Str(VonStr { value: maybe_extern_name.0.to_string() }),
                                    },
                                    VonMember {
                                        field_name: "id".to_string(),
                                        value: self.vonify_simple_id(*simple_id),
                                    },
                                    VonMember {
                                        field_name: "kind".to_string(),
                                        value: self.vonify_kind(*kind),
                                    },
                                ],
                            })
                        }).collect(),
                    }),
                },
            ],
        })
    }


// mig: fn vonify_region
    pub fn vonify_region(&self, region: RegionH) -> IVonData {
        panic!("Unimplemented: vonify_region");
    }


// mig: fn vonify_struct_h
    pub fn vonify_struct_h(&self, r#ref: &StructHT<'s, 'h>) -> IVonData {
        let StructHT { id: full_name, .. } = *r#ref;
        IVonData::Object(VonObject {
            tyype: "StructId".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(full_name) },
            ],
        })
    }


// mig: fn vonify_interface (Scala overload — by InterfaceHT)
    pub fn vonify_interface(&self, r#ref: &InterfaceHT<'s, 'h>) -> IVonData {
        let InterfaceHT { id: full_name, .. } = *r#ref;
        IVonData::Object(VonObject {
            tyype: "InterfaceId".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(full_name) },
            ],
        })
    }


// mig: fn vonify_interface_method
    pub fn vonify_interface_method(
        &self,
        interface_method_h: &InterfaceMethodH<'s, 'h>,
    ) -> IVonData {
        let InterfaceMethodH { prototype_h: prototype, virtual_param_index } = *interface_method_h;
        IVonData::Object(VonObject {
            tyype: "InterfaceMethod".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "prototype".to_string(), value: self.vonify_prototype(prototype) },
                VonMember { field_name: "virtualParamIndex".to_string(), value: IVonData::Int(VonInt { value: virtual_param_index as i64 }) },
            ],
        })
    }


// mig: fn vonify_interface_def (Scala overload — by InterfaceDefinitionH;
// disambiguated per overload-suffix pattern.)
    pub fn vonify_interface_def(&self, interface: &InterfaceDefinitionH<'s, 'h>) -> IVonData {
        let InterfaceDefinitionH { id: full_name, weakable, mutability, super_interfaces, methods: prototypes } = *interface;
        IVonData::Object(VonObject {
            tyype: "Interface".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(full_name) },
                VonMember { field_name: "kind".to_string(), value: self.vonify_interface(interface.get_ref(self.interner)) },
                VonMember { field_name: "weakable".to_string(), value: IVonData::Bool(VonBool { value: weakable }) },
                VonMember { field_name: "mutability".to_string(), value: self.vonify_mutability(mutability) },
                VonMember { field_name: "superInterfaces".to_string(), value: IVonData::Array(VonArray { id: None, members: super_interfaces.iter().map(|i| self.vonify_interface(i)).collect() }) },
                VonMember { field_name: "methods".to_string(), value: IVonData::Array(VonArray { id: None, members: prototypes.iter().map(|p| self.vonify_interface_method(p)).collect() }) },
            ],
        })
    }


// mig: fn vonify_struct
    pub fn vonify_struct(&self, r#struct: &StructDefinitionH<'s, 'h>) -> IVonData {
        let StructDefinitionH { id: full_name, weakable, extern_, mutability, edges, members } = *r#struct;
        IVonData::Object(VonObject {
            tyype: "Struct".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(full_name) },
                VonMember { field_name: "kind".to_string(), value: self.vonify_struct_h(r#struct.get_ref(self.interner)) },
                VonMember { field_name: "weakable".to_string(), value: IVonData::Bool(VonBool { value: weakable }) },
                VonMember { field_name: "extern".to_string(), value: IVonData::Bool(VonBool { value: extern_ }) },
                VonMember { field_name: "mutability".to_string(), value: self.vonify_mutability(mutability) },
                VonMember { field_name: "edges".to_string(), value: IVonData::Array(VonArray { id: None, members: edges.iter().map(|edge| self.vonify_edge(edge)).collect() }) },
                VonMember { field_name: "members".to_string(), value: IVonData::Array(VonArray { id: None, members: members.iter().map(|m| self.vonify_struct_member(m)).collect() }) },
            ],
        })
    }


// mig: fn vonify_mutability
    pub fn vonify_mutability(&self, mutability: Mutability) -> IVonData {
        match mutability {
            Mutability::Immutable => IVonData::Object(VonObject { tyype: "Immutable".to_string(), id: None, members: vec![] }),
            Mutability::Mutable => IVonData::Object(VonObject { tyype: "Mutable".to_string(), id: None, members: vec![] }),
        }
    }


// mig: fn vonify_location
    pub fn vonify_location(&self, location: LocationH) -> IVonData {
        match location {
            LocationH::InlineH => IVonData::Object(VonObject { tyype: "Inline".to_string(), id: None, members: vec![] }),
            LocationH::YonderH => IVonData::Object(VonObject { tyype: "Yonder".to_string(), id: None, members: vec![] }),
        }
    }


// mig: fn vonify_variability
    pub fn vonify_variability(&self, variability: Variability) -> IVonData {
        match variability {
            Variability::Varying => IVonData::Object(VonObject { tyype: "Varying".to_string(), id: None, members: vec![] }),
            Variability::Final => IVonData::Object(VonObject { tyype: "Final".to_string(), id: None, members: vec![] }),
        }
    }


// mig: fn vonify_prototype
    pub fn vonify_prototype(&self, prototype: &PrototypeH<'s, 'h>) -> IVonData {
        let PrototypeH { id: full_name, params, return_type, _must_intern: _ } = prototype;
        IVonData::Object(VonObject {
            tyype: "Prototype".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "name".to_string(),
                    value: self.vonify_name(full_name),
                },
                VonMember {
                    field_name: "params".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: params.iter().map(|c| self.vonify_coord(*c)).collect(),
                    }),
                },
                VonMember {
                    field_name: "return".to_string(),
                    value: self.vonify_coord(*return_type),
                },
            ],
        })
    }


// mig: fn vonify_coord
    pub fn vonify_coord(&self, coord: CoordH<'s, 'h>) -> IVonData {
        let CoordH { ownership, location, kind } = coord;
        IVonData::Object(VonObject {
            tyype: "Ref".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "ownership".to_string(),
                    value: self.vonify_ownership(ownership),
                },
                VonMember {
                    field_name: "location".to_string(),
                    value: self.vonify_location(location),
                },
                VonMember {
                    field_name: "kind".to_string(),
                    value: self.vonify_kind(kind),
                },
            ],
        })
    }


// mig: fn vonify_edge
    pub fn vonify_edge(&self, edge_h: &EdgeH<'s, 'h>) -> IVonData {
        let EdgeH { struct_, interface, struct_prototypes_by_interface_method } = *edge_h;
        IVonData::Object(VonObject {
            tyype: "Edge".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "structName".to_string(), value: self.vonify_struct_h(struct_) },
                VonMember { field_name: "interfaceName".to_string(), value: self.vonify_interface(interface) },
                VonMember {
                    field_name: "methods".to_string(),
                    value: IVonData::Array(VonArray {
                        id: None,
                        members: struct_prototypes_by_interface_method.iter().map(|(interface_method, struct_prototype)| {
                            IVonData::Object(VonObject {
                                tyype: "Entry".to_string(),
                                id: None,
                                members: vec![
                                    VonMember { field_name: "method".to_string(), value: self.vonify_interface_method(interface_method) },
                                    VonMember { field_name: "override".to_string(), value: self.vonify_prototype(struct_prototype) },
                                ],
                            })
                        }).collect(),
                    }),
                },
            ],
        })
    }


// mig: fn vonify_ownership
    pub fn vonify_ownership(&self, ownership: OwnershipH) -> IVonData {
        match ownership {
            OwnershipH::OwnH => IVonData::Object(VonObject { tyype: "Own".to_string(), id: None, members: vec![] }),
            OwnershipH::ImmutableBorrowH => IVonData::Object(VonObject { tyype: "ImmutableBorrow".to_string(), id: None, members: vec![] }),
            OwnershipH::MutableBorrowH => IVonData::Object(VonObject { tyype: "MutableBorrow".to_string(), id: None, members: vec![] }),
            OwnershipH::ImmutableShareH => IVonData::Object(VonObject { tyype: "ImmutableShare".to_string(), id: None, members: vec![] }),
            OwnershipH::MutableShareH => IVonData::Object(VonObject { tyype: "MutableShare".to_string(), id: None, members: vec![] }),
            OwnershipH::WeakH => IVonData::Object(VonObject { tyype: "Weak".to_string(), id: None, members: vec![] }),
        }
    }


// mig: fn vonify_struct_member
    pub fn vonify_struct_member(&self, struct_member_h: &StructMemberH<'s, 'h>) -> IVonData {
        let StructMemberH { name, variability, tyype } = *struct_member_h;
        IVonData::Object(VonObject {
            tyype: "StructMember".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "fullName".to_string(), value: self.vonify_name(name) },
                VonMember { field_name: "name".to_string(), value: IVonData::Str(VonStr { value: name.local_name.0.to_string() }) },
                VonMember { field_name: "variability".to_string(), value: self.vonify_variability(variability) },
                VonMember { field_name: "type".to_string(), value: self.vonify_coord(tyype) },
            ],
        })
    }


// mig: fn vonify_runtime_sized_array_definition
    pub fn vonify_runtime_sized_array_definition(
        &self,
        rsa_def: &RuntimeSizedArrayDefinitionHT<'s, 'h>,
    ) -> IVonData {
        let RuntimeSizedArrayDefinitionHT { name, mutability, element_type } = *rsa_def;
        IVonData::Object(VonObject {
            tyype: "RuntimeSizedArrayDefinition".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(name) },
                VonMember { field_name: "kind".to_string(), value: self.vonify_kind(KindHT::RuntimeSizedArrayHT(rsa_def.kind(self.interner))) },
                VonMember { field_name: "mutability".to_string(), value: self.vonify_mutability(mutability) },
                VonMember { field_name: "elementType".to_string(), value: self.vonify_coord(element_type) },
            ],
        })
    }


// mig: fn vonify_static_sized_array_definition
    pub fn vonify_static_sized_array_definition(
        &self,
        ssa_def: &StaticSizedArrayDefinitionHT<'s, 'h>,
    ) -> IVonData {
        let StaticSizedArrayDefinitionHT { name, size, mutability, variability, element_type } = *ssa_def;
        IVonData::Object(VonObject {
            tyype: "StaticSizedArrayDefinition".to_string(),
            id: None,
            members: vec![
                VonMember { field_name: "name".to_string(), value: self.vonify_name(name) },
                VonMember { field_name: "kind".to_string(), value: self.vonify_kind(KindHT::StaticSizedArrayHT(ssa_def.kind(self.interner))) },
                VonMember { field_name: "size".to_string(), value: IVonData::Int(VonInt { value: size }) },
                VonMember { field_name: "mutability".to_string(), value: self.vonify_mutability(mutability) },
                VonMember { field_name: "variability".to_string(), value: self.vonify_variability(variability) },
                VonMember { field_name: "elementType".to_string(), value: self.vonify_coord(element_type) },
            ],
        })
    }


// mig: fn vonify_kind
    pub fn vonify_kind(&self, kind: KindHT<'s, 'h>) -> IVonData {
        match kind {
            KindHT::NeverHT(_) => IVonData::Object(VonObject { tyype: "Never".to_string(), id: None, members: vec![] }),
            KindHT::IntHT(IntHT { bits }) => IVonData::Object(VonObject {
                tyype: "Int".to_string(),
                id: None,
                members: vec![
                    VonMember {
                        field_name: "bits".to_string(),
                        value: IVonData::Int(VonInt { value: bits as i64 }),
                    },
                ],
            }),
            KindHT::BoolHT(_) => IVonData::Object(VonObject { tyype: "Bool".to_string(), id: None, members: vec![] }),
            KindHT::StrHT(_) => IVonData::Object(VonObject { tyype: "Str".to_string(), id: None, members: vec![] }),
            KindHT::VoidHT(_) => IVonData::Object(VonObject { tyype: "Void".to_string(), id: None, members: vec![] }),
            KindHT::FloatHT(_) => IVonData::Object(VonObject { tyype: "Float".to_string(), id: None, members: vec![] }),
            KindHT::InterfaceHT(ir) => self.vonify_interface(ir),
            KindHT::StructHT(sr) => self.vonify_struct_h(sr),
            KindHT::RuntimeSizedArrayHT(rsa) => IVonData::Object(VonObject {
                tyype: "RuntimeSizedArray".to_string(),
                id: None,
                members: vec![
                    VonMember {
                        field_name: "name".to_string(),
                        value: self.vonify_name(rsa.name),
                    },
                ],
            }),
            KindHT::StaticSizedArrayHT(StaticSizedArrayHT { id: name, _must_intern: _ }) => IVonData::Object(VonObject {
                tyype: "StaticSizedArray".to_string(),
                id: None,
                members: vec![
                    VonMember {
                        field_name: "name".to_string(),
                        value: self.vonify_name(name),
                    },
                ],
            }),
            KindHT::OpaqueHT(opaque) => IVonData::Object(VonObject {
                tyype: "Opaque".to_string(),
                id: None,
                members: vec![
                    VonMember {
                        field_name: "structId".to_string(),
                        value: self.vonify_name(opaque.struct_id),
                    },
                    VonMember {
                        field_name: "structSimpleId".to_string(),
                        value: self.vonify_simple_id(opaque.simple_id),
                    },
                ],
            }),
        }
    }


// mig: fn vonify_function
    pub fn vonify_function(&self, function_h: &FunctionH<'s, 'h>) -> IVonData {
        let FunctionH { prototype, is_abstract: _, is_extern: _, attributes: _, body } = function_h;
        IVonData::Object(VonObject {
            tyype: "Function".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "prototype".to_string(),
                    value: self.vonify_prototype(prototype),
                },
                VonMember {
                    field_name: "block".to_string(),
                    value: self.vonify_expression(*body),
                },
            ],
        })
    }


// mig: fn vonify_expression
    pub fn vonify_expression(&self, node: ExpressionH<'s, 'h>) -> IVonData {
        match node {
            ExpressionH::ConstantVoidH(_) => {
                IVonData::Object(VonObject {
                    tyype: "ConstantVoid".to_string(),
                    id: None,
                    members: vec![],
                })
            }
            ExpressionH::ConstantBoolH(c) => {
                let ConstantBoolH { value } = *c;
                IVonData::Object(VonObject {
                    tyype: "ConstantBool".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "value".to_string(),
                            value: IVonData::Bool(VonBool { value }),
                        },
                    ],
                })
            }
            ExpressionH::ConstantIntH(c) => {
                let ConstantIntH { value, bits } = *c;
                IVonData::Object(VonObject {
                    tyype: "ConstantInt".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "value".to_string(),
                            value: IVonData::Str(VonStr { value: value.to_string() }),
                        },
                        VonMember {
                            field_name: "bits".to_string(),
                            value: IVonData::Int(VonInt { value: bits as i64 }),
                        },
                    ],
                })
            }
            ExpressionH::ConstantStrH(c) => {
                let ConstantStrH { value} = *c;
                IVonData::Object(VonObject {
                    tyype: "ConstantStr".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "value".to_string(),
                            value: IVonData::Str(VonStr { value: value.to_string() }),
                        },
                    ],
                })
            }
            ExpressionH::ConstantF64H(c) => {
                let ConstantF64H { value } = *c;
                IVonData::Object(VonObject {
                    tyype: "ConstantF64".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "value".to_string(),
                            value: IVonData::Float(VonFloat { value }),
                        },
                    ],
                })
            }
            ExpressionH::ArgumentH(a) => {
                let ArgumentH { result_type, argument_index } = *a;
                IVonData::Object(VonObject {
                    tyype: "Argument".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "resultType".to_string(),
                            value: self.vonify_coord(result_type),
                        },
                        VonMember {
                            field_name: "argumentIndex".to_string(),
                            value: IVonData::Int(VonInt { value: argument_index as i64 }),
                        },
                    ],
                })
            }
            ExpressionH::StackifyH(s) => {
                let StackifyH { source_expr, local, name } = *s;
                IVonData::Object(VonObject {
                    tyype: "Stackify".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "sourceExpr".to_string(),
                            value: self.vonify_expression(source_expr),
                        },
                        VonMember {
                            field_name: "local".to_string(),
                            value: self.vonify_local(local),
                        },
                        VonMember {
                            field_name: "knownLive".to_string(),
                            value: IVonData::Bool(VonBool { value: false }),
                        },
                        VonMember {
                            field_name: "optName".to_string(),
                            value: self.vonify_optional(name, |n| self.vonify_name(n)),
                        },
                    ],
                })
            }
            ExpressionH::RestackifyH(s) => {
                let RestackifyH { source_expr, local, name } = *s;
                IVonData::Object(VonObject {
                    tyype: "Restackify".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "sourceExpr".to_string(),
                            value: self.vonify_expression(source_expr),
                        },
                        VonMember {
                            field_name: "local".to_string(),
                            value: self.vonify_local(local),
                        },
                        VonMember {
                            field_name: "knownLive".to_string(),
                            value: IVonData::Bool(VonBool { value: false }),
                        },
                        VonMember {
                            field_name: "optName".to_string(),
                            value: self.vonify_optional(name, |n| self.vonify_name(n)),
                        },
                    ],
                })
            }
            ExpressionH::UnstackifyH(u) => {
                let UnstackifyH { local } = *u;
                IVonData::Object(VonObject {
                    tyype: "Unstackify".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "local".to_string(),
                            value: self.vonify_local(local),
                        },
                    ],
                })
            }
            ExpressionH::DestroyH(d) => {
                let DestroyH { struct_expression: struct_expr, local_types, local_indices: locals } = *d;
                IVonData::Object(VonObject {
                    tyype: "Destroy".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "structExpr".to_string(), value: self.vonify_expression(struct_expr) },
                        VonMember { field_name: "structType".to_string(), value: self.vonify_coord(struct_expr.result_type()) },
                        VonMember { field_name: "localTypes".to_string(), value: IVonData::array(local_types.iter().map(|lt| self.vonify_coord(*lt)).collect()) },
                        VonMember { field_name: "localIndices".to_string(), value: IVonData::array(locals.iter().map(|l| self.vonify_local(*l)).collect()) },
                        VonMember { field_name: "localsKnownLives".to_string(), value: IVonData::array(locals.iter().map(|_| IVonData::Bool(VonBool { value: false })).collect()) },
                    ],
                })
            }
            ExpressionH::DestroyStaticSizedArrayIntoLocalsH(d) => {
                let DestroyStaticSizedArrayIntoLocalsH { struct_expression: struct_expr, local_types, local_indices } = *d;
                IVonData::Object(VonObject {
                    tyype: "DestroyStaticSizedArrayIntoLocals".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(struct_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(struct_expr.result_type()) },
                        VonMember { field_name: "localTypes".to_string(), value: IVonData::Array(VonArray { id: None, members: local_types.iter().map(|local_type| self.vonify_coord(*local_type)).collect() }) },
                        VonMember { field_name: "localIndices".to_string(), value: IVonData::Array(VonArray { id: None, members: local_indices.iter().map(|local| self.vonify_local(*local)).collect() }) },
                    ],
                })
            }
            ExpressionH::StructToInterfaceUpcastH(si) => {
                let StructToInterfaceUpcastH { source_expression: source_expr, target_interface: target_interface_ref } = *si;
                let source_struct_kind = match source_expr.result_type().kind {
                    KindHT::StructHT(sr) => self.vonify_struct_h(sr),
                    _ => panic!("vonify_expression: StructToInterfaceUpcastH source.result_type.kind not StructHT"),
                };
                IVonData::Object(VonObject {
                    tyype: "StructToInterfaceUpcast".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceStructType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceStructKind".to_string(), value: source_struct_kind },
                        VonMember { field_name: "targetInterfaceType".to_string(), value: self.vonify_coord(ExpressionH::StructToInterfaceUpcastH(si).result_type()) },
                        VonMember { field_name: "targetInterfaceKind".to_string(), value: self.vonify_interface(target_interface_ref) },
                    ],
                })
            }
            ExpressionH::InterfaceToInterfaceUpcastH(_) => panic!("vonify_expression: InterfaceToInterfaceUpcastH"),
            ExpressionH::LocalStoreH(s) => {
                let LocalStoreH { local, source_expression: source_expr, local_name } = *s;
                IVonData::Object(VonObject {
                    tyype: "LocalStore".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "local".to_string(), value: self.vonify_local(local) },
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "localName".to_string(), value: self.vonify_name(local_name) },
                        VonMember { field_name: "knownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                    ],
                })
            }
            ExpressionH::LocalLoadH(l) => {
                let LocalLoadH { local, target_ownership, local_name } = *l;
                IVonData::Object(VonObject {
                    tyype: "LocalLoad".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "local".to_string(), value: self.vonify_local(local) },
                        VonMember { field_name: "targetOwnership".to_string(), value: self.vonify_ownership(target_ownership) },
                        VonMember { field_name: "localName".to_string(), value: self.vonify_name(local_name) },
                    ],
                })
            }
            ExpressionH::MemberStoreH(ms) => {
                let MemberStoreH { result_type, struct_expression: struct_expr, member_index, source_expression: source_expr, member_name } = *ms;
                IVonData::Object(VonObject {
                    tyype: "MemberStore".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "structExpr".to_string(), value: self.vonify_expression(struct_expr) },
                        VonMember { field_name: "structType".to_string(), value: self.vonify_coord(struct_expr.result_type()) },
                        VonMember { field_name: "structKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "memberIndex".to_string(), value: IVonData::Int(VonInt { value: member_index as i64 }) },
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "memberName".to_string(), value: self.vonify_name(member_name) },
                    ],
                })
            }
            ExpressionH::MemberLoadH(ml) => {
                let MemberLoadH { struct_expression: struct_expr, member_index, expected_member_type, result_type, member_name } = *ml;
                IVonData::Object(VonObject {
                    tyype: "MemberLoad".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "structExpr".to_string(), value: self.vonify_expression(struct_expr) },
                        VonMember { field_name: "structId".to_string(), value: self.vonify_struct_h(struct_expr.result_type().kind.expect_struct_h()) },
                        VonMember { field_name: "structType".to_string(), value: self.vonify_coord(struct_expr.result_type()) },
                        VonMember { field_name: "structKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "memberIndex".to_string(), value: IVonData::Int(VonInt { value: member_index as i64 }) },
                        VonMember { field_name: "targetOwnership".to_string(), value: self.vonify_ownership(result_type.ownership) },
                        VonMember { field_name: "expectedMemberType".to_string(), value: self.vonify_coord(expected_member_type) },
                        VonMember { field_name: "expectedResultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "memberName".to_string(), value: self.vonify_name(member_name) },
                    ],
                })
            }
            ExpressionH::NewArrayFromValuesH(n) => {
                let NewArrayFromValuesH { result_type, source_expressions: source_exprs } = *n;
                IVonData::Object(VonObject {
                    tyype: "NewArrayFromValues".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExprs".to_string(), value: IVonData::Array(VonArray { id: None, members: source_exprs.iter().map(|e| self.vonify_expression(*e)).collect() }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "resultKind".to_string(), value: self.vonify_kind(result_type.kind) },
                    ],
                })
            }
            ExpressionH::StaticSizedArrayStoreH(_) => panic!("vonify_expression: StaticSizedArrayStoreH"),
            ExpressionH::RuntimeSizedArrayStoreH(rsas) => {
                let RuntimeSizedArrayStoreH { array_expression: array_expr, index_expression: index_expr, source_expression: source_expr, result_type } = *rsas;
                IVonData::Object(VonObject {
                    tyype: "RuntimeSizedArrayStore".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "arrayKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "indexExpr".to_string(), value: self.vonify_expression(index_expr) },
                        VonMember { field_name: "indexType".to_string(), value: self.vonify_coord(index_expr.result_type()) },
                        VonMember { field_name: "indexKind".to_string(), value: self.vonify_kind(index_expr.result_type().kind) },
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKind".to_string(), value: self.vonify_kind(source_expr.result_type().kind) },
                        VonMember { field_name: "sourceKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::RuntimeSizedArrayLoadH(rsal) => {
                let RuntimeSizedArrayLoadH { array_expression: array_expr, index_expression: index_expr, target_ownership, expected_element_type, result_type } = *rsal;
                IVonData::Object(VonObject {
                    tyype: "RuntimeSizedArrayLoad".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "arrayKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "indexExpr".to_string(), value: self.vonify_expression(index_expr) },
                        VonMember { field_name: "indexType".to_string(), value: self.vonify_coord(index_expr.result_type()) },
                        VonMember { field_name: "indexKind".to_string(), value: self.vonify_kind(index_expr.result_type().kind) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(rsal.result_type) },
                        VonMember { field_name: "targetOwnership".to_string(), value: self.vonify_ownership(target_ownership) },
                        VonMember { field_name: "expectedElementType".to_string(), value: self.vonify_coord(expected_element_type) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::StaticSizedArrayLoadH(ssal) => {
                let StaticSizedArrayLoadH { array_expression: array_expr, index_expression: index_expr, target_ownership, expected_element_type, array_size, result_type } = *ssal;
                IVonData::Object(VonObject {
                    tyype: "StaticSizedArrayLoad".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "arrayKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "indexExpr".to_string(), value: self.vonify_expression(index_expr) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(ssal.result_type) },
                        VonMember { field_name: "targetOwnership".to_string(), value: self.vonify_ownership(target_ownership) },
                        VonMember { field_name: "expectedElementType".to_string(), value: self.vonify_coord(expected_element_type) },
                        VonMember { field_name: "arraySize".to_string(), value: IVonData::Int(VonInt { value: array_size }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::CallH(c) => {
                let CallH { function: function_expr, args_expressions: args_exprs } = *c;
                IVonData::Object(VonObject {
                    tyype: "Call".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "function".to_string(),
                            value: self.vonify_prototype(function_expr),
                        },
                        VonMember {
                            field_name: "argExprs".to_string(),
                            value: IVonData::Array(VonArray { id: None, members: args_exprs.iter().map(|e| self.vonify_expression(*e)).collect() }),
                        },
                    ],
                })
            }
            ExpressionH::ExternCallH(e) => {
                let ExternCallH { function: function_expr, args_expressions: args_exprs } = *e;
                IVonData::Object(VonObject {
                    tyype: "ExternCall".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "function".to_string(), value: self.vonify_prototype(function_expr) },
                        VonMember { field_name: "argExprs".to_string(), value: IVonData::array(args_exprs.iter().map(|a| self.vonify_expression(*a)).collect()) },
                        VonMember { field_name: "argTypes".to_string(), value: IVonData::array(args_exprs.iter().map(|a| self.vonify_coord(a.result_type())).collect()) },
                    ],
                })
            }
            ExpressionH::InterfaceCallH(c) => {
                let InterfaceCallH { args_expressions: args_exprs, virtual_param_index, interface_h: interface_ref_h, index_in_edge, function_type } = *c;
                IVonData::Object(VonObject {
                    tyype: "InterfaceCall".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "argExprs".to_string(), value: IVonData::Array(VonArray { id: None, members: args_exprs.iter().map(|e| self.vonify_expression(*e)).collect() }) },
                        VonMember { field_name: "virtualParamIndex".to_string(), value: IVonData::Int(VonInt { value: virtual_param_index as i64 }) },
                        VonMember { field_name: "interfaceRef".to_string(), value: self.vonify_interface(interface_ref_h) },
                        VonMember { field_name: "indexInEdge".to_string(), value: IVonData::Int(VonInt { value: index_in_edge as i64 }) },
                        VonMember { field_name: "functionType".to_string(), value: self.vonify_prototype(function_type) },
                    ],
                })
            }
            ExpressionH::IfH(i) => {
                let IfH { condition_block, then_block, else_block, common_supertype } = *i;
                IVonData::Object(VonObject {
                    tyype: "If".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "conditionBlock".to_string(), value: self.vonify_expression(condition_block) },
                        VonMember { field_name: "thenBlock".to_string(), value: self.vonify_expression(then_block) },
                        VonMember { field_name: "thenResultType".to_string(), value: self.vonify_coord(then_block.result_type()) },
                        VonMember { field_name: "elseBlock".to_string(), value: self.vonify_expression(else_block) },
                        VonMember { field_name: "elseResultType".to_string(), value: self.vonify_coord(else_block.result_type()) },
                        VonMember { field_name: "commonSupertype".to_string(), value: self.vonify_coord(common_supertype) },
                    ],
                })
            }
            ExpressionH::WhileH(w) => {
                let WhileH { body_block } = *w;
                IVonData::Object(VonObject {
                    tyype: "While".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "bodyBlock".to_string(), value: self.vonify_expression(body_block) },
                    ],
                })
            }
            ExpressionH::ConsecutorH(c) => {
                let ConsecutorH { exprs: nodes } = *c;
                IVonData::Object(VonObject {
                    tyype: "Consecutor".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "exprs".to_string(),
                            value: IVonData::Array(VonArray {
                                id: None,
                                members: nodes.iter().map(|node| self.vonify_expression(*node)).collect(),
                            }),
                        },
                    ],
                })
            }
            ExpressionH::BlockH(b) => {
                let BlockH { inner } = *b;
                IVonData::Object(VonObject {
                    tyype: "Block".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "innerExpr".to_string(),
                            value: self.vonify_expression(inner),
                        },
                        VonMember {
                            field_name: "innerType".to_string(),
                            value: self.vonify_coord(inner.result_type()),
                        },
                    ],
                })
            }
            ExpressionH::MutabilifyH(_) => panic!("vonify_expression: MutabilifyH"),
            ExpressionH::ImmutabilifyH(_) => panic!("vonify_expression: ImmutabilifyH"),
            ExpressionH::ReturnH(r) => {
                let ReturnH { source_expression } = *r;
                IVonData::Object(VonObject {
                    tyype: "Return".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "sourceExpr".to_string(),
                            value: self.vonify_expression(source_expression),
                        },
                        VonMember {
                            field_name: "sourceType".to_string(),
                            value: self.vonify_coord(source_expression.result_type()),
                        },
                    ],
                })
            }
            ExpressionH::NewImmRuntimeSizedArrayH(n) => {
                let NewImmRuntimeSizedArrayH { size_expression: size_expr, generator_expression: generator_expr, generator_method, element_type: _, result_type } = *n;
                IVonData::Object(VonObject {
                    tyype: "NewImmRuntimeSizedArray".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sizeExpr".to_string(), value: self.vonify_expression(size_expr) },
                        VonMember { field_name: "sizeType".to_string(), value: self.vonify_coord(size_expr.result_type()) },
                        VonMember { field_name: "sizeKind".to_string(), value: self.vonify_kind(size_expr.result_type().kind) },
                        VonMember { field_name: "generatorExpr".to_string(), value: self.vonify_expression(generator_expr) },
                        VonMember { field_name: "generatorType".to_string(), value: self.vonify_coord(generator_expr.result_type()) },
                        VonMember { field_name: "generatorKind".to_string(), value: self.vonify_kind(generator_expr.result_type().kind) },
                        VonMember { field_name: "generatorMethod".to_string(), value: self.vonify_prototype(generator_method) },
                        VonMember { field_name: "generatorKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "elementType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::NewMutRuntimeSizedArrayH(n) => {
                let NewMutRuntimeSizedArrayH { capacity_expression: capacity_expr, element_type, result_type } = *n;
                IVonData::Object(VonObject {
                    tyype: "NewMutRuntimeSizedArray".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "capacityExpr".to_string(), value: self.vonify_expression(capacity_expr) },
                        VonMember { field_name: "capacityType".to_string(), value: self.vonify_coord(capacity_expr.result_type()) },
                        VonMember { field_name: "capacityKind".to_string(), value: self.vonify_kind(capacity_expr.result_type().kind) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "elementType".to_string(), value: self.vonify_coord(element_type) },
                    ],
                })
            }
            ExpressionH::PushRuntimeSizedArrayH(p) => {
                let PushRuntimeSizedArrayH { array_expression: array_expr, newcomer_expression: newcomer_expr } = *p;
                IVonData::Object(VonObject {
                    tyype: "PushRuntimeSizedArray".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "newcomerExpr".to_string(), value: self.vonify_expression(newcomer_expr) },
                        VonMember { field_name: "newcomerType".to_string(), value: self.vonify_coord(newcomer_expr.result_type()) },
                        VonMember { field_name: "newcomerKind".to_string(), value: self.vonify_kind(newcomer_expr.result_type().kind) },
                        VonMember { field_name: "consumerKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                    ],
                })
            }
            ExpressionH::PopRuntimeSizedArrayH(p) => {
                let PopRuntimeSizedArrayH { array_expression: array_expr, element_type: array_element_type } = *p;
                IVonData::Object(VonObject {
                    tyype: "PopRuntimeSizedArray".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "arrayElementType".to_string(), value: self.vonify_coord(array_element_type) },
                        VonMember { field_name: "consumerKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                    ],
                })
            }
            ExpressionH::StaticArrayFromCallableH(s) => {
                let StaticArrayFromCallableH { generator_expression: generator_expr, generator_method, element_type: _, result_type } = *s;
                IVonData::Object(VonObject {
                    tyype: "StaticArrayFromCallable".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "generatorExpr".to_string(), value: self.vonify_expression(generator_expr) },
                        VonMember { field_name: "generatorType".to_string(), value: self.vonify_coord(generator_expr.result_type()) },
                        VonMember { field_name: "generatorKind".to_string(), value: self.vonify_kind(generator_expr.result_type().kind) },
                        VonMember { field_name: "generatorMethod".to_string(), value: self.vonify_prototype(generator_method) },
                        VonMember { field_name: "generatorKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                        VonMember { field_name: "elementType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::DestroyStaticSizedArrayIntoFunctionH(d) => {
                let DestroyStaticSizedArrayIntoFunctionH { array_expression: array_expr, consumer_expression: consumer_expr, consumer_method, array_element_type, array_size } = *d;
                IVonData::Object(VonObject {
                    tyype: "DestroyStaticSizedArrayIntoFunction".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                        VonMember { field_name: "consumerExpr".to_string(), value: self.vonify_expression(consumer_expr) },
                        VonMember { field_name: "consumerType".to_string(), value: self.vonify_coord(consumer_expr.result_type()) },
                        VonMember { field_name: "consumerMethod".to_string(), value: self.vonify_prototype(consumer_method) },
                        VonMember { field_name: "consumerKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "arrayElementType".to_string(), value: self.vonify_coord(array_element_type) },
                        VonMember { field_name: "arraySize".to_string(), value: IVonData::Int(VonInt { value: array_size }) },
                    ],
                })
            }
            ExpressionH::DestroyImmRuntimeSizedArrayH(_) => panic!("vonify_expression: DestroyImmRuntimeSizedArrayH"),
            ExpressionH::DestroyMutRuntimeSizedArrayH(d) => {
                let DestroyMutRuntimeSizedArrayH { array_expression: array_expr } = *d;
                IVonData::Object(VonObject {
                    tyype: "DestroyMutRuntimeSizedArray".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "arrayExpr".to_string(), value: self.vonify_expression(array_expr) },
                        VonMember { field_name: "arrayType".to_string(), value: self.vonify_coord(array_expr.result_type()) },
                        VonMember { field_name: "arrayKind".to_string(), value: self.vonify_kind(array_expr.result_type().kind) },
                    ],
                })
            }
            ExpressionH::BreakH(_) => IVonData::Object(VonObject {
                tyype: "Break".to_string(),
                id: None,
                members: vec![],
            }),
            ExpressionH::NewStructH(n) => {
                let NewStructH { source_expressions, target_member_names, result_type } = *n;
                IVonData::Object(VonObject {
                    tyype: "NewStruct".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExprs".to_string(), value: IVonData::Array(VonArray { id: None, members: source_expressions.iter().map(|e| self.vonify_expression(*e)).collect() }) },
                        VonMember { field_name: "memberNames".to_string(), value: IVonData::Array(VonArray { id: None, members: target_member_names.iter().map(|n| self.vonify_name(n)).collect() }) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(result_type) },
                    ],
                })
            }
            ExpressionH::ArrayLengthH(a) => {
                let ArrayLengthH { source_expression: source_expr } = *a;
                IVonData::Object(VonObject {
                    tyype: "ArrayLength".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                    ],
                })
            }
            ExpressionH::ArrayCapacityH(a) => {
                let ArrayCapacityH { source_expression: source_expr } = *a;
                IVonData::Object(VonObject {
                    tyype: "ArrayCapacity".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                    ],
                })
            }
            ExpressionH::BorrowToWeakH(wa) => {
                let BorrowToWeakH { ref_expression: source_expr } = *wa;
                let wa_result_type = ExpressionH::BorrowToWeakH(wa).result_type();
                IVonData::Object(VonObject {
                    tyype: "BorrowToWeak".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKind".to_string(), value: self.vonify_kind(source_expr.result_type().kind) },
                        VonMember { field_name: "resultType".to_string(), value: self.vonify_coord(wa_result_type) },
                        VonMember { field_name: "resultKind".to_string(), value: self.vonify_kind(wa_result_type.kind) },
                    ],
                })
            }
            ExpressionH::IsSameInstanceH(isi) => {
                let IsSameInstanceH { left_expression: left, right_expression: right } = *isi;
                IVonData::Object(VonObject {
                    tyype: "Is".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "leftExpr".to_string(), value: self.vonify_expression(left) },
                        VonMember { field_name: "leftExprType".to_string(), value: self.vonify_coord(left.result_type()) },
                        VonMember { field_name: "rightExpr".to_string(), value: self.vonify_expression(right) },
                        VonMember { field_name: "rightExprType".to_string(), value: self.vonify_coord(right.result_type()) },
                    ],
                })
            }
            ExpressionH::AsSubtypeH(a) => {
                let AsSubtypeH { source_expression: source_expr, target_type, result_type: result_result_type, some_constructor: ok_constructor, none_constructor: err_constructor } = *a;
                IVonData::Object(VonObject {
                    tyype: "AsSubtype".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "targetKind".to_string(), value: self.vonify_kind(target_type) },
                        VonMember { field_name: "okConstructor".to_string(), value: self.vonify_prototype(ok_constructor) },
                        VonMember { field_name: "okType".to_string(), value: self.vonify_coord(ok_constructor.return_type) },
                        VonMember { field_name: "okKind".to_string(), value: self.vonify_kind(ok_constructor.return_type.kind) },
                        VonMember { field_name: "errConstructor".to_string(), value: self.vonify_prototype(err_constructor) },
                        VonMember { field_name: "errType".to_string(), value: self.vonify_coord(err_constructor.return_type) },
                        VonMember { field_name: "errKind".to_string(), value: self.vonify_kind(err_constructor.return_type.kind) },
                        VonMember { field_name: "resultResultType".to_string(), value: self.vonify_coord(result_result_type) },
                        VonMember { field_name: "resultResultKind".to_string(), value: self.vonify_kind(result_result_type.kind) },
                    ],
                })
            }
            ExpressionH::LockWeakH(lw) => {
                let LockWeakH { source_expression: source_expr, result_type: result_opt_type, some_constructor, none_constructor } = *lw;
                IVonData::Object(VonObject {
                    tyype: "LockWeak".to_string(),
                    id: None,
                    members: vec![
                        VonMember { field_name: "sourceExpr".to_string(), value: self.vonify_expression(source_expr) },
                        VonMember { field_name: "sourceType".to_string(), value: self.vonify_coord(source_expr.result_type()) },
                        VonMember { field_name: "sourceKnownLive".to_string(), value: IVonData::Bool(VonBool { value: false }) },
                        VonMember { field_name: "someConstructor".to_string(), value: self.vonify_prototype(some_constructor) },
                        VonMember { field_name: "someType".to_string(), value: self.vonify_coord(some_constructor.return_type) },
                        VonMember { field_name: "someKind".to_string(), value: self.vonify_kind(some_constructor.return_type.kind) },
                        VonMember { field_name: "noneConstructor".to_string(), value: self.vonify_prototype(none_constructor) },
                        VonMember { field_name: "noneType".to_string(), value: self.vonify_coord(none_constructor.return_type) },
                        VonMember { field_name: "noneKind".to_string(), value: self.vonify_kind(none_constructor.return_type.kind) },
                        VonMember { field_name: "resultOptType".to_string(), value: self.vonify_coord(result_opt_type) },
                        VonMember { field_name: "resultOptKind".to_string(), value: self.vonify_kind(result_opt_type.kind) },
                    ],
                })
            }
            ExpressionH::DiscardH(d) => {
                let DiscardH { source_expression: source_expr } = *d;
                IVonData::Object(VonObject {
                    tyype: "Discard".to_string(),
                    id: None,
                    members: vec![
                        VonMember {
                            field_name: "sourceExpr".to_string(),
                            value: self.vonify_expression(source_expr),
                        },
                        VonMember {
                            field_name: "sourceResultType".to_string(),
                            value: self.vonify_coord(source_expr.result_type()),
                        },
                    ],
                })
            }
            ExpressionH::PreCheckBorrowH(_) => panic!("vonify_expression: PreCheckBorrowH"),
        }
    }


// mig: fn vonify_function_ref
    pub fn vonify_function_ref(&self, r#ref: &FunctionRefH<'s, 'h>) -> IVonData {
        panic!("Unimplemented: vonify_function_ref");
    }


// mig: fn vonify_local
    pub fn vonify_local(&self, local: Local<'s, 'h>) -> IVonData {
        let Local { id, variability, type_h: tyype } = local;
        IVonData::Object(VonObject {
            tyype: "Local".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "id".to_string(),
                    value: self.vonify_variable_id(id),
                },
                VonMember {
                    field_name: "variability".to_string(),
                    value: self.vonify_variability(variability),
                },
                VonMember {
                    field_name: "type".to_string(),
                    value: self.vonify_coord(tyype),
                },
            ],
        })
    }


// mig: fn vonify_variable_id
    pub fn vonify_variable_id(&self, id: VariableIdH<'s, 'h>) -> IVonData {
        let VariableIdH { number, height: _, name: maybe_name } = id;
        IVonData::Object(VonObject {
            tyype: "VariableId".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "number".to_string(),
                    value: IVonData::Int(VonInt { value: number as i64 }),
                },
                VonMember {
                    field_name: "height".to_string(),
                    value: IVonData::Int(VonInt { value: number as i64 }),
                },
                VonMember {
                    field_name: "optName".to_string(),
                    value: self.vonify_optional(maybe_name, |x| self.vonify_name(x)),
                },
            ],
        })
    }


// mig: fn vonify_optional
    pub fn vonify_optional<I>(&self, opt: Option<I>, func: impl Fn(I) -> IVonData) -> IVonData {
        match opt {
            None => IVonData::Object(VonObject { tyype: "None".to_string(), id: None, members: vec![] }),
            Some(value) => IVonData::Object(VonObject {
                tyype: "Some".to_string(),
                id: None,
                members: vec![
                    VonMember {
                        field_name: "value".to_string(),
                        value: func(value),
                    },
                ],
            }),
        }
    }


// mig: fn vonify_code_location
    pub fn vonify_code_location(&self, code_location: CodeLocation) -> IVonData {
        panic!("Unimplemented: vonify_code_location");
    }


// mig: fn vonify_code_location2
    pub fn vonify_code_location2(&self, code_location: &CodeLocationS<'s>) -> IVonData {
        panic!("Unimplemented: vonify_code_location2");
    }


// mig: fn vonify_ranges
    pub fn vonify_ranges(&self, ranges: &[RangeS<'s>]) -> IVonData {
        panic!("Unimplemented: vonify_ranges");
    }


// mig: fn vonify_range
    pub fn vonify_range(&self, range: RangeS<'s>) -> IVonData {
        panic!("Unimplemented: vonify_range");
    }


// mig: fn vonify_name
    pub fn vonify_name(&self, h: &IdH<'s>) -> IVonData {
        let IdH { local_name, package_coordinate, shortened_name, fully_qualified_name: _, _must_intern: _} = h;
        IVonData::Object(VonObject {
            tyype: "Name".to_string(),
            id: None,
            members: vec![
                VonMember {
                    field_name: "localName".to_string(),
                    value: IVonData::Str(VonStr { value: local_name.0.to_string() }),
                },
                VonMember {
                    field_name: "packageCoordinate".to_string(),
                    value: IVonData::Object(translate_package_coordinate(package_coordinate)),
                },
                VonMember {
                    field_name: "shortenedName".to_string(),
                    value: IVonData::Str(VonStr { value: shortened_name.0.to_string() }),
                },
            ],
        })
    }
}

