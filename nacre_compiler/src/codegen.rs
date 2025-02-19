use crate::{Ir, IrInstr, IrType};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, StructValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::cmp::max;

fn emit_tag_type(variant_count: usize, context: &Context) -> BasicTypeEnum {
    let tag_bits = (2 * variant_count - 1).ilog2();
    if tag_bits > 0 && tag_bits <= 128 {
        context.custom_width_int_type(tag_bits).into()
    } else {
        todo!();
    }
}

fn emit_type<'a>(
    value_type: usize,
    types: &[Option<IrType>],
    context: &'a Context,
) -> BasicTypeEnum<'a> {
    match types[value_type].as_ref().unwrap() {
        IrType::Enum(variants) => {
            let variant_count = variants.len();
            let tag_type = emit_tag_type(variant_count, context);
            let (tag_size, _) = compute_type_size(tag_type);
            // TODO: put the tag inside the union, for better alignment and compatibility with Rust
            let nontag_size = variants
                .iter()
                .map(|vt| match vt {
                    Some(vts) => {
                        let (variant_size, variant_alignment) =
                            compute_type_size(emit_type(*vts, types, context));
                        let field_alignment =
                            variant_alignment * tag_size.div_ceil(variant_alignment);
                        let middle_padding = field_alignment - tag_size;
                        variant_size + middle_padding
                    }
                    None => 0,
                })
                .max()
                .unwrap();
            if nontag_size > 0 {
                /*if tag_size + nontag_size < 8 {
                    context.custom_width_int_type((8 * (tag_size + nontag_size)).try_into().unwrap()).into()
                } else {*/
                let struct_members: Vec<_> = [tag_type]
                    .into_iter()
                    .chain((0..nontag_size).map(|_| context.i8_type().into()))
                    .collect();
                context.struct_type(&struct_members, true).into()
                //}
            } else {
                //context.struct_type(&[tag_type], false).into()
                tag_type.into()
            }
        }
        IrType::Closure(_, _) | IrType::Any => {
            let ptr_type = context.ptr_type(AddressSpace::from(0));
            ptr_type.into()
        }
        _ => todo!(),
    }
}

fn compute_type_size(t: BasicTypeEnum) -> (usize, usize) {
    match t {
        BasicTypeEnum::IntType(tt) => {
            let bit_width = tt.get_bit_width();
            let log_bit_width = (2 * bit_width - 1).ilog2();
            match log_bit_width {
                0..=3 => (1, 1),
                4 => (2, 2),
                5 => (4, 4),
                6 => (8, 8),
                7 => (16, 16),
                _ => todo!(),
            }
        }
        BasicTypeEnum::StructType(tt) => {
            let sizes: Vec<_> = tt.get_field_types_iter().map(compute_type_size).collect();
            let mut s = 0;
            let mut a = 1;
            for (size, alignment) in sizes {
                s += (alignment - (s % alignment)) % alignment;
                s += size;
                a = max(a, alignment);
            }
            (s, a)
        }
        BasicTypeEnum::ArrayType(tt) => {
            let (size, alignment) = compute_type_size(tt.get_element_type());
            (tt.len() as usize * size, alignment)
        }
        BasicTypeEnum::PointerType(_) => (8, 8),
        _ => todo!(),
    }
}

fn emit_instr_param<'a>(
    p: &usize,
    vt: BasicTypeEnum<'a>,
    function: &FunctionValue<'a>,
    closure: bool,
    context: &'a Context,
    builder: &'a Builder,
) -> BasicValueEnum<'a> {
    let raw_param = function
        .get_nth_param(*p as u32 + if closure { 1 } else { 0 })
        .unwrap();
    let param_ptr = builder
        .build_alloca(raw_param.get_type(), "param_ptr")
        .unwrap();
    builder.build_store(param_ptr, raw_param);
    builder.build_load(vt, param_ptr, "param").unwrap()
}

fn emit_instr_capture<'a>(
    c: &usize,
    t: Option<usize>,
    types: &[Option<IrType>],
    function: &FunctionValue<'a>,
    context: &'a Context,
    builder: &'a Builder,
) -> BasicValueEnum<'a> {
    let self_param = function.get_nth_param(0).unwrap();
    let closure = self_param.into_pointer_value();
    let i32_type = context.i32_type();
    let closure_type = context.ptr_type(AddressSpace::from(0));
    let capture_type = emit_type(t.unwrap(), types, context);
    let capture_address = unsafe {
        builder.build_gep(
            closure_type,
            closure,
            &[i32_type.const_int(*c as u64 + 1, false)],
            "capture_addr",
        )
    }
    .unwrap();
    builder
        .build_load(capture_type, capture_address, "capture")
        .unwrap()
    // TODO: emit correct capture type
}

fn emit_instr_apply<'a>(
    closure: BasicValueEnum<'a>,
    params: Vec<BasicValueEnum<'a>>,
    ir_type: Option<usize>,
    param_ir_types: Vec<Option<usize>>,
    types: &[Option<IrType>],
    function: &FunctionValue<'a>,
    context: &'a Context,
    builder: &'a Builder,
) -> BasicValueEnum<'a> {
    match types[ir_type.unwrap()].as_ref().unwrap() {
        IrType::Closure(p, r) => {
            let closure_type = context.ptr_type(AddressSpace::from(0));
            let params: Vec<_> = params
                .into_iter()
                .map(|param| {
                    let param_type = param.get_type();
                    let param_size = compute_type_size(param_type).0;
                    let cast_param_type =
                        context.custom_width_int_type((8 * param_size).try_into().unwrap());
                    let apply_param_ptr =
                        builder.build_alloca(param_type, "apply_param_ptr").unwrap();
                    builder.build_store(apply_param_ptr, param);
                    builder
                        .build_load(cast_param_type, apply_param_ptr, "apply_param")
                        .unwrap()
                })
                .collect();
            let param_types: Vec<BasicMetadataTypeEnum> = [closure_type.into()]
                .into_iter()
                .chain(p.iter().map(|t| {
                    context
                        .custom_width_int_type(
                            (8 * compute_type_size(emit_type(t.unwrap(), types, context).into()).0)
                                .try_into()
                                .unwrap(),
                        )
                        .into()
                }))
                .collect::<Vec<_>>();
            let fn_type = match r {
                Some(rs) => {
                    let return_type: BasicTypeEnum = context
                        .custom_width_int_type(
                            (8 * compute_type_size(emit_type(*rs, types, context)).0)
                                .try_into()
                                .unwrap(),
                        )
                        .into();
                    return_type.fn_type(&param_types, false)
                }
                None => context.void_type().fn_type(&param_types, false),
            };
            let rv = {
                let closure = closure.into_pointer_value();
                let func = builder
                    .build_load(context.ptr_type(AddressSpace::from(0)), closure, "func")
                    .unwrap()
                    .into_pointer_value();
                builder
                    .build_indirect_call(
                        fn_type,
                        func,
                        &[closure.into()]
                            .into_iter()
                            .chain(params.into_iter().map(|x| x.into()))
                            .collect::<Vec<_>>(),
                        "apply",
                    )
                    .unwrap()
            };
            let cast_return_value = rv.try_as_basic_value().left().unwrap();
            let cast_return_type = cast_return_value.get_type();
            let return_type = emit_type(r.unwrap(), types, context);
            let return_ptr = builder
                .build_alloca(return_type, "apply_return_ptr")
                .unwrap();
            builder.build_store(return_ptr, cast_return_value);
            let return_value = builder
                .build_load(return_type, return_ptr, "apply_return_value")
                .unwrap();
            return_value.into()
        }
        IrType::Function(p, r) => {
            let params: Vec<_> = params
                .into_iter()
                .map(|param| {
                    let param_type = param.get_type();
                    let param_size = compute_type_size(param_type).0;
                    let cast_param_type =
                        context.custom_width_int_type((8 * param_size).try_into().unwrap());
                    let apply_param_ptr =
                        builder.build_alloca(param_type, "apply_param_ptr").unwrap();
                    builder.build_store(apply_param_ptr, param);
                    builder
                        .build_load(cast_param_type, apply_param_ptr, "apply_param")
                        .unwrap()
                })
                .collect();
            let param_types = p
                .iter()
                .map(|t| {
                    context
                        .custom_width_int_type(
                            (8 * compute_type_size(emit_type(t.unwrap(), types, context).into()).0)
                                .try_into()
                                .unwrap(),
                        )
                        .into()
                })
                .collect::<Vec<_>>();
            let fn_type = match r {
                Some(rs) => {
                    let return_type: BasicTypeEnum = context
                        .custom_width_int_type(
                            (8 * compute_type_size(emit_type(*rs, types, context)).0)
                                .try_into()
                                .unwrap(),
                        )
                        .into();
                    return_type.fn_type(&param_types, false)
                }
                None => context.void_type().fn_type(&param_types, false),
            };
            let rv = {
                let func = closure.into_pointer_value();
                builder
                    .build_indirect_call(
                        fn_type,
                        func,
                        &params.into_iter().map(|x| x.into()).collect::<Vec<_>>(),
                        "apply",
                    )
                    .unwrap()
            };
            let cast_return_value = rv.try_as_basic_value().left().unwrap();
            let cast_return_type = cast_return_value.get_type();
            let return_type = emit_type(r.unwrap(), types, context);
            let return_ptr = builder
                .build_alloca(return_type, "apply_return_ptr")
                .unwrap();
            builder.build_store(return_ptr, cast_return_value);
            let return_value = builder
                .build_load(return_type, return_ptr, "apply_return_value")
                .unwrap();
            return_value.into()
        }
        IrType::Enum(variants) => {
            // get enum tag
            let (switch_value, tag_type) =
                if let BasicValueEnum::StructValue(struct_value) = closure {
                    let switch_value: IntValue = builder
                        .build_extract_value(struct_value, 0, "tag")
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let tag_type: IntType = struct_value
                        .get_type()
                        .get_field_type_at_index(0)
                        .unwrap()
                        .try_into()
                        .unwrap();
                    (switch_value, tag_type)
                } else {
                    (
                        closure.try_into().unwrap(),
                        closure.get_type().try_into().unwrap(),
                    )
                };
            let start_block = builder.get_insert_block().unwrap();
            let landing_block = context.append_basic_block(*function, "match_landing");
            // get branches
            let mut cases = vec![];
            let mut values = vec![];
            for (n, variant) in variants.iter().enumerate() {
                let enum_value = tag_type.const_int(n.try_into().unwrap(), false);
                let variant_block = context.append_basic_block(*function, "match_variant");
                builder.position_at_end(variant_block);
                match variant {
                    None => values.push(params[n]),
                    Some(field_type) => {
                        // TODO: special provision for IrType::Struct field (multiple fields)
                        // TODO: handle recursive enum fields
                        let struct_ptr = builder
                            .build_alloca(closure.get_type(), "apply_struct_ptr")
                            .unwrap();
                        builder.build_store(struct_ptr, closure).unwrap();
                        let inner_type = emit_type(*field_type, types, context);
                        let value_type = context.struct_type(&[tag_type.into(), inner_type], false);
                        let cast_struct_value: StructValue = builder
                            .build_load(value_type, struct_ptr, "apply_struct")
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let field_value = builder
                            .build_extract_value(cast_struct_value, 1, "apply_field")
                            .unwrap();
                        let closure_type = param_ir_types[n];
                        let value = emit_instr_apply(
                            params[n],
                            vec![field_value],
                            param_ir_types[n],
                            vec![Some(*field_type)],
                            types,
                            function,
                            context,
                            builder,
                        );
                        values.push(value);
                    }
                }
                builder.build_unconditional_branch(landing_block).unwrap();
                cases.push((enum_value, variant_block));
            }
            let error_block = context.append_basic_block(*function, "match_error");
            builder.position_at_end(error_block);
            builder.build_unreachable().unwrap();
            builder.position_at_end(start_block);
            builder
                .build_switch(switch_value, error_block, &cases)
                .unwrap();
            builder.position_at_end(landing_block);
            let output = builder.build_phi(params[0].get_type(), "match").unwrap();
            for (n, v) in values.into_iter().enumerate() {
                output.add_incoming(&[(&v as &dyn BasicValue, cases[n].1)]);
            }
            output.as_basic_value()
        }
        _ => todo!(),
    }
}

fn emit_instr_closure<'a>(
    func: FunctionValue,
    captures: Vec<BasicValueEnum>,
    context: &'a Context,
    builder: &'a Builder,
) -> Result<BasicValueEnum<'a>, ()> {
    let len = captures.len() as u64 + 1;
    let func = func.as_global_value().as_pointer_value();
    let i32_type = context.i32_type();
    let closure_type = context.ptr_type(AddressSpace::from(0));
    let closure = builder
        .build_array_malloc(closure_type, i32_type.const_int(len, false), "closure")
        .unwrap();
    builder.build_store(closure, func).or(Err(()))?;
    for (capn, capture_value) in captures.iter().enumerate() {
        let capture_address = unsafe {
            builder.build_gep(
                closure_type,
                closure,
                &[i32_type.const_int(capn as u64 + 1, false)],
                "capture_addr",
            )
        }
        .unwrap();
        builder
            .build_store(capture_address, *capture_value)
            .or(Err(()))?;
    }
    Ok(closure.into())
}

fn emit_instr_enum<'a>(
    variant: usize,
    contained: Option<BasicValueEnum<'a>>,
    vt: Option<usize>,
    types: &[Option<IrType>],
    context: &'a Context,
    builder: &'a Builder,
) -> Result<BasicValueEnum<'a>, ()> {
    let value_type = types[vt.unwrap()].as_ref().unwrap();
    let variants = if let IrType::Enum(variants) = value_type {
        variants
    } else {
        panic!()
    };
    let tag_type: IntType = emit_tag_type(variants.len(), context).try_into().unwrap();
    let tag_value = tag_type.const_int(variant.try_into().unwrap(), false);
    let struct_value: BasicValueEnum = match contained {
        Some(c) => {
            let sv = context.const_struct(&[tag_value.into(), c.get_type().const_zero()], false);
            let sv: StructValue = builder
                .build_insert_value(sv, c, 1, "enum_inserted")
                .unwrap()
                .try_into()
                .unwrap();
            sv.into()
        }
        None => tag_value.into(),
    };
    let struct_ptr = builder
        .build_alloca(struct_value.get_type(), "enum_struct_ptr")
        .unwrap();
    builder.build_store(struct_ptr, struct_value).unwrap();
    let value_type = emit_type(vt.unwrap(), types, context);
    Ok(builder.build_load(value_type, struct_ptr, "enum").unwrap())
}

pub(crate) fn emit_code(ir: &Ir) -> Result<(), ()> {
    let context = Context::create();
    let module = context.create_module("nacre");
    let builder = context.create_builder();
    let closure_type = context.ptr_type(AddressSpace::from(0));
    // first param is self, second is actual param
    let mut functions = vec![];
    for (n, def) in ir.defs.iter().enumerate() {
        let function = if let Some(d) = def {
            let function_name = if let Some(name) = &d.name {
                name.clone()
            } else {
                format!(".fn{}", n)
            };
            let param_types = if d.captures.is_empty() {
                d.param_types
                    .iter()
                    .map(|t| {
                        context
                            .custom_width_int_type(
                                (8 * compute_type_size(
                                    emit_type(t.unwrap(), &ir.types, &context).into(),
                                )
                                .0)
                                    .try_into()
                                    .unwrap(),
                            )
                            .into()
                    })
                    .collect::<Vec<_>>()
            } else {
                [closure_type.into()]
                    .into_iter()
                    .chain(d.param_types.iter().map(|t| {
                        context
                            .custom_width_int_type(
                                (8 * compute_type_size(
                                    emit_type(t.unwrap(), &ir.types, &context).into(),
                                )
                                .0)
                                    .try_into()
                                    .unwrap(),
                            )
                            .into()
                    }))
                    .collect::<Vec<_>>()
            };
            let r = d.code.last().unwrap().value_type;
            let function_type = match r {
                Some(rs) => {
                    let return_type: BasicTypeEnum = context
                        .custom_width_int_type(
                            (8 * compute_type_size(emit_type(rs, &ir.types, &context)).0)
                                .try_into()
                                .unwrap(),
                        )
                        .into();
                    return_type.fn_type(&param_types, false)
                }
                None => context.void_type().fn_type(&param_types, false),
            };
            let function_linkage = if d.export {
                Some(Linkage::External)
            } else {
                Some(Linkage::Private)
            };
            Some(module.add_function(&function_name, function_type, function_linkage))
        } else {
            None
        };
        functions.push(function);
    }
    for (n, def) in ir.defs.iter().enumerate() {
        if let Some(d) = def {
            let basic_block = context.append_basic_block(functions[n].unwrap(), "");
            builder.position_at_end(basic_block);
            let mut values = vec![];
            for loc in &d.code {
                match &loc.instr {
                    IrInstr::Param(p) => {
                        values.push(emit_instr_param(
                            p,
                            emit_type(loc.value_type.unwrap(), &ir.types, &context),
                            &functions[n].unwrap(),
                            !d.captures.is_empty(),
                            &context,
                            &builder,
                        ));
                    }
                    IrInstr::Capture(c) => {
                        values.push(emit_instr_capture(
                            c,
                            def.as_ref().unwrap().capture_types[*c],
                            &ir.types,
                            &functions[n].unwrap(),
                            &context,
                            &builder,
                        ));
                    }
                    IrInstr::Apply(f, p) => {
                        values.push(emit_instr_apply(
                            values[*f],
                            p.iter().map(|x| values[*x]).collect(),
                            d.code[*f].value_type,
                            p.iter().map(|x| d.code[*x].value_type).collect(),
                            &ir.types,
                            &functions[n].unwrap(),
                            &context,
                            &builder,
                        ));
                    }
                    IrInstr::Closure(f, c) => {
                        let captures: Vec<_> = c.iter().map(|cap| values[*cap]).collect();
                        values.push(emit_instr_closure(
                            functions[*f].unwrap(),
                            captures,
                            &context,
                            &builder,
                        )?);
                    }
                    IrInstr::Function(f) => {
                        values.push(
                            functions[*f]
                                .unwrap()
                                .as_global_value()
                                .as_pointer_value()
                                .into(),
                        );
                    }
                    IrInstr::Move(p) => {
                        let param = values[*p];
                        values.push(param);
                    }
                    IrInstr::Enum(v, c) => {
                        let contained = c.map(|cc| values[cc]);
                        values.push(emit_instr_enum(
                            *v,
                            contained,
                            loc.value_type,
                            &ir.types,
                            &context,
                            &builder,
                        )?);
                    }
                }
            }
            if !values.is_empty() {
                let return_value = values.last().unwrap();
                let return_type = return_value.get_type();
                let cast_return_type: BasicTypeEnum = context
                    .custom_width_int_type(
                        (8 * compute_type_size(return_type).0).try_into().unwrap(),
                    )
                    .into();
                let return_ptr = builder.build_alloca(return_type, "return_ptr").unwrap();
                builder.build_store(return_ptr, *return_value);
                let cast_return_value = builder
                    .build_load(cast_return_type, return_ptr, "cast_return_value")
                    .unwrap();
                builder.build_return(Some(&cast_return_value)).unwrap();
            }
        }
    }
    //module.print_to_stderr();
    module.verify().unwrap();
    let target_triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let _ = Target::initialize_native(&InitializationConfig {
        asm_parser: false,
        asm_printer: true,
        base: true,
        disassembler: false,
        info: false,
        machine_code: true,
    });
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "x86-64",
            "",
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();
    module.set_triple(&target_triple);
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module
        .run_passes("inline", &target_machine, PassBuilderOptions::create())
        .unwrap();
    //module.print_to_stderr();
    target_machine
        .write_to_file(&module, FileType::Assembly, std::path::Path::new("./out.s"))
        .or(Err(()))?;
    target_machine
        .write_to_file(&module, FileType::Object, std::path::Path::new("./out.o"))
        .or(Err(()))?;
    Ok(())
}
