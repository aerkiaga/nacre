use crate::{Ir, IrInstr, IrType};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{BasicType, BasicTypeEnum, IntType};
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
            // TODO: handle content
            let variant_count = variants.len();
            let tag_type = emit_tag_type(variant_count, context);
            let (tag_size, _) = compute_type_size(tag_type);
            let (max_variant_size, max_variant_alignment) = variants
                .iter()
                .map(|vt| match vt {
                    Some(vts) => compute_type_size(emit_type(*vts, types, context)),
                    None => (0, 1),
                })
                .fold((0, 1), |(xs, xa), (ys, ya)| (max(xs, ys), max(xa, ya)));
            let field_alignment = max_variant_alignment * tag_size.div_ceil(max_variant_alignment);
            let middle_padding = field_alignment - tag_size;
            let nontag_size = max_variant_size + middle_padding;
            let struct_type = if nontag_size > 0 {
                let array_type = context
                    .i8_type()
                    .array_type(nontag_size.try_into().unwrap());
                context.struct_type(&[tag_type, array_type.into()], false)
            } else {
                context.struct_type(&[tag_type], false)
            };
            struct_type.into()
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
        _ => todo!(),
    }
}

fn emit_instr_param<'a>(
    p: &usize,
    function: &FunctionValue<'a>,
    closure: bool,
) -> BasicValueEnum<'a> {
    function
        .get_nth_param(*p as u32 + if closure { 1 } else { 0 })
        .unwrap()
}

fn emit_instr_capture<'a>(
    c: &usize,
    function: &FunctionValue<'a>,
    context: &'a Context,
    builder: &'a Builder,
) -> BasicValueEnum<'a> {
    let self_param = function.get_nth_param(0).unwrap();
    let closure = self_param.into_pointer_value();
    let i32_type = context.i32_type();
    let closure_type = context.ptr_type(AddressSpace::from(0));
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
        .build_load(closure_type, capture_address, "capture")
        .unwrap()
    // TODO: emit correct capture type
}

fn emit_instr_apply<'a>(
    closure: BasicValueEnum<'a>,
    params: Vec<BasicValueEnum<'a>>,
    ir_type: Option<usize>,
    types: &[Option<IrType>],
    function: &FunctionValue<'a>,
    context: &'a Context,
    builder: &'a Builder,
) -> BasicValueEnum<'a> {
    match types[ir_type.unwrap()].as_ref().unwrap() {
        IrType::Closure(p, r) => {
            let closure_type = context.ptr_type(AddressSpace::from(0));
            let param_types = [closure_type.into()]
                .into_iter()
                .chain(
                    p.iter()
                        .map(|t| emit_type(t.unwrap(), types, context).into()),
                )
                .collect::<Vec<_>>();
            let fn_type = match r {
                Some(rs) => emit_type(*rs, types, context).fn_type(&param_types, false),
                None => context.void_type().fn_type(&param_types, false),
            };
            let r = {
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
            r.try_as_basic_value().left().unwrap()
        }
        IrType::Function(p, r) => {
            let param_types = p
                .iter()
                .map(|t| emit_type(t.unwrap(), types, context).into())
                .collect::<Vec<_>>();
            let fn_type = match r {
                Some(rs) => emit_type(*rs, types, context).fn_type(&param_types, false),
                None => context.void_type().fn_type(&param_types, false),
            };
            let r = {
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
            r.try_as_basic_value().left().unwrap()
        }
        IrType::Enum(variants) => {
            let struct_value: StructValue = closure.try_into().unwrap();
            let switch_value: IntValue = builder
                .build_extract_value(struct_value, 0, "tag")
                .unwrap()
                .try_into()
                .unwrap();
            let start_block = builder.get_insert_block().unwrap();
            let landing_block = context.append_basic_block(*function, "match_landing");
            let tag_type: IntType = struct_value
                .get_type()
                .get_field_type_at_index(0)
                .unwrap()
                .try_into()
                .unwrap();
            let mut cases = vec![];
            let mut values = vec![];
            for (n, _variant) in variants.iter().enumerate() {
                let enum_value = tag_type.const_int(n.try_into().unwrap(), false);
                let variant_block = context.append_basic_block(*function, "match_variant");
                builder.position_at_end(variant_block);
                values.push(params[n]);
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
    contained: Option<BasicValueEnum>,
    value_type: Option<usize>,
    types: &[Option<IrType>],
    context: &'a Context,
) -> Result<BasicValueEnum<'a>, ()> {
    let value_type = types[value_type.unwrap()].as_ref().unwrap();
    if contained.is_some() {
        todo!();
    }
    let variants = if let IrType::Enum(variants) = value_type {
        variants
    } else {
        panic!()
    };
    let tag_type: IntType = emit_tag_type(variants.len(), context).try_into().unwrap();
    let tag_value = tag_type.const_int(variant.try_into().unwrap(), false);
    let field_type = variants[variant].map(|v| emit_type(v, types, context));
    Ok(match field_type {
        Some(_ft) => todo!(),
        None => context.const_struct(&[tag_value.into()], false).into(),
    })
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
                    .map(|t| emit_type(t.unwrap(), &ir.types, &context).into())
                    .collect::<Vec<_>>()
            } else {
                [closure_type.into()]
                    .into_iter()
                    .chain(
                        d.param_types
                            .iter()
                            .map(|t| emit_type(t.unwrap(), &ir.types, &context).into()),
                    )
                    .collect::<Vec<_>>()
            };
            let r = d.code.last().unwrap().value_type;
            let function_type = match r {
                Some(rs) => emit_type(rs, &ir.types, &context).fn_type(&param_types, false),
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
                            &functions[n].unwrap(),
                            !d.captures.is_empty(),
                        ));
                    }
                    IrInstr::Capture(c) => {
                        values.push(emit_instr_capture(
                            c,
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
                        if c.is_some() {
                            todo!();
                        }
                        values.push(emit_instr_enum(
                            *v,
                            None,
                            loc.value_type,
                            &ir.types,
                            &context,
                        )?);
                    }
                }
            }
            if !values.is_empty() {
                builder.build_return(Some(values.last().unwrap())).unwrap();
            }
        }
    }
    //module.print_to_stderr();
    module.verify().unwrap();
    let target_triple = TargetTriple::create("x86_64-unknown-linux-gnu");
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
            "",
            "",
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();
    module
        .run_passes("inline", &target_machine, PassBuilderOptions::create())
        .unwrap();
    module.print_to_stderr();
    target_machine
        .write_to_file(&module, FileType::Assembly, std::path::Path::new("./out.s"))
        .or(Err(()))?;
    target_machine
        .write_to_file(&module, FileType::Object, std::path::Path::new("./out.o"))
        .or(Err(()))?;
    Ok(())
}
