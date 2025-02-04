use crate::{Ir, IrInstr, IrType};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue};
use inkwell::{AddressSpace, OptimizationLevel};

fn emit_type<'a>(
    value_type: Option<usize>,
    types: &[Option<IrType>],
    context: &'a Context,
) -> BasicTypeEnum<'a> {
    match value_type {
        Some(t) => match types[t].as_ref().unwrap() {
            IrType::Enum(_variants) => {
                // TODO: handle content
                context.i64_type().into()
            }
            IrType::Closure(_, _) => {
                let closure_type = context.ptr_type(AddressSpace::from(0));
                closure_type.into()
            }
            _ => todo!(),
        },
        None => todo!(),
    }
}

fn emit_instr_param<'a>(p: &usize, function: &FunctionValue<'a>) -> BasicValueEnum<'a> {
    function.get_nth_param(*p as u32 + 1).unwrap()
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
            let return_type = emit_type(*r, types, context);
            let fn_type = return_type.fn_type(
                &[closure_type.into()]
                    .into_iter()
                    .chain(p.iter().map(|t| emit_type(*t, types, context).into()))
                    .collect::<Vec<_>>(),
                false,
            );
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
        IrType::Enum(variants) => {
            let switch_value: IntValue = closure.try_into().unwrap();
            let start_block = builder.get_insert_block().unwrap();
            let landing_block = context.append_basic_block(*function, "match_landing");
            let enum_type = context.i64_type();
            let mut cases = vec![];
            let mut values = vec![];
            for (n, _variant) in variants.iter().enumerate() {
                let enum_value = enum_type.const_int(n.try_into().unwrap(), false);
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
    context: &'a Context,
) -> Result<BasicValueEnum<'a>, ()> {
    if contained.is_some() {
        todo!();
    }
    let enum_type = context.i64_type();
    let enum_value = enum_type.const_int(variant.try_into().unwrap(), false);
    Ok(enum_value.into())
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
            let return_type = emit_type(d.code.last().unwrap().value_type, &ir.types, &context);
            let function_type = return_type.fn_type(
                &[closure_type.into()]
                    .into_iter()
                    .chain(
                        d.param_types
                            .iter()
                            .map(|t| emit_type(*t, &ir.types, &context).into()),
                    )
                    .collect::<Vec<_>>(),
                false,
            );
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
                        values.push(emit_instr_param(p, &functions[n].unwrap()));
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
                    IrInstr::Move(p) => {
                        let param = values[*p];
                        values.push(param);
                    }
                    IrInstr::Enum(v, c) => {
                        if c.is_some() {
                            todo!();
                        }
                        values.push(emit_instr_enum(*v, None, &context)?);
                    }
                }
            }
            if !values.is_empty() {
                builder.build_return(Some(values.last().unwrap())).unwrap();
            }
        }
    }
    module.print_to_stderr();
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
    target_machine
        .write_to_file(&module, FileType::Assembly, std::path::Path::new("./out.s"))
        .or(Err(()))?;
    target_machine
        .write_to_file(&module, FileType::Object, std::path::Path::new("./out.o"))
        .or(Err(()))?;
    Ok(())
}
