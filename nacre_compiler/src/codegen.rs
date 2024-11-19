use crate::{Ir, IrDef, IrInstr};
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::IntType;
use inkwell::{AddressSpace, OptimizationLevel};
use std::cell::LazyCell;
use std::sync::Arc;
use std::sync::Mutex;

pub(crate) fn emit_code(ir: &Ir) -> Result<(), ()> {
    let context = Context::create();
    let module = context.create_module("nacre");
    let builder = context.create_builder();
    let closure_type = context.ptr_type(AddressSpace::from(0));
    // first param is self, second is actual param
    let fn_type = closure_type.fn_type(&[closure_type.into(), closure_type.into()], false);
    let mut functions = vec![];
    for (n, def) in ir.defs.iter().enumerate() {
        let function = if let Some(d) = def {
            let function_name = if let Some(name) = &d.name {
                name.clone()
            } else {
                format!(".fn{}", n)
            };
            let function_type = fn_type;
            let function_linkage = if d.export {
                Some(Linkage::External)
            } else {
                Some(Linkage::Private)
            };
            module.add_function(&function_name, function_type, function_linkage)
        } else {
            module.add_function(&format!(".fn{}", n), fn_type, None)
        };
        functions.push(function);
    }
    for (n, def) in ir.defs.iter().enumerate() {
        match def {
            Some(d) => {
                let basic_block = context.append_basic_block(functions[n], "");
                builder.position_at_end(basic_block);
                let mut values = vec![];
                for instr in &d.code {
                    match instr {
                        IrInstr::Param(p) => {
                            let param = functions[n].get_nth_param(*p as u32 + 1).unwrap();
                            values.push(param);
                        }
                        IrInstr::Capture(c) => {
                            // TODO: compute actual captures
                            let self_param = functions[n].get_nth_param(0).unwrap();
                            let closure = self_param.into_pointer_value();
                            let i32_type = context.i32_type();
                            let capture_address = unsafe {
                                builder.build_gep(
                                    closure_type,
                                    closure,
                                    &[i32_type.const_int(*c as u64 + 1, false)],
                                    "capture_addr",
                                )
                            }
                            .unwrap();
                            let capture = builder
                                .build_load(closure_type, capture_address, "capture")
                                .unwrap();
                            values.push(capture);
                        }
                        IrInstr::Apply(f, p) => {
                            let param = values[p[0]].clone();
                            let r = {
                                let closure = values[*f].clone().into_pointer_value();
                                let func = builder
                                    .build_load(
                                        fn_type.ptr_type(AddressSpace::from(0)),
                                        closure,
                                        "func",
                                    )
                                    .unwrap()
                                    .into_pointer_value();
                                builder
                                    .build_indirect_call(
                                        fn_type,
                                        func,
                                        &[closure.into(), param.into()],
                                        "apply",
                                    )
                                    .unwrap()
                            };
                            values.push(r.try_as_basic_value().left().unwrap());
                        }
                        IrInstr::Closure(f, c) => {
                            let len = c.len() as u64 + 1;
                            let func = functions[*f].as_global_value().as_pointer_value();
                            let i32_type = context.i32_type();
                            let closure = builder
                                .build_array_malloc(
                                    closure_type,
                                    i32_type.const_int(len, false),
                                    "closure",
                                )
                                .unwrap();
                            builder.build_store(closure, func);
                            for (capn, cap) in c.iter().enumerate() {
                                let capture_address = unsafe {
                                    builder.build_gep(
                                        closure_type,
                                        closure,
                                        &[i32_type.const_int(capn as u64 + 1, false)],
                                        "capture_addr",
                                    )
                                }
                                .unwrap();
                                let capture_value = values[*cap].clone();
                                builder.build_store(capture_address, capture_value);
                            }
                            values.push(closure.into());
                        }
                    }
                }
                if values.len() > 0 {
                    builder.build_return(Some(values.last().unwrap())).unwrap();
                }
            }
            None => {}
        }
    }
    module.print_to_stderr();
    module.verify().unwrap();
    let target_triple = TargetTriple::create("x86_64-unknown-linux-gnu");
    let target = Target::initialize_native(&InitializationConfig {
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
    target_machine.write_to_file(&module, FileType::Assembly, std::path::Path::new("./out.s"));
    target_machine.write_to_file(&module, FileType::Object, std::path::Path::new("./out.o"));
    Ok(())
}
