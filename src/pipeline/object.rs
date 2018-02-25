//!
//! Koak's modules-to-object pipeline
//!

use std::ptr;
use std::process::{Command, exit};
use std::path::Path;
use std::ffi::{CStr, CString};

use llvm_sys::target_machine::*;

use iron_llvm::LLVMRef;
use iron_llvm::core::Module;

use args::Args;

pub struct ObjectPipeline<'a> {
    pub args: &'a Args,
    pub modules: Vec<(String, Module)>
}


impl<'a> ObjectPipeline<'a> {
    pub fn new(args: &'a Args) -> ObjectPipeline<'a> {
        ObjectPipeline {
            args: args,
            modules: Vec::new(),
        }
    }

    pub unsafe fn run(&self) {
        // Error handler
        let mut errors: *mut ::libc::c_char = ptr::null_mut();

        // Get target triple
        let target_triple = LLVMGetDefaultTargetTriple();

        // Get target
        let mut target_ref: LLVMTargetRef = ptr::null_mut();
        if LLVMGetTargetFromTriple(target_triple, &mut target_ref, &mut errors) != 0 {
            eprintln!("Failed creating a target from triple {:?}", CStr::from_ptr(target_triple));
            exit(1);
        }

        // Get target machine
        let tm = LLVMCreateTargetMachine(
            target_ref,
            target_triple,
            "\0".as_ptr() as *const ::libc::c_char,
            "\0".as_ptr() as *const ::libc::c_char,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocPIC,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        if tm == ptr::null_mut() {
            eprintln!("Failed creating a target machine from triple {:?}", CStr::from_ptr(target_triple));
            exit(1);
        }

        // Emit object code for modules
        let mut c = Command::new("cc");

        for &(ref name, ref module) in &self.modules {
            let path = Path::new(name).with_extension("o");
            c.arg(&path);
            let c = CString::new(path.to_str().expect("Invalid output path")).expect(&format!("Can't create output file {}", path.display()));
            if LLVMTargetMachineEmitToFile(tm, module.to_ref(), c.into_raw() as *mut ::libc::c_char, LLVMCodeGenFileType::LLVMObjectFile, &mut errors) != 0 {
                eprintln!("Can't emit object file {}", path.display());
                exit(1);
            }
        }
        if self.args.link {
            if let Some(ref name) = self.args.out {
                c.arg("-o");
                c.arg(name);
            }
            c.spawn().expect("cc failed");
        }
    }
}
