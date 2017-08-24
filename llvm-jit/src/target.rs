use std::borrow::Cow;
use std::fmt;
use std::path::Path;
use std::ptr;

use llvm::prelude::*;
use llvm::target::*;
use llvm::target_machine::*;

use context::Context;
use errors::{ErrorKind, Result};
use global::GlobalVar;
use membuf::MemoryBuffer;
use module::{AddressSpace, Module};
use types::{AsTypeRef, StructType, TypeRef};
use utils::{AsBool, AsLLVMBool, AsRaw, AsResult, DisposableMessage, UncheckedCStr};

pub type CodeGenFileType = LLVMCodeGenFileType;

#[derive(Clone, Debug, PartialEq)]
pub struct Target(LLVMTargetRef);

inherit_from!(Target, LLVMTargetRef);

impl Default for Target {
    fn default() -> Self {
        Target::from_triple(Target::default_triple()).unwrap()
    }
}

impl Target {
    /// Finds the target corresponding to the given name
    pub fn from_name<S: AsRef<str>>(name: S) -> Option<Target> {
        unsafe { LLVMGetTargetFromName(cstr!(name.as_ref())).as_mut() }.map(|target| Target(target))
    }

    /// Finds the target corresponding to the given triple
    pub fn from_triple<S: AsRef<str>>(triple: S) -> Result<Target> {
        let mut target = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe { LLVMGetTargetFromTriple(cstr!(triple.as_ref()), &mut target, &mut msg) }
            .ok_or_else(|| msg.into_string().into())
            .map(|_| target.into())
    }

    /// Get a triple for the host machine as a string.
    pub fn default_triple() -> String {
        unsafe { LLVMGetDefaultTargetTriple() }.into_string()
    }

    /// Returns the name of a target.
    pub fn name(&self) -> Cow<str> {
        unsafe { LLVMGetTargetName(self.as_raw()) }.as_str()
    }

    /// Returns the description  of a target.
    pub fn description(&self) -> Cow<str> {
        unsafe { LLVMGetTargetDescription(self.as_raw()) }.as_str()
    }

    /// Returns if the target has a JIT
    pub fn has_jit(&self) -> bool {
        unsafe { LLVMTargetHasJIT(self.as_raw()) }.as_bool()
    }

    /// Returns if the target has a TargetMachine associated
    pub fn has_target_machine(&self) -> bool {
        unsafe { LLVMTargetHasTargetMachine(self.as_raw()) }.as_bool()
    }

    /// Returns if the target as an ASM backend (required for emitting output)
    pub fn has_asm_backend(&self) -> bool {
        unsafe { LLVMTargetHasAsmBackend(self.as_raw()) }.as_bool()
    }
}

pub fn targets() -> TargetIter {
    TargetIter::new()
}

#[derive(Debug, Default)]
pub struct TargetIter(Option<LLVMTargetRef>);

impl TargetIter {
    pub fn new() -> Self {
        TargetIter::default()
    }
}

impl Iterator for TargetIter {
    type Item = Target;

    fn next(&mut self) -> Option<Self::Item> {
        self.0 = unsafe {
            if let Some(next) = self.0 {
                LLVMGetNextTarget(next)
            } else {
                LLVMGetFirstTarget()
            }.as_mut()
        }.map(|target| target as *mut LLVMTarget);

        self.0.map(Target)
    }
}

#[derive(Debug, PartialEq)]
pub struct TargetMachine(LLVMTargetMachineRef);

inherit_from!(TargetMachine, LLVMTargetMachineRef);

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetMachine(self.as_raw()) }
    }
}

impl TargetMachine {
    pub fn new(
        target: &Target,
        triple: &str,
        cpu: &str,
        feature: &str,
        opt_level: LLVMCodeGenOptLevel,
        reloc_mode: LLVMRelocMode,
        code_model: LLVMCodeModel,
    ) -> Self {
        TargetMachine(unsafe {
            LLVMCreateTargetMachine(
                target.0,
                cstr!(triple),
                cstr!(cpu),
                cstr!(feature),
                opt_level,
                reloc_mode,
                code_model,
            )
        })
    }

    /// Returns the Target used in a TargetMachine
    pub fn target(&self) -> Target {
        unsafe { LLVMGetTargetMachineTarget(self.as_raw()) }.into()
    }

    /// Returns the triple used creating this target machine.
    pub fn triple(&self) -> String {
        unsafe { LLVMGetTargetMachineTriple(self.as_raw()) }.into_string()
    }

    /// Returns the cpu used creating this target machine.
    pub fn cpu(&self) -> String {
        unsafe { LLVMGetTargetMachineCPU(self.as_raw()) }.into_string()
    }

    /// Returns the feature string used creating this target machine.
    pub fn feature(&self) -> String {
        unsafe { LLVMGetTargetMachineFeatureString(self.as_raw()) }.into_string()
    }

    /// Create a DataLayout based on the targetMachine.
    pub fn create_data_layout(&self) -> TargetData {
        TargetData(unsafe { LLVMCreateTargetDataLayout(self.as_raw()) })
    }

    /// Set the target machine's ASM verbosity.
    pub fn set_asm_verbosity(&self, verbose: bool) {
        unsafe { LLVMSetTargetMachineAsmVerbosity(self.as_raw(), verbose.as_bool()) }
    }

    /// Emits an asm or object file for the given module to the filename.
    pub fn emit_to_file<M, P>(&self, module: M, path: P, codegen: CodeGenFileType) -> Result<()>
    where
        M: AsRaw<RawType = LLVMModuleRef>,
        P: AsRef<Path>,
    {
        let mut err = DisposableMessage::new();

        unsafe {
            LLVMTargetMachineEmitToFile(
                self.as_raw(),
                module.as_raw(),
                cpath!(path.as_ref()) as *mut i8,
                codegen,
                &mut err,
            )
        }.ok_or_else(|| {
            ErrorKind::Msg(format!("fail to emit to file, {}", err.into_string())).into()
        })
    }

    /// Emits an asm or object file for the given module to the filename.
    pub fn emit_to_memory_buffer<M>(
        &self,
        module: M,
        codegen: CodeGenFileType,
    ) -> Result<MemoryBuffer>
    where
        M: AsRaw<RawType = LLVMModuleRef>,
    {
        let mut err = DisposableMessage::new();
        let mut buf = ptr::null_mut();

        unsafe {
            LLVMTargetMachineEmitToMemoryBuffer(
                self.as_raw(),
                module.as_raw(),
                codegen,
                &mut err,
                &mut buf,
            )
        }.ok_or_else(|| {
            ErrorKind::Msg(format!(
                "fail to emit to memory buffer, {}",
                err.into_string()
            )).into()
        })
            .map(|_| buf.into())
    }

    /// Adds the target-specific analysis passes to the pass manager.
    pub fn add_analysis_passes<P>(&self, passmgr: P)
    where
        P: AsRaw<RawType = LLVMPassManagerRef>,
    {
        unsafe { LLVMAddAnalysisPasses(self.as_raw(), passmgr.as_raw()) }
    }
}

#[derive(Debug, PartialEq)]
pub struct TargetData(LLVMTargetDataRef);

inherit_from!(TargetData, LLVMTargetDataRef);

impl Drop for TargetData {
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetData(self.as_raw()) }
    }
}

impl fmt::Display for TargetData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            unsafe { LLVMCopyStringRepOfTargetData(self.as_raw()) }.into_string()
        )
    }
}

impl TargetData {
    /// Creates target data from a target layout string.
    pub fn create<S: AsRef<str>>(s: S) -> Self {
        unsafe { LLVMCreateTargetData(cstr!(s.as_ref())) }.into()
    }

    /// Returns the byte order of a target, either `LLVMBigEndian` or `LLVMLittleEndian`.
    pub fn byte_order(&self) -> LLVMByteOrdering {
        unsafe { LLVMByteOrder(self.as_raw()) }
    }

    /// Returns the pointer size in bytes for a target.
    pub fn pointer_size(&self) -> usize {
        unsafe { LLVMPointerSize(self.as_raw()) as usize }
    }

    /// Returns the pointer size in bytes for a target for a specified address space.
    pub fn pointer_size_for_address_space(&self, address_space: AddressSpace) -> usize {
        unsafe { LLVMPointerSizeForAS(self.as_raw(), address_space) as usize }
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn pointer_type(&self) -> TypeRef {
        unsafe { LLVMIntPtrType(self.as_raw()) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn int_ptr_type(&self) -> TypeRef {
        unsafe { LLVMIntPtrType(self.as_raw()) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    ///
    /// This version allows the address space to be specified.
    pub fn int_ptr_type_for_address_space(&self, address_space: AddressSpace) -> TypeRef {
        unsafe { LLVMIntPtrTypeForAS(self.as_raw(), address_space) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn int_ptr_type_in_context(&self, context: &Context) -> TypeRef {
        unsafe { LLVMIntPtrTypeInContext(context.as_raw(), self.as_raw()) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    ///
    /// This version allows the address space to be specified.
    pub fn int_ptr_type_for_address_space_in_context(
        &self,
        context: &Context,
        address_space: AddressSpace,
    ) -> TypeRef {
        unsafe { LLVMIntPtrTypeForASInContext(context.as_raw(), self.as_raw(), address_space) }
            .into()
    }

    /// Computes the size of a type in bits for a target.
    pub fn size_of_type_in_bits<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMSizeOfTypeInBits(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the storage size of a type in bytes for a target.
    pub fn storage_size_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMStoreSizeOfType(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the ABI size of a type in bytes for a target.
    pub fn abi_size_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMABISizeOfType(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the ABI alignment of a type in bytes for a target.
    pub fn abi_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMABIAlignmentOfType(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the call frame alignment of a type in bytes for a target.
    pub fn call_frame_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMCallFrameAlignmentOfType(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the preferred alignment of a type in bytes for a target.
    pub fn preferred_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMPreferredAlignmentOfType(self.as_raw(), ty.as_raw()) as usize }
    }

    /// Computes the preferred alignment of a global variable in bytes for a target.
    pub fn preferred_alignment_of_global(&self, var: GlobalVar) -> usize {
        unsafe { LLVMPreferredAlignmentOfGlobal(self.as_raw(), var.as_raw()) as usize }
    }

    /// Computes the structure element that contains the byte offset for a target.
    pub fn element_at_offset(&self, ty: StructType, offset: usize) -> u32 {
        unsafe { LLVMElementAtOffset(self.as_raw(), ty.as_raw(), offset as u64) as u32 }
    }

    /// Computes the byte offset of the indexed struct element for a target.
    pub fn offset_of_element(&self, ty: StructType, element: u32) -> usize {
        unsafe { LLVMOffsetOfElement(self.as_raw(), ty.as_raw(), element) as usize }
    }
}

impl Module {
    /// Obtain the data layout for a module.
    pub fn data_layout(&self) -> TargetData {
        unsafe { LLVMGetModuleDataLayout(self.as_raw()) }.into()
    }

    /// Set the data layout for a module.
    pub fn set_data_layout(&self, layout: TargetData) {
        unsafe { LLVMSetModuleDataLayout(self.as_raw(), layout.as_raw()) }
    }
}

macro_rules! define_target {
    ($name:ident, $init:path) => {
        pub struct $name;

        impl $name {
            pub fn init() {
                unsafe { $init() }
            }
        }
    };
    ($name:ident, $init:path, $err:expr) => {
        pub struct $name;

        impl $name {
            pub fn init() -> $crate::errors::Result<()> {
                if unsafe { $init() } == 0 {
                    Ok(())
                } else {
                    bail!($err)
                }
            }
        }
    };
}

define_target!(AllTargets, LLVM_InitializeAllTargets);
define_target!(AllTargetInfos, LLVM_InitializeAllTargetInfos);
define_target!(AllTargetMCs, LLVM_InitializeAllTargetMCs);
define_target!(AllAsmPrinters, LLVM_InitializeAllAsmPrinters);
define_target!(AllAsmParsers, LLVM_InitializeAllAsmParsers);
define_target!(AllDisassemblers, LLVM_InitializeAllDisassemblers);

define_target!(
    NativeTarget,
    LLVM_InitializeNativeTarget,
    "fail to initialize native target"
);
define_target!(
    NativeAsmParser,
    LLVM_InitializeNativeAsmParser,
    "fail to initialize native assembler parser"
);
define_target!(
    NativeAsmPrinter,
    LLVM_InitializeNativeAsmPrinter,
    "fail to initialize native assembler printer"
);
define_target!(
    NativeDisassembler,
    LLVM_InitializeNativeDisassembler,
    "fail to initialize native disassemblerer"
);
