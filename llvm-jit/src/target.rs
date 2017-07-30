use std::ffi::CStr;
use std::fmt;

use llvm::core::*;
use llvm::target::*;

use context::Context;
use global::GlobalVar;
use module::{AddressSpace, Module};
use types::{AsTypeRef, StructType, TypeRef};
use utils::unchecked_cstring;

pub struct TargetData(LLVMTargetDataRef);

inherit_from!(TargetData, LLVMTargetDataRef);

impl Drop for TargetData {
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetData(self.0) }
    }
}

impl fmt::Display for TargetData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let s = LLVMCopyStringRepOfTargetData(self.0);

            let r = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());

            LLVMDisposeMessage(s);

            r
        }
    }
}

impl TargetData {
    /// Creates target data from a target layout string.
    pub fn create<S: AsRef<str>>(s: S) -> Self {
        unsafe { LLVMCreateTargetData(unchecked_cstring(s).as_ptr()) }.into()
    }

    /// Returns the byte order of a target, either `LLVMBigEndian` or `LLVMLittleEndian`.
    pub fn byte_order(&self) -> LLVMByteOrdering {
        unsafe { LLVMByteOrder(self.0) }
    }

    /// Returns the pointer size in bytes for a target.
    pub fn pointer_size(&self) -> usize {
        unsafe { LLVMPointerSize(self.0) as usize }
    }

    /// Returns the pointer size in bytes for a target for a specified address space.
    pub fn pointer_size_for_address_space(&self, address_space: AddressSpace) -> usize {
        unsafe { LLVMPointerSizeForAS(self.0, address_space) as usize }
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn pointer_type(&self) -> TypeRef {
        unsafe { LLVMIntPtrType(self.0) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn int_ptr_type(&self) -> TypeRef {
        unsafe { LLVMIntPtrType(self.0) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    ///
    /// This version allows the address space to be specified.
    pub fn int_ptr_type_for_address_space(&self, address_space: AddressSpace) -> TypeRef {
        unsafe { LLVMIntPtrTypeForAS(self.0, address_space) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    pub fn int_ptr_type_in_context(&self, context: &Context) -> TypeRef {
        unsafe { LLVMIntPtrTypeInContext(context.as_raw(), self.0) }.into()
    }

    /// Returns the integer type that is the same size as a pointer on a target.
    ///
    /// This version allows the address space to be specified.
    pub fn int_ptr_type_for_address_space_in_context(
        &self,
        context: &Context,
        address_space: AddressSpace,
    ) -> TypeRef {
        unsafe { LLVMIntPtrTypeForASInContext(context.as_raw(), self.0, address_space) }.into()
    }

    /// Computes the size of a type in bits for a target.
    pub fn size_of_type_in_bits<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMSizeOfTypeInBits(self.0, ty.as_raw()) as usize }
    }

    /// Computes the storage size of a type in bytes for a target.
    pub fn storage_size_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMStoreSizeOfType(self.0, ty.as_raw()) as usize }
    }

    /// Computes the ABI size of a type in bytes for a target.
    pub fn abi_size_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMABISizeOfType(self.0, ty.as_raw()) as usize }
    }

    /// Computes the ABI alignment of a type in bytes for a target.
    pub fn abi_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMABIAlignmentOfType(self.0, ty.as_raw()) as usize }
    }

    /// Computes the call frame alignment of a type in bytes for a target.
    pub fn call_frame_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMCallFrameAlignmentOfType(self.0, ty.as_raw()) as usize }
    }

    /// Computes the preferred alignment of a type in bytes for a target.
    pub fn preferred_alignment_of_type<T: AsTypeRef>(&self, ty: T) -> usize {
        unsafe { LLVMPreferredAlignmentOfType(self.0, ty.as_raw()) as usize }
    }

    /// Computes the preferred alignment of a global variable in bytes for a target.
    pub fn preferred_alignment_of_global(&self, var: GlobalVar) -> usize {
        unsafe { LLVMPreferredAlignmentOfGlobal(self.0, var.as_raw()) as usize }
    }

    /// Computes the structure element that contains the byte offset for a target.
    pub fn element_at_offset(&self, ty: StructType, offset: usize) -> u32 {
        unsafe { LLVMElementAtOffset(self.0, ty.as_raw(), offset as u64) as u32 }
    }

    /// Computes the byte offset of the indexed struct element for a target.
    pub fn offset_of_element(&self, ty: StructType, element: u32) -> usize {
        unsafe { LLVMOffsetOfElement(self.0, ty.as_raw(), element) as usize }
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
