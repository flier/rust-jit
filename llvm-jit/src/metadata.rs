use std::ptr;

use libc;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use context::{Context, GlobalContext};
use module::Module;
use utils::{AsBool, AsRaw};
use value::{Instruction, ValueRef};

pub type KindId = libc::c_uint;

impl Module {
    /// Obtain the named metadata operands for a module.
    pub fn get_named_operands<S: AsRef<str>>(&self, name: S) -> Vec<ValueRef> {
        let name = name.as_ref();
        let count = unsafe { LLVMGetNamedMetadataNumOperands(self.as_raw(), cstr!(name)) };

        let mut operands = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetNamedMetadataOperands(self.as_raw(), cstr!(name), operands.as_mut_ptr()) };

        operands.into_iter().map(|v| v.into()).collect()
    }

    /// Add an operand to named metadata.
    pub fn add_named_operand<S: AsRef<str>, V: AsRef<ValueRef>>(&self, name: S, v: V) {
        unsafe {
            LLVMAddNamedMetadataOperand(self.as_raw(), cstr!(name.as_ref()), v.as_ref().as_raw())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Metadata(LLVMMetadataRef);

inherit_from!(Metadata, LLVMMetadataRef);

impl<T: AsRaw<RawType = LLVMValueRef>> From<T> for Metadata {
    fn from(value: T) -> Self {
        Metadata(unsafe { LLVMValueAsMetadata(value.as_raw()) })
    }
}

impl Context {
    pub fn as_value<T: AsRaw<RawType = LLVMMetadataRef>>(&self, metadata: T) -> ValueRef {
        unsafe { LLVMMetadataAsValue(self.as_raw(), metadata.as_raw()) }.into()
    }

    pub fn metadata_id<T: AsRef<str>>(&self, name: T) -> KindId {
        let name = name.as_ref();

        unsafe {
            LLVMGetMDKindIDInContext(self.as_raw(), name.as_ptr() as *const i8, name.len() as u32)
        }
    }
}

impl GlobalContext {
    pub fn metadata_id<T: AsRef<str>>(name: T) -> KindId {
        let name = name.as_ref();

        unsafe { LLVMGetMDKindID(name.as_ptr() as *const i8, name.len() as u32) }
    }
}

impl Instruction {
    /// Determine whether an instruction has any metadata attached.
    pub fn has_metadata(&self) -> bool {
        unsafe { LLVMHasMetadata(self.as_raw()) }.as_bool()
    }

    /// Return metadata associated with an instruction value.
    pub fn get_metadata(&self, kind_id: KindId) -> ValueRef {
        unsafe { LLVMGetMetadata(self.as_raw(), kind_id) }.into()
    }

    /// Set metadata associated with an instruction value.
    pub fn set_metadata(&self, kind_id: KindId, node: ValueRef) -> &Self {
        unsafe { LLVMSetMetadata(self.as_raw(), kind_id, node.as_raw()) };
        self
    }
}
