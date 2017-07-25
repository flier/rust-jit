use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::TypeRef;
use utils::unchecked_cstring;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

pub type ValueKind = LLVMValueKind;

impl ValueRef {
    /// Wrap a raw value reference.
    pub fn from_raw(v: LLVMValueRef) -> Self {
        ValueRef(v)
    }

    /// Extracts the raw value reference.
    pub fn as_raw(&self) -> LLVMValueRef {
        self.0
    }

    /// Dump a representation of a value to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.0) }
    }

    /// Obtain the type of a value.
    pub fn type_of(&self) -> TypeRef {
        TypeRef::from_raw(unsafe { LLVMTypeOf(self.0) })
    }

    /// Obtain the enumerated type of a Value instance.
    pub fn value_kind(&self) -> ValueKind {
        unsafe { LLVMGetValueKind(self.0) }
    }

    /// Obtain the string name of a value.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe {
            let name = LLVMGetValueName(self.0);

            if name.is_null() {
                None
            } else {
                Some(CStr::from_ptr(name).to_string_lossy())
            }
        }
    }

    /// Set the string name of a value.
    pub fn set_name<S: AsRef<str>>(&mut self, name: S) {
        unsafe { LLVMSetValueName(self.0, unchecked_cstring(name).as_ptr()) }
    }

    /// Determine whether the specified value instance is constant.
    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) != 0 }
    }

    /// Determine whether a value instance is undefined.
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.0) != 0 }
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let s = LLVMPrintValueToString(self.0);

            let r = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());

            LLVMDisposeMessage(s);

            r
        }
    }
}

pub type Function = ValueRef;

impl Function {
    pub fn append_basic_block<S: AsRef<str>>(&self, context: &Context, name: S) -> BasicBlock {
        let cname = unchecked_cstring(name);
        let block =
            unsafe { LLVMAppendBasicBlockInContext(context.as_raw(), self.0, cname.as_ptr()) };

        trace!(
            "{:?} create `{}` BasicBlock({:?}) in {:?}",
            self,
            cname.to_string_lossy(),
            block,
            context
        );

        BasicBlock::from_raw(block)
    }

    pub fn params(&self) -> Vec<ValueRef> {
        let count = unsafe { LLVMCountParams(self.0) };
        let mut params: Vec<LLVMValueRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetParams(self.0, params.as_mut_ptr()) };

        params.into_iter().map(|v| ValueRef(v)).collect()
    }

    pub fn param(&self, index: u32) -> ValueRef {
        ValueRef(unsafe { LLVMGetParam(self.0, index) })
    }
}
