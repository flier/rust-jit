use std::borrow::Cow;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::{self, NonNull};
use std::slice;
use std::str;

use crate::llvm::LLVMModuleFlagBehavior;
use libc;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::context::{Context, GlobalContext};
use crate::module::Module;
use crate::utils::{unchecked_cstr, AsBool, AsRaw, AsResult, IntoRaw};
use crate::value::{AsValueRef, Instruction, ValueRef};

pub type MDKindId = libc::c_uint;

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MetadataAsValue(ValueRef);

inherit_from!(MetadataAsValue, ValueRef; LLVMValueRef);

impl MetadataAsValue {
    /// Obtain the given MDNode's operands.
    pub fn operands(&self) -> Vec<ValueRef> {
        let num = unsafe { LLVMGetMDNodeNumOperands(self.as_raw()) };
        let mut values = vec![ptr::null_mut(); num as usize];

        unsafe { LLVMGetMDNodeOperands(self.as_raw(), values.as_mut_ptr()) }

        values.into_iter().map(|v| v.into()).collect()
    }

    /// Obtain a Value as Metadata.
    pub fn as_metadata<T>(&self) -> T
    where
        T: From<LLVMMetadataRef>,
    {
        unsafe { LLVMValueAsMetadata(self.as_raw()) }.into()
    }

    pub fn is_md_node(&self) -> bool {
        !unsafe { LLVMIsAMDNode(self.as_raw()) }.is_null()
    }

    pub fn is_md_string(&self) -> bool {
        !unsafe { LLVMIsAMDString(self.as_raw()) }.is_null()
    }

    pub fn as_md_string(&self) -> Option<MDString> {
        if self.is_md_string() {
            Some(self.as_metadata())
        } else {
            None
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        let mut len = 0;

        unsafe {
            NonNull::new(LLVMGetMDString(self.as_raw(), &mut len) as *mut u8)
                .map(|data| str::from_utf8_unchecked(slice::from_raw_parts(data.as_ptr() as *const u8, len as usize)))
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MDString(Metadata);

inherit_from!(MDString, Metadata; LLVMMetadataRef);

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MDNode(Metadata);

inherit_from!(MDNode, Metadata; LLVMMetadataRef);

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Metadata(LLVMMetadataRef);

inherit_from!(Metadata; LLVMMetadataRef);

impl Metadata {
    /// Obtain Metadata as a Value.
    pub fn as_value(&self, ctxt: &Context) -> MetadataAsValue {
        unsafe { LLVMMetadataAsValue(ctxt.as_raw(), self.as_raw()) }.into()
    }
}

impl Module {
    /// Obtain the named metadata operands for a module.
    pub fn named_operands<S: AsRef<str>>(&self, name: S) -> Vec<MetadataAsValue> {
        let name = name.as_ref();
        let count = unsafe { LLVMGetNamedMetadataNumOperands(self.as_raw(), cstr!(name)) };

        let mut operands = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetNamedMetadataOperands(self.as_raw(), cstr!(name), operands.as_mut_ptr()) };

        operands.into_iter().map(|v| v.into()).collect()
    }

    /// Add an operand to named metadata.
    pub fn add_named_operand<S: AsRef<str>, V: Deref<Target = ValueRef>>(&self, name: S, v: V) {
        unsafe { LLVMAddNamedMetadataOperand(self.as_raw(), cstr!(name), v.as_raw()) }
    }

    /// Returns the module flags.
    pub fn flags(&self) -> ModuleFlagsMetadata {
        ModuleFlagsMetadata::new(self)
    }

    /// Get a module-level flag to the module-level flags metadata.
    pub fn flag<S: AsRef<str>>(&self, key: S) -> Metadata {
        let key = key.as_ref();

        unsafe { LLVMGetModuleFlag(self.as_raw(), key.as_ptr() as *const libc::c_char, key.len()) }.into()
    }

    /// Add a module-level flag to the module-level flags metadata if it doesn't already exist.
    pub fn add_flag<S: AsRef<str>>(&self, behavior: LLVMModuleFlagBehavior, key: S, metadata: Metadata) {
        let key = key.as_ref();

        unsafe {
            LLVMAddModuleFlag(
                self.as_raw(),
                behavior,
                key.as_ptr() as *const libc::c_char,
                key.len(),
                metadata.as_raw(),
            )
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleFlagEntry<'a> {
    pub behavior: LLVMModuleFlagBehavior,
    pub key: Cow<'a, str>,
    pub metadata: Metadata,
}

#[derive(Debug)]
pub struct ModuleFlagsMetadata<'a> {
    entry: *mut LLVMModuleFlagEntry,
    len: usize,
    index: usize,
    phantom: PhantomData<&'a u8>,
}

impl<'a> Drop for ModuleFlagsMetadata<'a> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModuleFlagsMetadata(self.entry) }
    }
}

impl<'a> ModuleFlagsMetadata<'a> {
    fn new<M: AsRaw<RawType = LLVMModuleRef>>(m: &M) -> Self {
        let mut len = 0;
        let entry = unsafe { LLVMCopyModuleFlagsMetadata(m.as_raw(), &mut len) };

        ModuleFlagsMetadata {
            entry,
            len,
            index: 0,
            phantom: PhantomData,
        }
    }
}

impl<'a> Iterator for ModuleFlagsMetadata<'a> {
    type Item = ModuleFlagEntry<'a>;

    fn next(&mut self) -> Option<ModuleFlagEntry<'a>> {
        if self.index < self.len {
            let behavior = unsafe { LLVMModuleFlagEntriesGetFlagBehavior(self.entry, self.index as libc::c_uint) };
            let key = unsafe {
                let mut len = 0;
                let p = LLVMModuleFlagEntriesGetKey(self.entry, self.index as libc::c_uint, &mut len);

                unchecked_cstr(p as *const u8, len as usize + 1)
            };
            let metadata = unsafe { LLVMModuleFlagEntriesGetMetadata(self.entry, self.index as libc::c_uint) }.into();

            self.index += 1;

            Some(ModuleFlagEntry {
                behavior,
                key,
                metadata,
            })
        } else {
            None
        }
    }
}

impl Context {
    pub fn md_kind_id<T: AsRef<str>>(&self, name: T) -> MDKindId {
        let name = name.as_ref();

        unsafe { LLVMGetMDKindIDInContext(self.as_raw(), name.as_ptr() as *const i8, name.len() as u32) }
    }

    pub fn md_string<T: AsRef<str>>(&self, name: T) -> MetadataAsValue {
        let name = name.as_ref();

        unsafe { LLVMMDStringInContext(self.as_raw(), name.as_ptr() as *const i8, name.len() as u32) }.into()
    }

    pub fn md_node<I: Iterator<Item = ValueRef>>(&self, values: I) -> MetadataAsValue {
        let mut values = values.map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMMDNodeInContext(self.as_raw(), values.as_mut_ptr(), values.len() as u32) }.into()
    }
}

impl GlobalContext {
    pub fn md_kind_id<T: AsRef<str>>(name: T) -> MDKindId {
        let name = name.as_ref();

        unsafe { LLVMGetMDKindID(name.as_ptr() as *const i8, name.len() as u32) }
    }

    pub fn md_string<T: AsRef<str>>(name: T) -> MetadataAsValue {
        let name = name.as_ref();

        unsafe { LLVMMDString(name.as_ptr() as *const i8, name.len() as u32) }.into()
    }

    pub fn md_node<I: Iterator<Item = ValueRef>>(values: I) -> MetadataAsValue {
        let mut values = values.map(|v| v.as_raw()).collect::<Vec<_>>();

        unsafe { LLVMMDNode(values.as_mut_ptr(), values.len() as u32) }.into()
    }
}

impl Instruction {
    /// Determine whether an instruction has any metadata attached.
    pub fn has_metadata(&self) -> bool {
        unsafe { LLVMHasMetadata(self.as_raw()) }.as_bool()
    }

    /// Return metadata associated with an instruction value.
    pub fn metadata(&self, kind_id: MDKindId) -> Option<MetadataAsValue> {
        unsafe { LLVMGetMetadata(self.as_raw(), kind_id) }
            .ok()
            .map(|v: LLVMValueRef| MetadataAsValue(v.into()))
    }

    /// Set metadata associated with an instruction value.
    pub fn set_metadata<T: AsValueRef>(&self, kind_id: MDKindId, node: T) -> &Self {
        unsafe { LLVMSetMetadata(self.as_raw(), kind_id, node.into_raw()) };
        self
    }
}
