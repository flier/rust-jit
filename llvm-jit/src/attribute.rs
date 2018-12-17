#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::ptr;

use boolinator::Boolinator;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::*;

use crate::context::Context;
use crate::function::Function;
use crate::insts::CallSite;
use crate::utils::{from_unchecked_cstr, AsBool, AsRaw};

pub type AttributeIndex = LLVMAttributeIndex;

pub mod attrs {
    //! The target-independent enum attributes.

    use lazy_static::lazy_static;

    use super::AttributeKind;

    lazy_static! {
        /// Alignment of parameter (5 bits) stored as log2 of alignment with +1 bias.
        /// 0 means unaligned (different from align(1)).
        pub static ref Alignment: AttributeKind = AttributeKind::get("align").unwrap();

        /// The result of the function is guaranteed to point to a number of bytes that
        /// we can determine if we know the value of the function's arguments.
        pub static ref AllocSize: AttributeKind = AttributeKind::get("allocsize").unwrap();

        /// inline=always.
        pub static ref AlwaysInline: AttributeKind = AttributeKind::get("alwaysinline").unwrap();

        /// Function can access memory only using pointers based on its arguments.
        pub static ref ArgMemOnly: AttributeKind = AttributeKind::get("argmemonly").unwrap();

        /// Callee is recognized as a builtin, despite nobuiltin attribute on its declaration.
        pub static ref Builtin: AttributeKind = AttributeKind::get("builtin").unwrap();

        /// Pass structure by value.
        pub static ref ByVal: AttributeKind = AttributeKind::get("byval").unwrap();

        /// Marks function as being in a cold path.
        pub static ref Cold: AttributeKind = AttributeKind::get("cold").unwrap();

        /// Can only be moved to control-equivalent blocks.
        pub static ref Convergent: AttributeKind = AttributeKind::get("convergent").unwrap();

        /// Pointer is known to be dereferenceable.
        pub static ref Dereferenceable: AttributeKind = AttributeKind::get("dereferenceable").unwrap();

        /// Pointer is either null or dereferenceable.
        pub static ref DereferenceableOrNull: AttributeKind = AttributeKind::get("dereferenceable_or_null").unwrap();

        /// Function may only access memory that is inaccessible from IR.
        pub static ref InaccessibleMemOnly: AttributeKind = AttributeKind::get("inaccessiblememonly").unwrap();

        /// Function may only access memory that is either inaccessible from the IR,
        /// or pointed to by its pointer arguments.
        pub static ref InaccessibleMemOrArgMemOnly: AttributeKind = AttributeKind::get("inaccessiblemem_or_argmemonly").unwrap();

        /// Pass structure in an alloca.
        pub static ref InAlloca: AttributeKind = AttributeKind::get("inalloca").unwrap();

        /// Source said inlining was desirable.
        pub static ref InlineHint: AttributeKind = AttributeKind::get("inlinehint").unwrap();

        /// Force argument to be passed in register.
        pub static ref InReg: AttributeKind = AttributeKind::get("inreg").unwrap();

        /// Build jump-instruction tables and replace refs.
        pub static ref JumpTable: AttributeKind = AttributeKind::get("jumptable").unwrap();

        /// Function must be optimized for size first.
        pub static ref MinSize: AttributeKind = AttributeKind::get("minsize").unwrap();

        /// Naked function.
        pub static ref Naked: AttributeKind = AttributeKind::get("naked").unwrap();

        /// Nested function static chain.
        pub static ref Nest: AttributeKind = AttributeKind::get("nest").unwrap();

        /// Considered to not alias after call.
        pub static ref NoAlias: AttributeKind = AttributeKind::get("noalias").unwrap();

        /// Callee isn't recognized as a builtin.
        pub static ref NoBuiltin: AttributeKind = AttributeKind::get("nobuiltin").unwrap();

        /// Function creates no aliases of pointer.
        pub static ref NoCapture: AttributeKind = AttributeKind::get("nocapture").unwrap();

        /// Call cannot be duplicated.
        pub static ref NoDuplicate: AttributeKind = AttributeKind::get("noduplicate").unwrap();

        /// Disable implicit floating point insts.
        pub static ref NoImplicitFloat: AttributeKind = AttributeKind::get("noimplicitfloat").unwrap();

        /// inline=never.
        pub static ref NoInline: AttributeKind = AttributeKind::get("noinline").unwrap();

        /// Function is called early and/or often, so lazy binding isn't worthwhile.
        pub static ref NonLazyBind: AttributeKind = AttributeKind::get("nonlazybind").unwrap();

        /// Pointer is known to be not null.
        pub static ref NonNull: AttributeKind = AttributeKind::get("nonnull").unwrap();

        /// The function does not recurse.
        pub static ref NoRecurse: AttributeKind = AttributeKind::get("norecurse").unwrap();

        /// Disable redzone.
        pub static ref NoRedZone: AttributeKind = AttributeKind::get("noredzone").unwrap();

        /// Mark the function as not returning.
        pub static ref NoReturn: AttributeKind = AttributeKind::get("noreturn").unwrap();

        /// Function doesn't unwind stack.
        pub static ref NoUnwind: AttributeKind = AttributeKind::get("nounwind").unwrap();

        /// opt_size.
        pub static ref OptimizeForSize: AttributeKind = AttributeKind::get("optsize").unwrap();

        /// Function must not be optimized.
        pub static ref OptimizeNone: AttributeKind = AttributeKind::get("optnone").unwrap();

        /// Function does not access memory.
        pub static ref ReadNone: AttributeKind = AttributeKind::get("readnone").unwrap();

        /// Function only reads from memory.
        pub static ref ReadOnly: AttributeKind = AttributeKind::get("readonly").unwrap();

        /// Return value is always equal to this argument.
        pub static ref Returned: AttributeKind = AttributeKind::get("returned").unwrap();

        /// Function can return twice.
        pub static ref ReturnsTwice: AttributeKind = AttributeKind::get("returns_twice").unwrap();

        /// Safe Stack protection.
        pub static ref SafeStack: AttributeKind = AttributeKind::get("safestack").unwrap();

        /// Sign extended before/after call.
        pub static ref SExt: AttributeKind = AttributeKind::get("signext").unwrap();

        /// Alignment of stack for function (3 bits)  stored as log2 of alignment with
        /// +1 bias 0 means unaligned (different from alignstack=(1)).
        pub static ref StackAlignment: AttributeKind = AttributeKind::get("alignstack").unwrap();

        /// Function can be speculated.
        pub static ref Speculatable: AttributeKind = AttributeKind::get("speculatable").unwrap();

        /// Stack protection.
        pub static ref StackProtect: AttributeKind = AttributeKind::get("ssp").unwrap();

        /// Stack protection required.
        pub static ref StackProtectReq: AttributeKind = AttributeKind::get("sspreq").unwrap();

        /// Strong Stack protection.
        pub static ref StackProtectStrong: AttributeKind = AttributeKind::get("sspstrong").unwrap();

        /// Hidden pointer to structure to return.
        pub static ref StructRet: AttributeKind = AttributeKind::get("sret").unwrap();

        /// AddressSanitizer is on.
        pub static ref SanitizeAddress: AttributeKind = AttributeKind::get("sanitize_address").unwrap();

        /// ThreadSanitizer is on.
        pub static ref SanitizeThread: AttributeKind = AttributeKind::get("sanitize_thread").unwrap();

        /// MemorySanitizer is on.
        pub static ref SanitizeMemory: AttributeKind = AttributeKind::get("sanitize_memory").unwrap();

        /// Argument is swift error.
        pub static ref SwiftError: AttributeKind = AttributeKind::get("swifterror").unwrap();

        /// Argument is swift self/context.
        pub static ref SwiftSelf: AttributeKind = AttributeKind::get("swiftself").unwrap();

        /// Function must be in a unwind table.
        pub static ref UWTable: AttributeKind = AttributeKind::get("uwtable").unwrap();

        /// Function only writes to memory.
        pub static ref WriteOnly: AttributeKind = AttributeKind::get("writeonly").unwrap();

        /// Zero extended before/after call.
        pub static ref ZExt: AttributeKind = AttributeKind::get("zeroext").unwrap();
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AttributeKind(u32);

inherit_from!(AttributeKind, u32);

impl AttributeKind {
    /// Return an unique id given the name of a enum attribute.
    pub fn get<S: AsRef<str>>(name: S) -> Option<Self> {
        let name = name.as_ref();

        let kind = unsafe { LLVMGetEnumAttributeKindForName(cstr!(name), name.len()) };

        (kind != 0).as_some(kind.into())
    }

    pub fn last() -> Self {
        unsafe { LLVMGetLastEnumAttributeKind() }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Attribute(LLVMAttributeRef);

inherit_from!(Attribute, LLVMAttributeRef);

impl Attribute {
    pub fn is_enum_attribute(&self) -> bool {
        unsafe { LLVMIsEnumAttribute(self.as_raw()).as_bool() }
    }

    pub fn is_string_attribute(&self) -> bool {
        unsafe { LLVMIsStringAttribute(self.as_raw()).as_bool() }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnumAttribute(Attribute);

inherit_from!(EnumAttribute, Attribute, LLVMAttributeRef);

impl EnumAttribute {
    /// Get the unique id corresponding to the enum attribute passed as argument.
    pub fn kind(&self) -> AttributeKind {
        unsafe { LLVMGetEnumAttributeKind(self.as_raw()) }.into()
    }

    /// Get the enum attribute's value.
    pub fn value(&self) -> u64 {
        unsafe { LLVMGetEnumAttributeValue(self.as_raw()) }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StringAttribute(Attribute);

inherit_from!(StringAttribute, Attribute, LLVMAttributeRef);

impl StringAttribute {
    /// Get the string attribute's kind.
    pub fn kind(&self) -> Cow<str> {
        let mut len = 0;
        let p = unsafe { LLVMGetStringAttributeKind(self.as_raw(), &mut len) };

        from_unchecked_cstr(p as *const u8, len as usize)
    }

    /// Get the string attribute's value.
    pub fn value(&self) -> Cow<str> {
        let mut len = 0;
        let p = unsafe { LLVMGetStringAttributeValue(self.as_raw(), &mut len) };

        from_unchecked_cstr(p as *const u8, len as usize)
    }
}

pub trait AttributeGroups {
    /// adds the attribute to the list of attributes.
    fn add_attribute<A: Into<Attribute>>(&self, attr: A);

    /// get the attribute from the list of attributes.
    fn get_attributes(&self) -> Vec<Attribute>;

    /// get the attribute from the list of attributes.
    fn get_enum_attribute(&self, kind: AttributeKind) -> EnumAttribute;

    /// get the attribute from the list of attributes.
    fn get_string_attribute(&self, kind: &str) -> StringAttribute;

    /// removes the attribute from the list of attributes.
    fn remove_enum_attribute(&self, kind: AttributeKind);

    /// removes the attribute from the list of attributes.
    fn remove_string_attribute(&self, kind: &str);
}

impl Context {
    /// Create an enum attribute.
    pub fn create_enum_attribute(&self, kind: AttributeKind, value: u64) -> EnumAttribute {
        unsafe { LLVMCreateEnumAttribute(self.as_raw(), kind.as_raw(), value) }.into()
    }

    /// Create a string attribute.
    pub fn create_string_attribute(&self, kind: &str, value: &str) -> StringAttribute {
        unsafe {
            LLVMCreateStringAttribute(
                self.as_raw(),
                kind.as_ptr() as *const i8,
                kind.len() as u32,
                value.as_ptr() as *const i8,
                value.len() as u32,
            )
        }
        .into()
    }
}

impl Function {
    /// Add a target-dependent attribute to a function
    pub fn add_target_dependent_attribute<K, V>(&self, kind: K, value: V)
    where
        K: AsRef<str>,
        V: AsRef<str>,
    {
        unsafe { LLVMAddTargetDependentFunctionAttr(self.as_raw(), cstr!(kind), cstr!(value)) }
    }
}

impl AttributeGroups for Function {
    fn add_attribute<A: Into<Attribute>>(&self, attr: A) {
        unsafe { LLVMAddAttributeAtIndex(self.as_raw(), LLVMAttributeFunctionIndex, attr.into().as_raw()) }
    }

    fn get_attributes(&self) -> Vec<Attribute> {
        let count = unsafe { LLVMGetAttributeCountAtIndex(self.as_raw(), LLVMAttributeFunctionIndex) };
        let mut attrs: Vec<LLVMAttributeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetAttributesAtIndex(self.as_raw(), LLVMAttributeFunctionIndex, attrs.as_mut_ptr()) };

        attrs.into_iter().map(|attr| attr.into()).collect()
    }

    fn get_enum_attribute(&self, kind: AttributeKind) -> EnumAttribute {
        unsafe { LLVMGetEnumAttributeAtIndex(self.as_raw(), LLVMAttributeFunctionIndex, kind.as_raw()) }.into()
    }

    fn get_string_attribute(&self, kind: &str) -> StringAttribute {
        unsafe {
            LLVMGetStringAttributeAtIndex(
                self.as_raw(),
                LLVMAttributeFunctionIndex,
                kind.as_ptr() as *const i8,
                kind.len() as u32,
            )
        }
        .into()
    }

    fn remove_enum_attribute(&self, kind: AttributeKind) {
        unsafe { LLVMRemoveEnumAttributeAtIndex(self.as_raw(), LLVMAttributeFunctionIndex, kind.as_raw()) }
    }

    fn remove_string_attribute(&self, kind: &str) {
        unsafe {
            LLVMRemoveStringAttributeAtIndex(
                self.as_raw(),
                LLVMAttributeFunctionIndex,
                kind.as_ptr() as *const i8,
                kind.len() as u32,
            )
        }
    }
}

impl<T: CallSite> AttributeGroups for T {
    fn add_attribute<A: Into<Attribute>>(&self, attr: A) {
        unsafe { LLVMAddCallSiteAttribute(self.as_raw(), LLVMAttributeReturnIndex, attr.into().as_raw()) }
    }

    fn get_attributes(&self) -> Vec<Attribute> {
        let count = unsafe { LLVMGetCallSiteAttributeCount(self.as_raw(), LLVMAttributeReturnIndex) };
        let mut attrs: Vec<LLVMAttributeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetCallSiteAttributes(self.as_raw(), LLVMAttributeReturnIndex, attrs.as_mut_ptr()) };

        attrs.into_iter().map(|attr| attr.into()).collect()
    }

    fn get_enum_attribute(&self, kind: AttributeKind) -> EnumAttribute {
        unsafe { LLVMGetCallSiteEnumAttribute(self.as_raw(), LLVMAttributeReturnIndex, kind.as_raw()) }.into()
    }

    fn get_string_attribute(&self, kind: &str) -> StringAttribute {
        unsafe {
            LLVMGetCallSiteStringAttribute(
                self.as_raw(),
                LLVMAttributeReturnIndex,
                kind.as_ptr() as *const i8,
                kind.len() as u32,
            )
        }
        .into()
    }

    fn remove_enum_attribute(&self, kind: AttributeKind) {
        unsafe { LLVMRemoveCallSiteEnumAttribute(self.as_raw(), LLVMAttributeReturnIndex, kind.as_raw()) }
    }

    fn remove_string_attribute(&self, kind: &str) {
        unsafe {
            LLVMRemoveCallSiteStringAttribute(
                self.as_raw(),
                LLVMAttributeReturnIndex,
                kind.as_ptr() as *const i8,
                kind.len() as u32,
            )
        }
    }
}
