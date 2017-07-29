use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::ops::Deref;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use constant::Constant;
use types::TypeRef;
use utils::{AsBool, AsLLVMBool, unchecked_cstring};

/// Represents an individual value in LLVM IR.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

inherit_from!(ValueRef, LLVMValueRef);

macro_rules! inherit_value_ref {
    ($ty:ident) => {
        inherit_from!($ty, ValueRef, LLVMValueRef);
    }
}

pub trait AsValueRef {
    /// Extracts the raw typedef reference.
    fn as_raw(&self) -> LLVMValueRef;
}

impl<T> AsValueRef for T
where
    T: Deref<Target = ValueRef>,
{
    fn as_raw(&self) -> LLVMValueRef {
        self.deref().as_raw()
    }
}

pub type ValueKind = LLVMValueKind;

impl ValueRef {
    /// Dump a representation of a value to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.0) }
    }

    /// Obtain the type of a value.
    pub fn type_of(&self) -> TypeRef {
        unsafe { LLVMTypeOf(self.0) }.into()
    }

    /// Obtain the enumerated type of a Value instance.
    pub fn kind(&self) -> ValueKind {
        unsafe { LLVMGetValueKind(self.0) }
    }

    /// Obtain the string name of a value.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe {
            LLVMGetValueName(self.0).as_ref().map(|name| {
                CStr::from_ptr(name).to_string_lossy()
            })
        }
    }

    /// Set the string name of a value.
    pub fn set_name<S: AsRef<str>>(&mut self, name: S) {
        let cname = unchecked_cstring(name);

        unsafe { LLVMSetValueName(self.0, cname.as_ptr()) }
    }

    /// Determine whether the specified value instance is constant.
    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) }.as_bool()
    }

    /// Determine whether a value instance is undefined.
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.0) }.as_bool()
    }

    /// Determine whether a value instance is null.
    pub fn is_null(&self) -> bool {
        unsafe { LLVMIsNull(self.0) }.as_bool()
    }

    /// Determine whether an LLVMValueRef is itself a basic block.
    pub fn is_basic_block(&self) -> bool {
        unsafe { LLVMValueIsBasicBlock(self.0) }.as_bool()
    }

    /// Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
    pub fn as_basic_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMValueAsBasicBlock(self.0).as_mut() }.map(|block| BasicBlock::from_raw(block))
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlockAddress(ValueRef);

inherit_value_ref!(BlockAddress);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction(ValueRef);

inherit_value_ref!(Instruction);

impl Instruction {
    /// Obtain the code opcode for an individual instruction.
    pub fn opcode(&self) -> LLVMOpcode {
        unsafe { LLVMGetInstructionOpcode(self.as_raw()) }
    }

    /// Create a copy of 'this' instruction that is identical in all ways except the following:
    ///   * The instruction has no parent
    ///   * The instruction has no name
    pub fn copy(&self) -> Self {
        unsafe { LLVMInstructionClone(self.as_raw()) }.into()
    }

    /// Obtain the basic block to which an instruction belongs.
    pub fn parent(&self) -> BasicBlock {
        unsafe { LLVMGetInstructionParent(self.as_raw()) }.into()
    }

    /// Remove and delete an instruction.
    ///
    /// The instruction specified is removed from its containing building block but is kept alive.
    pub fn remove(&self) {
        unsafe { LLVMInstructionRemoveFromParent(self.as_raw()) }
    }

    /// Remove and delete an instruction.
    ///
    /// The instruction specified is removed from its containing building block and then deleted.
    pub fn erase(self) {
        unsafe { LLVMInstructionEraseFromParent(self.as_raw()) }
    }

    /// Determine whether an instruction has any metadata attached.
    pub fn has_metadata(&self) -> bool {
        unsafe { LLVMHasMetadata(self.as_raw()) }.as_bool()
    }

    /// Return metadata associated with an instruction value.
    pub fn get_metadata(&self, kind_id: u32) -> ValueRef {
        unsafe { LLVMGetMetadata(self.as_raw(), kind_id) }.into()
    }

    /// Set metadata associated with an instruction value.
    pub fn set_metadata(&self, kind_id: u32, node: ValueRef) {
        unsafe { LLVMSetMetadata(self.as_raw(), kind_id, node.as_raw()) }
    }

    /// Obtain the instruction that occurs after the one specified.
    pub fn next(&self) -> Option<Instruction> {
        unsafe { LLVMGetNextInstruction(self.as_raw()).as_mut() }.map(|v| Instruction::from_raw(v))
    }

    /// Obtain the instruction that occurred before this one.
    pub fn previous(&self) -> Option<Instruction> {
        unsafe { LLVMGetPreviousInstruction(self.as_raw()).as_mut() }
            .map(|v| Instruction::from_raw(v))
    }
}

pub type ThreadLocalMode = LLVMThreadLocalMode;

/// Global Variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(ValueRef);

inherit_value_ref!(GlobalVar);

impl GlobalVar {
    pub fn delete(&self) {
        unsafe { LLVMDeleteGlobal(self.as_raw()) }
    }

    pub fn initializer(&self) -> Option<ValueRef> {
        unsafe { LLVMGetInitializer(self.as_raw()).as_mut() }.map(|var| ValueRef::from_raw(var))
    }

    pub fn set_initializer(&self, initializer: Constant) {
        unsafe { LLVMSetInitializer(self.as_raw(), initializer.as_raw()) }
    }

    pub fn is_thread_local(&self) -> bool {
        unsafe { LLVMIsThreadLocal(self.as_raw()) }.as_bool()
    }

    pub fn set_thread_local(&self, is_thread_local: bool) {
        unsafe { LLVMSetThreadLocal(self.as_raw(), is_thread_local.as_bool()) }
    }

    pub fn is_global_constant(&self) -> bool {
        unsafe { LLVMIsGlobalConstant(self.as_raw()) }.as_bool()
    }

    pub fn set_global_constant(&self, is_global_constant: bool) {
        unsafe { LLVMSetGlobalConstant(self.as_raw(), is_global_constant.as_bool()) }
    }

    pub fn thread_local_mode(&self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_raw()) }
    }

    pub fn set_thread_local_mode(&self, mode: ThreadLocalMode) {
        unsafe { LLVMSetThreadLocalMode(self.as_raw(), mode) }
    }

    pub fn is_externally_initialized(&self) -> bool {
        unsafe { LLVMIsExternallyInitialized(self.as_raw()) }.as_bool()
    }

    pub fn set_externally_initialized(&self, is_externally_initialized: bool) {
        unsafe { LLVMSetExternallyInitialized(self.as_raw(), is_externally_initialized.as_bool()) }
    }
}

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::Context;
    use module::Module;
    use prelude::*;
    use types::*;

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = Module::with_name_in_context("test", &c);

        assert_eq!(m.get_global_var("x"), None);

        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        m.add_global_var("x", i64_t);
        m.add_global_var("y", f64_t);

        let x = m.get_global_var("x").unwrap();
        let y = m.get_global_var("y").unwrap();

        assert_eq!(m.global_vars().collect::<Vec<GlobalVar>>(), vec![x, y]);

        assert_eq!(x.name().unwrap(), "x");
        assert_eq!(y.name().unwrap(), "y");

        assert_eq!(x.to_string(), "@x = external global i64");

        // set initializer
        assert_eq!(x.initializer(), None);

        let v = i64_t.uint(123);

        x.set_initializer(v);

        assert_eq!(x.initializer(), Some(v));

        assert_eq!(x.to_string(), "@x = global i64 123");

        // set as global constant
        assert!(!x.is_global_constant());

        x.set_global_constant(true);

        assert_eq!(x.to_string(), "@x = constant i64 123");

        x.set_global_constant(false);

        // set as externally initialized
        assert!(!x.is_externally_initialized());

        x.set_externally_initialized(true);

        assert_eq!(x.to_string(), "@x = externally_initialized global i64 123");

        x.set_externally_initialized(false);

        // set as thread local
        assert!(!x.is_thread_local());

        x.set_thread_local(true);

        assert!(x.is_thread_local());

        assert_eq!(x.to_string(), "@x = thread_local global i64 123");

        x.set_thread_local(false);

        // set thread local mod
        assert!(matches!(
            x.thread_local_mode(),
            llvm::LLVMThreadLocalMode::LLVMNotThreadLocal
        ));
    }
}
