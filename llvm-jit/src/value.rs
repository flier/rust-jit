use std::borrow::Cow;
use std::fmt;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use constant::Constant;
use types::TypeRef;
use utils::{AsBool, AsRaw, DisposableMessage, FromRaw, UncheckedCStr};

#[macro_export]
macro_rules! values {
    ($($x:expr),*) => (&[ $($x.into()),* ]);
    ($($x:expr,)*) => (&[ $($x.into()),* ]);
}

/// Represents an individual value in LLVM IR.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

unsafe impl Send for ValueRef {}
unsafe impl Sync for ValueRef {}

inherit_from!(ValueRef, LLVMValueRef);

macro_rules! inherit_value_ref {
    ($ty:ident) => {
        inherit_from!($ty, ValueRef, LLVMValueRef);
    };
    ($ty:ident, $parent:ty) => (
        inherit_from!($ty, $parent, ValueRef, LLVMValueRef);
    );
}

pub trait AsValueRef: AsRaw<RawType = LLVMValueRef> {}

impl<T> AsValueRef for T
where
    T: AsRaw<RawType = LLVMValueRef>,
{
}

pub type ValueKind = LLVMValueKind;
pub type Opcode = LLVMOpcode;

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

    pub fn opcode(&self) -> Opcode {
        unsafe { LLVMGetConstOpcode(self.as_raw()) }
    }

    /// Obtain the string name of a value.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe { LLVMGetValueName(self.0).as_ref() }.map(|name| name.as_str())
    }

    /// Set the string name of a value.
    pub fn set_name<S: AsRef<str>>(&self, name: S) -> &Self {
        unsafe { LLVMSetValueName(self.0, cstr!(name.as_ref())) };

        self
    }

    /// Determine whether the specified value instance is constant.
    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) }.as_bool()
    }

    pub fn as_constant(&self) -> Option<Constant> {
        if self.is_constant() {
            Some(self.as_raw().into())
        } else {
            None
        }
    }

    /// Determine whether a value instance is undefined.
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.0) }.as_bool()
    }

    /// Determine whether a value instance is null.
    pub fn is_null(&self) -> bool {
        unsafe { LLVMIsNull(self.0) }.as_bool()
    }

    /// Determine whether a `ValueRef` is itself a basic block.
    pub fn is_basic_block(&self) -> bool {
        unsafe { LLVMValueIsBasicBlock(self.0) }.as_bool()
    }

    /// Convert a `ValueRef` to an `BasicBlock` instance.
    pub fn as_basic_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMValueAsBasicBlock(self.0) }.wrap()
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            unsafe { LLVMPrintValueToString(self.0) }.into_string()
        )
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
    pub fn set_metadata(&self, kind_id: u32, node: ValueRef) -> &Self {
        unsafe { LLVMSetMetadata(self.as_raw(), kind_id, node.as_raw()) };
        self
    }

    /// Obtain the instruction that occurs after the one specified.
    pub fn next(&self) -> Option<Instruction> {
        unsafe { LLVMGetNextInstruction(self.as_raw()) }.wrap()
    }

    /// Obtain the instruction that occurred before this one.
    pub fn previous(&self) -> Option<Instruction> {
        unsafe { LLVMGetPreviousInstruction(self.as_raw()) }.wrap()
    }
}
