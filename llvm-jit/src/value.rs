use std::fmt;
use std::mem;
use std::slice;
use std::str;

use boolinator::Boolinator;
use libc::c_char;

use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use block::BasicBlock;
use constant::Constant;
use types::TypeRef;
use utils::{AsBool, AsRaw, AsResult, DisposableMessage};

#[macro_export]
macro_rules! values {
    ($($x:expr),*) => (&[ $($crate::ValueRef::from($x)),* ]);
    ($($x:expr,)*) => (&[ $($crate::ValueRef::from($x)),* ]);
}

/// Represents an individual value in LLVM IR.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

unsafe impl Send for ValueRef {}
unsafe impl Sync for ValueRef {}

inherit_from!(ValueRef, LLVMValueRef);

macro_rules! inherit_value_ref {
    ($ty: ident) => {
        inherit_from!($ty, ValueRef, LLVMValueRef);
    };
    ($ty: ident, $parent: ty) => {
        inherit_from!($ty, $parent, ValueRef, LLVMValueRef);
    };
}

pub trait AsValueRef: AsRaw<RawType = LLVMValueRef> {}

impl<T> AsValueRef for T where T: AsRaw<RawType = LLVMValueRef> {}

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
    pub fn name(&self) -> Option<&str> {
        let mut len = 0;
        let ptr = unsafe { LLVMGetValueName2(self.0, &mut len) };

        if ptr.is_null() {
            None
        } else {
            Some(unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr as *const u8, len)) })
        }
    }

    /// Set the string name of a value.
    pub fn set_name<S: AsRef<str>>(&self, name: S) -> &Self {
        let s = name.as_ref();

        unsafe { LLVMSetValueName2(self.0, s.as_ptr() as *const c_char, s.len()) };

        self
    }

    /// Determine whether the specified value instance is constant.
    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) }.as_bool()
    }

    pub fn as_constant(&self) -> Option<Constant> {
        self.is_constant().as_some(self.as_raw().into())
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
        unsafe { LLVMValueAsBasicBlock(self.0) }.ok()
    }

    /// Returns an iterator over the use of a value.
    pub fn uses(&self) -> Uses {
        Uses(unsafe { LLVMGetFirstUse(self.0) })
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", unsafe { LLVMPrintValueToString(self.0) }.into_string())
    }
}

pub struct Use(LLVMUseRef);

impl Use {
    /// Obtain the user value for a user.
    pub fn user(&self) -> ValueRef {
        unsafe { LLVMGetUser(self.0) }.into()
    }

    /// Obtain the value this use corresponds to.
    pub fn value(&self) -> ValueRef {
        unsafe { LLVMGetUsedValue(self.0) }.into()
    }
}

pub struct Uses(LLVMUseRef);

impl Iterator for Uses {
    type Item = Use;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            None
        } else {
            let next = unsafe { LLVMGetNextUse(self.0) };

            Some(Use(mem::replace(&mut self.0, next)))
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

    /// Obtain the instruction that occurs after the one specified.
    pub fn next(&self) -> Option<Instruction> {
        unsafe { LLVMGetNextInstruction(self.as_raw()) }.ok()
    }

    /// Obtain the instruction that occurred before this one.
    pub fn previous(&self) -> Option<Instruction> {
        unsafe { LLVMGetPreviousInstruction(self.as_raw()) }.ok()
    }
}

#[cfg(test)]
mod tests {
    use constant::{ConstantInts, ToConstantStruct};
    use context::Context;
    use types::IntegerTypes;

    #[test]
    fn uses() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let n = i64_t.int(123);
        let v = c.struct_of(values![n], true);

        let mut uses = n.uses();
        let u = uses.next().unwrap();

        assert_eq!(u.user(), v.into());
        assert_eq!(u.value(), n.into());

        assert!(uses.next().is_none());
    }
}
