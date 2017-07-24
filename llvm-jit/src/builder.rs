use std::fmt;

use llvm::prelude::*;
use llvm::core::*;

use context::Context;
use types::ValueRef;
use block::BasicBlock;
use utils::unchecked_cstring;

/// An instruction builder represents a point within a basic block and is the exclusive means of building instructions.
pub struct Builder(LLVMBuilderRef);

pub enum Position {
    AtEnd(BasicBlock),
}

impl Builder {
    pub fn new() -> Self {
        let builder = unsafe { LLVMCreateBuilder() };

        trace!("create builder@{:?} in global context", builder);

        Builder(builder)
    }

    pub fn with_context(context: &Context) -> Self {
        let builder = unsafe { LLVMCreateBuilderInContext(context.as_raw()) };

        trace!("create builder@{:?} in {}", builder, context);

        Builder(builder)
    }

    pub fn position(&self, position: Position) -> &Self {
        unsafe {
            match position {
                Position::AtEnd(block) => {
                    trace!("move {} position at end of {}", self, block);

                    LLVMPositionBuilderAtEnd(self.0, block.as_raw())
                }
            }
        }

        &self
    }

    pub fn add<S: AsRef<str>>(&self, lhs: ValueRef, rhs: ValueRef, name: S) -> ValueRef {
        unsafe {
            let cname = unchecked_cstring(name);

            ValueRef::from_raw(LLVMBuildAdd(
                self.0,
                lhs.as_raw(),
                rhs.as_raw(),
                cname.as_ptr(),
            ))
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        trace!("drop builder@{:?}", self.0);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}

impl fmt::Display for Builder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "builder@{:?}", self.0)
    }
}
