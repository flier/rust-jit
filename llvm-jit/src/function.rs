use std::ptr;

use boolinator::Boolinator;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::TypeRef;
use utils::{AsBool, AsRaw, FromRaw};
use value::ValueRef;

#[macro_export]
macro_rules! func {
    ( || -> $ret:ident ) => {
        $crate::FunctionType::new($ret, &[], false)
    };
    ( | $( $arg:ident ),* | -> $ret:ident ) => {
        $crate::FunctionType::new($ret, &[$( $arg ),*], false)
    };
    ( | ... | -> $ret:ident ) => {
        $crate::FunctionType::new($ret, &[], true)
    };
    ( | $( $arg:ident ),* , ... | -> $ret:ident ) => {
        $crate::FunctionType::new($ret, &[$( $arg ),*], true)
    };
}

/// Structure to represent function types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionType(TypeRef);

inherit_from!(FunctionType, TypeRef, LLVMTypeRef);

impl FunctionType {
    /// Obtain a function type consisting of a specified signature.
    pub fn new(return_type: TypeRef, params_type: &[TypeRef], var_arg: bool) -> Self {
        let mut params = params_type.iter().map(|t| t.as_raw()).collect::<Vec<LLVMTypeRef>>();

        let function = unsafe {
            LLVMFunctionType(
                return_type.as_raw(),
                params.as_mut_ptr(),
                params.len() as u32,
                var_arg as i32,
            )
        };

        trace!(
            "create Function({:?}): ({}) -> {}",
            function,
            params_type.iter().fold("".to_owned(), |s, t| if s.is_empty() {
                t.to_string()
            } else {
                format!("{}, {}", s, t)
            },),
            return_type,
        );

        function.into()
    }

    /// Returns whether a function type is variadic.
    pub fn is_var_arg(&self) -> bool {
        unsafe { LLVMIsFunctionVarArg(self.as_raw()) }.as_bool()
    }

    /// Obtain the Type this function Type returns.
    pub fn return_type(&self) -> TypeRef {
        unsafe { LLVMGetReturnType(self.as_raw()) }.into()
    }

    /// Obtain the types of a function's parameters.
    pub fn param_types(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMCountParamTypes(self.as_raw()) };
        let mut params: Vec<LLVMTypeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetParamTypes(self.as_raw(), params.as_mut_ptr()) };

        params.into_iter().map(|p| p.into()).collect()
    }
}

/// Functions in this group operate on `ValueRef` instances that correspond to `Function` instances.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Function(ValueRef);

inherit_from!(Function, ValueRef, LLVMValueRef);

impl Function {
    pub fn is_empty(&self) -> bool {
        self.basic_block_count() == 0
    }

    /// Obtain the count of basic blocks in a function.
    pub fn basic_block_count(&self) -> usize {
        unsafe { LLVMCountBasicBlocks(self.as_raw()) as usize }
    }

    /// Obtain an iterator to the basic blocks in a function.
    pub fn basic_blocks(&self) -> BasicBlockIter {
        BasicBlockIter::new(self.as_raw())
    }

    /// Obtain all of the basic blocks in a function.
    pub fn get_basic_blocks(&self) -> Vec<BasicBlock> {
        let count = unsafe { LLVMCountBasicBlocks(self.as_raw()) };
        let mut blocks: Vec<LLVMBasicBlockRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetBasicBlocks(self.as_raw(), blocks.as_mut_ptr()) };

        blocks.into_iter().map(|bb| bb.into()).collect()
    }

    /// Obtain the basic block that corresponds to the entry point of a function.
    pub fn entry(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetEntryBasicBlock(self.as_raw()) }.wrap()
    }

    /// Append a basic block to the end of a function using the global context.
    pub fn append_basic_block<S: AsRef<str>>(&self, name: S) -> BasicBlock {
        let name = name.as_ref();
        let block = unsafe { LLVMAppendBasicBlock(self.as_raw(), cstr!(name)) }.into();

        trace!("{:?} create `{}` block in the global context: {:?}", self, name, block);

        block
    }

    /// Append a basic block to the end of a function.
    pub fn append_basic_block_in_context<S: AsRef<str>>(&self, name: S, context: &Context) -> BasicBlock {
        let name = name.as_ref();
        let block = unsafe { LLVMAppendBasicBlockInContext(context.as_raw(), self.as_raw(), cstr!(name)) }.into();

        trace!("{:?} create `{}` block in {:?}: {:?}", self, name, context, block,);

        block
    }

    /// Obtain the count of parameters
    pub fn param_count(&self) -> usize {
        unsafe { LLVMCountParams(self.as_raw()) as usize }
    }

    /// Obtain an iterator to the parameters in a function.
    pub fn params(&self) -> ParamIter {
        ParamIter::new(self.as_raw())
    }

    /// Obtain the parameters in a function.
    pub fn get_params(&self) -> Vec<ValueRef> {
        let count = unsafe { LLVMCountParams(self.as_raw()) };
        let mut params: Vec<LLVMValueRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetParams(self.as_raw(), params.as_mut_ptr()) };

        params.into_iter().map(|p| p.into()).collect()
    }

    /// Obtain the parameter at the specified index.
    pub fn get_param(&self, index: u32) -> Option<ValueRef> {
        let count = unsafe { LLVMCountParams(self.as_raw()) };

        (index < count).and_option_from(|| unsafe { LLVMGetParam(self.as_raw(), index) }.wrap())
    }

    /// Remove a function from its containing module and deletes it.
    pub fn delete(self) {
        unsafe { LLVMDeleteFunction(self.as_raw()) }
    }

    /// Check whether the given function has a personality function.
    pub fn has_personality_function(&self) -> bool {
        unsafe { LLVMHasPersonalityFn(self.as_raw()).as_bool() }
    }

    /// Obtain the personality function attached to the function.
    pub fn get_personality_function(&self) -> Function {
        unsafe { LLVMGetPersonalityFn(self.as_raw()) }.into()
    }

    /// Set the personality function attached to the function.
    pub fn set_personality_function(&self, func: Function) {
        unsafe { LLVMSetPersonalityFn(self.as_raw(), func.as_raw()) }
    }
}

impl_iter!(
    BasicBlockIter,
    LLVMGetFirstBasicBlock | LLVMGetLastBasicBlock[LLVMValueRef],
    LLVMGetNextBasicBlock | LLVMGetPreviousBasicBlock[LLVMBasicBlockRef],
    BasicBlock
);

impl_iter!(
    ParamIter,
    LLVMGetFirstParam | LLVMGetLastParam[LLVMValueRef],
    LLVMGetNextParam | LLVMGetPreviousParam[LLVMValueRef],
    ValueRef
);

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::Context;
    use prelude::*;

    #[test]
    fn function() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let argts = [i64_t, i64_t, i64_t];
        let t = FunctionType::new(i64_t, &argts, false);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMFunctionTypeKind));
        assert!(!t.is_var_arg());
        assert_eq!(t.return_type(), i64_t);
        assert_eq!(t.param_types(), argts);
    }
}
