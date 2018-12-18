use std::ptr;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use boolinator::Boolinator;

use crate::block::BasicBlock;
use crate::context::Context;
use crate::global::GlobalValue;
use crate::module::Module;
use crate::types::TypeRef;
use crate::utils::{AsBool, AsRaw, AsResult};
use crate::value::ValueRef;

#[macro_export]
macro_rules! func {
    ( || -> $ret:ident ) => {
        $crate::FunctionType::new($ret, &[], false)
    };
    ( | $( $arg:ident ),* | -> $ret:ident ) => {
        $crate::FunctionType::new($ret.into(), &[$( $arg.into() ),*], false)
    };
    ( | ... | -> $ret:ident ) => {
        $crate::FunctionType::new($ret.into(), &[], true)
    };
    ( | $( $arg:ident ),* , ... | -> $ret:ident ) => {
        $crate::FunctionType::new($ret.into(), &[$( $arg.into() ),*], true)
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

        let function: FunctionType = unsafe {
            LLVMFunctionType(
                return_type.as_raw(),
                params.as_mut_ptr(),
                params.len() as u32,
                var_arg as i32,
            )
        }
        .into();

        info!("create {:?} with signature: {}", function, *function,);

        function
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

impl GlobalValue for Function {}

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
        unsafe { LLVMGetEntryBasicBlock(self.as_raw()) }.ok()
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

        (index < count).and_option_from(|| unsafe { LLVMGetParam(self.as_raw(), index) }.ok())
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

impl Module {
    /// Look up the specified function in the module symbol table.
    ///
    /// If it does not exist, add a prototype for the function and return it.
    /// This is nice because it allows most passes to get away with not handling
    /// the symbol table directly for this common task.
    pub fn get_or_insert_function<S: AsRef<str>, T: Into<TypeRef>>(
        &self,
        name: S,
        return_type: T,
        params_type: &[TypeRef],
    ) -> Function {
        self.get_function(name.as_ref())
            .unwrap_or_else(|| self.add_function(name, FunctionType::new(return_type.into(), params_type, false)))
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func_type: FunctionType) -> Function {
        let name = name.as_ref();
        let func = unsafe { LLVMAddFunction(self.as_raw(), cstr!(name), func_type.as_raw()) };

        trace!(
            "add function `{}`: {:?} to {:?}: Function({:?})",
            name,
            func_type,
            self,
            func,
        );

        func.into()
    }

    /// Obtain a Function value from a Module by its name.
    pub fn get_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let name = name.as_ref();

        unsafe { LLVMGetNamedFunction(self.as_raw(), cstr!(name)) }.ok()
    }

    /// Obtain an iterator to the Function in a module.
    pub fn functions(&self) -> FuncIter {
        FuncIter::new(self.as_raw())
    }
}

impl_iter!(
    FuncIter,
    LLVMGetFirstFunction | LLVMGetLastFunction[LLVMModuleRef],
    LLVMGetNextFunction | LLVMGetPreviousFunction[LLVMValueRef],
    Function
);

#[cfg(test)]
mod tests {
    use crate::llvm;

    use super::*;
    use crate::context::Context;
    use crate::prelude::*;

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

    #[test]
    fn module() {
        let context = Context::new();

        let m = context.create_module("test");
        assert!(!m.as_raw().is_null());

        let foo = m.add_function("foo", FunctionType::new(context.void_t(), &[], false));
        assert!(!foo.as_raw().is_null());
        assert_eq!(m.get_function("foo"), Some(foo));
        assert!(foo.verify().is_ok());

        let bar = m.get_or_insert_function("bar", context.int8_t(), &[context.int16_t()]);
        assert!(!bar.as_raw().is_null());
        assert_eq!(m.get_function("bar"), Some(bar));
        assert!(bar.verify().is_ok());

        assert_eq!(m.functions().collect::<Vec<_>>(), vec![foo, bar]);

        assert_eq!(
            m.to_string(),
            r#"; ModuleID = 'test'
source_filename = "test"

declare void @foo()

declare i8 @bar(i16)
"#
        );

        m.verify().unwrap();
    }
}
