use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use types::TypeRef;
use utils::AsBool;

/// Structure to represent function types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionType(TypeRef);

inherit_from!(FunctionType, TypeRef, LLVMTypeRef);

impl FunctionType {
    /// Obtain a function type consisting of a specified signature.
    pub fn new(return_type: TypeRef, params_type: &[TypeRef], var_arg: bool) -> Self {
        let mut params = params_type
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

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
            params_type.iter().fold(
                "".to_owned(),
                |s, t| if s.is_empty() {
                    t.to_string()
                } else {
                    format!("{}, {}", s, t)
                },
            ),
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

        params.into_iter().map(TypeRef::from_raw).collect()
    }
}

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::{Context, GlobalContext};

    #[test]
    fn function() {
        let i64_t = GlobalContext::int64_t();
        let argts = [i64_t, i64_t, i64_t];
        let t = FunctionType::new(i64_t, &argts, false);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMFunctionTypeKind));
        assert!(!t.is_var_arg());
        assert_eq!(t.return_type(), i64_t);
        assert_eq!(t.param_types(), argts);
    }
}
