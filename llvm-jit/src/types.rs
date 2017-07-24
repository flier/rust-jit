use std::ptr;
use std::fmt;
use std::ffi::CStr;

use llvm::*;
use llvm::prelude::*;
use llvm::core::*;

use context::Context;
use block::BasicBlock;
use utils::unchecked_cstring;

#[derive(Clone, Copy, PartialEq)]
pub struct TypeRef(LLVMTypeRef);

#[derive(Clone, Copy, PartialEq)]
pub struct ValueRef(LLVMValueRef);

impl ValueRef {
    /// Wrap a raw value reference.
    pub fn from_raw(v: LLVMValueRef) -> Self {
        ValueRef(v)
    }

    /// Extracts the raw value reference.
    pub fn as_raw(&self) -> LLVMValueRef {
        self.0
    }
}

pub type TypeKind = LLVMTypeKind;

impl TypeRef {
    /// Extracts the raw typedef reference.
    pub fn as_raw(&self) -> LLVMTypeRef {
        self.0
    }

    /// Obtain the enumerated type of a Type instance.
    pub fn kind(&self) -> TypeKind {
        unsafe { LLVMGetTypeKind(self.0) }
    }

    /// Whether the type has a known size.
    pub fn is_sized(&self) -> bool {
        unsafe { LLVMTypeIsSized(self.0) != 0 }
    }

    /// Obtain the context to which this type instance is associated.
    pub fn context(&self) -> Context {
        Context::from_raw(unsafe { LLVMGetTypeContext(self.0) })
    }
}

impl fmt::Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let s = LLVMPrintTypeToString(self.0);

            let r = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());

            LLVMDisposeMessage(s);

            r
        }
    }
}

pub type IntegerType = TypeRef;

impl IntegerType {
    pub fn width(&self) -> u32 {
        unsafe { LLVMGetIntTypeWidth(self.0) }
    }
}

/// Obtain an integer type from a context with specified bit width.
pub trait IntegerTypes {
    fn int1(&self) -> IntegerType;

    fn int8(&self) -> IntegerType;

    fn int16(&self) -> IntegerType;

    fn int32(&self) -> IntegerType;

    fn int64(&self) -> IntegerType;

    fn int128(&self) -> IntegerType;

    fn int_type(&self, bits: u32) -> IntegerType;
}

impl IntegerTypes for Context {
    fn int1(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt1TypeInContext(self.as_raw()) })
    }

    fn int8(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt8TypeInContext(self.as_raw()) })
    }

    fn int16(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt16TypeInContext(self.as_raw()) })
    }

    fn int32(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt32TypeInContext(self.as_raw()) })
    }

    fn int64(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt64TypeInContext(self.as_raw()) })
    }

    fn int128(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt128TypeInContext(self.as_raw()) })
    }

    fn int_type(&self, bits: u32) -> IntegerType {
        TypeRef(unsafe { LLVMIntTypeInContext(self.as_raw(), bits) })
    }
}

pub fn int1() -> IntegerType {
    TypeRef(unsafe { LLVMInt1Type() })
}

pub fn int8() -> IntegerType {
    TypeRef(unsafe { LLVMInt8Type() })
}

pub fn int16() -> IntegerType {
    TypeRef(unsafe { LLVMInt16Type() })
}

pub fn int32() -> IntegerType {
    TypeRef(unsafe { LLVMInt32Type() })
}

pub fn int64() -> IntegerType {
    TypeRef(unsafe { LLVMInt64Type() })
}

pub fn int128() -> IntegerType {
    TypeRef(unsafe { LLVMInt128Type() })
}

pub fn int_type(bits: u32) -> IntegerType {
    TypeRef(unsafe { LLVMIntType(bits) })
}

pub type FunctionType = TypeRef;

impl FunctionType {
    /// Obtain a function type consisting of a specified signature.
    pub fn new(return_type: TypeRef, params_type: &[TypeRef], var_arg: bool) -> Self {
        let mut params = params_type
            .iter()
            .map(|t| t.0)
            .collect::<Vec<LLVMTypeRef>>();

        let function = unsafe {
            LLVMFunctionType(
                return_type.0,
                params.as_mut_ptr(),
                params.len() as u32,
                var_arg as i32,
            )
        };

        trace!(
            "create function ({}) -> {} #{:?}",
            params_type.iter().fold(
                "".to_owned(),
                |s, t| if s.is_empty() {
                    t.to_string()
                } else {
                    format!("{}, {}", s, t)
                },
            ),
            return_type,
            function
        );

        TypeRef(function)
    }

    /// Returns whether a function type is variadic.
    pub fn is_var_arg(&self) -> bool {
        unsafe { LLVMIsFunctionVarArg(self.0) != 0 }
    }

    /// Obtain the Type this function Type returns.
    pub fn return_type(&self) -> TypeRef {
        TypeRef(unsafe { LLVMGetReturnType(self.0) })
    }

    /// Obtain the types of a function's parameters.
    pub fn params_type(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMCountParamTypes(self.0) };
        let mut params: Vec<LLVMTypeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetParamTypes(self.0, params.as_mut_ptr()) };

        params.into_iter().map(|t| TypeRef(t)).collect()
    }
}

pub type Function = ValueRef;

impl Function {
    pub fn append_basic_block<S: AsRef<str>>(&self, context: &Context, name: S) -> BasicBlock {
        let cname = unchecked_cstring(name);
        let block =
            unsafe { LLVMAppendBasicBlockInContext(context.as_raw(), self.0, cname.as_ptr()) };

        trace!(
            "create `{}` basic block@{:?} in {}",
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
