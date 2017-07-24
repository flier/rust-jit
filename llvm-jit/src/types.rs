use std::ffi::CStr;
use std::fmt;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use context::Context;

#[derive(Clone, Copy, Debug)]
pub struct TypeRef(LLVMTypeRef);

pub type TypeKind = LLVMTypeKind;

impl TypeRef {
    /// Wrap a raw typedef reference.
    pub fn from_raw(t: LLVMTypeRef) -> Self {
        TypeRef(t)
    }

    /// Extracts the raw typedef reference.
    pub fn as_raw(&self) -> LLVMTypeRef {
        self.0
    }

    /// Dump a representation of a type to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpType(self.0) }
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
    /// a single-bit integer.
    fn int1(&self) -> IntegerType;

    /// a 8-bit integer.
    fn int8(&self) -> IntegerType;

    /// a 16-bit integer.
    fn int16(&self) -> IntegerType;

    /// a 32-bit integer.
    fn int32(&self) -> IntegerType;

    /// a 64-bit integer.
    fn int64(&self) -> IntegerType;

    /// a 64-bit integer.
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

// Floating Point Types
pub type FloatingPointType = TypeRef;

pub trait FloatingPointTypes {
    /// Obtain a 16-bit floating point type from a context.
    fn half(&self) -> FloatingPointType;

    /// Obtain a 32-bit floating point type from a context.
    fn float(&self) -> FloatingPointType;

    /// Obtain a 64-bit floating point type from a context.
    fn double(&self) -> FloatingPointType;

    /// Obtain a 80-bit floating point type (X87) from a context.
    fn x86_fp80(&self) -> FloatingPointType;

    /// Obtain a 128-bit floating point type (112-bit mantissa) from a context.
    fn fp128(&self) -> FloatingPointType;

    /// Obtain a 128-bit floating point type (two 64-bits) from a context.
    fn ppc_fp128(&self) -> FloatingPointType;
}

impl FloatingPointTypes for Context {
    fn half(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMHalfTypeInContext(self.as_raw()) })
    }

    fn float(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMFloatTypeInContext(self.as_raw()) })
    }

    fn double(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMDoubleTypeInContext(self.as_raw()) })
    }

    fn x86_fp80(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMX86FP80TypeInContext(self.as_raw()) })
    }

    fn fp128(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMFP128TypeInContext(self.as_raw()) })
    }

    fn ppc_fp128(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMPPCFP128TypeInContext(self.as_raw()) })
    }
}

/// Obtain a 16-bit floating point type from the global context.
pub fn half() -> FloatingPointType {
    TypeRef(unsafe { LLVMHalfType() })
}

/// Obtain a 32-bit floating point type from the global context.
pub fn float() -> FloatingPointType {
    TypeRef(unsafe { LLVMFloatType() })
}

/// Obtain a 64-bit floating point type from the global context.
pub fn double() -> FloatingPointType {
    TypeRef(unsafe { LLVMDoubleType() })
}

/// Obtain a 80-bit floating point type (X87) from the global context.
pub fn x86_fp80() -> FloatingPointType {
    TypeRef(unsafe { LLVMX86FP80Type() })
}

/// Obtain a 128-bit floating point type (112-bit mantissa) from the global context.
pub fn fp128() -> FloatingPointType {
    TypeRef(unsafe { LLVMFP128Type() })
}

/// Obtain a 128-bit floating point type (two 64-bits) from the global context.
pub fn ppc_fp128() -> FloatingPointType {
    TypeRef(unsafe { LLVMPPCFP128Type() })
}

// Other Types
pub type OtherType = TypeRef;

pub trait OtherTypes {
    fn void(&self) -> OtherType;

    fn label(&self) -> OtherType;

    fn x86mmx(&self) -> OtherType;
}

impl OtherTypes for Context {
    fn void(&self) -> OtherType {
        TypeRef(unsafe { LLVMVoidTypeInContext(self.as_raw()) })
    }

    fn label(&self) -> OtherType {
        TypeRef(unsafe { LLVMLabelTypeInContext(self.as_raw()) })
    }

    fn x86mmx(&self) -> OtherType {
        TypeRef(unsafe { LLVMX86MMXTypeInContext(self.as_raw()) })
    }
}

pub fn void() -> OtherType {
    TypeRef(unsafe { LLVMVoidType() })
}

pub fn label() -> OtherType {
    TypeRef(unsafe { LLVMLabelType() })
}

pub fn x86mmx() -> OtherType {
    TypeRef(unsafe { LLVMX86MMXType() })
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
