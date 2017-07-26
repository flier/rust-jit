use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::ops::Deref;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use context::Context;
use utils::unchecked_cstring;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeRef(LLVMTypeRef);

impl From<LLVMTypeRef> for TypeRef {
    fn from(v: LLVMTypeRef) -> Self {
        TypeRef(v)
    }
}

macro_rules! inherit_type_ref {
    ($ty:ident) => {
        impl Deref for $ty {
            type Target = TypeRef;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl From<$ty> for TypeRef {
            fn from(f: $ty) -> Self {
                f.0
            }
        }
    }
}

pub type TypeKind = LLVMTypeKind;

pub trait AsTypeRef {
    /// Extracts the raw typedef reference.
    fn as_raw(&self) -> LLVMTypeRef;
}

impl<T> AsTypeRef for T
where
    T: Deref<Target = TypeRef>,
{
    fn as_raw(&self) -> LLVMTypeRef {
        self.deref().as_raw()
    }
}

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

// Floating Point Types
pub type FloatingPointType = TypeRef;

impl FloatingPointType {
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
}

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

// Other Types
pub type OtherType = TypeRef;

impl OtherType {
    /// Create a void type in the global context.
    pub fn void() -> OtherType {
        TypeRef(unsafe { LLVMVoidType() })
    }

    /// Create a label type in the global context.
    pub fn label() -> OtherType {
        TypeRef(unsafe { LLVMLabelType() })
    }

    /// Create a X86 MMX type in the global context.
    pub fn x86_mmx() -> OtherType {
        TypeRef(unsafe { LLVMX86MMXType() })
    }
}

pub trait OtherTypes {
    /// Create a void type in a context.
    fn void(&self) -> OtherType;

    /// Create a label type in a context.
    fn label(&self) -> OtherType;

    /// Create a X86 MMX type in a context.
    fn x86_mmx(&self) -> OtherType;
}

impl OtherTypes for Context {
    fn void(&self) -> OtherType {
        TypeRef(unsafe { LLVMVoidTypeInContext(self.as_raw()) })
    }

    fn label(&self) -> OtherType {
        TypeRef(unsafe { LLVMLabelTypeInContext(self.as_raw()) })
    }

    fn x86_mmx(&self) -> OtherType {
        TypeRef(unsafe { LLVMX86MMXTypeInContext(self.as_raw()) })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionType(TypeRef);

inherit_type_ref!(FunctionType);

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

        FunctionType::from_raw(function)
    }

    fn from_raw(t: LLVMTypeRef) -> Self {
        FunctionType(TypeRef(t))
    }

    /// Returns whether a function type is variadic.
    pub fn is_var_arg(&self) -> bool {
        unsafe { LLVMIsFunctionVarArg(self.as_raw()) != 0 }
    }

    /// Obtain the Type this function Type returns.
    pub fn return_type(&self) -> TypeRef {
        TypeRef(unsafe { LLVMGetReturnType(self.as_raw()) })
    }

    /// Obtain the types of a function's parameters.
    pub fn param_types(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMCountParamTypes(self.as_raw()) };
        let mut params: Vec<LLVMTypeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetParamTypes(self.as_raw(), params.as_mut_ptr()) };

        params.into_iter().map(|t| TypeRef(t)).collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StructType(TypeRef);

inherit_type_ref!(StructType);

impl StructType {
    /// Create a new structure type in the global context.
    pub fn new(elements: &[TypeRef], packed: bool) -> Self {
        let mut elements = elements
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

        let t = unsafe {
            LLVMStructType(
                elements.as_mut_ptr(),
                elements.len() as u32,
                if packed { 1 } else { 0 },
            )
        };

        StructType(TypeRef(t))
    }

    fn from_raw(t: LLVMTypeRef) -> Self {
        StructType(TypeRef(t))
    }

    /// Obtain the name of a structure.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe {
            LLVMGetStructName(self.as_raw()).as_ref().map(|name| {
                CStr::from_ptr(name).to_string_lossy()
            })
        }
    }

    /// Set the contents of a structure type.
    pub fn set_body(&self, elements: &[TypeRef], packed: bool) -> &Self {
        let mut elements = elements
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

        unsafe {
            LLVMStructSetBody(
                self.as_raw(),
                elements.as_mut_ptr(),
                elements.len() as u32,
                if packed { 1 } else { 0 },
            )
        }

        &self
    }

    /// Get the elements within a structure.
    pub fn element_types(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMCountStructElementTypes(self.as_raw()) };
        let mut elements: Vec<LLVMTypeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetStructElementTypes(self.as_raw(), elements.as_mut_ptr()) };

        elements.into_iter().map(|t| TypeRef(t)).collect()
    }

    /// Get the number of elements defined inside the structure.
    pub fn element_count(&self) -> usize {
        unsafe { LLVMCountStructElementTypes(self.as_raw()) as usize }
    }

    /// Get the type of the element at a given index in the structure.
    pub fn element_type(self, index: usize) -> Option<TypeRef> {
        if index >= self.element_count() {
            None
        } else {
            unsafe { LLVMStructGetTypeAtIndex(self.as_raw(), index as u32).as_mut() }
                .map(|t| TypeRef::from_raw(t))
        }
    }

    /// Determine whether a structure is packed.
    pub fn is_packed(&self) -> bool {
        unsafe { LLVMIsPackedStruct(self.as_raw()) != 0 }
    }

    /// Determine whether a structure is opaque.
    pub fn is_opaque(&self) -> bool {
        unsafe { LLVMIsOpaqueStruct(self.as_raw()) != 0 }
    }
}

pub trait StructTypes {
    /// Create a new structure type in a context.
    fn structure(&self, elements: &[TypeRef], packed: bool) -> StructType;

    /// Create an empty structure in a context having a specified name.
    fn named_struct<S: AsRef<str>>(&self, name: S) -> StructType;
}

impl StructTypes for Context {
    fn structure(&self, elements: &[TypeRef], packed: bool) -> StructType {
        let mut elements = elements
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

        let t = unsafe {
            LLVMStructTypeInContext(
                self.as_raw(),
                elements.as_mut_ptr(),
                elements.len() as u32,
                if packed { 1 } else { 0 },
            )
        };

        StructType::from_raw(t)
    }

    fn named_struct<S: AsRef<str>>(&self, name: S) -> StructType {
        let cname = unchecked_cstring(name);

        StructType::from_raw(unsafe {
            LLVMStructCreateNamed(self.as_raw(), cname.as_ptr())
        })
    }
}

pub trait SeqType: AsTypeRef {
    /// Obtain the type of elements within a sequential type.
    fn element_type(&self) -> TypeRef {
        TypeRef(unsafe { LLVMGetElementType(self.as_raw()) })
    }
}

impl SeqType for ArrayType {}
impl SeqType for PointerType {}
impl SeqType for VectorType {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ArrayType(TypeRef);

inherit_type_ref!(ArrayType);

impl ArrayType {
    /// Create a fixed size array type that refers to a specific type.
    ///
    /// The created type will exist in the context that its element type exists in.
    pub fn new(element_type: TypeRef, element_count: usize) -> Self {
        ArrayType::from_raw(unsafe {
            LLVMArrayType(element_type.as_raw(), element_count as u32)
        })
    }

    fn from_raw(t: LLVMTypeRef) -> Self {
        ArrayType(TypeRef(t))
    }

    /// Obtain the length of an array type.
    pub fn len(&self) -> usize {
        unsafe { LLVMGetArrayLength(self.as_raw()) as usize }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PointerType(TypeRef);

inherit_type_ref!(PointerType);

impl PointerType {
    /// Create a pointer type that points to a defined type.
    ///
    /// The created type will exist in the context that its pointee type exists in.
    pub fn new(element_type: TypeRef, address_space: u32) -> Self {
        PointerType::from_raw(unsafe {
            LLVMPointerType(element_type.as_raw(), address_space)
        })
    }

    fn from_raw(t: LLVMTypeRef) -> Self {
        PointerType(TypeRef(t))
    }

    /// Obtain the address space of a pointer type.
    pub fn address_space(&self) -> u32 {
        unsafe { LLVMGetPointerAddressSpace(self.as_raw()) }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VectorType(TypeRef);

inherit_type_ref!(VectorType);

impl VectorType {
    /// Create a vector type that contains a defined type and has a specific number of elements.
    ///
    /// The created type will exist in the context thats its element type exists in.
    pub fn new(element_type: TypeRef, element_count: usize) -> Self {
        VectorType::from_raw(unsafe {
            LLVMVectorType(element_type.as_raw(), element_count as u32)
        })
    }

    fn from_raw(t: LLVMTypeRef) -> Self {
        VectorType(TypeRef(t))
    }

    /// Obtain the number of elements in a vector type.
    pub fn size(&self) -> usize {
        unsafe { LLVMGetVectorSize(self.as_raw()) as usize }
    }
}

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::Context;

    #[test]
    fn typeref() {
        let t = OtherType::void();

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMVoidTypeKind));
        assert!(!t.is_sized());
        assert!(!t.context().as_raw().is_null());

        assert_eq!(t.to_string(), "void");

        drop(t);

        let i = IntegerType::int64();

        assert!(!i.as_raw().is_null());
        assert!(matches!(i.kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(i.is_sized());
        assert!(!i.context().as_raw().is_null());

        assert_eq!(i.to_string(), "i64");
    }

    #[test]
    fn integer() {
        assert_eq!(IntegerType::int1().to_string(), "i1");
        assert_eq!(IntegerType::int8().to_string(), "i8");
        assert_eq!(IntegerType::int16().to_string(), "i16");
        assert_eq!(IntegerType::int32().to_string(), "i32");
        assert_eq!(IntegerType::int64().to_string(), "i64");
        assert_eq!(IntegerType::int128().to_string(), "i128");
        assert_eq!(IntegerType::int_type(512).to_string(), "i512");

        assert_eq!(IntegerType::int1().width(), 1);
        assert_eq!(IntegerType::int8().width(), 8);
        assert_eq!(IntegerType::int16().width(), 16);
        assert_eq!(IntegerType::int32().width(), 32);
        assert_eq!(IntegerType::int64().width(), 64);
        assert_eq!(IntegerType::int128().width(), 128);
        assert_eq!(IntegerType::int_type(512).width(), 512);

        let c = Context::new();

        assert_eq!(c.int1().to_string(), "i1");
        assert_eq!(c.int8().to_string(), "i8");
        assert_eq!(c.int16().to_string(), "i16");
        assert_eq!(c.int32().to_string(), "i32");
        assert_eq!(c.int64().to_string(), "i64");
        assert_eq!(c.int128().to_string(), "i128");
        assert_eq!(c.int_type(512).to_string(), "i512");

        assert_eq!(c.int1().width(), 1);
        assert_eq!(c.int8().width(), 8);
        assert_eq!(c.int16().width(), 16);
        assert_eq!(c.int32().width(), 32);
        assert_eq!(c.int64().width(), 64);
        assert_eq!(c.int128().width(), 128);
        assert_eq!(c.int_type(512).width(), 512);

        assert!(matches!(
            c.int1().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int8().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int16().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int32().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int64().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int128().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int_type(512).kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
    }

    #[test]
    fn floating_point() {
        assert_eq!(FloatingPointType::half().to_string(), "half");
        assert_eq!(FloatingPointType::float().to_string(), "float");
        assert_eq!(FloatingPointType::double().to_string(), "double");
        assert_eq!(FloatingPointType::x86_fp80().to_string(), "x86_fp80");
        assert_eq!(FloatingPointType::fp128().to_string(), "fp128");
        assert_eq!(FloatingPointType::ppc_fp128().to_string(), "ppc_fp128");

        let c = Context::new();

        assert_eq!(c.half().to_string(), "half");
        assert_eq!(c.float().to_string(), "float");
        assert_eq!(c.double().to_string(), "double");
        assert_eq!(c.x86_fp80().to_string(), "x86_fp80");
        assert_eq!(c.fp128().to_string(), "fp128");
        assert_eq!(c.ppc_fp128().to_string(), "ppc_fp128");

        assert!(matches!(
            c.half().kind(),
            llvm::LLVMTypeKind::LLVMHalfTypeKind
        ));
        assert!(matches!(
            c.float().kind(),
            llvm::LLVMTypeKind::LLVMFloatTypeKind
        ));
        assert!(matches!(
            c.double().kind(),
            llvm::LLVMTypeKind::LLVMDoubleTypeKind
        ));
        assert!(matches!(
            c.x86_fp80().kind(),
            llvm::LLVMTypeKind::LLVMX86_FP80TypeKind
        ));
        assert!(matches!(
            c.fp128().kind(),
            llvm::LLVMTypeKind::LLVMFP128TypeKind
        ));
        assert!(matches!(
            c.ppc_fp128().kind(),
            llvm::LLVMTypeKind::LLVMPPC_FP128TypeKind
        ));
    }

    #[test]
    fn other() {
        assert_eq!(OtherType::void().to_string(), "void");
        assert_eq!(OtherType::label().to_string(), "label");
        assert_eq!(OtherType::x86_mmx().to_string(), "x86_mmx");

        let c = Context::new();

        assert_eq!(c.void().to_string(), "void");
        assert_eq!(c.label().to_string(), "label");
        assert_eq!(c.x86_mmx().to_string(), "x86_mmx");

        assert!(matches!(
            c.void().kind(),
            llvm::LLVMTypeKind::LLVMVoidTypeKind
        ));
        assert!(matches!(
            c.label().kind(),
            llvm::LLVMTypeKind::LLVMLabelTypeKind
        ));
        assert!(matches!(
            c.x86_mmx().kind(),
            llvm::LLVMTypeKind::LLVMX86_MMXTypeKind
        ));
    }

    #[test]
    fn function() {
        let i64t = IntegerType::int64();
        let argts = [i64t, i64t, i64t];
        let t = FunctionType::new(i64t, &argts, false);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMFunctionTypeKind));
        assert!(!t.is_var_arg());
        assert_eq!(t.return_type(), i64t);
        assert_eq!(t.param_types(), argts);
    }

    #[test]
    fn struct_in_global_context() {
        let i16t = IntegerType::int16();
        let i32t = IntegerType::int32();
        let i64t = IntegerType::int64();
        let argts = [i16t, i32t, i64t];
        let t = StructType::new(&argts, true);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), None);
        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn struct_in_context() {
        let c = Context::new();

        let i16t = c.int16();
        let i32t = c.int32();
        let i64t = c.int64();
        let argts = [i16t, i32t, i64t];
        let t = c.structure(&argts, true);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), None);
        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn named_struct_in_context() {
        let c = Context::new();

        let t = c.named_struct("test");

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), Some("test".into()));
        assert_eq!(t.element_count(), 0);

        let i16t = c.int16();
        let i32t = c.int32();
        let i64t = c.int64();
        let argts = [i16t, i32t, i64t];
        t.set_body(&argts, true);

        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn array() {
        let c = Context::new();
        let i64t = c.int64();

        let t = ArrayType::new(i64t, 8);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMArrayTypeKind));
        assert_eq!(t.element_type(), i64t);
        assert_eq!(t.len(), 8);
    }

    #[test]
    fn pointer() {
        let c = Context::new();
        let i64t = c.int64();

        let t = PointerType::new(i64t, 123);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMPointerTypeKind));
        assert_eq!(t.element_type(), i64t);
        assert_eq!(t.address_space(), 123);
    }

    #[test]
    fn vector() {
        let c = Context::new();
        let i64t = c.int64();

        let t = VectorType::new(i64t, 8);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMVectorTypeKind));
        assert_eq!(t.element_type(), i64t);
        assert_eq!(t.size(), 8);
    }
}
