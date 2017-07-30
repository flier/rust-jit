use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::ops::Deref;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use context::{Context, GlobalContext};
use module::AddressSpace;
use utils::{AsBool, AsLLVMBool, unchecked_cstring};
use value::ValueRef;

/// Each value in the LLVM IR has a type, an `TypeRef`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeRef(LLVMTypeRef);

inherit_from!(TypeRef, LLVMTypeRef);

macro_rules! inherit_type_ref {
    ($ty:ident) => {
        inherit_from!($ty, TypeRef, LLVMTypeRef);
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
        unsafe { LLVMTypeIsSized(self.0) }.as_bool()
    }

    /// Obtain the context to which this type instance is associated.
    pub fn context(&self) -> Context {
        unsafe { LLVMGetTypeContext(self.0) }.into()
    }

    /// Obtain a constant value referring to an undefined value of a type.
    pub fn undef(&self) -> ValueRef {
        unsafe { LLVMGetUndef(self.0) }.into()
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

impl GlobalContext {
    pub fn int1_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt1Type() })
    }

    pub fn int8_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt8Type() })
    }

    pub fn int16_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt16Type() })
    }

    pub fn int32_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt32Type() })
    }

    pub fn int64_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt64Type() })
    }

    pub fn int128_t() -> IntegerType {
        TypeRef(unsafe { LLVMInt128Type() })
    }

    pub fn int_type(bits: u32) -> IntegerType {
        TypeRef(unsafe { LLVMIntType(bits) })
    }
}

/// Obtain an integer type from a context with specified bit width.
pub trait IntegerTypes {
    /// a single-bit integer.
    fn int1_t(&self) -> IntegerType;

    /// a 8-bit integer.
    fn int8_t(&self) -> IntegerType;

    /// a 16-bit integer.
    fn int16_t(&self) -> IntegerType;

    /// a 32-bit integer.
    fn int32_t(&self) -> IntegerType;

    /// a 64-bit integer.
    fn int64_t(&self) -> IntegerType;

    /// a 64-bit integer.
    fn int128_t(&self) -> IntegerType;

    fn int_type(&self, bits: u32) -> IntegerType;
}

impl IntegerTypes for Context {
    fn int1_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt1TypeInContext(self.as_raw()) })
    }

    fn int8_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt8TypeInContext(self.as_raw()) })
    }

    fn int16_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt16TypeInContext(self.as_raw()) })
    }

    fn int32_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt32TypeInContext(self.as_raw()) })
    }

    fn int64_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt64TypeInContext(self.as_raw()) })
    }

    fn int128_t(&self) -> IntegerType {
        TypeRef(unsafe { LLVMInt128TypeInContext(self.as_raw()) })
    }

    fn int_type(&self, bits: u32) -> IntegerType {
        TypeRef(unsafe { LLVMIntTypeInContext(self.as_raw(), bits) })
    }
}

// Floating Point Types
pub type FloatingPointType = TypeRef;

impl GlobalContext {
    /// Obtain a 16-bit floating point type from the global context.
    pub fn half_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMHalfType() })
    }

    /// Obtain a 32-bit floating point type from the global context.
    pub fn float_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMFloatType() })
    }

    /// Obtain a 64-bit floating point type from the global context.
    pub fn double_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMDoubleType() })
    }

    /// Obtain a 80-bit floating point type (X87) from the global context.
    pub fn x86_fp80_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMX86FP80Type() })
    }

    /// Obtain a 128-bit floating point type (112-bit mantissa) from the global context.
    pub fn fp128_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMFP128Type() })
    }

    /// Obtain a 128-bit floating point type (two 64-bits) from the global context.
    pub fn ppc_fp128_t() -> FloatingPointType {
        TypeRef(unsafe { LLVMPPCFP128Type() })
    }
}

pub trait FloatingPointTypes {
    /// Obtain a 16-bit floating point type from a context.
    fn half_t(&self) -> FloatingPointType;

    /// Obtain a 32-bit floating point type from a context.
    fn float_t(&self) -> FloatingPointType;

    /// Obtain a 64-bit floating point type from a context.
    fn double_t(&self) -> FloatingPointType;

    /// Obtain a 80-bit floating point type (X87) from a context.
    fn x86_fp80_t(&self) -> FloatingPointType;

    /// Obtain a 128-bit floating point type (112-bit mantissa) from a context.
    fn fp128_t(&self) -> FloatingPointType;

    /// Obtain a 128-bit floating point type (two 64-bits) from a context.
    fn ppc_fp128_t(&self) -> FloatingPointType;
}

impl FloatingPointTypes for Context {
    fn half_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMHalfTypeInContext(self.as_raw()) })
    }

    fn float_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMFloatTypeInContext(self.as_raw()) })
    }

    fn double_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMDoubleTypeInContext(self.as_raw()) })
    }

    fn x86_fp80_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMX86FP80TypeInContext(self.as_raw()) })
    }

    fn fp128_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMFP128TypeInContext(self.as_raw()) })
    }

    fn ppc_fp128_t(&self) -> FloatingPointType {
        TypeRef(unsafe { LLVMPPCFP128TypeInContext(self.as_raw()) })
    }
}

// Other Types
pub type OtherType = TypeRef;

impl GlobalContext {
    /// Create a void type in the global context.
    pub fn void_t() -> OtherType {
        TypeRef(unsafe { LLVMVoidType() })
    }

    /// Create a label type in the global context.
    pub fn label_t() -> OtherType {
        TypeRef(unsafe { LLVMLabelType() })
    }

    /// Create a X86 MMX type in the global context.
    pub fn x86_mmx_t() -> OtherType {
        TypeRef(unsafe { LLVMX86MMXType() })
    }
}

pub trait OtherTypes {
    /// Create a void type in a context.
    fn void_t(&self) -> OtherType;

    /// Create a label type in a context.
    fn label_t(&self) -> OtherType;

    /// Create a X86 MMX type in a context.
    fn x86_mmx_t(&self) -> OtherType;
}

impl OtherTypes for Context {
    fn void_t(&self) -> OtherType {
        TypeRef(unsafe { LLVMVoidTypeInContext(self.as_raw()) })
    }

    fn label_t(&self) -> OtherType {
        TypeRef(unsafe { LLVMLabelTypeInContext(self.as_raw()) })
    }

    fn x86_mmx_t(&self) -> OtherType {
        TypeRef(unsafe { LLVMX86MMXTypeInContext(self.as_raw()) })
    }
}

/// Structure to represent struct types.
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
                packed.as_bool(),
            )
        };

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
    pub fn set_body(self, elements: &[TypeRef], packed: bool) -> Self {
        let mut elements = elements
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

        unsafe {
            LLVMStructSetBody(
                self.as_raw(),
                elements.as_mut_ptr(),
                elements.len() as u32,
                packed.as_bool(),
            )
        }

        self
    }

    /// Get the elements within a structure.
    pub fn element_types(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMCountStructElementTypes(self.as_raw()) };
        let mut elements: Vec<LLVMTypeRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetStructElementTypes(self.as_raw(), elements.as_mut_ptr()) };

        elements.into_iter().map(TypeRef).collect()
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
        unsafe { LLVMIsPackedStruct(self.as_raw()) }.as_bool()
    }

    /// Determine whether a structure is opaque.
    pub fn is_opaque(&self) -> bool {
        unsafe { LLVMIsOpaqueStruct(self.as_raw()) }.as_bool()
    }
}

pub trait ToStructType {
    /// Create a new structure type in a context.
    fn anonymous_struct_t(&self, elements: &[TypeRef], packed: bool) -> StructType;

    /// Create an empty structure in a context having a specified name.
    fn empty_struct_t<S: AsRef<str>>(&self, name: S) -> StructType;

    fn struct_t<S: AsRef<str>>(&self, name: S, elements: &[TypeRef], packed: bool) -> StructType {
        let ty = self.empty_struct_t(name);

        ty.set_body(elements, packed)
    }
}

impl ToStructType for Context {
    fn anonymous_struct_t(&self, elements: &[TypeRef], packed: bool) -> StructType {
        let mut elements = elements
            .iter()
            .map(|t| t.as_raw())
            .collect::<Vec<LLVMTypeRef>>();

        let ty = unsafe {
            LLVMStructTypeInContext(
                self.as_raw(),
                elements.as_mut_ptr(),
                elements.len() as u32,
                packed.as_bool(),
            )
        }.into();

        trace!("created annonymous structure in {:?}: {:?}", self, ty);

        ty
    }

    fn empty_struct_t<S: AsRef<str>>(&self, name: S) -> StructType {
        let cname = unchecked_cstring(name);

        let ty = unsafe { LLVMStructCreateNamed(self.as_raw(), cname.as_ptr()) }.into();

        trace!(
            "created `{}` structure in {:?}: {:?}",
            cname.to_string_lossy(),
            self,
            ty
        );

        ty
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

/// Structure to represent array types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ArrayType(TypeRef);

inherit_type_ref!(ArrayType);

impl ArrayType {
    /// Obtain the length of an array type.
    pub fn len(&self) -> usize {
        unsafe { LLVMGetArrayLength(self.as_raw()) as usize }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub trait ToArrayType {
    /// Create a fixed size array type that refers to a specific type.
    ///
    /// The created type will exist in the context that its element type exists in.
    fn array_t(&self, element_count: usize) -> ArrayType;
}

impl ToArrayType for TypeRef {
    fn array_t(&self, element_count: usize) -> ArrayType {
        unsafe { LLVMArrayType(self.as_raw(), element_count as u32) }.into()
    }
}

/// Structure to represent pointers.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PointerType(TypeRef);

inherit_type_ref!(PointerType);

impl PointerType {
    /// Obtain the address space of a pointer type.
    pub fn address_space(&self) -> AddressSpace {
        unsafe { LLVMGetPointerAddressSpace(self.as_raw()) }
    }
}

pub trait ToPointerType {
    /// Create a pointer type that points to a defined type.
    ///
    /// The created type will exist in the context that its pointee type exists in.
    fn ptr_t(&self) -> PointerType {
        self.ptr_t_in_address_space(0)
    }

    /// Create a pointer type in the address space that points to a defined type.
    ///
    /// The created type will exist in the context that its pointee type exists in.
    fn ptr_t_in_address_space(&self, address_space: AddressSpace) -> PointerType;
}

impl ToPointerType for TypeRef {
    fn ptr_t_in_address_space(&self, address_space: AddressSpace) -> PointerType {
        unsafe { LLVMPointerType(self.as_raw(), address_space) }.into()
    }
}

/// Structure to represent vector types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VectorType(TypeRef);

inherit_type_ref!(VectorType);

impl VectorType {
    /// Obtain the number of elements in a vector type.
    pub fn size(&self) -> usize {
        unsafe { LLVMGetVectorSize(self.as_raw()) as usize }
    }
}

pub trait ToVectorType {
    /// Create a vector type that contains a defined type and has a specific number of elements.
    ///
    /// The created type will exist in the context thats its element type exists in.
    fn vector_t(&self, element_count: usize) -> VectorType;
}

impl ToVectorType for TypeRef {
    fn vector_t(&self, element_count: usize) -> VectorType {
        unsafe { LLVMVectorType(self.as_raw(), element_count as u32) }.into()
    }
}

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::Context;

    #[test]
    fn typeref() {
        let void_t = GlobalContext::void_t();

        assert!(!void_t.as_raw().is_null());
        assert!(matches!(
            void_t.kind(),
            llvm::LLVMTypeKind::LLVMVoidTypeKind
        ));
        assert!(!void_t.is_sized());
        assert!(!void_t.context().as_raw().is_null());

        assert_eq!(void_t.to_string(), "void");

        let i64_t = GlobalContext::int64_t();

        assert!(!i64_t.as_raw().is_null());
        assert!(matches!(
            i64_t.kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(i64_t.is_sized());
        assert!(!i64_t.context().as_raw().is_null());

        assert_eq!(i64_t.to_string(), "i64");
    }

    #[test]
    fn integer() {
        assert_eq!(GlobalContext::int1_t().to_string(), "i1");
        assert_eq!(GlobalContext::int8_t().to_string(), "i8");
        assert_eq!(GlobalContext::int16_t().to_string(), "i16");
        assert_eq!(GlobalContext::int32_t().to_string(), "i32");
        assert_eq!(GlobalContext::int64_t().to_string(), "i64");
        assert_eq!(GlobalContext::int128_t().to_string(), "i128");
        assert_eq!(GlobalContext::int_type(512).to_string(), "i512");

        assert_eq!(GlobalContext::int1_t().width(), 1);
        assert_eq!(GlobalContext::int8_t().width(), 8);
        assert_eq!(GlobalContext::int16_t().width(), 16);
        assert_eq!(GlobalContext::int32_t().width(), 32);
        assert_eq!(GlobalContext::int64_t().width(), 64);
        assert_eq!(GlobalContext::int128_t().width(), 128);
        assert_eq!(GlobalContext::int_type(512).width(), 512);

        let c = Context::new();

        assert_eq!(c.int1_t().to_string(), "i1");
        assert_eq!(c.int8_t().to_string(), "i8");
        assert_eq!(c.int16_t().to_string(), "i16");
        assert_eq!(c.int32_t().to_string(), "i32");
        assert_eq!(c.int64_t().to_string(), "i64");
        assert_eq!(c.int128_t().to_string(), "i128");
        assert_eq!(c.int_type(512).to_string(), "i512");

        assert_eq!(c.int1_t().width(), 1);
        assert_eq!(c.int8_t().width(), 8);
        assert_eq!(c.int16_t().width(), 16);
        assert_eq!(c.int32_t().width(), 32);
        assert_eq!(c.int64_t().width(), 64);
        assert_eq!(c.int128_t().width(), 128);
        assert_eq!(c.int_type(512).width(), 512);

        assert!(matches!(
            c.int1_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int8_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int16_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int32_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int64_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int128_t().kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
        assert!(matches!(
            c.int_type(512).kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
    }

    #[test]
    fn floating_point() {
        assert_eq!(GlobalContext::half_t().to_string(), "half");
        assert_eq!(GlobalContext::float_t().to_string(), "float");
        assert_eq!(GlobalContext::double_t().to_string(), "double");
        assert_eq!(GlobalContext::x86_fp80_t().to_string(), "x86_fp80");
        assert_eq!(GlobalContext::fp128_t().to_string(), "fp128");
        assert_eq!(GlobalContext::ppc_fp128_t().to_string(), "ppc_fp128");

        let c = Context::new();

        assert_eq!(c.half_t().to_string(), "half");
        assert_eq!(c.float_t().to_string(), "float");
        assert_eq!(c.double_t().to_string(), "double");
        assert_eq!(c.x86_fp80_t().to_string(), "x86_fp80");
        assert_eq!(c.fp128_t().to_string(), "fp128");
        assert_eq!(c.ppc_fp128_t().to_string(), "ppc_fp128");

        assert!(matches!(
            c.half_t().kind(),
            llvm::LLVMTypeKind::LLVMHalfTypeKind
        ));
        assert!(matches!(
            c.float_t().kind(),
            llvm::LLVMTypeKind::LLVMFloatTypeKind
        ));
        assert!(matches!(
            c.double_t().kind(),
            llvm::LLVMTypeKind::LLVMDoubleTypeKind
        ));
        assert!(matches!(
            c.x86_fp80_t().kind(),
            llvm::LLVMTypeKind::LLVMX86_FP80TypeKind
        ));
        assert!(matches!(
            c.fp128_t().kind(),
            llvm::LLVMTypeKind::LLVMFP128TypeKind
        ));
        assert!(matches!(
            c.ppc_fp128_t().kind(),
            llvm::LLVMTypeKind::LLVMPPC_FP128TypeKind
        ));
    }

    #[test]
    fn other() {
        assert_eq!(GlobalContext::void_t().to_string(), "void");
        assert_eq!(GlobalContext::label_t().to_string(), "label");
        assert_eq!(GlobalContext::x86_mmx_t().to_string(), "x86_mmx");

        let c = Context::new();

        assert_eq!(c.void_t().to_string(), "void");
        assert_eq!(c.label_t().to_string(), "label");
        assert_eq!(c.x86_mmx_t().to_string(), "x86_mmx");

        assert!(matches!(
            c.void_t().kind(),
            llvm::LLVMTypeKind::LLVMVoidTypeKind
        ));
        assert!(matches!(
            c.label_t().kind(),
            llvm::LLVMTypeKind::LLVMLabelTypeKind
        ));
        assert!(matches!(
            c.x86_mmx_t().kind(),
            llvm::LLVMTypeKind::LLVMX86_MMXTypeKind
        ));
    }

    #[test]
    fn struct_in_global_context() {
        let i16_t = GlobalContext::int16_t();
        let i32_t = GlobalContext::int32_t();
        let i64_t = GlobalContext::int64_t();
        let argts = [i16_t, i32_t, i64_t];
        let t = StructType::new(&argts, true);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), None);
        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32_t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn struct_in_context() {
        let c = Context::new();

        let i16_t = c.int16_t();
        let i32_t = c.int32_t();
        let i64_t = c.int64_t();
        let argts = [i16_t, i32_t, i64_t];
        let t = c.anonymous_struct_t(&argts, true);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), None);
        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32_t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn named_struct_in_context() {
        let c = Context::new();

        let t = c.empty_struct_t("test");

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMStructTypeKind));
        assert_eq!(t.name(), Some("test".into()));
        assert_eq!(t.element_count(), 0);

        let i16_t = c.int16_t();
        let i32_t = c.int32_t();
        let i64_t = c.int64_t();
        let argts = [i16_t, i32_t, i64_t];
        t.set_body(&argts, true);

        assert_eq!(t.element_count(), 3);
        assert_eq!(t.element_type(1), Some(i32_t));
        assert_eq!(t.element_type(4), None);
        assert_eq!(t.element_types(), argts);
        assert!(t.is_packed());
        assert!(!t.is_opaque());
    }

    #[test]
    fn array() {
        let c = Context::new();
        let i64_t = c.int64_t();

        let t = i64_t.array_t(8);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMArrayTypeKind));
        assert_eq!(t.element_type(), i64_t);
        assert_eq!(t.len(), 8);
    }

    #[test]
    fn pointer() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let t = i64_t.ptr_t_in_address_space(123);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMPointerTypeKind));
        assert_eq!(t.element_type(), i64_t);
        assert_eq!(t.address_space(), 123);
    }

    #[test]
    fn vector() {
        let c = Context::new();
        let i64_t = c.int64_t();

        let t = i64_t.vector_t(8);

        assert!(!t.as_raw().is_null());
        assert!(matches!(t.kind(), llvm::LLVMTypeKind::LLVMVectorTypeKind));
        assert_eq!(t.element_type(), i64_t);
        assert_eq!(t.size(), 8);
    }
}
