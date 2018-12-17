use std::borrow::Cow;
use std::fmt;
use std::ptr;

use boolinator::Boolinator;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::*;

use crate::constant::ConstantInt;
use crate::context::{Context, GlobalContext};
use crate::function::FunctionType;
use crate::module::AddressSpace;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, AsResult, DisposableMessage, UncheckedCStr};
use crate::value::ValueRef;

#[macro_export]
macro_rules! types {
    ($($x:expr),*) => (&[ $($crate::TypeRef::from($x)),* ]);
    ($($x:expr,)*) => (&[ $($crate::TypeRef::from($x)),* ]);
}

/// Each value in the LLVM IR has a type, an `TypeRef`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeRef(LLVMTypeRef);

unsafe impl Send for TypeRef {}
unsafe impl Sync for TypeRef {}

inherit_from!(TypeRef, LLVMTypeRef);

macro_rules! inherit_type_ref {
    ($ty: ident) => {
        inherit_from!($ty, TypeRef, LLVMTypeRef);
    };
}

pub type TypeKind = LLVMTypeKind;

pub trait AsTypeRef: AsRaw<RawType = LLVMTypeRef> {}

impl<T> AsTypeRef for T where T: AsRaw<RawType = LLVMTypeRef> {}

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

    /// Computes the alignment of a type in a target independent way (Note: the return type is an i64).
    pub fn align_of(&self) -> ConstantInt {
        unsafe { LLVMAlignOf(self.0) }.into()
    }

    /// Computes the (alloc) size of a type (in address-units, not bits)
    /// in a target independent way (Note: the return type is an i64).
    pub fn size_of(&self) -> ConstantInt {
        unsafe { LLVMSizeOf(self.0) }.into()
    }

    /// Return the basic size of this type if it is a primitive type. These are
    /// fixed by LLVM and are not target-dependent.
    /// This will return zero if the type does not have a size or is not a
    /// primitive type.
    pub fn primitive_size_in_bits(&self) -> Option<usize> {
        match self.kind() {
            LLVMTypeKind::LLVMHalfTypeKind => Some(16),
            LLVMTypeKind::LLVMFloatTypeKind => Some(32),
            LLVMTypeKind::LLVMDoubleTypeKind | LLVMTypeKind::LLVMX86_MMXTypeKind => Some(64),
            LLVMTypeKind::LLVMX86_FP80TypeKind => Some(80),
            LLVMTypeKind::LLVMFP128TypeKind | LLVMTypeKind::LLVMPPC_FP128TypeKind => Some(128),
            LLVMTypeKind::LLVMIntegerTypeKind => Some(self.bit_width()),
            LLVMTypeKind::LLVMVectorTypeKind => VectorType(*self).bit_width(),
            _ => None,
        }
    }

    /// Return true if this is 'void'.
    pub fn is_void_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMVoidTypeKind
    }

    /// Return true if this is 'half', a 16-bit IEEE fp type.
    pub fn is_half_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMHalfTypeKind
    }

    /// Return true if this is 'float', a 32-bit IEEE fp type.
    pub fn is_float_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMFloatTypeKind
    }

    /// Return true if this is 'double', a 64-bit IEEE fp type.
    pub fn is_double_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMDoubleTypeKind
    }

    /// Return true if this is x86 long double.
    pub fn is_x86_fp80_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMX86_FP80TypeKind
    }

    /// Return true if this is 'fp128'.
    pub fn is_fp128_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMFP128TypeKind
    }

    /// Return true if this is powerpc long double.
    pub fn is_ppc_fp128_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMPPC_FP128TypeKind
    }

    /// Return true if this is one of the six floating-point types
    pub fn is_floating_point_ty(&self) -> bool {
        match self.kind() {
            LLVMTypeKind::LLVMHalfTypeKind
            | LLVMTypeKind::LLVMFloatTypeKind
            | LLVMTypeKind::LLVMDoubleTypeKind
            | LLVMTypeKind::LLVMX86_FP80TypeKind
            | LLVMTypeKind::LLVMFP128TypeKind
            | LLVMTypeKind::LLVMPPC_FP128TypeKind => true,
            _ => false,
        }
    }

    /// Return true if this is 'label'.
    pub fn is_label_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMLabelTypeKind
    }

    /// True if this is an instance of IntegerType.
    pub fn is_integer_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMIntegerTypeKind
    }

    /// True if this is an instance of FunctionType.
    pub fn is_func_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMFunctionTypeKind
    }

    /// True if this is an instance of StructType.
    pub fn is_struct_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMStructTypeKind
    }

    /// True if this is an instance of ArrayType.
    pub fn is_array_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMArrayTypeKind
    }

    /// True if this is an instance of PointerType.
    pub fn is_pointer_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMPointerTypeKind
    }

    /// True if this is an instance of VectorType.
    pub fn is_vector_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMVectorTypeKind
    }

    /// Return true if this is 'metadata'.
    pub fn is_metadata_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMMetadataTypeKind
    }

    /// Return true if this is X86 MMX.
    pub fn is_x86_mmx_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMX86_MMXTypeKind
    }

    /// Return true if this is 'token'.
    pub fn is_token_ty(&self) -> bool {
        self.kind() == LLVMTypeKind::LLVMTokenTypeKind
    }

    pub fn as_struct_ty(&self) -> Option<StructType> {
        if self.kind() == LLVMTypeKind::LLVMStructTypeKind {
            Some((*self).into())
        } else {
            None
        }
    }

    pub fn as_array_ty(&self) -> Option<ArrayType> {
        if self.kind() == LLVMTypeKind::LLVMArrayTypeKind {
            Some((*self).into())
        } else {
            None
        }
    }

    pub fn as_ptr_ty(&self) -> Option<PointerType> {
        if self.kind() == LLVMTypeKind::LLVMPointerTypeKind {
            Some((*self).into())
        } else {
            None
        }
    }

    pub fn as_vector_ty(&self) -> Option<VectorType> {
        if self.kind() == LLVMTypeKind::LLVMVectorTypeKind {
            Some((*self).into())
        } else {
            None
        }
    }

    pub fn as_func_ty(&self) -> Option<FunctionType> {
        if self.kind() == LLVMTypeKind::LLVMFunctionTypeKind {
            Some((*self).into())
        } else {
            None
        }
    }
}

impl fmt::Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", unsafe { LLVMPrintTypeToString(self.0) }.into_string())
    }
}

pub type IntegerType = TypeRef;

impl IntegerType {
    pub fn bit_width(&self) -> usize {
        unsafe { LLVMGetIntTypeWidth(self.0) as usize }
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

    pub fn int_type(bits: usize) -> IntegerType {
        TypeRef(unsafe { LLVMIntType(bits as u32) })
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

    fn int_type(&self, bits: usize) -> IntegerType;
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

    fn int_type(&self, bits: usize) -> IntegerType {
        TypeRef(unsafe { LLVMIntTypeInContext(self.as_raw(), bits as u32) })
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
    /// Create a token type in a context.
    fn token_t(&self) -> OtherType;

    /// Create a metadata type in a context.
    fn metadata_t(&self) -> OtherType;

    /// Create a void type in a context.
    fn void_t(&self) -> OtherType;

    /// Create a label type in a context.
    fn label_t(&self) -> OtherType;

    /// Create a X86 MMX type in a context.
    fn x86_mmx_t(&self) -> OtherType;
}

impl OtherTypes for Context {
    fn token_t(&self) -> OtherType {
        unsafe { LLVMTokenTypeInContext(self.as_raw()) }.into()
    }

    fn metadata_t(&self) -> OtherType {
        unsafe { LLVMMetadataTypeInContext(self.as_raw()) }.into()
    }

    fn void_t(&self) -> OtherType {
        unsafe { LLVMVoidTypeInContext(self.as_raw()) }.into()
    }

    fn label_t(&self) -> OtherType {
        unsafe { LLVMLabelTypeInContext(self.as_raw()) }.into()
    }

    fn x86_mmx_t(&self) -> OtherType {
        unsafe { LLVMX86MMXTypeInContext(self.as_raw()) }.into()
    }
}

/// Structure to represent struct types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StructType(TypeRef);

inherit_type_ref!(StructType);

impl StructType {
    /// Create a new structure type in the global context.
    pub fn new(elements: &[TypeRef], packed: bool) -> Self {
        let mut elements = elements.iter().map(|t| t.as_raw()).collect::<Vec<LLVMTypeRef>>();

        let t = unsafe { LLVMStructType(elements.as_mut_ptr(), elements.len() as u32, packed.as_bool()) };

        StructType(TypeRef(t))
    }

    /// Obtain the name of a structure.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe { LLVMGetStructName(self.as_raw()).as_ref() }.map(|name| name.as_str())
    }

    /// Set the contents of a structure type.
    pub fn set_body(self, elements: &[TypeRef], packed: bool) -> Self {
        let mut elements = elements.iter().map(|t| t.as_raw()).collect::<Vec<LLVMTypeRef>>();

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
        (index < self.element_count())
            .and_option_from(|| unsafe { LLVMStructGetTypeAtIndex(self.as_raw(), index as u32) }.ok())
    }

    /// Get the elements within a structure.
    pub fn elements(&self) -> ElementTypes {
        let count = unsafe { LLVMCountStructElementTypes(self.as_raw()) };

        ElementTypes {
            ty: self.as_raw(),
            index: 0,
            count,
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

pub struct ElementTypes {
    ty: LLVMTypeRef,
    index: u32,
    count: u32,
}

impl Iterator for ElementTypes {
    type Item = TypeRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.count {
            None
        } else {
            let ty = unsafe { LLVMStructGetTypeAtIndex(self.ty, self.index) };

            self.index += 1;

            Some(TypeRef(ty))
        }
    }
}

pub trait ToStructType {
    /// Create a new structure type in a context.
    fn struct_t(&self, elements: &[TypeRef], packed: bool) -> StructType;

    /// Create an empty structure in a context having a specified name.
    fn empty_struct_t<S: AsRef<str>>(&self, name: S) -> StructType;

    fn named_struct_t<S: AsRef<str>>(&self, name: S, elements: &[TypeRef], packed: bool) -> StructType {
        self.empty_struct_t(name).set_body(elements, packed)
    }
}

impl ToStructType for Context {
    fn struct_t(&self, elements: &[TypeRef], packed: bool) -> StructType {
        let mut elements = elements.iter().map(|t| t.as_raw()).collect::<Vec<LLVMTypeRef>>();

        let ty = unsafe {
            LLVMStructTypeInContext(
                self.as_raw(),
                elements.as_mut_ptr(),
                elements.len() as u32,
                packed.as_bool(),
            )
        }
        .into();

        trace!("created annonymous structure in {:?}: {:?}", self, ty);

        ty
    }

    fn empty_struct_t<S: AsRef<str>>(&self, name: S) -> StructType {
        let name = name.as_ref();
        let ty = unsafe { LLVMStructCreateNamed(self.as_raw(), cstr!(name)) }.into();

        trace!("created `{}` structure in {:?}: {:?}", name, self, ty);

        ty
    }
}

pub trait SequentialType: AsTypeRef {
    /// Obtain the type of elements within a sequential type.
    fn element_type(&self) -> TypeRef {
        TypeRef(unsafe { LLVMGetElementType(self.as_raw()) })
    }

    /// Returns type's subtypes
    fn subtypes(&self) -> Vec<TypeRef> {
        let count = unsafe { LLVMGetNumContainedTypes(self.as_raw()) };
        let mut subtypes = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetSubtypes(self.as_raw(), subtypes.as_mut_ptr()) }

        subtypes.into_iter().map(|ty| ty.into()).collect()
    }
}

impl SequentialType for ArrayType {}
impl SequentialType for PointerType {}
impl SequentialType for VectorType {}

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
    pub fn len(&self) -> usize {
        unsafe { LLVMGetVectorSize(self.as_raw()) as usize }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return the number of bits in the Vector type.
    /// Returns zero when the vector is a vector of pointers.
    pub fn bit_width(&self) -> Option<usize> {
        self.element_type()
            .primitive_size_in_bits()
            .map(|size| size * self.len())
    }

    /// This method gets a `VectorType` with the same number of elements as
    /// the input type, and the element type is an integer type of the same width
    /// as the input element type.
    pub fn integer_vector_t(&self) -> VectorType {
        let bits = self.element_type().primitive_size_in_bits().unwrap_or_default();

        self.context().int_type(bits).vector_t(self.len())
    }

    /// This method is like `integer_vector_t` except that the element types are
    /// twice as wide as the elements in the input type.
    pub fn extended_element_vector_t(&self) -> VectorType {
        let bits = self.element_type().primitive_size_in_bits().unwrap_or_default() * 2;

        self.context().int_type(bits).vector_t(self.len())
    }

    /// This method is like `integer_vector_t` except that the element types are
    /// half as wide as the elements in the input type.
    pub fn truncated_element_vector_t(&self) -> VectorType {
        let bits = self.element_type().primitive_size_in_bits().unwrap_or_default() / 2;

        self.context().int_type(bits).vector_t(self.len())
    }

    /// This method returns a VectorType with half as many elements as the
    /// input type and the same element type.
    pub fn half_elements_vector_t(&self) -> VectorType {
        self.element_type().vector_t(self.len() / 2)
    }

    /// This method returns a VectorType with twice as many elements as the
    /// input type and the same element type.
    pub fn double_elements_vector_t(&self) -> VectorType {
        self.element_type().vector_t(self.len() * 2)
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
    use crate::llvm;

    use super::*;
    use crate::context::Context;

    #[test]
    fn typeref() {
        let c = Context::new();
        let void_t = c.void_t();

        assert!(!void_t.as_raw().is_null());
        assert!(matches!(void_t.kind(), llvm::LLVMTypeKind::LLVMVoidTypeKind));
        assert!(!void_t.is_sized());
        assert!(!void_t.context().as_raw().is_null());

        assert_eq!(void_t.to_string(), "void");

        let i64_t = c.int64_t();

        assert!(!i64_t.as_raw().is_null());
        assert!(matches!(i64_t.kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(i64_t.is_sized());
        assert!(!i64_t.context().as_raw().is_null());

        assert_eq!(i64_t.to_string(), "i64");
    }

    #[test]
    fn global_context() {
        assert_eq!(GlobalContext::int1_t().to_string(), "i1");
        assert_eq!(GlobalContext::int8_t().to_string(), "i8");
        assert_eq!(GlobalContext::int16_t().to_string(), "i16");
        assert_eq!(GlobalContext::int32_t().to_string(), "i32");
        assert_eq!(GlobalContext::int64_t().to_string(), "i64");
        assert_eq!(GlobalContext::int128_t().to_string(), "i128");
        assert_eq!(GlobalContext::int_type(512).to_string(), "i512");

        assert_eq!(GlobalContext::int1_t().bit_width(), 1);
        assert_eq!(GlobalContext::int8_t().bit_width(), 8);
        assert_eq!(GlobalContext::int16_t().bit_width(), 16);
        assert_eq!(GlobalContext::int32_t().bit_width(), 32);
        assert_eq!(GlobalContext::int64_t().bit_width(), 64);
        assert_eq!(GlobalContext::int128_t().bit_width(), 128);
        assert_eq!(GlobalContext::int_type(512).bit_width(), 512);

        assert_eq!(GlobalContext::half_t().to_string(), "half");
        assert_eq!(GlobalContext::float_t().to_string(), "float");
        assert_eq!(GlobalContext::double_t().to_string(), "double");
        assert_eq!(GlobalContext::x86_fp80_t().to_string(), "x86_fp80");
        assert_eq!(GlobalContext::fp128_t().to_string(), "fp128");
        assert_eq!(GlobalContext::ppc_fp128_t().to_string(), "ppc_fp128");

        assert_eq!(GlobalContext::void_t().to_string(), "void");
        assert_eq!(GlobalContext::label_t().to_string(), "label");
        assert_eq!(GlobalContext::x86_mmx_t().to_string(), "x86_mmx");

        struct_in_global_context()
    }

    #[test]
    fn integer() {
        let c = Context::new();

        assert_eq!(c.int1_t().to_string(), "i1");
        assert_eq!(c.int8_t().to_string(), "i8");
        assert_eq!(c.int16_t().to_string(), "i16");
        assert_eq!(c.int32_t().to_string(), "i32");
        assert_eq!(c.int64_t().to_string(), "i64");
        assert_eq!(c.int128_t().to_string(), "i128");
        assert_eq!(c.int_type(512).to_string(), "i512");

        assert_eq!(c.int1_t().bit_width(), 1);
        assert_eq!(c.int8_t().bit_width(), 8);
        assert_eq!(c.int16_t().bit_width(), 16);
        assert_eq!(c.int32_t().bit_width(), 32);
        assert_eq!(c.int64_t().bit_width(), 64);
        assert_eq!(c.int128_t().bit_width(), 128);
        assert_eq!(c.int_type(512).bit_width(), 512);

        assert!(matches!(c.int1_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(c.int8_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(c.int16_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(c.int32_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(c.int64_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(c.int128_t().kind(), llvm::LLVMTypeKind::LLVMIntegerTypeKind));
        assert!(matches!(
            c.int_type(512).kind(),
            llvm::LLVMTypeKind::LLVMIntegerTypeKind
        ));
    }

    #[test]
    fn floating_point() {
        let c = Context::new();

        assert_eq!(c.half_t().to_string(), "half");
        assert_eq!(c.float_t().to_string(), "float");
        assert_eq!(c.double_t().to_string(), "double");
        assert_eq!(c.x86_fp80_t().to_string(), "x86_fp80");
        assert_eq!(c.fp128_t().to_string(), "fp128");
        assert_eq!(c.ppc_fp128_t().to_string(), "ppc_fp128");

        assert!(matches!(c.half_t().kind(), llvm::LLVMTypeKind::LLVMHalfTypeKind));
        assert!(matches!(c.float_t().kind(), llvm::LLVMTypeKind::LLVMFloatTypeKind));
        assert!(matches!(c.double_t().kind(), llvm::LLVMTypeKind::LLVMDoubleTypeKind));
        assert!(matches!(
            c.x86_fp80_t().kind(),
            llvm::LLVMTypeKind::LLVMX86_FP80TypeKind
        ));
        assert!(matches!(c.fp128_t().kind(), llvm::LLVMTypeKind::LLVMFP128TypeKind));
        assert!(matches!(
            c.ppc_fp128_t().kind(),
            llvm::LLVMTypeKind::LLVMPPC_FP128TypeKind
        ));
    }

    #[test]
    fn other() {
        let c = Context::new();

        assert_eq!(c.void_t().to_string(), "void");
        assert_eq!(c.label_t().to_string(), "label");
        assert_eq!(c.x86_mmx_t().to_string(), "x86_mmx");

        assert!(matches!(c.void_t().kind(), llvm::LLVMTypeKind::LLVMVoidTypeKind));
        assert!(matches!(c.label_t().kind(), llvm::LLVMTypeKind::LLVMLabelTypeKind));
        assert!(matches!(c.x86_mmx_t().kind(), llvm::LLVMTypeKind::LLVMX86_MMXTypeKind));
    }

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
        let t = c.struct_t(&argts, true);

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
        assert_eq!(t.len(), 8);
        assert_eq!(t.bit_width().unwrap(), 512);
    }
}
