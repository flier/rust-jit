use std::borrow::Cow;
use std::mem;
use std::slice;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::LLVMOpcode;

use crate::context::Context;
use crate::types::{FloatingPointType, FloatingPointTypes, IntegerType, IntegerTypes, StructType, TypeRef};
use crate::utils::{unchecked_cstr, AsBool, AsLLVMBool, AsRaw, AsResult};
use crate::value::{AsValueRef, ValueRef};

pub trait AsConstant: AsValueRef {
    fn as_const(&self) -> &Constant;
}

macro_rules! impl_constant {
    ($ty: ident) => {
        inherit_value_ref!($ty);

        impl AsConstant for $ty {
            fn as_const(&self) -> &Constant {
                &self
            }
        }
    };
    ($ty: ident, $parent: ty) => {
        inherit_value_ref!($ty, $parent);

        impl AsConstant for $ty {
            fn as_const(&self) -> &Constant {
                &self.0
            }
        }
    };
}

impl ValueRef {
    pub fn as_constant_fp(&self) -> Option<ConstantFP> {
        unsafe { LLVMIsAConstantFP(self.as_raw()) }.ok()
    }

    pub fn as_constant_int(&self) -> Option<ConstantInt> {
        unsafe { LLVMIsAConstantInt(self.as_raw()) }.ok()
    }

    pub fn as_constant_array(&self) -> Option<ConstantArray> {
        unsafe { LLVMIsAConstantArray(self.as_raw()) }.ok()
    }

    pub fn as_constant_vector(&self) -> Option<ConstantVector> {
        unsafe { LLVMIsAConstantVector(self.as_raw()) }.ok()
    }

    pub fn as_constant_struct(&self) -> Option<ConstantStruct> {
        unsafe { LLVMIsAConstantStruct(self.as_raw()) }.ok()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Constant(ValueRef);

impl_constant!(Constant);

impl Constant {
    /// Obtain the code opcode for the constant.
    pub fn opcode(&self) -> LLVMOpcode {
        unsafe { LLVMGetConstOpcode(self.as_raw()) }
    }
}

#[macro_export]
macro_rules! constants {
    ($($x:expr),*) => (&[ $( $x.into() ),* ]);
    ($($x:expr,)*) => (&[ $( $x.into() ),* ]);
}

pub trait Constants {
    /// Obtain a constant value referring to the null instance of a type.
    fn null(&self) -> Constant;

    /// Obtain a constant value referring to the instance of a type consisting of all ones.
    fn all_ones(&self) -> Constant;

    /// Obtain a constant value referring to an undefined value of a type.
    fn undef(&self) -> Constant;

    /// Obtain a constant that is a constant pointer pointing to NULL for a specified type.
    fn null_ptr(&self) -> Constant;
}

impl Constants for TypeRef {
    fn null(&self) -> Constant {
        unsafe { LLVMConstNull(self.as_raw()) }.into()
    }

    fn all_ones(&self) -> Constant {
        unsafe { LLVMConstAllOnes(self.as_raw()) }.into()
    }

    fn undef(&self) -> Constant {
        unsafe { LLVMGetUndef(self.as_raw()) }.into()
    }

    fn null_ptr(&self) -> Constant {
        unsafe { LLVMConstPointerNull(self.as_raw()) }.into()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantInt(Constant);

impl_constant!(ConstantInt, Constant);

impl ConstantInt {
    /// Obtain the zero extended value for an integer constant value.
    pub fn uint(&self) -> u64 {
        unsafe { LLVMConstIntGetZExtValue(self.as_raw()) }
    }

    /// Obtain the sign extended value for an integer constant value.
    pub fn int(&self) -> i64 {
        unsafe { LLVMConstIntGetSExtValue(self.as_raw()) }
    }

    pub fn bool(&self) -> bool {
        self.int() != 0
    }
}

pub trait ConstantInts {
    /// Obtain a constant value for an integer type.
    fn int_value(&self, n: u64, sign: bool) -> ConstantInt;

    /// Obtain a constant value for an integer of arbitrary precision.
    fn int_with_precision(&self, words: &[u64]) -> ConstantInt;

    /// Obtain a constant value for an integer parsed from a string.
    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt;

    /// Obtain a constant value for an unsigned integer type.
    fn uint(&self, n: u64) -> ConstantInt {
        self.int_value(n, false)
    }

    /// Obtain a constant value for an signed integer type.
    fn int(&self, n: i64) -> ConstantInt {
        self.int_value(unsafe { mem::transmute(n) }, true)
    }
}

impl ConstantInts for IntegerType {
    fn int_value(&self, n: u64, sign: bool) -> ConstantInt {
        unsafe { LLVMConstInt(self.as_raw(), n, sign.as_bool()) }.into()
    }

    fn int_with_precision(&self, words: &[u64]) -> ConstantInt {
        unsafe { LLVMConstIntOfArbitraryPrecision(self.as_raw(), words.len() as u32, words.as_ptr()) }.into()
    }

    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt {
        let s = s.as_ref();

        unsafe { LLVMConstIntOfStringAndSize(self.as_raw(), s.as_ptr() as *const _, s.len() as u32, radix) }.into()
    }
}

impl Context {
    pub fn uint8<V: Into<u8>>(&self, v: V) -> ConstantInt {
        self.int8_t().uint(u64::from(v.into()))
    }

    pub fn uint16<V: Into<u16>>(&self, v: V) -> ConstantInt {
        self.int16_t().uint(u64::from(v.into()))
    }

    pub fn uint32<V: Into<u32>>(&self, v: V) -> ConstantInt {
        self.int32_t().uint(u64::from(v.into()))
    }

    pub fn uint64<V: Into<u64>>(&self, v: V) -> ConstantInt {
        self.int64_t().uint(v.into())
    }

    pub fn uint128<V: Into<u128>>(&self, v: V) -> ConstantInt {
        let v: u128 = v.into();

        self.int128_t()
            .int_with_precision(unsafe { slice::from_raw_parts(&v as *const _ as *const u64, 2) })
    }

    pub fn int8<V: Into<i8>>(&self, v: V) -> ConstantInt {
        self.int8_t().int(i64::from(v.into()))
    }

    pub fn int16<V: Into<i16>>(&self, v: V) -> ConstantInt {
        self.int16_t().int(i64::from(v.into()))
    }

    pub fn int32<V: Into<i32>>(&self, v: V) -> ConstantInt {
        self.int32_t().int(i64::from(v.into()))
    }

    pub fn int64<V: Into<i64>>(&self, v: V) -> ConstantInt {
        self.int64_t().int(v.into())
    }

    pub fn int128<V: Into<i128>>(&self, v: V) -> ConstantInt {
        let v: i128 = v.into();

        self.int128_t()
            .int_with_precision(unsafe { slice::from_raw_parts(&v as *const _ as *const u64, 2) })
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantFP(Constant);

impl_constant!(ConstantFP, Constant);

impl ConstantFP {
    /// Obtain the double value for an floating point constant value.
    pub fn as_double(&self) -> f64 {
        let mut loses = unsafe { mem::zeroed() };

        unsafe { LLVMConstRealGetDouble(self.as_raw(), &mut loses) }
    }
}

pub trait ConstantFPs {
    /// Obtain a constant value referring to a double floating point value.
    fn real(&self, n: f64) -> ConstantFP;

    /// Obtain a constant for a floating point value parsed from a string.
    fn real_of_string<S: AsRef<str>>(&self, s: S) -> ConstantFP;
}

impl ConstantFPs for FloatingPointType {
    fn real(&self, n: f64) -> ConstantFP {
        unsafe { LLVMConstReal(self.as_raw(), n) }.into()
    }

    fn real_of_string<S: AsRef<str>>(&self, s: S) -> ConstantFP {
        let s = s.as_ref();

        unsafe { LLVMConstRealOfStringAndSize(self.as_raw(), s.as_ptr() as *const _, s.len() as u32) }.into()
    }
}

impl Context {
    pub fn half<V: Into<f32>>(&self, v: V) -> ConstantFP {
        self.half_t().real(f64::from(v.into()))
    }

    pub fn float<V: Into<f32>>(&self, v: V) -> ConstantFP {
        self.float_t().real(f64::from(v.into()))
    }

    pub fn double<V: Into<f64>>(&self, v: V) -> ConstantFP {
        self.double_t().real(v.into())
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantString(Constant);

impl_constant!(ConstantString, Constant);

impl ConstantString {
    /// Create a ConstantString with string content in the global context.
    pub fn str<S: AsRef<str>>(s: S) -> ConstantString {
        let s = s.as_ref();

        unsafe { LLVMConstString(s.as_ptr() as *const _, s.len() as u32, 0) }.into()
    }

    /// Returns true if the specified constant is an array of i8.
    pub fn is_const_str(&self) -> bool {
        unsafe { LLVMIsConstantString(self.as_raw()) }.as_bool()
    }

    /// Get the given constant data sequential as a string.
    pub fn as_str(&self) -> Option<Cow<str>> {
        let mut len = 0;

        unsafe {
            LLVMGetAsString(self.as_raw(), &mut len)
                .as_ref()
                .map(|p| unchecked_cstr(p as *const i8 as *const u8, len))
        }
    }
}

pub trait ConstantStrings {
    /// Create a ConstantString and initialize it with a string.
    fn str<S: AsRef<str>>(&self, s: S) -> ConstantString;
}

impl ConstantStrings for Context {
    fn str<S: AsRef<str>>(&self, s: S) -> ConstantString {
        let s = s.as_ref();

        unsafe { LLVMConstStringInContext(self.as_raw(), s.as_ptr() as *const _, s.len() as u32, 0) }.into()
    }
}

macro_rules! from_buildin_type {
    ($from:ty => $ty:ident : $to:ty) => {
        impl From<$from> for $to {
            fn from(v: $from) -> Self {
                crate::Context::global().$ty(v)
            }
        }

        impl From<$from> for Constant {
            fn from(v: $from) -> Self {
                crate::Context::global().$ty(v).into()
            }
        }

        impl From<$from> for ValueRef {
            fn from(v: $from) -> Self {
                crate::Context::global().$ty(v).into()
            }
        }
    };
}

from_buildin_type!(u8 => uint8: ConstantInt);
from_buildin_type!(u16 => uint16: ConstantInt);
from_buildin_type!(u32 => uint32: ConstantInt);
from_buildin_type!(u64 => uint64: ConstantInt);
from_buildin_type!(u128 => uint128: ConstantInt);
from_buildin_type!(i8 => int8: ConstantInt);
from_buildin_type!(i16 => int16: ConstantInt);
from_buildin_type!(i32 => int32: ConstantInt);
from_buildin_type!(i64 => int64: ConstantInt);
from_buildin_type!(i128 => int128: ConstantInt);
from_buildin_type!(f32 => float: ConstantFP);
from_buildin_type!(f64 => double: ConstantFP);
from_buildin_type!(&str => str: ConstantString);

#[macro_export]
macro_rules! struct_of {
    ( | $context:ident | $( $value:expr ),* ; packed ) => {
        $context.struct_of(constants!{ $( $value ),* }, true)
    };
    ( | $context:ident | $( $value:expr ),* ) => {
        $context.struct_of(constants!{ $( $value ),* }, false)
    };
    ( $( $value:expr ),* ; packed ) => {
        $crate::ConstantStruct::structure(constants!{ $( $value ),* }, true)
    };
    ( $( $value:expr ),* ) => {
        $crate::ConstantStruct::structure(constants!{ $( $value ),* }, false)
    };
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantStruct(Constant);

impl_constant!(ConstantStruct, Constant);

impl ConstantStruct {
    /// Create a ConstantStruct in the global Context.
    pub fn structure(values: &[ValueRef], packed: bool) -> Self {
        let mut values = values.iter().map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        let ty = unsafe { LLVMConstStruct(values.as_mut_ptr(), values.len() as u32, packed.as_bool()) }.into();

        trace!("create constant struct: {:?}", ty);

        ty
    }
}

pub trait ToNamedConstantStruct {
    /// Create a non-anonymous ConstantStruct from values.
    fn struct_of(&self, values: &[ValueRef]) -> ConstantStruct;
}

impl ToNamedConstantStruct for StructType {
    fn struct_of(&self, values: &[ValueRef]) -> ConstantStruct {
        let mut values = values.iter().map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        let ty = unsafe { LLVMConstNamedStruct(self.as_raw(), values.as_mut_ptr(), values.len() as u32) }.into();

        trace!("create constant struct: {:?}", ty);

        ty
    }
}

pub trait ToConstantStruct {
    /// Create a non-anonymous ConstantStruct from values.
    fn struct_of(&self, values: &[ValueRef], packed: bool) -> ConstantStruct;
}

impl ToConstantStruct for Context {
    fn struct_of(&self, values: &[ValueRef], packed: bool) -> ConstantStruct {
        let mut values = values.iter().map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        let ty = unsafe {
            LLVMConstStructInContext(
                self.as_raw(),
                values.as_mut_ptr(),
                values.len() as u32,
                packed.as_bool(),
            )
        }
        .into();

        trace!("create constant struct: {:?}", ty);

        ty
    }
}

pub trait ConstantDataSequential: AsValueRef {
    /// Get an element at specified index as a constant.
    fn element(&self, index: usize) -> Option<Constant> {
        unsafe { LLVMGetElementAsConstant(self.as_raw(), index as u32) }.ok()
    }
}

impl ConstantDataSequential for ConstantArray {}
impl ConstantDataSequential for ConstantVector {}

#[macro_export]
macro_rules! array_of {
    ( $ty:ident [ $( $value:expr ),* ] ) => {
        values!{ $( $value ),* }
    };
}

/// Constant Array Declarations.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantArray(Constant);

impl_constant!(ConstantArray, Constant);

pub trait ToConstantArray {
    /// Create a ConstantArray from values.
    fn array_of(&self, values: &[ValueRef]) -> ConstantArray;
}

impl ToConstantArray for TypeRef {
    fn array_of(&self, values: &[ValueRef]) -> ConstantArray {
        let mut values = values.iter().map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        let ty = unsafe { LLVMConstArray(self.as_raw(), values.as_mut_ptr(), values.len() as u32) }.into();

        trace!("create constant array: {:?}", ty);

        ty
    }
}

/// Constant Vector Declarations.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantVector(Constant);

impl_constant!(ConstantVector, Constant);

impl ConstantVector {
    /// Create a ConstantVector from values.
    pub fn new(values: &[ValueRef]) -> ConstantVector {
        let mut values = values.iter().map(|v| v.as_raw()).collect::<Vec<LLVMValueRef>>();

        let ty = unsafe { LLVMConstVector(values.as_mut_ptr(), values.len() as u32) }.into();

        trace!("create constant vector: {:?}", ty);

        ty
    }
}

pub trait ToConstantVector {
    /// Create a ConstantVector from values.
    fn vector_of(&self, values: &[ValueRef]) -> ConstantVector;
}

impl ToConstantVector for TypeRef {
    fn vector_of(&self, values: &[ValueRef]) -> ConstantVector {
        ConstantVector::new(values)
    }
}

/// A vector type is a simple derived type that represents a vector of elements.
#[macro_export]
macro_rules! vector {
    [$( $value:expr ),*] => (
        $crate::ConstantVector::new(&[$( $value.into() ),*])
    )
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InlineAsm(Constant);

impl_constant!(InlineAsm, Constant);

#[cfg(test)]
mod tests {
    use std::f64;

    use crate::llvm;
    use hamcrest::prelude::*;

    use super::*;
    use crate::context::Context;
    use crate::types::*;

    #[test]
    fn null() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let v = i64_t.null();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.ty(), i64_t);
        assert_eq!(v.to_string(), "i64 0");
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantIntValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(v.is_null());
    }

    #[test]
    fn undef() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let v = i64_t.undef();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.ty(), i64_t);
        assert_eq!(v.to_string(), "i64 undef");
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMUndefValueValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn null_ptr() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let i64_ptr_t = i64_t.ptr_t();
        let v = i64_ptr_t.null_ptr();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.ty(), i64_ptr_t.into());
        assert_eq!(v.to_string(), "i64* null");
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantPointerNullValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(v.is_null());
    }

    #[test]
    fn int() {
        let c = Context::new();
        let i32_t = c.int32_t();
        let v = i32_t.int(-123);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.ty(), i32_t);
        assert_eq!(v.to_string(), "i32 -123");
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantIntValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_eq!(v.uint(), 0xffff_ff85);
        assert_eq!(v.int(), -123);
    }

    #[test]
    fn floating_point() {
        let c = Context::new();
        let f32_t = c.float_t();
        let v = f32_t.real(-123.0);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.ty(), f32_t);
        assert_eq!(v.to_string(), "float -1.230000e+02");
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantFPValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_that!(v.as_double(), is(close_to(-123.0, f64::EPSILON)));
    }

    #[test]
    fn string_in_global_context() {
        let c = Context::new();
        let v = c.str("hello");

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"[6 x i8] c"hello\00""#);
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantDataArrayValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert!(v.is_const_str());
        assert_eq!(v.as_str(), Some("hello".into()));
    }

    #[test]
    fn string_in_context() {
        let c = Context::new();
        let v = c.str("hello");

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"[6 x i8] c"hello\00""#);
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantDataArrayValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert!(v.is_const_str());
        assert_eq!(v.as_str(), Some("hello".into()));
    }

    #[test]
    fn structure() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let v = c.struct_of(values![i64_t.int(123), c.str("hello")], true);

        assert!(!v.as_raw().is_null());
        assert_eq!(
            v.to_string(),
            r#"<{ i64, [6 x i8] }> <{ i64 123, [6 x i8] c"hello\00" }>"#
        );
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantStructValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn array() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let v = i64_t.array_of(values![i64_t.int(123), i64_t.int(456)]);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"[2 x i64] [i64 123, i64 456]"#);
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantDataArrayValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn vector() {
        let c = Context::new();
        let i64_t = c.int64_t();
        let v = vector![i64_t.int(123), i64_t.int(456)];

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"<2 x i64> <i64 123, i64 456>"#);
        assert_eq!(v.kind(), llvm::LLVMValueKind::LLVMConstantDataVectorValueKind);
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_eq!(v.element(0), Some(i64_t.int(123).into()));
        assert_eq!(v.element(1), Some(i64_t.int(456).into()));
    }
}
