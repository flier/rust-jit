use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::{StructType, TypeRef};
use utils::{from_unchecked_cstr, unchecked_cstring};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

pub trait AsValueRef {
    /// Extracts the raw typedef reference.
    fn as_raw(&self) -> LLVMValueRef;
}

impl<T> AsValueRef for T
where
    T: Deref<Target = ValueRef>,
{
    fn as_raw(&self) -> LLVMValueRef {
        self.deref().as_raw()
    }
}

pub type ValueKind = LLVMValueKind;

impl ValueRef {
    /// Wrap a raw value reference.
    pub fn from_raw(v: LLVMValueRef) -> Self {
        ValueRef(v)
    }

    /// Extracts the raw value reference.
    pub fn as_raw(&self) -> LLVMValueRef {
        self.0
    }

    /// Dump a representation of a value to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.0) }
    }

    /// Obtain the type of a value.
    pub fn type_of(&self) -> TypeRef {
        TypeRef::from_raw(unsafe { LLVMTypeOf(self.0) })
    }

    /// Obtain the enumerated type of a Value instance.
    pub fn kind(&self) -> ValueKind {
        unsafe { LLVMGetValueKind(self.0) }
    }

    /// Obtain the string name of a value.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe {
            let name = LLVMGetValueName(self.0);

            if name.is_null() {
                None
            } else {
                Some(CStr::from_ptr(name).to_string_lossy())
            }
        }
    }

    /// Set the string name of a value.
    pub fn set_name<S: AsRef<str>>(&mut self, name: S) {
        let cname = unchecked_cstring(name);

        unsafe { LLVMSetValueName(self.0, cname.as_ptr()) }
    }

    /// Determine whether the specified value instance is constant.
    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) != 0 }
    }

    /// Determine whether a value instance is undefined.
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.0) != 0 }
    }

    /// Determine whether a value instance is null.
    pub fn is_null(&self) -> bool {
        unsafe { LLVMIsNull(self.0) != 0 }
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let s = LLVMPrintValueToString(self.0);

            let r = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());

            LLVMDisposeMessage(s);

            r
        }
    }
}

pub type Constant = ValueRef;

pub trait Constants {
    /// Obtain a constant value referring to the null instance of a type.
    fn null(&self) -> Constant;

    /// Obtain a constant value referring to an undefined value of a type.
    fn undef(&self) -> Constant;

    /// Obtain a constant that is a constant pointer pointing to NULL for a specified type.
    fn null_ptr(&self) -> Constant;
}

impl Constants for TypeRef {
    fn null(&self) -> Constant {
        Constant::from_raw(unsafe { LLVMConstNull(self.as_raw()) })
    }

    fn undef(&self) -> Constant {
        Constant::from_raw(unsafe { LLVMGetUndef(self.as_raw()) })
    }

    fn null_ptr(&self) -> Constant {
        Constant::from_raw(unsafe { LLVMConstPointerNull(self.as_raw()) })
    }
}

pub type ConstantInt = Constant;

impl ConstantInt {
    /// Obtain the zero extended value for an integer constant value.
    pub fn zero_extended(&self) -> u64 {
        unsafe { LLVMConstIntGetZExtValue(self.as_raw()) }
    }

    /// Obtain the sign extended value for an integer constant value.
    pub fn sign_extended(&self) -> i64 {
        unsafe { LLVMConstIntGetSExtValue(self.as_raw()) }
    }
}

pub trait ConstantInts {
    /// Obtain a constant value for an integer type.
    fn int(&self, n: u64, sign: bool) -> ConstantInt;

    /// Obtain a constant value for an integer of arbitrary precision.
    fn int_with_precision(&self, words: &[u64]) -> ConstantInt;

    /// Obtain a constant value for an integer parsed from a string.
    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt;
}

impl ConstantInts for TypeRef {
    fn int(&self, n: u64, sign: bool) -> ConstantInt {
        ConstantInt::from_raw(unsafe {
            LLVMConstInt(self.as_raw(), n, if sign { 1 } else { 0 })
        })
    }

    fn int_with_precision(&self, words: &[u64]) -> ConstantInt {
        ConstantInt::from_raw(unsafe {
            LLVMConstIntOfArbitraryPrecision(self.as_raw(), words.len() as u32, words.as_ptr())
        })
    }

    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt {
        let len = s.as_ref().len();
        let s = unchecked_cstring(s);

        ConstantInt::from_raw(unsafe {
            LLVMConstIntOfStringAndSize(self.as_raw(), s.as_ptr(), len as u32, radix)
        })
    }
}

pub type ConstantFP = Constant;

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

impl ConstantFPs for TypeRef {
    fn real(&self, n: f64) -> ConstantFP {
        Constant::from_raw(unsafe { LLVMConstReal(self.as_raw(), n) })
    }

    fn real_of_string<S: AsRef<str>>(&self, s: S) -> ConstantFP {
        let len = s.as_ref().len();
        let s = unchecked_cstring(s);

        Constant::from_raw(unsafe {
            LLVMConstRealOfStringAndSize(self.as_raw(), s.as_ptr(), len as u32)
        })
    }
}

pub type ConstantString = Constant;

impl ConstantString {
    /// Create a ConstantString with string content in the global context.
    pub fn str<S: AsRef<str>>(s: S) -> ConstantString {
        let len = s.as_ref().len();
        let buf = unchecked_cstring(s);

        ConstantString::from_raw(unsafe { LLVMConstString(buf.as_ptr(), len as u32, 0) })
    }

    /// Returns true if the specified constant is an array of i8.
    pub fn is_const_str(&self) -> bool {
        unsafe { LLVMIsConstantString(self.as_raw()) != 0 }
    }

    /// Get the given constant data sequential as a string.
    pub fn as_str(&self) -> Option<Cow<str>> {
        let mut len = 0;

        let p = unsafe { LLVMGetAsString(self.as_raw(), &mut len) };

        if p.is_null() {
            None
        } else {
            Some(from_unchecked_cstr(p as *const u8, len))
        }
    }
}

pub trait ConstantStrings {
    /// Create a ConstantString and initialize it with a string.
    fn str<S: AsRef<str>>(&self, s: S) -> ConstantString;
}

impl ConstantStrings for Context {
    fn str<S: AsRef<str>>(&self, s: S) -> ConstantString {
        let len = s.as_ref().len();
        let buf = unchecked_cstring(s);

        ConstantString::from_raw(unsafe {
            LLVMConstStringInContext(self.as_raw(), buf.as_ptr(), len as u32, 0)
        })
    }
}

pub type ConstantStruct = Constant;

impl ConstantStruct {
    /// Create a ConstantStruct in the global Context.
    pub fn structure(values: &[ValueRef], packed: bool) -> Self {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let t = unsafe {
            LLVMConstStruct(
                values.as_mut_ptr(),
                values.len() as u32,
                if packed { 1 } else { 0 },
            )
        };

        ConstantStruct::from_raw(t)
    }
}

pub trait ConstantStructs {
    /// Create a non-anonymous ConstantStruct from values.
    fn structure(&self, values: &[ValueRef]) -> ConstantStruct;
}

impl ConstantStructs for StructType {
    fn structure(&self, values: &[ValueRef]) -> ConstantStruct {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let t = unsafe {
            LLVMConstNamedStruct(self.as_raw(), values.as_mut_ptr(), values.len() as u32)
        };

        ConstantStruct::from_raw(t)
    }
}

pub type ConstantDataSequential = Constant;

impl ConstantDataSequential {
    /// Get an element at specified index as a constant.
    pub fn element(&self, index: usize) -> Option<Constant> {
        let element = unsafe { LLVMGetElementAsConstant(self.as_raw(), index as u32) };

        if element.is_null() {
            None
        } else {
            Some(Constant::from_raw(element))
        }
    }
}

pub type ConstantArray = ConstantDataSequential;

impl ConstantArray {
    /// Create a ConstantArray from values.
    pub fn array(element_type: TypeRef, values: &[ValueRef]) -> Self {
        element_type.array(values)
    }
}

pub trait ConstantArrays {
    /// Create a ConstantArray from values.
    fn array(&self, values: &[ValueRef]) -> ConstantArray;
}

impl ConstantArrays for TypeRef {
    fn array(&self, values: &[ValueRef]) -> ConstantArray {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let t = unsafe { LLVMConstArray(self.as_raw(), values.as_mut_ptr(), values.len() as u32) };

        ConstantArray::from_raw(t)
    }
}

pub type ConstantVector = ConstantDataSequential;

impl ConstantVector {
    /// Create a ConstantVector from values.
    pub fn vector(values: &[ValueRef]) -> Self {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let t = unsafe { LLVMConstVector(values.as_mut_ptr(), values.len() as u32) };

        ConstantVector::from_raw(t)
    }
}

pub type Function = ValueRef;

impl Function {
    pub fn append_basic_block<S: AsRef<str>>(&self, context: &Context, name: S) -> BasicBlock {
        let cname = unchecked_cstring(name);
        let block =
            unsafe { LLVMAppendBasicBlockInContext(context.as_raw(), self.0, cname.as_ptr()) };

        trace!(
            "{:?} create `{}` BasicBlock({:?}) in {:?}",
            self,
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

    pub fn param(&self, index: u32) -> Option<ValueRef> {
        let count = unsafe { LLVMCountParams(self.0) };

        if index >= count {
            None
        } else {
            let param = unsafe { LLVMGetParam(self.0, index) };

            if param.is_null() {
                None
            } else {
                Some(ValueRef(param))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use hamcrest::prelude::*;
    use llvm;

    use super::*;
    use context::Context;
    use types::*;

    #[test]
    fn null() {
        let i64t = IntegerType::int64();
        let v = i64t.null();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.type_of(), i64t);
        assert_eq!(v.to_string(), "i64 0");
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantIntValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(v.is_null());
    }

    #[test]
    fn undef() {
        let i64t = IntegerType::int64();
        let v = i64t.undef();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.type_of(), i64t);
        assert_eq!(v.to_string(), "i64 undef");
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMUndefValueValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn null_ptr() {
        let i64t = IntegerType::int64();
        let v = i64t.null_ptr();

        assert!(!v.as_raw().is_null());
        assert_eq!(v.type_of(), i64t);
        assert_eq!(v.to_string(), "i64 null");
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantPointerNullValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(v.is_null());
    }

    #[test]
    fn int() {
        let i32t = IntegerType::int32();
        let v = i32t.int(unsafe { mem::transmute(-123i64) }, true);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.type_of(), i32t);
        assert_eq!(v.to_string(), "i32 -123");
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantIntValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_eq!(v.zero_extended(), 0xffffff85);
        assert_eq!(v.sign_extended(), -123);
    }

    #[test]
    fn floating_point() {
        let f32t = FloatingPointType::float();
        let v = f32t.real(unsafe { mem::transmute(-123f64) });

        assert!(!v.as_raw().is_null());
        assert_eq!(v.type_of(), f32t);
        assert_eq!(v.to_string(), "float -1.230000e+02");
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantFPValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_that!(v.as_double(), is(close_to(-123f64, f64::EPSILON)));
    }

    #[test]
    fn string_in_global_context() {
        let v = ConstantString::str("hello");

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"[6 x i8] c"hello\00""#);
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantDataArrayValueKind
        ));
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
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantDataArrayValueKind
        ));
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
        let v = ConstantStruct::structure(&[c.int64().int(123, false), c.str("hello")], true);

        assert!(!v.as_raw().is_null());
        assert_eq!(
            v.to_string(),
            r#"<{ i64, [6 x i8] }> <{ i64 123, [6 x i8] c"hello\00" }>"#
        );
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantStructValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn array() {
        let c = Context::new();
        let i64t = c.int64();
        let v = i64t.array(&[i64t.int(123, false), i64t.int(456, false)]);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"[2 x i64] [i64 123, i64 456]"#);
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantDataArrayValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());
    }

    #[test]
    fn vector() {
        let c = Context::new();
        let i64t = c.int64();
        let v = ConstantVector::vector(&[i64t.int(123, false), i64t.int(456, false)]);

        assert!(!v.as_raw().is_null());
        assert_eq!(v.to_string(), r#"<2 x i64> <i64 123, i64 456>"#);
        assert!(matches!(
            v.kind(),
            llvm::LLVMValueKind::LLVMConstantDataVectorValueKind
        ));
        assert_eq!(v.name(), Some("".into()));
        assert!(v.is_constant());
        assert!(!v.is_undef());
        assert!(!v.is_null());

        assert_eq!(v.element(0), Some(i64t.int(123, false)));
        assert_eq!(v.element(1), Some(i64t.int(456, false)));
    }
}
