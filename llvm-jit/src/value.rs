use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::mem;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::TypeRef;
use utils::unchecked_cstring;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

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
pub type ConstantInt = Constant;
pub type ConstantFP = Constant;

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

impl ConstantFP {
    /// Obtain the double value for an floating point constant value.
    pub fn as_double(&self) -> f64 {
        let mut loses = unsafe { mem::zeroed() };

        unsafe { LLVMConstRealGetDouble(self.as_raw(), &mut loses) }
    }
}

pub trait Constants {
    /// Obtain a constant value referring to the null instance of a type.
    fn null(&self) -> Constant;

    /// Obtain a constant value referring to an undefined value of a type.
    fn undef(&self) -> Constant;

    /// Obtain a constant that is a constant pointer pointing to NULL for a specified type.
    fn null_ptr(&self) -> Constant;
}

pub trait ConstantInts {
    /// Obtain a constant value for an integer type.
    fn int(&self, n: u64, sign: bool) -> ConstantInt;

    /// Obtain a constant value for an integer of arbitrary precision.
    fn int_with_precision(&self, words: &[u64]) -> ConstantInt;

    /// Obtain a constant value for an integer parsed from a string.
    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt;
}

pub trait ConstantFPs {
    /// Obtain a constant value referring to a double floating point value.
    fn real(&self, n: f64) -> ConstantFP;

    /// Obtain a constant for a floating point value parsed from a string.
    fn real_of_string<S: AsRef<str>>(&self, s: S) -> ConstantFP;
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
}
