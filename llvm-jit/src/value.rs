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

/// Represents an individual value in LLVM IR.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValueRef(LLVMValueRef);

inherit_from!(ValueRef, LLVMValueRef);

macro_rules! inherit_value_ref {
    ($ty:ident) => {
        inherit_from!($ty, ValueRef, LLVMValueRef);
    }
}

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
    /// Dump a representation of a value to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.0) }
    }

    /// Obtain the type of a value.
    pub fn type_of(&self) -> TypeRef {
        unsafe { LLVMTypeOf(self.0) }.into()
    }

    /// Obtain the enumerated type of a Value instance.
    pub fn kind(&self) -> ValueKind {
        unsafe { LLVMGetValueKind(self.0) }
    }

    /// Obtain the string name of a value.
    pub fn name(&self) -> Option<Cow<str>> {
        unsafe {
            LLVMGetValueName(self.0).as_ref().map(|name| {
                CStr::from_ptr(name).to_string_lossy()
            })
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

    /// Determine whether an LLVMValueRef is itself a basic block.
    pub fn is_basic_block(&self) -> bool {
        unsafe { LLVMValueIsBasicBlock(self.0) != 0 }
    }

    /// Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
    pub fn as_basic_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMValueAsBasicBlock(self.0).as_mut() }.map(|block| BasicBlock::from_raw(block))
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
        unsafe { LLVMConstNull(self.as_raw()) }.into()
    }

    fn undef(&self) -> Constant {
        unsafe { LLVMGetUndef(self.as_raw()) }.into()
    }

    fn null_ptr(&self) -> Constant {
        unsafe { LLVMConstPointerNull(self.as_raw()) }.into()
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

impl ConstantInts for TypeRef {
    fn int_value(&self, n: u64, sign: bool) -> ConstantInt {
        unsafe { LLVMConstInt(self.as_raw(), n, if sign { 1 } else { 0 }) }.into()
    }

    fn int_with_precision(&self, words: &[u64]) -> ConstantInt {
        unsafe {
            LLVMConstIntOfArbitraryPrecision(self.as_raw(), words.len() as u32, words.as_ptr())
        }.into()
    }

    fn int_of_string<S: AsRef<str>>(&self, s: S, radix: u8) -> ConstantInt {
        let len = s.as_ref().len();
        let s = unchecked_cstring(s);

        unsafe { LLVMConstIntOfStringAndSize(self.as_raw(), s.as_ptr(), len as u32, radix) }.into()
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
        unsafe { LLVMConstReal(self.as_raw(), n) }.into()
    }

    fn real_of_string<S: AsRef<str>>(&self, s: S) -> ConstantFP {
        let len = s.as_ref().len();
        let s = unchecked_cstring(s);

        unsafe { LLVMConstRealOfStringAndSize(self.as_raw(), s.as_ptr(), len as u32) }.into()
    }
}

pub type ConstantString = Constant;

impl ConstantString {
    /// Create a ConstantString with string content in the global context.
    pub fn str<S: AsRef<str>>(s: S) -> ConstantString {
        let len = s.as_ref().len();
        let buf = unchecked_cstring(s);

        unsafe { LLVMConstString(buf.as_ptr(), len as u32, 0) }.into()
    }

    /// Returns true if the specified constant is an array of i8.
    pub fn is_const_str(&self) -> bool {
        unsafe { LLVMIsConstantString(self.as_raw()) != 0 }
    }

    /// Get the given constant data sequential as a string.
    pub fn as_str(&self) -> Option<Cow<str>> {
        let mut len = 0;

        unsafe {
            LLVMGetAsString(self.as_raw(), &mut len).as_ref().map(|p| {
                from_unchecked_cstr(p as *const i8 as *const u8, len)
            })
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

        unsafe { LLVMConstStringInContext(self.as_raw(), buf.as_ptr(), len as u32, 0) }.into()
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

        let ty = unsafe {
            LLVMConstStruct(
                values.as_mut_ptr(),
                values.len() as u32,
                if packed { 1 } else { 0 },
            )
        }.into();

        trace!("create constant struct: {:?}", ty);

        ty
    }
}

pub trait ToConstantStruct {
    /// Create a non-anonymous ConstantStruct from values.
    fn struct_of(&self, values: &[ValueRef]) -> ConstantStruct;
}

impl ToConstantStruct for StructType {
    fn struct_of(&self, values: &[ValueRef]) -> ConstantStruct {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let ty = unsafe {
            LLVMConstNamedStruct(self.as_raw(), values.as_mut_ptr(), values.len() as u32)
        }.into();

        trace!("create constant struct: {:?}", ty);

        ty
    }
}

pub trait ConstantDataSequential: AsValueRef {
    /// Get an element at specified index as a constant.
    fn element(&self, index: usize) -> Option<Constant> {
        unsafe { LLVMGetElementAsConstant(self.as_raw(), index as u32).as_mut() }
            .map(|element| Constant::from_raw(element))
    }
}

/// Constant Array Declarations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantArray(ValueRef);

inherit_value_ref!(ConstantArray);

impl ConstantDataSequential for ConstantArray {}

pub trait ToConstantArray {
    /// Create a ConstantArray from values.
    fn array_of(&self, values: &[ValueRef]) -> ConstantArray;
}

impl ToConstantArray for TypeRef {
    fn array_of(&self, values: &[ValueRef]) -> ConstantArray {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let ty = unsafe { LLVMConstArray(self.as_raw(), values.as_mut_ptr(), values.len() as u32) }
            .into();

        trace!("create constant array: {:?}", ty);

        ty
    }
}

/// Constant Vector Declarations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConstantVector(ValueRef);

inherit_value_ref!(ConstantVector);

impl ConstantDataSequential for ConstantVector {}

impl ConstantVector {
    /// Create a ConstantVector from values.
    pub fn new(values: &[ValueRef]) -> ConstantVector {
        let mut values = values
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

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
    [$( $dest:expr ),*] => (
        $crate::ConstantVector::new(&[$( $dest.into() ),*])
    )
}

/// Functions in this group operate on `ValueRef` instances that correspond to `Function` instances.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Function(ValueRef);

inherit_value_ref!(Function);

impl Function {
    /// Obtain an iterator to the basic blocks in a function.
    pub fn basic_blocks(&self) -> BasicBlockIter {
        BasicBlockIter::new(self.as_raw())
    }

    /// Obtain all of the basic blocks in a function.
    pub fn get_basic_blocks(&self) -> Vec<BasicBlock> {
        let count = unsafe { LLVMCountBasicBlocks(self.as_raw()) };
        let mut blocks: Vec<LLVMBasicBlockRef> = vec![ptr::null_mut(); count as usize];

        unsafe { LLVMGetBasicBlocks(self.as_raw(), blocks.as_mut_ptr()) };

        blocks.into_iter().map(BasicBlock::from_raw).collect()
    }

    /// Obtain the basic block that corresponds to the entry point of a function.
    pub fn entry(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetEntryBasicBlock(self.as_raw()).as_mut() }
            .map(|entry| BasicBlock::from_raw(entry))
    }

    /// Append a basic block to the end of a function using the global context.
    pub fn append_basic_block<S: AsRef<str>>(&self, name: S) -> BasicBlock {
        let cname = unchecked_cstring(name);
        let block = unsafe { LLVMAppendBasicBlock(self.as_raw(), cname.as_ptr()) }.into();

        trace!(
            "{:?} create `{}` {:?} in the global context",
            self,
            cname.to_string_lossy(),
            block
        );

        block
    }

    /// Append a basic block to the end of a function.
    pub fn append_basic_block_in_context<S: AsRef<str>>(
        &self,
        name: S,
        context: &Context,
    ) -> BasicBlock {
        let cname = unchecked_cstring(name);
        let block = unsafe {
            LLVMAppendBasicBlockInContext(context.as_raw(), self.as_raw(), cname.as_ptr())
        }.into();

        trace!(
            "{:?} create `{}` {:?} in {:?}",
            self,
            cname.to_string_lossy(),
            block,
            context
        );

        block
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

        params.into_iter().map(ValueRef).collect()
    }

    /// Obtain the parameter at the specified index.
    pub fn get_param(&self, index: u32) -> Option<ValueRef> {
        let count = unsafe { LLVMCountParams(self.as_raw()) };

        if index >= count {
            None
        } else {
            unsafe { LLVMGetParam(self.as_raw(), index).as_mut() }.map(
                |param| ValueRef::from_raw(param),
            )
        }
    }

    /// Remove a function from its containing module and deletes it.
    pub fn delete(self) {
        unsafe { LLVMDeleteFunction(self.as_raw()) }
    }
}

impl_iter!(
    BasicBlockIter,
    LLVMGetFirstBasicBlock | LLVMGetLastBasicBlock[LLVMValueRef],
    LLVMGetNextBasicBlock | LLVMGetPreviousBasicBlock[LLVMBasicBlockRef],
    BasicBlock::from_raw
);

impl_iter!(
    ParamIter,
    LLVMGetFirstParam | LLVMGetLastParam[LLVMValueRef],
    LLVMGetNextParam | LLVMGetPreviousParam[LLVMValueRef],
    ValueRef::from_raw
);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlockAddress(ValueRef);

inherit_value_ref!(BlockAddress);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction(ValueRef);

inherit_value_ref!(Instruction);

impl Instruction {
    /// Obtain the code opcode for an individual instruction.
    pub fn opcode(&self) -> LLVMOpcode {
        unsafe { LLVMGetInstructionOpcode(self.as_raw()) }
    }

    /// Create a copy of 'this' instruction that is identical in all ways except the following:
    ///   * The instruction has no parent
    ///   * The instruction has no name
    pub fn copy(&self) -> Self {
        unsafe { LLVMInstructionClone(self.as_raw()) }.into()
    }

    /// Obtain the basic block to which an instruction belongs.
    pub fn parent(&self) -> BasicBlock {
        unsafe { LLVMGetInstructionParent(self.as_raw()) }.into()
    }

    /// Remove and delete an instruction.
    ///
    /// The instruction specified is removed from its containing building block but is kept alive.
    pub fn remove(&self) {
        unsafe { LLVMInstructionRemoveFromParent(self.as_raw()) }
    }

    /// Remove and delete an instruction.
    ///
    /// The instruction specified is removed from its containing building block and then deleted.
    pub fn erase(self) {
        unsafe { LLVMInstructionEraseFromParent(self.as_raw()) }
    }

    /// Determine whether an instruction has any metadata attached.
    pub fn has_metadata(&self) -> bool {
        unsafe { LLVMHasMetadata(self.as_raw()) != 0 }
    }

    /// Return metadata associated with an instruction value.
    pub fn get_metadata(&self, kind_id: u32) -> ValueRef {
        unsafe { LLVMGetMetadata(self.as_raw(), kind_id) }.into()
    }

    /// Set metadata associated with an instruction value.
    pub fn set_metadata(&self, kind_id: u32, node: ValueRef) {
        unsafe { LLVMSetMetadata(self.as_raw(), kind_id, node.as_raw()) }
    }

    /// Obtain the instruction that occurs after the one specified.
    pub fn next(&self) -> Option<Instruction> {
        unsafe { LLVMGetNextInstruction(self.as_raw()).as_mut() }.map(|v| Instruction::from_raw(v))
    }

    /// Obtain the instruction that occurred before this one.
    pub fn previous(&self) -> Option<Instruction> {
        unsafe { LLVMGetPreviousInstruction(self.as_raw()).as_mut() }
            .map(|v| Instruction::from_raw(v))
    }
}

pub type ThreadLocalMode = LLVMThreadLocalMode;

/// Global Variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(ValueRef);

inherit_value_ref!(GlobalVar);

impl GlobalVar {
    pub fn delete(&self) {
        unsafe { LLVMDeleteGlobal(self.as_raw()) }
    }

    pub fn initializer(&self) -> Option<ValueRef> {
        unsafe { LLVMGetInitializer(self.as_raw()).as_mut() }.map(|var| ValueRef::from_raw(var))
    }

    pub fn set_initializer(&self, initializer: Constant) {
        unsafe { LLVMSetInitializer(self.as_raw(), initializer.as_raw()) }
    }

    pub fn is_thread_local(&self) -> bool {
        unsafe { LLVMIsThreadLocal(self.as_raw()) != 0 }
    }

    pub fn set_thread_local(&self, is_thread_local: bool) {
        unsafe { LLVMSetThreadLocal(self.as_raw(), if is_thread_local { 1 } else { 0 }) }
    }

    pub fn is_global_constant(&self) -> bool {
        unsafe { LLVMIsGlobalConstant(self.as_raw()) != 0 }
    }

    pub fn set_global_constant(&self, is_global_constant: bool) {
        unsafe { LLVMSetGlobalConstant(self.as_raw(), if is_global_constant { 1 } else { 0 }) }
    }

    pub fn thread_local_mode(&self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_raw()) }
    }

    pub fn set_thread_local_mode(&self, mode: ThreadLocalMode) {
        unsafe { LLVMSetThreadLocalMode(self.as_raw(), mode) }
    }

    pub fn is_externally_initialized(&self) -> bool {
        unsafe { LLVMIsExternallyInitialized(self.as_raw()) != 0 }
    }

    pub fn set_externally_initialized(&self, is_externally_initialized: bool) {
        unsafe {
            LLVMSetExternallyInitialized(
                self.as_raw(),
                if is_externally_initialized { 1 } else { 0 },
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use hamcrest::prelude::*;
    use llvm;

    use super::*;
    use context::{Context, GlobalContext};
    use module::Module;
    use types::*;

    #[test]
    fn null() {
        let i64t = GlobalContext::int64_t();
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
        let i64t = GlobalContext::int64_t();
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
        let i64t = GlobalContext::int64_t();
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
        let i32t = GlobalContext::int32_t();
        let v = i32t.int(-123);

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
        let f32t = GlobalContext::float_t();
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
        let v = ConstantStruct::structure(&[c.int64_t().int(123), c.str("hello")], true);

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
        let i64t = c.int64_t();
        let v = i64t.array_of(&[i64t.int(123), i64t.int(456)]);

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
        let i64t = c.int64_t();
        let v = vector![i64t.int(123), i64t.int(456)];

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

        assert_eq!(v.element(0), Some(i64t.int(123)));
        assert_eq!(v.element(1), Some(i64t.int(456)));
    }

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = Module::with_name_in_context("test", &c);

        assert_eq!(m.get_global_var("x"), None);

        let i64t = c.int64_t();
        let f64t = c.double_t();

        m.add_global_var("x", i64t);
        m.add_global_var("y", f64t);

        let x = m.get_global_var("x").unwrap();
        let y = m.get_global_var("y").unwrap();

        assert_eq!(m.global_vars().collect::<Vec<GlobalVar>>(), vec![x, y]);

        assert_eq!(x.name().unwrap(), "x");
        assert_eq!(y.name().unwrap(), "y");

        assert_eq!(x.to_string(), "@x = external global i64");

        // set initializer
        assert_eq!(x.initializer(), None);

        let v = i64t.uint(123);

        x.set_initializer(v);

        assert_eq!(x.initializer(), Some(v));

        assert_eq!(x.to_string(), "@x = global i64 123");

        // set as global constant
        assert!(!x.is_global_constant());

        x.set_global_constant(true);

        assert_eq!(x.to_string(), "@x = constant i64 123");

        x.set_global_constant(false);

        // set as externally initialized
        assert!(!x.is_externally_initialized());

        x.set_externally_initialized(true);

        assert_eq!(x.to_string(), "@x = externally_initialized global i64 123");

        x.set_externally_initialized(false);

        // set as thread local
        assert!(!x.is_thread_local());

        x.set_thread_local(true);

        assert!(x.is_thread_local());

        assert_eq!(x.to_string(), "@x = thread_local global i64 123");

        x.set_thread_local(false);

        // set thread local mod
        assert!(matches!(
            x.thread_local_mode(),
            llvm::LLVMThreadLocalMode::LLVMNotThreadLocal
        ));
    }
}
