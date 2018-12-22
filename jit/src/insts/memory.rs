use std::borrow::Cow;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::LLVMAtomicOrdering;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::types::TypeRef;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Malloc<'a> {
    ty: TypeRef,
    size: Option<Box<AstNode<'a>>>,
    name: Cow<'a, str>,
}

impl<'a> Malloc<'a> {
    pub fn new<T, N>(ty: T, name: N) -> Self
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        Malloc {
            ty: ty.into(),
            size: None,
            name: name.into(),
        }
    }
    pub fn array<T, V, N>(ty: T, size: V, name: N) -> Self
    where
        T: Into<TypeRef>,
        V: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Malloc {
            ty: ty.into(),
            size: Some(Box::new(size.into())),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for Malloc<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.emit_to(builder).into_raw(),
                    cstr!(self.name),
                )
            } else {
                LLVMBuildMalloc(builder.as_raw(), self.ty.as_raw(), cstr!(self.name))
            }
        }
        .into()
    }
}

/// Invoke `malloc` function to allocates memory on the heap, need to be expicity released by its caller.
#[macro_export]
macro_rules! malloc {
    ($ty: expr; $name: expr) => {{
        $crate::insts::Malloc::new($ty, $name)
    }};
    ($array_ty: expr, $size: expr; $name: expr) => {{
        $crate::insts::Malloc::array($array_ty, $size, $name)
    }};

    ($ty: expr) => {{
        malloc!($ty ; "malloc")
    }};
    ($array_ty: expr, $size: expr) => {{
        malloc!($array_ty, $size ; "array_malloc")
    }};
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alloca<'a> {
    ty: TypeRef,
    size: Option<Box<AstNode<'a>>>,
    name: Cow<'a, str>,
}

impl<'a> Alloca<'a> {
    pub fn new<T, N>(ty: T, name: N) -> Self
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        Alloca {
            ty: ty.into(),
            size: None,
            name: name.into(),
        }
    }
    pub fn array<T, V, N>(ty: T, size: V, name: N) -> Self
    where
        T: Into<TypeRef>,
        V: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Alloca {
            ty: ty.into(),
            size: Some(Box::new(size.into())),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for Alloca<'a> {
    type Target = AllocaInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.emit_to(builder).into_raw(),
                    cstr!(self.name),
                )
            } else {
                LLVMBuildAlloca(builder.as_raw(), self.ty.as_raw(), cstr!(self.name))
            }
        }
        .into()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AllocaInst(Instruction);

inherit_from!(AllocaInst, Instruction, ValueRef, LLVMValueRef);

impl AllocaInst {
    /// Obtain the type that is being allocated by the alloca instruction.
    pub fn allocated_type(&self) -> TypeRef {
        unsafe { LLVMGetAllocatedType(self.as_raw()) }.into()
    }
}

/// The `alloca` instruction allocates memory on the stack frame of the currently executing function,
/// to be automatically released when this function returns to its caller.
///
/// The object is always allocated in the generic address space (address space zero).
#[macro_export]
macro_rules! alloca {
    ($ty: expr; $name: expr) => {{
        $crate::insts::Alloca::new($ty, $name)
    }};
    ($array_ty: expr, $size: expr; $name: expr) => {{
        $crate::insts::Alloca::array($array_ty, $size, $name)
    }};

    ($ty: expr) => {{
        alloca!($ty ; "alloca")
    }};
    ($array_ty: expr, $size: expr) => {{
        alloca!($array_ty, $size ; "array_alloca")
    }};
}

#[derive(Clone, Debug, PartialEq)]
pub struct Free<'a>(Box<AstNode<'a>>);

impl<'a> Free<'a> {
    pub fn new<V>(ptr: V) -> Self
    where
        V: Into<AstNode<'a>>,
    {
        Free(Box::new(ptr.into()))
    }
}

impl<'a> InstructionBuilder for Free<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildFree(builder.as_raw(), self.0.emit_to(builder).into_raw()) }.into()
    }
}

/// Deallocates the memory allocation pointed to by ptr.
pub fn free<'a, P>(ptr: P) -> Free<'a>
where
    P: Into<AstNode<'a>>,
{
    Free::new(ptr)
}

#[macro_export]
macro_rules! free {
    ($ptr: expr) => {
        $crate::insts::free($ptr)
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Load<'a> {
    ptr: Box<AstNode<'a>>,
    name: Cow<'a, str>,
    volatile: bool,
    ordering: LLVMAtomicOrdering,
}

impl<'a> Load<'a> {
    pub fn new<V, N>(ptr: V, name: N) -> Self
    where
        V: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Load {
            ptr: Box::new(ptr.into()),
            name: name.into(),
            volatile: false,
            ordering: LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic,
        }
    }

    pub fn with_volatile(mut self) -> Self {
        self.volatile = true;
        self
    }

    pub fn with_aotmic_ordering(mut self, ordering: LLVMAtomicOrdering) -> Self {
        self.ordering = ordering;
        self
    }
}

impl<'a> InstructionBuilder for Load<'a> {
    type Target = LoadInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let inst: LoadInst =
            unsafe { LLVMBuildLoad(builder.as_raw(), self.ptr.emit_to(builder).into_raw(), cstr!(self.name)) }.into();

        if self.volatile {
            inst.set_volatile(self.volatile)
        }

        if self.ordering != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic {
            inst.set_atomic_ordering(self.ordering)
        }

        inst
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LoadInst(Instruction);

inherit_from!(LoadInst, Instruction, ValueRef, LLVMValueRef);

/// The `load` instruction is used to read from memory.
pub fn load<'a, P, N>(ptr: P, name: N) -> Load<'a>
where
    P: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    Load::new(ptr, name)
}

#[macro_export]
macro_rules! load {
    ($ptr: expr; $name: expr) => {
        $crate::insts::load($ptr, $name)
    };
    ($ptr: expr) => {
        load!($ptr; "load")
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store<'a> {
    value: Box<AstNode<'a>>,
    ptr: Box<AstNode<'a>>,
    volatile: bool,
    ordering: LLVMAtomicOrdering,
}

impl<'a> Store<'a> {
    pub fn new<V, P>(value: V, ptr: P) -> Self
    where
        V: Into<AstNode<'a>>,
        P: Into<AstNode<'a>>,
    {
        Store {
            value: Box::new(value.into()),
            ptr: Box::new(ptr.into()),
            volatile: false,
            ordering: LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic,
        }
    }

    pub fn with_volatile(mut self) -> Self {
        self.volatile = true;
        self
    }

    pub fn with_aotmic_ordering(mut self, ordering: LLVMAtomicOrdering) -> Self {
        self.ordering = ordering;
        self
    }
}

impl<'a> InstructionBuilder for Store<'a> {
    type Target = StoreInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let inst: StoreInst = unsafe {
            LLVMBuildStore(
                builder.as_raw(),
                self.value.emit_to(builder).into_raw(),
                self.ptr.emit_to(builder).into_raw(),
            )
        }
        .into();

        if self.volatile {
            inst.set_volatile(self.volatile)
        }

        if self.ordering != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic {
            inst.set_atomic_ordering(self.ordering)
        }

        inst
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StoreInst(Instruction);

inherit_from!(StoreInst, Instruction, ValueRef, LLVMValueRef);

/// The `store` instruction is used to write to memory.
pub fn store<'a, V, P>(value: V, ptr: P) -> Store<'a>
where
    V: Into<AstNode<'a>>,
    P: Into<AstNode<'a>>,
{
    Store::new(value, ptr)
}

#[macro_export]
macro_rules! store {
    ($value: expr, $ptr: expr) => {
        $crate::insts::store($value, $ptr)
    };
}

impl IRBuilder {
    /// Invoke `malloc` function to allocates memory on the heap, need to be expicity released by its caller.
    pub fn malloc<'a, T, V, N>(&self, ty: T, size: Option<V>, name: N) -> Instruction
    where
        T: Into<TypeRef>,
        V: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        if let Some(size) = size {
            Malloc::array(ty, size, name)
        } else {
            Malloc::new(ty, name)
        }
        .emit_to(self)
    }

    /// Deallocates the memory allocation pointed to by ptr.
    pub fn free<'a, P>(&self, ptr: P) -> Instruction
    where
        P: Into<AstNode<'a>>,
    {
        free(ptr).emit_to(self)
    }

    /// The `alloca` instruction allocates memory on the stack frame of the currently executing function,
    /// to be automatically released when this function returns to its caller.
    pub fn alloca<'a, T, V, N>(&self, ty: T, size: Option<V>, name: N) -> AllocaInst
    where
        T: Into<TypeRef>,
        V: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        if let Some(size) = size {
            Alloca::array(ty, size, name)
        } else {
            Alloca::new(ty, name)
        }
        .emit_to(self)
    }

    /// The `load` instruction is used to read from memory.
    pub fn load<'a, P, N>(&self, ptr: P, name: N) -> LoadInst
    where
        P: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        load(ptr, name).emit_to(self)
    }

    /// The `store` instruction is used to write to memory.
    pub fn store<'a, V, P>(&self, value: V, ptr: P) -> StoreInst
    where
        V: Into<AstNode<'a>>,
        P: Into<AstNode<'a>>,
    {
        store(value, ptr).emit_to(self)
    }
}

pub trait MemAccessInst: AsRaw<RawType = LLVMValueRef> {
    fn is_volatile(&self) -> bool {
        unsafe { LLVMGetVolatile(self.as_raw()) }.as_bool()
    }

    fn set_volatile(&self, v: bool) {
        unsafe { LLVMSetVolatile(self.as_raw(), v.as_bool()) }
    }

    fn atomic_ordering(&self) -> LLVMAtomicOrdering {
        unsafe { LLVMGetOrdering(self.as_raw()) }
    }

    fn set_atomic_ordering(&self, ordering: LLVMAtomicOrdering) {
        unsafe { LLVMSetOrdering(self.as_raw(), ordering) }
    }
}

impl MemAccessInst for LoadInst {}
impl MemAccessInst for StoreInst {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::insts::*;
    use crate::prelude::*;

    #[test]
    fn memory() {
        let context = Context::new();
        let module = context.create_module("memory");
        let mut builder = context.create_builder();

        let i64_t = context.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let function_type = FunctionType::new(context.void_t(), &[p_i64_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_p_i64 = function.get_param(0).unwrap();

        let p = malloc!(i64_t);

        builder <<= free!(p);

        assert_eq!(
            bb.last_instructions(4),
            vec![
                "%malloccall = tail call i8* @malloc(i32 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32))",
                "%malloc = bitcast i8* %malloccall to i64*",
                "%1 = bitcast i64* %malloc to i8*",
                "tail call void @free(i8* %1)",
            ]
        );

        builder <<= malloc!(i64_t, i64_t.int(123));

        assert_eq!(
            bb.last_instructions(4),
            vec![
                "%2 = trunc i64 123 to i32",
                "%mallocsize = mul i32 %2, ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32)",
                "%malloccall1 = tail call i8* @malloc(i32 %mallocsize)",
                "%array_malloc = bitcast i8* %malloccall1 to i64*",
            ]
        );

        let alloca = alloca!(i64_t).emit_to(&builder);

        assert_eq!(alloca.to_string().trim(), "%alloca = alloca i64");
        assert_eq!(alloca.allocated_type(), i64_t);

        assert_eq!(
            alloca!(i64_t, i64_t.int(123)).emit_to(&builder).to_string().trim(),
            "%array_alloca = alloca i64, i64 123"
        );

        let load = load!(arg0_p_i64).emit_to(&builder);

        assert_eq!(load.to_string().trim(), "%load = load i64, i64* %0");

        assert!(!load.is_volatile());
        assert_eq!(load.atomic_ordering(), LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic);

        load.set_volatile(true);
        assert_eq!(load.to_string().trim(), "%load = load volatile i64, i64* %0");

        load.set_atomic_ordering(LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
        assert_eq!(
            load.to_string().trim(),
            "%load = load atomic volatile i64, i64* %0 acquire"
        );

        let store = store!(i64_t.int(123), arg0_p_i64).emit_to(&builder);

        assert_eq!(store.to_string().trim(), "store i64 123, i64* %0");

        assert!(!store.is_volatile());
        assert_eq!(store.atomic_ordering(), LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic);

        store.set_volatile(true);
        assert_eq!(store.to_string().trim(), "store volatile i64 123, i64* %0");

        store.set_atomic_ordering(LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
        assert_eq!(
            store.to_string().trim(),
            "store atomic volatile i64 123, i64* %0 release"
        );
    }
}
