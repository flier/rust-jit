use std::borrow::Cow;
use std::fmt;

use llvm::core::*;
use llvm::prelude::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::AsRaw;
use value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Malloc<'a, V> {
    ty: TypeRef,
    size: Option<V>,
    name: Cow<'a, str>,
}

impl<'a, V> Malloc<'a, V> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: V, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a, V> InstructionBuilder for Malloc<'a, V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.emit_to(builder).into().as_raw(),
                    cstr!(self.name),
                )
            } else {
                LLVMBuildMalloc(builder.as_raw(), self.ty.as_raw(), cstr!(self.name))
            }
        }.into()
    }
}

/// Invoke `malloc` function to allocates memory on the heap, need to be expicity released by its caller.
#[macro_export]
macro_rules! malloc {
    ($ty:expr; $name:expr) => ({
        $crate::insts::Malloc::<ValueRef>::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr; $name:expr) => ({
        $crate::insts::Malloc::array($array_ty.into(), $size, $name.into())
    });

    ($ty:expr) => ({
        malloc!($ty ; "malloc")
    });
    ($array_ty:expr, $size:expr) => ({
        malloc!($array_ty, $size ; "array_malloc")
    });
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alloca<'a, V> {
    ty: TypeRef,
    size: Option<V>,
    name: Cow<'a, str>,
}

impl<'a, V> Alloca<'a, V> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: V, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a, V> InstructionBuilder for Alloca<'a, V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = AllocaInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.emit_to(builder).into().as_raw(),
                    cstr!(self.name),
                )
            } else {
                LLVMBuildAlloca(builder.as_raw(), self.ty.as_raw(), cstr!(self.name))
            }
        }.into()
    }
}

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
    ($ty:expr; $name:expr) => ({
        $crate::insts::Alloca::<ValueRef>::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr; $name:expr) => ({
        $crate::insts::Alloca::array($array_ty.into(), $size, $name.into())
    });

    ($ty:expr) => ({
        alloca!($ty ; "alloca")
    });
    ($array_ty:expr, $size:expr) => ({
        alloca!($array_ty, $size ; "array_alloca")
    });
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Free<V>(V);

impl<V> Free<V> {
    pub fn new(ptr: V) -> Self {
        Free(ptr)
    }
}

impl<V> InstructionBuilder for Free<V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildFree(builder.as_raw(), self.0.emit_to(builder).into().as_raw()) }.into()
    }
}

/// Deallocates the memory allocation pointed to by ptr.
pub fn free<P>(ptr: P) -> Free<P> {
    Free::new(ptr)
}

#[macro_export]
macro_rules! free {
    ($ptr:expr) => (
        $crate::insts::free($ptr)
    )
}

#[derive(Clone, Debug, PartialEq)]
pub struct Load<'a, V> {
    ptr: V,
    name: Cow<'a, str>,
}

impl<'a, V> Load<'a, V> {
    pub fn new(ptr: V, name: Cow<'a, str>) -> Self {
        Load { ptr, name }
    }
}

impl<'a, V> InstructionBuilder for Load<'a, V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildLoad(
                builder.as_raw(),
                self.ptr.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `load` instruction is used to read from memory.
pub fn load<'a, P, N>(ptr: P, name: N) -> Load<'a, P>
where
    N: Into<Cow<'a, str>>,
{
    Load::new(ptr, name.into())
}

#[macro_export]
macro_rules! load {
    ($ptr:expr; $name:expr) => (
        $crate::insts::load($ptr, $name)
    );
    ($ptr:expr) => {
        load!($ptr; "load")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store<V, P> {
    value: V,
    ptr: P,
}

impl<V, P> Store<V, P> {
    pub fn new(value: V, ptr: P) -> Self {
        Store { value, ptr }
    }
}

impl<V, P> InstructionBuilder for Store<V, P>
where
    V: InstructionBuilder + fmt::Debug,
    P: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildStore(
                builder.as_raw(),
                self.value.emit_to(builder).into().as_raw(),
                self.ptr.emit_to(builder).into().as_raw(),
            )
        }.into()
    }
}

/// The `store` instruction is used to write to memory.
pub fn store<V, P>(value: V, ptr: P) -> Store<V, P> {
    Store::new(value, ptr)
}

#[macro_export]
macro_rules! store {
    ($value:expr, $ptr:expr) => {
        $crate::insts::store($value, $ptr)
    }
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

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
            alloca!(i64_t, i64_t.int(123))
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%array_alloca = alloca i64, i64 123"
        );

        assert_eq!(
            load!(arg0_p_i64).emit_to(&builder).to_string().trim(),
            "%load = load i64, i64* %0"
        );

        assert_eq!(
            store!(i64_t.int(123), arg0_p_i64)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "store i64 123, i64* %0"
        );
    }
}
