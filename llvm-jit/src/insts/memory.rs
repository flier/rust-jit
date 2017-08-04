use std::borrow::Cow;

use llvm::core::*;
use llvm::prelude::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::unchecked_cstring;
use value::{AsValueRef, Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Malloc<'a> {
    ty: TypeRef,
    size: Option<ValueRef>,
    name: Cow<'a, str>,
}

impl<'a> Malloc<'a> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: ValueRef, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a> InstructionBuilder for Malloc<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            } else {
                LLVMBuildMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            }
        }.into()
    }
}

/// Invoke `malloc` function to allocates memory on the heap, need to be expicity released by its caller.
#[macro_export]
macro_rules! malloc {
    ($ty:expr; $name:expr) => ({
        $crate::insts::Malloc::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr; $name:expr) => ({
        $crate::insts::Malloc::array($array_ty.into(), $size.into(), $name.into())
    });

    ($ty:expr) => ({
        malloc!($ty ; "malloc")
    });
    ($array_ty:expr, $size:expr) => ({
        malloc!($array_ty, $size ; "array_malloc")
    });
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alloca<'a> {
    ty: TypeRef,
    size: Option<ValueRef>,
    name: Cow<'a, str>,
}

impl<'a> Alloca<'a> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: ValueRef, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a> InstructionBuilder for Alloca<'a> {
    type Target = AllocaInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            } else {
                LLVMBuildAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
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
        $crate::insts::Alloca::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr; $name:expr) => ({
        $crate::insts::Alloca::array($array_ty.into(), $size.into(), $name.into())
    });

    ($ty:expr) => ({
        alloca!($ty ; "alloca")
    });
    ($array_ty:expr, $size:expr) => ({
        alloca!($array_ty, $size ; "array_alloca")
    });
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Free(ValueRef);

impl Free {
    pub fn new(ptr: ValueRef) -> Self {
        Free(ptr)
    }
}

impl InstructionBuilder for Free {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildFree(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// Deallocates the memory allocation pointed to by ptr.
pub fn free<P: Into<ValueRef>>(ptr: P) -> Free {
    Free::new(ptr.into())
}

#[derive(Clone, Debug, PartialEq)]
pub struct Load<'a> {
    ptr: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> Load<'a> {
    pub fn new(ptr: ValueRef, name: Cow<'a, str>) -> Self {
        Load { ptr, name }
    }
}

impl<'a> InstructionBuilder for Load<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildLoad(
                builder.as_raw(),
                self.ptr.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `load` instruction is used to read from memory.
pub fn load<'a, P, N>(ptr: P, name: N) -> Load<'a>
where
    P: Into<ValueRef>,
    N: Into<Cow<'a, str>>,
{
    Load::new(ptr.into(), name.into())
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store {
    value: ValueRef,
    ptr: ValueRef,
}

impl Store {
    pub fn new(value: ValueRef, ptr: ValueRef) -> Self {
        Store { value, ptr }
    }
}

impl InstructionBuilder for Store {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildStore(builder.as_raw(), self.value.as_raw(), self.ptr.as_raw()) }.into()
    }
}

/// The `store` instruction is used to write to memory.
pub fn store<V, P>(value: V, ptr: P) -> Store
where
    V: Into<ValueRef>,
    P: Into<ValueRef>,
{
    Store::new(value.into(), ptr.into())
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn memory() {
        let context = Context::new();
        let module = context.create_module("memory");
        let builder = IRBuilder::within_context(&context);

        let i64_t = context.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let function_type = FunctionType::new(context.void_t(), &[p_i64_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_p_i64 = function.get_param(0).unwrap();

        let p = malloc!(i64_t).emit_to(&builder);

        free(p).emit_to(&builder);

        assert_eq!(
            bb.last_instructions(4),
            vec![
                "%malloccall = tail call i8* @malloc(i32 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32))",
                "%malloc = bitcast i8* %malloccall to i64*",
                "%1 = bitcast i64* %malloc to i8*",
                "tail call void @free(i8* %1)",
            ]
        );

        malloc!(i64_t, i64_t.int(123)).emit_to(&builder);

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
            load(arg0_p_i64, "load")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%load = load i64, i64* %0"
        );

        assert_eq!(
            store(i64_t.int(123), arg0_p_i64)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "store i64 123, i64* %0"
        );
    }
}
