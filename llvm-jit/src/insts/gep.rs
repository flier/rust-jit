use std::borrow::Cow;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::utils::{AsBool, AsLLVMBool, AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

/// an instruction for type-safe pointer arithmetic to access elements of arrays and structs
#[derive(Clone, Debug, PartialEq)]
pub struct GetElementPtr<'a> {
    ptr: Box<AstNode<'a>>,
    gep: GEP<'a>,
    name: Cow<'a, str>,
}

#[derive(Clone, Debug, PartialEq)]
enum GEP<'a> {
    Indices(Vec<AstNode<'a>>),
    InBounds(Vec<AstNode<'a>>),
    Struct(u32),
}

impl<'a> GetElementPtr<'a> {
    pub fn new<P, I, N>(ptr: P, indices: I, name: N) -> Self
    where
        P: Into<AstNode<'a>>,
        I: IntoIterator<Item = AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr {
            ptr: Box::new(ptr.into()),
            gep: GEP::Indices(indices.into_iter().collect()),
            name: name.into(),
        }
    }

    pub fn in_bounds<P, I, N>(ptr: P, indices: I, name: N) -> Self
    where
        P: Into<AstNode<'a>>,
        I: IntoIterator<Item = AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr {
            ptr: Box::new(ptr.into()),
            gep: GEP::InBounds(indices.into_iter().collect()),
            name: name.into(),
        }
    }

    pub fn in_struct<P, N>(ptr: P, index: u32, name: N) -> Self
    where
        P: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr {
            ptr: Box::new(ptr.into()),
            gep: GEP::Struct(index),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for GetElementPtr<'a> {
    type Target = GetElementPtrInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            match self.gep {
                GEP::Indices(indices) => {
                    let mut indices = indices
                        .into_iter()
                        .map(|v| v.emit_to(builder).into_raw())
                        .collect::<Vec<LLVMValueRef>>();

                    LLVMBuildGEP(
                        builder.as_raw(),
                        self.ptr.emit_to(builder).into_raw(),
                        indices.as_mut_ptr(),
                        indices.len() as u32,
                        cstr!(self.name),
                    )
                }
                GEP::InBounds(indices) => {
                    let mut indices = indices
                        .into_iter()
                        .map(|v| v.emit_to(builder).into_raw())
                        .collect::<Vec<LLVMValueRef>>();

                    LLVMBuildInBoundsGEP(
                        builder.as_raw(),
                        self.ptr.emit_to(builder).into_raw(),
                        indices.as_mut_ptr(),
                        indices.len() as u32,
                        cstr!(self.name),
                    )
                }
                GEP::Struct(index) => LLVMBuildStructGEP(
                    builder.as_raw(),
                    self.ptr.emit_to(builder).into_raw(),
                    index,
                    cstr!(self.name),
                ),
            }
        }
        .into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GetElementPtrInst(Instruction);

inherit_from!(GetElementPtrInst, Instruction, ValueRef, LLVMValueRef);

/// an instruction for type-safe pointer arithmetic to access elements of arrays and structs
impl GetElementPtrInst {
    /// Check whether the given GEP instruction is inbounds.
    pub fn in_bounds(&self) -> bool {
        unsafe { LLVMIsInBounds(self.as_raw()) }.as_bool()
    }

    /// Set the given GEP instruction to be inbounds or not.
    pub fn set_in_bounds(&self, in_bounds: bool) {
        unsafe { LLVMSetIsInBounds(self.as_raw(), in_bounds.as_bool()) }
    }
}

/// The `getelementptr` instruction is used to get the address of a subelement of an aggregate data structure.
///
/// It performs address calculation only and does not access memory.
/// The instruction can also be used to calculate a vector of such addresses.
///
/// If the `inbounds` keyword is present, the result value of the getelementptr is a poison value
/// if the base pointer is not an in bounds address of an allocated object, or if any of the addresses
/// that would be formed by successive addition of the offsets implied by the indices to the base address
/// with infinitely precise signed arithmetic are not an in bounds address of that allocated object.
/// The in bounds addresses for an allocated object are all the addresses that point into the object,
/// plus the address one byte past the end. In cases where the base is a vector of pointers
/// the inbounds keyword applies to each of the computations element-wise.
///
/// If the `inbounds` keyword is not present, the offsets are added to the base address
/// with silently-wrapping two’s complement arithmetic. If the offsets have a different width from the pointer,
/// they are sign-extended or truncated to the width of the pointer.
/// The result value of the getelementptr may be outside the object pointed to by the base pointer.
/// The result value may not necessarily be used to access memory though,
/// even if it happens to point into allocated storage.
#[macro_export]
macro_rules! gep {
    ($ptr:expr, $( $index:expr ),* ; $name:expr) => ({
        $crate::insts::GetElementPtr::new($ptr, vec![ $( $index.into() ),* ], $name)
    });
    (inbounds $ptr:expr, $( $index:expr ),* ; $name:expr) => ({
        $crate::insts::GetElementPtr::in_bounds($ptr, vec![ $( $index.into() ),* ], $name)
    });

    ($ptr:expr, $( $index:expr ),*) => ({
        gep!($ptr, $( $index ),* ; "gep")
    });
    (inbounds $ptr:expr, $( $index:expr ),*) => ({
        gep!(inbounds $ptr, $( $index ),* ; "gep_inbounds")
    });
}

#[macro_export]
macro_rules! struct_gep {
    ($struct_ptr: expr, $index: expr; $name: expr) => {{
        $crate::insts::GetElementPtr::in_struct($struct_ptr, $index, $name)
    }};
    ($struct_ptr: expr, $index: expr) => {{
        struct_gep!($struct_ptr, $index; "struct_gep")
    }};
}

impl IRBuilder {
    pub fn gep<'a, P, I, N>(&self, ptr: P, indices: I, name: N) -> GetElementPtrInst
    where
        P: Into<AstNode<'a>>,
        I: IntoIterator<Item = AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr::new(ptr, indices, name).emit_to(self)
    }

    pub fn gep_in_bounds<'a, P, I, N>(&self, ptr: P, indices: I, name: N) -> GetElementPtrInst
    where
        P: Into<AstNode<'a>>,
        I: IntoIterator<Item = AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr::in_bounds(ptr, indices, name).emit_to(self)
    }

    pub fn gep_in_struct<'a, P, N>(&self, ptr: P, index: u32, name: N) -> GetElementPtrInst
    where
        P: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        GetElementPtr::in_struct(ptr, index, name).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    #[test]
    fn gep() {
        let context = Context::new();
        let module = context.create_module("gep");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let i32_t = context.int32_t();
        let i64_t = context.int64_t();
        let array_t = i64_t.array_t(4);
        let vector_t = i64_t.vector_t(4);
        let struct_t = context.named_struct_t("struct", &[i32_t, i64_t, vector_t.into()], false);

        let p_array = alloca!(array_t; "p_array");
        let p_vector = alloca!(vector_t; "p_vector");
        let p_struct = alloca!(struct_t; "p_struct").emit_to(&builder);

        assert_eq!(
            gep!(p_array, i64_t.int(1)).emit_to(&builder).to_string().trim(),
            "%gep = getelementptr [4 x i64], [4 x i64]* %p_array, i64 1"
        );

        assert_eq!(
            gep!(inbounds p_vector, i64_t.int(1))
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%gep_inbounds = getelementptr inbounds <4 x i64>, <4 x i64>* %p_vector, i64 1"
        );

        assert_eq!(
            struct_gep!(p_struct, 1).emit_to(&builder).to_string().trim(),
            "%struct_gep = getelementptr inbounds %struct, %struct* %p_struct, i32 0, i32 1"
        );

        assert_eq!(
            gep!(inbounds p_struct, i32_t.int(2), i32_t.int(1))
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%gep_inbounds1 = getelementptr inbounds %struct, %struct* %p_struct, i32 2, i32 1"
        );
    }
}
