use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::AsRaw;
use value::Instruction;

/// Make a new global variable with an initializer that has array of i8 type
/// filled in with the null terminated string value specified.
/// The new global variable will be marked mergable with any others of the same contents.
/// If Name is specified, it is the name of the global variable created.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalString<'a> {
    s: Cow<'a, str>,
    name: Cow<'a, str>,
}

impl<'a> GlobalString<'a> {
    pub fn new(s: Cow<'a, str>, name: Cow<'a, str>) -> Self {
        GlobalString { s, name }
    }
}

impl<'a> InstructionBuilder for GlobalString<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildGlobalString(builder.as_raw(), cstr!(self.s), cstr!(self.name)) }.into()
    }
}

/// Make a new global variable with initializer type i8*
pub fn global_str<'a, S, N>(s: S, name: N) -> GlobalString<'a>
where
    S: Into<Cow<'a, str>>,
    N: Into<Cow<'a, str>>,
{
    GlobalString::new(s.into(), name.into())
}

#[macro_export]
macro_rules! global_str {
    ($s:expr; $name:expr) => (
        $crate::insts::global_str($s, $name)
    );
    ($s:expr) => {
        $crate::insts::global_str($s, "")
    }
}

/// Same as `GlobalString`, but return a pointer with "i8*" type instead of a pointer to array of i8.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalStringPtr<'a> {
    s: Cow<'a, str>,
    name: Cow<'a, str>,
}

impl<'a> GlobalStringPtr<'a> {
    pub fn new(s: Cow<'a, str>, name: Cow<'a, str>) -> Self {
        GlobalStringPtr { s, name }
    }
}

impl<'a> InstructionBuilder for GlobalStringPtr<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildGlobalStringPtr(builder.as_raw(), cstr!(self.s), cstr!(self.name)) }.into()
    }
}

/// Make a new global variable with initializer type i8*,
/// return a pointer with "i8*" type instead of a pointer to array of i8.
pub fn global_str_ptr<'a, S, N>(s: S, name: N) -> GlobalStringPtr<'a>
where
    S: Into<Cow<'a, str>>,
    N: Into<Cow<'a, str>>,
{
    GlobalStringPtr::new(s.into(), name.into())
}

#[macro_export]
macro_rules! global_str_ptr {
    ($s:expr; $name:expr) => (
        $crate::insts::global_str_ptr($s, $name)
    );
    ($s:expr) => {
        $crate::insts::global_str_ptr($s, "")
    }
}

impl IRBuilder {
    /// Make a new global variable with initializer type i8*
    pub fn global_str<'a, S, N>(&self, s: S, name: N) -> Instruction
    where
        S: Into<Cow<'a, str>>,
        N: Into<Cow<'a, str>>,
    {
        global_str(s, name).emit_to(self)
    }

    /// Make a new global variable with initializer type i8*,
    /// return a pointer with "i8*" type instead of a pointer to array of i8.
    pub fn global_str_ptr<'a, S, N>(&self, s: S, name: N) -> Instruction
    where
        S: Into<Cow<'a, str>>,
        N: Into<Cow<'a, str>>,
    {
        global_str_ptr(s, name).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn globalstr() {
        let context = Context::new();
        let module = context.create_module("globalstr");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        assert_eq!(
            global_str!("global_str"; "global_str")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "@global_str = private unnamed_addr constant [11 x i8] c\"global_str\\00\""
        );

        assert_eq!(
            global_str_ptr!("global_str_ptr"; "global_str_ptr")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "i8* getelementptr inbounds ([15 x i8], [15 x i8]* @global_str_ptr, i32 0, i32 0)"
        );

        assert_eq!(
            module.global_vars().rev().next().unwrap().to_string(),
            "@global_str_ptr = private unnamed_addr constant [15 x i8] c\"global_str_ptr\\00\""
        );
    }
}
