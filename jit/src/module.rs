use std::borrow::Cow;
use std::fmt;
use std::path::Path;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::context::{Context, GlobalContext};
use crate::errors::Result;
use crate::types::TypeRef;
use crate::utils::{from_unchecked_cstr, AsRaw, AsResult, DisposableMessage, UncheckedCStr};

pub type AddressSpace = u32;

/// Modules represent the top-level structure in an LLVM program.
#[derive(Debug)]
pub struct Module(State);

#[derive(Debug)]
pub enum State {
    Owned(LLVMModuleRef),
    Borrowed(LLVMModuleRef),
}

impl AsRaw for Module {
    type RawType = LLVMModuleRef;

    fn as_raw(&self) -> Self::RawType {
        match self.0 {
            State::Owned(m) | State::Borrowed(m) => m,
        }
    }
}

impl Module {
    pub fn borrow(&self) -> Self {
        self.as_raw().into()
    }

    /// Obtain the context to which this module is associated.
    pub fn context(&self) -> Context {
        unsafe { LLVMGetModuleContext(self.as_raw()) }.into()
    }

    /// Obtain the identifier of a module.
    pub fn name(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetModuleIdentifier(self.as_raw(), &mut len);

            from_unchecked_cstr(p as *const u8, len as usize + 1)
        }
    }

    /// Set the identifier of a module to a string Ident with length Len.
    pub fn set_name<S: AsRef<str>>(&self, name: S) {
        let name = name.as_ref();

        unsafe { LLVMSetModuleIdentifier(self.as_raw(), cstr!(name), name.len()) }
    }

    /// Obtain the module's original source file name.
    pub fn source_filename(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetSourceFileName(self.as_raw(), &mut len);

            from_unchecked_cstr(p as *const u8, len as usize + 1)
        }
    }

    /// Set the original source file name of a module to a string Name with length Len.
    pub fn set_source_filename<S: AsRef<str>>(&self, filename: S) {
        let filename = filename.as_ref();

        unsafe { LLVMSetSourceFileName(self.as_raw(), cstr!(filename), filename.len()) }
    }

    /// Obtain the data layout for a module.
    pub fn data_layout_str(&self) -> Cow<str> {
        unsafe { LLVMGetDataLayoutStr(self.as_raw()) }.as_str()
    }

    /// Set the data layout for a module.
    pub fn set_data_layout_str<S: AsRef<str>>(&self, name: S) {
        unsafe { LLVMSetDataLayout(self.as_raw(), cstr!(name.as_ref())) }
    }

    /// Obtain the target triple for a module.
    pub fn target_triple(&self) -> Cow<str> {
        unsafe { LLVMGetTarget(self.as_raw()) }.as_str()
    }

    /// Set the target triple for a module.
    pub fn set_target_triple<S: AsRef<str>>(&self, triple: S) {
        unsafe { LLVMSetTarget(self.as_raw(), cstr!(triple.as_ref())) }
    }

    /// Dump a representation of a module to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.as_raw()) }
    }

    /// Print a representation of a module to a file.
    pub fn print_to_file<P: AsRef<Path>>(&self, filename: P) -> Result<()> {
        let filename = filename.as_ref();
        let mut err = DisposableMessage::new();

        unsafe { LLVMPrintModuleToFile(self.as_raw(), cpath!(filename), &mut err) }.ok_or_else(|| {
            format_err!(
                "fail to print {:?} to file `{:?}`, {}",
                self,
                filename,
                err.into_string()
            )
        })
    }

    /// Obtain a Type from a module by its registered name.
    pub fn get_type<S: AsRef<str>>(&self, name: S) -> Option<TypeRef> {
        let name = name.as_ref();

        unsafe { LLVMGetTypeByName(self.as_raw(), cstr!(name)) }.ok()
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        if let State::Owned(m) = self.0 {
            trace!("drop {:?}", self);

            unsafe { LLVMDisposeModule(m) }
        }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        trace!("clone {:?}", self);

        unsafe { LLVMCloneModule(self.as_raw()) }.into()
    }
}

impl From<LLVMModuleRef> for Module {
    fn from(m: LLVMModuleRef) -> Self {
        Module(State::Borrowed(m))
    }
}

impl From<Module> for LLVMModuleRef {
    fn from(m: Module) -> Self {
        m.as_raw()
    }
}

impl PartialEq<Module> for Module {
    fn eq(&self, other: &Module) -> bool {
        self.as_raw() == other.as_raw()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", unsafe { LLVMPrintModuleToString(self.as_raw()) }.into_string())
    }
}

impl Context {
    /// Create a new, empty module in a specific context.
    pub fn create_module<S: AsRef<str>>(&self, name: S) -> Module {
        let name = name.as_ref();
        let module = unsafe { LLVMModuleCreateWithNameInContext(cstr!(name), self.as_raw()) };

        trace!("create `{}` module in {:?}: Module({:?})", name, self, module,);

        Module(State::Owned(module))
    }
}

impl GlobalContext {
    /// Create a new, empty module in the global context.
    pub fn create_module<S: AsRef<str>>(name: S) -> Module {
        let name = name.as_ref();
        let module = unsafe { LLVMModuleCreateWithName(cstr!(name)) };

        trace!("create `{}` module in global context: Module({:?})", name, module);

        Module(State::Owned(module))
    }
}

#[cfg(test)]
mod tests {
    use std::io::prelude::*;

    use crate::llvm::*;
    use tempfile::NamedTempFile;

    use super::*;
    use crate::metadata::ModuleFlagEntry;

    #[test]
    fn create() {
        let context = Context::new();

        let m = context.create_module("test");
        assert!(!m.as_raw().is_null());

        assert_eq!(m.name(), "test");
        m.set_name("hello");
        assert_eq!(m.name(), "hello");

        assert_eq!(m.source_filename(), "test");
        m.set_source_filename("file");
        assert_eq!(m.source_filename(), "file");

        assert_eq!(m.data_layout_str(), "");
        m.set_data_layout_str("e");
        assert_eq!(m.data_layout_str(), "e");

        assert_eq!(m.target_triple(), "");
        m.set_target_triple("x86_64-apple-darwin");
        assert_eq!(m.target_triple(), "x86_64-apple-darwin");

        assert_eq!(m.flags().collect::<Vec<_>>(), vec![]);

        let md = GlobalContext::md_string("value").as_metadata();
        m.add_flag(LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorError, "key", md);

        assert_eq!(m.get_flag("key"), md);
        assert_eq!(
            m.flags().collect::<Vec<_>>(),
            vec![ModuleFlagEntry {
                behavior: LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorError,
                key: "key".into(),
                metadata: md
            }]
        );

        m.set_inline_asm("pushq   %rbp");
        m.append_inline_asm("movq    %rsp, %rbp");

        assert_eq!(m.inline_asm(), "pushq   %rbp\nmovq    %rsp, %rbp\n");

        let mut f = NamedTempFile::new().unwrap();

        m.print_to_file(f.path()).unwrap();

        let mut buf = String::new();

        f.read_to_string(&mut buf).unwrap();

        assert_eq!(
            buf,
            r#"; ModuleID = 'hello'
source_filename = "file"
target datalayout = "e"
target triple = "x86_64-apple-darwin"

module asm "pushq   %rbp"
module asm "movq    %rsp, %rbp"

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"key", !"value"}
"#
        );

        assert_eq!(m.to_string(), buf);
    }
}
