use std::path::Path;

use failure::err_msg;
use llvm::debuginfo::*;
use llvm::prelude::*;

use context::Context;
use errors::Result;
use metadata::Metadata;
use module::Module;
use utils::{AsBool, AsLLVMBool, AsRaw, AsResult, IntoRaw};

pub type DWARFSourceLanguage = LLVMDWARFSourceLanguage;
pub type DWARFEmissionKind = LLVMDWARFEmissionKind;

/// The current debug metadata version number.
pub fn debug_metadata_version() -> u32 {
    unsafe { LLVMDebugMetadataVersion() }
}

impl Context {
    /// Creates a new DebugLocation that describes a source location.
    pub fn create_debug_location(&self, line: u32, column: u32, scope: Metadata, inlined_at: Metadata) -> Metadata {
        unsafe {
            LLVMDIBuilderCreateDebugLocation(self.as_raw(), line, column, scope.into_raw(), inlined_at.into_raw())
        }.into()
    }
}

impl Module {
    /// The version of debug metadata that's present in the provided Module.
    pub fn debug_metadata_version(&self) -> u32 {
        unsafe { LLVMGetModuleDebugMetadataVersion(self.as_raw()) }
    }

    /// Strip debug info in the module if it exists.
    pub fn strip_debug_info(&self) -> bool {
        unsafe { LLVMStripModuleDebugInfo(self.as_raw()) }.as_bool()
    }

    /// Construct a builder for a module, do not allow unresolved nodes.
    pub fn create_di_builder_without_unresolved(&self) -> DIBuilder {
        unsafe { LLVMCreateDIBuilderDisallowUnresolved(self.as_raw()) }.into()
    }

    /// Construct a builder for a module and collect unresolved nodes.
    pub fn create_di_builder(&self) -> DIBuilder {
        unsafe { LLVMCreateDIBuilder(self.as_raw()) }.into()
    }
}

#[derive(Debug)]
pub struct DIBuilder(LLVMDIBuilderRef);

inherit_from!(DIBuilder, LLVMDIBuilderRef);

impl Drop for DIBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeDIBuilder(self.0) }
    }
}

impl DIBuilder {
    /// Construct any deferred debug info descriptors.
    pub fn finalize(&self) {
        unsafe { LLVMDIBuilderFinalize(self.as_raw()) }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::too_many_arguments))]
    pub fn create_compile_unit(
        &self,
        lang: DWARFSourceLanguage,
        file: Metadata,
        producer: &str,
        optimized: bool,
        flags: &str,
        runtime_version: u32,
        split_name: &str,
        kind: DWARFEmissionKind,
        dwoid: u32,
        split_debug_inlining: bool,
        for_profiling: bool,
    ) -> Result<Metadata> {
        unsafe {
            LLVMDIBuilderCreateCompileUnit(
                self.as_raw(),
                lang,
                file.into_raw(),
                cstr!(producer),
                producer.len(),
                optimized.as_bool(),
                cstr!(flags),
                flags.len(),
                runtime_version,
                cstr!(split_name),
                split_name.len(),
                kind,
                dwoid,
                split_debug_inlining.as_bool(),
                for_profiling.as_bool(),
            )
        }.ok_or_else(|| err_msg("fail to create compile unit"))
    }

    /// Create a file descriptor to hold debugging information for a file.
    pub fn create_file<P: AsRef<Path>>(&self, path: P) -> Result<Metadata> {
        let path = path.as_ref();
        let filename = path
            .file_name()
            .and_then(|filename| filename.to_str())
            .ok_or_else(|| err_msg("missed filename"))?;
        let dir = path
            .parent()
            .and_then(|dir| dir.to_str())
            .ok_or_else(|| err_msg("missed directory"))?;

        unsafe { LLVMDIBuilderCreateFile(self.as_raw(), cstr!(filename), filename.len(), cstr!(dir), dir.len()) }
            .ok_or_else(|| err_msg("fail to create file"))
    }
}
