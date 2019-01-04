use std::path::Path;
use std::ptr::NonNull;
use std::slice;
use std::str;

use failure::err_msg;

use crate::context::Context;
use crate::errors::Result;
use crate::llvm::debuginfo::*;
use crate::llvm::prelude::*;
use crate::metadata::Metadata;
use crate::module::Module;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, AsResult, IntoRaw};

pub type DWARFSourceLanguage = LLVMDWARFSourceLanguage;
pub type DWARFEmissionKind = LLVMDWARFEmissionKind;
pub type DWARFTypeEncoding = LLVMDWARFTypeEncoding;

/// The current debug metadata version number.
pub fn debug_metadata_version() -> u32 {
    unsafe { LLVMDebugMetadataVersion() }
}

impl Context {
    /// Creates a new DebugLocation that describes a source location.
    pub fn create_debug_location(&self, line: u32, column: u32, scope: Metadata, inlined_at: Metadata) -> Metadata {
        unsafe {
            LLVMDIBuilderCreateDebugLocation(self.as_raw(), line, column, scope.into_raw(), inlined_at.into_raw())
        }
        .into()
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

#[repr(transparent)]
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

    /// Create a CompileUnit which provides an anchor
    /// for all debugging information generated during this instance of compilation.
    pub fn create_compile_unit<'a>(
        &'a self,
        lang: DWARFSourceLanguage,
        file: DIFile,
        producer: &'a str,
    ) -> DICompileUnitBuilder<'a> {
        DICompileUnitBuilder::new(self, lang, file, producer)
    }

    /// Create a file descriptor to hold debugging information for a file.
    pub fn create_file<P: AsRef<Path>>(&self, path: P) -> Result<DIFile> {
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

    /// Creates a new descriptor for a module with the specified parent scope.
    pub fn create_module<'a, S>(&'a self, scope: S, name: &'a str) -> DIModuleBuilder<'a>
    where
        S: Into<DIScope>,
    {
        DIModuleBuilder::new(self, scope.into(), name)
    }

    /// Create a new descriptor for the specified subprogram.
    pub fn create_function<'a, S>(
        &'a self,
        scope: S,
        name: &'a str,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> DIFunctionBuilder<'a>
    where
        S: Into<DIScope>,
    {
        DIFunctionBuilder::new(self, scope.into(), name, file, line_no, func_ty, scope_line)
    }

    /// Create subroutine type.
    pub fn create_subroutine_type(
        &self,
        file: DIFile,
        params: &[DIType],
        flags: LLVMDIFlags,
    ) -> Result<DISubroutineType> {
        let params = params.iter().map(|param| param.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateSubroutineType(
                self.as_raw(),
                file.into_raw(),
                params.as_ptr() as *mut _,
                params.len() as u32,
                flags,
            )
        }
        .ok_or_else(|| err_msg("fail to create subroutine type"))
    }

    /// Create debugging information entry for a basic type.
    pub fn create_basic_type<S: AsRef<str>>(
        &self,
        name: S,
        bits: u64,
        encoding: DWARFTypeEncoding,
    ) -> Result<DIBasicType> {
        let name = name.as_ref();

        unsafe { LLVMDIBuilderCreateBasicType(self.as_raw(), cstr!(name), name.len(), bits, encoding) }
            .ok_or_else(|| err_msg("fail to create basic type"))
    }
}

macro_rules! define_debug_info_type {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub struct $name(Metadata);

        inherit_from!($name, Metadata, LLVMMetadataRef);
    };
    ($name:ident, $parent:ident) => {
        #[repr(transparent)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub struct $name($parent);

        inherit_from!($name, $parent, Metadata, LLVMMetadataRef);
    };
}

define_debug_info_type!(DIType);
define_debug_info_type!(DIBasicType, DIType);
define_debug_info_type!(DISubroutineType, DIType);

define_debug_info_type!(DIScope);
define_debug_info_type!(DICompileUnit, DIScope);
define_debug_info_type!(DIFile, DIScope);
define_debug_info_type!(DIModule, DIScope);
define_debug_info_type!(DISubprogram, DIScope);

#[macro_export]
macro_rules! ditypes {
    ($($x:expr),*) => (&[ $($crate::DIType::from($x)),* ]);
    ($($x:expr,)*) => (&[ $($crate::DIType::from($x)),* ]);
}

impl DIType {
    /// Get the name of this DIType.
    pub fn name(&self) -> &str {
        let mut len = 0;
        let data = unsafe { LLVMDITypeGetName(self.as_raw(), &mut len) };

        NonNull::new(data as *mut _)
            .map(|data| unsafe { slice::from_raw_parts(data.as_ptr(), len) })
            .and_then(|data| str::from_utf8(data).ok())
            .unwrap_or_default()
    }

    /// Get the size of this DIType in bits.
    pub fn size(&self) -> usize {
        unsafe { LLVMDITypeGetSizeInBits(self.as_raw()) as usize }
    }

    /// Get the offset of this DIType in bits.
    pub fn offset(&self) -> usize {
        unsafe { LLVMDITypeGetOffsetInBits(self.as_raw()) as usize }
    }

    /// Get the alignment of this DIType in bits.
    pub fn align(&self) -> usize {
        unsafe { LLVMDITypeGetAlignInBits(self.as_raw()) as usize }
    }

    /// Get the source line where this DIType is declared.
    pub fn line(&self) -> usize {
        unsafe { LLVMDITypeGetLine(self.as_raw()) as usize }
    }

    /// Get the flags associated with this DIType.
    pub fn flags(&self) -> LLVMDIFlags {
        unsafe { LLVMDITypeGetFlags(self.as_raw()) }
    }
}

pub struct DICompileUnitBuilder<'a> {
    pub builder: &'a DIBuilder,
    pub lang: DWARFSourceLanguage,
    pub file: DIFile,
    pub producer: &'a str,
    pub optimized: bool,
    pub flags: &'a str,
    pub runtime_version: u32,
    pub split_name: &'a str,
    pub emission_kind: DWARFEmissionKind,
    pub dwoid: u32,
    pub split_debug_inlining: bool,
    pub for_profiling: bool,
}

impl<'a> DICompileUnitBuilder<'a> {
    pub fn new(
        builder: &'a DIBuilder,
        lang: DWARFSourceLanguage,
        file: DIFile,
        producer: &'a str,
    ) -> DICompileUnitBuilder<'a> {
        DICompileUnitBuilder {
            builder,
            lang,
            file,
            producer,
            optimized: false,
            flags: "",
            runtime_version: 0,
            split_name: "",
            emission_kind: LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
            dwoid: 0,
            split_debug_inlining: false,
            for_profiling: false,
        }
    }

    pub fn with_optimized(mut self) -> Self {
        self.optimized = true;
        self
    }

    pub fn with_flags(mut self, flags: &'a str) -> DICompileUnitBuilder<'a> {
        self.flags = flags;
        self
    }

    pub fn with_runtime_version(mut self, runtime_version: u32) -> Self {
        self.runtime_version = runtime_version;
        self
    }

    pub fn with_split_name(mut self, split_name: &'a str) -> Self {
        self.split_name = split_name;
        self
    }

    pub fn with_emission_kind(mut self, kind: DWARFEmissionKind) -> Self {
        self.emission_kind = kind;
        self
    }

    pub fn with_dwoid(mut self, dwoid: u32) -> Self {
        self.dwoid = dwoid;
        self
    }

    pub fn split_debug_inlining(mut self, v: bool) -> Self {
        self.split_debug_inlining = v;
        self
    }

    pub fn for_profiling(mut self, v: bool) -> Self {
        self.for_profiling = v;
        self
    }

    pub fn build(self) -> Result<DICompileUnit> {
        unsafe {
            LLVMDIBuilderCreateCompileUnit(
                self.builder.as_raw(),
                self.lang,
                self.file.into_raw(),
                cstr!(self.producer),
                self.producer.len(),
                self.optimized.as_bool(),
                cstr!(self.flags),
                self.flags.len(),
                self.runtime_version,
                cstr!(self.split_name),
                self.split_name.len(),
                self.emission_kind,
                self.dwoid,
                self.split_debug_inlining.as_bool(),
                self.for_profiling.as_bool(),
            )
        }
        .ok_or_else(|| err_msg("fail to create compile unit"))
    }
}

pub struct DIModuleBuilder<'a> {
    pub builder: &'a DIBuilder,
    pub scope: DIScope,
    pub name: &'a str,
    pub config_macros: &'a str,
    pub include_path: &'a str,
    pub isys_root: &'a str,
}

impl<'a> DIModuleBuilder<'a> {
    pub fn new(builder: &'a DIBuilder, scope: DIScope, name: &'a str) -> Self {
        DIModuleBuilder {
            builder,
            scope,
            name,
            config_macros: "",
            include_path: "",
            isys_root: "",
        }
    }

    pub fn build(self) -> Result<DIModule> {
        unsafe {
            LLVMDIBuilderCreateModule(
                self.builder.as_raw(),
                self.scope.as_raw(),
                cstr!(self.name),
                self.name.len(),
                cstr!(self.config_macros),
                self.config_macros.len(),
                cstr!(self.include_path),
                self.include_path.len(),
                cstr!(self.isys_root),
                self.isys_root.len(),
            )
        }
        .ok_or_else(|| err_msg("fail to create module"))
    }
}

pub struct DIFunctionBuilder<'a> {
    pub builder: &'a DIBuilder,
    pub scope: DIScope,
    pub name: &'a str,
    pub linkage_name: &'a str,
    pub file: DIFile,
    pub line_no: u32,
    pub func_ty: DISubroutineType,
    pub local_to_unit: bool,
    pub definition: bool,
    pub scope_line: u32,
    pub flags: LLVMDIFlags,
    pub optimized: bool,
}

impl<'a> DIFunctionBuilder<'a> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: DIScope,
        name: &'a str,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> Self {
        DIFunctionBuilder {
            builder,
            scope,
            name,
            linkage_name: "",
            file,
            line_no,
            func_ty,
            local_to_unit: false,
            definition: false,
            scope_line,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            optimized: false,
        }
    }

    pub fn with_linkage_name(mut self, name: &'a str) -> Self {
        self.linkage_name = name;
        self
    }

    pub fn with_local_to_unit(mut self, v: bool) -> Self {
        self.local_to_unit = v;
        self
    }
    pub fn with_definition(mut self, v: bool) -> Self {
        self.definition = v;
        self
    }
    pub fn with_optimized(mut self, v: bool) -> Self {
        self.optimized = v;
        self
    }

    pub fn build(self) -> Result<DISubprogram> {
        unsafe {
            LLVMDIBuilderCreateFunction(
                self.builder.as_raw(),
                self.scope.into_raw(),
                cstr!(self.name),
                self.name.len(),
                cstr!(self.linkage_name),
                self.linkage_name.len(),
                self.file.into_raw(),
                self.line_no,
                self.func_ty.into_raw(),
                self.local_to_unit.as_bool(),
                self.definition.as_bool(),
                self.scope_line,
                self.flags,
                self.optimized.as_bool(),
            )
        }
        .ok_or_else(|| err_msg("fail to create function"))
    }
}
