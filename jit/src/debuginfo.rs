use std::alloc::Layout;
use std::ops::Deref;
use std::path::Path;
use std::ptr::{self, NonNull};
use std::slice;
use std::str;

use gimli::{DwAte, DwTag};

use crate::llvm::debuginfo::*;
use crate::llvm::prelude::*;
use crate::prelude::*;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, AsResult, IntoRaw};

pub type DWARFSourceLanguage = LLVMDWARFSourceLanguage;
pub type DWARFEmissionKind = LLVMDWARFEmissionKind;
pub type DWARFTypeEncoding = LLVMDWARFTypeEncoding;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeModifier(DwTag);

impl From<TypeModifier> for u32 {
    fn from(tm: TypeModifier) -> u32 {
        (tm.0).0 as u32
    }
}

pub mod type_modifier {
    use gimli::*;

    use super::TypeModifier;

    /// atomic qualified type (for example, in C)
    pub const ATOMIC: TypeModifier = TypeModifier(DW_TAG_atomic_type);
    /// const qualified type (for example in C, C++)
    pub const CONST: TypeModifier = TypeModifier(DW_TAG_const_type);
    /// immutable type (for example, in D)
    pub const IMMUTABLE: TypeModifier = TypeModifier(DW_TAG_immutable_type);
    /// packed type (for example in Ada, Pascal)
    pub const PACKED: TypeModifier = TypeModifier(DW_TAG_packed_type);
    /// pointer to an object of the type being modified
    pub const POINTER: TypeModifier = TypeModifier(DW_TAG_pointer_type);
    /// reference to (lvalue of) an object of the type being modified
    pub const REFERENCE: TypeModifier = TypeModifier(DW_TAG_reference_type);
    /// restrict qualified type
    pub const RESTRICT: TypeModifier = TypeModifier(DW_TAG_restrict_type);
    /// rvalue reference to an object of the type being modified (for example, in C++)
    pub const RVALUE_REF: TypeModifier = TypeModifier(DW_TAG_rvalue_reference_type);
    /// shared qualified type (for example, in UPC)
    pub const SHARED: TypeModifier = TypeModifier(DW_TAG_shared_type);
    /// volatile qualified type (for example, in C, C++)
    pub const VOLATILE: TypeModifier = TypeModifier(DW_TAG_volatile_type);
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Encoding(DwAte);

impl From<Encoding> for u32 {
    fn from(encoding: Encoding) -> u32 {
        (encoding.0).0 as u32
    }
}

pub mod encoding {
    use gimli::*;

    use super::Encoding;

    /// true or false
    pub const BOOLEAN: Encoding = Encoding(DW_ATE_boolean);

    /// linear machine addressa
    pub const ADDRESS: Encoding = Encoding(DW_ATE_address);
    /// signed binary integer
    pub const SIGNED_INT: Encoding = Encoding(DW_ATE_signed);
    /// signed character
    pub const SIGNED_CHAR: Encoding = Encoding(DW_ATE_signed_char);
    /// unsigned binary integer
    pub const UNSIGNED_INT: Encoding = Encoding(DW_ATE_unsigned);
    /// unsigned character
    pub const UNSIGNED_CHAR: Encoding = Encoding(DW_ATE_unsigned_char);

    /// ISO/IEC 646:1991 character
    pub const ASCII: Encoding = Encoding(DW_ATE_ASCII);
    /// ISO/IEC 10646-1:1993 character (UCS-4)
    pub const UCS: Encoding = Encoding(DW_ATE_UCS);
    /// ISO/IEC 10646-1:1993 character
    pub const UTF: Encoding = Encoding(DW_ATE_UTF);

    /// signed fixed-point scaled integer
    pub const SIGNED_FIXED: Encoding = Encoding(DW_ATE_signed_fixed);
    /// unsigned fixed-point scaled integer
    pub const UNSIGNED_FIXED: Encoding = Encoding(DW_ATE_unsigned_fixed);

    /// binary floating-point number
    pub const FLOAT: Encoding = Encoding(DW_ATE_float);
    /// complex binary floating-point number
    pub const COMPLEX: Encoding = Encoding(DW_ATE_complex_float);
    /// imaginary binary floating-point number
    pub const IMAGINARY_FLOAT: Encoding = Encoding(DW_ATE_imaginary_float);
    /// IEEE 754R decimal floating-point number
    pub const DECIMAL_FLOAT: Encoding = Encoding(DW_ATE_decimal_float);

    /// packed decimal number
    pub const PACKED_DECIMAL: Encoding = Encoding(DW_ATE_packed_decimal);
    /// numeric string
    pub const NUMERIC_STRING: Encoding = Encoding(DW_ATE_numeric_string);
    /// edited string
    pub const EDITED: Encoding = Encoding(DW_ATE_edited);
}

/// The current debug metadata version number.
pub fn metadata_version() -> u32 {
    unsafe { LLVMDebugMetadataVersion() }
}

impl Context {
    /// Creates a new DebugLocation that describes a source location.
    pub fn create_debug_location<S>(&self, line: u32, column: u32, scope: S, inline_at: Option<Metadata>) -> DILocation
    where
        S: Deref<Target = DIScope>,
    {
        unsafe {
            LLVMDIBuilderCreateDebugLocation(
                self.as_raw(),
                line,
                column,
                scope.as_raw(),
                inline_at
                    .map(|metadata| metadata.as_raw())
                    .unwrap_or_else(|| ptr::null_mut()),
            )
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

impl Function {
    /// Get the metadata of the subprogram attached to a function.
    pub fn subprogram(&self) -> Option<DISubprogram> {
        unsafe { LLVMGetSubprogram(self.as_raw()) }.ok()
    }

    /// Set the subprogram attached to a function.
    pub fn set_subprogram(&self, subprogram: DISubprogram) {
        unsafe { LLVMSetSubprogram(self.as_raw(), subprogram.into_raw()) }
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
    pub fn create_compile_unit(&self, lang: DWARFSourceLanguage, file: DIFile, producer: &str) -> DICompileUnit {
        self.create_compile_unit_builder(lang, file, producer).build()
    }

    /// Create a CompileUnit which provides an anchor
    /// for all debugging information generated during this instance of compilation.
    pub fn create_compile_unit_builder<'a>(
        &'a self,
        lang: DWARFSourceLanguage,
        file: DIFile,
        producer: &'a str,
    ) -> DICompileUnitBuilder<'a> {
        DICompileUnitBuilder::new(self, lang, file, producer)
    }

    /// Create a file descriptor to hold debugging information for a file.
    pub fn create_file<P: AsRef<Path>>(&self, path: P) -> DIFile {
        let path = path.as_ref();
        let filename = path
            .file_name()
            .and_then(|filename| filename.to_str())
            .unwrap_or_default();
        let dir = path.parent().and_then(|dir| dir.to_str()).unwrap_or_default();

        unsafe { LLVMDIBuilderCreateFile(self.as_raw(), cstr!(filename), filename.len(), cstr!(dir), dir.len()) }.into()
    }

    /// Creates a new descriptor for a module with the specified parent scope.
    pub fn create_module<S, N>(&self, scope: S, name: N) -> DIModule
    where
        S: Deref<Target = DIScope>,
        N: AsRef<str>,
    {
        self.create_module_builder(scope, name).build()
    }

    /// Creates a new descriptor for a module with the specified parent scope.
    pub fn create_module_builder<'a, S, N>(&'a self, scope: S, name: N) -> DIModuleBuilder<'a, S, N> {
        DIModuleBuilder::new(self, scope, name)
    }

    /// Creates a new descriptor for a namespace with the specified parent scope.
    pub fn create_namespace<S, N>(&self, parent: S, name: N, export_symbols: bool) -> DINamespace
    where
        S: Deref<Target = DIScope>,
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateNameSpace(
                self.as_raw(),
                parent.as_raw(),
                cstr!(name),
                name.len(),
                export_symbols.as_bool(),
            )
        }
        .into()
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
    ) -> DISubprogram
    where
        S: Deref<Target = DIScope>,
    {
        self.create_function_builder(scope, name, file, line_no, func_ty, scope_line)
            .build()
    }

    /// Create a new descriptor for the specified subprogram.
    pub fn create_function_builder<'a, S>(
        &'a self,
        scope: S,
        name: &'a str,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> DIFunctionBuilder<'a, S> {
        DIFunctionBuilder::new(self, scope, name, file, line_no, func_ty, scope_line)
    }

    /// Create a descriptor for a lexical block with the specified parent context.
    pub fn create_lexical_block<S>(&self, scope: S, file: DIFile, line: u32, column: u32) -> DILexicalBlock
    where
        S: Deref<Target = DIScope>,
    {
        unsafe { LLVMDIBuilderCreateLexicalBlock(self.as_raw(), scope.as_raw(), file.into_raw(), line, column) }.into()
    }

    /// Create a descriptor for a lexical block with a new file attached.
    pub fn create_lexical_block_file<S>(&self, scope: S, file: DIFile, discriminator: u32) -> DILexicalBlockFile
    where
        S: Deref<Target = DIScope>,
    {
        unsafe { LLVMDIBuilderCreateLexicalBlockFile(self.as_raw(), scope.as_raw(), file.into_raw(), discriminator) }
            .into()
    }

    /// Create a descriptor for an imported namespace. Suitable for e.g. C++ using declarations.
    pub fn create_imported_module_from_namespace<S>(
        &self,
        scope: S,
        ns: DINamespace,
        file: DIFile,
        line: u32,
    ) -> DIImportedEntity
    where
        S: Deref<Target = DIScope>,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromNamespace(
                self.as_raw(),
                scope.as_raw(),
                ns.into_raw(),
                file.into_raw(),
                line,
            )
        }
        .into()
    }

    /// Create a descriptor for an imported module that aliases another imported entity descriptor.
    pub fn create_imported_module_from_alias<S>(
        &self,
        scope: S,
        imported: DIImportedEntity,
        file: DIFile,
        line: u32,
    ) -> DIImportedEntity
    where
        S: Deref<Target = DIScope>,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromAlias(
                self.as_raw(),
                scope.as_raw(),
                imported.into_raw(),
                file.into_raw(),
                line,
            )
        }
        .into()
    }

    /// Create a descriptor for an imported module.
    pub fn create_imported_module_from_module<S>(
        &self,
        scope: S,
        m: DIModule,
        file: DIFile,
        line: u32,
    ) -> DIImportedEntity
    where
        S: Deref<Target = DIScope>,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromModule(
                self.as_raw(),
                scope.as_raw(),
                m.into_raw(),
                file.into_raw(),
                line,
            )
        }
        .into()
    }

    /// Create a descriptor for an imported function, type, or variable.
    ///
    /// Suitable for e.g. FORTRAN-style USE declarations.
    pub fn create_imported_declaration<S, D, N>(
        &self,
        scope: S,
        decl: D,
        file: DIFile,
        line: u32,
        name: N,
    ) -> DIImportedEntity
    where
        S: Deref<Target = DIScope>,
        D: Deref<Target = DINode>,
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateImportedDeclaration(
                self.as_raw(),
                scope.as_raw(),
                decl.as_raw(),
                file.as_raw(),
                line,
                cstr!(name),
                name.len(),
            )
        }
        .into()
    }

    /// Create a type array.
    pub fn get_or_create_type_array<I>(&self, elements: I) -> DITypeRefArray
    where
        I: IntoIterator<Item = DIType>,
    {
        let elements = elements.into_iter().map(|param| param.as_raw()).collect::<Vec<_>>();

        unsafe { LLVMDIBuilderGetOrCreateTypeArray(self.as_raw(), elements.as_ptr() as *mut _, elements.len()) }.into()
    }

    /// Create subroutine type.
    pub fn create_subroutine_type<I>(&self, file: DIFile, params: I, flags: LLVMDIFlags) -> DISubroutineType
    where
        I: IntoIterator<Item = DIType>,
    {
        let params = params.into_iter().map(|param| param.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateSubroutineType(
                self.as_raw(),
                file.into_raw(),
                params.as_ptr() as *mut _,
                params.len() as u32,
                flags,
            )
        }
        .into()
    }

    /// Create debugging information entry for an enumeration.
    pub fn create_enumeration_type<S, N, I, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
        ty: T,
    ) -> DICompositeType
    where
        S: Deref<Target = DIScope>,
        N: AsRef<str>,
        I: IntoIterator<Item = Metadata>,
        T: Deref<Target = DIType>,
    {
        let name = name.as_ref();
        let elements = elements.into_iter().map(|md| md.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateEnumerationType(
                self.as_raw(),
                scope.as_raw(),
                cstr!(name),
                name.len(),
                file.into_raw(),
                line,
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                ty.as_raw(),
            )
        }
        .into()
    }

    /// Create debugging information entry for a union.
    pub fn create_union_type<S, N, I>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
        flags: LLVMDIFlags,
    ) -> DICompositeType
    where
        S: Deref<Target = DIScope>,
        N: AsRef<str>,
        I: IntoIterator<Item = Metadata>,
    {
        let name = name.as_ref();
        let elements = elements.into_iter().map(|md| md.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateUnionType(
                self.as_raw(),
                scope.as_raw(),
                cstr!(name),
                name.len(),
                file.into_raw(),
                line,
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                flags,
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                0,
                ptr::null_mut(),
                0,
            )
        }
        .into()
    }

    /// Create debugging information entry for an array.
    pub fn create_array_type<T, I>(&self, layout: Layout, ty: T, subscripts: I) -> DICompositeType
    where
        T: Deref<Target = DIType>,
        I: IntoIterator<Item = Metadata>,
    {
        let subscripts = subscripts.into_iter().map(|md| md.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateArrayType(
                self.as_raw(),
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                ty.as_raw(),
                subscripts.as_ptr() as *mut _,
                subscripts.len() as u32,
            )
        }
        .into()
    }

    /// Create debugging information entry for a vector type.
    pub fn create_vector_type<T, I>(&self, layout: Layout, ty: T, subscripts: I) -> DICompositeType
    where
        T: Deref<Target = DIType>,
        I: IntoIterator<Item = Metadata>,
    {
        let subscripts = subscripts.into_iter().map(|md| md.as_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateVectorType(
                self.as_raw(),
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                ty.as_raw(),
                subscripts.as_ptr() as *mut _,
                subscripts.len() as u32,
            )
        }
        .into()
    }

    /// Create a DWARF unspecified type.
    pub fn create_unspecified_type<N>(&self, name: N) -> DIBasicType
    where
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe { LLVMDIBuilderCreateUnspecifiedType(self.as_raw(), cstr!(name), name.len()) }.into()
    }

    /// Create debugging information entry for a basic type.
    pub fn create_basic_type<N>(&self, name: N, bits: u64, encoding: Encoding) -> DIBasicType
    where
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe { LLVMDIBuilderCreateBasicType(self.as_raw(), cstr!(name), name.len(), bits, encoding.into()) }.into()
    }

    /// Create debugging information entry for a pointer.
    pub fn create_pointer_type<T>(&self, pointee_ty: T, size: u64) -> DIDerivedType
    where
        T: Deref<Target = DIType>,
    {
        self.create_pointer_builder(pointee_ty, size).build()
    }

    /// Create debugging information entry for a pointer.
    pub fn create_pointer_builder<'a, T>(&'a self, pointee_ty: T, size: u64) -> DIPointerBuilder<'a, T> {
        DIPointerBuilder::new(self, pointee_ty, size)
    }

    /// Create debugging information entry for a struct.
    pub fn create_struct_type<'a, S, N, I>(
        &'a self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> DICompositeType
    where
        S: Deref<Target = DIScope>,
        N: AsRef<str>,
        I: IntoIterator<Item = DIType>,
    {
        self.create_struct_builder(scope, name, file, line, layout, elements)
            .build()
    }

    /// Create debugging information entry for a struct.
    pub fn create_struct_builder<'a, S, N, I>(
        &'a self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> DIStructBuilder<'a, S, N, I> {
        DIStructBuilder::new(self, scope, name, file, line, layout, elements)
    }

    /// Create debugging information entry for a member.
    pub fn create_member_type<'a, S, N>(
        &'a self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset: u64,
    ) -> DIMemberBuilder<'a, S, N> {
        DIMemberBuilder::new(self, scope, name, file, line, layout, offset)
    }

    /// Create debugging information entry for a C++ static data member.
    pub fn create_static_member_builder<'a, S, N, T, V>(
        &'a self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        value: V,
    ) -> DIStaticMemberBuilder<'a, S, N, T, V> {
        DIStaticMemberBuilder::new(self, scope, name, file, line, ty, value)
    }

    /// Create debugging information entry for a pointer to member.
    pub fn create_member_pointer_builder<'a, T, C>(
        &'a self,
        pointee_ty: T,
        class_ty: C,
        size: u64,
    ) -> DIMemberPointerBuilder<'a, T, C> {
        DIMemberPointerBuilder::new(self, pointee_ty, class_ty, size)
    }

    /// Create a uniqued DIType* clone with FlagObjectPointer and FlagArtificial set.
    pub fn create_object_pointer_type<T>(&self, ty: T) -> DIType
    where
        T: Deref<Target = DIType>,
    {
        unsafe { LLVMDIBuilderCreateObjectPointerType(self.as_raw(), ty.as_raw()) }.into()
    }

    /// Create debugging information entry for a qualified type, e.g. 'const int'.
    pub fn create_qualified_type<T>(&self, type_modifier: TypeModifier, ty: T) -> DIDerivedType
    where
        T: Deref<Target = DIType>,
    {
        unsafe { LLVMDIBuilderCreateQualifiedType(self.as_raw(), type_modifier.into(), ty.as_raw()) }.into()
    }

    /// Create debugging information entry for a c++ style reference or rvalue reference type.
    pub fn create_reference_type<T>(&self, type_modifier: TypeModifier, ty: T) -> DIDerivedType
    where
        T: Deref<Target = DIType>,
    {
        unsafe { LLVMDIBuilderCreateReferenceType(self.as_raw(), type_modifier.into(), ty.as_raw()) }.into()
    }

    /// Create C++11 nullptr type.
    pub fn create_null_ptr_type(&self) -> DIBasicType {
        unsafe { LLVMDIBuilderCreateNullPtrType(self.as_raw()) }.into()
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

define_debug_info_type!(DINode);

define_debug_info_type!(DIScope, DINode);
define_debug_info_type!(DICompileUnit, DIScope);
define_debug_info_type!(DIFile, DIScope);
define_debug_info_type!(DIModule, DIScope);
define_debug_info_type!(DINamespace, DIScope);
define_debug_info_type!(DILexicalBlock, DIScope);
define_debug_info_type!(DILexicalBlockFile, DIScope);
define_debug_info_type!(DISubprogram, DIScope);

define_debug_info_type!(DIImportedEntity, DINode);
define_debug_info_type!(DILocation);

define_debug_info_type!(DITypeRefArray);

define_debug_info_type!(DIType, DIScope);
define_debug_info_type!(DICompositeType, DIType);
define_debug_info_type!(DIBasicType, DIType);
define_debug_info_type!(DIDerivedType, DIType);
define_debug_info_type!(DISubroutineType, DIType);

impl DILocation {
    /// Get the line number of this debug location.
    pub fn line(&self) -> usize {
        unsafe { LLVMDILocationGetLine(self.as_raw()) as usize }
    }

    /// Get the column number of this debug location.
    pub fn column(&self) -> usize {
        unsafe { LLVMDILocationGetColumn(self.as_raw()) as usize }
    }

    /// Get the local scope associated with this debug location.
    pub fn scope(&self) -> Option<DIScope> {
        unsafe { LLVMDILocationGetScope(self.as_raw()) }.ok()
    }
}

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
    builder: &'a DIBuilder,
    lang: DWARFSourceLanguage,
    file: DIFile,
    producer: &'a str,
    optimized: bool,
    flags: &'a str,
    runtime_version: u32,
    split_name: &'a str,
    emission_kind: DWARFEmissionKind,
    dwoid: u32,
    split_debug_inlining: bool,
    for_profiling: bool,
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

    pub fn build(self) -> DICompileUnit {
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
        .into()
    }
}

pub struct DIModuleBuilder<'a, S, N> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    config_macros: &'a str,
    include_path: &'a str,
    isysroot: &'a str,
}

impl<'a, S, N> DIModuleBuilder<'a, S, N> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N) -> Self {
        DIModuleBuilder {
            builder,
            scope,
            name,
            config_macros: "",
            include_path: "",
            isysroot: "",
        }
    }

    pub fn with_config_macros(mut self, config_macros: &'a str) -> Self {
        self.config_macros = config_macros;
        self
    }

    pub fn with_include_path(mut self, include_path: &'a str) -> Self {
        self.include_path = include_path;
        self
    }

    pub fn with_isysroot(mut self, isysroot: &'a str) -> Self {
        self.isysroot = isysroot;
        self
    }
}

impl<'a, S, N> DIModuleBuilder<'a, S, N>
where
    S: Deref<Target = DIScope>,
    N: AsRef<str>,
{
    pub fn build(self) -> DIModule {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateModule(
                self.builder.as_raw(),
                self.scope.as_raw(),
                cstr!(name),
                name.len(),
                cstr!(self.config_macros),
                self.config_macros.len(),
                cstr!(self.include_path),
                self.include_path.len(),
                cstr!(self.isysroot),
                self.isysroot.len(),
            )
        }
        .into()
    }
}

pub struct DIFunctionBuilder<'a, S> {
    builder: &'a DIBuilder,
    scope: S,
    name: &'a str,
    linkage_name: &'a str,
    file: DIFile,
    line_no: u32,
    func_ty: DISubroutineType,
    local_to_unit: bool,
    definition: bool,
    scope_line: u32,
    flags: LLVMDIFlags,
    optimized: bool,
}

impl<'a, S> DIFunctionBuilder<'a, S> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
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
}

impl<'a, S> DIFunctionBuilder<'a, S>
where
    S: Deref<Target = DIScope>,
{
    pub fn build(self) -> DISubprogram {
        unsafe {
            LLVMDIBuilderCreateFunction(
                self.builder.as_raw(),
                self.scope.as_raw(),
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
        .into()
    }
}

pub struct DIPointerBuilder<'a, T> {
    builder: &'a DIBuilder,
    pointee_ty: T,
    size: u64,
    align: Option<u32>,
    addr_space: Option<u32>,
    name: Option<&'a str>,
}

impl<'a, T> DIPointerBuilder<'a, T> {
    pub fn new(builder: &'a DIBuilder, pointee_ty: T, size: u64) -> Self {
        DIPointerBuilder {
            builder,
            pointee_ty,
            size,
            align: None,
            addr_space: None,
            name: None,
        }
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }

    pub fn with_addr_space(mut self, addr_space: u32) -> Self {
        self.addr_space = Some(addr_space);
        self
    }

    pub fn with_name(mut self, name: &'a str) -> Self {
        self.name = Some(name);
        self
    }
}

impl<'a, T> DIPointerBuilder<'a, T>
where
    T: Deref<Target = DIType>,
{
    /// Create debugging information entry for a pointer.
    pub fn build(self) -> DIDerivedType {
        let name = self.name.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreatePointerType(
                self.builder.as_raw(),
                self.pointee_ty.as_raw(),
                self.size,
                self.align.unwrap_or_default(),
                self.addr_space.unwrap_or_default(),
                cstr!(name),
                name.len(),
            )
        }
        .into()
    }
}

pub struct DIStructBuilder<'a, S, N, I> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    layout: Layout,
    elements: I,
    flags: LLVMDIFlags,
    derived_from: Option<DIType>,
    vtable: Option<DIType>,
    id: Option<&'a str>,
}

impl<'a, S, N, I> DIStructBuilder<'a, S, N, I> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> Self {
        DIStructBuilder {
            builder,
            scope,
            name,
            file,
            line,
            layout,
            elements,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            derived_from: None,
            vtable: None,
            id: None,
        }
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_derived_from<T>(mut self, derived_from: T) -> Self
    where
        T: Into<DIType>,
    {
        self.derived_from = Some(derived_from.into());
        self
    }

    pub fn with_vtable<T>(mut self, vtable: T) -> Self
    where
        T: Into<DIType>,
    {
        self.vtable = Some(vtable.into());
        self
    }

    pub fn with_unique_id(mut self, id: &'a str) -> Self {
        self.id = Some(id);
        self
    }
}

impl<'a, S, N, I> DIStructBuilder<'a, S, N, I>
where
    S: Deref<Target = DIScope>,
    N: AsRef<str>,
    I: IntoIterator<Item = DIType>,
{
    pub fn build(self) -> DICompositeType {
        let name = self.name.as_ref();
        let elements = self.elements.into_iter().map(|md| md.as_raw()).collect::<Vec<_>>();
        let id = self.id.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateStructType(
                self.builder.as_raw(),
                self.scope.as_raw(),
                cstr!(name),
                name.len(),
                self.file.as_raw(),
                self.line,
                self.layout.size() as u64 * 8,
                self.layout.align() as u32 * 8,
                self.flags,
                self.derived_from
                    .map(|ty| ty.as_raw())
                    .unwrap_or_else(|| ptr::null_mut()),
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                0,
                self.vtable.map(|ty| ty.as_raw()).unwrap_or_else(|| ptr::null_mut()),
                cstr!(id),
                id.len(),
            )
        }
        .into()
    }
}

pub struct DIMemberBuilder<'a, S, N> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    layout: Layout,
    offset: u64,
    flags: LLVMDIFlags,
    parent_ty: Option<DIType>,
}

impl<'a, S, N> DIMemberBuilder<'a, S, N> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset: u64,
    ) -> Self {
        DIMemberBuilder {
            builder,
            scope,
            name,
            file,
            line,
            layout,
            offset,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            parent_ty: None,
        }
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_parent_type<T>(mut self, parent_ty: T) -> Self
    where
        T: Into<DIType>,
    {
        self.parent_ty = Some(parent_ty.into());
        self
    }
}

impl<'a, S, N> DIMemberBuilder<'a, S, N>
where
    S: Deref<Target = DIScope>,
    N: AsRef<str>,
{
    pub fn build(self) -> DIDerivedType {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateMemberType(
                self.builder.as_raw(),
                self.scope.as_raw(),
                cstr!(name),
                name.len(),
                self.file.as_raw(),
                self.line,
                self.layout.size() as u64 * 8,
                self.layout.align() as u32 * 8,
                self.offset,
                self.flags,
                self.parent_ty.map(|ty| ty.as_raw()).unwrap_or_else(|| ptr::null_mut()),
            )
        }
        .into()
    }
}

pub struct DIStaticMemberBuilder<'a, S, N, T, V> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    ty: T,
    value: V,
    flags: LLVMDIFlags,
    align: Option<u32>,
}

impl<'a, S, N, T, V> DIStaticMemberBuilder<'a, S, N, T, V> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, file: DIFile, line: u32, ty: T, value: V) -> Self {
        DIStaticMemberBuilder {
            builder,
            scope,
            name,
            file,
            line,
            ty,
            value,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            align: None,
        }
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }
}

impl<'a, S, N, T, V> DIStaticMemberBuilder<'a, S, N, T, V>
where
    S: Deref<Target = DIScope>,
    N: AsRef<str>,
    T: Deref<Target = DIType>,
    V: Deref<Target = Constant>,
{
    pub fn build(self) -> DIDerivedType {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateStaticMemberType(
                self.builder.as_raw(),
                self.scope.as_raw(),
                cstr!(name),
                name.len(),
                self.file.as_raw(),
                self.line,
                self.ty.as_raw(),
                self.flags,
                self.value.as_raw(),
                self.align.unwrap_or_default(),
            )
        }
        .into()
    }
}

pub struct DIMemberPointerBuilder<'a, T, C> {
    builder: &'a DIBuilder,
    pointee_ty: T,
    class_ty: C,
    size: u64,
    align: Option<u32>,
    flags: LLVMDIFlags,
}

impl<'a, T, C> DIMemberPointerBuilder<'a, T, C> {
    pub fn new(builder: &'a DIBuilder, pointee_ty: T, class_ty: C, size: u64) -> Self {
        DIMemberPointerBuilder {
            builder,
            pointee_ty,
            class_ty,
            size,
            align: None,
            flags: LLVMDIFlags::LLVMDIFlagZero,
        }
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }
}

impl<'a, T, C> DIMemberPointerBuilder<'a, T, C>
where
    T: Deref<Target = DIType>,
    C: Deref<Target = DIType>,
{
    pub fn build(self) -> DIDerivedType {
        unsafe {
            LLVMDIBuilderCreateMemberPointerType(
                self.builder.as_raw(),
                self.pointee_ty.as_raw(),
                self.class_ty.as_raw(),
                self.size,
                self.align.unwrap_or_default(),
                self.flags,
            )
        }
        .into()
    }
}
