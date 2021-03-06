//! This file defines a bunch of datatypes that are useful for creating and
//! walking debug info in LLVM IR form. They essentially provide wrappers around
//! the information in the global variables that's needed when constructing the
//! DWARF information.

use std::alloc::Layout;
use std::fmt;
use std::ops::Range;
use std::path::Path;
use std::ptr::{self, NonNull};
use std::slice;
use std::str;

use crate::llvm::core::*;
use crate::llvm::debuginfo::*;
use crate::llvm::prelude::*;

use crate::metadata::{MDNode, MetadataAsValue};
use crate::prelude::*;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, AsResult, IntoRaw};

use self::dwarf::{DwAte, DwTag};

/// The kind of debug information to generate.
pub use llvm_sys::debuginfo::LLVMDWARFEmissionKind as EmissionKind;
/// Source languages known by DWARF.
pub use llvm_sys::debuginfo::LLVMDWARFSourceLanguage as SourceLanguage;

#[doc(hidden)]
pub mod dwarf {
    #![allow(non_camel_case_types)]

    /// The tag encodings for DIE attributes.
    /// See Section 7.5.3, Table 7.3.
    #[repr(u32)]
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum DwTag {
        DW_TAG_null = 0x00,

        DW_TAG_array_type = 0x01,
        DW_TAG_class_type = 0x02,
        DW_TAG_entry_point = 0x03,
        DW_TAG_enumeration_type = 0x04,
        DW_TAG_formal_parameter = 0x05,
        DW_TAG_imported_declaration = 0x08,
        DW_TAG_label = 0x0a,
        DW_TAG_lexical_block = 0x0b,
        DW_TAG_member = 0x0d,
        DW_TAG_pointer_type = 0x0f,
        DW_TAG_reference_type = 0x10,
        DW_TAG_compile_unit = 0x11,
        DW_TAG_string_type = 0x12,
        DW_TAG_structure_type = 0x13,
        DW_TAG_subroutine_type = 0x15,
        DW_TAG_typedef = 0x16,
        DW_TAG_union_type = 0x17,
        DW_TAG_unspecified_parameters = 0x18,
        DW_TAG_variant = 0x19,
        DW_TAG_common_block = 0x1a,
        DW_TAG_common_inclusion = 0x1b,
        DW_TAG_inheritance = 0x1c,
        DW_TAG_inlined_subroutine = 0x1d,
        DW_TAG_module = 0x1e,
        DW_TAG_ptr_to_member_type = 0x1f,
        DW_TAG_set_type = 0x20,
        DW_TAG_subrange_type = 0x21,
        DW_TAG_with_stmt = 0x22,
        DW_TAG_access_declaration = 0x23,
        DW_TAG_base_type = 0x24,
        DW_TAG_catch_block = 0x25,
        DW_TAG_const_type = 0x26,
        DW_TAG_constant = 0x27,
        DW_TAG_enumerator = 0x28,
        DW_TAG_file_type = 0x29,
        DW_TAG_friend = 0x2a,
        DW_TAG_namelist = 0x2b,
        DW_TAG_namelist_item = 0x2c,
        DW_TAG_packed_type = 0x2d,
        DW_TAG_subprogram = 0x2e,
        DW_TAG_template_type_parameter = 0x2f,
        DW_TAG_template_value_parameter = 0x30,
        DW_TAG_thrown_type = 0x31,
        DW_TAG_try_block = 0x32,
        DW_TAG_variant_part = 0x33,
        DW_TAG_variable = 0x34,
        DW_TAG_volatile_type = 0x35,

        // DWARF 3.
        DW_TAG_dwarf_procedure = 0x36,
        DW_TAG_restrict_type = 0x37,
        DW_TAG_interface_type = 0x38,
        DW_TAG_namespace = 0x39,
        DW_TAG_imported_module = 0x3a,
        DW_TAG_unspecified_type = 0x3b,
        DW_TAG_partial_unit = 0x3c,
        DW_TAG_imported_unit = 0x3d,
        DW_TAG_condition = 0x3f,
        DW_TAG_shared_type = 0x40,

        // DWARF 4.
        DW_TAG_type_unit = 0x41,
        DW_TAG_rvalue_reference_type = 0x42,
        DW_TAG_template_alias = 0x43,

        // DWARF 5.
        DW_TAG_coarray_type = 0x44,
        DW_TAG_generic_subrange = 0x45,
        DW_TAG_dynamic_type = 0x46,
        DW_TAG_atomic_type = 0x47,
        DW_TAG_call_site = 0x48,
        DW_TAG_call_site_parameter = 0x49,
        DW_TAG_skeleton_unit = 0x4a,
        DW_TAG_immutable_type = 0x4b,

        DW_TAG_lo_user = 0x4080,
        DW_TAG_hi_user = 0xffff,

        // SGI/MIPS extensions.
        DW_TAG_MIPS_loop = 0x4081,

        // HP extensions.
        DW_TAG_HP_array_descriptor = 0x4090,
        DW_TAG_HP_Bliss_field = 0x4091,
        DW_TAG_HP_Bliss_field_set = 0x4092,

        // GNU extensions.
        DW_TAG_format_label = 0x4101,
        DW_TAG_function_template = 0x4102,
        DW_TAG_class_template = 0x4103,
        DW_TAG_GNU_BINCL = 0x4104,
        DW_TAG_GNU_EINCL = 0x4105,
        DW_TAG_GNU_template_template_param = 0x4106,
        DW_TAG_GNU_template_parameter_pack = 0x4107,
        DW_TAG_GNU_formal_parameter_pack = 0x4108,
        DW_TAG_GNU_call_site = 0x4109,
        DW_TAG_GNU_call_site_parameter = 0x410a,

        DW_TAG_APPLE_property = 0x4200,

        // SUN extensions.
        DW_TAG_SUN_function_template = 0x4201,
        DW_TAG_SUN_class_template = 0x4202,
        DW_TAG_SUN_struct_template = 0x4203,
        DW_TAG_SUN_union_template = 0x4204,
        DW_TAG_SUN_indirect_inheritance = 0x4205,
        DW_TAG_SUN_codeflags = 0x4206,
        DW_TAG_SUN_memop_info = 0x4207,
        DW_TAG_SUN_omp_child_func = 0x4208,
        DW_TAG_SUN_rtti_descriptor = 0x4209,
        DW_TAG_SUN_dtor_info = 0x420a,
        DW_TAG_SUN_dtor = 0x420b,
        DW_TAG_SUN_f90_interface = 0x420c,
        DW_TAG_SUN_fortran_vax_structure = 0x420d,

        // ALTIUM extensions.
        DW_TAG_ALTIUM_circ_type = 0x5101,
        DW_TAG_ALTIUM_mwa_circ_type = 0x5102,
        DW_TAG_ALTIUM_rev_carry_type = 0x5103,
        DW_TAG_ALTIUM_rom = 0x5111,

        // Extensions for UPC.
        DW_TAG_upc_shared_type = 0x8765,
        DW_TAG_upc_strict_type = 0x8766,
        DW_TAG_upc_relaxed_type = 0x8767,

        // PGI (STMicroelectronics) extensions.
        DW_TAG_PGI_kanji_type = 0xa000,
        DW_TAG_PGI_interface_block = 0xa020,

        // Borland extensions.
        DW_TAG_BORLAND_property = 0xb000,
        DW_TAG_BORLAND_Delphi_string = 0xb001,
        DW_TAG_BORLAND_Delphi_dynamic_array = 0xb002,
        DW_TAG_BORLAND_Delphi_set = 0xb003,
        DW_TAG_BORLAND_Delphi_variant = 0xb004,
    }

    /// The encodings of the constants used in the `DW_AT_encoding` attribute.
    /// See Section 7.8, Table 7.11.
    #[repr(u32)]
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum DwAte {
        DW_ATE_address = 0x01,
        DW_ATE_boolean = 0x02,
        DW_ATE_complex_float = 0x03,
        DW_ATE_float = 0x04,
        DW_ATE_signed = 0x05,
        DW_ATE_signed_char = 0x06,
        DW_ATE_unsigned = 0x07,
        DW_ATE_unsigned_char = 0x08,

        // DWARF 3.
        DW_ATE_imaginary_float = 0x09,
        DW_ATE_packed_decimal = 0x0a,
        DW_ATE_numeric_string = 0x0b,
        DW_ATE_edited = 0x0c,
        DW_ATE_signed_fixed = 0x0d,
        DW_ATE_unsigned_fixed = 0x0e,
        DW_ATE_decimal_float = 0x0f,

        // DWARF 4.
        DW_ATE_UTF = 0x10,
        DW_ATE_UCS = 0x11,
        DW_ATE_ASCII = 0x12,

        DW_ATE_lo_user = 0x80,
        DW_ATE_hi_user = 0xff,
    }
}

/// Type modifier tags
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeModifier(DwTag);

impl From<TypeModifier> for u32 {
    fn from(tm: TypeModifier) -> u32 {
        tm.0 as u32
    }
}

pub mod type_modifier {
    //! A base or user-defined type may be modified in different ways in different languages.
    //! A type modifier is represented in DWARF by a debugging information entry with one of the tags.

    use super::dwarf::DwTag::*;
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

/// Base type encoding
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Encoding(DwAte);

impl From<Encoding> for u32 {
    fn from(encoding: Encoding) -> u32 {
        encoding.0 as u32
    }
}

pub mod encoding {
    //! The encodings of the constants used in the DW_AT_encoding attribute

    use super::dwarf::DwAte::*;
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
    pub fn create_debug_location<S>(
        &self,
        line: u32,
        column: u32,
        scope: S,
        inline_at: Option<DILocation>,
    ) -> DILocation
    where
        S: AsDILocalScope,
    {
        unsafe { LLVMDIBuilderCreateDebugLocation(self.as_raw(), line, column, scope.into_raw(), inline_at.into_raw()) }
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

impl IRBuilder {
    /// Set location information used by debugging information.
    pub fn set_current_debug_location(&self, loc: DILocation, ctxt: &Context) {
        unsafe { LLVMSetCurrentDebugLocation(self.as_raw(), loc.as_value(ctxt).into_raw()) }
    }

    /// Get location information used by debugging information.
    pub fn current_debug_location(&self) -> DILocation {
        MetadataAsValue::from(unsafe { LLVMGetCurrentDebugLocation(self.as_raw()) }).as_metadata()
    }

    /// If this builder has a current debug location, set it on the specified instruction.
    pub fn set_inst_debug_location<T>(&self, inst: T)
    where
        T: AsInstruction,
    {
        unsafe { LLVMSetInstDebugLocation(self.as_raw(), inst.into_raw()) }
    }
}

/// DIBuilder is useful for creating debugging information entries in LLVM IR form.
#[repr(transparent)]
#[derive(Debug)]
pub struct DIBuilder(LLVMDIBuilderRef);

inherit_from!(DIBuilder; LLVMDIBuilderRef);

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
    pub fn create_compile_unit<N>(&self, lang: SourceLanguage, file: DIFile, producer: N) -> DICompileUnit
    where
        N: AsRef<str>,
    {
        self.create_compile_unit_builder(lang, file, producer).build()
    }

    /// Create a CompileUnit which provides an anchor
    /// for all debugging information generated during this instance of compilation.
    pub fn create_compile_unit_builder<N>(
        &self,
        lang: SourceLanguage,
        file: DIFile,
        producer: N,
    ) -> DICompileUnitBuilder<N> {
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

        unsafe {
            LLVMDIBuilderCreateFile(
                self.as_raw(),
                filename.as_ptr() as *const _,
                filename.len(),
                dir.as_ptr() as *const _,
                dir.len(),
            )
        }
        .into()
    }

    /// Creates a new descriptor for a module with the specified parent scope.
    pub fn create_module<S, N>(&self, scope: Option<S>, name: N) -> DIModule
    where
        S: AsDIScope,
        N: AsRef<str>,
    {
        self.create_module_builder(scope, name).build()
    }

    /// Creates a new descriptor for a module with the specified parent scope.
    pub fn create_module_builder<S, N>(&self, scope: Option<S>, name: N) -> DIModuleBuilder<S, N> {
        DIModuleBuilder::new(self, scope, name)
    }

    /// Creates a new descriptor for a namespace with the specified parent scope.
    pub fn create_namespace<S, N>(&self, parent: S, name: N, export_symbols: bool) -> DINamespace
    where
        S: AsDIScope,
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateNameSpace(
                self.as_raw(),
                parent.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                export_symbols.as_bool(),
            )
        }
        .into()
    }

    /// Create a new descriptor for the specified subprogram.
    pub fn create_function<S, N>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> DISubprogram
    where
        S: AsDIScope,
        N: AsRef<str>,
    {
        self.create_function_builder(scope, name, file, line_no, func_ty, scope_line)
            .build()
    }

    /// Create a new descriptor for the specified subprogram.
    pub fn create_function_builder<S, N>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> DIFunctionBuilder<S, N> {
        DIFunctionBuilder::new(self, scope, name, file, line_no, func_ty, scope_line)
    }

    /// Create a descriptor for a lexical block with the specified parent context.
    pub fn create_lexical_block<S>(&self, scope: S, file: DIFile, line: u32, column: u32) -> DILexicalBlock
    where
        S: AsDIScope,
    {
        unsafe { LLVMDIBuilderCreateLexicalBlock(self.as_raw(), scope.into_raw(), file.into_raw(), line, column) }
            .into()
    }

    /// Create a descriptor for a lexical block with a new file attached.
    pub fn create_lexical_block_file<S>(&self, scope: S, file: DIFile, discriminator: u32) -> DILexicalBlockFile
    where
        S: AsDIScope,
    {
        unsafe { LLVMDIBuilderCreateLexicalBlockFile(self.as_raw(), scope.into_raw(), file.into_raw(), discriminator) }
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
        S: AsDIScope,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromNamespace(
                self.as_raw(),
                scope.into_raw(),
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
        S: AsDIScope,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromAlias(
                self.as_raw(),
                scope.into_raw(),
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
        S: AsDIScope,
    {
        unsafe {
            LLVMDIBuilderCreateImportedModuleFromModule(
                self.as_raw(),
                scope.into_raw(),
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
        S: AsDIScope,
        D: AsDINode,
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateImportedDeclaration(
                self.as_raw(),
                scope.into_raw(),
                decl.into_raw(),
                file.into_raw(),
                line,
                name.as_ptr() as *const _,
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
        let elements = elements.into_iter().map(|param| param.into_raw()).collect::<Vec<_>>();

        unsafe { LLVMDIBuilderGetOrCreateTypeArray(self.as_raw(), elements.as_ptr() as *mut _, elements.len()) }.into()
    }

    /// Create subroutine type.
    pub fn create_subroutine_type<I>(&self, file: DIFile, params: I, flags: LLVMDIFlags) -> DISubroutineType
    where
        I: IntoIterator<Item = DIType>,
    {
        let params = params.into_iter().map(|param| param.into_raw()).collect::<Vec<_>>();

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
        S: AsDIScope,
        N: AsRef<str>,
        I: IntoIterator<Item = Metadata>,
        T: AsDIType,
    {
        let name = name.as_ref();
        let elements = elements.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateEnumerationType(
                self.as_raw(),
                scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                file.into_raw(),
                line,
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                ty.into_raw(),
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
        S: AsDIScope,
        N: AsRef<str>,
        I: IntoIterator<Item = Metadata>,
    {
        let name = name.as_ref();
        let elements = elements.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateUnionType(
                self.as_raw(),
                scope.into_raw(),
                name.as_ptr() as *const _,
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
        T: AsDIType,
        I: IntoIterator<Item = Range<i64>>,
    {
        let subscripts = subscripts
            .into_iter()
            .map(|subscript| self.get_or_create_subrange(subscript).into_raw())
            .collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateArrayType(
                self.as_raw(),
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                ty.into_raw(),
                subscripts.as_ptr() as *mut _,
                subscripts.len() as u32,
            )
        }
        .into()
    }

    /// Create debugging information entry for a vector type.
    pub fn create_vector_type<T, I>(&self, layout: Layout, ty: T, subscripts: I) -> DICompositeType
    where
        T: AsDIType,
        I: IntoIterator<Item = Metadata>,
    {
        let subscripts = subscripts.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMDIBuilderCreateVectorType(
                self.as_raw(),
                layout.size() as u64 * 8,
                layout.align() as u32 * 8,
                ty.into_raw(),
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

        unsafe { LLVMDIBuilderCreateUnspecifiedType(self.as_raw(), name.as_ptr() as *const _, name.len()) }.into()
    }

    /// Create debugging information entry for a basic type.
    pub fn create_basic_type<N>(&self, name: N, bits: u64, encoding: Encoding) -> DIBasicType
    where
        N: AsRef<str>,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateBasicType(
                self.as_raw(),
                name.as_ptr() as *const _,
                name.len(),
                bits,
                encoding.into(),
            )
        }
        .into()
    }

    /// Create debugging information entry for a pointer.
    pub fn create_pointer_type<T>(&self, pointee_ty: T, size_in_bits: u64) -> DIDerivedType
    where
        T: AsDIType,
    {
        self.create_pointer_type_builder(pointee_ty, size_in_bits).build()
    }

    /// Create debugging information entry for a pointer.
    pub fn create_pointer_type_builder<T>(&self, pointee_ty: T, size_in_bits: u64) -> DIPointerTypeBuilder<T> {
        DIPointerTypeBuilder::new(self, pointee_ty, size_in_bits)
    }

    /// Create debugging information entry for a struct.
    pub fn create_struct_type<S, N, I>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> DICompositeType
    where
        S: AsDIScope,
        N: AsRef<str>,
        I: IntoIterator<Item = DIType>,
    {
        self.create_struct_type_builder(scope, name, file, line, layout, elements)
            .build()
    }

    /// Create debugging information entry for a struct.
    pub fn create_struct_type_builder<S, N, I>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> DIStructTypeBuilder<S, N, I> {
        DIStructTypeBuilder::new(self, scope, name, file, line, layout, elements)
    }

    /// Create debugging information entry for a member.
    pub fn create_member_type<S, N>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset: u64,
    ) -> DIMemberTypeBuilder<S, N> {
        DIMemberTypeBuilder::new(self, scope, name, file, line, layout, offset)
    }

    /// Create debugging information entry for a C++ static data member.
    pub fn create_static_member<S, N, T, V>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        value: V,
    ) -> DIDerivedType
    where
        S: AsDIScope,
        N: AsRef<str>,
        T: AsDIType,
        V: AsConstant,
    {
        self.create_static_member_builder(scope, name, file, line, ty, value)
            .build()
    }

    /// Create debugging information entry for a C++ static data member.
    pub fn create_static_member_builder<S, N, T, V>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        value: V,
    ) -> DIStaticMemberTypeBuilder<S, N, T, V> {
        DIStaticMemberTypeBuilder::new(self, scope, name, file, line, ty, value)
    }

    /// Create debugging information entry for a pointer to member.
    pub fn create_member_pointer_type<T, C>(&self, pointee_ty: T, class_ty: C, size_in_bits: u64) -> DIDerivedType
    where
        T: AsDIType,
        C: AsDIType,
    {
        self.create_member_pointer_type_builder(pointee_ty, class_ty, size_in_bits)
            .build()
    }

    /// Create debugging information entry for a pointer to member.
    pub fn create_member_pointer_type_builder<T, C>(
        &self,
        pointee_ty: T,
        class_ty: C,
        size_in_bits: u64,
    ) -> DIMemberPointerTypeBuilder<T, C> {
        DIMemberPointerTypeBuilder::new(self, pointee_ty, class_ty, size_in_bits)
    }

    /// Create a uniqued DIType* clone with FlagObjectPointer and FlagArtificial set.
    pub fn create_object_pointer_type<T>(&self, ty: T) -> DIType
    where
        T: AsDIType,
    {
        unsafe { LLVMDIBuilderCreateObjectPointerType(self.as_raw(), ty.into_raw()) }.into()
    }

    /// Create debugging information entry for a qualified type, e.g. 'const int'.
    pub fn create_qualified_type<T>(&self, type_modifier: TypeModifier, ty: T) -> DIDerivedType
    where
        T: AsDIType,
    {
        unsafe { LLVMDIBuilderCreateQualifiedType(self.as_raw(), type_modifier.into(), ty.into_raw()) }.into()
    }

    /// Create debugging information entry for a c++ style reference or rvalue reference type.
    pub fn create_reference_type<T>(&self, type_modifier: TypeModifier, ty: T) -> DIDerivedType
    where
        T: AsDIType,
    {
        unsafe { LLVMDIBuilderCreateReferenceType(self.as_raw(), type_modifier.into(), ty.into_raw()) }.into()
    }

    /// Create C++11 nullptr type.
    pub fn create_null_ptr_type(&self) -> DIBasicType {
        unsafe { LLVMDIBuilderCreateNullPtrType(self.as_raw()) }.into()
    }

    /// Create debugging information entry for a typedef.
    pub fn create_typedef<T, N, S>(&self, ty: T, name: N, file: DIFile, line: u32, scope: S) -> DIDerivedType
    where
        T: AsDIType,
        N: AsRef<str>,
        S: AsDIScope,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateTypedef(
                self.as_raw(),
                ty.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                file.into_raw(),
                line,
                scope.into_raw(),
            )
        }
        .into()
    }

    /// Create debugging information entry to establish inheritance relationship between two types.
    pub fn create_inheritance<T, B>(
        &self,
        ty: T,
        base_ty: B,
        base_offset: u64,
        vtable_ptr_offset: u32,
        flags: LLVMDIFlags,
    ) -> DIDerivedType
    where
        T: AsDIType,
        B: AsDIType,
    {
        unsafe {
            LLVMDIBuilderCreateInheritance(
                self.as_raw(),
                ty.into_raw(),
                base_ty.into_raw(),
                base_offset,
                vtable_ptr_offset,
                flags,
            )
        }
        .into()
    }

    /// Create a permanent forward-declared type.
    pub fn create_forward_decl<N, S>(&self, tag: DwTag, name: N, scope: S, file: DIFile, line: u32) -> DICompositeType
    where
        N: AsRef<str>,
        S: AsDIScope,
    {
        self.create_forward_decl_builder(tag, name, scope, file, line).build()
    }

    /// Create a permanent forward-declared type.
    pub fn create_forward_decl_builder<N, S>(
        &self,
        tag: DwTag,
        name: N,
        scope: S,
        file: DIFile,
        line: u32,
    ) -> DIForwardDeclBuilder<N, S> {
        DIForwardDeclBuilder::new(self, tag, name, scope, file, line)
    }

    /// Create a temporary forward-declared type.
    pub fn create_replaceable_composite_type<N, S>(
        &self,
        tag: DwTag,
        name: N,
        scope: S,
        file: DIFile,
        line: u32,
    ) -> DIReplaceableCompositeTypeBuilder<N, S>
    where
        N: AsRef<str>,
        S: AsDIScope,
    {
        DIReplaceableCompositeTypeBuilder::new(self, tag, name, scope, file, line)
    }

    /// Create debugging information entry for a bit field member.
    pub fn create_bit_field_member_type<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        size_in_bits: u64,
        offset_in_bits: u64,
        storage_offset_in_bits: u64,
        flags: LLVMDIFlags,
        ty: T,
    ) -> DIDerivedType
    where
        S: AsDIScope,
        N: AsRef<str>,
        T: AsDIType,
    {
        let name = name.as_ref();

        unsafe {
            LLVMDIBuilderCreateBitFieldMemberType(
                self.as_raw(),
                scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                file.into_raw(),
                line,
                size_in_bits,
                offset_in_bits,
                storage_offset_in_bits,
                flags,
                ty.into_raw(),
            )
        }
        .into()
    }

    /// Create debugging information entry for a class.
    pub fn create_class_type<S, N, I>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset_in_bits: u64,
        elements: I,
    ) -> DICompositeType
    where
        S: AsDIScope,
        N: AsRef<str>,
        I: IntoIterator<Item = DIType>,
    {
        self.create_class_type_builder(scope, name, file, line, layout, offset_in_bits, elements)
            .build()
    }

    /// Create debugging information entry for a class.
    pub fn create_class_type_builder<S, N, I>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset_in_bits: u64,
        elements: I,
    ) -> DIClassTypeBuilder<S, N, I> {
        DIClassTypeBuilder::new(self, scope, name, file, line, layout, offset_in_bits, elements)
    }

    /// Create a uniqued DIType* clone with FlagArtificial set.
    pub fn create_artificial_type<T>(&self, ty: T) -> DIType
    where
        T: AsDIType,
    {
        unsafe { LLVMDIBuilderCreateArtificialType(self.as_raw(), ty.into_raw()) }.into()
    }

    /// Create a descriptor for a value range.
    pub fn get_or_create_subrange(&self, range: Range<i64>) -> DISubrange {
        unsafe { LLVMDIBuilderGetOrCreateSubrange(self.as_raw(), range.start, range.end - range.start) }.into()
    }

    /// Create an array of DI Nodes.
    pub fn get_or_create_array<I>(&self, elements: I) -> DINodeArray
    where
        I: IntoIterator<Item = Metadata>,
    {
        let elements = elements.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();

        unsafe { LLVMDIBuilderGetOrCreateArray(self.as_raw(), elements.as_ptr() as *mut _, elements.len()) }.into()
    }

    /// Create a new descriptor for the specified variable which has a complex
    pub fn create_expression<T: AsRef<[i64]>>(&self, addr: T) -> DIExpression {
        let addr = addr.as_ref();

        unsafe { LLVMDIBuilderCreateExpression(self.as_raw(), addr.as_ptr() as *mut _, addr.len()) }.into()
    }

    /// Create a new descriptor for the specified variable that does not have an
    pub fn create_constant_value_expression(&self, value: i64) -> DIExpression {
        unsafe { LLVMDIBuilderCreateConstantValueExpression(self.as_raw(), value) }.into()
    }

    /// Create a new descriptor for the specified variable.
    pub fn create_global_variable_expression<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        local_to_unit: bool,
    ) -> DIGlobalVariableExpression
    where
        S: AsDIScope,
        N: AsRef<str>,
        T: AsDIType,
    {
        self.create_global_variable_expression_builder(scope, name, file, line, ty, local_to_unit)
            .build()
    }

    /// Create a new descriptor for the specified variable.
    pub fn create_global_variable_expression_builder<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        local_to_unit: bool,
    ) -> DIGlobalVariableExpressionBuilder<S, N, T> {
        DIGlobalVariableExpressionBuilder::new(self, scope, name, file, line, ty, local_to_unit)
    }

    /// Create a new descriptor for the specified global variable that is temporary
    pub fn create_temp_global_variable_forward_decl<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        local_to_unit: bool,
    ) -> DIGlobalVariableExpression
    where
        S: AsDIScope,
        N: AsRef<str>,
        T: AsDIType,
    {
        self.create_temp_global_variable_forward_decl_builder(scope, name, file, line, ty, local_to_unit)
            .build()
    }

    /// Create a new descriptor for the specified global variable that is temporary
    pub fn create_temp_global_variable_forward_decl_builder<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
        local_to_unit: bool,
    ) -> DITempGlobalVariableFwdDeclBuilder<S, N, T> {
        DITempGlobalVariableFwdDeclBuilder::new(self, scope, name, file, line, ty, local_to_unit)
    }

    /// Insert a new llvm.dbg.declare intrinsic call before the given instruction.
    pub fn insert_declare_before<V, I>(
        &self,
        storage: V,
        var: DILocalVariable,
        expr: DIExpression,
        loc: DILocation,
        inst: I,
    ) -> Instruction
    where
        V: AsValueRef,
        I: AsInstruction,
    {
        unsafe {
            LLVMDIBuilderInsertDeclareBefore(
                self.as_raw(),
                storage.into_raw(),
                var.as_raw(),
                expr.into_raw(),
                loc.into_raw(),
                inst.into_raw(),
            )
        }
        .into()
    }

    /// Insert a new llvm.dbg.declare intrinsic call at the end of the given basic block.
    /// If the basic block has a terminator instruction, the intrinsic is inserted before that terminator instruction.
    pub fn insert_declare_at_end<V>(
        &self,
        storage: V,
        var: DILocalVariable,
        expr: DIExpression,
        loc: DILocation,
        bb: BasicBlock,
    ) -> Instruction
    where
        V: AsValueRef,
    {
        unsafe {
            LLVMDIBuilderInsertDeclareAtEnd(
                self.as_raw(),
                storage.into_raw(),
                var.as_raw(),
                expr.into_raw(),
                loc.into_raw(),
                bb.into_raw(),
            )
        }
        .into()
    }

    /// Insert a new llvm.dbg.value intrinsic call before the given instruction.
    pub fn insert_debug_value_before<V, I>(
        &self,
        value: V,
        var: DILocalVariable,
        expr: DIExpression,
        loc: DILocation,
        inst: I,
    ) -> Instruction
    where
        V: AsValueRef,
        I: AsInstruction,
    {
        unsafe {
            LLVMDIBuilderInsertDbgValueBefore(
                self.as_raw(),
                value.into_raw(),
                var.as_raw(),
                expr.into_raw(),
                loc.into_raw(),
                inst.into_raw(),
            )
        }
        .into()
    }

    /// Insert a new llvm.dbg.value intrinsic call at the end of the given basic block.
    /// If the basic block has a terminator instruction, the intrinsic is inserted before that terminator instruction.
    pub fn insert_debug_value_at_end<V>(
        &self,
        value: V,
        var: DILocalVariable,
        expr: DIExpression,
        loc: DILocation,
        bb: BasicBlock,
    ) -> Instruction
    where
        V: AsValueRef,
    {
        unsafe {
            LLVMDIBuilderInsertDbgValueAtEnd(
                self.as_raw(),
                value.into_raw(),
                var.as_raw(),
                expr.into_raw(),
                loc.into_raw(),
                bb.into_raw(),
            )
        }
        .into()
    }

    /// Create a new descriptor for a local auto variable.
    pub fn create_auto_variable<S, N, T>(&self, scope: S, name: N, file: DIFile, line: u32, ty: T) -> DILocalVariable
    where
        S: AsDILocalScope,
        N: AsRef<str>,
        T: AsDIType,
    {
        self.create_auto_variable_builder(scope, name, file, line, ty).build()
    }

    /// Create a new descriptor for a local auto variable.
    pub fn create_auto_variable_builder<S, N, T>(
        &self,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        ty: T,
    ) -> DIAutoVariableBuilder<S, N, T> {
        DIAutoVariableBuilder::new(self, scope, name, file, line, ty)
    }

    /// Create a new descriptor for a function parameter variable.
    pub fn create_parameter_variable<S, N, T>(
        &self,
        scope: S,
        name: N,
        arg_no: u32,
        file: DIFile,
        line: u32,
        ty: T,
    ) -> DILocalVariable
    where
        S: AsDILocalScope,
        N: AsRef<str>,
        T: AsDIType,
    {
        self.create_parameter_variable_builder(scope, name, arg_no, file, line, ty)
            .build()
    }

    /// Create a new descriptor for a function parameter variable.
    pub fn create_parameter_variable_builder<S, N, T>(
        &self,
        scope: S,
        name: N,
        arg_no: u32,
        file: DIFile,
        line: u32,
        ty: T,
    ) -> DIParameterVariableBuilder<S, N, T> {
        DIParameterVariableBuilder::new(self, scope, name, arg_no, file, line, ty)
    }
}

macro_rules! def_class {
    (__def_struct $(#[$meta:meta])* $name:ident, $parent:ty) => {
        $(#[$meta])*
        #[repr(transparent)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub struct $name($parent);
    };

    ($(#[$meta:meta])* $name:ident) => {
        def_class!(__def_struct $(#[$meta])* $name, MDNode);

        inherit_from!($name, MDNode, Metadata; LLVMMetadataRef);
    };

    ($(#[$meta:meta])* $name:ident, $parent:ident $( , $base:ident )*) => {
        def_class!(__def_struct $(#[$meta])* $name, $parent);

        inherit_from!($name, $parent, $( $base, )* MDNode, Metadata; LLVMMetadataRef);
    };
}

macro_rules! def_node {
    ( $(#[$meta:meta])* $( $name:ident ),* ) => {
        def_class!( $(#[$meta])* $( $name, )* DINode);
    };
}

macro_rules! def_scope {
    ( $(#[$meta:meta])* $( $name:ident ),* ) => {
        def_node!( $(#[$meta])* $( $name, )* DIScope);
    };
}

macro_rules! def_local_scope {
    ( $(#[$meta:meta])* $( $name:ident ),* ) => {
        def_scope!($(#[$meta])*  $( $name, )* DILocalScope);
    };
}

macro_rules! def_type {
    ( $(#[$meta:meta])* $( $name:ident ),* ) => {
        def_node!( $(#[$meta])* $( $name, )* DIType);
    };
}

/// A typeref array.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DITypeRefArray(Metadata);

inherit_from!(DITypeRefArray, Metadata; LLVMMetadataRef);

def_class!(
    #[doc = "Tagged DWARF-like metadata node."]
    DINode
);

def_node!(
    #[doc = "Base class for scope-like contexts."]
    DIScope
);

def_scope!(
    #[doc = "Compile unit."]
    DICompileUnit
);
def_scope!(
    #[doc = "File."]
    DIFile
);
def_scope!(
    #[doc = "A module that has been imported by the compile unit."]
    DIModule
);
def_scope!(
    #[doc = "Namespace."]
    DINamespace
);

def_scope!(
    #[doc = "A scope for locals."]
    DILocalScope
);
def_local_scope!(
    #[doc = "A lexical block."]
    DILexicalBlock
);
def_local_scope!(
    #[doc = "A lexical block with a new file attached."]
    DILexicalBlockFile
);
def_local_scope!(
    #[doc = "Subprogram description."]
    DISubprogram
);

def_node!(
    #[doc = "An imported module (C++ using directive or similar)."]
    DIImportedEntity
);

def_class!(
    #[doc = "A debug location in source code, used for debug info and otherwise."]
    DILocation
);

def_class!(
    #[doc = "A node array."]
    DINodeArray
);
def_class!(
    #[doc = "A DWARF expression."]
    DIExpression
);
def_class!(
    #[doc = "A pair of DIGlobalVariable and DIExpression."]
    DIGlobalVariableExpression
);

def_node!(
    #[doc = "Array subrange."]
    DISubrange
);

def_node!(
    #[doc = "Base class for variables."]
    DIVariable
);
def_node!(
    #[doc = "Local variable."]
    DILocalVariable,
    DIVariable
);

def_scope!(
    #[doc = "Base class for types."]
    DIType
);
def_type!(
    #[doc = "Composite types."]
    DICompositeType
);
def_type!(
    #[doc = "Basic type, like 'int' or 'float'."]
    DIBasicType
);
def_type!(
    #[doc = "Derived types."]
    DIDerivedType
);
def_type!(
    #[doc = "Type array for a subprogram."]
    DISubroutineType
);

pub trait AsDINode: AsRaw<RawType = LLVMMetadataRef> + Into<DINode> {}
impl<T> AsDINode for T where T: AsRaw<RawType = LLVMMetadataRef> + Into<DINode> {}

pub trait AsDIScope: AsRaw<RawType = LLVMMetadataRef> + Into<DIScope> {}
impl<T> AsDIScope for T where T: AsRaw<RawType = LLVMMetadataRef> + Into<DIScope> {}

pub trait AsDILocalScope: AsRaw<RawType = LLVMMetadataRef> + Into<DILocalScope> {}
impl<T> AsDILocalScope for T where T: AsRaw<RawType = LLVMMetadataRef> + Into<DILocalScope> {}

pub trait AsDIType: AsRaw<RawType = LLVMMetadataRef> + Into<DIType> {}
impl<T> AsDIType for T where T: AsRaw<RawType = LLVMMetadataRef> + Into<DIType> {}

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

impl fmt::Display for DILocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "!DILocation(line: {}, column: {})", self.line(), self.column())
    }
}

#[macro_export]
macro_rules! ditypes {
    ($($x:expr),*) => (vec![ $($crate::debuginfo::DIType::from($x)),* ]);
    ($($x:expr,)*) => (vec![ $($crate::debuginfo::DIType::from($x)),* ]);
}

impl DIType {
    /// Get the name of this DIType.
    pub fn name(&self) -> Option<&str> {
        let mut len = 0;
        let data = unsafe { LLVMDITypeGetName(self.as_raw(), &mut len) };

        NonNull::new(data as *mut _)
            .map(|data| unsafe { slice::from_raw_parts(data.as_ptr(), len) })
            .and_then(|data| str::from_utf8(data).ok())
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

/// A builder used to create a compile unit
pub struct DICompileUnitBuilder<'a, N> {
    builder: &'a DIBuilder,
    lang: SourceLanguage,
    file: DIFile,
    producer: N,
    optimized: bool,
    flags: &'a str,
    runtime_version: u32,
    split_name: &'a str,
    emission_kind: EmissionKind,
    dwoid: u32,
    split_debug_inlining: bool,
    for_profiling: bool,
}

impl<'a, N> DICompileUnitBuilder<'a, N> {
    pub fn new(builder: &'a DIBuilder, lang: SourceLanguage, file: DIFile, producer: N) -> DICompileUnitBuilder<'a, N> {
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

    pub fn with_flags(mut self, flags: &'a str) -> Self {
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

    pub fn with_emission_kind(mut self, kind: EmissionKind) -> Self {
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
}

impl<'a, N> DICompileUnitBuilder<'a, N>
where
    N: AsRef<str>,
{
    pub fn build(self) -> DICompileUnit {
        let producer = self.producer.as_ref();

        unsafe {
            LLVMDIBuilderCreateCompileUnit(
                self.builder.as_raw(),
                self.lang,
                self.file.into_raw(),
                producer.as_ptr() as *const _,
                producer.len(),
                self.optimized.as_bool(),
                self.flags.as_ptr() as *const _,
                self.flags.len(),
                self.runtime_version,
                self.split_name.as_ptr() as *const _,
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

/// A builder used to create a module.
pub struct DIModuleBuilder<'a, S, N> {
    builder: &'a DIBuilder,
    scope: Option<S>,
    name: N,
    config_macros: &'a str,
    include_path: &'a str,
    isysroot: &'a str,
}

impl<'a, S, N> DIModuleBuilder<'a, S, N> {
    pub fn new(builder: &'a DIBuilder, scope: Option<S>, name: N) -> Self {
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
    S: AsDIScope,
    N: AsRef<str>,
{
    pub fn build(self) -> DIModule {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateModule(
                self.builder.as_raw(),
                self.scope
                    .map(|scope| scope.into_raw())
                    .unwrap_or(ptr::null_mut() as *mut _),
                name.as_ptr() as *const _,
                name.len(),
                self.config_macros.as_ptr() as *const _,
                self.config_macros.len(),
                self.include_path.as_ptr() as *const _,
                self.include_path.len(),
                self.isysroot.as_ptr() as *const _,
                self.isysroot.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a subprogram.
pub struct DIFunctionBuilder<'a, S, N> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    linkage_name: Option<&'a str>,
    file: DIFile,
    line_no: u32,
    func_ty: DISubroutineType,
    local_to_unit: bool,
    definition: bool,
    scope_line: u32,
    flags: LLVMDIFlags,
    optimized: bool,
}

impl<'a, S, N> DIFunctionBuilder<'a, S, N> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line_no: u32,
        func_ty: DISubroutineType,
        scope_line: u32,
    ) -> Self {
        DIFunctionBuilder {
            builder,
            scope,
            name,
            linkage_name: None,
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
        self.linkage_name = Some(name);
        self
    }

    pub fn local_to_unit(mut self) -> Self {
        self.local_to_unit = true;
        self
    }

    pub fn with_definition(mut self) -> Self {
        self.definition = true;
        self
    }

    pub fn with_optimized(mut self) -> Self {
        self.optimized = true;
        self
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }
}

impl<'a, S, N> DIFunctionBuilder<'a, S, N>
where
    S: AsDIScope,
    N: AsRef<str>,
{
    pub fn build(self) -> DISubprogram {
        let name = self.name.as_ref();
        let linkage_name = self.linkage_name.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateFunction(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                linkage_name.as_ptr() as *const _,
                linkage_name.len(),
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

/// A builder used to create a pointer type.
pub struct DIPointerTypeBuilder<'a, T> {
    builder: &'a DIBuilder,
    pointee_ty: T,
    size: u64,
    align: Option<u32>,
    addr_space: Option<u32>,
    name: Option<&'a str>,
}

impl<'a, T> DIPointerTypeBuilder<'a, T> {
    pub fn new(builder: &'a DIBuilder, pointee_ty: T, size: u64) -> Self {
        DIPointerTypeBuilder {
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

impl<'a, T> DIPointerTypeBuilder<'a, T>
where
    T: AsDIType,
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
                name.as_ptr() as *const _,
                name.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a structure type.
pub struct DIStructTypeBuilder<'a, S, N, I> {
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

impl<'a, S, N, I> DIStructTypeBuilder<'a, S, N, I> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        elements: I,
    ) -> Self {
        DIStructTypeBuilder {
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
        T: AsDIType,
    {
        self.derived_from = Some(derived_from.into());
        self
    }

    pub fn with_vtable<T>(mut self, vtable: T) -> Self
    where
        T: AsDIType,
    {
        self.vtable = Some(vtable.into());
        self
    }

    pub fn with_unique_id(mut self, id: &'a str) -> Self {
        self.id = Some(id);
        self
    }
}

impl<'a, S, N, I> DIStructTypeBuilder<'a, S, N, I>
where
    S: AsDIScope,
    N: AsRef<str>,
    I: IntoIterator<Item = DIType>,
{
    pub fn build(self) -> DICompositeType {
        let name = self.name.as_ref();
        let elements = self.elements.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();
        let id = self.id.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateStructType(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.file.into_raw(),
                self.line,
                self.layout.size() as u64 * 8,
                self.layout.align() as u32 * 8,
                self.flags,
                self.derived_from.into_raw(),
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                0,
                self.vtable.into_raw(),
                id.as_ptr() as *const _,
                id.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a member type.
pub struct DIMemberTypeBuilder<'a, S, N> {
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

impl<'a, S, N> DIMemberTypeBuilder<'a, S, N> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset: u64,
    ) -> Self {
        DIMemberTypeBuilder {
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
        T: AsDIType,
    {
        self.parent_ty = Some(parent_ty.into());
        self
    }
}

impl<'a, S, N> DIMemberTypeBuilder<'a, S, N>
where
    S: AsDIScope,
    N: AsRef<str>,
{
    pub fn build(self) -> DIDerivedType {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateMemberType(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.file.into_raw(),
                self.line,
                self.layout.size() as u64 * 8,
                self.layout.align() as u32 * 8,
                self.offset,
                self.flags,
                self.parent_ty.into_raw(),
            )
        }
        .into()
    }
}

/// A builder used to create a static member type.
pub struct DIStaticMemberTypeBuilder<'a, S, N, T, V> {
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

impl<'a, S, N, T, V> DIStaticMemberTypeBuilder<'a, S, N, T, V> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, file: DIFile, line: u32, ty: T, value: V) -> Self {
        DIStaticMemberTypeBuilder {
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

impl<'a, S, N, T, V> DIStaticMemberTypeBuilder<'a, S, N, T, V>
where
    S: AsDIScope,
    N: AsRef<str>,
    T: AsDIType,
    V: AsConstant,
{
    pub fn build(self) -> DIDerivedType {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateStaticMemberType(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.file.into_raw(),
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

/// A builder used to create a member pointer type.
pub struct DIMemberPointerTypeBuilder<'a, T, C> {
    builder: &'a DIBuilder,
    pointee_ty: T,
    class_ty: C,
    size: u64,
    align: Option<u32>,
    flags: LLVMDIFlags,
}

impl<'a, T, C> DIMemberPointerTypeBuilder<'a, T, C> {
    pub fn new(builder: &'a DIBuilder, pointee_ty: T, class_ty: C, size: u64) -> Self {
        DIMemberPointerTypeBuilder {
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

impl<'a, T, C> DIMemberPointerTypeBuilder<'a, T, C>
where
    T: AsDIType,
    C: AsDIType,
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

/// A builder used to create a forward declartion.
pub struct DIForwardDeclBuilder<'a, N, S> {
    builder: &'a DIBuilder,
    tag: DwTag,
    name: N,
    scope: S,
    file: DIFile,
    line: u32,
    runtime_lang: Option<u32>,
    size: Option<u64>,
    align: Option<u32>,
    id: Option<&'a str>,
}

impl<'a, N, S> DIForwardDeclBuilder<'a, N, S> {
    pub fn new(builder: &'a DIBuilder, tag: DwTag, name: N, scope: S, file: DIFile, line: u32) -> Self {
        DIForwardDeclBuilder {
            builder,
            tag,
            name,
            scope,
            file,
            line,
            runtime_lang: None,
            size: None,
            align: None,
            id: None,
        }
    }

    pub fn with_runtime_lang(mut self, lang: u32) -> Self {
        self.runtime_lang = Some(lang);
        self
    }

    pub fn with_size(mut self, bits: u64) -> Self {
        self.size = Some(bits);
        self
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }

    pub fn with_unique_id(mut self, id: &'a str) -> Self {
        self.id = Some(id);
        self
    }
}

impl<'a, N, S> DIForwardDeclBuilder<'a, N, S>
where
    N: AsRef<str>,
    S: AsDIScope,
{
    pub fn build(self) -> DICompositeType {
        let name = self.name.as_ref();
        let id = self.id.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateForwardDecl(
                self.builder.as_raw(),
                self.tag as u32,
                name.as_ptr() as *const _,
                name.len(),
                self.scope.into_raw(),
                self.file.into_raw(),
                self.line,
                self.runtime_lang.unwrap_or_default(),
                self.size.unwrap_or_default(),
                self.align.unwrap_or_default(),
                id.as_ptr() as *const _,
                id.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a replaceable composite type.
pub struct DIReplaceableCompositeTypeBuilder<'a, N, S> {
    builder: &'a DIBuilder,
    tag: DwTag,
    name: N,
    scope: S,
    file: DIFile,
    line: u32,
    runtime_lang: Option<u32>,
    size: Option<u64>,
    align: Option<u32>,
    flags: LLVMDIFlags,
    id: Option<&'a str>,
}

impl<'a, N, S> DIReplaceableCompositeTypeBuilder<'a, N, S> {
    pub fn new(builder: &'a DIBuilder, tag: DwTag, name: N, scope: S, file: DIFile, line: u32) -> Self {
        DIReplaceableCompositeTypeBuilder {
            builder,
            tag,
            name,
            scope,
            file,
            line,
            runtime_lang: None,
            size: None,
            align: None,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            id: None,
        }
    }

    pub fn with_runtime_lang(mut self, lang: u32) -> Self {
        self.runtime_lang = Some(lang);
        self
    }

    pub fn with_size(mut self, bits: u64) -> Self {
        self.size = Some(bits);
        self
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_unique_id(mut self, id: &'a str) -> Self {
        self.id = Some(id);
        self
    }
}

impl<'a, N, S> DIReplaceableCompositeTypeBuilder<'a, N, S>
where
    N: AsRef<str>,
    S: AsDIScope,
{
    pub fn build(self) -> DICompositeType {
        let name = self.name.as_ref();
        let id = self.id.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateReplaceableCompositeType(
                self.builder.as_raw(),
                self.tag as u32,
                name.as_ptr() as *const _,
                name.len(),
                self.scope.into_raw(),
                self.file.into_raw(),
                self.line,
                self.runtime_lang.unwrap_or_default(),
                self.size.unwrap_or_default(),
                self.align.unwrap_or_default(),
                self.flags,
                id.as_ptr() as *const _,
                id.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a class type.
pub struct DIClassTypeBuilder<'a, S, N, I> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    layout: Layout,
    offset_in_bits: u64,
    elements: I,
    flags: LLVMDIFlags,
    derived_from: Option<DIType>,
    vtable: Option<DIType>,
    template_params: Option<Metadata>,
    id: Option<&'a str>,
}

impl<'a, S, N, I> DIClassTypeBuilder<'a, S, N, I> {
    pub fn new(
        builder: &'a DIBuilder,
        scope: S,
        name: N,
        file: DIFile,
        line: u32,
        layout: Layout,
        offset_in_bits: u64,
        elements: I,
    ) -> Self {
        DIClassTypeBuilder {
            builder,
            scope,
            name,
            file,
            line,
            layout,
            offset_in_bits,
            elements,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            derived_from: None,
            vtable: None,
            template_params: None,
            id: None,
        }
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_derived_from<T>(mut self, derived_from: T) -> Self
    where
        T: AsDIType,
    {
        self.derived_from = Some(derived_from.into());
        self
    }

    pub fn with_vtable<T>(mut self, vtable: T) -> Self
    where
        T: AsDIType,
    {
        self.vtable = Some(vtable.into());
        self
    }

    pub fn with_unique_id(mut self, id: &'a str) -> Self {
        self.id = Some(id);
        self
    }
}

impl<'a, S, N, I> DIClassTypeBuilder<'a, S, N, I>
where
    S: AsDIScope,
    N: AsRef<str>,
    I: IntoIterator<Item = DIType>,
{
    pub fn build(self) -> DICompositeType {
        let name = self.name.as_ref();
        let elements = self.elements.into_iter().map(|md| md.into_raw()).collect::<Vec<_>>();
        let id = self.id.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateClassType(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.file.into_raw(),
                self.line,
                self.layout.size() as u64,
                self.layout.align() as u32,
                self.offset_in_bits,
                self.flags,
                self.derived_from.into_raw(),
                elements.as_ptr() as *mut _,
                elements.len() as u32,
                self.vtable.into_raw(),
                self.template_params.into_raw(),
                id.as_ptr() as *const _,
                id.len(),
            )
        }
        .into()
    }
}

/// A builder used to create a global variable expression.
pub struct DIGlobalVariableExpressionBuilder<'a, S, N, T> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    ty: T,
    local_to_unit: bool,
    linkage_name: Option<&'a str>,
    expr: Option<DIExpression>,
    decl: Option<Metadata>,
    align: Option<u32>,
}

impl<'a, S, N, T> DIGlobalVariableExpressionBuilder<'a, S, N, T> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, file: DIFile, line: u32, ty: T, local_to_unit: bool) -> Self {
        DIGlobalVariableExpressionBuilder {
            builder,
            scope,
            name,
            file,
            line,
            ty,
            local_to_unit,
            linkage_name: None,
            expr: None,
            decl: None,
            align: None,
        }
    }

    pub fn with_local_to_unit(mut self, v: bool) -> Self {
        self.local_to_unit = v;
        self
    }

    pub fn with_linkage_name(mut self, name: &'a str) -> Self {
        self.linkage_name = Some(name);
        self
    }

    pub fn with_expression(mut self, expr: DIExpression) -> Self {
        self.expr = Some(expr);
        self
    }

    pub fn with_declaration(mut self, decl: Metadata) -> Self {
        self.decl = Some(decl);
        self
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }
}

impl<'a, S, N, T> DIGlobalVariableExpressionBuilder<'a, S, N, T>
where
    S: AsDIScope,
    N: AsRef<str>,
    T: AsDIType,
{
    pub fn build(self) -> DIGlobalVariableExpression {
        let name = self.name.as_ref();
        let linkage_name = self.linkage_name.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateGlobalVariableExpression(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                linkage_name.as_ptr() as *const _,
                linkage_name.len(),
                self.file.into_raw(),
                self.line,
                self.ty.as_raw(),
                self.local_to_unit.as_bool(),
                self.expr.into_raw(),
                self.decl.into_raw(),
                self.align.unwrap_or_default(),
            )
        }
        .into()
    }
}

/// A builder used to create a tempary global variable forward declaration;
pub struct DITempGlobalVariableFwdDeclBuilder<'a, S, N, T> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    ty: T,
    local_to_unit: bool,
    linkage_name: Option<&'a str>,
    decl: Option<Metadata>,
    align: Option<u32>,
}

impl<'a, S, N, T> DITempGlobalVariableFwdDeclBuilder<'a, S, N, T> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, file: DIFile, line: u32, ty: T, local_to_unit: bool) -> Self {
        DITempGlobalVariableFwdDeclBuilder {
            builder,
            scope,
            name,
            file,
            line,
            ty,
            local_to_unit,
            linkage_name: None,
            decl: None,
            align: None,
        }
    }

    pub fn with_local_to_unit(mut self, v: bool) -> Self {
        self.local_to_unit = v;
        self
    }

    pub fn with_linkage_name(mut self, name: &'a str) -> Self {
        self.linkage_name = Some(name);
        self
    }

    pub fn with_declaration(mut self, decl: Metadata) -> Self {
        self.decl = Some(decl);
        self
    }

    pub fn with_align(mut self, bits: u32) -> Self {
        self.align = Some(bits);
        self
    }
}

impl<'a, S, N, T> DITempGlobalVariableFwdDeclBuilder<'a, S, N, T>
where
    S: AsDIScope,
    N: AsRef<str>,
    T: AsDIType,
{
    pub fn build(self) -> DIGlobalVariableExpression {
        let name = self.name.as_ref();
        let linkage_name = self.linkage_name.unwrap_or_default();

        unsafe {
            LLVMDIBuilderCreateTempGlobalVariableFwdDecl(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                linkage_name.as_ptr() as *const _,
                linkage_name.len(),
                self.file.into_raw(),
                self.line,
                self.ty.as_raw(),
                self.local_to_unit.as_bool(),
                self.decl.into_raw(),
                self.align.unwrap_or_default(),
            )
        }
        .into()
    }
}

/// A builder used to create a auto variable.
pub struct DIAutoVariableBuilder<'a, S, N, T> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    file: DIFile,
    line: u32,
    ty: T,
    always_preserve: bool,
    flags: LLVMDIFlags,
    align: Option<u32>,
}

impl<'a, S, N, T> DIAutoVariableBuilder<'a, S, N, T> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, file: DIFile, line: u32, ty: T) -> Self {
        DIAutoVariableBuilder {
            builder,
            scope,
            name,
            file,
            line,
            ty,
            always_preserve: false,
            flags: LLVMDIFlags::LLVMDIFlagZero,
            align: None,
        }
    }

    pub fn with_always_preserve(mut self) -> Self {
        self.always_preserve = true;
        self
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

impl<'a, S, N, T> DIAutoVariableBuilder<'a, S, N, T>
where
    S: AsDILocalScope,
    N: AsRef<str>,
    T: AsDIType,
{
    pub fn build(self) -> DILocalVariable {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateAutoVariable(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.file.into_raw(),
                self.line,
                self.ty.as_raw(),
                self.always_preserve.as_bool(),
                self.flags,
                self.align.unwrap_or_default(),
            )
        }
        .into()
    }
}

/// A builder used to create a parameter variable.
pub struct DIParameterVariableBuilder<'a, S, N, T> {
    builder: &'a DIBuilder,
    scope: S,
    name: N,
    arg_no: u32,
    file: DIFile,
    line: u32,
    ty: T,
    always_preserve: bool,
    flags: LLVMDIFlags,
}

impl<'a, S, N, T> DIParameterVariableBuilder<'a, S, N, T> {
    pub fn new(builder: &'a DIBuilder, scope: S, name: N, arg_no: u32, file: DIFile, line: u32, ty: T) -> Self {
        DIParameterVariableBuilder {
            builder,
            scope,
            name,
            arg_no,
            file,
            line,
            ty,
            always_preserve: false,
            flags: LLVMDIFlags::LLVMDIFlagZero,
        }
    }

    pub fn with_always_preserve(mut self) -> Self {
        self.always_preserve = true;
        self
    }

    pub fn with_flags(mut self, flags: LLVMDIFlags) -> Self {
        self.flags = flags;
        self
    }
}

impl<'a, S, N, T> DIParameterVariableBuilder<'a, S, N, T>
where
    S: AsDILocalScope,
    N: AsRef<str>,
    T: AsDIType,
{
    pub fn build(self) -> DILocalVariable {
        let name = self.name.as_ref();

        unsafe {
            LLVMDIBuilderCreateParameterVariable(
                self.builder.as_raw(),
                self.scope.into_raw(),
                name.as_ptr() as *const _,
                name.len(),
                self.arg_no,
                self.file.into_raw(),
                self.line,
                self.ty.as_raw(),
                self.always_preserve.as_bool(),
                self.flags,
            )
        }
        .into()
    }
}
