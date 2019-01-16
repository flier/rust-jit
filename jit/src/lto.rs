//! This file declares functions and classes used to support LTO.
//!
//! It is intended to be used both by LTO classes as well as by clients (gold-plugin)
//! that don't utilize the LTO code generator interfaces.

use std::borrow::Cow;
use std::iter::ExactSizeIterator;
use std::mem;
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::ptr::{self, NonNull};
use std::result::Result as StdResult;
use std::slice;
use std::time::Duration;

use llvm_sys::lto::*;

use crate::errors::Result;
use crate::utils::{AsRaw, AsResult, IntoRaw, UncheckedCStr};

trait AsBool {
    type Boolean;

    fn as_bool(self) -> Self::Boolean;
}

impl AsBool for bool {
    type Boolean = lto_bool_t;

    fn as_bool(self) -> Self::Boolean {
        if self {
            1
        } else {
            0
        }
    }
}

impl AsBool for lto_bool_t {
    type Boolean = bool;

    fn as_bool(self) -> Self::Boolean {
        self != 0
    }
}

impl AsResult<()> for lto_bool_t {
    fn is_ok(self) -> bool {
        self.as_bool()
    }

    fn ok_or_else<F, E>(self, err: F) -> StdResult<(), E>
    where
        F: FnOnce() -> E,
    {
        if self.is_ok() {
            Ok(())
        } else {
            Err(err())
        }
    }
}

/// Returns a printable string.
pub fn version() -> Cow<'static, str> {
    unsafe { lto_get_version() }.as_str()
}

/// Returns the runtime API version.
pub fn api_version() -> u32 {
    unsafe { lto_api_version() }
}

/// Returns the last error string or None if last operation was successful.
pub fn error_message() -> Option<Cow<'static, str>> {
    NonNull::new(unsafe { lto_get_error_message() as *mut _ }).map(|msg| UncheckedCStr::as_str(msg.as_ptr()))
}

fn report_last_error() -> failure::Error {
    error_message()
        .map(failure::err_msg)
        .unwrap_or_else(|| failure::err_msg("unexpected LTO error"))
}

/// A loadable object file.
pub trait ObjectFile {
    /// Checks if a file or memory is a loadable object file.
    fn is_object_file(&self) -> bool;

    /// Checks if a file or memory is a loadable object compiled for requested target.
    fn is_object_file_for_target<S: AsRef<str>>(&self, target_triple_prefix: S) -> bool;

    /// Loads an object file from disk or memory.
    fn create(&self) -> Result<Module>;
}

impl ObjectFile for Path {
    fn is_object_file(&self) -> bool {
        unsafe { lto_module_is_object_file(cpath!(self)) }.as_bool()
    }

    fn is_object_file_for_target<S: AsRef<str>>(&self, target_triple_prefix: S) -> bool {
        unsafe { lto_module_is_object_file_for_target(cpath!(self), cstr!(target_triple_prefix)) }.as_bool()
    }

    fn create(&self) -> Result<Module> {
        unsafe { lto_module_create(cpath!(self)) }.ok_or_else(report_last_error)
    }
}

impl ObjectFile for [u8] {
    fn is_object_file(&self) -> bool {
        unsafe { lto_module_is_object_file_in_memory(self.as_ptr() as *const _, self.len()) }.as_bool()
    }

    fn is_object_file_for_target<S: AsRef<str>>(&self, target_triple_prefix: S) -> bool {
        unsafe {
            lto_module_is_object_file_in_memory_for_target(
                self.as_ptr() as *const _,
                self.len(),
                cstr!(target_triple_prefix),
            )
            .as_bool()
        }
    }

    fn create(&self) -> Result<Module> {
        unsafe { lto_module_create_from_memory(self.as_ptr() as *const _, self.len()) }.ok_or_else(report_last_error)
    }
}

/// Checks if a file or memory is a loadable object file.
pub fn is_object_file<T: AsRef<O>, O: ObjectFile>(target: T) -> bool {
    target.as_ref().is_object_file()
}

/// Checks if a file or memory is a loadable object compiled for requested target.
pub fn is_object_file_for_target<T: AsRef<O>, O: ObjectFile, S: AsRef<str>>(
    target: T,
    target_triple_prefix: S,
) -> bool {
    target.as_ref().is_object_file_for_target(target_triple_prefix)
}

/// A loaded object module.
#[repr(transparent)]
#[derive(Debug)]
pub struct Module(lto_module_t);

inherit_from!(Module; lto_module_t);

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { lto_module_dispose(self.as_raw()) }
    }
}

impl Module {
    /// Loads an object file from disk or memory.
    pub fn create<T: AsRef<O>, O: ObjectFile>(object_file: T) -> Result<Self> {
        object_file.as_ref().create()
    }

    /// Loads an object file from memory with an extra path argument.
    pub fn with_path<T: AsRef<[u8]>, P: AsRef<Path>>(mem: T, path: P) -> Result<Self> {
        let mem = mem.as_ref();

        unsafe { lto_module_create_from_memory_with_path(mem.as_ptr() as *const _, mem.len(), cpath!(path)) }
            .ok_or_else(report_last_error)
    }

    /// Loads an object file in its own context.
    pub fn with_local_context<T: AsRef<[u8]>, P: AsRef<Path>>(mem: T, path: P) -> Result<Self> {
        let mem = mem.as_ref();

        unsafe { lto_module_create_in_local_context(mem.as_ptr() as *const _, mem.len(), cpath!(path)) }
            .ok_or_else(report_last_error)
    }

    /// Loads an object file from disk. The seek point of fd is not preserved.
    pub fn from_fd<T: AsRawFd, P: AsRef<Path>>(f: &T, path: P, file_size: usize) -> Result<Self> {
        unsafe { lto_module_create_from_fd(f.as_raw_fd(), cpath!(path), file_size) }.ok_or_else(report_last_error)
    }

    /// Loads an object file from disk. The seek point of fd is not preserved.
    pub fn from_fd_and_offset<T: AsRef<F>, F: AsRawFd, P: AsRef<Path>>(
        f: T,
        path: P,
        file_size: usize,
        map_size: usize,
        offset: i64,
    ) -> Result<Self> {
        unsafe {
            lto_module_create_from_fd_at_offset(f.as_ref().as_raw_fd(), cpath!(path), file_size, map_size, offset)
        }
        .ok_or_else(report_last_error)
    }

    /// Returns triple string which the object module was compiled under.
    pub fn target_triple(&self) -> Cow<str> {
        unsafe { lto_module_get_target_triple(self.as_raw()) }.as_str()
    }

    /// Sets triple string with which the object will be codegened.
    pub fn set_target_triple<S: AsRef<str>>(&self, triple: S) {
        unsafe { lto_module_set_target_triple(self.as_raw(), cstr!(triple)) }
    }

    /// Returns the symbols in the object module.
    pub fn symbols(&self) -> Symbols {
        let len = unsafe { lto_module_get_num_symbols(self.as_raw()) };

        Symbols {
            module: self,
            len,
            pos: 0,
        }
    }

    /// Returns the module's linker options.
    pub fn linkeropts(&self) -> Cow<str> {
        unsafe { lto_module_get_linkeropts(self.as_raw()) }.as_str()
    }
}

bitflags! {
    /// The attributes of the symbol.
    pub struct SymbolAttrs: u32 {
        const LTO_SYMBOL_ALIGNMENT_MASK = 31;
        const LTO_SYMBOL_PERMISSIONS_MASK = 224;
        const LTO_SYMBOL_PERMISSIONS_CODE = 160;
        const LTO_SYMBOL_PERMISSIONS_DATA = 192;
        const LTO_SYMBOL_PERMISSIONS_RODATA = 128;
        const LTO_SYMBOL_DEFINITION_MASK = 1792;
        const LTO_SYMBOL_DEFINITION_REGULAR = 256;
        const LTO_SYMBOL_DEFINITION_TENTATIVE = 512;
        const LTO_SYMBOL_DEFINITION_WEAK = 768;
        const LTO_SYMBOL_DEFINITION_UNDEFINED = 1024;
        const LTO_SYMBOL_DEFINITION_WEAKUNDEF = 1280;
        const LTO_SYMBOL_SCOPE_MASK = 14336;
        const LTO_SYMBOL_SCOPE_INTERNAL = 2048;
        const LTO_SYMBOL_SCOPE_HIDDEN = 0x1000;
        const LTO_SYMBOL_SCOPE_PROTECTED = 0x2000;
        const LTO_SYMBOL_SCOPE_DEFAULT = 0x1800;
        const LTO_SYMBOL_SCOPE_DEFAULT_CAN_BE_HIDDEN = 0x2800;
        const LTO_SYMBOL_COMDAT = 0x4000;
        const LTO_SYMBOL_ALIAS = 0x8000;
    }
}

impl SymbolAttrs {
    pub fn align(&self) -> usize {
        (*self & SymbolAttrs::LTO_SYMBOL_ALIGNMENT_MASK).bits as usize
    }
}

/// An iterator over the symbols of a module.
pub struct Symbols<'a> {
    module: &'a Module,
    len: u32,
    pos: u32,
}

impl<'a> Iterator for Symbols<'a> {
    type Item = (Cow<'a, str>, SymbolAttrs);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.len {
            let name = unsafe { lto_module_get_symbol_name(self.module.as_raw(), self.pos) }.as_str();
            let attrs = SymbolAttrs::from_bits_truncate(unsafe {
                lto_module_get_symbol_attribute(self.module.as_raw(), self.pos) as u32
            });

            self.pos += 1;

            Some((name, attrs))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len as usize, Some(self.len as usize))
    }
}

impl<'a> ExactSizeIterator for Symbols<'a> {
    fn len(&self) -> usize {
        self.len as usize
    }
}

/// The debug model.
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DebugModel {
    Dwarf = lto_debug_model::LTO_DEBUG_MODEL_DWARF as u32,
}

/// The PIC code model.
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CodeGenModel {
    Static = lto_codegen_model::LTO_CODEGEN_PIC_MODEL_STATIC as u32,
    Dynamic = lto_codegen_model::LTO_CODEGEN_PIC_MODEL_DYNAMIC as u32,
    DynamicNoPic = lto_codegen_model::LTO_CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC as u32,
    Default = lto_codegen_model::LTO_CODEGEN_PIC_MODEL_DEFAULT as u32,
}

/// Diagnostic severity.
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DiagnosticSeverity {
    Error = lto_codegen_diagnostic_severity_t::LTO_DS_ERROR as u32,
    Warning = lto_codegen_diagnostic_severity_t::LTO_DS_WARNING as u32,
    Remark = lto_codegen_diagnostic_severity_t::LTO_DS_REMARK as u32,
    Note = lto_codegen_diagnostic_severity_t::LTO_DS_NOTE as u32,
}

/// Diagnostic handler type.
pub type DiagnosticHandler<T> = fn(severity: DiagnosticSeverity, diag: &str, data: Option<&T>);

struct DiagnosticContext<T> {
    handler: DiagnosticHandler<T>,
    data: Option<T>,
}

extern "C" fn diagnostic_handler_stub<T>(
    severity: lto_codegen_diagnostic_severity_t,
    diag: *const ::libc::c_char,
    ctxt: *mut ::libc::c_void,
) {
    unsafe {
        let ctxt = mem::ManuallyDrop::new(Box::from_raw(ctxt as *mut DiagnosticContext<T>));

        (ctxt.handler)(mem::transmute(severity), &diag.as_str(), ctxt.data.as_ref());
    }
}

/// A code generator
#[repr(transparent)]
#[derive(Debug)]
pub struct CodeGenerator(lto_code_gen_t);

inherit_from!(CodeGenerator; lto_code_gen_t);

impl Drop for CodeGenerator {
    fn drop(&mut self) {
        unsafe { lto_codegen_dispose(self.as_raw()) }
    }
}

impl CodeGenerator {
    /// Instantiates a code generator.
    pub fn create() -> Result<Self> {
        unsafe { lto_codegen_create() }.ok_or_else(report_last_error)
    }

    /// Instantiate a code generator in its own context.
    pub fn with_local_context() -> Result<Self> {
        unsafe { lto_codegen_create_in_local_context() }.ok_or_else(report_last_error)
    }

    /// Set a diagnostic handler and the related context.
    pub fn set_diagnostic_handler<T>(&self, handler: DiagnosticHandler<T>, data: Option<T>) {
        let ctxt = Box::into_raw(Box::new(DiagnosticContext { handler, data }));

        unsafe { lto_codegen_set_diagnostic_handler(self.as_raw(), Some(diagnostic_handler_stub::<T>), ctxt as *mut _) }
    }

    /// Add an object module to the set of modules for which code will be generated.
    pub fn add_module(&self, module: &Module) -> Result<()> {
        unsafe { lto_codegen_add_module(self.as_raw(), module.as_raw()) }.ok_or_else(report_last_error)
    }

    /// Sets the object module for code generation.
    ///
    /// This will transfer the ownership of the module to the code generator.
    pub fn set_module(&self, module: Module) {
        unsafe { lto_codegen_set_module(self.as_raw(), module.into_raw()) }
    }

    /// Sets if debug info should be generated.
    pub fn set_debug_mode(&self, model: Option<DebugModel>) -> Result<()> {
        unsafe {
            lto_codegen_set_debug_model(
                self.as_raw(),
                model
                    .map(|m| mem::transmute(m))
                    .unwrap_or(lto_debug_model::LTO_DEBUG_MODEL_NONE),
            )
        }
        .ok_or_else(report_last_error)
    }

    /// Sets which PIC code model to generated.
    pub fn set_pic_mode(&self, model: CodeGenModel) -> Result<()> {
        unsafe { lto_codegen_set_pic_model(self.as_raw(), mem::transmute(model)) }.ok_or_else(report_last_error)
    }

    /// Sets the cpu to generate code for.
    pub fn set_cpu<S: AsRef<str>>(&self, cpu: S) {
        unsafe { lto_codegen_set_cpu(self.as_raw(), cstr!(cpu)) }
    }

    /// Adds to a list of all global symbols that must exist in the final generated code.
    ///
    /// If a function is not listed there, it might be inlined into every usage and optimized away.
    pub fn add_must_preserve_symbol<S: AsRef<str>>(&self, symbol: S) {
        unsafe { lto_codegen_add_must_preserve_symbol(self.as_raw(), cstr!(symbol)) }
    }

    /// Writes a new object file at the specified path that contains the merged contents of all modules added so far.
    pub fn write_merged_modules<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        unsafe { lto_codegen_write_merged_modules(self.as_raw(), cpath!(path)) }.ok_or_else(report_last_error)
    }

    /// Generates code for all added modules into one native object file.
    pub fn compile(&self) -> Result<&[u8]> {
        let mut len = 0;

        unsafe {
            lto_codegen_compile(self.as_raw(), &mut len)
                .ok_or_else(report_last_error)
                .map(|data: *const libc::c_void| slice::from_raw_parts(data as *const _, len))
        }
    }

    /// Generates code for all added modules into one native object file.
    pub fn compile_to_file(&self) -> Result<PathBuf> {
        let mut path = ptr::null();

        unsafe { lto_codegen_compile_to_file(self.as_raw(), &mut path) }
            .ok_or_else(report_last_error)
            .map(|_| path.as_str().as_ref().to_owned().into())
    }

    /// Runs optimization for the merged module.
    pub fn optimize(&self) -> Result<()> {
        unsafe { lto_codegen_optimize(self.as_raw()) }.ok_or_else(report_last_error)
    }

    /// Generates code for the optimized merged module into one native object file.
    ///
    /// It will not run any IR optimizations on the merged module.
    pub fn compile_optimized(&self) -> Result<&[u8]> {
        let mut len = 0;

        unsafe {
            lto_codegen_compile_optimized(self.as_raw(), &mut len)
                .ok_or_else(report_last_error)
                .map(|data: *mut libc::c_void| slice::from_raw_parts(data as *const _, len))
        }
    }

    /// Sets options to help debug codegen bugs.
    pub fn set_debug_options(&self, options: &[&str]) {
        let options = itertools::join(options, "\t");

        unsafe { lto_codegen_debug_options(self.as_raw(), cstr!(&options)) }
    }

    /// Sets if we should run internalize pass during optimization and code generation.
    pub fn set_should_internalize(&self, should_internalize: bool) {
        unsafe { lto_codegen_set_should_internalize(self.as_raw(), should_internalize.as_bool()) }
    }

    /// Set whether to embed uselists in bitcode.
    pub fn set_should_embed_uselists(&self, should_embed_uselists: bool) {
        unsafe { lto_codegen_set_should_embed_uselists(self.as_raw(), should_embed_uselists.as_bool()) }
    }
}

/// A thin code generator
#[repr(transparent)]
#[derive(Debug)]
pub struct ThinCodeGenerator(thinlto_code_gen_t);

inherit_from!(ThinCodeGenerator; thinlto_code_gen_t);

impl Drop for ThinCodeGenerator {
    fn drop(&mut self) {
        unsafe { thinlto_codegen_dispose(self.as_raw()) }
    }
}

impl ThinCodeGenerator {
    /// Instantiates a ThinLTO code generator.
    pub fn create() -> Result<Self> {
        unsafe { thinlto_create_codegen() }.ok_or_else(report_last_error)
    }

    /// Add a module to a code generator.
    ///
    /// Identifier must be unique among all the modules in the code generator.
    /// The data buffer remains owned by the client, and must live at least
    /// as long as the code generator.
    pub fn add_module<T: AsRef<str>, D: AsRef<[u8]>>(&self, id: T, data: D) {
        let data = data.as_ref();

        unsafe { thinlto_codegen_add_module(self.as_raw(), cstr!(id), data.as_ptr() as *const _, data.len() as i32) }
    }

    /// Optimize and codegen all modules attached to the code generator.
    pub fn process(&self) {
        unsafe { thinlto_codegen_process(self.as_raw()) }
    }

    /// Return the object files produced by the code generator.
    pub fn objects(&self) -> Objects {
        Objects {
            gen: self,
            len: unsafe { thinlto_module_get_num_objects(self.as_raw()) as u32 },
            pos: 0,
        }
    }

    /// Return the object files produced by the code generator.
    pub fn object_files(&self) -> ObjectFiles {
        ObjectFiles {
            gen: self,
            len: unsafe { thinlto_module_get_num_object_files(self.as_raw()) },
            pos: 0,
        }
    }

    /// Set which PIC code model to generate.
    pub fn set_pic_model(&self, model: CodeGenModel) -> Result<()> {
        unsafe { thinlto_codegen_set_pic_model(self.as_raw(), mem::transmute(model)) }.ok_or_else(report_last_error)
    }

    /// Sets the path to a directory to use as a cache storage for incremental build.
    ///
    /// Setting this activates caching.
    pub fn set_cache_dir<P: AsRef<Path>>(&self, path: P) {
        unsafe { thinlto_codegen_set_cache_dir(self.as_raw(), cpath!(path)) }
    }

    /// Sets the cache pruning interval (in seconds).
    pub fn set_cache_pruning(&self, pruning: CachePruning) {
        unsafe { thinlto_codegen_set_cache_pruning_interval(self.as_raw(), pruning.into()) }
    }

    /// Set the maximum cache size to persist across builds.
    ///
    /// This is expressed as a percentage of available disk space. 100 means no limit,
    /// and 50 means no more than half of the available disk space. 0 is ignored, and
    /// values over 100 will be reduced to 100.
    pub fn set_final_cache_size_relative_to_available_space(&self, percentage: u32) {
        unsafe { thinlto_codegen_set_final_cache_size_relative_to_available_space(self.as_raw(), percentage) }
    }

    /// Sets the expiration (in seconds) for an entry in the cache.
    pub fn set_cache_entry_expiration(&self, expiration: Duration) {
        unsafe { thinlto_codegen_set_cache_entry_expiration(self.as_raw(), expiration.as_secs() as u32) }
    }

    ///  Sets the maximum size of the cache directory (in bytes).
    pub fn set_cache_size_bytes(&self, max_size_bytes: usize) {
        unsafe { thinlto_codegen_set_cache_size_bytes(self.as_raw(), max_size_bytes as u32) }
    }

    /// Sets the maximum number of files in the cache directory.
    pub fn set_cache_size_files(&self, max_size_files: usize) {
        unsafe { thinlto_codegen_set_cache_size_files(self.as_raw(), max_size_files as u32) }
    }

    /// Set the path to a directory to use as temporary bitcode storage.
    ///
    /// This is meant to make the bitcode files available for debugging.
    pub fn set_savetemps_dir<P: AsRef<Path>>(&self, save_temps_dir: P) {
        unsafe { thinlto_codegen_set_savetemps_dir(self.as_raw(), cpath!(save_temps_dir)) }
    }

    /// Set the path to a directory to save generated object files.
    ///
    /// Set this to request on-disk rather than in-memory buffers. When set, use
    /// `thinlto_module_get_object_file` instead of `thinlto_module_get_object`.
    pub fn set_generated_objects_dir<P: AsRef<Path>>(&self, save_temps_dir: P) {
        unsafe { thinlto_set_generated_objects_dir(self.as_raw(), cpath!(save_temps_dir)) }
    }

    /// Set the CPU to generate code for.
    pub fn thinlto_codegen_set_cpu<S: AsRef<str>>(&self, cpu: S) {
        unsafe { thinlto_codegen_set_cpu(self.as_raw(), cstr!(cpu)) }
    }

    /// Disable code generation (running all stages until codegen).
    ///
    /// The output with codegen disabled is bitcode.
    pub fn disable_codegen(&self, disable: bool) {
        unsafe { thinlto_codegen_disable_codegen(self.as_raw(), disable.as_bool()) }
    }

    /// Perform codegen only; disable all other stages.
    pub fn set_codegen_only(&self, codegen_only: bool) {
        unsafe { thinlto_codegen_set_codegen_only(self.as_raw(), codegen_only.as_bool()) }
    }

    /// Sets options to help debug codegen bugs.
    pub fn set_debug_options(options: &[&str]) {
        let options = options.iter().map(|opt| opt.as_ptr() as *const _).collect::<Vec<_>>();

        unsafe { thinlto_debug_options(options.as_ptr(), options.len() as i32) }
    }

    /// Add a symbol to the list of global symbols that must exist in the
    /// final generated code.
    ///
    /// Functions not listed may be inlined in every usage and optimized away.
    pub fn add_must_preserve_symbol<S: AsRef<str>>(&self, symbol: S) {
        let symbol = symbol.as_ref();

        unsafe {
            thinlto_codegen_add_must_preserve_symbol(self.as_raw(), symbol.as_ptr() as *const _, symbol.len() as i32)
        }
    }

    /// Add a symbol to the list of global symbols that are cross-referenced
    /// between ThinLTO files.
    ///
    /// Symbols listed can be discarded if every reference from a ThinLTO module
    /// to a symbol is optimized away, then the symbol can be discarded.
    pub fn add_cross_referenced_symbol<S: AsRef<str>>(&self, symbol: S) {
        let symbol = symbol.as_ref();

        unsafe {
            thinlto_codegen_add_cross_referenced_symbol(self.as_raw(), symbol.as_ptr() as *const _, symbol.len() as i32)
        }
    }
}

impl Module {
    /// Test if a module has ThinLTO linking support.
    pub fn is_thinlto(&self) -> bool {
        unsafe { lto_module_is_thinlto(self.as_raw()) }.as_bool()
    }
}

/// the cache pruning action.
#[derive(Debug)]
pub enum CachePruning {
    /// The cache pruning interval
    Interval(Duration),
    /// Force prunning to occur
    ForcePruning,
    /// Disables the pruning.
    Disable,
}

impl From<CachePruning> for i32 {
    fn from(pruning: CachePruning) -> Self {
        match pruning {
            CachePruning::Interval(d) => d.as_secs() as i32,
            CachePruning::ForcePruning => 0,
            CachePruning::Disable => -1,
        }
    }
}

#[repr(C)]
#[allow(non_snake_case)]
struct LTOObjectBuffer {
    pub buf: *const ::libc::c_char,
    pub len: ::libc::size_t,
}

/// An iterator over the object of a thin code generator.
pub struct Objects<'a> {
    gen: &'a ThinCodeGenerator,
    len: u32,
    pos: u32,
}

impl<'a> Iterator for Objects<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.len {
            let data: LTOObjectBuffer =
                unsafe { mem::transmute(thinlto_module_get_object(self.gen.as_raw(), self.pos)) };

            self.pos += 1;

            Some(unsafe { slice::from_raw_parts(data.buf as *const _, data.len) })
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len as usize, Some(self.len as usize))
    }
}

impl<'a> ExactSizeIterator for Objects<'a> {
    fn len(&self) -> usize {
        self.len as usize
    }
}

/// An iterator over the object files of a thin code generator.
pub struct ObjectFiles<'a> {
    gen: &'a ThinCodeGenerator,
    len: u32,
    pos: u32,
}

impl<'a> Iterator for ObjectFiles<'a> {
    type Item = Cow<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.len {
            let path = unsafe { thinlto_module_get_object_file(self.gen.as_raw(), self.pos) }.as_str();

            self.pos += 1;

            Some(path)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len as usize, Some(self.len as usize))
    }
}

impl<'a> ExactSizeIterator for ObjectFiles<'a> {
    fn len(&self) -> usize {
        self.len as usize
    }
}
