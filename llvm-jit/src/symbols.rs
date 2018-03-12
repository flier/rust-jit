use std::env;
use std::path::Path;
use std::ptr;

use libc::c_void;
use llvm::support::*;

use errors::Result;
use utils::{AsPtr, AsResult};

pub struct Symbols {}

impl Symbols {
    /// This function permanently loads the dynamic library at the given path.
    ///
    /// Use this instead of getPermanentLibrary() when you won't need to get
    /// symbols from the library itself.
    pub fn load_library<P: AsRef<Path>>(filename: P) -> Result<()> {
        let filename = filename.as_ref();

        debug!("load library: {:?}", filename);

        unsafe { LLVMLoadLibraryPermanently(cpath!(filename)) }
            .ok_or_else(|| format_err!("fail to load library {:?}", filename))
    }

    /// This function load’the host process itself, making its exported symbols available for execution.
    pub fn load_current_exe() -> Result<()> {
        let filename = env::current_exe()?;

        debug!("load executable: {:?}", filename);

        unsafe { LLVMLoadLibraryPermanently(ptr::null()) }
            .ok_or_else(|| format_err!("fail to load executable {:?}", filename))
    }

    /// This functions permanently adds the symbol with the value.
    ///
    /// These symbols are searched before any libraries.
    pub fn add_symbol<S: AsRef<str>, P: AsPtr<T>, T>(symbol: S, value: P) {
        let symbol = symbol.as_ref();
        let addr = value.as_ptr() as *const c_void as *mut c_void;

        trace!("add symbol `{}` to the global symbols @ {:?}", symbol, addr);

        unsafe { LLVMAddSymbol(cstr!(symbol), addr) }
    }

    /// This function will search through all previously loaded dynamic libraries for the symbol.
    pub fn search_for_address<S: AsRef<str>, P>(symbol: S) -> Option<*const P> {
        let symbol = symbol.as_ref();

        trace!("search symbol `{}` in the global symbols", symbol);

        unsafe { LLVMSearchForAddressOfSymbol(cstr!(symbol)).as_ref() }.map(|p| {
            let p = p as *const c_void as *const P;

            trace!("got symbol `{}` from the global symbols @ {:?}", symbol, p);

            p
        })
    }
}
