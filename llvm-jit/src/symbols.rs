use std::path::Path;

use libc::c_void;
use llvm::support::*;

use errors::Result;
use utils::{AsMutPtr, AsResult};

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
            .ok_or_else(|| format!("fail to load library {:?}", filename).into())
    }

    /// This functions permanently adds the symbol with the value.
    ///
    /// These symbols are searched before any libraries.
    pub fn add<S: AsRef<str>, P: AsMutPtr<T>, T>(symbol: S, value: P) {
        let symbol = symbol.as_ref();
        let addr = value.as_mut_ptr();

        trace!("add symbol `{}` @ {:?}", symbol, addr);

        unsafe { LLVMAddSymbol(cstr!(symbol), addr) }
    }

    /// This function will search through all previously loaded dynamic libraries for the symbol.
    pub fn search<S: AsRef<str>, T>(symbol: S) -> Option<*const c_void> {
        let symbol = symbol.as_ref();
        let addr = unsafe { LLVMSearchForAddressOfSymbol(cstr!(symbol)) };

        trace!("got symbol `{}` @ {:?}", symbol, addr);

        if addr.is_null() { None } else { Some(addr) }
    }
}
