use std::ffi::CString;
use std::path::Path;
use std::ptr;
use std::slice;

use llvm::core::*;
use llvm::prelude::*;

use errors::Result;
use utils::{AsLLVMBool, AsRaw, AsResult, DisposableMessage};

#[derive(Debug)]
pub struct MemoryBuffer(LLVMMemoryBufferRef);

inherit_from!(MemoryBuffer, LLVMMemoryBufferRef);

impl Drop for MemoryBuffer {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMemoryBuffer(self.0) }
    }
}

impl Clone for MemoryBuffer {
    fn clone(&self) -> Self {
        MemoryBuffer::from_bytes(self.as_bytes(), "clone")
    }
}

impl MemoryBuffer {
    /// Open the specified file as a MemoryBuffer.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = CString::new(path.as_ref().to_string_lossy().as_bytes())?;
        let mut buf = ptr::null_mut();
        let mut msg = ptr::null_mut();

        unsafe { LLVMCreateMemoryBufferWithContentsOfFile(path.as_ptr(), &mut buf, &mut msg) }
            .ok_or_else(|| {
                format!(
                "fail to create memory buffer from file {}, {}",
                path.to_string_lossy(),
                msg.to_string(),
            ).into()
            })
            .map(|_| buf.into())
    }

    /// Read all of stdin into a MemoryBuffer, and return it.
    pub fn from_stdin() -> Result<Self> {
        let mut buf = ptr::null_mut();
        let mut msg = ptr::null_mut();

        unsafe { LLVMCreateMemoryBufferWithSTDIN(&mut buf, &mut msg) }
            .ok_or_else(|| {
                format!(
                "fail to create memory buffer from STDIN, {}",
                msg.to_string(),
            ).into()
            })
            .map(|_| buf.into())
    }

    /// Open the specified memory range as a MemoryBuffer.
    pub fn from_bytes(data: &[u8], name: &str) -> Self {
        unsafe {
            LLVMCreateMemoryBufferWithMemoryRange(
                data.as_ptr() as *const i8,
                data.len(),
                cstr!(name),
                false.as_bool(),
            )
        }.into()
    }

    /// Open the specified memory range as a MemoryBuffer, copying the contents and taking ownership of it.
    pub fn copy_from_bytes(data: &[u8], name: &str) -> Self {
        unsafe {
            LLVMCreateMemoryBufferWithMemoryRangeCopy(
                data.as_ptr() as *const i8,
                data.len(),
                cstr!(name),
            )
        }.into()
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.as_ptr(), self.len()) }
    }

    pub fn as_ptr<T>(&self) -> *const T {
        unsafe { LLVMGetBufferStart(self.as_raw()) as *const T }
    }

    pub fn len(&self) -> usize {
        unsafe { LLVMGetBufferSize(self.as_raw()) as usize }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Consumes the wrapper, returning the wrapped raw pointer.
    pub fn into_raw(self) -> LLVMMemoryBufferRef {
        let raw = self.as_raw();
        ::std::mem::forget(self);
        raw
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_bytes() {
        let data = b"test";
        let buf = MemoryBuffer::from_bytes(data, "buf");

        assert_eq!(buf.as_bytes(), data);
        assert_eq!(buf.as_ptr(), data.as_ptr());
        assert_eq!(buf.len(), 4);
    }
}
