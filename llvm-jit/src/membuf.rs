use std::ffi::CString;
use std::path::Path;
use std::ptr;
use std::slice;

use llvm::core::*;
use llvm::prelude::*;

use errors::Result;
use utils::{AsLLVMBool, AsResult, DisposableMessage, unchecked_cstring};

#[derive(Debug)]
pub struct MemoryBuffer(LLVMMemoryBufferRef);

inherit_from!(MemoryBuffer, LLVMMemoryBufferRef);

impl Drop for MemoryBuffer {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMemoryBuffer(self.0) }
    }
}

impl MemoryBuffer {
    /// Open the specified file as a MemoryBuffer.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = CString::new(path.as_ref().to_string_lossy().as_bytes())?;
        let mut buf = ptr::null_mut();
        let mut msg = ptr::null_mut();

        if unsafe { LLVMCreateMemoryBufferWithContentsOfFile(path.as_ptr(), &mut buf, &mut msg) }
            .is_ok()
        {
            Ok(buf.into())
        } else {
            bail!(format!(
                "fail to create memory buffer from file {}, {}",
                path.to_string_lossy(),
                msg.to_string(),
            ))
        }
    }

    /// Read all of stdin into a MemoryBuffer, and return it.
    pub fn from_stdin() -> Result<Self> {
        let mut buf = ptr::null_mut();
        let mut msg = ptr::null_mut();

        if unsafe { LLVMCreateMemoryBufferWithSTDIN(&mut buf, &mut msg) }.is_ok() {
            Ok(buf.into())
        } else {
            bail!(format!(
                "fail to create memory buffer from STDIN, {}",
                msg.to_string(),
            ))
        }
    }

    /// Open the specified memory range as a MemoryBuffer.
    pub fn from_bytes(data: &[u8], name: &str) -> Self {
        unsafe {
            LLVMCreateMemoryBufferWithMemoryRange(
                data.as_ptr() as *const i8,
                data.len(),
                unchecked_cstring(name).as_ptr(),
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
                unchecked_cstring(name).as_ptr(),
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
