use std::path::Path;
use std::ptr;
use std::slice;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::errors::Result;
use crate::utils::{AsRaw, AsResult, DisposableMessage, FALSE};

#[derive(Debug)]
pub struct MemoryBuffer<'a>(LLVMMemoryBufferRef, Option<&'a [u8]>);

impl<'a> Drop for MemoryBuffer<'a> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMemoryBuffer(self.0) }
    }
}

impl<'a> Clone for MemoryBuffer<'a> {
    fn clone(&self) -> MemoryBuffer<'a> {
        MemoryBuffer::from_bytes(self.as_bytes(), "clone")
    }
}

impl<'a> From<LLVMMemoryBufferRef> for MemoryBuffer<'a> {
    fn from(buf: LLVMMemoryBufferRef) -> Self {
        MemoryBuffer(buf, None)
    }
}

impl<'a> AsRaw for MemoryBuffer<'a> {
    type RawType = LLVMMemoryBufferRef;

    fn as_raw(&self) -> Self::RawType {
        self.0
    }
}

impl<'a> MemoryBuffer<'a> {
    /// Open the specified file as a MemoryBuffer.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut buf = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        trace!("MemoryBuffer from file: {:?}", path.as_ref());

        unsafe { LLVMCreateMemoryBufferWithContentsOfFile(cpath!(path), &mut buf, &mut msg) }
            .ok_or_else(|| format_err!("fail to create memory buffer from file , {}", msg.into_string(),))
            .map(|_| buf.into())
    }

    /// Read all of stdin into a MemoryBuffer, and return it.
    pub fn from_stdin() -> Result<Self> {
        let mut buf = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        trace!("MemoryBuffer from STDIN");

        unsafe { LLVMCreateMemoryBufferWithSTDIN(&mut buf, &mut msg) }
            .ok_or_else(|| format_err!("fail to create memory buffer from STDIN, {}", msg.into_string(),))
            .map(|_| buf.into())
    }

    /// Open the specified memory range as a MemoryBuffer.
    pub fn from_str(s: &'a str, name: &str) -> MemoryBuffer<'a> {
        MemoryBuffer::from_bytes(s.as_bytes(), name)
    }

    /// Open the specified memory range as a MemoryBuffer.
    pub fn copy_from_str(s: &str, name: &str) -> MemoryBuffer<'a> {
        MemoryBuffer::copy_from_bytes(s.as_bytes(), name)
    }

    /// Open the specified memory range as a MemoryBuffer.
    pub fn from_bytes(data: &'a [u8], name: &str) -> MemoryBuffer<'a> {
        trace!(
            "MemoryBuffer `{}` from bytes @ 0x{:p} with {} bytes:\n{}",
            name,
            data.as_ptr(),
            data.len(),
            hexdump!(data)
        );

        MemoryBuffer(
            unsafe {
                LLVMCreateMemoryBufferWithMemoryRange(data.as_ptr() as *const i8, data.len(), cstr!(name), FALSE)
            },
            Some(data),
        )
    }

    /// Open the specified memory range as a MemoryBuffer, copying the contents and taking ownership of it.
    pub fn copy_from_bytes(data: &[u8], name: &str) -> Self {
        trace!(
            "MemoryBuffer `{}` copied from bytes @ 0x{:p} with {} bytes:\n{}",
            name,
            data.as_ptr(),
            data.len(),
            hexdump!(data)
        );

        unsafe { LLVMCreateMemoryBufferWithMemoryRangeCopy(data.as_ptr() as *const i8, data.len(), cstr!(name)) }.into()
    }

    pub fn as_bytes(&self) -> &'a [u8] {
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
        let buf = MemoryBuffer::from_bytes(data, "from_bytes");

        assert_eq!(buf.as_bytes(), data);
        assert_eq!(buf.as_ptr(), data.as_ptr());
        assert_eq!(buf.len(), 4);
    }
}
