use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::slice;

pub fn unchecked_cstring<S: AsRef<str>>(s: S) -> CString {
    unsafe { CString::from_vec_unchecked(s.as_ref().as_bytes().to_vec()) }
}

pub fn from_unchecked_cstr<'a>(p: *const u8, len: usize) -> Cow<'a, str> {
    unsafe {
        CStr::from_bytes_with_nul_unchecked(slice::from_raw_parts(p, len + 1)).to_string_lossy()
    }
}
