use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::mem;
use std::path::Path;
use std::ptr;
use std::result::Result as StdResult;
use std::slice;

use libc;

use crate::errors::Result;
use crate::llvm::core::LLVMDisposeMessage;
use crate::llvm::prelude::*;

pub trait AsRaw {
    type RawType;

    /// Extracts the wrapped raw reference.
    fn as_raw(&self) -> Self::RawType;

    fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Self::RawType) -> R,
    {
        f(self.as_raw())
    }
}

impl<R, T> AsRaw for Option<R>
where
    R: AsRaw<RawType = *mut T>,
{
    type RawType = *mut T;

    fn as_raw(&self) -> Self::RawType {
        self.as_ref()
            .map(|t| t.as_raw())
            .unwrap_or_else(|| ptr::null_mut() as *mut T)
    }
}

pub trait IntoRaw: AsRaw {
    /// Consumer the wrapper, returning the wrapped raw reference.
    fn into_raw(self) -> Self::RawType;
}

impl<T: AsRaw> IntoRaw for T {
    fn into_raw(self) -> Self::RawType {
        let raw = self.as_raw();
        mem::forget(self);
        raw
    }
}

pub const TRUE: LLVMBool = 1;
pub const FALSE: LLVMBool = 0;

pub trait AsBool {
    fn as_bool(self) -> bool;
}

impl AsBool for LLVMBool {
    fn as_bool(self) -> bool {
        self != FALSE
    }
}

pub trait AsResult<T>: Sized {
    fn is_ok(self) -> bool;

    fn ok(self) -> Option<T> {
        self.ok_or(()).ok()
    }

    fn ok_or<E>(self, err: E) -> StdResult<T, E> {
        self.ok_or_else(|| err)
    }

    fn ok_or_else<F, E>(self, err: F) -> StdResult<T, E>
    where
        F: FnOnce() -> E;
}

impl AsResult<()> for LLVMBool {
    fn is_ok(self) -> bool {
        self == FALSE
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

impl<P, T> AsResult<T> for *const P
where
    T: From<*const P>,
{
    fn is_ok(self) -> bool {
        !self.is_null()
    }

    fn ok_or_else<F, E>(self, err: F) -> StdResult<T, E>
    where
        F: FnOnce() -> E,
    {
        if !self.is_null() {
            Ok(self.into())
        } else {
            Err(err())
        }
    }
}

impl<P, T> AsResult<T> for *mut P
where
    T: From<*mut P>,
{
    fn is_ok(self) -> bool {
        !self.is_null()
    }

    fn ok_or_else<F, E>(self, err: F) -> StdResult<T, E>
    where
        F: FnOnce() -> E,
    {
        if !self.is_null() {
            Ok(self.into())
        } else {
            Err(err())
        }
    }
}

pub trait AsLLVMBool {
    fn as_bool(self) -> LLVMBool;
}

impl AsLLVMBool for bool {
    fn as_bool(self) -> LLVMBool {
        if self {
            TRUE
        } else {
            FALSE
        }
    }
}

pub trait AsPtr<T> {
    fn as_ptr<P>(self) -> *const P;
}

impl<T> AsPtr<T> for *const T {
    fn as_ptr<P>(self) -> *const P {
        self as *const P
    }
}

impl<'a, T> AsPtr<T> for &'a T {
    fn as_ptr<P>(self) -> *const P {
        self as *const T as *const P
    }
}

impl<'a, T> AsPtr<T> for Option<&'a T> {
    fn as_ptr<P>(self) -> *const P {
        match self {
            Some(value) => value as *const T as *const P,
            None => ptr::null(),
        }
    }
}

pub trait AsMutPtr<T> {
    fn as_mut_ptr<P>(self) -> *mut P;
}

impl<T> AsMutPtr<T> for *mut T {
    fn as_mut_ptr<P>(self) -> *mut P {
        self as *mut P
    }
}

impl<'a, T> AsMutPtr<T> for &'a mut T {
    fn as_mut_ptr<P>(self) -> *mut P {
        self as *mut T as *mut P
    }
}

impl<'a, T> AsMutPtr<T> for Option<&'a mut T> {
    fn as_mut_ptr<P>(self) -> *mut P {
        match self {
            Some(value) => value as *mut T as *mut P,
            None => ptr::null_mut(),
        }
    }
}

pub trait ToCString {
    fn to_cstring(&self) -> Result<CString>;
}

impl ToCString for str {
    fn to_cstring(&self) -> Result<CString> {
        Ok(CString::new(self)?)
    }
}

impl ToCString for Path {
    fn to_cstring(&self) -> Result<CString> {
        Ok(CString::new(self.to_string_lossy().as_ref())?)
    }
}

pub unsafe fn unchecked_cstring<S: AsRef<str>>(s: S) -> CString {
    CString::from_vec_unchecked(s.as_ref().as_bytes().to_vec())
}

pub unsafe fn unchecked_cpath<P: AsRef<Path>>(path: P) -> CString {
    unchecked_cstring(path.as_ref().to_string_lossy().as_ref())
}

macro_rules! cstr {
    ($s: expr) => {
        $crate::utils::unchecked_cstring($s).into_bytes_with_nul().as_ptr() as *const ::std::os::raw::c_char
    };
}

macro_rules! cpath {
    ($s: expr) => {
        $crate::utils::unchecked_cpath($s).into_bytes_with_nul().as_ptr() as *const ::std::os::raw::c_char
    };
}

pub unsafe fn unchecked_cstr<'a>(p: *const u8, len: usize) -> Cow<'a, str> {
    CStr::from_bytes_with_nul_unchecked(slice::from_raw_parts(p, len)).to_string_lossy()
}

pub trait UncheckedCStr<'a> {
    fn as_str(self) -> Cow<'a, str>;
}

impl<'a> UncheckedCStr<'a> for *const libc::c_char {
    fn as_str(self) -> Cow<'a, str> {
        if self.is_null() {
            "".into()
        } else {
            unsafe { ::std::ffi::CStr::from_ptr(self) }.to_string_lossy()
        }
    }
}

impl<'a> UncheckedCStr<'a> for *mut libc::c_char {
    fn as_str(self) -> Cow<'a, str> {
        if self.is_null() {
            "".into()
        } else {
            unsafe { ::std::ffi::CStr::from_ptr(self) }.to_string_lossy()
        }
    }
}

impl<'a> UncheckedCStr<'a> for &'a libc::c_char {
    fn as_str(self) -> Cow<'a, str> {
        unsafe { ::std::ffi::CStr::from_ptr(self) }.to_string_lossy()
    }
}

pub trait DisposableMessage {
    fn new() -> Self;

    fn into_string(self) -> String;
}

impl DisposableMessage for *mut libc::c_char {
    fn new() -> Self {
        ptr::null_mut()
    }

    fn into_string(self) -> String {
        unsafe {
            if self.is_null() {
                String::default()
            } else {
                let s = CStr::from_ptr(self).to_string_lossy().into();
                LLVMDisposeMessage(self);
                s
            }
        }
    }
}

macro_rules! inherit_from {
    (__impl_deref $ty:ident, $parent:ty) => {
        impl ::std::ops::Deref for $ty {
            type Target = $parent;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };

    (__impl_as_raw $ty:ident, $raw:ty) => {
        impl $crate::utils::AsRaw for $ty {
            type RawType = $raw;

            fn as_raw(&self) -> Self::RawType {
                self.0.as_raw()
            }
        }
    };

    (__impl_from_raw $ty:ident, $raw:ty) => {
        impl ::std::convert::From<$raw> for $ty {
            fn from(f: $raw) -> Self {
                $ty::from_raw(f)
            }
        }

        impl $ty {
            /// Wrap a raw $ty reference.
            pub fn from_raw(v: $raw) -> Self {
                $ty(v.into())
            }
        }
    };

    (__impl_raw $ty:ident, $raw:ty) => {
        inherit_from!(__impl_as_raw $ty, $raw);
        inherit_from!(__impl_from_raw $ty, $raw);
    };

    (__impl_from $ty:ty, $to:ty) => {
        impl ::std::convert::From<$ty> for $to {
            fn from(from: $ty) -> Self {
                from.as_raw().into()
            }
        }
    };

    (__impl_convert_between $from:ty, $to: ty) => {
        inherit_from!(__impl_from $from, $to);
        inherit_from!(__impl_from $to, $from);
    };

    ($ty:ident, $parent:ty, $grandfather:ty, $ancestor:ty, $raw:ty) => {
        inherit_from!(__impl_deref $ty, $parent);
        inherit_from!(__impl_raw $ty, $raw);
        inherit_from!(__impl_convert_between $ty, $parent);
        inherit_from!(__impl_convert_between $ty, $grandfather);
        inherit_from!(__impl_convert_between $ty, $ancestor);
    };

    ($ty:ident, $parent:ty, $ancestor:ty, $raw:ty) => {
        inherit_from!(__impl_deref $ty, $parent);
        inherit_from!(__impl_raw $ty, $raw);
        inherit_from!(__impl_convert_between $ty, $parent);
        inherit_from!(__impl_convert_between $ty, $ancestor);
    };

    ($ty: ident, $parent: ty, $raw: ty) => {
        inherit_from!(__impl_deref $ty, $parent);
        inherit_from!(__impl_raw $ty, $raw);
        inherit_from!(__impl_convert_between $ty, $parent);
    };

    ($ty:ident, $raw:ty) => {
        inherit_from!(__impl_deref $ty, $raw);
        inherit_from!(__impl_from_raw $ty, $raw);

        impl $crate::utils::AsRaw for $ty {
            type RawType = $raw;

            fn as_raw(&self) -> Self::RawType {
                self.0
            }
        }
    };
}

macro_rules! impl_iter {
    ($name: ident, $first: path[$list: ty], $next: path[$item: ty], $type: ident) => {
        pub struct $name {
            list: Option<$list>,
            item: Option<$item>,
        }

        impl $name {
            pub fn new(list: $list) -> Self {
                $name {
                    list: Some(list),
                    item: None,
                }
            }
        }

        impl ::std::iter::Iterator for $name {
            type Item = $type;

            fn next(&mut self) -> Option<Self::Item> {
                if let Some(list) = self.list {
                    let next = unsafe {
                        if let Some(item) = self.item {
                            $next(item)
                        } else {
                            $first(list)
                        }
                    };

                    self.item = if next.is_null() {
                        self.list = None;

                        None
                    } else {
                        Some(next)
                    };

                    self.item.map(|item| item.into())
                } else {
                    None
                }
            }
        }
    };
    ($name: ident, $first: path | $last: path[$list: ty], $next: path | $previous: path[$item: ty], $type: ident) => {
        pub struct $name {
            list: Option<$list>,
            next: Option<$item>,
            back: Option<$item>,
        }

        impl $name {
            pub fn new(list: $list) -> Self {
                $name {
                    list: Some(list),
                    next: None,
                    back: None,
                }
            }
        }

        impl ::std::iter::Iterator for $name {
            type Item = $type;

            fn next(&mut self) -> Option<Self::Item> {
                if let Some(list) = self.list {
                    let next = unsafe {
                        if let Some(next) = self.next {
                            $next(next)
                        } else {
                            $first(list)
                        }
                    };

                    self.next = if next.is_null() || Some(next) == self.back {
                        self.list = None;

                        None
                    } else {
                        Some(next)
                    };

                    self.next.map(|next| next.into())
                } else {
                    None
                }
            }
        }

        impl ::std::iter::DoubleEndedIterator for $name {
            fn next_back(&mut self) -> Option<Self::Item> {
                if let Some(list) = self.list {
                    let back = unsafe {
                        if let Some(back) = self.back {
                            $previous(back)
                        } else {
                            $last(list)
                        }
                    };

                    self.back = if back.is_null() || Some(back) == self.next {
                        self.list = None;

                        None
                    } else {
                        Some(back)
                    };

                    self.back.map(|back| back.into())
                } else {
                    None
                }
            }
        }
    };
}
