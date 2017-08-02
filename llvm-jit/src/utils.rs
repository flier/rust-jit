use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::ptr;
use std::slice;

use libc;

use llvm::core::LLVMDisposeMessage;
use llvm::prelude::*;

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

pub trait AsResult {
    fn is_ok(self) -> bool;
}

impl AsResult for LLVMBool {
    fn is_ok(self) -> bool {
        self == FALSE
    }
}

pub trait AsLLVMBool {
    fn as_bool(self) -> LLVMBool;
}

impl AsLLVMBool for bool {
    fn as_bool(self) -> LLVMBool {
        if self { TRUE } else { FALSE }
    }
}

/// This trait defines a number of combinator-style methods for use with `bool` values.
///
/// In general, `true`/`false` map to `Some(_)`/`None` and `Ok(_)`/`Err(_)` respectively.
pub trait Boolinator: AsBool + Sized {
    /// If this value is `true`, returns `Some(())`; `None` otherwise.
    fn as_option(self) -> Option<()>;

    /// If this value is `true`, returns `Some(some)`; `None` otherwise.
    fn as_some<T>(self, some: T) -> Option<T>;

    /// If this value is `true`, returns `Some(some())`; `None` otherwise.
    fn as_some_from<T, F>(self, some: F) -> Option<T>
    where
        F: FnOnce() -> T;

    /// If this value is `true`, returns `opt`; `None` otherwise.
    fn and_option<T>(self, opt: Option<T>) -> Option<T>;

    /// If this value is `true`, returns `opt()`; `None` otherwise.
    fn and_option_from<T, F>(self, opt: F) -> Option<T>
    where
        F: FnOnce() -> Option<T>;

    /// If this value is `true`, returns `Ok(ok)`; `Err(err)` otherwise.
    fn as_result<T, E>(self, ok: T, err: E) -> Result<T, E>;

    /// If this value is `true`, returns `Ok(ok())`; `Err(err())` otherwise.
    fn as_result_from<T, E, F, G>(self, ok: F, err: G) -> Result<T, E>
    where
        F: FnOnce() -> T,
        G: FnOnce() -> E;

    /// If this value is `true`, returns `Ok(())`; `Err(err)` otherwise.
    fn ok_or<E>(self, err: E) -> Result<(), E>;

    /// If this value is `true`, returns `Ok(())`; `Err(err())` otherwise.
    fn ok_or_else<E, G>(self, err: G) -> Result<(), E>
    where
        G: FnOnce() -> E;

    /// If this value is `true`, panics with `msg`; does nothing otherwise.
    fn expect(self, msg: &str);
}

impl Boolinator for LLVMBool {
    fn as_option(self) -> Option<()> {
        if self.as_bool() { Some(()) } else { None }
    }

    fn as_some<T>(self, some: T) -> Option<T> {
        if self.as_bool() { Some(some) } else { None }
    }

    fn as_some_from<T, F>(self, some: F) -> Option<T>
    where
        F: FnOnce() -> T,
    {
        if self.as_bool() { Some(some()) } else { None }
    }

    fn and_option<T>(self, opt: Option<T>) -> Option<T> {
        self.as_option().and(opt)
    }

    fn and_option_from<T, F>(self, opt: F) -> Option<T>
    where
        F: FnOnce() -> Option<T>,
    {
        self.as_option().and(opt())
    }

    fn as_result<T, E>(self, ok: T, err: E) -> Result<T, E> {
        if self.as_bool() { Ok(ok) } else { Err(err) }
    }

    fn as_result_from<T, E, F, G>(self, ok: F, err: G) -> Result<T, E>
    where
        F: FnOnce() -> T,
        G: FnOnce() -> E,
    {
        if self.as_bool() { Ok(ok()) } else { Err(err()) }
    }

    fn ok_or<E>(self, err: E) -> Result<(), E> {
        if self.as_bool() { Ok(()) } else { Err(err) }
    }

    fn ok_or_else<E, G>(self, err: G) -> Result<(), E>
    where
        G: FnOnce() -> E,
    {
        if self.as_bool() { Ok(()) } else { Err(err()) }
    }

    fn expect(self, msg: &str) {
        if self.as_bool() {
            panic!("{}", msg)
        }
    }
}

pub trait AsMutPtr<T> {
    fn as_mut_ptr(self) -> *mut T;
}

impl<'a, T> AsMutPtr<T> for Option<&'a mut T> {
    fn as_mut_ptr(self) -> *mut T {
        match self {
            Some(value) => value as *mut T,
            None => ptr::null_mut(),
        }
    }
}

pub fn unchecked_cstring<S: AsRef<str>>(s: S) -> CString {
    unsafe { CString::from_vec_unchecked(s.as_ref().as_bytes().to_vec()) }
}

pub fn from_unchecked_cstr<'a>(p: *const u8, len: usize) -> Cow<'a, str> {
    unsafe { CStr::from_bytes_with_nul_unchecked(slice::from_raw_parts(p, len)).to_string_lossy() }
}

pub trait DisposableMessage {
    fn to_string(self) -> String;
}

impl DisposableMessage for *mut libc::c_char {
    fn to_string(self) -> String {
        unsafe {
            let s = CStr::from_ptr(self).to_string_lossy().into();
            LLVMDisposeMessage(self);
            s
        }
    }
}

macro_rules! inherit_from {
    ($ty:ident, $raw:ty) => {
        impl ::std::ops::Deref for $ty {
            type Target = $raw;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl ::std::convert::From<$raw> for $ty {
            fn from(f: $raw) -> Self {
                $ty::from_raw(f)
            }
        }

        impl $ty {
            /// Wrap a raw reference.
            pub fn from_raw(v: $raw) -> Self {
                $ty(v.into())
            }

            /// Extracts the raw reference.
            pub fn as_raw(&self) -> $raw {
                self.0
            }

            /// Consumes the wrapper, returning the wrapped raw pointer.
            pub fn into_raw(self) -> $raw {
                let raw = self.as_raw();
                ::std::mem::forget(self);
                raw
            }
        }
    };

    ($ty:ident, $parent:ty, $ancestor:ty, $raw:ty) => {
        impl ::std::ops::Deref for $ty {
            type Target = $ancestor;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl ::std::convert::From<$ty> for $parent {
            fn from(f: $ty) -> Self {
                f.0
            }
        }

        impl ::std::convert::From<$ty> for $ancestor {
            fn from(f: $ty) -> Self {
                f.0.into()
            }
        }

        impl ::std::convert::From<$parent> for $ty {
            fn from(parent: $parent) -> Self {
                $ty(parent)
            }
        }

        impl ::std::convert::From<$ancestor> for $ty {
            fn from(ancestor: $ancestor) -> Self {
                ancestor.into()
            }
        }

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

    ($ty:ident, $parent:ty, $raw:ty) => {
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

        impl ::std::convert::From<$ty> for $parent {
            fn from(f: $ty) -> Self {
                f.0
            }
        }

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
    }
}

macro_rules! impl_iter {
    ($name:ident, $first:path [ $list:ty ], $next:path [ $item:ty ], $type:ident :: $ctor:ident) => {
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

                    self.item.map($type :: $ctor)
                } else {
                    None
                }
            }
        }
    };
    (
        $name:ident,
        $first:path | $last:path [ $list:ty ],
        $next:path | $previous:path [ $item:ty ],
        $type:ident :: $ctor:ident
    ) => {
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

                    self.next.map($type :: $ctor)
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

                    self.back.map($type :: $ctor)
                } else {
                    None
                }
            }
        }
    };
}
