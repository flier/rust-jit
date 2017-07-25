use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::slice;

pub fn unchecked_cstring<S: AsRef<str>>(s: S) -> CString {
    unsafe { CString::from_vec_unchecked(s.as_ref().as_bytes().to_vec()) }
}

pub fn from_unchecked_cstr<'a>(p: *const u8, len: usize) -> Cow<'a, str> {
    unsafe { CStr::from_bytes_with_nul_unchecked(slice::from_raw_parts(p, len)).to_string_lossy() }
}

#[macro_export]
macro_rules! impl_iter {
    ($name:ident, $first:path [ $list:ty ], $next:path [ $item:ty ], $type:ident :: $ctor:ident) => {
        pub struct $name {
            l: Option<$list>,
            i: Option<$item>,
        }

        impl $name {
            pub fn new(l: $list) -> Self {
                $name {
                    l: Some(l),
                    i: None,
                }
            }
        }

        impl ::std::iter::Iterator for $name {
            type Item = $type;

            fn next(&mut self) -> Option<Self::Item> {
                if let Some(l) = self.l {
                    let next = unsafe {
                        if let Some(i) = self.i {
                            $next(i)
                        } else {
                            $first(l)
                        }
                    };

                    self.i = if next.is_null() {
                        self.l = None;

                        None
                    } else {
                        Some(next)
                    };

                    self.i.map(|i| $type :: $ctor(i))
                } else {
                    None
                }
            }
        }
    }
}
