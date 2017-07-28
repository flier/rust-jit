use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::slice;

pub fn unchecked_cstring<S: AsRef<str>>(s: S) -> CString {
    unsafe { CString::from_vec_unchecked(s.as_ref().as_bytes().to_vec()) }
}

pub fn from_unchecked_cstr<'a>(p: *const u8, len: usize) -> Cow<'a, str> {
    unsafe { CStr::from_bytes_with_nul_unchecked(slice::from_raw_parts(p, len)).to_string_lossy() }
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

                    self.item.map(|item| $type :: $ctor(item))
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

                    self.next.map(|next| $type :: $ctor(next))
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

                    self.back.map(|back| $type :: $ctor(back))
                } else {
                    None
                }
            }
        }
    };
}
