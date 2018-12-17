use std::borrow::Cow;
use std::slice;

use crate::llvm::object::*;

use crate::membuf::MemoryBuffer;
use crate::utils::{AsBool, AsRaw, UncheckedCStr};

#[derive(Debug)]
pub struct ObjectFile(LLVMObjectFileRef);

inherit_from!(ObjectFile, LLVMObjectFileRef);

impl Drop for ObjectFile {
    fn drop(&mut self) {
        unsafe { LLVMDisposeObjectFile(self.as_raw()) }
    }
}

impl ObjectFile {
    pub fn new(buf: MemoryBuffer) -> Self {
        unsafe { LLVMCreateObjectFile(buf.into_raw()) }.into()
    }

    pub fn sections(&self) -> SectionIter {
        SectionIter::new(self)
    }

    pub fn symbols(&self) -> SymbolIter {
        SymbolIter::new(self)
    }
}

pub enum IterState<'a, T: 'a, I> {
    Start(&'a T),
    Next(&'a T, I),
    End,
}

macro_rules! impl_iter {
    ($name:ident < $container:ty, $raw:ty > -> $item:ty {
        dispose => $dispose:path,
        first => $first:path,
        next => $next:path,
        is_end => $is_end:path,
    }) => (
        pub struct $name<'a>(IterState<'a, $container, $raw>);

        impl<'a> $name<'a> {
            pub fn new(container: &'a $container) -> Self {
                $name(IterState::Start(container))
            }
        }

        impl<'a> Drop for $name<'a> {
            fn drop(&mut self) {
                if let IterState::Next(_, iter) = self.0 {
                    unsafe { $dispose(iter) };

                    self.0 = IterState::End;
                }
            }
        }

        impl<'a> Iterator for $name<'a> {
            type Item = $item;

            fn next(&mut self) -> Option<Self::Item> {
                unsafe {
                    match self.0 {
                        IterState::Start(container) => {
                            let iter = $first(container.as_raw());

                            Some((container, iter))
                        }
                        IterState::Next(container, iter) => {
                            $next(iter);

                            Some((container, iter))
                        }
                        IterState::End => None,
                    }.and_then(|(container, iter)| {
                        let finished = $is_end(container.as_raw(), iter).as_bool();

                        if finished {
                            $dispose(iter);

                            self.0 = IterState::End;

                            None
                        } else {
                            self.0 = IterState::Next(container, iter);

                            Some(iter.into())
                        }
                    })
                }
            }
        }
    )
}

impl_iter!(SectionIter<ObjectFile, LLVMSectionIteratorRef> -> Section {
    dispose => LLVMDisposeSectionIterator,
    first => LLVMGetSections,
    next => LLVMMoveToNextSection,
    is_end => LLVMIsSectionIteratorAtEnd,
});

impl_iter!(SymbolIter<ObjectFile, LLVMSymbolIteratorRef> -> Symbol {
    dispose => LLVMDisposeSymbolIterator,
    first => LLVMGetSymbols,
    next => LLVMMoveToNextSymbol,
    is_end => LLVMIsSymbolIteratorAtEnd,
});

pub struct Section(LLVMSectionIteratorRef);

impl From<LLVMSectionIteratorRef> for Section {
    fn from(iter: LLVMSectionIteratorRef) -> Self {
        Section(iter)
    }
}

impl AsRaw for Section {
    type RawType = LLVMSectionIteratorRef;

    fn as_raw(&self) -> Self::RawType {
        self.0
    }
}

impl Section {
    pub fn name(&self) -> Cow<str> {
        unsafe { LLVMGetSectionName(self.0) }.as_str()
    }

    pub fn size(&self) -> usize {
        unsafe { LLVMGetSectionSize(self.0) as usize }
    }

    pub fn contents(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(
                LLVMGetSectionContents(self.0) as *const u8,
                LLVMGetSectionSize(self.0) as usize,
            )
        }
    }

    pub fn address(&self) -> u64 {
        unsafe { LLVMGetSectionAddress(self.0) }
    }

    pub fn relocations(&self) -> RelocIter {
        RelocIter::new(self)
    }
}

impl_iter!(RelocIter<Section, LLVMRelocationIteratorRef> -> Relocation {
    dispose => LLVMDisposeRelocationIterator,
    first => LLVMGetRelocations,
    next => LLVMMoveToNextRelocation,
    is_end => LLVMIsRelocationIteratorAtEnd,
});

pub enum Symbol {
    Owned(LLVMSymbolIteratorRef),
    Borrowed(LLVMSymbolIteratorRef),
}

impl From<LLVMSymbolIteratorRef> for Symbol {
    fn from(iter: LLVMSymbolIteratorRef) -> Self {
        Symbol::Borrowed(iter)
    }
}

impl AsRaw for Symbol {
    type RawType = LLVMSymbolIteratorRef;

    fn as_raw(&self) -> Self::RawType {
        match *self {
            Symbol::Owned(iter) | Symbol::Borrowed(iter) => iter,
        }
    }
}

impl Symbol {
    pub fn name(&self) -> Cow<str> {
        unsafe { LLVMGetSymbolName(self.as_raw()) }.as_str()
    }

    pub fn size(&self) -> usize {
        unsafe { LLVMGetSymbolSize(self.as_raw()) as usize }
    }

    pub fn address(&self) -> u64 {
        unsafe { LLVMGetSymbolAddress(self.as_raw()) }
    }
}

pub struct Relocation(LLVMRelocationIteratorRef);

impl From<LLVMRelocationIteratorRef> for Relocation {
    fn from(iter: LLVMRelocationIteratorRef) -> Self {
        Relocation(iter)
    }
}

impl Relocation {
    pub fn offset(&self) -> u64 {
        unsafe { LLVMGetRelocationOffset(self.0) }
    }

    pub fn symbol(&self) -> Symbol {
        Symbol::Owned(unsafe { LLVMGetRelocationSymbol(self.0) })
    }

    pub fn type_id(&self) -> u64 {
        unsafe { LLVMGetRelocationType(self.0) }
    }

    pub fn type_name(&self) -> Cow<str> {
        unsafe { LLVMGetRelocationTypeName(self.0) }.as_str()
    }

    pub fn value_str(&self) -> Cow<str> {
        unsafe { LLVMGetRelocationValueString(self.0) }.as_str()
    }
}

#[cfg(test)]
mod tests {
    use std::env;

    use hamcrest::prelude::*;

    use super::*;
    use crate::membuf::MemoryBuffer;

    #[test]
    fn object_file() {
        let buf = MemoryBuffer::from_file(env::args().next().unwrap()).unwrap();
        let obj = ObjectFile::new(buf);

        let mut sections = obj.sections()
            .map(|section| {
                assert_that!(section.size(), is(greater_than(0)));
                assert_that!(section.contents().is_empty(), is(equal_to(false)));
                assert_that!(section.address(), is_not(equal_to(0)));

                section.name().into()
            })
            .collect::<Vec<String>>();

        sections.sort();

        assert_that!(
            &sections.iter().map(|s| s.as_str()).collect::<Vec<&str>>(),
            contains(vec!["__bss", "__common", "__const", "__data", "__text"])
        );

        let mut symbols = obj.symbols()
            .map(|symbol| symbol.name().into())
            .collect::<Vec<String>>();

        symbols.sort();

        assert_that!(
            &symbols.iter().map(|s| s.as_str()).collect::<Vec<&str>>(),
            contains(vec!["_main", "_malloc", "_free"])
        );
    }
}
