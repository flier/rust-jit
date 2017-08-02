use std::borrow::Cow;
use std::ffi::CStr;
use std::slice;

use llvm::object::*;

use membuf::MemoryBuffer;
use utils::AsBool;

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
        SectionIter {
            obj: self,
            iter: None,
        }
    }

    pub fn symbols(&self) -> SymbolIter {
        SymbolIter {
            obj: self,
            iter: None,
        }
    }
}

pub struct SectionIter<'a> {
    obj: &'a ObjectFile,
    iter: Option<LLVMSectionIteratorRef>,
}

impl<'a> Drop for SectionIter<'a> {
    fn drop(&mut self) {
        if let Some(iter) = self.iter.take() {
            unsafe { LLVMDisposeSectionIterator(iter) }
        }
    }
}

impl<'a> Iterator for SectionIter<'a> {
    type Item = Section;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if let Some(iter) = self.iter {
                LLVMMoveToNextSection(iter);
            } else {
                self.iter = Some(LLVMGetSections(self.obj.as_raw()));
            }

            if let Some(iter) = self.iter {
                if LLVMIsSectionIteratorAtEnd(self.obj.as_raw(), iter).as_bool() {
                    None
                } else {
                    Some(Section(iter))
                }
            } else {
                None
            }
        }
    }
}

pub struct Section(LLVMSectionIteratorRef);

impl Section {
    fn as_raw(&self) -> LLVMSectionIteratorRef {
        self.0
    }

    pub fn name(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetSectionName(self.0)).to_string_lossy() }
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
        RelocIter {
            section: self,
            iter: None,
        }
    }
}

pub struct SymbolIter<'a> {
    obj: &'a ObjectFile,
    iter: Option<LLVMSymbolIteratorRef>,
}

impl<'a> Drop for SymbolIter<'a> {
    fn drop(&mut self) {
        if let Some(iter) = self.iter.take() {
            unsafe { LLVMDisposeSymbolIterator(iter) }
        }
    }
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if let Some(iter) = self.iter {
                LLVMMoveToNextSymbol(iter);
            } else {
                self.iter = Some(LLVMGetSymbols(self.obj.as_raw()));
            }

            if let Some(iter) = self.iter {
                if LLVMIsSymbolIteratorAtEnd(self.obj.as_raw(), iter).as_bool() {
                    None
                } else {
                    Some(Symbol(iter))
                }
            } else {
                None
            }
        }
    }
}

pub struct Symbol(LLVMSymbolIteratorRef);

impl Symbol {
    pub fn name(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetSymbolName(self.0)).to_string_lossy() }
    }

    pub fn size(&self) -> usize {
        unsafe { LLVMGetSymbolSize(self.0) as usize }
    }

    pub fn address(&self) -> u64 {
        unsafe { LLVMGetSymbolAddress(self.0) }
    }
}

pub struct RelocIter<'a> {
    section: &'a Section,
    iter: Option<LLVMRelocationIteratorRef>,
}

impl<'a> Drop for RelocIter<'a> {
    fn drop(&mut self) {
        if let Some(iter) = self.iter.take() {
            unsafe { LLVMDisposeRelocationIterator(iter) }
        }
    }
}

impl<'a> Iterator for RelocIter<'a> {
    type Item = Relocation;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if let Some(iter) = self.iter {
                LLVMMoveToNextRelocation(iter);
            } else {
                self.iter = Some(LLVMGetRelocations(self.section.as_raw()));
            }

            if let Some(iter) = self.iter {
                if LLVMIsRelocationIteratorAtEnd(self.section.as_raw(), iter).as_bool() {
                    None
                } else {
                    Some(Relocation(iter))
                }
            } else {
                None
            }
        }
    }
}

pub struct Relocation(LLVMRelocationIteratorRef);

impl Relocation {
    pub fn offset(&self) -> u64 {
        unsafe { LLVMGetRelocationOffset(self.0) }
    }

    pub fn symbol(&self) -> Symbol {
        Symbol(unsafe { LLVMGetRelocationSymbol(self.0) })
    }

    pub fn type_id(&self) -> u64 {
        unsafe { LLVMGetRelocationType(self.0) }
    }

    pub fn type_name(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetRelocationTypeName(self.0)).to_string_lossy() }
    }

    pub fn value_str(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetRelocationValueString(self.0)).to_string_lossy() }
    }
}

#[cfg(test)]
mod tests {
    use std::env;

    use hamcrest::prelude::*;

    use super::*;
    use membuf::MemoryBuffer;

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
