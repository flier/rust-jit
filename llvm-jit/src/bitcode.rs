#![allow(deprecated)]

use std::path::Path;
use std::ptr;

use llvm::bit_reader::*;
use llvm::bit_writer::*;
use llvm::ir_reader::*;

use context::{Context, GlobalContext};
use errors::Result;
use membuf::MemoryBuffer;
use module::Module;
use utils::{AsRaw, AsResult, DisposableMessage};

impl GlobalContext {
    /// Parse the specified bitcode file, returning the module.
    pub fn parse_bitcode(buf: &MemoryBuffer) -> Result<Module> {
        let mut module = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe { LLVMParseBitcode(buf.as_raw(), &mut module, &mut msg) }
            .ok_or_else(|| {
                format!("fail to parse bitcode, {}", msg.into_string()).into()
            })
            .map(|_| module.into())
    }

    /// Read the header of the specified bitcode buffer and prepare for lazy deserialization of function bodies.
    pub fn get_bitcode_module(buf: MemoryBuffer) -> Result<Module> {
        let mut module = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe { LLVMGetBitcodeModule(buf.into_raw(), &mut module, &mut msg) }
            .ok_or_else(|| {
                format!("fail to get bitcode module, {}", msg.into_string()).into()
            })
            .map(|_| module.into())
    }
}

impl Context {
    /// Read LLVM IR from a memory buffer and convert it to an in-memory Module.
    pub fn parse_ir(&self, buf: MemoryBuffer) -> Result<Module> {
        let mut module = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe { LLVMParseIRInContext(self.as_raw(), buf.into_raw(), &mut module, &mut msg) }
            .ok_or_else(|| {
                format!("fail to parse IR code, {}", msg.into_string()).into()
            })
            .map(|_| module.into())
    }

    /// Parse the specified bitcode file, returning the module.
    pub fn parse_bitcode(&self, buf: &MemoryBuffer) -> Result<Module> {
        let mut module = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe { LLVMParseBitcodeInContext(self.as_raw(), buf.as_raw(), &mut module, &mut msg) }
            .ok_or_else(|| {
                format!("fail to parse bitcode, {}", msg.into_string()).into()
            })
            .map(|_| module.into())
    }

    /// Read the header of the specified bitcode buffer and prepare for lazy deserialization of function bodies.
    pub fn get_bitcode_module(&self, buf: MemoryBuffer) -> Result<Module> {
        let mut module = ptr::null_mut();
        let mut msg = DisposableMessage::new();

        unsafe {
            LLVMGetBitcodeModuleInContext(self.as_raw(), buf.into_raw(), &mut module, &mut msg)
                .ok_or_else(|| {
                    format!("fail to get bitcode module, {}", msg.into_string()).into()
                })
                .map(|_| module.into())
        }
    }
}

impl Module {
    ///  Write a module to the specified path.
    pub fn write_bitcode<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();

        if unsafe { LLVMWriteBitcodeToFile(self.as_raw(), cpath!(path)) } == 0 {
            Ok(())
        } else {
            bail!("fail to write bitcode to file {:?}", path);
        }
    }

    /// Writes a module to a new memory buffer.
    pub fn bitcode(&self) -> MemoryBuffer {
        unsafe { LLVMWriteBitcodeToMemoryBuffer(self.as_raw()) }.into()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Read;
    use std::str;

    use tempfile::NamedTempFile;

    use super::*;
    use errors::{Error, ErrorKind};
    use insts::Position;
    use prelude::*;

    #[test]
    fn ir_code() {
        let c = Context::new();
        let buf = MemoryBuffer::from_bytes(
            br#"
source_filename = "test"

define void @nop() {
entry:
  ret void
}"#,
            "test",
        );
        let m = c.parse_ir(buf).unwrap();

        assert!(m.get_function("nop").is_some());
    }

    #[test]
    fn bitcode() {
        let c = Context::new();
        let m = c.create_module("test");
        let mut b = c.create_builder();

        let f = m.add_function("nop", FunctionType::new(c.void_t(), &[], false));

        // Create a basic block in the function and set our builder to generate code in it.
        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        // Emit a `ret void` into the function
        b <<= ret!();

        let bitcode = m.bitcode();

        assert!(!bitcode.is_empty());

        let mut f = NamedTempFile::new().unwrap();

        m.write_bitcode(f.path()).unwrap();

        let mut data = vec![];

        assert_eq!(f.read_to_end(&mut data).unwrap(), bitcode.len());
        assert_eq!(data, bitcode.as_bytes());

        assert!(
            Context::new()
                .parse_bitcode(&bitcode)
                .unwrap()
                .get_function("nop")
                .is_some()
        );

        assert!(
            Context::new()
                .get_bitcode_module(bitcode.clone())
                .unwrap()
                .get_function("nop")
                .is_some()
        );

        assert!(
            GlobalContext::parse_bitcode(&bitcode)
                .unwrap()
                .get_function("nop")
                .is_some()
        );

        assert!(
            GlobalContext::get_bitcode_module(bitcode)
                .unwrap()
                .get_function("nop")
                .is_some()
        );

        let err = GlobalContext::get_bitcode_module(MemoryBuffer::from_bytes(b"", "test")).err();

        match err {
            Some(Error(ErrorKind::Msg(msg), _)) => {
                assert_eq!(msg, "fail to get bitcode module, Invalid bitcode signature");
            }
            _ => {
                panic!(err);
            }
        }
    }
}
