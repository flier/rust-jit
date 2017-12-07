use std::io::{Cursor, Seek, SeekFrom};
use std::mem;

use llvm::disassembler::*;

use errors::Result;
use utils::{AsMutPtr, AsRaw, UncheckedCStr};

bitflags! {
    pub struct DisasmOptions: u64 {
        /// The option to produce marked up assembly.
        const USE_MARKUP = LLVMDisassembler_Option_UseMarkup;

        /// The option to print immediates as hex.
        const PRINT_IMM_HEX = LLVMDisassembler_Option_PrintImmHex;

        /// The option use the other assembler printer variant
        const ASM_PRINTER_VARIANT= LLVMDisassembler_Option_AsmPrinterVariant;

        /// The option to set comment on instructions
        const SET_INSTR_COMMENTS = LLVMDisassembler_Option_SetInstrComments;

        /// The option to print latency information alongside instructions
        const PRINT_LATENCY = LLVMDisassembler_Option_PrintLatency;
    }
}

#[derive(Debug)]
pub struct Disasm(LLVMDisasmContextRef);

inherit_from!(Disasm, LLVMDisasmContextRef);

impl Disasm {
    pub fn new<T>(
        triple_name: &str,
        tag_type: i32,
        get_op_info: Option<LLVMOpInfoCallback>,
        symbol_lookup: Option<LLVMSymbolLookupCallback>,
        data: Option<&mut T>,
    ) -> Self {
        unsafe {
            LLVMCreateDisasm(
                cstr!(triple_name),
                data.as_mut_ptr(),
                tag_type,
                mem::transmute(get_op_info),
                mem::transmute(symbol_lookup),
            )
        }.into()
    }

    pub fn for_cpu<T>(
        triple_name: &str,
        cpu: &str,
        tag_type: i32,
        get_op_info: Option<LLVMOpInfoCallback>,
        symbol_lookup: Option<LLVMSymbolLookupCallback>,
        data: Option<&mut T>,
    ) -> Self {
        unsafe {
            LLVMCreateDisasmCPU(
                cstr!(triple_name),
                cstr!(cpu),
                data.as_mut_ptr(),
                tag_type,
                mem::transmute(get_op_info),
                mem::transmute(symbol_lookup),
            )
        }.into()
    }

    pub fn for_cpu_features<T>(
        triple_name: &str,
        cpu: &str,
        features: &str,
        tag_type: i32,
        get_op_info: Option<LLVMOpInfoCallback>,
        symbol_lookup: Option<LLVMSymbolLookupCallback>,
        data: Option<&mut T>,
    ) -> Self {
        unsafe {
            LLVMCreateDisasmCPUFeatures(
                cstr!(triple_name),
                cstr!(cpu),
                cstr!(features),
                data.as_mut_ptr(),
                tag_type,
                mem::transmute(get_op_info),
                mem::transmute(symbol_lookup),
            )
        }.into()
    }

    pub fn set_options(&self, option: DisasmOptions) -> &Self {
        unsafe { LLVMSetDisasmOptions(self.as_raw(), option.bits()) };
        self
    }

    /// disassembles a single instruction using the disassembler context specified in the parameter DC.
    ///
    /// The bytes of the instruction are specified in the parameter `bytes`.
    /// The instruction is at the address specified by the `pc` parameter.
    /// If a valid instruction can be disassembled its string is returned.
    pub fn disasm_inst<T: AsRef<[u8]>>(&self, cur: &mut Cursor<T>, pc: u64) -> Result<String> {
        let mut buf = [0u8; 512];
        let offset = cur.position();

        let size = {
            let bytes = cur.get_mut();
            let bytes = &bytes.as_ref()[offset as usize..];

            trace!(
                "disassemble instruction with {} bytes:\n{}",
                bytes.len(),
                hexdump!(bytes)
            );

            unsafe {
                LLVMDisasmInstruction(
                    self.as_raw(),
                    bytes.as_ptr() as *mut u8,
                    bytes.len() as u64,
                    pc,
                    buf.as_mut_ptr(),
                    buf.len(),
                )
            }
        };

        if size > 0 {
            let inst = (buf.as_ptr() as *const i8).as_str();

            cur.seek(SeekFrom::Current(size as i64))?;

            trace!("found {} bytes instruction: {}", size, inst);

            Ok(inst.into())
        } else {
            bail!("fail to disassemble instruction");
        }
    }

    pub fn disasm_insts<'a, T: 'a + AsRef<[u8]>>(
        &'a self,
        cur: &'a mut Cursor<T>,
        pc: u64,
    ) -> DisasmInstIter<'a, T> {
        DisasmInstIter {
            disasm: self,
            cur,
            pc,
        }
    }
}

pub struct DisasmInstIter<'a, T>
where
    T: 'a + AsRef<[u8]>,
{
    disasm: &'a Disasm,
    cur: &'a mut Cursor<T>,
    pc: u64,
}

impl<'a, T> Iterator for DisasmInstIter<'a, T>
where
    T: 'a + AsRef<[u8]>,
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        self.disasm.disasm_inst(self.cur, self.pc).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use target::*;

    #[test]
    fn disasm_inst_iter() {
        NativeTarget::init().unwrap();
        NativeDisassembler::init().unwrap();
        NativeAsmPrinter::init().unwrap();

        let triple = Target::default_triple();
        let disasm = Disasm::new::<()>(&triple, 0, None, None, None);

        let mut cur = Cursor::new(vec![
            0x55,
            0x48,
            0x89,
            0xe5,
            0x48,
            0x83,
            0xec,
            0x10,
            0x48,
            0x8d,
            0x05,
            0xd1,
            0xff,
            0xff,
            0xff,
        ]);

        assert_eq!(
            disasm.disasm_insts(&mut cur, 0).collect::<Vec<String>>(),
            vec![
                "\tpushq\t%rbp",
                "\tmovq\t%rsp, %rbp",
                "\tsubq\t$16, %rsp",
                "\tleaq\t-47(%rip), %rax",
            ]
        );
    }

    #[test]
    fn disasm_option() {
        NativeTarget::init().unwrap();
        NativeDisassembler::init().unwrap();
        NativeAsmPrinter::init().unwrap();

        let triple = Target::default_triple();
        let disasm = Disasm::new::<()>(&triple, 0, None, None, None);

        let insts = vec![0x48, 0x8d, 0x05, 0xd1, 0xff, 0xff, 0xff];
        let mut cur = Cursor::new(&insts);

        assert_eq!(
            disasm.disasm_inst(&mut cur, 0).unwrap(),
            "\tleaq\t-47(%rip), %rax"
        );

        let mut cur = Cursor::new(&insts);

        disasm.set_options(DisasmOptions::USE_MARKUP);
        assert_eq!(
            disasm.disasm_inst(&mut cur, 0).unwrap(),
            "\tleaq\t<mem:-47(<reg:%rip>)>, <reg:%rax>"
        );

        let mut cur = Cursor::new(&insts);

        disasm.set_options(DisasmOptions::PRINT_IMM_HEX);
        assert_eq!(
            disasm.disasm_inst(&mut cur, 0).unwrap(),
            "\tleaq\t<mem:-0x2f(<reg:%rip>)>, <reg:%rax>"
        );
    }
}
