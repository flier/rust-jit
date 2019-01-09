use std::borrow::Cow;

use crate::llvm::core::*;
use crate::llvm::*;
use libc::c_char;

use crate::constant::InlineAsm;
use crate::function::FunctionType;
use crate::module::Module;
use crate::utils::{unchecked_cstr, AsLLVMBool, AsRaw};

impl Module {
    pub fn inline_asm(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetModuleInlineAsm(self.as_raw(), &mut len);

            unchecked_cstr(p as *const u8, len as usize + 1)
        }
    }

    pub fn set_inline_asm<S: AsRef<str>>(&self, code: S) {
        let s = code.as_ref();

        unsafe { LLVMSetModuleInlineAsm2(self.as_raw(), s.as_ptr() as *const c_char, s.len()) }
    }

    pub fn append_inline_asm<S: AsRef<str>>(&self, code: S) {
        let s = code.as_ref();

        unsafe { LLVMAppendModuleInlineAsm(self.as_raw(), s.as_ptr() as *const c_char, s.len()) }
    }
}

impl FunctionType {
    pub fn inline_asm<S: AsRef<str>>(
        &self,
        code: S,
        constraints: S,
        side_effects: bool,
        align_stack: bool,
        dialect: LLVMInlineAsmDialect,
    ) -> InlineAsm {
        let code = code.as_ref();
        let constraints = constraints.as_ref();

        unsafe {
            LLVMGetInlineAsm(
                self.as_raw(),
                code.as_ptr() as *mut c_char,
                code.len(),
                constraints.as_ptr() as *mut c_char,
                constraints.len(),
                side_effects.as_bool(),
                align_stack.as_bool(),
                dialect,
            )
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{FunctionType, GlobalContext};

    #[test]
    fn inline_asm() {
        let ft = FunctionType::new(GlobalContext::void_t(), &[GlobalContext::int16_t()], false);
        let asm = ft.inline_asm(
            "bswap $0",
            "=r,r",
            false,
            false,
            LLVMInlineAsmDialect::LLVMInlineAsmDialectIntel,
        );

        assert_eq!(asm.to_string(), "void (i16)* asm inteldialect \"bswap $0\", \"=r,r\"");
    }
}
