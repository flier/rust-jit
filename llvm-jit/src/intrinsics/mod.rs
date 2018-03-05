mod gen;

pub use self::gen::IntrinsicId;

use std::mem;

use llvm::core::*;
use boolinator::Boolinator;

use function::Function;
use utils::AsRaw;

impl IntrinsicId {
    /// Return the LLVM name for an intrinsic, such as "llvm.ppc.altivec.lvx".
    pub fn name(&self) -> &str {
        self::gen::INTRINSIC_NAMES[*self as usize]
    }
}

impl From<u32> for IntrinsicId {
    fn from(v: u32) -> Self {
        unsafe { mem::transmute(v) }
    }
}

impl Function {
    pub fn intrinsic_id(&self) -> Option<IntrinsicId> {
        let id = unsafe { LLVMGetIntrinsicID(self.as_raw()) };

        (id != IntrinsicId::not_intrinsic as u32).as_some(IntrinsicId::from(id))
    }
}
