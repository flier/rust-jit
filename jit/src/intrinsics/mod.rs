mod desc;
mod gen;
mod name;

pub use self::gen::IntrinsicId;

use std::mem;

use crate::llvm::core::LLVMGetIntrinsicID;

use boolinator::Boolinator;

use crate::function::Function;
use crate::module::Module;
use crate::types::TypeRef;
use crate::utils::AsRaw;

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

impl Module {
    /// Create or insert an LLVM Function declaration for an intrinsic, and return it.
    pub fn intrinsic_declaration(&self, id: IntrinsicId, arg_types: &[TypeRef]) -> Function {
        let func_name = id.name_of(arg_types);
        let func_type = id.function_type(&self.context(), arg_types).unwrap();

        self.get_or_insert_function(func_name, func_type.return_type(), &func_type.param_types())
    }
}
