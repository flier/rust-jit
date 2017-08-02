use std::ptr;

use llvm::analysis::*;

use errors::Result;
use function::Function;
use module::Module;
use utils::{AsResult, DisposableMessage};

impl Module {
    /// Verify that a module is valid, taking the specified action if not.
    pub fn verify(&self, action: LLVMVerifierFailureAction) -> Result<()> {
        let mut msg = ptr::null_mut();

        if unsafe { LLVMVerifyModule(self.as_raw(), action, &mut msg) }.is_ok() {
            Ok(())
        } else {
            bail!(format!("verify {:?} failed, {}", self, msg.to_string()))
        }
    }
}

impl Function {
    /// Verify that a single function is valid, taking the specified action.
    ///
    /// Useful for debugging.
    pub fn verify(&self, action: LLVMVerifierFailureAction) -> Result<()> {
        if unsafe { LLVMVerifyFunction(self.as_raw(), action) }.is_ok() {
            Ok(())
        } else {
            bail!(format!("verify {:?} failed", self))
        }
    }

    /// Open a ghostview window displaying the CFG of the given function.
    ///
    /// This function is meant for use from the debugger.
    /// You can just say 'call F->viewCFG()' and a ghostview window should pop up from the program,
    /// displaying the CFG of the current function with the code for each basic block inside.
    /// This depends on there being a 'dot' and 'gv' program in your path.
    pub fn view_cfg(&self) {
        unsafe { LLVMViewFunctionCFG(self.as_raw()) }
    }

    /// Open a ghostview window displaying the CFG of the given function.
    ///
    /// This function is meant for use from the debugger.
    /// It works just like view_cfg, but it does not include the contents of basic blocks into the nodes,
    /// just the label.  If you are only interested in the CFG this can make the graph smaller.
    pub fn view_cfg_only(&self) {
        unsafe { LLVMViewFunctionCFGOnly(self.as_raw()) }
    }
}