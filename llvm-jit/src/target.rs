use llvm::target::*;

macro_rules! define_target {
    ($name:ident, $init:path) => {
        pub struct $name;

        impl $name {
            pub fn init() {
                unsafe { $init() }
            }
        }
    };
    ($name:ident, $init:path, $err:expr) => {
        pub struct $name;

        impl $name {
            pub fn init() -> $crate::errors::Result<()> {
                if unsafe { $init() } == 0 {
                    Ok(())
                } else {
                    bail!($err)
                }
            }
        }
    };
}

define_target!(AllTargets, LLVM_InitializeAllTargets);
define_target!(AllTargetInfos, LLVM_InitializeAllTargetInfos);
define_target!(AllTargetMCs, LLVM_InitializeAllTargetMCs);
define_target!(AllAsmPrinters, LLVM_InitializeAllAsmPrinters);
define_target!(AllAsmParsers, LLVM_InitializeAllAsmParsers);
define_target!(AllDisassemblers, LLVM_InitializeAllDisassemblers);

define_target!(
    NativeTarget,
    LLVM_InitializeNativeTarget,
    "fail to initialize native target"
);
define_target!(
    NativeAsmParser,
    LLVM_InitializeNativeAsmParser,
    "fail to initialize native assembler parser"
);
define_target!(
    NativeAsmPrinter,
    LLVM_InitializeNativeAsmPrinter,
    "fail to initialize native assembler printer"
);
define_target!(
    NativeDisassembler,
    LLVM_InitializeNativeDisassembler,
    "fail to initialize native disassemblerer"
);
