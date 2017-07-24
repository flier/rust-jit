#!/usr/bin/env sh
RUST_BACKTRACE=1 RUST_LOG=llvm LLVM_SYS_40_PREFIX=/usr/local/opt/llvm LLVM_SYS_40_FFI_WORKAROUND=1 cargo test --all
