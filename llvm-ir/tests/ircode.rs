#![feature(proc_macro_hygiene)]

#[macro_use]
extern crate llvm_jit as jit;
#[macro_use]
extern crate llvm_ir;

use jit::insts::*;

#[test]
pub fn ret() {
    assert_eq!(ir! { ret void }, Ret::void());

    assert_eq!(ir! { ret i32 5 }, Ret::value(5));
}

#[test]
pub fn unreachable() {
    assert_eq!(ir! { unreachable }, Unreachable);
}
