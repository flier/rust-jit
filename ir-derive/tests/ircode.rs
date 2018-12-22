#![feature(proc_macro_hygiene)]

#[macro_use]
extern crate matches;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_ir as ir;
#[macro_use]
extern crate llvm_ir_derive;

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

#[test]
pub fn fneg() {
    let val = 5;

    assert_matches!(ir! { fneg float %val }, FNeg{..});
}
