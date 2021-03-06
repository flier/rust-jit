#![feature(proc_macro_hygiene)]

#[macro_use]
extern crate matches;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_ir as ir;
#[macro_use]
extern crate llvm_ir_derive;

use std::ptr;

use jit::insts::*;

#[test]
pub fn value() {
    let _ = pretty_env_logger::try_init();

    assert_eq!(ir! { true }, true);
    assert_eq!(ir! { false }, false);
    assert_eq!(ir! { 123 }, 123);
    assert_eq!(ir! { -123 }, -123);
    assert_eq!(ir! { 0xabcd }, 0xabcd);
    assert_eq!(ir! { 3.14 }, 3.14);
    assert_eq!(ir! { -3.14 }, -3.14);
    assert_eq!(ir! { 4.5e+15 }, 4.5e+15);
    assert_eq!(ir! { c"hello world" }, "hello world");

    assert_eq!(ir! { null }, ptr::null_mut::<*const u8>());

    let ir: Option<()> = ir! { none };
    assert_eq!(ir, None);

    let n: i32 = ir! { zeroinitializer };
    assert_eq!(n, 0);

    let s = ir! { { i32 4, float 17.0 } };
    assert_matches!(s.kind(), jit::llvm::LLVMValueKind::LLVMConstantStructValueKind);
    assert!(!s.ty().as_struct_ty().unwrap().is_packed());
    let s = ir! { <{ i32 4, float 17.0 }> };
    assert_matches!(s.kind(), jit::llvm::LLVMValueKind::LLVMConstantStructValueKind);
    assert!(s.ty().as_struct_ty().unwrap().is_packed());

    let a = ir! { [ i32 42, i32 11, i32 74 ] };
    assert_eq!(a.len(), 3);

    let v = ir! { < i32 42, i32 11, i32 74 > };
    assert_matches!(v, (_, _, _));
}

#[test]
pub fn ret() {
    assert_eq!(ir! { ret void }, Ret::void());

    assert_matches!(ir! { ret i32 5 }, Ret::Value(_));
}

#[test]
pub fn unreachable() {
    assert_eq!(ir! { unreachable }, Unreachable);
}

#[test]
pub fn fneg() {
    let val = 5;

    assert_matches!(ir! { fneg float %val }, FNeg{..});
    assert_matches!(ir! { fneg float 3.14 }, FNeg{..});
}

#[test]
pub fn add() {
    let x = 3;
    let y = 4;

    assert_matches!(ir! { add i32 %x, %y }, Add{..});
    assert_matches!(ir! { add i32 3, %y }, Add{..});
    assert_matches!(ir! { add i32 %x, 4 }, Add{..});
    assert_matches!(ir! { add i32 3, 4 }, Add{..});

    assert_matches!(ir! { add nsw i32 3, %y }, NSWAdd{..});
    assert_matches!(ir! { add nuw i32 %x, 4 }, NUWAdd{..});
}

#[test]
pub fn fadd() {
    let x = 3.0;
    let y = 4.0;

    assert_matches!(ir! { fadd float %x, %y }, FAdd{..});
    assert_matches!(ir! { fadd float 3.0, %y }, FAdd{..});
    assert_matches!(ir! { fadd float %x, 4.0 }, FAdd{..});
    assert_matches!(ir! { fadd float 3.0, 4.0 }, FAdd{..});
}
