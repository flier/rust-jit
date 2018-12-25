use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

bitflags! {
    struct FastMath: u8 {
        const nnan = 0x01;
        const ninf = 0x02;
        const nsz = 0x04;
        const arcp = 0x08;
        const contract = 0x10;
        const afn = 0x20;
        const reassoc = 0x40;
        const fast = 0x80;
    }
}

impl Parse for FastMath {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut flags = FastMath::empty();

        loop {
            if input.peek(kw::nnan) {
                let _ = input.parse::<kw::nnan>()?;

                flags |= FastMath::nnan;
            } else if input.peek(kw::ninf) {
                let _ = input.parse::<kw::ninf>()?;

                flags |= FastMath::ninf;
            } else if input.peek(kw::nsz) {
                let _ = input.parse::<kw::nsz>()?;

                flags |= FastMath::nsz;
            } else if input.peek(kw::arcp) {
                let _ = input.parse::<kw::arcp>()?;

                flags |= FastMath::arcp;
            } else if input.peek(kw::contract) {
                let _ = input.parse::<kw::contract>()?;

                flags |= FastMath::contract;
            } else if input.peek(kw::afn) {
                let _ = input.parse::<kw::afn>()?;

                flags |= FastMath::afn;
            } else if input.peek(kw::reassoc) {
                let _ = input.parse::<kw::reassoc>()?;

                flags |= FastMath::reassoc;
            } else if input.peek(kw::fast) {
                let _ = input.parse::<kw::fast>()?;

                flags |= FastMath::fast;
            } else {
                break;
            }
        }

        Ok(flags)
    }
}

pub enum Expr {
    FNeg(FNeg),
    Add(Add),
    FAdd(FAdd),
    Sub(Sub),
    FSub(FSub),
    Mul(Mul),
    FMul(FMul),
    UDiv(UDiv),
    SDiv(SDiv),
    FDiv(FDiv),
    URem(URem),
    SRem(SRem),
    FRem(FRem),
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::fneg) {
            input.parse().map(Expr::FNeg)
        } else if lookahead.peek(kw::add) {
            input.parse().map(Expr::Add)
        } else if lookahead.peek(kw::fadd) {
            input.parse().map(Expr::FAdd)
        } else if lookahead.peek(kw::sub) {
            input.parse().map(Expr::Sub)
        } else if lookahead.peek(kw::fsub) {
            input.parse().map(Expr::FSub)
        } else if lookahead.peek(kw::mul) {
            input.parse().map(Expr::Mul)
        } else if lookahead.peek(kw::fmul) {
            input.parse().map(Expr::FMul)
        } else if lookahead.peek(kw::udiv) {
            input.parse().map(Expr::UDiv)
        } else if lookahead.peek(kw::sdiv) {
            input.parse().map(Expr::SDiv)
        } else if lookahead.peek(kw::fdiv) {
            input.parse().map(Expr::FDiv)
        } else if lookahead.peek(kw::urem) {
            input.parse().map(Expr::URem)
        } else if lookahead.peek(kw::srem) {
            input.parse().map(Expr::SRem)
        } else if lookahead.peek(kw::frem) {
            input.parse().map(Expr::FRem)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Expr::FNeg(fneg) => fneg.to_tokens(tokens),
            _ => unimplemented!(),
        }
    }
}

pub struct FNeg {
    fneg: kw::fneg,
    ty: Type,
    op: Operand,
}

impl Parse for FNeg {
    fn parse(input: ParseStream) -> Result<Self> {
        let fneg = input.parse()?;
        let ty = input.parse()?;
        let op = input.parse()?;

        Ok(FNeg { fneg, ty, op })
    }
}

impl ToTokens for FNeg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(ident) = self.op.ident() {
            quote! { fneg!(#ident) }
        } else {
            quote! {}
        }
        .to_tokens(tokens)
    }
}

pub struct Add {
    add: kw::add,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for Add {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Add {
            add: input.parse()?,
            nuw: input.parse::<kw::nuw>().ok(),
            nsw: input.parse::<kw::nsw>().ok(),
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct FAdd {
    fadd: kw::fadd,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FAdd {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FAdd {
            fadd: input.parse()?,
            fast_math: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct Sub {
    sub: kw::sub,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for Sub {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Sub {
            sub: input.parse()?,
            nuw: input.parse::<kw::nuw>().ok(),
            nsw: input.parse::<kw::nsw>().ok(),
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct FSub {
    fsub: kw::fsub,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FSub {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FSub {
            fsub: input.parse()?,
            fast_math: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct Mul {
    mul: kw::mul,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for Mul {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Mul {
            mul: input.parse()?,
            nuw: input.parse::<kw::nuw>().ok(),
            nsw: input.parse::<kw::nsw>().ok(),
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct FMul {
    fmul: kw::fmul,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FMul {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FMul {
            fmul: input.parse()?,
            fast_math: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct UDiv {
    udiv: kw::udiv,
    exact: Option<kw::exact>,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for UDiv {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(UDiv {
            udiv: input.parse()?,
            exact: input.parse().ok(),
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct SDiv {
    sdiv: kw::sdiv,
    exact: Option<kw::exact>,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for SDiv {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(SDiv {
            sdiv: input.parse()?,
            exact: input.parse().ok(),
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct FDiv {
    fdiv: kw::fdiv,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FDiv {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FDiv {
            fdiv: input.parse()?,
            fast_math: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct URem {
    urem: kw::urem,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for URem {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(URem {
            urem: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct SRem {
    srem: kw::srem,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for SRem {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(SRem {
            srem: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}

pub struct FRem {
    frem: kw::frem,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FRem {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FRem {
            frem: input.parse()?,
            fast_math: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}
