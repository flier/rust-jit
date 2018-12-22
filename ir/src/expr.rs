use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

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
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FAdd {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FAdd {
            fadd: input.parse()?,
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
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FSub {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FSub {
            fsub: input.parse()?,
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
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FMul {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FMul {
            fmul: input.parse()?,
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
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FDiv {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FDiv {
            fdiv: input.parse()?,
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
    ty: Type,
    op1: Operand,
    op2: Operand,
}

impl Parse for FRem {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FRem {
            frem: input.parse()?,
            ty: input.parse()?,
            op1: input.parse()?,
            op2: input.parse()?,
        })
    }
}
