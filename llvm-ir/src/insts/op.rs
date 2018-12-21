use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitBool, LitChar, LitFloat, LitInt, LitStr, Result};

use crate::kw;
use crate::ty::Type;

pub enum Operand {
    Ident(Ident),
    Str(LitStr),
    Char(LitChar),
    Int(LitInt),
    Float(LitFloat),
    Bool(LitBool),
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![%]) && (input.peek2(Ident) || input.peek2(LitInt)) {
            let _rem = input.parse::<Token![%]>()?;

            if input.peek(Ident) {
                input.parse::<Ident>().map(Operand::Ident)
            } else if input.peek(LitInt) {
                input
                    .parse::<LitInt>()
                    .map(|int| Ident::new(&format!("v{}", int.value()), Span::call_site()))
                    .map(Operand::Ident)
            } else {
                unreachable!()
            }
        } else if lookahead.peek(LitStr) {
            input.parse().map(Operand::Str)
        } else if lookahead.peek(LitChar) {
            input.parse().map(Operand::Char)
        } else if lookahead.peek(LitInt) {
            input.parse().map(Operand::Int)
        } else if lookahead.peek(LitFloat) {
            input.parse().map(Operand::Float)
        } else if lookahead.peek(LitBool) {
            input.parse().map(Operand::Bool)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Operand::Ident(ident) => ident.to_tokens(tokens),
            Operand::Str(s) => s.value().to_tokens(tokens),
            Operand::Char(c) => c.value().to_tokens(tokens),
            Operand::Int(n) => n.value().to_tokens(tokens),
            Operand::Float(f) => f.value().to_tokens(tokens),
            Operand::Bool(b) => b.value.to_tokens(tokens),
        }
    }
}

pub enum Operation {
    Fneg(Fneg),
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

impl Parse for Operation {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::fneg) {
            input.parse().map(Operation::Fneg)
        } else if lookahead.peek(kw::add) {
            input.parse().map(Operation::Add)
        } else if lookahead.peek(kw::fadd) {
            input.parse().map(Operation::FAdd)
        } else if lookahead.peek(kw::sub) {
            input.parse().map(Operation::Sub)
        } else if lookahead.peek(kw::fsub) {
            input.parse().map(Operation::FSub)
        } else if lookahead.peek(kw::mul) {
            input.parse().map(Operation::Mul)
        } else if lookahead.peek(kw::fmul) {
            input.parse().map(Operation::FMul)
        } else if lookahead.peek(kw::udiv) {
            input.parse().map(Operation::UDiv)
        } else if lookahead.peek(kw::sdiv) {
            input.parse().map(Operation::SDiv)
        } else if lookahead.peek(kw::fdiv) {
            input.parse().map(Operation::FDiv)
        } else if lookahead.peek(kw::urem) {
            input.parse().map(Operation::URem)
        } else if lookahead.peek(kw::srem) {
            input.parse().map(Operation::SRem)
        } else if lookahead.peek(kw::frem) {
            input.parse().map(Operation::FRem)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Operation {
    fn to_tokens(&self, tokens: &mut TokenStream) {}
}

pub struct Fneg {
    fneg: kw::fneg,
    ty: Type,
    op: Operand,
}

impl Parse for Fneg {
    fn parse(input: ParseStream) -> Result<Self> {
        let fneg = input.parse()?;
        let ty = input.parse()?;
        let op = input.parse()?;

        Ok(Fneg { fneg, ty, op })
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
