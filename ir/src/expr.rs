use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

#[derive(Debug)]
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

#[derive(Debug, Parse)]
pub struct FNeg {
    fneg: kw::fneg,
    fast_math: FastMath,
    ty: Type,
    op: Operand,
}

#[derive(Debug, Parse)]
pub struct Add {
    add: kw::add,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct FAdd {
    fadd: kw::fadd,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct Sub {
    sub: kw::sub,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct FSub {
    fsub: kw::fsub,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct Mul {
    mul: kw::mul,
    nuw: Option<kw::nuw>,
    nsw: Option<kw::nsw>,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct FMul {
    fmul: kw::fmul,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct UDiv {
    udiv: kw::udiv,
    exact: Option<kw::exact>,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct SDiv {
    sdiv: kw::sdiv,
    exact: Option<kw::exact>,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct FDiv {
    fdiv: kw::fdiv,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct URem {
    urem: kw::urem,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct SRem {
    srem: kw::srem,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[derive(Debug, Parse)]
pub struct FRem {
    frem: kw::frem,
    fast_math: FastMath,
    ty: Type,
    op1: Operand,
    comma: Token![,],
    op2: Operand,
}

#[cfg(feature = "display")]
mod display {
    use std::fmt;

    use syn::token::CustomKeyword;

    use super::*;

    impl fmt::Display for FastMath {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let fast_math_flags = [
                FastMath::nnan,
                FastMath::ninf,
                FastMath::nsz,
                FastMath::arcp,
                FastMath::contract,
                FastMath::afn,
                FastMath::reassoc,
                FastMath::fast,
            ];

            itertools::join(
                fast_math_flags.into_iter().flat_map(|flag| {
                    if self.contains(*flag) {
                        Some(kw::nnan::ident())
                    } else {
                        None
                    }
                }),
                " ",
            )
            .fmt(f)
        }
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Expr::FNeg(fneg) => fneg.fmt(f),
                Expr::Add(add) => add.fmt(f),
                Expr::FAdd(fadd) => fadd.fmt(f),
                Expr::Sub(sub) => sub.fmt(f),
                Expr::FSub(fsub) => fsub.fmt(f),
                Expr::Mul(mul) => mul.fmt(f),
                Expr::FMul(fmul) => fmul.fmt(f),
                Expr::UDiv(udiv) => udiv.fmt(f),
                Expr::SDiv(sdiv) => sdiv.fmt(f),
                Expr::FDiv(fdiv) => fdiv.fmt(f),
                Expr::URem(urem) => urem.fmt(f),
                Expr::SRem(srem) => srem.fmt(f),
                Expr::FRem(frem) => frem.fmt(f),
            }
        }
    }

    impl fmt::Display for FNeg {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}",
                kw::fneg::ident(),
                if self.fast_math.is_empty() {
                    "".to_string()
                } else {
                    format!("{} ", self.fast_math)
                },
                self.ty,
                self.op
            )
        }
    }

    impl fmt::Display for Add {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{}{} {}, {}",
                kw::add::ident(),
                self.nuw.map_or("".to_string(), |_| format!("{} ", kw::nuw::ident())),
                self.nsw.map_or("".to_string(), |_| format!("{} ", kw::nsw::ident())),
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for FAdd {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::fadd::ident(),
                if self.fast_math.is_empty() {
                    "".to_string()
                } else {
                    format!("{} ", self.fast_math)
                },
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for Sub {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{}{} {}, {}",
                kw::sub::ident(),
                self.nuw.map_or("".to_string(), |_| format!("{} ", kw::nuw::ident())),
                self.nsw.map_or("".to_string(), |_| format!("{} ", kw::nsw::ident())),
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for FSub {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::fsub::ident(),
                if self.fast_math.is_empty() {
                    "".to_string()
                } else {
                    format!("{} ", self.fast_math)
                },
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for Mul {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{}{} {}, {}",
                kw::mul::ident(),
                self.nuw.map_or("".to_string(), |_| format!("{} ", kw::nuw::ident())),
                self.nsw.map_or("".to_string(), |_| format!("{} ", kw::nsw::ident())),
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for FMul {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::fmul::ident(),
                if self.fast_math.is_empty() {
                    "".to_string()
                } else {
                    format!("{} ", self.fast_math)
                },
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for UDiv {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::udiv::ident(),
                self.exact
                    .map_or("".to_string(), |_| format!("{} ", kw::exact::ident())),
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for SDiv {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::sdiv::ident(),
                self.exact
                    .map_or("".to_string(), |_| format!("{} ", kw::exact::ident())),
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for FDiv {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::fdiv::ident(),
                if self.fast_math.is_empty() {
                    "".to_string()
                } else {
                    format!("{} ", self.fast_math)
                },
                self.ty,
                self.op1,
                self.op2
            )
        }
    }

    impl fmt::Display for URem {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} {} {}, {}", kw::urem::ident(), self.ty, self.op1, self.op2)
        }
    }

    impl fmt::Display for SRem {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} {} {}, {}", kw::srem::ident(), self.ty, self.op1, self.op2)
        }
    }

    impl fmt::Display for FRem {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "{} {}{} {}, {}",
                kw::frem::ident(),
                self.fast_math,
                self.ty,
                self.op1,
                self.op2
            )
        }
    }
}

#[cfg(feature = "gen")]
mod gen {
    use proc_macro2::TokenStream;
    use quote::ToTokens;

    use super::*;

    impl ToTokens for Expr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Expr::FNeg(fneg) => fneg.to_tokens(tokens),
                Expr::Add(add) => add.to_tokens(tokens),
                Expr::FAdd(fadd) => fadd.to_tokens(tokens),
                _ => unimplemented!(),
            }
        }
    }

    impl ToTokens for FNeg {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let op = &self.op;

            quote!(fneg!(#op)).to_tokens(tokens)
        }
    }

    impl ToTokens for Add {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let lhs = &self.op1;
            let rhs = &self.op2;

            let expanded = if self.nsw.is_some() {
                quote! { add_nsw!(#lhs, #rhs) }
            } else if self.nuw.is_some() {
                quote! { add_nuw!(#lhs, #rhs) }
            } else if self.nsw.is_some() && self.nuw.is_some() {
                unimplemented!()
            } else {
                quote! { add!(#lhs, #rhs) }
            };

            expanded.to_tokens(tokens)
        }
    }

    impl ToTokens for FAdd {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let lhs = &self.op1;
            let rhs = &self.op2;

            quote!(fadd!(#lhs, #rhs)).to_tokens(tokens)
        }
    }
}
