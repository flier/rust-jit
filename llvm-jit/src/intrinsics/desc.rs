#![allow(dead_code, non_camel_case_types)]

use std::borrow::Cow;
use std::mem;

use failure::Error;
use nom;

use context::Context;
use errors::{JitError, Result};
use function::FunctionType;
use intrinsics::gen::{IntrinsicId, IIT_LONG_ENCODING_TABLE, IIT_TABLE};
use types::*;

impl IntrinsicId {
    pub fn function_type(&self, context: &Context, arg_types: &[TypeRef]) -> Result<FunctionType> {
        let entries = self.info_table_entries();

        let (result_ty, arg_tys) = IITDescriptor::parse(entries.as_ref())?;

        let result_ty = result_ty.decode_fixed_type(context, arg_types)?;
        let arg_tys = arg_tys
            .into_iter()
            .map(|descriptor| descriptor.decode_fixed_type(context, arg_types))
            .collect::<Result<Vec<TypeRef>>>()?;

        if !arg_tys.is_empty() && arg_tys.last().map_or(false, |ty| ty.is_void_ty()) {
            Ok(FunctionType::new(result_ty, &arg_tys[..arg_tys.len() - 2], true))
        } else {
            Ok(FunctionType::new(result_ty, &arg_tys, false))
        }
    }

    pub fn info_table_entries(&self) -> Cow<[u8]> {
        // Check to see if the intrinsic's type was expressible by the table.
        let mut v = IIT_TABLE[*self as usize - 1];

        // Decode the TableVal into an array of IITValues.
        if (v >> 31) != 0 {
            let idx = (v ^ (1 << 31)) as usize;

            // This is an offset into the IIT_LongEncodingTable.
            IIT_LONG_ENCODING_TABLE[idx..].into()
        } else {
            // Decode the TableVal into an array of IITValues.
            // If the entry was encoded into a single word in the table itself, decode it now.
            let mut entries = vec![];

            loop {
                entries.push((v & 0x0F) as u8);

                v >>= 4;

                if v == 0 {
                    break;
                }
            }

            entries.into()
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ArgKind {
    Any,
    AnyInteger,
    AnyFloat,
    AnyVector,
    AnyPointer,
}

impl ArgKind {
    pub fn check_type<T: Into<TypeRef>>(&self, ty: T) -> Result<TypeRef> {
        let ty = ty.into();

        match *self {
            ArgKind::Any => Ok(ty),
            ArgKind::AnyInteger if ty.is_integer_ty() => Ok(ty),
            ArgKind::AnyFloat if ty.is_floating_point_ty() => Ok(ty),
            ArgKind::AnyVector if ty.is_vector_ty() => Ok(ty),
            ArgKind::AnyPointer if ty.is_pointer_ty() => Ok(ty),
            _ => bail!(JitError::UnexpectedType(ty)),
        }
    }
}

impl From<u8> for ArgKind {
    fn from(v: u8) -> Self {
        unsafe { mem::transmute(v) }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ArgumentInfo {
    kind: ArgKind,
    index: usize,
}

impl ArgumentInfo {
    pub fn any(index: usize) -> Self {
        ArgumentInfo {
            kind: ArgKind::Any,
            index,
        }
    }

    pub fn any_integer(index: usize) -> Self {
        ArgumentInfo {
            kind: ArgKind::AnyInteger,
            index,
        }
    }

    pub fn any_float(index: usize) -> Self {
        ArgumentInfo {
            kind: ArgKind::AnyFloat,
            index,
        }
    }

    pub fn any_vector(index: usize) -> Self {
        ArgumentInfo {
            kind: ArgKind::AnyVector,
            index,
        }
    }

    pub fn any_pointer(index: usize) -> Self {
        ArgumentInfo {
            kind: ArgKind::AnyPointer,
            index,
        }
    }

    pub fn checked_type(&self, arg_types: &[TypeRef]) -> Result<TypeRef> {
        arg_types
            .get(self.index)
            .ok_or_else(|| JitError::OutOfRange(self.index).into())
            .and_then(|&ty| self.kind.check_type(*ty))
    }
}

impl From<u8> for ArgumentInfo {
    fn from(n: u8) -> Self {
        ArgumentInfo {
            kind: ArgKind::from(n & 0x07),
            index: n as usize >> 3,
        }
    }
}

impl From<ArgumentInfo> for u8 {
    fn from(arg: ArgumentInfo) -> Self {
        (arg.index << 3) as u8 + arg.kind as u8
    }
}

#[derive(Clone, Debug, PartialEq)]
enum IITDescriptor {
    Void,
    VarArg,
    MMX,
    Token,
    Metadata,
    Half,
    Float,
    Double,
    Integer(usize),
    Vector(Box<IITDescriptor>, usize),
    Pointer(Box<IITDescriptor>, u8),
    Struct(Vec<IITDescriptor>),
    Argument(ArgumentInfo),
    ExtendArgument(ArgumentInfo),
    TruncArgument(ArgumentInfo),
    HalfVecArgument(ArgumentInfo),
    SameVecWidthArgument(Box<IITDescriptor>, ArgumentInfo),
    PtrToArgument(ArgumentInfo),
    PtrToElement(ArgumentInfo),
    VecOfAnyPtrsToElt(usize, usize),
}

impl IITDescriptor {
    pub fn parse<'a>(input: &'a [u8]) -> Result<(IITDescriptor, Vec<IITDescriptor>)> {
        parse_descripters(input)
            .to_result()
            .map_err(|err| JitError::Parse(err).into())
    }

    pub fn decode_fixed_type(&self, ctxt: &Context, arg_types: &[TypeRef]) -> Result<TypeRef> {
        match *self {
            IITDescriptor::Void | IITDescriptor::VarArg => Ok(ctxt.void_t()),
            IITDescriptor::MMX => Ok(ctxt.x86_mmx_t()),
            IITDescriptor::Token => bail!("Unexpected type: token"),
            IITDescriptor::Metadata => bail!("Unexpected type: metadata"),
            IITDescriptor::Half => Ok(ctxt.half_t()),
            IITDescriptor::Float => Ok(ctxt.float_t()),
            IITDescriptor::Double => Ok(ctxt.double_t()),
            IITDescriptor::Integer(bit_width) => Ok(ctxt.int_type(bit_width)),
            IITDescriptor::Vector(ref element, size) => {
                let ty = element.decode_fixed_type(ctxt, arg_types)?;

                Ok(ty.vector_t(size).into())
            }
            IITDescriptor::Pointer(ref element, addrspace) => {
                let ty = element.decode_fixed_type(ctxt, arg_types)?;

                Ok(ty.ptr_t_in_address_space(u32::from(addrspace)).into())
            }
            IITDescriptor::Struct(ref elements) => {
                let elements = elements
                    .iter()
                    .map(|element| element.decode_fixed_type(ctxt, arg_types))
                    .collect::<Result<Vec<_>>>()?;

                Ok(ctxt.struct_t(&elements, false).into())
            }
            IITDescriptor::Argument(ref arg) => arg.checked_type(arg_types),
            IITDescriptor::ExtendArgument(ref arg) => {
                let ty = arg.checked_type(arg_types)?;

                if let Some(v) = ty.as_vector_ty() {
                    Ok(v.extended_element_vector_t().into())
                } else {
                    let bits = ty.primitive_size_in_bits().unwrap_or_default() * 2;

                    Ok(ctxt.int_type(bits))
                }
            }
            IITDescriptor::TruncArgument(ref arg) => {
                let ty = arg.checked_type(arg_types)?;

                if let Some(v) = ty.as_vector_ty() {
                    Ok(v.truncated_element_vector_t().into())
                } else {
                    let bits = ty.primitive_size_in_bits().unwrap_or_default() / 2;

                    Ok(ctxt.int_type(bits))
                }
            }
            IITDescriptor::HalfVecArgument(ref arg) => {
                let ty = arg.checked_type(arg_types)?;

                Ok(ty.as_vector_ty()
                    .ok_or_else(|| Error::from(JitError::UnexpectedType(ty)))?
                    .half_elements_vector_t()
                    .into())
            }
            IITDescriptor::SameVecWidthArgument(ref element, ref arg) => {
                let ty = arg.checked_type(arg_types)?;
                let element_ty = element.decode_fixed_type(ctxt, arg_types)?;
                let count = ty.as_vector_ty()
                    .ok_or_else(|| Error::from(JitError::UnexpectedType(ty)))?
                    .len();

                Ok(element_ty.vector_t(count).into())
            }
            IITDescriptor::PtrToArgument(ref arg) => {
                let ty = arg.checked_type(arg_types)?;

                Ok(ty.ptr_t().into())
            }
            IITDescriptor::PtrToElement(ref arg) => {
                let ty = arg.checked_type(arg_types)?;
                let vt = ty.as_vector_ty()
                    .ok_or_else(|| Error::from(JitError::UnexpectedType(ty)))?;

                Ok(vt.element_type().ptr_t().into())
            }
            IITDescriptor::VecOfAnyPtrsToElt(arg_num, _) => arg_types
                .get(arg_num)
                .cloned()
                .ok_or_else(|| Error::from(JitError::OutOfRange(arg_num))),
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    parse_descripters<(IITDescriptor, Vec<IITDescriptor>)>,
    do_parse!(
        result_type: call!(parse_descripter) >>
        arg_types: alt_complete!(map!(many_till!(parse_descripter, done), |(res, _)| res) | value!(vec![])) >>
        (
            (result_type, arg_types)
        )
    )
);

named!(done, alt_complete!(tag!("\0") | eof!()));

named!(
    parse_descripter<IITDescriptor>,
    do_parse!(
        descriptor:
            switch!(map!(call!(nom::le_u8), IIT_Info::from),
                IIT_Info::IIT_Done => value!(IITDescriptor::Void) |
                IIT_Info::IIT_VARARG => value!(IITDescriptor::VarArg) |
                IIT_Info::IIT_MMX => value!(IITDescriptor::MMX) |
                IIT_Info::IIT_TOKEN => value!(IITDescriptor::Token) |
                IIT_Info::IIT_METADATA => value!(IITDescriptor::Metadata) |
                IIT_Info::IIT_F16 => value!(IITDescriptor::Half) |
                IIT_Info::IIT_F32 => value!(IITDescriptor::Float) |
                IIT_Info::IIT_F64 => value!(IITDescriptor::Double) |
                IIT_Info::IIT_I1 => value!(IITDescriptor::Integer(1)) |
                IIT_Info::IIT_I8 => value!(IITDescriptor::Integer(8)) |
                IIT_Info::IIT_I16 => value!(IITDescriptor::Integer(16)) |
                IIT_Info::IIT_I32 => value!(IITDescriptor::Integer(32)) |
                IIT_Info::IIT_I64 => value!(IITDescriptor::Integer(64)) |
                IIT_Info::IIT_I128 => value!(IITDescriptor::Integer(128)) |
                IIT_Info::IIT_V1 => call!(parse_vector_descripter, 1) |
                IIT_Info::IIT_V2 => call!(parse_vector_descripter, 2) |
                IIT_Info::IIT_V4 => call!(parse_vector_descripter, 4) |
                IIT_Info::IIT_V8 => call!(parse_vector_descripter, 8) |
                IIT_Info::IIT_V16 => call!(parse_vector_descripter, 16) |
                IIT_Info::IIT_V32 => call!(parse_vector_descripter, 32) |
                IIT_Info::IIT_V64 => call!(parse_vector_descripter, 64) |
                IIT_Info::IIT_V512 => call!(parse_vector_descripter, 512) |
                IIT_Info::IIT_V1024 => call!(parse_vector_descripter, 1024) |
                IIT_Info::IIT_PTR => do_parse!(
                    element: call!(parse_descripter) >>
                    (
                        IITDescriptor::Pointer(Box::new(element), 0)
                    )
                ) |
                IIT_Info::IIT_ANYPTR => do_parse!(
                    addrspace: call!(nom::le_u8) >>
                    element: call!(parse_descripter) >>
                    (
                        IITDescriptor::Pointer(Box::new(element), addrspace)
                    )
                ) |
                IIT_Info::IIT_ARG => map!(call!(parse_argument_info), IITDescriptor::Argument) |
                IIT_Info::IIT_EXTEND_ARG => map!(call!(parse_argument_info), IITDescriptor::ExtendArgument) |
                IIT_Info::IIT_TRUNC_ARG => map!(call!(parse_argument_info), IITDescriptor::TruncArgument) |
                IIT_Info::IIT_HALF_VEC_ARG => map!(call!(parse_argument_info), IITDescriptor::HalfVecArgument) |
                IIT_Info::IIT_PTR_TO_ARG => map!(call!(parse_argument_info), IITDescriptor::PtrToArgument) |
                IIT_Info::IIT_PTR_TO_ELT => map!(call!(parse_argument_info), IITDescriptor::PtrToElement) |
                IIT_Info::IIT_SAME_VEC_WIDTH_ARG => do_parse!(
                    arg: call!(parse_argument_info) >>
                    element: call!(parse_descripter) >>
                    (
                        IITDescriptor::SameVecWidthArgument(Box::new(element), arg)
                    )
                ) |
                IIT_Info::IIT_VEC_OF_ANYPTRS_TO_ELT => do_parse!(
                    arg_num: call!(nom::le_u8) >>
                    ref_num: call!(nom::le_u8) >>
                    (
                        IITDescriptor::VecOfAnyPtrsToElt(arg_num as usize, ref_num as usize)
                    )
                ) |
                IIT_Info::IIT_EMPTYSTRUCT => call!(parse_struct_descripter, 0) |
                IIT_Info::IIT_STRUCT5 => call!(parse_struct_descripter, 5) |
                IIT_Info::IIT_STRUCT4 => call!(parse_struct_descripter, 4) |
                IIT_Info::IIT_STRUCT3 => call!(parse_struct_descripter, 3) |
                IIT_Info::IIT_STRUCT2 => call!(parse_struct_descripter, 2)
        ) >> (descriptor)
    )
);

named_args!(
    parse_vector_descripter(len: usize)<IITDescriptor>, do_parse!(
        element: call!(parse_descripter) >>
        (
            IITDescriptor::Vector(Box::new(element), len)
        )
    )
);

named_args!(
    parse_pointer_descripter(addrspace: u8)<IITDescriptor>, do_parse!(
        element: call!(parse_descripter) >>
        (
            IITDescriptor::Pointer(Box::new(element), addrspace)
        )
    )
);

named_args!(
    parse_struct_descripter(count: usize)<IITDescriptor>, do_parse!(
        elements: many_m_n!(count, count, parse_descripter) >>
        (
            IITDescriptor::Struct(elements)
        )
    )
);

named!(
    parse_argument_info<ArgumentInfo>,
    map!(call!(nom::le_u8), ArgumentInfo::from)
);

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum IIT_Info {
    // Common values should be encoded with 0-15.
    IIT_Done = 0,
    IIT_I1 = 1,
    IIT_I8 = 2,
    IIT_I16 = 3,
    IIT_I32 = 4,
    IIT_I64 = 5,
    IIT_F16 = 6,
    IIT_F32 = 7,
    IIT_F64 = 8,
    IIT_V2 = 9,
    IIT_V4 = 10,
    IIT_V8 = 11,
    IIT_V16 = 12,
    IIT_V32 = 13,
    IIT_PTR = 14,
    IIT_ARG = 15,

    // Values from 16+ are only encodable with the inefficient encoding.
    IIT_V64 = 16,
    IIT_MMX = 17,
    IIT_TOKEN = 18,
    IIT_METADATA = 19,
    IIT_EMPTYSTRUCT = 20,
    IIT_STRUCT2 = 21,
    IIT_STRUCT3 = 22,
    IIT_STRUCT4 = 23,
    IIT_STRUCT5 = 24,
    IIT_EXTEND_ARG = 25,
    IIT_TRUNC_ARG = 26,
    IIT_ANYPTR = 27,
    IIT_V1 = 28,
    IIT_VARARG = 29,
    IIT_HALF_VEC_ARG = 30,
    IIT_SAME_VEC_WIDTH_ARG = 31,
    IIT_PTR_TO_ARG = 32,
    IIT_PTR_TO_ELT = 33,
    IIT_VEC_OF_ANYPTRS_TO_ELT = 34,
    IIT_I128 = 35,
    IIT_V512 = 36,
    IIT_V1024 = 37,
}

impl From<u8> for IIT_Info {
    fn from(v: u8) -> Self {
        unsafe { mem::transmute(v) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref IIT_ADDRESS_OF_RETURN_ADDRESS: Vec<u8> = vec![IIT_Info::IIT_PTR as u8, IIT_Info::IIT_I8 as u8];
        static ref IIT_ANNOTATION: Vec<u8> = vec![
            IIT_Info::IIT_ARG as u8,
            ArgumentInfo::any_integer(0).into(),
            IIT_Info::IIT_ARG as u8,
            ArgumentInfo::any_integer(0).into(),
            IIT_Info::IIT_PTR as u8,
            IIT_Info::IIT_I8 as u8,
            IIT_Info::IIT_PTR as u8,
            IIT_Info::IIT_I8 as u8,
            IIT_Info::IIT_I32 as u8,
        ];
        static ref IIT_MEMSET: Vec<u8> = vec![
            IIT_Info::IIT_Done as u8,
            IIT_Info::IIT_ARG as u8,
            ArgumentInfo::any_pointer(0).into(),
            IIT_Info::IIT_I8 as u8,
            IIT_Info::IIT_ARG as u8,
            ArgumentInfo::any_integer(1).into(),
            IIT_Info::IIT_I32 as u8,
            IIT_Info::IIT_I1 as u8,
        ];
        static ref DESCRIPTORS: Vec<(&'static [u8], IITDescriptor)> = vec![
            (&[IIT_Info::IIT_Done as u8], IITDescriptor::Void),
            (&[IIT_Info::IIT_VARARG as u8], IITDescriptor::VarArg),
            (&[IIT_Info::IIT_I1 as u8], IITDescriptor::Integer(1)),
            (&[IIT_Info::IIT_I8 as u8], IITDescriptor::Integer(8)),
            (&[IIT_Info::IIT_I16 as u8], IITDescriptor::Integer(16)),
            (&[IIT_Info::IIT_I32 as u8], IITDescriptor::Integer(32)),
            (&[IIT_Info::IIT_I64 as u8], IITDescriptor::Integer(64)),
            (&[IIT_Info::IIT_I128 as u8], IITDescriptor::Integer(128)),
            (&[IIT_Info::IIT_F16 as u8], IITDescriptor::Half),
            (&[IIT_Info::IIT_F32 as u8], IITDescriptor::Float),
            (&[IIT_Info::IIT_F64 as u8], IITDescriptor::Double),
            (&[IIT_Info::IIT_MMX as u8], IITDescriptor::MMX),
            (&[IIT_Info::IIT_TOKEN as u8], IITDescriptor::Token),
            (&[IIT_Info::IIT_METADATA as u8], IITDescriptor::Metadata),
            (
                &[IIT_Info::IIT_PTR as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 0),
            ),
            (
                &[IIT_Info::IIT_ANYPTR as u8, 1, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 1),
            ),
            (
                &[IIT_Info::IIT_ARG as u8, 0],
                IITDescriptor::Argument(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_EXTEND_ARG as u8, 0],
                IITDescriptor::ExtendArgument(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_TRUNC_ARG as u8, 0],
                IITDescriptor::TruncArgument(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_HALF_VEC_ARG as u8, 0],
                IITDescriptor::HalfVecArgument(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_SAME_VEC_WIDTH_ARG as u8, 1, IIT_Info::IIT_I8 as u8],
                IITDescriptor::SameVecWidthArgument(Box::new(IITDescriptor::Integer(8)), ArgumentInfo::any_integer(0)),
            ),
            (
                &[IIT_Info::IIT_PTR_TO_ARG as u8, 0],
                IITDescriptor::PtrToArgument(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_PTR_TO_ELT as u8, 0],
                IITDescriptor::PtrToElement(ArgumentInfo::any(0)),
            ),
            (
                &[IIT_Info::IIT_VEC_OF_ANYPTRS_TO_ELT as u8, 1, 2],
                IITDescriptor::VecOfAnyPtrsToElt(1, 2),
            ),
            (
                &[IIT_Info::IIT_V1 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 1),
            ),
            (
                &[IIT_Info::IIT_V2 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 2),
            ),
            (
                &[IIT_Info::IIT_V4 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 4),
            ),
            (
                &[IIT_Info::IIT_V8 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 8),
            ),
            (
                &[IIT_Info::IIT_V16 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 16),
            ),
            (
                &[IIT_Info::IIT_V32 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 32),
            ),
            (
                &[IIT_Info::IIT_V64 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 64),
            ),
            (
                &[IIT_Info::IIT_V512 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 512),
            ),
            (
                &[IIT_Info::IIT_V1024 as u8, IIT_Info::IIT_I8 as u8],
                IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 1024),
            ),
            (&[IIT_Info::IIT_EMPTYSTRUCT as u8], IITDescriptor::Struct(vec![])),
            (
                &[
                    IIT_Info::IIT_STRUCT2 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                ],
                IITDescriptor::Struct(vec![IITDescriptor::Integer(8), IITDescriptor::Integer(8)]),
            ),
            (
                &[
                    IIT_Info::IIT_STRUCT3 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                ],
                IITDescriptor::Struct(vec![
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                ]),
            ),
            (
                &[
                    IIT_Info::IIT_STRUCT4 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                ],
                IITDescriptor::Struct(vec![
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                ]),
            ),
            (
                &[
                    IIT_Info::IIT_STRUCT5 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                    IIT_Info::IIT_I8 as u8,
                ],
                IITDescriptor::Struct(vec![
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Integer(8),
                ]),
            ),
        ];
    }

    #[test]
    fn decode_info_table_entries() {
        assert_eq!(
            IntrinsicId::addressofreturnaddress.info_table_entries().as_ref(),
            IIT_ADDRESS_OF_RETURN_ADDRESS.as_slice()
        );
        assert!(
            IntrinsicId::annotation
                .info_table_entries()
                .starts_with(IIT_ANNOTATION.as_slice())
        );
        assert_eq!(IntrinsicId::memset.info_table_entries().as_ref(), IIT_MEMSET.as_slice());

        for id in (0..IIT_TABLE.len()).map(|n| IntrinsicId::from(n as u32 + 1)) {
            assert!(!id.info_table_entries().is_empty());
        }
    }

    #[test]
    fn parse_descriptors() {
        assert_eq!(
            IITDescriptor::parse(IIT_ADDRESS_OF_RETURN_ADDRESS.as_slice()).unwrap(),
            (IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 0), vec![],)
        );

        assert_eq!(
            IITDescriptor::parse(IIT_ANNOTATION.as_slice()).unwrap(),
            (
                IITDescriptor::Argument(ArgumentInfo::any_integer(0)),
                vec![
                    IITDescriptor::Argument(ArgumentInfo::any_integer(0)),
                    IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 0),
                    IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 0),
                    IITDescriptor::Integer(32),
                ]
            )
        );

        assert_eq!(
            IITDescriptor::parse(IIT_MEMSET.as_slice()).unwrap(),
            (
                IITDescriptor::Void,
                vec![
                    IITDescriptor::Argument(ArgumentInfo::any_pointer(0)),
                    IITDescriptor::Integer(8),
                    IITDescriptor::Argument(ArgumentInfo::any_integer(1)),
                    IITDescriptor::Integer(32),
                    IITDescriptor::Integer(1),
                ]
            )
        );

        for &(input, ref descriptor) in DESCRIPTORS.iter() {
            assert_eq!(parse_descripter(input).unwrap(), (&[][..], descriptor.clone()));
        }
    }

    #[test]
    fn decode_fixed_type() {
        let ctxt = Context::new();

        assert_eq!(
            IITDescriptor::Void.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.void_t()
        );
        assert_eq!(
            IITDescriptor::VarArg.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.void_t()
        );
        assert_eq!(
            IITDescriptor::MMX.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.x86_mmx_t()
        );
        assert_eq!(
            IITDescriptor::Half.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.half_t()
        );
        assert_eq!(
            IITDescriptor::Float.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.float_t()
        );
        assert_eq!(
            IITDescriptor::Double.decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.double_t()
        );

        assert_eq!(
            IITDescriptor::Integer(1).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int1_t()
        );
        assert_eq!(
            IITDescriptor::Integer(8).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int8_t()
        );
        assert_eq!(
            IITDescriptor::Integer(16).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int16_t()
        );
        assert_eq!(
            IITDescriptor::Integer(32).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int32_t()
        );
        assert_eq!(
            IITDescriptor::Integer(64).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int64_t()
        );
        assert_eq!(
            IITDescriptor::Integer(128).decode_fixed_type(&ctxt, &[]).unwrap(),
            ctxt.int128_t()
        );

        let int8_t = ctxt.int8_t();
        let int16_t = ctxt.int16_t();

        assert_eq!(
            IITDescriptor::Vector(Box::new(IITDescriptor::Integer(8)), 4)
                .decode_fixed_type(&ctxt, &[])
                .unwrap(),
            int8_t.vector_t(4).into()
        );
        assert_eq!(
            IITDescriptor::Pointer(Box::new(IITDescriptor::Integer(8)), 0)
                .decode_fixed_type(&ctxt, &[])
                .unwrap(),
            int8_t.ptr_t().into()
        );
        assert_eq!(
            IITDescriptor::Struct(vec![IITDescriptor::Integer(8), IITDescriptor::Integer(16)])
                .decode_fixed_type(&ctxt, &[])
                .unwrap(),
            ctxt.struct_t(types![int8_t, int16_t], false).into()
        );

        assert_eq!(
            IITDescriptor::Argument(ArgumentInfo::any_integer(0))
                .decode_fixed_type(&ctxt, &[int8_t])
                .unwrap(),
            int8_t
        );
        assert_eq!(
            IITDescriptor::ExtendArgument(ArgumentInfo::any_vector(0))
                .decode_fixed_type(&ctxt, types![int8_t.vector_t(4)])
                .unwrap(),
            int16_t.vector_t(4).into()
        );
        assert_eq!(
            IITDescriptor::ExtendArgument(ArgumentInfo::any_integer(0))
                .decode_fixed_type(&ctxt, types![int8_t])
                .unwrap(),
            int16_t
        );
        assert_eq!(
            IITDescriptor::TruncArgument(ArgumentInfo::any_vector(0))
                .decode_fixed_type(&ctxt, types![int16_t.vector_t(4)])
                .unwrap(),
            int8_t.vector_t(4).into()
        );
        assert_eq!(
            IITDescriptor::TruncArgument(ArgumentInfo::any_integer(0))
                .decode_fixed_type(&ctxt, types![int16_t])
                .unwrap(),
            int8_t
        );
        assert_eq!(
            IITDescriptor::HalfVecArgument(ArgumentInfo::any_vector(0))
                .decode_fixed_type(&ctxt, types![int8_t.vector_t(4)])
                .unwrap(),
            int8_t.vector_t(2).into()
        );
        assert_eq!(
            IITDescriptor::SameVecWidthArgument(Box::new(IITDescriptor::Integer(16)), ArgumentInfo::any_vector(0))
                .decode_fixed_type(&ctxt, types![int8_t.vector_t(4)])
                .unwrap(),
            int16_t.vector_t(4).into()
        );
        assert_eq!(
            IITDescriptor::PtrToArgument(ArgumentInfo::any(0))
                .decode_fixed_type(&ctxt, types![int8_t])
                .unwrap(),
            int8_t.ptr_t().into()
        );
        assert_eq!(
            IITDescriptor::PtrToElement(ArgumentInfo::any_vector(0))
                .decode_fixed_type(&ctxt, types![int8_t.vector_t(4)])
                .unwrap(),
            int8_t.ptr_t().into()
        );
        assert_eq!(
            IITDescriptor::VecOfAnyPtrsToElt(0, 1)
                .decode_fixed_type(&ctxt, types![int8_t, int16_t])
                .unwrap(),
            int8_t.into()
        );
    }
}
