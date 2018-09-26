use std::result::Result as StdResult;

use failure::Error;

use types::TypeRef;

#[derive(Debug, Fail)]
pub enum JitError {
    #[fail(display = "index out of range: {}", _0)]
    OutOfRange(usize),

    #[fail(display = "unexpected type: '{}'", _0)]
    UnexpectedType(TypeRef),
}

pub type Result<T> = StdResult<T, Error>;

macro_rules! hexdump {
    ($buf: expr) => {
        hexdump!($buf, 0)
    };
    ($buf: expr, $off: expr) => {
        ::hexplay::HexViewBuilder::new($buf)
            .codepage(::hexplay::CODEPAGE_ASCII)
            .address_offset($off)
            .row_width(16)
            .finish()
    };
}
