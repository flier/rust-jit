use types::TypeRef;

error_chain! {
    foreign_links {
        Nul(::std::ffi::NulError);
        Io(::std::io::Error);
        Utf8(::std::str::Utf8Error);
        FromBytesWithNul(::std::ffi::FromBytesWithNulError);
        Parse(::nom::Err);
    }
    errors {
        OutOfRange(index: usize) {
            description("index out of range")
            display("index out of range: '{}'", index)
        }
        UnexpectedType(ty: TypeRef) {
            description("unexpected type")
            display("unexpected type: '{}'", ty)
        }
    }
}

macro_rules! hexdump {
    ($buf:expr) => (hexdump!($buf, 0));
    ($buf:expr, $off:expr) => (::hexplay::HexViewBuilder::new($buf)
                                  .codepage(::hexplay::CODEPAGE_ASCII)
                                  .address_offset($off)
                                  .row_width(16)
                                  .finish());
}
