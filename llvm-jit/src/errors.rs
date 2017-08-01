error_chain! {
    foreign_links {
        Nul(::std::ffi::NulError);
        Io(::std::io::Error);
        Utf8(::std::str::Utf8Error);
        FromBytesWithNul(::std::ffi::FromBytesWithNulError);
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
