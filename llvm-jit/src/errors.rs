error_chain! {
    foreign_links {
        Nul(::std::ffi::NulError);
        Utf8(::std::str::Utf8Error);
    }
}
