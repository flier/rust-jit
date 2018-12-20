use syn::custom_keyword;

custom_keyword!(void);

/// 16-bit floating-point value
custom_keyword!(half);
/// 32-bit floating-point value
custom_keyword!(float);
/// 64-bit floating-point value
custom_keyword!(double);
/// 128-bit floating-point value (112-bit mantissa)
custom_keyword!(fp128);
/// 80-bit floating-point value (X87)
custom_keyword!(x86_fp80);
/// 128-bit floating-point value (two 64-bits)
custom_keyword!(ppc_fp128);

/// The x86_mmx type represents a value held in an MMX register on an x86 machine.
custom_keyword!(x86_mmx);

custom_keyword!(ret);
