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

custom_keyword!(x);

custom_keyword!(ret);
custom_keyword!(unreachable);
custom_keyword!(fneg);

custom_keyword!(add);
custom_keyword!(fadd);
custom_keyword!(sub);
custom_keyword!(fsub);
custom_keyword!(mul);
custom_keyword!(fmul);
custom_keyword!(udiv);
custom_keyword!(sdiv);
custom_keyword!(fdiv);
custom_keyword!(urem);
custom_keyword!(srem);
custom_keyword!(frem);
custom_keyword!(shl);
custom_keyword!(lshr);
custom_keyword!(ashr);
custom_keyword!(and);
custom_keyword!(or);
custom_keyword!(xor);

custom_keyword!(nuw);
custom_keyword!(nsw);

custom_keyword!(extractelement);
custom_keyword!(insertelement);
custom_keyword!(shufflevector);
custom_keyword!(extractvalue);
custom_keyword!(insertvalue);
custom_keyword!(alloca);
custom_keyword!(load);
custom_keyword!(store);
custom_keyword!(fence);
custom_keyword!(cmpxchg);
custom_keyword!(atomicrmw);
custom_keyword!(getelementptr);

custom_keyword!(trunc);
custom_keyword!(zext);
custom_keyword!(sext);
custom_keyword!(fptrunc);
custom_keyword!(fpext);
custom_keyword!(fptoui);
custom_keyword!(fptosi);
custom_keyword!(uitofp);
custom_keyword!(sitofp);
custom_keyword!(ptrtoint);
custom_keyword!(inttoptr);
custom_keyword!(bitcast);
custom_keyword!(addrspacecast);

custom_keyword!(exact);
custom_keyword!(to);

custom_keyword!(icmp);
custom_keyword!(fcmp);
custom_keyword!(eq);
custom_keyword!(ne);
custom_keyword!(ugt);
custom_keyword!(uge);
custom_keyword!(ult);
custom_keyword!(ule);
custom_keyword!(sgt);
custom_keyword!(sge);
custom_keyword!(slt);
custom_keyword!(sle);
custom_keyword!(oeq);
custom_keyword!(ogt);
custom_keyword!(oge);
custom_keyword!(olt);
custom_keyword!(ole);
custom_keyword!(one);
custom_keyword!(ord);
custom_keyword!(ueq);
custom_keyword!(une);
custom_keyword!(uno);

custom_keyword!(phi);
custom_keyword!(select);

custom_keyword!(call);
custom_keyword!(tail);
custom_keyword!(musttail);
custom_keyword!(notail);
custom_keyword!(va_arg);

custom_keyword!(landingpad);
custom_keyword!(cleanup);
custom_keyword!(catch);
custom_keyword!(filter);
custom_keyword!(catchpad);
custom_keyword!(within);
custom_keyword!(cleanuppad);

custom_keyword!(zeroext);
custom_keyword!(signext);
custom_keyword!(inreg);
custom_keyword!(byval);
custom_keyword!(inalloca);
custom_keyword!(sret);
custom_keyword!(align);
custom_keyword!(noalias);
custom_keyword!(nocapture);
custom_keyword!(nest);
custom_keyword!(returned);
custom_keyword!(nonnull);
custom_keyword!(dereferenceable);
custom_keyword!(dereferenceable_or_null);
custom_keyword!(swiftself);
custom_keyword!(swifterror);
