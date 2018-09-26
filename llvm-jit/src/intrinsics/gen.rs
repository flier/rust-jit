#![allow(non_camel_case_types)]
#![cfg_attr(
    feature = "cargo-clippy",
    allow(clippy::unreadable_literal, clippy::identity_op)
)]

#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntrinsicId {
    not_intrinsic,
    addressofreturnaddress,                       // llvm.addressofreturnaddress
    adjust_trampoline,                            // llvm.adjust.trampoline
    annotation,                                   // llvm.annotation
    assume,                                       // llvm.assume
    bitreverse,                                   // llvm.bitreverse
    bswap,                                        // llvm.bswap
    canonicalize,                                 // llvm.canonicalize
    ceil,                                         // llvm.ceil
    clear_cache,                                  // llvm.clear_cache
    codeview_annotation,                          // llvm.codeview.annotation
    convert_from_fp16,                            // llvm.convert.from.fp16
    convert_to_fp16,                              // llvm.convert.to.fp16
    copysign,                                     // llvm.copysign
    coro_alloc,                                   // llvm.coro.alloc
    coro_begin,                                   // llvm.coro.begin
    coro_destroy,                                 // llvm.coro.destroy
    coro_done,                                    // llvm.coro.done
    coro_end,                                     // llvm.coro.end
    coro_frame,                                   // llvm.coro.frame
    coro_free,                                    // llvm.coro.free
    coro_id,                                      // llvm.coro.id
    coro_param,                                   // llvm.coro.param
    coro_promise,                                 // llvm.coro.promise
    coro_resume,                                  // llvm.coro.resume
    coro_save,                                    // llvm.coro.save
    coro_size,                                    // llvm.coro.size
    coro_subfn_addr,                              // llvm.coro.subfn.addr
    coro_suspend,                                 // llvm.coro.suspend
    cos,                                          // llvm.cos
    ctlz,                                         // llvm.ctlz
    ctpop,                                        // llvm.ctpop
    cttz,                                         // llvm.cttz
    dbg_addr,                                     // llvm.dbg.addr
    dbg_declare,                                  // llvm.dbg.declare
    dbg_value,                                    // llvm.dbg.value
    debugtrap,                                    // llvm.debugtrap
    donothing,                                    // llvm.donothing
    eh_dwarf_cfa,                                 // llvm.eh.dwarf.cfa
    eh_exceptioncode,                             // llvm.eh.exceptioncode
    eh_exceptionpointer,                          // llvm.eh.exceptionpointer
    eh_return_i32,                                // llvm.eh.return.i32
    eh_return_i64,                                // llvm.eh.return.i64
    eh_sjlj_callsite,                             // llvm.eh.sjlj.callsite
    eh_sjlj_functioncontext,                      // llvm.eh.sjlj.functioncontext
    eh_sjlj_longjmp,                              // llvm.eh.sjlj.longjmp
    eh_sjlj_lsda,                                 // llvm.eh.sjlj.lsda
    eh_sjlj_setjmp,                               // llvm.eh.sjlj.setjmp
    eh_sjlj_setup_dispatch,                       // llvm.eh.sjlj.setup.dispatch
    eh_typeid_for,                                // llvm.eh.typeid.for
    eh_unwind_init,                               // llvm.eh.unwind.init
    exp,                                          // llvm.exp
    exp2,                                         // llvm.exp2
    expect,                                       // llvm.expect
    experimental_constrained_cos,                 // llvm.experimental.constrained.cos
    experimental_constrained_exp,                 // llvm.experimental.constrained.exp
    experimental_constrained_exp2,                // llvm.experimental.constrained.exp2
    experimental_constrained_fadd,                // llvm.experimental.constrained.fadd
    experimental_constrained_fdiv,                // llvm.experimental.constrained.fdiv
    experimental_constrained_fma,                 // llvm.experimental.constrained.fma
    experimental_constrained_fmul,                // llvm.experimental.constrained.fmul
    experimental_constrained_frem,                // llvm.experimental.constrained.frem
    experimental_constrained_fsub,                // llvm.experimental.constrained.fsub
    experimental_constrained_log,                 // llvm.experimental.constrained.log
    experimental_constrained_log10,               // llvm.experimental.constrained.log10
    experimental_constrained_log2,                // llvm.experimental.constrained.log2
    experimental_constrained_nearbyint,           // llvm.experimental.constrained.nearbyint
    experimental_constrained_pow,                 // llvm.experimental.constrained.pow
    experimental_constrained_powi,                // llvm.experimental.constrained.powi
    experimental_constrained_rint,                // llvm.experimental.constrained.rint
    experimental_constrained_sin,                 // llvm.experimental.constrained.sin
    experimental_constrained_sqrt,                // llvm.experimental.constrained.sqrt
    experimental_deoptimize,                      // llvm.experimental.deoptimize
    experimental_gc_relocate,                     // llvm.experimental.gc.relocate
    experimental_gc_result,                       // llvm.experimental.gc.result
    experimental_gc_statepoint,                   // llvm.experimental.gc.statepoint
    experimental_guard,                           // llvm.experimental.guard
    experimental_patchpoint_i64,                  // llvm.experimental.patchpoint.i64
    experimental_patchpoint_void,                 // llvm.experimental.patchpoint.void
    experimental_stackmap,                        // llvm.experimental.stackmap
    experimental_vector_reduce_add,               // llvm.experimental.vector.reduce.add
    experimental_vector_reduce_and,               // llvm.experimental.vector.reduce.and
    experimental_vector_reduce_fadd,              // llvm.experimental.vector.reduce.fadd
    experimental_vector_reduce_fmax,              // llvm.experimental.vector.reduce.fmax
    experimental_vector_reduce_fmin,              // llvm.experimental.vector.reduce.fmin
    experimental_vector_reduce_fmul,              // llvm.experimental.vector.reduce.fmul
    experimental_vector_reduce_mul,               // llvm.experimental.vector.reduce.mul
    experimental_vector_reduce_or,                // llvm.experimental.vector.reduce.or
    experimental_vector_reduce_smax,              // llvm.experimental.vector.reduce.smax
    experimental_vector_reduce_smin,              // llvm.experimental.vector.reduce.smin
    experimental_vector_reduce_umax,              // llvm.experimental.vector.reduce.umax
    experimental_vector_reduce_umin,              // llvm.experimental.vector.reduce.umin
    experimental_vector_reduce_xor,               // llvm.experimental.vector.reduce.xor
    fabs,                                         // llvm.fabs
    floor,                                        // llvm.floor
    flt_rounds,                                   // llvm.flt.rounds
    fma,                                          // llvm.fma
    fmuladd,                                      // llvm.fmuladd
    frameaddress,                                 // llvm.frameaddress
    gcread,                                       // llvm.gcread
    gcroot,                                       // llvm.gcroot
    gcwrite,                                      // llvm.gcwrite
    get_dynamic_area_offset,                      // llvm.get.dynamic.area.offset
    init_trampoline,                              // llvm.init.trampoline
    instrprof_increment,                          // llvm.instrprof.increment
    instrprof_increment_step,                     // llvm.instrprof.increment.step
    instrprof_value_profile,                      // llvm.instrprof.value.profile
    invariant_end,                                // llvm.invariant.end
    invariant_group_barrier,                      // llvm.invariant.group.barrier
    invariant_start,                              // llvm.invariant.start
    lifetime_end,                                 // llvm.lifetime.end
    lifetime_start,                               // llvm.lifetime.start
    load_relative,                                // llvm.load.relative
    localaddress,                                 // llvm.localaddress
    localescape,                                  // llvm.localescape
    localrecover,                                 // llvm.localrecover
    log,                                          // llvm.log
    log10,                                        // llvm.log10
    log2,                                         // llvm.log2
    longjmp,                                      // llvm.longjmp
    masked_compressstore,                         // llvm.masked.compressstore
    masked_expandload,                            // llvm.masked.expandload
    masked_gather,                                // llvm.masked.gather
    masked_load,                                  // llvm.masked.load
    masked_scatter,                               // llvm.masked.scatter
    masked_store,                                 // llvm.masked.store
    maxnum,                                       // llvm.maxnum
    memcpy,                                       // llvm.memcpy
    memcpy_element_unordered_atomic,              // llvm.memcpy.element.unordered.atomic
    memmove,                                      // llvm.memmove
    memmove_element_unordered_atomic,             // llvm.memmove.element.unordered.atomic
    memset,                                       // llvm.memset
    memset_element_unordered_atomic,              // llvm.memset.element.unordered.atomic
    minnum,                                       // llvm.minnum
    nearbyint,                                    // llvm.nearbyint
    objectsize,                                   // llvm.objectsize
    pcmarker,                                     // llvm.pcmarker
    pow,                                          // llvm.pow
    powi,                                         // llvm.powi
    prefetch,                                     // llvm.prefetch
    ptr_annotation,                               // llvm.ptr.annotation
    read_register,                                // llvm.read_register
    readcyclecounter,                             // llvm.readcyclecounter
    returnaddress,                                // llvm.returnaddress
    rint,                                         // llvm.rint
    round,                                        // llvm.round
    sadd_with_overflow,                           // llvm.sadd.with.overflow
    setjmp,                                       // llvm.setjmp
    sideeffect,                                   // llvm.sideeffect
    siglongjmp,                                   // llvm.siglongjmp
    sigsetjmp,                                    // llvm.sigsetjmp
    sin,                                          // llvm.sin
    smul_with_overflow,                           // llvm.smul.with.overflow
    sqrt,                                         // llvm.sqrt
    ssa_copy,                                     // llvm.ssa.copy
    ssub_with_overflow,                           // llvm.ssub.with.overflow
    stackguard,                                   // llvm.stackguard
    stackprotector,                               // llvm.stackprotector
    stackrestore,                                 // llvm.stackrestore
    stacksave,                                    // llvm.stacksave
    thread_pointer,                               // llvm.thread.pointer
    trap,                                         // llvm.trap
    trunc,                                        // llvm.trunc
    type_checked_load,                            // llvm.type.checked.load
    type_test,                                    // llvm.type.test
    uadd_with_overflow,                           // llvm.uadd.with.overflow
    umul_with_overflow,                           // llvm.umul.with.overflow
    usub_with_overflow,                           // llvm.usub.with.overflow
    vacopy,                                       // llvm.va_copy
    vaend,                                        // llvm.va_end
    vastart,                                      // llvm.va_start
    var_annotation,                               // llvm.var.annotation
    write_register,                               // llvm.write_register
    xray_customevent,                             // llvm.xray.customevent
    aarch64_clrex,                                // llvm.aarch64.clrex
    aarch64_crc32b,                               // llvm.aarch64.crc32b
    aarch64_crc32cb,                              // llvm.aarch64.crc32cb
    aarch64_crc32ch,                              // llvm.aarch64.crc32ch
    aarch64_crc32cw,                              // llvm.aarch64.crc32cw
    aarch64_crc32cx,                              // llvm.aarch64.crc32cx
    aarch64_crc32h,                               // llvm.aarch64.crc32h
    aarch64_crc32w,                               // llvm.aarch64.crc32w
    aarch64_crc32x,                               // llvm.aarch64.crc32x
    aarch64_crypto_aesd,                          // llvm.aarch64.crypto.aesd
    aarch64_crypto_aese,                          // llvm.aarch64.crypto.aese
    aarch64_crypto_aesimc,                        // llvm.aarch64.crypto.aesimc
    aarch64_crypto_aesmc,                         // llvm.aarch64.crypto.aesmc
    aarch64_crypto_sha1c,                         // llvm.aarch64.crypto.sha1c
    aarch64_crypto_sha1h,                         // llvm.aarch64.crypto.sha1h
    aarch64_crypto_sha1m,                         // llvm.aarch64.crypto.sha1m
    aarch64_crypto_sha1p,                         // llvm.aarch64.crypto.sha1p
    aarch64_crypto_sha1su0,                       // llvm.aarch64.crypto.sha1su0
    aarch64_crypto_sha1su1,                       // llvm.aarch64.crypto.sha1su1
    aarch64_crypto_sha256h,                       // llvm.aarch64.crypto.sha256h
    aarch64_crypto_sha256h2,                      // llvm.aarch64.crypto.sha256h2
    aarch64_crypto_sha256su0,                     // llvm.aarch64.crypto.sha256su0
    aarch64_crypto_sha256su1,                     // llvm.aarch64.crypto.sha256su1
    aarch64_dmb,                                  // llvm.aarch64.dmb
    aarch64_dsb,                                  // llvm.aarch64.dsb
    aarch64_hint,                                 // llvm.aarch64.hint
    aarch64_isb,                                  // llvm.aarch64.isb
    aarch64_ldaxp,                                // llvm.aarch64.ldaxp
    aarch64_ldaxr,                                // llvm.aarch64.ldaxr
    aarch64_ldxp,                                 // llvm.aarch64.ldxp
    aarch64_ldxr,                                 // llvm.aarch64.ldxr
    aarch64_neon_abs,                             // llvm.aarch64.neon.abs
    aarch64_neon_addhn,                           // llvm.aarch64.neon.addhn
    aarch64_neon_addp,                            // llvm.aarch64.neon.addp
    aarch64_neon_cls,                             // llvm.aarch64.neon.cls
    aarch64_neon_fabd,                            // llvm.aarch64.neon.fabd
    aarch64_neon_facge,                           // llvm.aarch64.neon.facge
    aarch64_neon_facgt,                           // llvm.aarch64.neon.facgt
    aarch64_neon_faddv,                           // llvm.aarch64.neon.faddv
    aarch64_neon_fcvtas,                          // llvm.aarch64.neon.fcvtas
    aarch64_neon_fcvtau,                          // llvm.aarch64.neon.fcvtau
    aarch64_neon_fcvtms,                          // llvm.aarch64.neon.fcvtms
    aarch64_neon_fcvtmu,                          // llvm.aarch64.neon.fcvtmu
    aarch64_neon_fcvtns,                          // llvm.aarch64.neon.fcvtns
    aarch64_neon_fcvtnu,                          // llvm.aarch64.neon.fcvtnu
    aarch64_neon_fcvtps,                          // llvm.aarch64.neon.fcvtps
    aarch64_neon_fcvtpu,                          // llvm.aarch64.neon.fcvtpu
    aarch64_neon_fcvtxn,                          // llvm.aarch64.neon.fcvtxn
    aarch64_neon_fcvtzs,                          // llvm.aarch64.neon.fcvtzs
    aarch64_neon_fcvtzu,                          // llvm.aarch64.neon.fcvtzu
    aarch64_neon_fmax,                            // llvm.aarch64.neon.fmax
    aarch64_neon_fmaxnm,                          // llvm.aarch64.neon.fmaxnm
    aarch64_neon_fmaxnmp,                         // llvm.aarch64.neon.fmaxnmp
    aarch64_neon_fmaxnmv,                         // llvm.aarch64.neon.fmaxnmv
    aarch64_neon_fmaxp,                           // llvm.aarch64.neon.fmaxp
    aarch64_neon_fmaxv,                           // llvm.aarch64.neon.fmaxv
    aarch64_neon_fmin,                            // llvm.aarch64.neon.fmin
    aarch64_neon_fminnm,                          // llvm.aarch64.neon.fminnm
    aarch64_neon_fminnmp,                         // llvm.aarch64.neon.fminnmp
    aarch64_neon_fminnmv,                         // llvm.aarch64.neon.fminnmv
    aarch64_neon_fminp,                           // llvm.aarch64.neon.fminp
    aarch64_neon_fminv,                           // llvm.aarch64.neon.fminv
    aarch64_neon_fmulx,                           // llvm.aarch64.neon.fmulx
    aarch64_neon_frecpe,                          // llvm.aarch64.neon.frecpe
    aarch64_neon_frecps,                          // llvm.aarch64.neon.frecps
    aarch64_neon_frecpx,                          // llvm.aarch64.neon.frecpx
    aarch64_neon_frintn,                          // llvm.aarch64.neon.frintn
    aarch64_neon_frsqrte,                         // llvm.aarch64.neon.frsqrte
    aarch64_neon_frsqrts,                         // llvm.aarch64.neon.frsqrts
    aarch64_neon_ld1x2,                           // llvm.aarch64.neon.ld1x2
    aarch64_neon_ld1x3,                           // llvm.aarch64.neon.ld1x3
    aarch64_neon_ld1x4,                           // llvm.aarch64.neon.ld1x4
    aarch64_neon_ld2,                             // llvm.aarch64.neon.ld2
    aarch64_neon_ld2lane,                         // llvm.aarch64.neon.ld2lane
    aarch64_neon_ld2r,                            // llvm.aarch64.neon.ld2r
    aarch64_neon_ld3,                             // llvm.aarch64.neon.ld3
    aarch64_neon_ld3lane,                         // llvm.aarch64.neon.ld3lane
    aarch64_neon_ld3r,                            // llvm.aarch64.neon.ld3r
    aarch64_neon_ld4,                             // llvm.aarch64.neon.ld4
    aarch64_neon_ld4lane,                         // llvm.aarch64.neon.ld4lane
    aarch64_neon_ld4r,                            // llvm.aarch64.neon.ld4r
    aarch64_neon_pmul,                            // llvm.aarch64.neon.pmul
    aarch64_neon_pmull,                           // llvm.aarch64.neon.pmull
    aarch64_neon_pmull64,                         // llvm.aarch64.neon.pmull64
    aarch64_neon_raddhn,                          // llvm.aarch64.neon.raddhn
    aarch64_neon_rbit,                            // llvm.aarch64.neon.rbit
    aarch64_neon_rshrn,                           // llvm.aarch64.neon.rshrn
    aarch64_neon_rsubhn,                          // llvm.aarch64.neon.rsubhn
    aarch64_neon_sabd,                            // llvm.aarch64.neon.sabd
    aarch64_neon_saddlp,                          // llvm.aarch64.neon.saddlp
    aarch64_neon_saddlv,                          // llvm.aarch64.neon.saddlv
    aarch64_neon_saddv,                           // llvm.aarch64.neon.saddv
    aarch64_neon_scalar_sqxtn,                    // llvm.aarch64.neon.scalar.sqxtn
    aarch64_neon_scalar_sqxtun,                   // llvm.aarch64.neon.scalar.sqxtun
    aarch64_neon_scalar_uqxtn,                    // llvm.aarch64.neon.scalar.uqxtn
    aarch64_neon_shadd,                           // llvm.aarch64.neon.shadd
    aarch64_neon_shll,                            // llvm.aarch64.neon.shll
    aarch64_neon_shsub,                           // llvm.aarch64.neon.shsub
    aarch64_neon_smax,                            // llvm.aarch64.neon.smax
    aarch64_neon_smaxp,                           // llvm.aarch64.neon.smaxp
    aarch64_neon_smaxv,                           // llvm.aarch64.neon.smaxv
    aarch64_neon_smin,                            // llvm.aarch64.neon.smin
    aarch64_neon_sminp,                           // llvm.aarch64.neon.sminp
    aarch64_neon_sminv,                           // llvm.aarch64.neon.sminv
    aarch64_neon_smull,                           // llvm.aarch64.neon.smull
    aarch64_neon_sqabs,                           // llvm.aarch64.neon.sqabs
    aarch64_neon_sqadd,                           // llvm.aarch64.neon.sqadd
    aarch64_neon_sqdmulh,                         // llvm.aarch64.neon.sqdmulh
    aarch64_neon_sqdmull,                         // llvm.aarch64.neon.sqdmull
    aarch64_neon_sqdmulls_scalar,                 // llvm.aarch64.neon.sqdmulls.scalar
    aarch64_neon_sqneg,                           // llvm.aarch64.neon.sqneg
    aarch64_neon_sqrdmulh,                        // llvm.aarch64.neon.sqrdmulh
    aarch64_neon_sqrshl,                          // llvm.aarch64.neon.sqrshl
    aarch64_neon_sqrshrn,                         // llvm.aarch64.neon.sqrshrn
    aarch64_neon_sqrshrun,                        // llvm.aarch64.neon.sqrshrun
    aarch64_neon_sqshl,                           // llvm.aarch64.neon.sqshl
    aarch64_neon_sqshlu,                          // llvm.aarch64.neon.sqshlu
    aarch64_neon_sqshrn,                          // llvm.aarch64.neon.sqshrn
    aarch64_neon_sqshrun,                         // llvm.aarch64.neon.sqshrun
    aarch64_neon_sqsub,                           // llvm.aarch64.neon.sqsub
    aarch64_neon_sqxtn,                           // llvm.aarch64.neon.sqxtn
    aarch64_neon_sqxtun,                          // llvm.aarch64.neon.sqxtun
    aarch64_neon_srhadd,                          // llvm.aarch64.neon.srhadd
    aarch64_neon_srshl,                           // llvm.aarch64.neon.srshl
    aarch64_neon_sshl,                            // llvm.aarch64.neon.sshl
    aarch64_neon_sshll,                           // llvm.aarch64.neon.sshll
    aarch64_neon_st1x2,                           // llvm.aarch64.neon.st1x2
    aarch64_neon_st1x3,                           // llvm.aarch64.neon.st1x3
    aarch64_neon_st1x4,                           // llvm.aarch64.neon.st1x4
    aarch64_neon_st2,                             // llvm.aarch64.neon.st2
    aarch64_neon_st2lane,                         // llvm.aarch64.neon.st2lane
    aarch64_neon_st3,                             // llvm.aarch64.neon.st3
    aarch64_neon_st3lane,                         // llvm.aarch64.neon.st3lane
    aarch64_neon_st4,                             // llvm.aarch64.neon.st4
    aarch64_neon_st4lane,                         // llvm.aarch64.neon.st4lane
    aarch64_neon_subhn,                           // llvm.aarch64.neon.subhn
    aarch64_neon_suqadd,                          // llvm.aarch64.neon.suqadd
    aarch64_neon_tbl1,                            // llvm.aarch64.neon.tbl1
    aarch64_neon_tbl2,                            // llvm.aarch64.neon.tbl2
    aarch64_neon_tbl3,                            // llvm.aarch64.neon.tbl3
    aarch64_neon_tbl4,                            // llvm.aarch64.neon.tbl4
    aarch64_neon_tbx1,                            // llvm.aarch64.neon.tbx1
    aarch64_neon_tbx2,                            // llvm.aarch64.neon.tbx2
    aarch64_neon_tbx3,                            // llvm.aarch64.neon.tbx3
    aarch64_neon_tbx4,                            // llvm.aarch64.neon.tbx4
    aarch64_neon_uabd,                            // llvm.aarch64.neon.uabd
    aarch64_neon_uaddlp,                          // llvm.aarch64.neon.uaddlp
    aarch64_neon_uaddlv,                          // llvm.aarch64.neon.uaddlv
    aarch64_neon_uaddv,                           // llvm.aarch64.neon.uaddv
    aarch64_neon_uhadd,                           // llvm.aarch64.neon.uhadd
    aarch64_neon_uhsub,                           // llvm.aarch64.neon.uhsub
    aarch64_neon_umax,                            // llvm.aarch64.neon.umax
    aarch64_neon_umaxp,                           // llvm.aarch64.neon.umaxp
    aarch64_neon_umaxv,                           // llvm.aarch64.neon.umaxv
    aarch64_neon_umin,                            // llvm.aarch64.neon.umin
    aarch64_neon_uminp,                           // llvm.aarch64.neon.uminp
    aarch64_neon_uminv,                           // llvm.aarch64.neon.uminv
    aarch64_neon_umull,                           // llvm.aarch64.neon.umull
    aarch64_neon_uqadd,                           // llvm.aarch64.neon.uqadd
    aarch64_neon_uqrshl,                          // llvm.aarch64.neon.uqrshl
    aarch64_neon_uqrshrn,                         // llvm.aarch64.neon.uqrshrn
    aarch64_neon_uqshl,                           // llvm.aarch64.neon.uqshl
    aarch64_neon_uqshrn,                          // llvm.aarch64.neon.uqshrn
    aarch64_neon_uqsub,                           // llvm.aarch64.neon.uqsub
    aarch64_neon_uqxtn,                           // llvm.aarch64.neon.uqxtn
    aarch64_neon_urecpe,                          // llvm.aarch64.neon.urecpe
    aarch64_neon_urhadd,                          // llvm.aarch64.neon.urhadd
    aarch64_neon_urshl,                           // llvm.aarch64.neon.urshl
    aarch64_neon_ursqrte,                         // llvm.aarch64.neon.ursqrte
    aarch64_neon_ushl,                            // llvm.aarch64.neon.ushl
    aarch64_neon_ushll,                           // llvm.aarch64.neon.ushll
    aarch64_neon_usqadd,                          // llvm.aarch64.neon.usqadd
    aarch64_neon_vcopy_lane,                      // llvm.aarch64.neon.vcopy.lane
    aarch64_neon_vcvtfp2fxs,                      // llvm.aarch64.neon.vcvtfp2fxs
    aarch64_neon_vcvtfp2fxu,                      // llvm.aarch64.neon.vcvtfp2fxu
    aarch64_neon_vcvtfp2hf,                       // llvm.aarch64.neon.vcvtfp2hf
    aarch64_neon_vcvtfxs2fp,                      // llvm.aarch64.neon.vcvtfxs2fp
    aarch64_neon_vcvtfxu2fp,                      // llvm.aarch64.neon.vcvtfxu2fp
    aarch64_neon_vcvthf2fp,                       // llvm.aarch64.neon.vcvthf2fp
    aarch64_neon_vsli,                            // llvm.aarch64.neon.vsli
    aarch64_neon_vsri,                            // llvm.aarch64.neon.vsri
    aarch64_sdiv,                                 // llvm.aarch64.sdiv
    aarch64_sisd_fabd,                            // llvm.aarch64.sisd.fabd
    aarch64_sisd_fcvtxn,                          // llvm.aarch64.sisd.fcvtxn
    aarch64_stlxp,                                // llvm.aarch64.stlxp
    aarch64_stlxr,                                // llvm.aarch64.stlxr
    aarch64_stxp,                                 // llvm.aarch64.stxp
    aarch64_stxr,                                 // llvm.aarch64.stxr
    aarch64_udiv,                                 // llvm.aarch64.udiv
    amdgcn_alignbit,                              // llvm.amdgcn.alignbit
    amdgcn_alignbyte,                             // llvm.amdgcn.alignbyte
    amdgcn_atomic_dec,                            // llvm.amdgcn.atomic.dec
    amdgcn_atomic_inc,                            // llvm.amdgcn.atomic.inc
    amdgcn_break,                                 // llvm.amdgcn.break
    amdgcn_buffer_atomic_add,                     // llvm.amdgcn.buffer.atomic.add
    amdgcn_buffer_atomic_and,                     // llvm.amdgcn.buffer.atomic.and
    amdgcn_buffer_atomic_cmpswap,                 // llvm.amdgcn.buffer.atomic.cmpswap
    amdgcn_buffer_atomic_or,                      // llvm.amdgcn.buffer.atomic.or
    amdgcn_buffer_atomic_smax,                    // llvm.amdgcn.buffer.atomic.smax
    amdgcn_buffer_atomic_smin,                    // llvm.amdgcn.buffer.atomic.smin
    amdgcn_buffer_atomic_sub,                     // llvm.amdgcn.buffer.atomic.sub
    amdgcn_buffer_atomic_swap,                    // llvm.amdgcn.buffer.atomic.swap
    amdgcn_buffer_atomic_umax,                    // llvm.amdgcn.buffer.atomic.umax
    amdgcn_buffer_atomic_umin,                    // llvm.amdgcn.buffer.atomic.umin
    amdgcn_buffer_atomic_xor,                     // llvm.amdgcn.buffer.atomic.xor
    amdgcn_buffer_load,                           // llvm.amdgcn.buffer.load
    amdgcn_buffer_load_format,                    // llvm.amdgcn.buffer.load.format
    amdgcn_buffer_store,                          // llvm.amdgcn.buffer.store
    amdgcn_buffer_store_format,                   // llvm.amdgcn.buffer.store.format
    amdgcn_buffer_wbinvl1,                        // llvm.amdgcn.buffer.wbinvl1
    amdgcn_buffer_wbinvl1_sc,                     // llvm.amdgcn.buffer.wbinvl1.sc
    amdgcn_buffer_wbinvl1_vol,                    // llvm.amdgcn.buffer.wbinvl1.vol
    amdgcn_class,                                 // llvm.amdgcn.class
    amdgcn_cos,                                   // llvm.amdgcn.cos
    amdgcn_cubeid,                                // llvm.amdgcn.cubeid
    amdgcn_cubema,                                // llvm.amdgcn.cubema
    amdgcn_cubesc,                                // llvm.amdgcn.cubesc
    amdgcn_cubetc,                                // llvm.amdgcn.cubetc
    amdgcn_cvt_pk_i16,                            // llvm.amdgcn.cvt.pk.i16
    amdgcn_cvt_pk_u16,                            // llvm.amdgcn.cvt.pk.u16
    amdgcn_cvt_pk_u8_f32,                         // llvm.amdgcn.cvt.pk.u8.f32
    amdgcn_cvt_pknorm_i16,                        // llvm.amdgcn.cvt.pknorm.i16
    amdgcn_cvt_pknorm_u16,                        // llvm.amdgcn.cvt.pknorm.u16
    amdgcn_cvt_pkrtz,                             // llvm.amdgcn.cvt.pkrtz
    amdgcn_dispatch_id,                           // llvm.amdgcn.dispatch.id
    amdgcn_dispatch_ptr,                          // llvm.amdgcn.dispatch.ptr
    amdgcn_div_fixup,                             // llvm.amdgcn.div.fixup
    amdgcn_div_fmas,                              // llvm.amdgcn.div.fmas
    amdgcn_div_scale,                             // llvm.amdgcn.div.scale
    amdgcn_ds_bpermute,                           // llvm.amdgcn.ds.bpermute
    amdgcn_ds_permute,                            // llvm.amdgcn.ds.permute
    amdgcn_ds_swizzle,                            // llvm.amdgcn.ds.swizzle
    amdgcn_else,                                  // llvm.amdgcn.else
    amdgcn_else_break,                            // llvm.amdgcn.else.break
    amdgcn_end_cf,                                // llvm.amdgcn.end.cf
    amdgcn_exp,                                   // llvm.amdgcn.exp
    amdgcn_exp_compr,                             // llvm.amdgcn.exp.compr
    amdgcn_fcmp,                                  // llvm.amdgcn.fcmp
    amdgcn_fdiv_fast,                             // llvm.amdgcn.fdiv.fast
    amdgcn_fmed3,                                 // llvm.amdgcn.fmed3
    amdgcn_fmul_legacy,                           // llvm.amdgcn.fmul.legacy
    amdgcn_fract,                                 // llvm.amdgcn.fract
    amdgcn_frexp_exp,                             // llvm.amdgcn.frexp.exp
    amdgcn_frexp_mant,                            // llvm.amdgcn.frexp.mant
    amdgcn_groupstaticsize,                       // llvm.amdgcn.groupstaticsize
    amdgcn_icmp,                                  // llvm.amdgcn.icmp
    amdgcn_if,                                    // llvm.amdgcn.if
    amdgcn_if_break,                              // llvm.amdgcn.if.break
    amdgcn_image_atomic_add,                      // llvm.amdgcn.image.atomic.add
    amdgcn_image_atomic_and,                      // llvm.amdgcn.image.atomic.and
    amdgcn_image_atomic_cmpswap,                  // llvm.amdgcn.image.atomic.cmpswap
    amdgcn_image_atomic_dec,                      // llvm.amdgcn.image.atomic.dec
    amdgcn_image_atomic_inc,                      // llvm.amdgcn.image.atomic.inc
    amdgcn_image_atomic_or,                       // llvm.amdgcn.image.atomic.or
    amdgcn_image_atomic_smax,                     // llvm.amdgcn.image.atomic.smax
    amdgcn_image_atomic_smin,                     // llvm.amdgcn.image.atomic.smin
    amdgcn_image_atomic_sub,                      // llvm.amdgcn.image.atomic.sub
    amdgcn_image_atomic_swap,                     // llvm.amdgcn.image.atomic.swap
    amdgcn_image_atomic_umax,                     // llvm.amdgcn.image.atomic.umax
    amdgcn_image_atomic_umin,                     // llvm.amdgcn.image.atomic.umin
    amdgcn_image_atomic_xor,                      // llvm.amdgcn.image.atomic.xor
    amdgcn_image_gather4,                         // llvm.amdgcn.image.gather4
    amdgcn_image_gather4_b,                       // llvm.amdgcn.image.gather4.b
    amdgcn_image_gather4_b_cl,                    // llvm.amdgcn.image.gather4.b.cl
    amdgcn_image_gather4_b_cl_o,                  // llvm.amdgcn.image.gather4.b.cl.o
    amdgcn_image_gather4_b_o,                     // llvm.amdgcn.image.gather4.b.o
    amdgcn_image_gather4_c,                       // llvm.amdgcn.image.gather4.c
    amdgcn_image_gather4_c_b,                     // llvm.amdgcn.image.gather4.c.b
    amdgcn_image_gather4_c_b_cl,                  // llvm.amdgcn.image.gather4.c.b.cl
    amdgcn_image_gather4_c_b_cl_o,                // llvm.amdgcn.image.gather4.c.b.cl.o
    amdgcn_image_gather4_c_b_o,                   // llvm.amdgcn.image.gather4.c.b.o
    amdgcn_image_gather4_c_cl,                    // llvm.amdgcn.image.gather4.c.cl
    amdgcn_image_gather4_c_cl_o,                  // llvm.amdgcn.image.gather4.c.cl.o
    amdgcn_image_gather4_c_l,                     // llvm.amdgcn.image.gather4.c.l
    amdgcn_image_gather4_c_l_o,                   // llvm.amdgcn.image.gather4.c.l.o
    amdgcn_image_gather4_c_lz,                    // llvm.amdgcn.image.gather4.c.lz
    amdgcn_image_gather4_c_lz_o,                  // llvm.amdgcn.image.gather4.c.lz.o
    amdgcn_image_gather4_c_o,                     // llvm.amdgcn.image.gather4.c.o
    amdgcn_image_gather4_cl,                      // llvm.amdgcn.image.gather4.cl
    amdgcn_image_gather4_cl_o,                    // llvm.amdgcn.image.gather4.cl.o
    amdgcn_image_gather4_l,                       // llvm.amdgcn.image.gather4.l
    amdgcn_image_gather4_l_o,                     // llvm.amdgcn.image.gather4.l.o
    amdgcn_image_gather4_lz,                      // llvm.amdgcn.image.gather4.lz
    amdgcn_image_gather4_lz_o,                    // llvm.amdgcn.image.gather4.lz.o
    amdgcn_image_gather4_o,                       // llvm.amdgcn.image.gather4.o
    amdgcn_image_getlod,                          // llvm.amdgcn.image.getlod
    amdgcn_image_getresinfo,                      // llvm.amdgcn.image.getresinfo
    amdgcn_image_load,                            // llvm.amdgcn.image.load
    amdgcn_image_load_mip,                        // llvm.amdgcn.image.load.mip
    amdgcn_image_sample,                          // llvm.amdgcn.image.sample
    amdgcn_image_sample_b,                        // llvm.amdgcn.image.sample.b
    amdgcn_image_sample_b_cl,                     // llvm.amdgcn.image.sample.b.cl
    amdgcn_image_sample_b_cl_o,                   // llvm.amdgcn.image.sample.b.cl.o
    amdgcn_image_sample_b_o,                      // llvm.amdgcn.image.sample.b.o
    amdgcn_image_sample_c,                        // llvm.amdgcn.image.sample.c
    amdgcn_image_sample_c_b,                      // llvm.amdgcn.image.sample.c.b
    amdgcn_image_sample_c_b_cl,                   // llvm.amdgcn.image.sample.c.b.cl
    amdgcn_image_sample_c_b_cl_o,                 // llvm.amdgcn.image.sample.c.b.cl.o
    amdgcn_image_sample_c_b_o,                    // llvm.amdgcn.image.sample.c.b.o
    amdgcn_image_sample_c_cd,                     // llvm.amdgcn.image.sample.c.cd
    amdgcn_image_sample_c_cd_cl,                  // llvm.amdgcn.image.sample.c.cd.cl
    amdgcn_image_sample_c_cd_cl_o,                // llvm.amdgcn.image.sample.c.cd.cl.o
    amdgcn_image_sample_c_cd_o,                   // llvm.amdgcn.image.sample.c.cd.o
    amdgcn_image_sample_c_cl,                     // llvm.amdgcn.image.sample.c.cl
    amdgcn_image_sample_c_cl_o,                   // llvm.amdgcn.image.sample.c.cl.o
    amdgcn_image_sample_c_d,                      // llvm.amdgcn.image.sample.c.d
    amdgcn_image_sample_c_d_cl,                   // llvm.amdgcn.image.sample.c.d.cl
    amdgcn_image_sample_c_d_cl_o,                 // llvm.amdgcn.image.sample.c.d.cl.o
    amdgcn_image_sample_c_d_o,                    // llvm.amdgcn.image.sample.c.d.o
    amdgcn_image_sample_c_l,                      // llvm.amdgcn.image.sample.c.l
    amdgcn_image_sample_c_l_o,                    // llvm.amdgcn.image.sample.c.l.o
    amdgcn_image_sample_c_lz,                     // llvm.amdgcn.image.sample.c.lz
    amdgcn_image_sample_c_lz_o,                   // llvm.amdgcn.image.sample.c.lz.o
    amdgcn_image_sample_c_o,                      // llvm.amdgcn.image.sample.c.o
    amdgcn_image_sample_cd,                       // llvm.amdgcn.image.sample.cd
    amdgcn_image_sample_cd_cl,                    // llvm.amdgcn.image.sample.cd.cl
    amdgcn_image_sample_cd_cl_o,                  // llvm.amdgcn.image.sample.cd.cl.o
    amdgcn_image_sample_cd_o,                     // llvm.amdgcn.image.sample.cd.o
    amdgcn_image_sample_cl,                       // llvm.amdgcn.image.sample.cl
    amdgcn_image_sample_cl_o,                     // llvm.amdgcn.image.sample.cl.o
    amdgcn_image_sample_d,                        // llvm.amdgcn.image.sample.d
    amdgcn_image_sample_d_cl,                     // llvm.amdgcn.image.sample.d.cl
    amdgcn_image_sample_d_cl_o,                   // llvm.amdgcn.image.sample.d.cl.o
    amdgcn_image_sample_d_o,                      // llvm.amdgcn.image.sample.d.o
    amdgcn_image_sample_l,                        // llvm.amdgcn.image.sample.l
    amdgcn_image_sample_l_o,                      // llvm.amdgcn.image.sample.l.o
    amdgcn_image_sample_lz,                       // llvm.amdgcn.image.sample.lz
    amdgcn_image_sample_lz_o,                     // llvm.amdgcn.image.sample.lz.o
    amdgcn_image_sample_o,                        // llvm.amdgcn.image.sample.o
    amdgcn_image_store,                           // llvm.amdgcn.image.store
    amdgcn_image_store_mip,                       // llvm.amdgcn.image.store.mip
    amdgcn_implicit_buffer_ptr,                   // llvm.amdgcn.implicit.buffer.ptr
    amdgcn_implicitarg_ptr,                       // llvm.amdgcn.implicitarg.ptr
    amdgcn_init_exec,                             // llvm.amdgcn.init.exec
    amdgcn_init_exec_from_input,                  // llvm.amdgcn.init.exec.from.input
    amdgcn_interp_mov,                            // llvm.amdgcn.interp.mov
    amdgcn_interp_p1,                             // llvm.amdgcn.interp.p1
    amdgcn_interp_p2,                             // llvm.amdgcn.interp.p2
    amdgcn_kernarg_segment_ptr,                   // llvm.amdgcn.kernarg.segment.ptr
    amdgcn_kill,                                  // llvm.amdgcn.kill
    amdgcn_ldexp,                                 // llvm.amdgcn.ldexp
    amdgcn_lerp,                                  // llvm.amdgcn.lerp
    amdgcn_log_clamp,                             // llvm.amdgcn.log.clamp
    amdgcn_loop,                                  // llvm.amdgcn.loop
    amdgcn_mbcnt_hi,                              // llvm.amdgcn.mbcnt.hi
    amdgcn_mbcnt_lo,                              // llvm.amdgcn.mbcnt.lo
    amdgcn_mov_dpp,                               // llvm.amdgcn.mov.dpp
    amdgcn_mqsad_pk_u16_u8,                       // llvm.amdgcn.mqsad.pk.u16.u8
    amdgcn_mqsad_u32_u8,                          // llvm.amdgcn.mqsad.u32.u8
    amdgcn_msad_u8,                               // llvm.amdgcn.msad.u8
    amdgcn_ps_live,                               // llvm.amdgcn.ps.live
    amdgcn_qsad_pk_u16_u8,                        // llvm.amdgcn.qsad.pk.u16.u8
    amdgcn_queue_ptr,                             // llvm.amdgcn.queue.ptr
    amdgcn_rcp,                                   // llvm.amdgcn.rcp
    amdgcn_rcp_legacy,                            // llvm.amdgcn.rcp.legacy
    amdgcn_readfirstlane,                         // llvm.amdgcn.readfirstlane
    amdgcn_readlane,                              // llvm.amdgcn.readlane
    amdgcn_rsq,                                   // llvm.amdgcn.rsq
    amdgcn_rsq_clamp,                             // llvm.amdgcn.rsq.clamp
    amdgcn_rsq_legacy,                            // llvm.amdgcn.rsq.legacy
    amdgcn_s_barrier,                             // llvm.amdgcn.s.barrier
    amdgcn_s_dcache_inv,                          // llvm.amdgcn.s.dcache.inv
    amdgcn_s_dcache_inv_vol,                      // llvm.amdgcn.s.dcache.inv.vol
    amdgcn_s_dcache_wb,                           // llvm.amdgcn.s.dcache.wb
    amdgcn_s_dcache_wb_vol,                       // llvm.amdgcn.s.dcache.wb.vol
    amdgcn_s_decperflevel,                        // llvm.amdgcn.s.decperflevel
    amdgcn_s_getpc,                               // llvm.amdgcn.s.getpc
    amdgcn_s_getreg,                              // llvm.amdgcn.s.getreg
    amdgcn_s_incperflevel,                        // llvm.amdgcn.s.incperflevel
    amdgcn_s_memrealtime,                         // llvm.amdgcn.s.memrealtime
    amdgcn_s_memtime,                             // llvm.amdgcn.s.memtime
    amdgcn_s_sendmsg,                             // llvm.amdgcn.s.sendmsg
    amdgcn_s_sendmsghalt,                         // llvm.amdgcn.s.sendmsghalt
    amdgcn_s_sleep,                               // llvm.amdgcn.s.sleep
    amdgcn_s_waitcnt,                             // llvm.amdgcn.s.waitcnt
    amdgcn_sad_hi_u8,                             // llvm.amdgcn.sad.hi.u8
    amdgcn_sad_u16,                               // llvm.amdgcn.sad.u16
    amdgcn_sad_u8,                                // llvm.amdgcn.sad.u8
    amdgcn_sbfe,                                  // llvm.amdgcn.sbfe
    amdgcn_set_inactive,                          // llvm.amdgcn.set.inactive
    amdgcn_sffbh,                                 // llvm.amdgcn.sffbh
    amdgcn_sin,                                   // llvm.amdgcn.sin
    amdgcn_tbuffer_load,                          // llvm.amdgcn.tbuffer.load
    amdgcn_tbuffer_store,                         // llvm.amdgcn.tbuffer.store
    amdgcn_trig_preop,                            // llvm.amdgcn.trig.preop
    amdgcn_ubfe,                                  // llvm.amdgcn.ubfe
    amdgcn_unreachable,                           // llvm.amdgcn.unreachable
    amdgcn_update_dpp,                            // llvm.amdgcn.update.dpp
    amdgcn_wave_barrier,                          // llvm.amdgcn.wave.barrier
    amdgcn_workgroup_id_x,                        // llvm.amdgcn.workgroup.id.x
    amdgcn_workgroup_id_y,                        // llvm.amdgcn.workgroup.id.y
    amdgcn_workgroup_id_z,                        // llvm.amdgcn.workgroup.id.z
    amdgcn_workitem_id_x,                         // llvm.amdgcn.workitem.id.x
    amdgcn_workitem_id_y,                         // llvm.amdgcn.workitem.id.y
    amdgcn_workitem_id_z,                         // llvm.amdgcn.workitem.id.z
    amdgcn_wqm,                                   // llvm.amdgcn.wqm
    amdgcn_wqm_vote,                              // llvm.amdgcn.wqm.vote
    amdgcn_wwm,                                   // llvm.amdgcn.wwm
    arm_cdp,                                      // llvm.arm.cdp
    arm_cdp2,                                     // llvm.arm.cdp2
    arm_clrex,                                    // llvm.arm.clrex
    arm_crc32b,                                   // llvm.arm.crc32b
    arm_crc32cb,                                  // llvm.arm.crc32cb
    arm_crc32ch,                                  // llvm.arm.crc32ch
    arm_crc32cw,                                  // llvm.arm.crc32cw
    arm_crc32h,                                   // llvm.arm.crc32h
    arm_crc32w,                                   // llvm.arm.crc32w
    arm_dbg,                                      // llvm.arm.dbg
    arm_dmb,                                      // llvm.arm.dmb
    arm_dsb,                                      // llvm.arm.dsb
    arm_get_fpscr,                                // llvm.arm.get.fpscr
    arm_hint,                                     // llvm.arm.hint
    arm_isb,                                      // llvm.arm.isb
    arm_ldaex,                                    // llvm.arm.ldaex
    arm_ldaexd,                                   // llvm.arm.ldaexd
    arm_ldc,                                      // llvm.arm.ldc
    arm_ldc2,                                     // llvm.arm.ldc2
    arm_ldc2l,                                    // llvm.arm.ldc2l
    arm_ldcl,                                     // llvm.arm.ldcl
    arm_ldrex,                                    // llvm.arm.ldrex
    arm_ldrexd,                                   // llvm.arm.ldrexd
    arm_mcr,                                      // llvm.arm.mcr
    arm_mcr2,                                     // llvm.arm.mcr2
    arm_mcrr,                                     // llvm.arm.mcrr
    arm_mcrr2,                                    // llvm.arm.mcrr2
    arm_mrc,                                      // llvm.arm.mrc
    arm_mrc2,                                     // llvm.arm.mrc2
    arm_mrrc,                                     // llvm.arm.mrrc
    arm_mrrc2,                                    // llvm.arm.mrrc2
    arm_neon_aesd,                                // llvm.arm.neon.aesd
    arm_neon_aese,                                // llvm.arm.neon.aese
    arm_neon_aesimc,                              // llvm.arm.neon.aesimc
    arm_neon_aesmc,                               // llvm.arm.neon.aesmc
    arm_neon_sha1c,                               // llvm.arm.neon.sha1c
    arm_neon_sha1h,                               // llvm.arm.neon.sha1h
    arm_neon_sha1m,                               // llvm.arm.neon.sha1m
    arm_neon_sha1p,                               // llvm.arm.neon.sha1p
    arm_neon_sha1su0,                             // llvm.arm.neon.sha1su0
    arm_neon_sha1su1,                             // llvm.arm.neon.sha1su1
    arm_neon_sha256h,                             // llvm.arm.neon.sha256h
    arm_neon_sha256h2,                            // llvm.arm.neon.sha256h2
    arm_neon_sha256su0,                           // llvm.arm.neon.sha256su0
    arm_neon_sha256su1,                           // llvm.arm.neon.sha256su1
    arm_neon_vabds,                               // llvm.arm.neon.vabds
    arm_neon_vabdu,                               // llvm.arm.neon.vabdu
    arm_neon_vabs,                                // llvm.arm.neon.vabs
    arm_neon_vacge,                               // llvm.arm.neon.vacge
    arm_neon_vacgt,                               // llvm.arm.neon.vacgt
    arm_neon_vbsl,                                // llvm.arm.neon.vbsl
    arm_neon_vcls,                                // llvm.arm.neon.vcls
    arm_neon_vcvtas,                              // llvm.arm.neon.vcvtas
    arm_neon_vcvtau,                              // llvm.arm.neon.vcvtau
    arm_neon_vcvtfp2fxs,                          // llvm.arm.neon.vcvtfp2fxs
    arm_neon_vcvtfp2fxu,                          // llvm.arm.neon.vcvtfp2fxu
    arm_neon_vcvtfp2hf,                           // llvm.arm.neon.vcvtfp2hf
    arm_neon_vcvtfxs2fp,                          // llvm.arm.neon.vcvtfxs2fp
    arm_neon_vcvtfxu2fp,                          // llvm.arm.neon.vcvtfxu2fp
    arm_neon_vcvthf2fp,                           // llvm.arm.neon.vcvthf2fp
    arm_neon_vcvtms,                              // llvm.arm.neon.vcvtms
    arm_neon_vcvtmu,                              // llvm.arm.neon.vcvtmu
    arm_neon_vcvtns,                              // llvm.arm.neon.vcvtns
    arm_neon_vcvtnu,                              // llvm.arm.neon.vcvtnu
    arm_neon_vcvtps,                              // llvm.arm.neon.vcvtps
    arm_neon_vcvtpu,                              // llvm.arm.neon.vcvtpu
    arm_neon_vhadds,                              // llvm.arm.neon.vhadds
    arm_neon_vhaddu,                              // llvm.arm.neon.vhaddu
    arm_neon_vhsubs,                              // llvm.arm.neon.vhsubs
    arm_neon_vhsubu,                              // llvm.arm.neon.vhsubu
    arm_neon_vld1,                                // llvm.arm.neon.vld1
    arm_neon_vld2,                                // llvm.arm.neon.vld2
    arm_neon_vld2lane,                            // llvm.arm.neon.vld2lane
    arm_neon_vld3,                                // llvm.arm.neon.vld3
    arm_neon_vld3lane,                            // llvm.arm.neon.vld3lane
    arm_neon_vld4,                                // llvm.arm.neon.vld4
    arm_neon_vld4lane,                            // llvm.arm.neon.vld4lane
    arm_neon_vmaxnm,                              // llvm.arm.neon.vmaxnm
    arm_neon_vmaxs,                               // llvm.arm.neon.vmaxs
    arm_neon_vmaxu,                               // llvm.arm.neon.vmaxu
    arm_neon_vminnm,                              // llvm.arm.neon.vminnm
    arm_neon_vmins,                               // llvm.arm.neon.vmins
    arm_neon_vminu,                               // llvm.arm.neon.vminu
    arm_neon_vmullp,                              // llvm.arm.neon.vmullp
    arm_neon_vmulls,                              // llvm.arm.neon.vmulls
    arm_neon_vmullu,                              // llvm.arm.neon.vmullu
    arm_neon_vmulp,                               // llvm.arm.neon.vmulp
    arm_neon_vpadals,                             // llvm.arm.neon.vpadals
    arm_neon_vpadalu,                             // llvm.arm.neon.vpadalu
    arm_neon_vpadd,                               // llvm.arm.neon.vpadd
    arm_neon_vpaddls,                             // llvm.arm.neon.vpaddls
    arm_neon_vpaddlu,                             // llvm.arm.neon.vpaddlu
    arm_neon_vpmaxs,                              // llvm.arm.neon.vpmaxs
    arm_neon_vpmaxu,                              // llvm.arm.neon.vpmaxu
    arm_neon_vpmins,                              // llvm.arm.neon.vpmins
    arm_neon_vpminu,                              // llvm.arm.neon.vpminu
    arm_neon_vqabs,                               // llvm.arm.neon.vqabs
    arm_neon_vqadds,                              // llvm.arm.neon.vqadds
    arm_neon_vqaddu,                              // llvm.arm.neon.vqaddu
    arm_neon_vqdmulh,                             // llvm.arm.neon.vqdmulh
    arm_neon_vqdmull,                             // llvm.arm.neon.vqdmull
    arm_neon_vqmovns,                             // llvm.arm.neon.vqmovns
    arm_neon_vqmovnsu,                            // llvm.arm.neon.vqmovnsu
    arm_neon_vqmovnu,                             // llvm.arm.neon.vqmovnu
    arm_neon_vqneg,                               // llvm.arm.neon.vqneg
    arm_neon_vqrdmulh,                            // llvm.arm.neon.vqrdmulh
    arm_neon_vqrshiftns,                          // llvm.arm.neon.vqrshiftns
    arm_neon_vqrshiftnsu,                         // llvm.arm.neon.vqrshiftnsu
    arm_neon_vqrshiftnu,                          // llvm.arm.neon.vqrshiftnu
    arm_neon_vqrshifts,                           // llvm.arm.neon.vqrshifts
    arm_neon_vqrshiftu,                           // llvm.arm.neon.vqrshiftu
    arm_neon_vqshiftns,                           // llvm.arm.neon.vqshiftns
    arm_neon_vqshiftnsu,                          // llvm.arm.neon.vqshiftnsu
    arm_neon_vqshiftnu,                           // llvm.arm.neon.vqshiftnu
    arm_neon_vqshifts,                            // llvm.arm.neon.vqshifts
    arm_neon_vqshiftsu,                           // llvm.arm.neon.vqshiftsu
    arm_neon_vqshiftu,                            // llvm.arm.neon.vqshiftu
    arm_neon_vqsubs,                              // llvm.arm.neon.vqsubs
    arm_neon_vqsubu,                              // llvm.arm.neon.vqsubu
    arm_neon_vraddhn,                             // llvm.arm.neon.vraddhn
    arm_neon_vrecpe,                              // llvm.arm.neon.vrecpe
    arm_neon_vrecps,                              // llvm.arm.neon.vrecps
    arm_neon_vrhadds,                             // llvm.arm.neon.vrhadds
    arm_neon_vrhaddu,                             // llvm.arm.neon.vrhaddu
    arm_neon_vrinta,                              // llvm.arm.neon.vrinta
    arm_neon_vrintm,                              // llvm.arm.neon.vrintm
    arm_neon_vrintn,                              // llvm.arm.neon.vrintn
    arm_neon_vrintp,                              // llvm.arm.neon.vrintp
    arm_neon_vrintx,                              // llvm.arm.neon.vrintx
    arm_neon_vrintz,                              // llvm.arm.neon.vrintz
    arm_neon_vrshiftn,                            // llvm.arm.neon.vrshiftn
    arm_neon_vrshifts,                            // llvm.arm.neon.vrshifts
    arm_neon_vrshiftu,                            // llvm.arm.neon.vrshiftu
    arm_neon_vrsqrte,                             // llvm.arm.neon.vrsqrte
    arm_neon_vrsqrts,                             // llvm.arm.neon.vrsqrts
    arm_neon_vrsubhn,                             // llvm.arm.neon.vrsubhn
    arm_neon_vshiftins,                           // llvm.arm.neon.vshiftins
    arm_neon_vshifts,                             // llvm.arm.neon.vshifts
    arm_neon_vshiftu,                             // llvm.arm.neon.vshiftu
    arm_neon_vst1,                                // llvm.arm.neon.vst1
    arm_neon_vst2,                                // llvm.arm.neon.vst2
    arm_neon_vst2lane,                            // llvm.arm.neon.vst2lane
    arm_neon_vst3,                                // llvm.arm.neon.vst3
    arm_neon_vst3lane,                            // llvm.arm.neon.vst3lane
    arm_neon_vst4,                                // llvm.arm.neon.vst4
    arm_neon_vst4lane,                            // llvm.arm.neon.vst4lane
    arm_neon_vtbl1,                               // llvm.arm.neon.vtbl1
    arm_neon_vtbl2,                               // llvm.arm.neon.vtbl2
    arm_neon_vtbl3,                               // llvm.arm.neon.vtbl3
    arm_neon_vtbl4,                               // llvm.arm.neon.vtbl4
    arm_neon_vtbx1,                               // llvm.arm.neon.vtbx1
    arm_neon_vtbx2,                               // llvm.arm.neon.vtbx2
    arm_neon_vtbx3,                               // llvm.arm.neon.vtbx3
    arm_neon_vtbx4,                               // llvm.arm.neon.vtbx4
    arm_qadd,                                     // llvm.arm.qadd
    arm_qadd16,                                   // llvm.arm.qadd16
    arm_qadd8,                                    // llvm.arm.qadd8
    arm_qasx,                                     // llvm.arm.qasx
    arm_qsax,                                     // llvm.arm.qsax
    arm_qsub,                                     // llvm.arm.qsub
    arm_qsub16,                                   // llvm.arm.qsub16
    arm_qsub8,                                    // llvm.arm.qsub8
    arm_sadd16,                                   // llvm.arm.sadd16
    arm_sadd8,                                    // llvm.arm.sadd8
    arm_sasx,                                     // llvm.arm.sasx
    arm_sel,                                      // llvm.arm.sel
    arm_set_fpscr,                                // llvm.arm.set.fpscr
    arm_shadd16,                                  // llvm.arm.shadd16
    arm_shadd8,                                   // llvm.arm.shadd8
    arm_shasx,                                    // llvm.arm.shasx
    arm_shsax,                                    // llvm.arm.shsax
    arm_shsub16,                                  // llvm.arm.shsub16
    arm_shsub8,                                   // llvm.arm.shsub8
    arm_smlabb,                                   // llvm.arm.smlabb
    arm_smlabt,                                   // llvm.arm.smlabt
    arm_smlad,                                    // llvm.arm.smlad
    arm_smladx,                                   // llvm.arm.smladx
    arm_smlald,                                   // llvm.arm.smlald
    arm_smlaldx,                                  // llvm.arm.smlaldx
    arm_smlatb,                                   // llvm.arm.smlatb
    arm_smlatt,                                   // llvm.arm.smlatt
    arm_smlawb,                                   // llvm.arm.smlawb
    arm_smlawt,                                   // llvm.arm.smlawt
    arm_smlsd,                                    // llvm.arm.smlsd
    arm_smlsdx,                                   // llvm.arm.smlsdx
    arm_smlsld,                                   // llvm.arm.smlsld
    arm_smlsldx,                                  // llvm.arm.smlsldx
    arm_smuad,                                    // llvm.arm.smuad
    arm_smuadx,                                   // llvm.arm.smuadx
    arm_smulbb,                                   // llvm.arm.smulbb
    arm_smulbt,                                   // llvm.arm.smulbt
    arm_smultb,                                   // llvm.arm.smultb
    arm_smultt,                                   // llvm.arm.smultt
    arm_smulwb,                                   // llvm.arm.smulwb
    arm_smulwt,                                   // llvm.arm.smulwt
    arm_smusd,                                    // llvm.arm.smusd
    arm_smusdx,                                   // llvm.arm.smusdx
    arm_space,                                    // llvm.arm.space
    arm_ssat,                                     // llvm.arm.ssat
    arm_ssat16,                                   // llvm.arm.ssat16
    arm_ssax,                                     // llvm.arm.ssax
    arm_ssub16,                                   // llvm.arm.ssub16
    arm_ssub8,                                    // llvm.arm.ssub8
    arm_stc,                                      // llvm.arm.stc
    arm_stc2,                                     // llvm.arm.stc2
    arm_stc2l,                                    // llvm.arm.stc2l
    arm_stcl,                                     // llvm.arm.stcl
    arm_stlex,                                    // llvm.arm.stlex
    arm_stlexd,                                   // llvm.arm.stlexd
    arm_strex,                                    // llvm.arm.strex
    arm_strexd,                                   // llvm.arm.strexd
    arm_sxtab16,                                  // llvm.arm.sxtab16
    arm_sxtb16,                                   // llvm.arm.sxtb16
    arm_uadd16,                                   // llvm.arm.uadd16
    arm_uadd8,                                    // llvm.arm.uadd8
    arm_uasx,                                     // llvm.arm.uasx
    arm_uhadd16,                                  // llvm.arm.uhadd16
    arm_uhadd8,                                   // llvm.arm.uhadd8
    arm_uhasx,                                    // llvm.arm.uhasx
    arm_uhsax,                                    // llvm.arm.uhsax
    arm_uhsub16,                                  // llvm.arm.uhsub16
    arm_uhsub8,                                   // llvm.arm.uhsub8
    arm_undefined,                                // llvm.arm.undefined
    arm_uqadd16,                                  // llvm.arm.uqadd16
    arm_uqadd8,                                   // llvm.arm.uqadd8
    arm_uqasx,                                    // llvm.arm.uqasx
    arm_uqsax,                                    // llvm.arm.uqsax
    arm_uqsub16,                                  // llvm.arm.uqsub16
    arm_uqsub8,                                   // llvm.arm.uqsub8
    arm_usad8,                                    // llvm.arm.usad8
    arm_usada8,                                   // llvm.arm.usada8
    arm_usat,                                     // llvm.arm.usat
    arm_usat16,                                   // llvm.arm.usat16
    arm_usax,                                     // llvm.arm.usax
    arm_usub16,                                   // llvm.arm.usub16
    arm_usub8,                                    // llvm.arm.usub8
    arm_uxtab16,                                  // llvm.arm.uxtab16
    arm_uxtb16,                                   // llvm.arm.uxtb16
    arm_vcvtr,                                    // llvm.arm.vcvtr
    arm_vcvtru,                                   // llvm.arm.vcvtru
    bpf_load_byte,                                // llvm.bpf.load.byte
    bpf_load_half,                                // llvm.bpf.load.half
    bpf_load_word,                                // llvm.bpf.load.word
    bpf_pseudo,                                   // llvm.bpf.pseudo
    hexagon_A2_abs,                               // llvm.hexagon.A2.abs
    hexagon_A2_absp,                              // llvm.hexagon.A2.absp
    hexagon_A2_abssat,                            // llvm.hexagon.A2.abssat
    hexagon_A2_add,                               // llvm.hexagon.A2.add
    hexagon_A2_addh_h16_hh,                       // llvm.hexagon.A2.addh.h16.hh
    hexagon_A2_addh_h16_hl,                       // llvm.hexagon.A2.addh.h16.hl
    hexagon_A2_addh_h16_lh,                       // llvm.hexagon.A2.addh.h16.lh
    hexagon_A2_addh_h16_ll,                       // llvm.hexagon.A2.addh.h16.ll
    hexagon_A2_addh_h16_sat_hh,                   // llvm.hexagon.A2.addh.h16.sat.hh
    hexagon_A2_addh_h16_sat_hl,                   // llvm.hexagon.A2.addh.h16.sat.hl
    hexagon_A2_addh_h16_sat_lh,                   // llvm.hexagon.A2.addh.h16.sat.lh
    hexagon_A2_addh_h16_sat_ll,                   // llvm.hexagon.A2.addh.h16.sat.ll
    hexagon_A2_addh_l16_hl,                       // llvm.hexagon.A2.addh.l16.hl
    hexagon_A2_addh_l16_ll,                       // llvm.hexagon.A2.addh.l16.ll
    hexagon_A2_addh_l16_sat_hl,                   // llvm.hexagon.A2.addh.l16.sat.hl
    hexagon_A2_addh_l16_sat_ll,                   // llvm.hexagon.A2.addh.l16.sat.ll
    hexagon_A2_addi,                              // llvm.hexagon.A2.addi
    hexagon_A2_addp,                              // llvm.hexagon.A2.addp
    hexagon_A2_addpsat,                           // llvm.hexagon.A2.addpsat
    hexagon_A2_addsat,                            // llvm.hexagon.A2.addsat
    hexagon_A2_addsp,                             // llvm.hexagon.A2.addsp
    hexagon_A2_and,                               // llvm.hexagon.A2.and
    hexagon_A2_andir,                             // llvm.hexagon.A2.andir
    hexagon_A2_andp,                              // llvm.hexagon.A2.andp
    hexagon_A2_aslh,                              // llvm.hexagon.A2.aslh
    hexagon_A2_asrh,                              // llvm.hexagon.A2.asrh
    hexagon_A2_combine_hh,                        // llvm.hexagon.A2.combine.hh
    hexagon_A2_combine_hl,                        // llvm.hexagon.A2.combine.hl
    hexagon_A2_combine_lh,                        // llvm.hexagon.A2.combine.lh
    hexagon_A2_combine_ll,                        // llvm.hexagon.A2.combine.ll
    hexagon_A2_combineii,                         // llvm.hexagon.A2.combineii
    hexagon_A2_combinew,                          // llvm.hexagon.A2.combinew
    hexagon_A2_max,                               // llvm.hexagon.A2.max
    hexagon_A2_maxp,                              // llvm.hexagon.A2.maxp
    hexagon_A2_maxu,                              // llvm.hexagon.A2.maxu
    hexagon_A2_maxup,                             // llvm.hexagon.A2.maxup
    hexagon_A2_min,                               // llvm.hexagon.A2.min
    hexagon_A2_minp,                              // llvm.hexagon.A2.minp
    hexagon_A2_minu,                              // llvm.hexagon.A2.minu
    hexagon_A2_minup,                             // llvm.hexagon.A2.minup
    hexagon_A2_neg,                               // llvm.hexagon.A2.neg
    hexagon_A2_negp,                              // llvm.hexagon.A2.negp
    hexagon_A2_negsat,                            // llvm.hexagon.A2.negsat
    hexagon_A2_not,                               // llvm.hexagon.A2.not
    hexagon_A2_notp,                              // llvm.hexagon.A2.notp
    hexagon_A2_or,                                // llvm.hexagon.A2.or
    hexagon_A2_orir,                              // llvm.hexagon.A2.orir
    hexagon_A2_orp,                               // llvm.hexagon.A2.orp
    hexagon_A2_roundsat,                          // llvm.hexagon.A2.roundsat
    hexagon_A2_sat,                               // llvm.hexagon.A2.sat
    hexagon_A2_satb,                              // llvm.hexagon.A2.satb
    hexagon_A2_sath,                              // llvm.hexagon.A2.sath
    hexagon_A2_satub,                             // llvm.hexagon.A2.satub
    hexagon_A2_satuh,                             // llvm.hexagon.A2.satuh
    hexagon_A2_sub,                               // llvm.hexagon.A2.sub
    hexagon_A2_subh_h16_hh,                       // llvm.hexagon.A2.subh.h16.hh
    hexagon_A2_subh_h16_hl,                       // llvm.hexagon.A2.subh.h16.hl
    hexagon_A2_subh_h16_lh,                       // llvm.hexagon.A2.subh.h16.lh
    hexagon_A2_subh_h16_ll,                       // llvm.hexagon.A2.subh.h16.ll
    hexagon_A2_subh_h16_sat_hh,                   // llvm.hexagon.A2.subh.h16.sat.hh
    hexagon_A2_subh_h16_sat_hl,                   // llvm.hexagon.A2.subh.h16.sat.hl
    hexagon_A2_subh_h16_sat_lh,                   // llvm.hexagon.A2.subh.h16.sat.lh
    hexagon_A2_subh_h16_sat_ll,                   // llvm.hexagon.A2.subh.h16.sat.ll
    hexagon_A2_subh_l16_hl,                       // llvm.hexagon.A2.subh.l16.hl
    hexagon_A2_subh_l16_ll,                       // llvm.hexagon.A2.subh.l16.ll
    hexagon_A2_subh_l16_sat_hl,                   // llvm.hexagon.A2.subh.l16.sat.hl
    hexagon_A2_subh_l16_sat_ll,                   // llvm.hexagon.A2.subh.l16.sat.ll
    hexagon_A2_subp,                              // llvm.hexagon.A2.subp
    hexagon_A2_subri,                             // llvm.hexagon.A2.subri
    hexagon_A2_subsat,                            // llvm.hexagon.A2.subsat
    hexagon_A2_svaddh,                            // llvm.hexagon.A2.svaddh
    hexagon_A2_svaddhs,                           // llvm.hexagon.A2.svaddhs
    hexagon_A2_svadduhs,                          // llvm.hexagon.A2.svadduhs
    hexagon_A2_svavgh,                            // llvm.hexagon.A2.svavgh
    hexagon_A2_svavghs,                           // llvm.hexagon.A2.svavghs
    hexagon_A2_svnavgh,                           // llvm.hexagon.A2.svnavgh
    hexagon_A2_svsubh,                            // llvm.hexagon.A2.svsubh
    hexagon_A2_svsubhs,                           // llvm.hexagon.A2.svsubhs
    hexagon_A2_svsubuhs,                          // llvm.hexagon.A2.svsubuhs
    hexagon_A2_swiz,                              // llvm.hexagon.A2.swiz
    hexagon_A2_sxtb,                              // llvm.hexagon.A2.sxtb
    hexagon_A2_sxth,                              // llvm.hexagon.A2.sxth
    hexagon_A2_sxtw,                              // llvm.hexagon.A2.sxtw
    hexagon_A2_tfr,                               // llvm.hexagon.A2.tfr
    hexagon_A2_tfrih,                             // llvm.hexagon.A2.tfrih
    hexagon_A2_tfril,                             // llvm.hexagon.A2.tfril
    hexagon_A2_tfrp,                              // llvm.hexagon.A2.tfrp
    hexagon_A2_tfrpi,                             // llvm.hexagon.A2.tfrpi
    hexagon_A2_tfrsi,                             // llvm.hexagon.A2.tfrsi
    hexagon_A2_vabsh,                             // llvm.hexagon.A2.vabsh
    hexagon_A2_vabshsat,                          // llvm.hexagon.A2.vabshsat
    hexagon_A2_vabsw,                             // llvm.hexagon.A2.vabsw
    hexagon_A2_vabswsat,                          // llvm.hexagon.A2.vabswsat
    hexagon_A2_vaddb_map,                         // llvm.hexagon.A2.vaddb.map
    hexagon_A2_vaddh,                             // llvm.hexagon.A2.vaddh
    hexagon_A2_vaddhs,                            // llvm.hexagon.A2.vaddhs
    hexagon_A2_vaddub,                            // llvm.hexagon.A2.vaddub
    hexagon_A2_vaddubs,                           // llvm.hexagon.A2.vaddubs
    hexagon_A2_vadduhs,                           // llvm.hexagon.A2.vadduhs
    hexagon_A2_vaddw,                             // llvm.hexagon.A2.vaddw
    hexagon_A2_vaddws,                            // llvm.hexagon.A2.vaddws
    hexagon_A2_vavgh,                             // llvm.hexagon.A2.vavgh
    hexagon_A2_vavghcr,                           // llvm.hexagon.A2.vavghcr
    hexagon_A2_vavghr,                            // llvm.hexagon.A2.vavghr
    hexagon_A2_vavgub,                            // llvm.hexagon.A2.vavgub
    hexagon_A2_vavgubr,                           // llvm.hexagon.A2.vavgubr
    hexagon_A2_vavguh,                            // llvm.hexagon.A2.vavguh
    hexagon_A2_vavguhr,                           // llvm.hexagon.A2.vavguhr
    hexagon_A2_vavguw,                            // llvm.hexagon.A2.vavguw
    hexagon_A2_vavguwr,                           // llvm.hexagon.A2.vavguwr
    hexagon_A2_vavgw,                             // llvm.hexagon.A2.vavgw
    hexagon_A2_vavgwcr,                           // llvm.hexagon.A2.vavgwcr
    hexagon_A2_vavgwr,                            // llvm.hexagon.A2.vavgwr
    hexagon_A2_vcmpbeq,                           // llvm.hexagon.A2.vcmpbeq
    hexagon_A2_vcmpbgtu,                          // llvm.hexagon.A2.vcmpbgtu
    hexagon_A2_vcmpheq,                           // llvm.hexagon.A2.vcmpheq
    hexagon_A2_vcmphgt,                           // llvm.hexagon.A2.vcmphgt
    hexagon_A2_vcmphgtu,                          // llvm.hexagon.A2.vcmphgtu
    hexagon_A2_vcmpweq,                           // llvm.hexagon.A2.vcmpweq
    hexagon_A2_vcmpwgt,                           // llvm.hexagon.A2.vcmpwgt
    hexagon_A2_vcmpwgtu,                          // llvm.hexagon.A2.vcmpwgtu
    hexagon_A2_vconj,                             // llvm.hexagon.A2.vconj
    hexagon_A2_vmaxb,                             // llvm.hexagon.A2.vmaxb
    hexagon_A2_vmaxh,                             // llvm.hexagon.A2.vmaxh
    hexagon_A2_vmaxub,                            // llvm.hexagon.A2.vmaxub
    hexagon_A2_vmaxuh,                            // llvm.hexagon.A2.vmaxuh
    hexagon_A2_vmaxuw,                            // llvm.hexagon.A2.vmaxuw
    hexagon_A2_vmaxw,                             // llvm.hexagon.A2.vmaxw
    hexagon_A2_vminb,                             // llvm.hexagon.A2.vminb
    hexagon_A2_vminh,                             // llvm.hexagon.A2.vminh
    hexagon_A2_vminub,                            // llvm.hexagon.A2.vminub
    hexagon_A2_vminuh,                            // llvm.hexagon.A2.vminuh
    hexagon_A2_vminuw,                            // llvm.hexagon.A2.vminuw
    hexagon_A2_vminw,                             // llvm.hexagon.A2.vminw
    hexagon_A2_vnavgh,                            // llvm.hexagon.A2.vnavgh
    hexagon_A2_vnavghcr,                          // llvm.hexagon.A2.vnavghcr
    hexagon_A2_vnavghr,                           // llvm.hexagon.A2.vnavghr
    hexagon_A2_vnavgw,                            // llvm.hexagon.A2.vnavgw
    hexagon_A2_vnavgwcr,                          // llvm.hexagon.A2.vnavgwcr
    hexagon_A2_vnavgwr,                           // llvm.hexagon.A2.vnavgwr
    hexagon_A2_vraddub,                           // llvm.hexagon.A2.vraddub
    hexagon_A2_vraddub_acc,                       // llvm.hexagon.A2.vraddub.acc
    hexagon_A2_vrsadub,                           // llvm.hexagon.A2.vrsadub
    hexagon_A2_vrsadub_acc,                       // llvm.hexagon.A2.vrsadub.acc
    hexagon_A2_vsubb_map,                         // llvm.hexagon.A2.vsubb.map
    hexagon_A2_vsubh,                             // llvm.hexagon.A2.vsubh
    hexagon_A2_vsubhs,                            // llvm.hexagon.A2.vsubhs
    hexagon_A2_vsubub,                            // llvm.hexagon.A2.vsubub
    hexagon_A2_vsububs,                           // llvm.hexagon.A2.vsububs
    hexagon_A2_vsubuhs,                           // llvm.hexagon.A2.vsubuhs
    hexagon_A2_vsubw,                             // llvm.hexagon.A2.vsubw
    hexagon_A2_vsubws,                            // llvm.hexagon.A2.vsubws
    hexagon_A2_xor,                               // llvm.hexagon.A2.xor
    hexagon_A2_xorp,                              // llvm.hexagon.A2.xorp
    hexagon_A2_zxtb,                              // llvm.hexagon.A2.zxtb
    hexagon_A2_zxth,                              // llvm.hexagon.A2.zxth
    hexagon_A4_andn,                              // llvm.hexagon.A4.andn
    hexagon_A4_andnp,                             // llvm.hexagon.A4.andnp
    hexagon_A4_bitsplit,                          // llvm.hexagon.A4.bitsplit
    hexagon_A4_bitspliti,                         // llvm.hexagon.A4.bitspliti
    hexagon_A4_boundscheck,                       // llvm.hexagon.A4.boundscheck
    hexagon_A4_cmpbeq,                            // llvm.hexagon.A4.cmpbeq
    hexagon_A4_cmpbeqi,                           // llvm.hexagon.A4.cmpbeqi
    hexagon_A4_cmpbgt,                            // llvm.hexagon.A4.cmpbgt
    hexagon_A4_cmpbgti,                           // llvm.hexagon.A4.cmpbgti
    hexagon_A4_cmpbgtu,                           // llvm.hexagon.A4.cmpbgtu
    hexagon_A4_cmpbgtui,                          // llvm.hexagon.A4.cmpbgtui
    hexagon_A4_cmpheq,                            // llvm.hexagon.A4.cmpheq
    hexagon_A4_cmpheqi,                           // llvm.hexagon.A4.cmpheqi
    hexagon_A4_cmphgt,                            // llvm.hexagon.A4.cmphgt
    hexagon_A4_cmphgti,                           // llvm.hexagon.A4.cmphgti
    hexagon_A4_cmphgtu,                           // llvm.hexagon.A4.cmphgtu
    hexagon_A4_cmphgtui,                          // llvm.hexagon.A4.cmphgtui
    hexagon_A4_combineir,                         // llvm.hexagon.A4.combineir
    hexagon_A4_combineri,                         // llvm.hexagon.A4.combineri
    hexagon_A4_cround_ri,                         // llvm.hexagon.A4.cround.ri
    hexagon_A4_cround_rr,                         // llvm.hexagon.A4.cround.rr
    hexagon_A4_modwrapu,                          // llvm.hexagon.A4.modwrapu
    hexagon_A4_orn,                               // llvm.hexagon.A4.orn
    hexagon_A4_ornp,                              // llvm.hexagon.A4.ornp
    hexagon_A4_rcmpeq,                            // llvm.hexagon.A4.rcmpeq
    hexagon_A4_rcmpeqi,                           // llvm.hexagon.A4.rcmpeqi
    hexagon_A4_rcmpneq,                           // llvm.hexagon.A4.rcmpneq
    hexagon_A4_rcmpneqi,                          // llvm.hexagon.A4.rcmpneqi
    hexagon_A4_round_ri,                          // llvm.hexagon.A4.round.ri
    hexagon_A4_round_ri_sat,                      // llvm.hexagon.A4.round.ri.sat
    hexagon_A4_round_rr,                          // llvm.hexagon.A4.round.rr
    hexagon_A4_round_rr_sat,                      // llvm.hexagon.A4.round.rr.sat
    hexagon_A4_tlbmatch,                          // llvm.hexagon.A4.tlbmatch
    hexagon_A4_vcmpbeq_any,                       // llvm.hexagon.A4.vcmpbeq.any
    hexagon_A4_vcmpbeqi,                          // llvm.hexagon.A4.vcmpbeqi
    hexagon_A4_vcmpbgt,                           // llvm.hexagon.A4.vcmpbgt
    hexagon_A4_vcmpbgti,                          // llvm.hexagon.A4.vcmpbgti
    hexagon_A4_vcmpbgtui,                         // llvm.hexagon.A4.vcmpbgtui
    hexagon_A4_vcmpheqi,                          // llvm.hexagon.A4.vcmpheqi
    hexagon_A4_vcmphgti,                          // llvm.hexagon.A4.vcmphgti
    hexagon_A4_vcmphgtui,                         // llvm.hexagon.A4.vcmphgtui
    hexagon_A4_vcmpweqi,                          // llvm.hexagon.A4.vcmpweqi
    hexagon_A4_vcmpwgti,                          // llvm.hexagon.A4.vcmpwgti
    hexagon_A4_vcmpwgtui,                         // llvm.hexagon.A4.vcmpwgtui
    hexagon_A4_vrmaxh,                            // llvm.hexagon.A4.vrmaxh
    hexagon_A4_vrmaxuh,                           // llvm.hexagon.A4.vrmaxuh
    hexagon_A4_vrmaxuw,                           // llvm.hexagon.A4.vrmaxuw
    hexagon_A4_vrmaxw,                            // llvm.hexagon.A4.vrmaxw
    hexagon_A4_vrminh,                            // llvm.hexagon.A4.vrminh
    hexagon_A4_vrminuh,                           // llvm.hexagon.A4.vrminuh
    hexagon_A4_vrminuw,                           // llvm.hexagon.A4.vrminuw
    hexagon_A4_vrminw,                            // llvm.hexagon.A4.vrminw
    hexagon_A5_vaddhubs,                          // llvm.hexagon.A5.vaddhubs
    hexagon_A6_vcmpbeq_notany,                    // llvm.hexagon.A6.vcmpbeq.notany
    hexagon_A6_vcmpbeq_notany_128B,               // llvm.hexagon.A6.vcmpbeq.notany.128B
    hexagon_C2_all8,                              // llvm.hexagon.C2.all8
    hexagon_C2_and,                               // llvm.hexagon.C2.and
    hexagon_C2_andn,                              // llvm.hexagon.C2.andn
    hexagon_C2_any8,                              // llvm.hexagon.C2.any8
    hexagon_C2_bitsclr,                           // llvm.hexagon.C2.bitsclr
    hexagon_C2_bitsclri,                          // llvm.hexagon.C2.bitsclri
    hexagon_C2_bitsset,                           // llvm.hexagon.C2.bitsset
    hexagon_C2_cmpeq,                             // llvm.hexagon.C2.cmpeq
    hexagon_C2_cmpeqi,                            // llvm.hexagon.C2.cmpeqi
    hexagon_C2_cmpeqp,                            // llvm.hexagon.C2.cmpeqp
    hexagon_C2_cmpgei,                            // llvm.hexagon.C2.cmpgei
    hexagon_C2_cmpgeui,                           // llvm.hexagon.C2.cmpgeui
    hexagon_C2_cmpgt,                             // llvm.hexagon.C2.cmpgt
    hexagon_C2_cmpgti,                            // llvm.hexagon.C2.cmpgti
    hexagon_C2_cmpgtp,                            // llvm.hexagon.C2.cmpgtp
    hexagon_C2_cmpgtu,                            // llvm.hexagon.C2.cmpgtu
    hexagon_C2_cmpgtui,                           // llvm.hexagon.C2.cmpgtui
    hexagon_C2_cmpgtup,                           // llvm.hexagon.C2.cmpgtup
    hexagon_C2_cmplt,                             // llvm.hexagon.C2.cmplt
    hexagon_C2_cmpltu,                            // llvm.hexagon.C2.cmpltu
    hexagon_C2_mask,                              // llvm.hexagon.C2.mask
    hexagon_C2_mux,                               // llvm.hexagon.C2.mux
    hexagon_C2_muxii,                             // llvm.hexagon.C2.muxii
    hexagon_C2_muxir,                             // llvm.hexagon.C2.muxir
    hexagon_C2_muxri,                             // llvm.hexagon.C2.muxri
    hexagon_C2_not,                               // llvm.hexagon.C2.not
    hexagon_C2_or,                                // llvm.hexagon.C2.or
    hexagon_C2_orn,                               // llvm.hexagon.C2.orn
    hexagon_C2_pxfer_map,                         // llvm.hexagon.C2.pxfer.map
    hexagon_C2_tfrpr,                             // llvm.hexagon.C2.tfrpr
    hexagon_C2_tfrrp,                             // llvm.hexagon.C2.tfrrp
    hexagon_C2_vitpack,                           // llvm.hexagon.C2.vitpack
    hexagon_C2_vmux,                              // llvm.hexagon.C2.vmux
    hexagon_C2_xor,                               // llvm.hexagon.C2.xor
    hexagon_C4_and_and,                           // llvm.hexagon.C4.and.and
    hexagon_C4_and_andn,                          // llvm.hexagon.C4.and.andn
    hexagon_C4_and_or,                            // llvm.hexagon.C4.and.or
    hexagon_C4_and_orn,                           // llvm.hexagon.C4.and.orn
    hexagon_C4_cmplte,                            // llvm.hexagon.C4.cmplte
    hexagon_C4_cmpltei,                           // llvm.hexagon.C4.cmpltei
    hexagon_C4_cmplteu,                           // llvm.hexagon.C4.cmplteu
    hexagon_C4_cmplteui,                          // llvm.hexagon.C4.cmplteui
    hexagon_C4_cmpneq,                            // llvm.hexagon.C4.cmpneq
    hexagon_C4_cmpneqi,                           // llvm.hexagon.C4.cmpneqi
    hexagon_C4_fastcorner9,                       // llvm.hexagon.C4.fastcorner9
    hexagon_C4_fastcorner9_not,                   // llvm.hexagon.C4.fastcorner9.not
    hexagon_C4_nbitsclr,                          // llvm.hexagon.C4.nbitsclr
    hexagon_C4_nbitsclri,                         // llvm.hexagon.C4.nbitsclri
    hexagon_C4_nbitsset,                          // llvm.hexagon.C4.nbitsset
    hexagon_C4_or_and,                            // llvm.hexagon.C4.or.and
    hexagon_C4_or_andn,                           // llvm.hexagon.C4.or.andn
    hexagon_C4_or_or,                             // llvm.hexagon.C4.or.or
    hexagon_C4_or_orn,                            // llvm.hexagon.C4.or.orn
    hexagon_F2_conv_d2df,                         // llvm.hexagon.F2.conv.d2df
    hexagon_F2_conv_d2sf,                         // llvm.hexagon.F2.conv.d2sf
    hexagon_F2_conv_df2d,                         // llvm.hexagon.F2.conv.df2d
    hexagon_F2_conv_df2d_chop,                    // llvm.hexagon.F2.conv.df2d.chop
    hexagon_F2_conv_df2sf,                        // llvm.hexagon.F2.conv.df2sf
    hexagon_F2_conv_df2ud,                        // llvm.hexagon.F2.conv.df2ud
    hexagon_F2_conv_df2ud_chop,                   // llvm.hexagon.F2.conv.df2ud.chop
    hexagon_F2_conv_df2uw,                        // llvm.hexagon.F2.conv.df2uw
    hexagon_F2_conv_df2uw_chop,                   // llvm.hexagon.F2.conv.df2uw.chop
    hexagon_F2_conv_df2w,                         // llvm.hexagon.F2.conv.df2w
    hexagon_F2_conv_df2w_chop,                    // llvm.hexagon.F2.conv.df2w.chop
    hexagon_F2_conv_sf2d,                         // llvm.hexagon.F2.conv.sf2d
    hexagon_F2_conv_sf2d_chop,                    // llvm.hexagon.F2.conv.sf2d.chop
    hexagon_F2_conv_sf2df,                        // llvm.hexagon.F2.conv.sf2df
    hexagon_F2_conv_sf2ud,                        // llvm.hexagon.F2.conv.sf2ud
    hexagon_F2_conv_sf2ud_chop,                   // llvm.hexagon.F2.conv.sf2ud.chop
    hexagon_F2_conv_sf2uw,                        // llvm.hexagon.F2.conv.sf2uw
    hexagon_F2_conv_sf2uw_chop,                   // llvm.hexagon.F2.conv.sf2uw.chop
    hexagon_F2_conv_sf2w,                         // llvm.hexagon.F2.conv.sf2w
    hexagon_F2_conv_sf2w_chop,                    // llvm.hexagon.F2.conv.sf2w.chop
    hexagon_F2_conv_ud2df,                        // llvm.hexagon.F2.conv.ud2df
    hexagon_F2_conv_ud2sf,                        // llvm.hexagon.F2.conv.ud2sf
    hexagon_F2_conv_uw2df,                        // llvm.hexagon.F2.conv.uw2df
    hexagon_F2_conv_uw2sf,                        // llvm.hexagon.F2.conv.uw2sf
    hexagon_F2_conv_w2df,                         // llvm.hexagon.F2.conv.w2df
    hexagon_F2_conv_w2sf,                         // llvm.hexagon.F2.conv.w2sf
    hexagon_F2_dfclass,                           // llvm.hexagon.F2.dfclass
    hexagon_F2_dfcmpeq,                           // llvm.hexagon.F2.dfcmpeq
    hexagon_F2_dfcmpge,                           // llvm.hexagon.F2.dfcmpge
    hexagon_F2_dfcmpgt,                           // llvm.hexagon.F2.dfcmpgt
    hexagon_F2_dfcmpuo,                           // llvm.hexagon.F2.dfcmpuo
    hexagon_F2_dfimm_n,                           // llvm.hexagon.F2.dfimm.n
    hexagon_F2_dfimm_p,                           // llvm.hexagon.F2.dfimm.p
    hexagon_F2_sfadd,                             // llvm.hexagon.F2.sfadd
    hexagon_F2_sfclass,                           // llvm.hexagon.F2.sfclass
    hexagon_F2_sfcmpeq,                           // llvm.hexagon.F2.sfcmpeq
    hexagon_F2_sfcmpge,                           // llvm.hexagon.F2.sfcmpge
    hexagon_F2_sfcmpgt,                           // llvm.hexagon.F2.sfcmpgt
    hexagon_F2_sfcmpuo,                           // llvm.hexagon.F2.sfcmpuo
    hexagon_F2_sffixupd,                          // llvm.hexagon.F2.sffixupd
    hexagon_F2_sffixupn,                          // llvm.hexagon.F2.sffixupn
    hexagon_F2_sffixupr,                          // llvm.hexagon.F2.sffixupr
    hexagon_F2_sffma,                             // llvm.hexagon.F2.sffma
    hexagon_F2_sffma_lib,                         // llvm.hexagon.F2.sffma.lib
    hexagon_F2_sffma_sc,                          // llvm.hexagon.F2.sffma.sc
    hexagon_F2_sffms,                             // llvm.hexagon.F2.sffms
    hexagon_F2_sffms_lib,                         // llvm.hexagon.F2.sffms.lib
    hexagon_F2_sfimm_n,                           // llvm.hexagon.F2.sfimm.n
    hexagon_F2_sfimm_p,                           // llvm.hexagon.F2.sfimm.p
    hexagon_F2_sfmax,                             // llvm.hexagon.F2.sfmax
    hexagon_F2_sfmin,                             // llvm.hexagon.F2.sfmin
    hexagon_F2_sfmpy,                             // llvm.hexagon.F2.sfmpy
    hexagon_F2_sfsub,                             // llvm.hexagon.F2.sfsub
    hexagon_L2_loadw_locked,                      // llvm.hexagon.L2.loadw.locked
    hexagon_L4_loadd_locked,                      // llvm.hexagon.L4.loadd.locked
    hexagon_M2_acci,                              // llvm.hexagon.M2.acci
    hexagon_M2_accii,                             // llvm.hexagon.M2.accii
    hexagon_M2_cmaci_s0,                          // llvm.hexagon.M2.cmaci.s0
    hexagon_M2_cmacr_s0,                          // llvm.hexagon.M2.cmacr.s0
    hexagon_M2_cmacs_s0,                          // llvm.hexagon.M2.cmacs.s0
    hexagon_M2_cmacs_s1,                          // llvm.hexagon.M2.cmacs.s1
    hexagon_M2_cmacsc_s0,                         // llvm.hexagon.M2.cmacsc.s0
    hexagon_M2_cmacsc_s1,                         // llvm.hexagon.M2.cmacsc.s1
    hexagon_M2_cmpyi_s0,                          // llvm.hexagon.M2.cmpyi.s0
    hexagon_M2_cmpyr_s0,                          // llvm.hexagon.M2.cmpyr.s0
    hexagon_M2_cmpyrs_s0,                         // llvm.hexagon.M2.cmpyrs.s0
    hexagon_M2_cmpyrs_s1,                         // llvm.hexagon.M2.cmpyrs.s1
    hexagon_M2_cmpyrsc_s0,                        // llvm.hexagon.M2.cmpyrsc.s0
    hexagon_M2_cmpyrsc_s1,                        // llvm.hexagon.M2.cmpyrsc.s1
    hexagon_M2_cmpys_s0,                          // llvm.hexagon.M2.cmpys.s0
    hexagon_M2_cmpys_s1,                          // llvm.hexagon.M2.cmpys.s1
    hexagon_M2_cmpysc_s0,                         // llvm.hexagon.M2.cmpysc.s0
    hexagon_M2_cmpysc_s1,                         // llvm.hexagon.M2.cmpysc.s1
    hexagon_M2_cnacs_s0,                          // llvm.hexagon.M2.cnacs.s0
    hexagon_M2_cnacs_s1,                          // llvm.hexagon.M2.cnacs.s1
    hexagon_M2_cnacsc_s0,                         // llvm.hexagon.M2.cnacsc.s0
    hexagon_M2_cnacsc_s1,                         // llvm.hexagon.M2.cnacsc.s1
    hexagon_M2_dpmpyss_acc_s0,                    // llvm.hexagon.M2.dpmpyss.acc.s0
    hexagon_M2_dpmpyss_nac_s0,                    // llvm.hexagon.M2.dpmpyss.nac.s0
    hexagon_M2_dpmpyss_rnd_s0,                    // llvm.hexagon.M2.dpmpyss.rnd.s0
    hexagon_M2_dpmpyss_s0,                        // llvm.hexagon.M2.dpmpyss.s0
    hexagon_M2_dpmpyuu_acc_s0,                    // llvm.hexagon.M2.dpmpyuu.acc.s0
    hexagon_M2_dpmpyuu_nac_s0,                    // llvm.hexagon.M2.dpmpyuu.nac.s0
    hexagon_M2_dpmpyuu_s0,                        // llvm.hexagon.M2.dpmpyuu.s0
    hexagon_M2_hmmpyh_rs1,                        // llvm.hexagon.M2.hmmpyh.rs1
    hexagon_M2_hmmpyh_s1,                         // llvm.hexagon.M2.hmmpyh.s1
    hexagon_M2_hmmpyl_rs1,                        // llvm.hexagon.M2.hmmpyl.rs1
    hexagon_M2_hmmpyl_s1,                         // llvm.hexagon.M2.hmmpyl.s1
    hexagon_M2_maci,                              // llvm.hexagon.M2.maci
    hexagon_M2_macsin,                            // llvm.hexagon.M2.macsin
    hexagon_M2_macsip,                            // llvm.hexagon.M2.macsip
    hexagon_M2_mmachs_rs0,                        // llvm.hexagon.M2.mmachs.rs0
    hexagon_M2_mmachs_rs1,                        // llvm.hexagon.M2.mmachs.rs1
    hexagon_M2_mmachs_s0,                         // llvm.hexagon.M2.mmachs.s0
    hexagon_M2_mmachs_s1,                         // llvm.hexagon.M2.mmachs.s1
    hexagon_M2_mmacls_rs0,                        // llvm.hexagon.M2.mmacls.rs0
    hexagon_M2_mmacls_rs1,                        // llvm.hexagon.M2.mmacls.rs1
    hexagon_M2_mmacls_s0,                         // llvm.hexagon.M2.mmacls.s0
    hexagon_M2_mmacls_s1,                         // llvm.hexagon.M2.mmacls.s1
    hexagon_M2_mmacuhs_rs0,                       // llvm.hexagon.M2.mmacuhs.rs0
    hexagon_M2_mmacuhs_rs1,                       // llvm.hexagon.M2.mmacuhs.rs1
    hexagon_M2_mmacuhs_s0,                        // llvm.hexagon.M2.mmacuhs.s0
    hexagon_M2_mmacuhs_s1,                        // llvm.hexagon.M2.mmacuhs.s1
    hexagon_M2_mmaculs_rs0,                       // llvm.hexagon.M2.mmaculs.rs0
    hexagon_M2_mmaculs_rs1,                       // llvm.hexagon.M2.mmaculs.rs1
    hexagon_M2_mmaculs_s0,                        // llvm.hexagon.M2.mmaculs.s0
    hexagon_M2_mmaculs_s1,                        // llvm.hexagon.M2.mmaculs.s1
    hexagon_M2_mmpyh_rs0,                         // llvm.hexagon.M2.mmpyh.rs0
    hexagon_M2_mmpyh_rs1,                         // llvm.hexagon.M2.mmpyh.rs1
    hexagon_M2_mmpyh_s0,                          // llvm.hexagon.M2.mmpyh.s0
    hexagon_M2_mmpyh_s1,                          // llvm.hexagon.M2.mmpyh.s1
    hexagon_M2_mmpyl_rs0,                         // llvm.hexagon.M2.mmpyl.rs0
    hexagon_M2_mmpyl_rs1,                         // llvm.hexagon.M2.mmpyl.rs1
    hexagon_M2_mmpyl_s0,                          // llvm.hexagon.M2.mmpyl.s0
    hexagon_M2_mmpyl_s1,                          // llvm.hexagon.M2.mmpyl.s1
    hexagon_M2_mmpyuh_rs0,                        // llvm.hexagon.M2.mmpyuh.rs0
    hexagon_M2_mmpyuh_rs1,                        // llvm.hexagon.M2.mmpyuh.rs1
    hexagon_M2_mmpyuh_s0,                         // llvm.hexagon.M2.mmpyuh.s0
    hexagon_M2_mmpyuh_s1,                         // llvm.hexagon.M2.mmpyuh.s1
    hexagon_M2_mmpyul_rs0,                        // llvm.hexagon.M2.mmpyul.rs0
    hexagon_M2_mmpyul_rs1,                        // llvm.hexagon.M2.mmpyul.rs1
    hexagon_M2_mmpyul_s0,                         // llvm.hexagon.M2.mmpyul.s0
    hexagon_M2_mmpyul_s1,                         // llvm.hexagon.M2.mmpyul.s1
    hexagon_M2_mpy_acc_hh_s0,                     // llvm.hexagon.M2.mpy.acc.hh.s0
    hexagon_M2_mpy_acc_hh_s1,                     // llvm.hexagon.M2.mpy.acc.hh.s1
    hexagon_M2_mpy_acc_hl_s0,                     // llvm.hexagon.M2.mpy.acc.hl.s0
    hexagon_M2_mpy_acc_hl_s1,                     // llvm.hexagon.M2.mpy.acc.hl.s1
    hexagon_M2_mpy_acc_lh_s0,                     // llvm.hexagon.M2.mpy.acc.lh.s0
    hexagon_M2_mpy_acc_lh_s1,                     // llvm.hexagon.M2.mpy.acc.lh.s1
    hexagon_M2_mpy_acc_ll_s0,                     // llvm.hexagon.M2.mpy.acc.ll.s0
    hexagon_M2_mpy_acc_ll_s1,                     // llvm.hexagon.M2.mpy.acc.ll.s1
    hexagon_M2_mpy_acc_sat_hh_s0,                 // llvm.hexagon.M2.mpy.acc.sat.hh.s0
    hexagon_M2_mpy_acc_sat_hh_s1,                 // llvm.hexagon.M2.mpy.acc.sat.hh.s1
    hexagon_M2_mpy_acc_sat_hl_s0,                 // llvm.hexagon.M2.mpy.acc.sat.hl.s0
    hexagon_M2_mpy_acc_sat_hl_s1,                 // llvm.hexagon.M2.mpy.acc.sat.hl.s1
    hexagon_M2_mpy_acc_sat_lh_s0,                 // llvm.hexagon.M2.mpy.acc.sat.lh.s0
    hexagon_M2_mpy_acc_sat_lh_s1,                 // llvm.hexagon.M2.mpy.acc.sat.lh.s1
    hexagon_M2_mpy_acc_sat_ll_s0,                 // llvm.hexagon.M2.mpy.acc.sat.ll.s0
    hexagon_M2_mpy_acc_sat_ll_s1,                 // llvm.hexagon.M2.mpy.acc.sat.ll.s1
    hexagon_M2_mpy_hh_s0,                         // llvm.hexagon.M2.mpy.hh.s0
    hexagon_M2_mpy_hh_s1,                         // llvm.hexagon.M2.mpy.hh.s1
    hexagon_M2_mpy_hl_s0,                         // llvm.hexagon.M2.mpy.hl.s0
    hexagon_M2_mpy_hl_s1,                         // llvm.hexagon.M2.mpy.hl.s1
    hexagon_M2_mpy_lh_s0,                         // llvm.hexagon.M2.mpy.lh.s0
    hexagon_M2_mpy_lh_s1,                         // llvm.hexagon.M2.mpy.lh.s1
    hexagon_M2_mpy_ll_s0,                         // llvm.hexagon.M2.mpy.ll.s0
    hexagon_M2_mpy_ll_s1,                         // llvm.hexagon.M2.mpy.ll.s1
    hexagon_M2_mpy_nac_hh_s0,                     // llvm.hexagon.M2.mpy.nac.hh.s0
    hexagon_M2_mpy_nac_hh_s1,                     // llvm.hexagon.M2.mpy.nac.hh.s1
    hexagon_M2_mpy_nac_hl_s0,                     // llvm.hexagon.M2.mpy.nac.hl.s0
    hexagon_M2_mpy_nac_hl_s1,                     // llvm.hexagon.M2.mpy.nac.hl.s1
    hexagon_M2_mpy_nac_lh_s0,                     // llvm.hexagon.M2.mpy.nac.lh.s0
    hexagon_M2_mpy_nac_lh_s1,                     // llvm.hexagon.M2.mpy.nac.lh.s1
    hexagon_M2_mpy_nac_ll_s0,                     // llvm.hexagon.M2.mpy.nac.ll.s0
    hexagon_M2_mpy_nac_ll_s1,                     // llvm.hexagon.M2.mpy.nac.ll.s1
    hexagon_M2_mpy_nac_sat_hh_s0,                 // llvm.hexagon.M2.mpy.nac.sat.hh.s0
    hexagon_M2_mpy_nac_sat_hh_s1,                 // llvm.hexagon.M2.mpy.nac.sat.hh.s1
    hexagon_M2_mpy_nac_sat_hl_s0,                 // llvm.hexagon.M2.mpy.nac.sat.hl.s0
    hexagon_M2_mpy_nac_sat_hl_s1,                 // llvm.hexagon.M2.mpy.nac.sat.hl.s1
    hexagon_M2_mpy_nac_sat_lh_s0,                 // llvm.hexagon.M2.mpy.nac.sat.lh.s0
    hexagon_M2_mpy_nac_sat_lh_s1,                 // llvm.hexagon.M2.mpy.nac.sat.lh.s1
    hexagon_M2_mpy_nac_sat_ll_s0,                 // llvm.hexagon.M2.mpy.nac.sat.ll.s0
    hexagon_M2_mpy_nac_sat_ll_s1,                 // llvm.hexagon.M2.mpy.nac.sat.ll.s1
    hexagon_M2_mpy_rnd_hh_s0,                     // llvm.hexagon.M2.mpy.rnd.hh.s0
    hexagon_M2_mpy_rnd_hh_s1,                     // llvm.hexagon.M2.mpy.rnd.hh.s1
    hexagon_M2_mpy_rnd_hl_s0,                     // llvm.hexagon.M2.mpy.rnd.hl.s0
    hexagon_M2_mpy_rnd_hl_s1,                     // llvm.hexagon.M2.mpy.rnd.hl.s1
    hexagon_M2_mpy_rnd_lh_s0,                     // llvm.hexagon.M2.mpy.rnd.lh.s0
    hexagon_M2_mpy_rnd_lh_s1,                     // llvm.hexagon.M2.mpy.rnd.lh.s1
    hexagon_M2_mpy_rnd_ll_s0,                     // llvm.hexagon.M2.mpy.rnd.ll.s0
    hexagon_M2_mpy_rnd_ll_s1,                     // llvm.hexagon.M2.mpy.rnd.ll.s1
    hexagon_M2_mpy_sat_hh_s0,                     // llvm.hexagon.M2.mpy.sat.hh.s0
    hexagon_M2_mpy_sat_hh_s1,                     // llvm.hexagon.M2.mpy.sat.hh.s1
    hexagon_M2_mpy_sat_hl_s0,                     // llvm.hexagon.M2.mpy.sat.hl.s0
    hexagon_M2_mpy_sat_hl_s1,                     // llvm.hexagon.M2.mpy.sat.hl.s1
    hexagon_M2_mpy_sat_lh_s0,                     // llvm.hexagon.M2.mpy.sat.lh.s0
    hexagon_M2_mpy_sat_lh_s1,                     // llvm.hexagon.M2.mpy.sat.lh.s1
    hexagon_M2_mpy_sat_ll_s0,                     // llvm.hexagon.M2.mpy.sat.ll.s0
    hexagon_M2_mpy_sat_ll_s1,                     // llvm.hexagon.M2.mpy.sat.ll.s1
    hexagon_M2_mpy_sat_rnd_hh_s0,                 // llvm.hexagon.M2.mpy.sat.rnd.hh.s0
    hexagon_M2_mpy_sat_rnd_hh_s1,                 // llvm.hexagon.M2.mpy.sat.rnd.hh.s1
    hexagon_M2_mpy_sat_rnd_hl_s0,                 // llvm.hexagon.M2.mpy.sat.rnd.hl.s0
    hexagon_M2_mpy_sat_rnd_hl_s1,                 // llvm.hexagon.M2.mpy.sat.rnd.hl.s1
    hexagon_M2_mpy_sat_rnd_lh_s0,                 // llvm.hexagon.M2.mpy.sat.rnd.lh.s0
    hexagon_M2_mpy_sat_rnd_lh_s1,                 // llvm.hexagon.M2.mpy.sat.rnd.lh.s1
    hexagon_M2_mpy_sat_rnd_ll_s0,                 // llvm.hexagon.M2.mpy.sat.rnd.ll.s0
    hexagon_M2_mpy_sat_rnd_ll_s1,                 // llvm.hexagon.M2.mpy.sat.rnd.ll.s1
    hexagon_M2_mpy_up,                            // llvm.hexagon.M2.mpy.up
    hexagon_M2_mpy_up_s1,                         // llvm.hexagon.M2.mpy.up.s1
    hexagon_M2_mpy_up_s1_sat,                     // llvm.hexagon.M2.mpy.up.s1.sat
    hexagon_M2_mpyd_acc_hh_s0,                    // llvm.hexagon.M2.mpyd.acc.hh.s0
    hexagon_M2_mpyd_acc_hh_s1,                    // llvm.hexagon.M2.mpyd.acc.hh.s1
    hexagon_M2_mpyd_acc_hl_s0,                    // llvm.hexagon.M2.mpyd.acc.hl.s0
    hexagon_M2_mpyd_acc_hl_s1,                    // llvm.hexagon.M2.mpyd.acc.hl.s1
    hexagon_M2_mpyd_acc_lh_s0,                    // llvm.hexagon.M2.mpyd.acc.lh.s0
    hexagon_M2_mpyd_acc_lh_s1,                    // llvm.hexagon.M2.mpyd.acc.lh.s1
    hexagon_M2_mpyd_acc_ll_s0,                    // llvm.hexagon.M2.mpyd.acc.ll.s0
    hexagon_M2_mpyd_acc_ll_s1,                    // llvm.hexagon.M2.mpyd.acc.ll.s1
    hexagon_M2_mpyd_hh_s0,                        // llvm.hexagon.M2.mpyd.hh.s0
    hexagon_M2_mpyd_hh_s1,                        // llvm.hexagon.M2.mpyd.hh.s1
    hexagon_M2_mpyd_hl_s0,                        // llvm.hexagon.M2.mpyd.hl.s0
    hexagon_M2_mpyd_hl_s1,                        // llvm.hexagon.M2.mpyd.hl.s1
    hexagon_M2_mpyd_lh_s0,                        // llvm.hexagon.M2.mpyd.lh.s0
    hexagon_M2_mpyd_lh_s1,                        // llvm.hexagon.M2.mpyd.lh.s1
    hexagon_M2_mpyd_ll_s0,                        // llvm.hexagon.M2.mpyd.ll.s0
    hexagon_M2_mpyd_ll_s1,                        // llvm.hexagon.M2.mpyd.ll.s1
    hexagon_M2_mpyd_nac_hh_s0,                    // llvm.hexagon.M2.mpyd.nac.hh.s0
    hexagon_M2_mpyd_nac_hh_s1,                    // llvm.hexagon.M2.mpyd.nac.hh.s1
    hexagon_M2_mpyd_nac_hl_s0,                    // llvm.hexagon.M2.mpyd.nac.hl.s0
    hexagon_M2_mpyd_nac_hl_s1,                    // llvm.hexagon.M2.mpyd.nac.hl.s1
    hexagon_M2_mpyd_nac_lh_s0,                    // llvm.hexagon.M2.mpyd.nac.lh.s0
    hexagon_M2_mpyd_nac_lh_s1,                    // llvm.hexagon.M2.mpyd.nac.lh.s1
    hexagon_M2_mpyd_nac_ll_s0,                    // llvm.hexagon.M2.mpyd.nac.ll.s0
    hexagon_M2_mpyd_nac_ll_s1,                    // llvm.hexagon.M2.mpyd.nac.ll.s1
    hexagon_M2_mpyd_rnd_hh_s0,                    // llvm.hexagon.M2.mpyd.rnd.hh.s0
    hexagon_M2_mpyd_rnd_hh_s1,                    // llvm.hexagon.M2.mpyd.rnd.hh.s1
    hexagon_M2_mpyd_rnd_hl_s0,                    // llvm.hexagon.M2.mpyd.rnd.hl.s0
    hexagon_M2_mpyd_rnd_hl_s1,                    // llvm.hexagon.M2.mpyd.rnd.hl.s1
    hexagon_M2_mpyd_rnd_lh_s0,                    // llvm.hexagon.M2.mpyd.rnd.lh.s0
    hexagon_M2_mpyd_rnd_lh_s1,                    // llvm.hexagon.M2.mpyd.rnd.lh.s1
    hexagon_M2_mpyd_rnd_ll_s0,                    // llvm.hexagon.M2.mpyd.rnd.ll.s0
    hexagon_M2_mpyd_rnd_ll_s1,                    // llvm.hexagon.M2.mpyd.rnd.ll.s1
    hexagon_M2_mpyi,                              // llvm.hexagon.M2.mpyi
    hexagon_M2_mpysmi,                            // llvm.hexagon.M2.mpysmi
    hexagon_M2_mpysu_up,                          // llvm.hexagon.M2.mpysu.up
    hexagon_M2_mpyu_acc_hh_s0,                    // llvm.hexagon.M2.mpyu.acc.hh.s0
    hexagon_M2_mpyu_acc_hh_s1,                    // llvm.hexagon.M2.mpyu.acc.hh.s1
    hexagon_M2_mpyu_acc_hl_s0,                    // llvm.hexagon.M2.mpyu.acc.hl.s0
    hexagon_M2_mpyu_acc_hl_s1,                    // llvm.hexagon.M2.mpyu.acc.hl.s1
    hexagon_M2_mpyu_acc_lh_s0,                    // llvm.hexagon.M2.mpyu.acc.lh.s0
    hexagon_M2_mpyu_acc_lh_s1,                    // llvm.hexagon.M2.mpyu.acc.lh.s1
    hexagon_M2_mpyu_acc_ll_s0,                    // llvm.hexagon.M2.mpyu.acc.ll.s0
    hexagon_M2_mpyu_acc_ll_s1,                    // llvm.hexagon.M2.mpyu.acc.ll.s1
    hexagon_M2_mpyu_hh_s0,                        // llvm.hexagon.M2.mpyu.hh.s0
    hexagon_M2_mpyu_hh_s1,                        // llvm.hexagon.M2.mpyu.hh.s1
    hexagon_M2_mpyu_hl_s0,                        // llvm.hexagon.M2.mpyu.hl.s0
    hexagon_M2_mpyu_hl_s1,                        // llvm.hexagon.M2.mpyu.hl.s1
    hexagon_M2_mpyu_lh_s0,                        // llvm.hexagon.M2.mpyu.lh.s0
    hexagon_M2_mpyu_lh_s1,                        // llvm.hexagon.M2.mpyu.lh.s1
    hexagon_M2_mpyu_ll_s0,                        // llvm.hexagon.M2.mpyu.ll.s0
    hexagon_M2_mpyu_ll_s1,                        // llvm.hexagon.M2.mpyu.ll.s1
    hexagon_M2_mpyu_nac_hh_s0,                    // llvm.hexagon.M2.mpyu.nac.hh.s0
    hexagon_M2_mpyu_nac_hh_s1,                    // llvm.hexagon.M2.mpyu.nac.hh.s1
    hexagon_M2_mpyu_nac_hl_s0,                    // llvm.hexagon.M2.mpyu.nac.hl.s0
    hexagon_M2_mpyu_nac_hl_s1,                    // llvm.hexagon.M2.mpyu.nac.hl.s1
    hexagon_M2_mpyu_nac_lh_s0,                    // llvm.hexagon.M2.mpyu.nac.lh.s0
    hexagon_M2_mpyu_nac_lh_s1,                    // llvm.hexagon.M2.mpyu.nac.lh.s1
    hexagon_M2_mpyu_nac_ll_s0,                    // llvm.hexagon.M2.mpyu.nac.ll.s0
    hexagon_M2_mpyu_nac_ll_s1,                    // llvm.hexagon.M2.mpyu.nac.ll.s1
    hexagon_M2_mpyu_up,                           // llvm.hexagon.M2.mpyu.up
    hexagon_M2_mpyud_acc_hh_s0,                   // llvm.hexagon.M2.mpyud.acc.hh.s0
    hexagon_M2_mpyud_acc_hh_s1,                   // llvm.hexagon.M2.mpyud.acc.hh.s1
    hexagon_M2_mpyud_acc_hl_s0,                   // llvm.hexagon.M2.mpyud.acc.hl.s0
    hexagon_M2_mpyud_acc_hl_s1,                   // llvm.hexagon.M2.mpyud.acc.hl.s1
    hexagon_M2_mpyud_acc_lh_s0,                   // llvm.hexagon.M2.mpyud.acc.lh.s0
    hexagon_M2_mpyud_acc_lh_s1,                   // llvm.hexagon.M2.mpyud.acc.lh.s1
    hexagon_M2_mpyud_acc_ll_s0,                   // llvm.hexagon.M2.mpyud.acc.ll.s0
    hexagon_M2_mpyud_acc_ll_s1,                   // llvm.hexagon.M2.mpyud.acc.ll.s1
    hexagon_M2_mpyud_hh_s0,                       // llvm.hexagon.M2.mpyud.hh.s0
    hexagon_M2_mpyud_hh_s1,                       // llvm.hexagon.M2.mpyud.hh.s1
    hexagon_M2_mpyud_hl_s0,                       // llvm.hexagon.M2.mpyud.hl.s0
    hexagon_M2_mpyud_hl_s1,                       // llvm.hexagon.M2.mpyud.hl.s1
    hexagon_M2_mpyud_lh_s0,                       // llvm.hexagon.M2.mpyud.lh.s0
    hexagon_M2_mpyud_lh_s1,                       // llvm.hexagon.M2.mpyud.lh.s1
    hexagon_M2_mpyud_ll_s0,                       // llvm.hexagon.M2.mpyud.ll.s0
    hexagon_M2_mpyud_ll_s1,                       // llvm.hexagon.M2.mpyud.ll.s1
    hexagon_M2_mpyud_nac_hh_s0,                   // llvm.hexagon.M2.mpyud.nac.hh.s0
    hexagon_M2_mpyud_nac_hh_s1,                   // llvm.hexagon.M2.mpyud.nac.hh.s1
    hexagon_M2_mpyud_nac_hl_s0,                   // llvm.hexagon.M2.mpyud.nac.hl.s0
    hexagon_M2_mpyud_nac_hl_s1,                   // llvm.hexagon.M2.mpyud.nac.hl.s1
    hexagon_M2_mpyud_nac_lh_s0,                   // llvm.hexagon.M2.mpyud.nac.lh.s0
    hexagon_M2_mpyud_nac_lh_s1,                   // llvm.hexagon.M2.mpyud.nac.lh.s1
    hexagon_M2_mpyud_nac_ll_s0,                   // llvm.hexagon.M2.mpyud.nac.ll.s0
    hexagon_M2_mpyud_nac_ll_s1,                   // llvm.hexagon.M2.mpyud.nac.ll.s1
    hexagon_M2_mpyui,                             // llvm.hexagon.M2.mpyui
    hexagon_M2_nacci,                             // llvm.hexagon.M2.nacci
    hexagon_M2_naccii,                            // llvm.hexagon.M2.naccii
    hexagon_M2_subacc,                            // llvm.hexagon.M2.subacc
    hexagon_M2_vabsdiffh,                         // llvm.hexagon.M2.vabsdiffh
    hexagon_M2_vabsdiffw,                         // llvm.hexagon.M2.vabsdiffw
    hexagon_M2_vcmac_s0_sat_i,                    // llvm.hexagon.M2.vcmac.s0.sat.i
    hexagon_M2_vcmac_s0_sat_r,                    // llvm.hexagon.M2.vcmac.s0.sat.r
    hexagon_M2_vcmpy_s0_sat_i,                    // llvm.hexagon.M2.vcmpy.s0.sat.i
    hexagon_M2_vcmpy_s0_sat_r,                    // llvm.hexagon.M2.vcmpy.s0.sat.r
    hexagon_M2_vcmpy_s1_sat_i,                    // llvm.hexagon.M2.vcmpy.s1.sat.i
    hexagon_M2_vcmpy_s1_sat_r,                    // llvm.hexagon.M2.vcmpy.s1.sat.r
    hexagon_M2_vdmacs_s0,                         // llvm.hexagon.M2.vdmacs.s0
    hexagon_M2_vdmacs_s1,                         // llvm.hexagon.M2.vdmacs.s1
    hexagon_M2_vdmpyrs_s0,                        // llvm.hexagon.M2.vdmpyrs.s0
    hexagon_M2_vdmpyrs_s1,                        // llvm.hexagon.M2.vdmpyrs.s1
    hexagon_M2_vdmpys_s0,                         // llvm.hexagon.M2.vdmpys.s0
    hexagon_M2_vdmpys_s1,                         // llvm.hexagon.M2.vdmpys.s1
    hexagon_M2_vmac2,                             // llvm.hexagon.M2.vmac2
    hexagon_M2_vmac2es,                           // llvm.hexagon.M2.vmac2es
    hexagon_M2_vmac2es_s0,                        // llvm.hexagon.M2.vmac2es.s0
    hexagon_M2_vmac2es_s1,                        // llvm.hexagon.M2.vmac2es.s1
    hexagon_M2_vmac2s_s0,                         // llvm.hexagon.M2.vmac2s.s0
    hexagon_M2_vmac2s_s1,                         // llvm.hexagon.M2.vmac2s.s1
    hexagon_M2_vmac2su_s0,                        // llvm.hexagon.M2.vmac2su.s0
    hexagon_M2_vmac2su_s1,                        // llvm.hexagon.M2.vmac2su.s1
    hexagon_M2_vmpy2es_s0,                        // llvm.hexagon.M2.vmpy2es.s0
    hexagon_M2_vmpy2es_s1,                        // llvm.hexagon.M2.vmpy2es.s1
    hexagon_M2_vmpy2s_s0,                         // llvm.hexagon.M2.vmpy2s.s0
    hexagon_M2_vmpy2s_s0pack,                     // llvm.hexagon.M2.vmpy2s.s0pack
    hexagon_M2_vmpy2s_s1,                         // llvm.hexagon.M2.vmpy2s.s1
    hexagon_M2_vmpy2s_s1pack,                     // llvm.hexagon.M2.vmpy2s.s1pack
    hexagon_M2_vmpy2su_s0,                        // llvm.hexagon.M2.vmpy2su.s0
    hexagon_M2_vmpy2su_s1,                        // llvm.hexagon.M2.vmpy2su.s1
    hexagon_M2_vraddh,                            // llvm.hexagon.M2.vraddh
    hexagon_M2_vradduh,                           // llvm.hexagon.M2.vradduh
    hexagon_M2_vrcmaci_s0,                        // llvm.hexagon.M2.vrcmaci.s0
    hexagon_M2_vrcmaci_s0c,                       // llvm.hexagon.M2.vrcmaci.s0c
    hexagon_M2_vrcmacr_s0,                        // llvm.hexagon.M2.vrcmacr.s0
    hexagon_M2_vrcmacr_s0c,                       // llvm.hexagon.M2.vrcmacr.s0c
    hexagon_M2_vrcmpyi_s0,                        // llvm.hexagon.M2.vrcmpyi.s0
    hexagon_M2_vrcmpyi_s0c,                       // llvm.hexagon.M2.vrcmpyi.s0c
    hexagon_M2_vrcmpyr_s0,                        // llvm.hexagon.M2.vrcmpyr.s0
    hexagon_M2_vrcmpyr_s0c,                       // llvm.hexagon.M2.vrcmpyr.s0c
    hexagon_M2_vrcmpys_acc_s1,                    // llvm.hexagon.M2.vrcmpys.acc.s1
    hexagon_M2_vrcmpys_s1,                        // llvm.hexagon.M2.vrcmpys.s1
    hexagon_M2_vrcmpys_s1rp,                      // llvm.hexagon.M2.vrcmpys.s1rp
    hexagon_M2_vrmac_s0,                          // llvm.hexagon.M2.vrmac.s0
    hexagon_M2_vrmpy_s0,                          // llvm.hexagon.M2.vrmpy.s0
    hexagon_M2_xor_xacc,                          // llvm.hexagon.M2.xor.xacc
    hexagon_M4_and_and,                           // llvm.hexagon.M4.and.and
    hexagon_M4_and_andn,                          // llvm.hexagon.M4.and.andn
    hexagon_M4_and_or,                            // llvm.hexagon.M4.and.or
    hexagon_M4_and_xor,                           // llvm.hexagon.M4.and.xor
    hexagon_M4_cmpyi_wh,                          // llvm.hexagon.M4.cmpyi.wh
    hexagon_M4_cmpyi_whc,                         // llvm.hexagon.M4.cmpyi.whc
    hexagon_M4_cmpyr_wh,                          // llvm.hexagon.M4.cmpyr.wh
    hexagon_M4_cmpyr_whc,                         // llvm.hexagon.M4.cmpyr.whc
    hexagon_M4_mac_up_s1_sat,                     // llvm.hexagon.M4.mac.up.s1.sat
    hexagon_M4_mpyri_addi,                        // llvm.hexagon.M4.mpyri.addi
    hexagon_M4_mpyri_addr,                        // llvm.hexagon.M4.mpyri.addr
    hexagon_M4_mpyri_addr_u2,                     // llvm.hexagon.M4.mpyri.addr.u2
    hexagon_M4_mpyrr_addi,                        // llvm.hexagon.M4.mpyrr.addi
    hexagon_M4_mpyrr_addr,                        // llvm.hexagon.M4.mpyrr.addr
    hexagon_M4_nac_up_s1_sat,                     // llvm.hexagon.M4.nac.up.s1.sat
    hexagon_M4_or_and,                            // llvm.hexagon.M4.or.and
    hexagon_M4_or_andn,                           // llvm.hexagon.M4.or.andn
    hexagon_M4_or_or,                             // llvm.hexagon.M4.or.or
    hexagon_M4_or_xor,                            // llvm.hexagon.M4.or.xor
    hexagon_M4_pmpyw,                             // llvm.hexagon.M4.pmpyw
    hexagon_M4_pmpyw_acc,                         // llvm.hexagon.M4.pmpyw.acc
    hexagon_M4_vpmpyh,                            // llvm.hexagon.M4.vpmpyh
    hexagon_M4_vpmpyh_acc,                        // llvm.hexagon.M4.vpmpyh.acc
    hexagon_M4_vrmpyeh_acc_s0,                    // llvm.hexagon.M4.vrmpyeh.acc.s0
    hexagon_M4_vrmpyeh_acc_s1,                    // llvm.hexagon.M4.vrmpyeh.acc.s1
    hexagon_M4_vrmpyeh_s0,                        // llvm.hexagon.M4.vrmpyeh.s0
    hexagon_M4_vrmpyeh_s1,                        // llvm.hexagon.M4.vrmpyeh.s1
    hexagon_M4_vrmpyoh_acc_s0,                    // llvm.hexagon.M4.vrmpyoh.acc.s0
    hexagon_M4_vrmpyoh_acc_s1,                    // llvm.hexagon.M4.vrmpyoh.acc.s1
    hexagon_M4_vrmpyoh_s0,                        // llvm.hexagon.M4.vrmpyoh.s0
    hexagon_M4_vrmpyoh_s1,                        // llvm.hexagon.M4.vrmpyoh.s1
    hexagon_M4_xor_and,                           // llvm.hexagon.M4.xor.and
    hexagon_M4_xor_andn,                          // llvm.hexagon.M4.xor.andn
    hexagon_M4_xor_or,                            // llvm.hexagon.M4.xor.or
    hexagon_M4_xor_xacc,                          // llvm.hexagon.M4.xor.xacc
    hexagon_M5_vdmacbsu,                          // llvm.hexagon.M5.vdmacbsu
    hexagon_M5_vdmpybsu,                          // llvm.hexagon.M5.vdmpybsu
    hexagon_M5_vmacbsu,                           // llvm.hexagon.M5.vmacbsu
    hexagon_M5_vmacbuu,                           // llvm.hexagon.M5.vmacbuu
    hexagon_M5_vmpybsu,                           // llvm.hexagon.M5.vmpybsu
    hexagon_M5_vmpybuu,                           // llvm.hexagon.M5.vmpybuu
    hexagon_M5_vrmacbsu,                          // llvm.hexagon.M5.vrmacbsu
    hexagon_M5_vrmacbuu,                          // llvm.hexagon.M5.vrmacbuu
    hexagon_M5_vrmpybsu,                          // llvm.hexagon.M5.vrmpybsu
    hexagon_M5_vrmpybuu,                          // llvm.hexagon.M5.vrmpybuu
    hexagon_M6_vabsdiffb,                         // llvm.hexagon.M6.vabsdiffb
    hexagon_M6_vabsdiffub,                        // llvm.hexagon.M6.vabsdiffub
    hexagon_S2_addasl_rrri,                       // llvm.hexagon.S2.addasl.rrri
    hexagon_S2_asl_i_p,                           // llvm.hexagon.S2.asl.i.p
    hexagon_S2_asl_i_p_acc,                       // llvm.hexagon.S2.asl.i.p.acc
    hexagon_S2_asl_i_p_and,                       // llvm.hexagon.S2.asl.i.p.and
    hexagon_S2_asl_i_p_nac,                       // llvm.hexagon.S2.asl.i.p.nac
    hexagon_S2_asl_i_p_or,                        // llvm.hexagon.S2.asl.i.p.or
    hexagon_S2_asl_i_p_xacc,                      // llvm.hexagon.S2.asl.i.p.xacc
    hexagon_S2_asl_i_r,                           // llvm.hexagon.S2.asl.i.r
    hexagon_S2_asl_i_r_acc,                       // llvm.hexagon.S2.asl.i.r.acc
    hexagon_S2_asl_i_r_and,                       // llvm.hexagon.S2.asl.i.r.and
    hexagon_S2_asl_i_r_nac,                       // llvm.hexagon.S2.asl.i.r.nac
    hexagon_S2_asl_i_r_or,                        // llvm.hexagon.S2.asl.i.r.or
    hexagon_S2_asl_i_r_sat,                       // llvm.hexagon.S2.asl.i.r.sat
    hexagon_S2_asl_i_r_xacc,                      // llvm.hexagon.S2.asl.i.r.xacc
    hexagon_S2_asl_i_vh,                          // llvm.hexagon.S2.asl.i.vh
    hexagon_S2_asl_i_vw,                          // llvm.hexagon.S2.asl.i.vw
    hexagon_S2_asl_r_p,                           // llvm.hexagon.S2.asl.r.p
    hexagon_S2_asl_r_p_acc,                       // llvm.hexagon.S2.asl.r.p.acc
    hexagon_S2_asl_r_p_and,                       // llvm.hexagon.S2.asl.r.p.and
    hexagon_S2_asl_r_p_nac,                       // llvm.hexagon.S2.asl.r.p.nac
    hexagon_S2_asl_r_p_or,                        // llvm.hexagon.S2.asl.r.p.or
    hexagon_S2_asl_r_p_xor,                       // llvm.hexagon.S2.asl.r.p.xor
    hexagon_S2_asl_r_r,                           // llvm.hexagon.S2.asl.r.r
    hexagon_S2_asl_r_r_acc,                       // llvm.hexagon.S2.asl.r.r.acc
    hexagon_S2_asl_r_r_and,                       // llvm.hexagon.S2.asl.r.r.and
    hexagon_S2_asl_r_r_nac,                       // llvm.hexagon.S2.asl.r.r.nac
    hexagon_S2_asl_r_r_or,                        // llvm.hexagon.S2.asl.r.r.or
    hexagon_S2_asl_r_r_sat,                       // llvm.hexagon.S2.asl.r.r.sat
    hexagon_S2_asl_r_vh,                          // llvm.hexagon.S2.asl.r.vh
    hexagon_S2_asl_r_vw,                          // llvm.hexagon.S2.asl.r.vw
    hexagon_S2_asr_i_p,                           // llvm.hexagon.S2.asr.i.p
    hexagon_S2_asr_i_p_acc,                       // llvm.hexagon.S2.asr.i.p.acc
    hexagon_S2_asr_i_p_and,                       // llvm.hexagon.S2.asr.i.p.and
    hexagon_S2_asr_i_p_nac,                       // llvm.hexagon.S2.asr.i.p.nac
    hexagon_S2_asr_i_p_or,                        // llvm.hexagon.S2.asr.i.p.or
    hexagon_S2_asr_i_p_rnd,                       // llvm.hexagon.S2.asr.i.p.rnd
    hexagon_S2_asr_i_p_rnd_goodsyntax,            // llvm.hexagon.S2.asr.i.p.rnd.goodsyntax
    hexagon_S2_asr_i_r,                           // llvm.hexagon.S2.asr.i.r
    hexagon_S2_asr_i_r_acc,                       // llvm.hexagon.S2.asr.i.r.acc
    hexagon_S2_asr_i_r_and,                       // llvm.hexagon.S2.asr.i.r.and
    hexagon_S2_asr_i_r_nac,                       // llvm.hexagon.S2.asr.i.r.nac
    hexagon_S2_asr_i_r_or,                        // llvm.hexagon.S2.asr.i.r.or
    hexagon_S2_asr_i_r_rnd,                       // llvm.hexagon.S2.asr.i.r.rnd
    hexagon_S2_asr_i_r_rnd_goodsyntax,            // llvm.hexagon.S2.asr.i.r.rnd.goodsyntax
    hexagon_S2_asr_i_svw_trun,                    // llvm.hexagon.S2.asr.i.svw.trun
    hexagon_S2_asr_i_vh,                          // llvm.hexagon.S2.asr.i.vh
    hexagon_S2_asr_i_vw,                          // llvm.hexagon.S2.asr.i.vw
    hexagon_S2_asr_r_p,                           // llvm.hexagon.S2.asr.r.p
    hexagon_S2_asr_r_p_acc,                       // llvm.hexagon.S2.asr.r.p.acc
    hexagon_S2_asr_r_p_and,                       // llvm.hexagon.S2.asr.r.p.and
    hexagon_S2_asr_r_p_nac,                       // llvm.hexagon.S2.asr.r.p.nac
    hexagon_S2_asr_r_p_or,                        // llvm.hexagon.S2.asr.r.p.or
    hexagon_S2_asr_r_p_xor,                       // llvm.hexagon.S2.asr.r.p.xor
    hexagon_S2_asr_r_r,                           // llvm.hexagon.S2.asr.r.r
    hexagon_S2_asr_r_r_acc,                       // llvm.hexagon.S2.asr.r.r.acc
    hexagon_S2_asr_r_r_and,                       // llvm.hexagon.S2.asr.r.r.and
    hexagon_S2_asr_r_r_nac,                       // llvm.hexagon.S2.asr.r.r.nac
    hexagon_S2_asr_r_r_or,                        // llvm.hexagon.S2.asr.r.r.or
    hexagon_S2_asr_r_r_sat,                       // llvm.hexagon.S2.asr.r.r.sat
    hexagon_S2_asr_r_svw_trun,                    // llvm.hexagon.S2.asr.r.svw.trun
    hexagon_S2_asr_r_vh,                          // llvm.hexagon.S2.asr.r.vh
    hexagon_S2_asr_r_vw,                          // llvm.hexagon.S2.asr.r.vw
    hexagon_S2_brev,                              // llvm.hexagon.S2.brev
    hexagon_S2_brevp,                             // llvm.hexagon.S2.brevp
    hexagon_S2_cabacencbin,                       // llvm.hexagon.S2.cabacencbin
    hexagon_S2_cl0,                               // llvm.hexagon.S2.cl0
    hexagon_S2_cl0p,                              // llvm.hexagon.S2.cl0p
    hexagon_S2_cl1,                               // llvm.hexagon.S2.cl1
    hexagon_S2_cl1p,                              // llvm.hexagon.S2.cl1p
    hexagon_S2_clb,                               // llvm.hexagon.S2.clb
    hexagon_S2_clbnorm,                           // llvm.hexagon.S2.clbnorm
    hexagon_S2_clbp,                              // llvm.hexagon.S2.clbp
    hexagon_S2_clrbit_i,                          // llvm.hexagon.S2.clrbit.i
    hexagon_S2_clrbit_r,                          // llvm.hexagon.S2.clrbit.r
    hexagon_S2_ct0,                               // llvm.hexagon.S2.ct0
    hexagon_S2_ct0p,                              // llvm.hexagon.S2.ct0p
    hexagon_S2_ct1,                               // llvm.hexagon.S2.ct1
    hexagon_S2_ct1p,                              // llvm.hexagon.S2.ct1p
    hexagon_S2_deinterleave,                      // llvm.hexagon.S2.deinterleave
    hexagon_S2_extractu,                          // llvm.hexagon.S2.extractu
    hexagon_S2_extractu_rp,                       // llvm.hexagon.S2.extractu.rp
    hexagon_S2_extractup,                         // llvm.hexagon.S2.extractup
    hexagon_S2_extractup_rp,                      // llvm.hexagon.S2.extractup.rp
    hexagon_S2_insert,                            // llvm.hexagon.S2.insert
    hexagon_S2_insert_rp,                         // llvm.hexagon.S2.insert.rp
    hexagon_S2_insertp,                           // llvm.hexagon.S2.insertp
    hexagon_S2_insertp_rp,                        // llvm.hexagon.S2.insertp.rp
    hexagon_S2_interleave,                        // llvm.hexagon.S2.interleave
    hexagon_S2_lfsp,                              // llvm.hexagon.S2.lfsp
    hexagon_S2_lsl_r_p,                           // llvm.hexagon.S2.lsl.r.p
    hexagon_S2_lsl_r_p_acc,                       // llvm.hexagon.S2.lsl.r.p.acc
    hexagon_S2_lsl_r_p_and,                       // llvm.hexagon.S2.lsl.r.p.and
    hexagon_S2_lsl_r_p_nac,                       // llvm.hexagon.S2.lsl.r.p.nac
    hexagon_S2_lsl_r_p_or,                        // llvm.hexagon.S2.lsl.r.p.or
    hexagon_S2_lsl_r_p_xor,                       // llvm.hexagon.S2.lsl.r.p.xor
    hexagon_S2_lsl_r_r,                           // llvm.hexagon.S2.lsl.r.r
    hexagon_S2_lsl_r_r_acc,                       // llvm.hexagon.S2.lsl.r.r.acc
    hexagon_S2_lsl_r_r_and,                       // llvm.hexagon.S2.lsl.r.r.and
    hexagon_S2_lsl_r_r_nac,                       // llvm.hexagon.S2.lsl.r.r.nac
    hexagon_S2_lsl_r_r_or,                        // llvm.hexagon.S2.lsl.r.r.or
    hexagon_S2_lsl_r_vh,                          // llvm.hexagon.S2.lsl.r.vh
    hexagon_S2_lsl_r_vw,                          // llvm.hexagon.S2.lsl.r.vw
    hexagon_S2_lsr_i_p,                           // llvm.hexagon.S2.lsr.i.p
    hexagon_S2_lsr_i_p_acc,                       // llvm.hexagon.S2.lsr.i.p.acc
    hexagon_S2_lsr_i_p_and,                       // llvm.hexagon.S2.lsr.i.p.and
    hexagon_S2_lsr_i_p_nac,                       // llvm.hexagon.S2.lsr.i.p.nac
    hexagon_S2_lsr_i_p_or,                        // llvm.hexagon.S2.lsr.i.p.or
    hexagon_S2_lsr_i_p_xacc,                      // llvm.hexagon.S2.lsr.i.p.xacc
    hexagon_S2_lsr_i_r,                           // llvm.hexagon.S2.lsr.i.r
    hexagon_S2_lsr_i_r_acc,                       // llvm.hexagon.S2.lsr.i.r.acc
    hexagon_S2_lsr_i_r_and,                       // llvm.hexagon.S2.lsr.i.r.and
    hexagon_S2_lsr_i_r_nac,                       // llvm.hexagon.S2.lsr.i.r.nac
    hexagon_S2_lsr_i_r_or,                        // llvm.hexagon.S2.lsr.i.r.or
    hexagon_S2_lsr_i_r_xacc,                      // llvm.hexagon.S2.lsr.i.r.xacc
    hexagon_S2_lsr_i_vh,                          // llvm.hexagon.S2.lsr.i.vh
    hexagon_S2_lsr_i_vw,                          // llvm.hexagon.S2.lsr.i.vw
    hexagon_S2_lsr_r_p,                           // llvm.hexagon.S2.lsr.r.p
    hexagon_S2_lsr_r_p_acc,                       // llvm.hexagon.S2.lsr.r.p.acc
    hexagon_S2_lsr_r_p_and,                       // llvm.hexagon.S2.lsr.r.p.and
    hexagon_S2_lsr_r_p_nac,                       // llvm.hexagon.S2.lsr.r.p.nac
    hexagon_S2_lsr_r_p_or,                        // llvm.hexagon.S2.lsr.r.p.or
    hexagon_S2_lsr_r_p_xor,                       // llvm.hexagon.S2.lsr.r.p.xor
    hexagon_S2_lsr_r_r,                           // llvm.hexagon.S2.lsr.r.r
    hexagon_S2_lsr_r_r_acc,                       // llvm.hexagon.S2.lsr.r.r.acc
    hexagon_S2_lsr_r_r_and,                       // llvm.hexagon.S2.lsr.r.r.and
    hexagon_S2_lsr_r_r_nac,                       // llvm.hexagon.S2.lsr.r.r.nac
    hexagon_S2_lsr_r_r_or,                        // llvm.hexagon.S2.lsr.r.r.or
    hexagon_S2_lsr_r_vh,                          // llvm.hexagon.S2.lsr.r.vh
    hexagon_S2_lsr_r_vw,                          // llvm.hexagon.S2.lsr.r.vw
    hexagon_S2_packhl,                            // llvm.hexagon.S2.packhl
    hexagon_S2_parityp,                           // llvm.hexagon.S2.parityp
    hexagon_S2_setbit_i,                          // llvm.hexagon.S2.setbit.i
    hexagon_S2_setbit_r,                          // llvm.hexagon.S2.setbit.r
    hexagon_S2_shuffeb,                           // llvm.hexagon.S2.shuffeb
    hexagon_S2_shuffeh,                           // llvm.hexagon.S2.shuffeh
    hexagon_S2_shuffob,                           // llvm.hexagon.S2.shuffob
    hexagon_S2_shuffoh,                           // llvm.hexagon.S2.shuffoh
    hexagon_S2_storew_locked,                     // llvm.hexagon.S2.storew.locked
    hexagon_S2_svsathb,                           // llvm.hexagon.S2.svsathb
    hexagon_S2_svsathub,                          // llvm.hexagon.S2.svsathub
    hexagon_S2_tableidxb_goodsyntax,              // llvm.hexagon.S2.tableidxb.goodsyntax
    hexagon_S2_tableidxd_goodsyntax,              // llvm.hexagon.S2.tableidxd.goodsyntax
    hexagon_S2_tableidxh_goodsyntax,              // llvm.hexagon.S2.tableidxh.goodsyntax
    hexagon_S2_tableidxw_goodsyntax,              // llvm.hexagon.S2.tableidxw.goodsyntax
    hexagon_S2_togglebit_i,                       // llvm.hexagon.S2.togglebit.i
    hexagon_S2_togglebit_r,                       // llvm.hexagon.S2.togglebit.r
    hexagon_S2_tstbit_i,                          // llvm.hexagon.S2.tstbit.i
    hexagon_S2_tstbit_r,                          // llvm.hexagon.S2.tstbit.r
    hexagon_S2_valignib,                          // llvm.hexagon.S2.valignib
    hexagon_S2_valignrb,                          // llvm.hexagon.S2.valignrb
    hexagon_S2_vcnegh,                            // llvm.hexagon.S2.vcnegh
    hexagon_S2_vcrotate,                          // llvm.hexagon.S2.vcrotate
    hexagon_S2_vrcnegh,                           // llvm.hexagon.S2.vrcnegh
    hexagon_S2_vrndpackwh,                        // llvm.hexagon.S2.vrndpackwh
    hexagon_S2_vrndpackwhs,                       // llvm.hexagon.S2.vrndpackwhs
    hexagon_S2_vsathb,                            // llvm.hexagon.S2.vsathb
    hexagon_S2_vsathb_nopack,                     // llvm.hexagon.S2.vsathb.nopack
    hexagon_S2_vsathub,                           // llvm.hexagon.S2.vsathub
    hexagon_S2_vsathub_nopack,                    // llvm.hexagon.S2.vsathub.nopack
    hexagon_S2_vsatwh,                            // llvm.hexagon.S2.vsatwh
    hexagon_S2_vsatwh_nopack,                     // llvm.hexagon.S2.vsatwh.nopack
    hexagon_S2_vsatwuh,                           // llvm.hexagon.S2.vsatwuh
    hexagon_S2_vsatwuh_nopack,                    // llvm.hexagon.S2.vsatwuh.nopack
    hexagon_S2_vsplatrb,                          // llvm.hexagon.S2.vsplatrb
    hexagon_S2_vsplatrh,                          // llvm.hexagon.S2.vsplatrh
    hexagon_S2_vspliceib,                         // llvm.hexagon.S2.vspliceib
    hexagon_S2_vsplicerb,                         // llvm.hexagon.S2.vsplicerb
    hexagon_S2_vsxtbh,                            // llvm.hexagon.S2.vsxtbh
    hexagon_S2_vsxthw,                            // llvm.hexagon.S2.vsxthw
    hexagon_S2_vtrunehb,                          // llvm.hexagon.S2.vtrunehb
    hexagon_S2_vtrunewh,                          // llvm.hexagon.S2.vtrunewh
    hexagon_S2_vtrunohb,                          // llvm.hexagon.S2.vtrunohb
    hexagon_S2_vtrunowh,                          // llvm.hexagon.S2.vtrunowh
    hexagon_S2_vzxtbh,                            // llvm.hexagon.S2.vzxtbh
    hexagon_S2_vzxthw,                            // llvm.hexagon.S2.vzxthw
    hexagon_S4_addaddi,                           // llvm.hexagon.S4.addaddi
    hexagon_S4_addi_asl_ri,                       // llvm.hexagon.S4.addi.asl.ri
    hexagon_S4_addi_lsr_ri,                       // llvm.hexagon.S4.addi.lsr.ri
    hexagon_S4_andi_asl_ri,                       // llvm.hexagon.S4.andi.asl.ri
    hexagon_S4_andi_lsr_ri,                       // llvm.hexagon.S4.andi.lsr.ri
    hexagon_S4_clbaddi,                           // llvm.hexagon.S4.clbaddi
    hexagon_S4_clbpaddi,                          // llvm.hexagon.S4.clbpaddi
    hexagon_S4_clbpnorm,                          // llvm.hexagon.S4.clbpnorm
    hexagon_S4_extract,                           // llvm.hexagon.S4.extract
    hexagon_S4_extract_rp,                        // llvm.hexagon.S4.extract.rp
    hexagon_S4_extractp,                          // llvm.hexagon.S4.extractp
    hexagon_S4_extractp_rp,                       // llvm.hexagon.S4.extractp.rp
    hexagon_S4_lsli,                              // llvm.hexagon.S4.lsli
    hexagon_S4_ntstbit_i,                         // llvm.hexagon.S4.ntstbit.i
    hexagon_S4_ntstbit_r,                         // llvm.hexagon.S4.ntstbit.r
    hexagon_S4_or_andi,                           // llvm.hexagon.S4.or.andi
    hexagon_S4_or_andix,                          // llvm.hexagon.S4.or.andix
    hexagon_S4_or_ori,                            // llvm.hexagon.S4.or.ori
    hexagon_S4_ori_asl_ri,                        // llvm.hexagon.S4.ori.asl.ri
    hexagon_S4_ori_lsr_ri,                        // llvm.hexagon.S4.ori.lsr.ri
    hexagon_S4_parity,                            // llvm.hexagon.S4.parity
    hexagon_S4_stored_locked,                     // llvm.hexagon.S4.stored.locked
    hexagon_S4_subaddi,                           // llvm.hexagon.S4.subaddi
    hexagon_S4_subi_asl_ri,                       // llvm.hexagon.S4.subi.asl.ri
    hexagon_S4_subi_lsr_ri,                       // llvm.hexagon.S4.subi.lsr.ri
    hexagon_S4_vrcrotate,                         // llvm.hexagon.S4.vrcrotate
    hexagon_S4_vrcrotate_acc,                     // llvm.hexagon.S4.vrcrotate.acc
    hexagon_S4_vxaddsubh,                         // llvm.hexagon.S4.vxaddsubh
    hexagon_S4_vxaddsubhr,                        // llvm.hexagon.S4.vxaddsubhr
    hexagon_S4_vxaddsubw,                         // llvm.hexagon.S4.vxaddsubw
    hexagon_S4_vxsubaddh,                         // llvm.hexagon.S4.vxsubaddh
    hexagon_S4_vxsubaddhr,                        // llvm.hexagon.S4.vxsubaddhr
    hexagon_S4_vxsubaddw,                         // llvm.hexagon.S4.vxsubaddw
    hexagon_S5_asrhub_rnd_sat_goodsyntax,         // llvm.hexagon.S5.asrhub.rnd.sat.goodsyntax
    hexagon_S5_asrhub_sat,                        // llvm.hexagon.S5.asrhub.sat
    hexagon_S5_popcountp,                         // llvm.hexagon.S5.popcountp
    hexagon_S5_vasrhrnd_goodsyntax,               // llvm.hexagon.S5.vasrhrnd.goodsyntax
    hexagon_S6_rol_i_p,                           // llvm.hexagon.S6.rol.i.p
    hexagon_S6_rol_i_p_acc,                       // llvm.hexagon.S6.rol.i.p.acc
    hexagon_S6_rol_i_p_and,                       // llvm.hexagon.S6.rol.i.p.and
    hexagon_S6_rol_i_p_nac,                       // llvm.hexagon.S6.rol.i.p.nac
    hexagon_S6_rol_i_p_or,                        // llvm.hexagon.S6.rol.i.p.or
    hexagon_S6_rol_i_p_xacc,                      // llvm.hexagon.S6.rol.i.p.xacc
    hexagon_S6_rol_i_r,                           // llvm.hexagon.S6.rol.i.r
    hexagon_S6_rol_i_r_acc,                       // llvm.hexagon.S6.rol.i.r.acc
    hexagon_S6_rol_i_r_and,                       // llvm.hexagon.S6.rol.i.r.and
    hexagon_S6_rol_i_r_nac,                       // llvm.hexagon.S6.rol.i.r.nac
    hexagon_S6_rol_i_r_or,                        // llvm.hexagon.S6.rol.i.r.or
    hexagon_S6_rol_i_r_xacc,                      // llvm.hexagon.S6.rol.i.r.xacc
    hexagon_S6_vsplatrbp,                         // llvm.hexagon.S6.vsplatrbp
    hexagon_S6_vtrunehb_ppp,                      // llvm.hexagon.S6.vtrunehb.ppp
    hexagon_S6_vtrunohb_ppp,                      // llvm.hexagon.S6.vtrunohb.ppp
    hexagon_SI_to_SXTHI_asrh,                     // llvm.hexagon.SI.to.SXTHI.asrh
    hexagon_V6_extractw,                          // llvm.hexagon.V6.extractw
    hexagon_V6_extractw_128B,                     // llvm.hexagon.V6.extractw.128B
    hexagon_V6_hi,                                // llvm.hexagon.V6.hi
    hexagon_V6_hi_128B,                           // llvm.hexagon.V6.hi.128B
    hexagon_V6_lo,                                // llvm.hexagon.V6.lo
    hexagon_V6_lo_128B,                           // llvm.hexagon.V6.lo.128B
    hexagon_V6_lvsplatb,                          // llvm.hexagon.V6.lvsplatb
    hexagon_V6_lvsplatb_128B,                     // llvm.hexagon.V6.lvsplatb.128B
    hexagon_V6_lvsplath,                          // llvm.hexagon.V6.lvsplath
    hexagon_V6_lvsplath_128B,                     // llvm.hexagon.V6.lvsplath.128B
    hexagon_V6_lvsplatw,                          // llvm.hexagon.V6.lvsplatw
    hexagon_V6_lvsplatw_128B,                     // llvm.hexagon.V6.lvsplatw.128B
    hexagon_V6_pred_and,                          // llvm.hexagon.V6.pred.and
    hexagon_V6_pred_and_128B,                     // llvm.hexagon.V6.pred.and.128B
    hexagon_V6_pred_and_n,                        // llvm.hexagon.V6.pred.and.n
    hexagon_V6_pred_and_n_128B,                   // llvm.hexagon.V6.pred.and.n.128B
    hexagon_V6_pred_not,                          // llvm.hexagon.V6.pred.not
    hexagon_V6_pred_not_128B,                     // llvm.hexagon.V6.pred.not.128B
    hexagon_V6_pred_or,                           // llvm.hexagon.V6.pred.or
    hexagon_V6_pred_or_128B,                      // llvm.hexagon.V6.pred.or.128B
    hexagon_V6_pred_or_n,                         // llvm.hexagon.V6.pred.or.n
    hexagon_V6_pred_or_n_128B,                    // llvm.hexagon.V6.pred.or.n.128B
    hexagon_V6_pred_scalar2,                      // llvm.hexagon.V6.pred.scalar2
    hexagon_V6_pred_scalar2_128B,                 // llvm.hexagon.V6.pred.scalar2.128B
    hexagon_V6_pred_scalar2v2,                    // llvm.hexagon.V6.pred.scalar2v2
    hexagon_V6_pred_scalar2v2_128B,               // llvm.hexagon.V6.pred.scalar2v2.128B
    hexagon_V6_pred_xor,                          // llvm.hexagon.V6.pred.xor
    hexagon_V6_pred_xor_128B,                     // llvm.hexagon.V6.pred.xor.128B
    hexagon_V6_shuffeqh,                          // llvm.hexagon.V6.shuffeqh
    hexagon_V6_shuffeqh_128B,                     // llvm.hexagon.V6.shuffeqh.128B
    hexagon_V6_shuffeqw,                          // llvm.hexagon.V6.shuffeqw
    hexagon_V6_shuffeqw_128B,                     // llvm.hexagon.V6.shuffeqw.128B
    hexagon_V6_vS32b_nqpred_ai,                   // llvm.hexagon.V6.vS32b.nqpred.ai
    hexagon_V6_vS32b_nqpred_ai_128B,              // llvm.hexagon.V6.vS32b.nqpred.ai.128B
    hexagon_V6_vS32b_nt_nqpred_ai,                // llvm.hexagon.V6.vS32b.nt.nqpred.ai
    hexagon_V6_vS32b_nt_nqpred_ai_128B,           // llvm.hexagon.V6.vS32b.nt.nqpred.ai.128B
    hexagon_V6_vS32b_nt_qpred_ai,                 // llvm.hexagon.V6.vS32b.nt.qpred.ai
    hexagon_V6_vS32b_nt_qpred_ai_128B,            // llvm.hexagon.V6.vS32b.nt.qpred.ai.128B
    hexagon_V6_vS32b_qpred_ai,                    // llvm.hexagon.V6.vS32b.qpred.ai
    hexagon_V6_vS32b_qpred_ai_128B,               // llvm.hexagon.V6.vS32b.qpred.ai.128B
    hexagon_V6_vabsb,                             // llvm.hexagon.V6.vabsb
    hexagon_V6_vabsb_128B,                        // llvm.hexagon.V6.vabsb.128B
    hexagon_V6_vabsb_sat,                         // llvm.hexagon.V6.vabsb.sat
    hexagon_V6_vabsb_sat_128B,                    // llvm.hexagon.V6.vabsb.sat.128B
    hexagon_V6_vabsdiffh,                         // llvm.hexagon.V6.vabsdiffh
    hexagon_V6_vabsdiffh_128B,                    // llvm.hexagon.V6.vabsdiffh.128B
    hexagon_V6_vabsdiffub,                        // llvm.hexagon.V6.vabsdiffub
    hexagon_V6_vabsdiffub_128B,                   // llvm.hexagon.V6.vabsdiffub.128B
    hexagon_V6_vabsdiffuh,                        // llvm.hexagon.V6.vabsdiffuh
    hexagon_V6_vabsdiffuh_128B,                   // llvm.hexagon.V6.vabsdiffuh.128B
    hexagon_V6_vabsdiffw,                         // llvm.hexagon.V6.vabsdiffw
    hexagon_V6_vabsdiffw_128B,                    // llvm.hexagon.V6.vabsdiffw.128B
    hexagon_V6_vabsh,                             // llvm.hexagon.V6.vabsh
    hexagon_V6_vabsh_128B,                        // llvm.hexagon.V6.vabsh.128B
    hexagon_V6_vabsh_sat,                         // llvm.hexagon.V6.vabsh.sat
    hexagon_V6_vabsh_sat_128B,                    // llvm.hexagon.V6.vabsh.sat.128B
    hexagon_V6_vabsw,                             // llvm.hexagon.V6.vabsw
    hexagon_V6_vabsw_128B,                        // llvm.hexagon.V6.vabsw.128B
    hexagon_V6_vabsw_sat,                         // llvm.hexagon.V6.vabsw.sat
    hexagon_V6_vabsw_sat_128B,                    // llvm.hexagon.V6.vabsw.sat.128B
    hexagon_V6_vaddb,                             // llvm.hexagon.V6.vaddb
    hexagon_V6_vaddb_128B,                        // llvm.hexagon.V6.vaddb.128B
    hexagon_V6_vaddb_dv,                          // llvm.hexagon.V6.vaddb.dv
    hexagon_V6_vaddb_dv_128B,                     // llvm.hexagon.V6.vaddb.dv.128B
    hexagon_V6_vaddbnq,                           // llvm.hexagon.V6.vaddbnq
    hexagon_V6_vaddbnq_128B,                      // llvm.hexagon.V6.vaddbnq.128B
    hexagon_V6_vaddbq,                            // llvm.hexagon.V6.vaddbq
    hexagon_V6_vaddbq_128B,                       // llvm.hexagon.V6.vaddbq.128B
    hexagon_V6_vaddbsat,                          // llvm.hexagon.V6.vaddbsat
    hexagon_V6_vaddbsat_128B,                     // llvm.hexagon.V6.vaddbsat.128B
    hexagon_V6_vaddbsat_dv,                       // llvm.hexagon.V6.vaddbsat.dv
    hexagon_V6_vaddbsat_dv_128B,                  // llvm.hexagon.V6.vaddbsat.dv.128B
    hexagon_V6_vaddcarry,                         // llvm.hexagon.V6.vaddcarry
    hexagon_V6_vaddcarry_128B,                    // llvm.hexagon.V6.vaddcarry.128B
    hexagon_V6_vaddclbh,                          // llvm.hexagon.V6.vaddclbh
    hexagon_V6_vaddclbh_128B,                     // llvm.hexagon.V6.vaddclbh.128B
    hexagon_V6_vaddclbw,                          // llvm.hexagon.V6.vaddclbw
    hexagon_V6_vaddclbw_128B,                     // llvm.hexagon.V6.vaddclbw.128B
    hexagon_V6_vaddh,                             // llvm.hexagon.V6.vaddh
    hexagon_V6_vaddh_128B,                        // llvm.hexagon.V6.vaddh.128B
    hexagon_V6_vaddh_dv,                          // llvm.hexagon.V6.vaddh.dv
    hexagon_V6_vaddh_dv_128B,                     // llvm.hexagon.V6.vaddh.dv.128B
    hexagon_V6_vaddhnq,                           // llvm.hexagon.V6.vaddhnq
    hexagon_V6_vaddhnq_128B,                      // llvm.hexagon.V6.vaddhnq.128B
    hexagon_V6_vaddhq,                            // llvm.hexagon.V6.vaddhq
    hexagon_V6_vaddhq_128B,                       // llvm.hexagon.V6.vaddhq.128B
    hexagon_V6_vaddhsat,                          // llvm.hexagon.V6.vaddhsat
    hexagon_V6_vaddhsat_128B,                     // llvm.hexagon.V6.vaddhsat.128B
    hexagon_V6_vaddhsat_dv,                       // llvm.hexagon.V6.vaddhsat.dv
    hexagon_V6_vaddhsat_dv_128B,                  // llvm.hexagon.V6.vaddhsat.dv.128B
    hexagon_V6_vaddhw,                            // llvm.hexagon.V6.vaddhw
    hexagon_V6_vaddhw_128B,                       // llvm.hexagon.V6.vaddhw.128B
    hexagon_V6_vaddhw_acc,                        // llvm.hexagon.V6.vaddhw.acc
    hexagon_V6_vaddhw_acc_128B,                   // llvm.hexagon.V6.vaddhw.acc.128B
    hexagon_V6_vaddubh,                           // llvm.hexagon.V6.vaddubh
    hexagon_V6_vaddubh_128B,                      // llvm.hexagon.V6.vaddubh.128B
    hexagon_V6_vaddubh_acc,                       // llvm.hexagon.V6.vaddubh.acc
    hexagon_V6_vaddubh_acc_128B,                  // llvm.hexagon.V6.vaddubh.acc.128B
    hexagon_V6_vaddubsat,                         // llvm.hexagon.V6.vaddubsat
    hexagon_V6_vaddubsat_128B,                    // llvm.hexagon.V6.vaddubsat.128B
    hexagon_V6_vaddubsat_dv,                      // llvm.hexagon.V6.vaddubsat.dv
    hexagon_V6_vaddubsat_dv_128B,                 // llvm.hexagon.V6.vaddubsat.dv.128B
    hexagon_V6_vaddububb_sat,                     // llvm.hexagon.V6.vaddububb.sat
    hexagon_V6_vaddububb_sat_128B,                // llvm.hexagon.V6.vaddububb.sat.128B
    hexagon_V6_vadduhsat,                         // llvm.hexagon.V6.vadduhsat
    hexagon_V6_vadduhsat_128B,                    // llvm.hexagon.V6.vadduhsat.128B
    hexagon_V6_vadduhsat_dv,                      // llvm.hexagon.V6.vadduhsat.dv
    hexagon_V6_vadduhsat_dv_128B,                 // llvm.hexagon.V6.vadduhsat.dv.128B
    hexagon_V6_vadduhw,                           // llvm.hexagon.V6.vadduhw
    hexagon_V6_vadduhw_128B,                      // llvm.hexagon.V6.vadduhw.128B
    hexagon_V6_vadduhw_acc,                       // llvm.hexagon.V6.vadduhw.acc
    hexagon_V6_vadduhw_acc_128B,                  // llvm.hexagon.V6.vadduhw.acc.128B
    hexagon_V6_vadduwsat,                         // llvm.hexagon.V6.vadduwsat
    hexagon_V6_vadduwsat_128B,                    // llvm.hexagon.V6.vadduwsat.128B
    hexagon_V6_vadduwsat_dv,                      // llvm.hexagon.V6.vadduwsat.dv
    hexagon_V6_vadduwsat_dv_128B,                 // llvm.hexagon.V6.vadduwsat.dv.128B
    hexagon_V6_vaddw,                             // llvm.hexagon.V6.vaddw
    hexagon_V6_vaddw_128B,                        // llvm.hexagon.V6.vaddw.128B
    hexagon_V6_vaddw_dv,                          // llvm.hexagon.V6.vaddw.dv
    hexagon_V6_vaddw_dv_128B,                     // llvm.hexagon.V6.vaddw.dv.128B
    hexagon_V6_vaddwnq,                           // llvm.hexagon.V6.vaddwnq
    hexagon_V6_vaddwnq_128B,                      // llvm.hexagon.V6.vaddwnq.128B
    hexagon_V6_vaddwq,                            // llvm.hexagon.V6.vaddwq
    hexagon_V6_vaddwq_128B,                       // llvm.hexagon.V6.vaddwq.128B
    hexagon_V6_vaddwsat,                          // llvm.hexagon.V6.vaddwsat
    hexagon_V6_vaddwsat_128B,                     // llvm.hexagon.V6.vaddwsat.128B
    hexagon_V6_vaddwsat_dv,                       // llvm.hexagon.V6.vaddwsat.dv
    hexagon_V6_vaddwsat_dv_128B,                  // llvm.hexagon.V6.vaddwsat.dv.128B
    hexagon_V6_valignb,                           // llvm.hexagon.V6.valignb
    hexagon_V6_valignb_128B,                      // llvm.hexagon.V6.valignb.128B
    hexagon_V6_valignbi,                          // llvm.hexagon.V6.valignbi
    hexagon_V6_valignbi_128B,                     // llvm.hexagon.V6.valignbi.128B
    hexagon_V6_vand,                              // llvm.hexagon.V6.vand
    hexagon_V6_vand_128B,                         // llvm.hexagon.V6.vand.128B
    hexagon_V6_vandnqrt,                          // llvm.hexagon.V6.vandnqrt
    hexagon_V6_vandnqrt_128B,                     // llvm.hexagon.V6.vandnqrt.128B
    hexagon_V6_vandnqrt_acc,                      // llvm.hexagon.V6.vandnqrt.acc
    hexagon_V6_vandnqrt_acc_128B,                 // llvm.hexagon.V6.vandnqrt.acc.128B
    hexagon_V6_vandqrt,                           // llvm.hexagon.V6.vandqrt
    hexagon_V6_vandqrt_128B,                      // llvm.hexagon.V6.vandqrt.128B
    hexagon_V6_vandqrt_acc,                       // llvm.hexagon.V6.vandqrt.acc
    hexagon_V6_vandqrt_acc_128B,                  // llvm.hexagon.V6.vandqrt.acc.128B
    hexagon_V6_vandvnqv,                          // llvm.hexagon.V6.vandvnqv
    hexagon_V6_vandvnqv_128B,                     // llvm.hexagon.V6.vandvnqv.128B
    hexagon_V6_vandvqv,                           // llvm.hexagon.V6.vandvqv
    hexagon_V6_vandvqv_128B,                      // llvm.hexagon.V6.vandvqv.128B
    hexagon_V6_vandvrt,                           // llvm.hexagon.V6.vandvrt
    hexagon_V6_vandvrt_128B,                      // llvm.hexagon.V6.vandvrt.128B
    hexagon_V6_vandvrt_acc,                       // llvm.hexagon.V6.vandvrt.acc
    hexagon_V6_vandvrt_acc_128B,                  // llvm.hexagon.V6.vandvrt.acc.128B
    hexagon_V6_vaslh,                             // llvm.hexagon.V6.vaslh
    hexagon_V6_vaslh_128B,                        // llvm.hexagon.V6.vaslh.128B
    hexagon_V6_vaslh_acc,                         // llvm.hexagon.V6.vaslh.acc
    hexagon_V6_vaslh_acc_128B,                    // llvm.hexagon.V6.vaslh.acc.128B
    hexagon_V6_vaslhv,                            // llvm.hexagon.V6.vaslhv
    hexagon_V6_vaslhv_128B,                       // llvm.hexagon.V6.vaslhv.128B
    hexagon_V6_vaslw,                             // llvm.hexagon.V6.vaslw
    hexagon_V6_vaslw_128B,                        // llvm.hexagon.V6.vaslw.128B
    hexagon_V6_vaslw_acc,                         // llvm.hexagon.V6.vaslw.acc
    hexagon_V6_vaslw_acc_128B,                    // llvm.hexagon.V6.vaslw.acc.128B
    hexagon_V6_vaslwv,                            // llvm.hexagon.V6.vaslwv
    hexagon_V6_vaslwv_128B,                       // llvm.hexagon.V6.vaslwv.128B
    hexagon_V6_vasrh,                             // llvm.hexagon.V6.vasrh
    hexagon_V6_vasrh_128B,                        // llvm.hexagon.V6.vasrh.128B
    hexagon_V6_vasrh_acc,                         // llvm.hexagon.V6.vasrh.acc
    hexagon_V6_vasrh_acc_128B,                    // llvm.hexagon.V6.vasrh.acc.128B
    hexagon_V6_vasrhbrndsat,                      // llvm.hexagon.V6.vasrhbrndsat
    hexagon_V6_vasrhbrndsat_128B,                 // llvm.hexagon.V6.vasrhbrndsat.128B
    hexagon_V6_vasrhbsat,                         // llvm.hexagon.V6.vasrhbsat
    hexagon_V6_vasrhbsat_128B,                    // llvm.hexagon.V6.vasrhbsat.128B
    hexagon_V6_vasrhubrndsat,                     // llvm.hexagon.V6.vasrhubrndsat
    hexagon_V6_vasrhubrndsat_128B,                // llvm.hexagon.V6.vasrhubrndsat.128B
    hexagon_V6_vasrhubsat,                        // llvm.hexagon.V6.vasrhubsat
    hexagon_V6_vasrhubsat_128B,                   // llvm.hexagon.V6.vasrhubsat.128B
    hexagon_V6_vasrhv,                            // llvm.hexagon.V6.vasrhv
    hexagon_V6_vasrhv_128B,                       // llvm.hexagon.V6.vasrhv.128B
    hexagon_V6_vasruhubrndsat,                    // llvm.hexagon.V6.vasruhubrndsat
    hexagon_V6_vasruhubrndsat_128B,               // llvm.hexagon.V6.vasruhubrndsat.128B
    hexagon_V6_vasruhubsat,                       // llvm.hexagon.V6.vasruhubsat
    hexagon_V6_vasruhubsat_128B,                  // llvm.hexagon.V6.vasruhubsat.128B
    hexagon_V6_vasruwuhrndsat,                    // llvm.hexagon.V6.vasruwuhrndsat
    hexagon_V6_vasruwuhrndsat_128B,               // llvm.hexagon.V6.vasruwuhrndsat.128B
    hexagon_V6_vasruwuhsat,                       // llvm.hexagon.V6.vasruwuhsat
    hexagon_V6_vasruwuhsat_128B,                  // llvm.hexagon.V6.vasruwuhsat.128B
    hexagon_V6_vasrw,                             // llvm.hexagon.V6.vasrw
    hexagon_V6_vasrw_128B,                        // llvm.hexagon.V6.vasrw.128B
    hexagon_V6_vasrw_acc,                         // llvm.hexagon.V6.vasrw.acc
    hexagon_V6_vasrw_acc_128B,                    // llvm.hexagon.V6.vasrw.acc.128B
    hexagon_V6_vasrwh,                            // llvm.hexagon.V6.vasrwh
    hexagon_V6_vasrwh_128B,                       // llvm.hexagon.V6.vasrwh.128B
    hexagon_V6_vasrwhrndsat,                      // llvm.hexagon.V6.vasrwhrndsat
    hexagon_V6_vasrwhrndsat_128B,                 // llvm.hexagon.V6.vasrwhrndsat.128B
    hexagon_V6_vasrwhsat,                         // llvm.hexagon.V6.vasrwhsat
    hexagon_V6_vasrwhsat_128B,                    // llvm.hexagon.V6.vasrwhsat.128B
    hexagon_V6_vasrwuhrndsat,                     // llvm.hexagon.V6.vasrwuhrndsat
    hexagon_V6_vasrwuhrndsat_128B,                // llvm.hexagon.V6.vasrwuhrndsat.128B
    hexagon_V6_vasrwuhsat,                        // llvm.hexagon.V6.vasrwuhsat
    hexagon_V6_vasrwuhsat_128B,                   // llvm.hexagon.V6.vasrwuhsat.128B
    hexagon_V6_vasrwv,                            // llvm.hexagon.V6.vasrwv
    hexagon_V6_vasrwv_128B,                       // llvm.hexagon.V6.vasrwv.128B
    hexagon_V6_vassign,                           // llvm.hexagon.V6.vassign
    hexagon_V6_vassign_128B,                      // llvm.hexagon.V6.vassign.128B
    hexagon_V6_vassignp,                          // llvm.hexagon.V6.vassignp
    hexagon_V6_vassignp_128B,                     // llvm.hexagon.V6.vassignp.128B
    hexagon_V6_vavgb,                             // llvm.hexagon.V6.vavgb
    hexagon_V6_vavgb_128B,                        // llvm.hexagon.V6.vavgb.128B
    hexagon_V6_vavgbrnd,                          // llvm.hexagon.V6.vavgbrnd
    hexagon_V6_vavgbrnd_128B,                     // llvm.hexagon.V6.vavgbrnd.128B
    hexagon_V6_vavgh,                             // llvm.hexagon.V6.vavgh
    hexagon_V6_vavgh_128B,                        // llvm.hexagon.V6.vavgh.128B
    hexagon_V6_vavghrnd,                          // llvm.hexagon.V6.vavghrnd
    hexagon_V6_vavghrnd_128B,                     // llvm.hexagon.V6.vavghrnd.128B
    hexagon_V6_vavgub,                            // llvm.hexagon.V6.vavgub
    hexagon_V6_vavgub_128B,                       // llvm.hexagon.V6.vavgub.128B
    hexagon_V6_vavgubrnd,                         // llvm.hexagon.V6.vavgubrnd
    hexagon_V6_vavgubrnd_128B,                    // llvm.hexagon.V6.vavgubrnd.128B
    hexagon_V6_vavguh,                            // llvm.hexagon.V6.vavguh
    hexagon_V6_vavguh_128B,                       // llvm.hexagon.V6.vavguh.128B
    hexagon_V6_vavguhrnd,                         // llvm.hexagon.V6.vavguhrnd
    hexagon_V6_vavguhrnd_128B,                    // llvm.hexagon.V6.vavguhrnd.128B
    hexagon_V6_vavguw,                            // llvm.hexagon.V6.vavguw
    hexagon_V6_vavguw_128B,                       // llvm.hexagon.V6.vavguw.128B
    hexagon_V6_vavguwrnd,                         // llvm.hexagon.V6.vavguwrnd
    hexagon_V6_vavguwrnd_128B,                    // llvm.hexagon.V6.vavguwrnd.128B
    hexagon_V6_vavgw,                             // llvm.hexagon.V6.vavgw
    hexagon_V6_vavgw_128B,                        // llvm.hexagon.V6.vavgw.128B
    hexagon_V6_vavgwrnd,                          // llvm.hexagon.V6.vavgwrnd
    hexagon_V6_vavgwrnd_128B,                     // llvm.hexagon.V6.vavgwrnd.128B
    hexagon_V6_vcl0h,                             // llvm.hexagon.V6.vcl0h
    hexagon_V6_vcl0h_128B,                        // llvm.hexagon.V6.vcl0h.128B
    hexagon_V6_vcl0w,                             // llvm.hexagon.V6.vcl0w
    hexagon_V6_vcl0w_128B,                        // llvm.hexagon.V6.vcl0w.128B
    hexagon_V6_vcombine,                          // llvm.hexagon.V6.vcombine
    hexagon_V6_vcombine_128B,                     // llvm.hexagon.V6.vcombine.128B
    hexagon_V6_vd0,                               // llvm.hexagon.V6.vd0
    hexagon_V6_vd0_128B,                          // llvm.hexagon.V6.vd0.128B
    hexagon_V6_vdd0,                              // llvm.hexagon.V6.vdd0
    hexagon_V6_vdd0_128B,                         // llvm.hexagon.V6.vdd0.128B
    hexagon_V6_vdealb,                            // llvm.hexagon.V6.vdealb
    hexagon_V6_vdealb_128B,                       // llvm.hexagon.V6.vdealb.128B
    hexagon_V6_vdealb4w,                          // llvm.hexagon.V6.vdealb4w
    hexagon_V6_vdealb4w_128B,                     // llvm.hexagon.V6.vdealb4w.128B
    hexagon_V6_vdealh,                            // llvm.hexagon.V6.vdealh
    hexagon_V6_vdealh_128B,                       // llvm.hexagon.V6.vdealh.128B
    hexagon_V6_vdealvdd,                          // llvm.hexagon.V6.vdealvdd
    hexagon_V6_vdealvdd_128B,                     // llvm.hexagon.V6.vdealvdd.128B
    hexagon_V6_vdelta,                            // llvm.hexagon.V6.vdelta
    hexagon_V6_vdelta_128B,                       // llvm.hexagon.V6.vdelta.128B
    hexagon_V6_vdmpybus,                          // llvm.hexagon.V6.vdmpybus
    hexagon_V6_vdmpybus_128B,                     // llvm.hexagon.V6.vdmpybus.128B
    hexagon_V6_vdmpybus_acc,                      // llvm.hexagon.V6.vdmpybus.acc
    hexagon_V6_vdmpybus_acc_128B,                 // llvm.hexagon.V6.vdmpybus.acc.128B
    hexagon_V6_vdmpybus_dv,                       // llvm.hexagon.V6.vdmpybus.dv
    hexagon_V6_vdmpybus_dv_128B,                  // llvm.hexagon.V6.vdmpybus.dv.128B
    hexagon_V6_vdmpybus_dv_acc,                   // llvm.hexagon.V6.vdmpybus.dv.acc
    hexagon_V6_vdmpybus_dv_acc_128B,              // llvm.hexagon.V6.vdmpybus.dv.acc.128B
    hexagon_V6_vdmpyhb,                           // llvm.hexagon.V6.vdmpyhb
    hexagon_V6_vdmpyhb_128B,                      // llvm.hexagon.V6.vdmpyhb.128B
    hexagon_V6_vdmpyhb_acc,                       // llvm.hexagon.V6.vdmpyhb.acc
    hexagon_V6_vdmpyhb_acc_128B,                  // llvm.hexagon.V6.vdmpyhb.acc.128B
    hexagon_V6_vdmpyhb_dv,                        // llvm.hexagon.V6.vdmpyhb.dv
    hexagon_V6_vdmpyhb_dv_128B,                   // llvm.hexagon.V6.vdmpyhb.dv.128B
    hexagon_V6_vdmpyhb_dv_acc,                    // llvm.hexagon.V6.vdmpyhb.dv.acc
    hexagon_V6_vdmpyhb_dv_acc_128B,               // llvm.hexagon.V6.vdmpyhb.dv.acc.128B
    hexagon_V6_vdmpyhisat,                        // llvm.hexagon.V6.vdmpyhisat
    hexagon_V6_vdmpyhisat_128B,                   // llvm.hexagon.V6.vdmpyhisat.128B
    hexagon_V6_vdmpyhisat_acc,                    // llvm.hexagon.V6.vdmpyhisat.acc
    hexagon_V6_vdmpyhisat_acc_128B,               // llvm.hexagon.V6.vdmpyhisat.acc.128B
    hexagon_V6_vdmpyhsat,                         // llvm.hexagon.V6.vdmpyhsat
    hexagon_V6_vdmpyhsat_128B,                    // llvm.hexagon.V6.vdmpyhsat.128B
    hexagon_V6_vdmpyhsat_acc,                     // llvm.hexagon.V6.vdmpyhsat.acc
    hexagon_V6_vdmpyhsat_acc_128B,                // llvm.hexagon.V6.vdmpyhsat.acc.128B
    hexagon_V6_vdmpyhsuisat,                      // llvm.hexagon.V6.vdmpyhsuisat
    hexagon_V6_vdmpyhsuisat_128B,                 // llvm.hexagon.V6.vdmpyhsuisat.128B
    hexagon_V6_vdmpyhsuisat_acc,                  // llvm.hexagon.V6.vdmpyhsuisat.acc
    hexagon_V6_vdmpyhsuisat_acc_128B,             // llvm.hexagon.V6.vdmpyhsuisat.acc.128B
    hexagon_V6_vdmpyhsusat,                       // llvm.hexagon.V6.vdmpyhsusat
    hexagon_V6_vdmpyhsusat_128B,                  // llvm.hexagon.V6.vdmpyhsusat.128B
    hexagon_V6_vdmpyhsusat_acc,                   // llvm.hexagon.V6.vdmpyhsusat.acc
    hexagon_V6_vdmpyhsusat_acc_128B,              // llvm.hexagon.V6.vdmpyhsusat.acc.128B
    hexagon_V6_vdmpyhvsat,                        // llvm.hexagon.V6.vdmpyhvsat
    hexagon_V6_vdmpyhvsat_128B,                   // llvm.hexagon.V6.vdmpyhvsat.128B
    hexagon_V6_vdmpyhvsat_acc,                    // llvm.hexagon.V6.vdmpyhvsat.acc
    hexagon_V6_vdmpyhvsat_acc_128B,               // llvm.hexagon.V6.vdmpyhvsat.acc.128B
    hexagon_V6_vdsaduh,                           // llvm.hexagon.V6.vdsaduh
    hexagon_V6_vdsaduh_128B,                      // llvm.hexagon.V6.vdsaduh.128B
    hexagon_V6_vdsaduh_acc,                       // llvm.hexagon.V6.vdsaduh.acc
    hexagon_V6_vdsaduh_acc_128B,                  // llvm.hexagon.V6.vdsaduh.acc.128B
    hexagon_V6_veqb,                              // llvm.hexagon.V6.veqb
    hexagon_V6_veqb_128B,                         // llvm.hexagon.V6.veqb.128B
    hexagon_V6_veqb_and,                          // llvm.hexagon.V6.veqb.and
    hexagon_V6_veqb_and_128B,                     // llvm.hexagon.V6.veqb.and.128B
    hexagon_V6_veqb_or,                           // llvm.hexagon.V6.veqb.or
    hexagon_V6_veqb_or_128B,                      // llvm.hexagon.V6.veqb.or.128B
    hexagon_V6_veqb_xor,                          // llvm.hexagon.V6.veqb.xor
    hexagon_V6_veqb_xor_128B,                     // llvm.hexagon.V6.veqb.xor.128B
    hexagon_V6_veqh,                              // llvm.hexagon.V6.veqh
    hexagon_V6_veqh_128B,                         // llvm.hexagon.V6.veqh.128B
    hexagon_V6_veqh_and,                          // llvm.hexagon.V6.veqh.and
    hexagon_V6_veqh_and_128B,                     // llvm.hexagon.V6.veqh.and.128B
    hexagon_V6_veqh_or,                           // llvm.hexagon.V6.veqh.or
    hexagon_V6_veqh_or_128B,                      // llvm.hexagon.V6.veqh.or.128B
    hexagon_V6_veqh_xor,                          // llvm.hexagon.V6.veqh.xor
    hexagon_V6_veqh_xor_128B,                     // llvm.hexagon.V6.veqh.xor.128B
    hexagon_V6_veqw,                              // llvm.hexagon.V6.veqw
    hexagon_V6_veqw_128B,                         // llvm.hexagon.V6.veqw.128B
    hexagon_V6_veqw_and,                          // llvm.hexagon.V6.veqw.and
    hexagon_V6_veqw_and_128B,                     // llvm.hexagon.V6.veqw.and.128B
    hexagon_V6_veqw_or,                           // llvm.hexagon.V6.veqw.or
    hexagon_V6_veqw_or_128B,                      // llvm.hexagon.V6.veqw.or.128B
    hexagon_V6_veqw_xor,                          // llvm.hexagon.V6.veqw.xor
    hexagon_V6_veqw_xor_128B,                     // llvm.hexagon.V6.veqw.xor.128B
    hexagon_V6_vgathermh,                         // llvm.hexagon.V6.vgathermh
    hexagon_V6_vgathermh_128B,                    // llvm.hexagon.V6.vgathermh.128B
    hexagon_V6_vgathermhq,                        // llvm.hexagon.V6.vgathermhq
    hexagon_V6_vgathermhq_128B,                   // llvm.hexagon.V6.vgathermhq.128B
    hexagon_V6_vgathermhw,                        // llvm.hexagon.V6.vgathermhw
    hexagon_V6_vgathermhw_128B,                   // llvm.hexagon.V6.vgathermhw.128B
    hexagon_V6_vgathermhwq,                       // llvm.hexagon.V6.vgathermhwq
    hexagon_V6_vgathermhwq_128B,                  // llvm.hexagon.V6.vgathermhwq.128B
    hexagon_V6_vgathermw,                         // llvm.hexagon.V6.vgathermw
    hexagon_V6_vgathermw_128B,                    // llvm.hexagon.V6.vgathermw.128B
    hexagon_V6_vgathermwq,                        // llvm.hexagon.V6.vgathermwq
    hexagon_V6_vgathermwq_128B,                   // llvm.hexagon.V6.vgathermwq.128B
    hexagon_V6_vgtb,                              // llvm.hexagon.V6.vgtb
    hexagon_V6_vgtb_128B,                         // llvm.hexagon.V6.vgtb.128B
    hexagon_V6_vgtb_and,                          // llvm.hexagon.V6.vgtb.and
    hexagon_V6_vgtb_and_128B,                     // llvm.hexagon.V6.vgtb.and.128B
    hexagon_V6_vgtb_or,                           // llvm.hexagon.V6.vgtb.or
    hexagon_V6_vgtb_or_128B,                      // llvm.hexagon.V6.vgtb.or.128B
    hexagon_V6_vgtb_xor,                          // llvm.hexagon.V6.vgtb.xor
    hexagon_V6_vgtb_xor_128B,                     // llvm.hexagon.V6.vgtb.xor.128B
    hexagon_V6_vgth,                              // llvm.hexagon.V6.vgth
    hexagon_V6_vgth_128B,                         // llvm.hexagon.V6.vgth.128B
    hexagon_V6_vgth_and,                          // llvm.hexagon.V6.vgth.and
    hexagon_V6_vgth_and_128B,                     // llvm.hexagon.V6.vgth.and.128B
    hexagon_V6_vgth_or,                           // llvm.hexagon.V6.vgth.or
    hexagon_V6_vgth_or_128B,                      // llvm.hexagon.V6.vgth.or.128B
    hexagon_V6_vgth_xor,                          // llvm.hexagon.V6.vgth.xor
    hexagon_V6_vgth_xor_128B,                     // llvm.hexagon.V6.vgth.xor.128B
    hexagon_V6_vgtub,                             // llvm.hexagon.V6.vgtub
    hexagon_V6_vgtub_128B,                        // llvm.hexagon.V6.vgtub.128B
    hexagon_V6_vgtub_and,                         // llvm.hexagon.V6.vgtub.and
    hexagon_V6_vgtub_and_128B,                    // llvm.hexagon.V6.vgtub.and.128B
    hexagon_V6_vgtub_or,                          // llvm.hexagon.V6.vgtub.or
    hexagon_V6_vgtub_or_128B,                     // llvm.hexagon.V6.vgtub.or.128B
    hexagon_V6_vgtub_xor,                         // llvm.hexagon.V6.vgtub.xor
    hexagon_V6_vgtub_xor_128B,                    // llvm.hexagon.V6.vgtub.xor.128B
    hexagon_V6_vgtuh,                             // llvm.hexagon.V6.vgtuh
    hexagon_V6_vgtuh_128B,                        // llvm.hexagon.V6.vgtuh.128B
    hexagon_V6_vgtuh_and,                         // llvm.hexagon.V6.vgtuh.and
    hexagon_V6_vgtuh_and_128B,                    // llvm.hexagon.V6.vgtuh.and.128B
    hexagon_V6_vgtuh_or,                          // llvm.hexagon.V6.vgtuh.or
    hexagon_V6_vgtuh_or_128B,                     // llvm.hexagon.V6.vgtuh.or.128B
    hexagon_V6_vgtuh_xor,                         // llvm.hexagon.V6.vgtuh.xor
    hexagon_V6_vgtuh_xor_128B,                    // llvm.hexagon.V6.vgtuh.xor.128B
    hexagon_V6_vgtuw,                             // llvm.hexagon.V6.vgtuw
    hexagon_V6_vgtuw_128B,                        // llvm.hexagon.V6.vgtuw.128B
    hexagon_V6_vgtuw_and,                         // llvm.hexagon.V6.vgtuw.and
    hexagon_V6_vgtuw_and_128B,                    // llvm.hexagon.V6.vgtuw.and.128B
    hexagon_V6_vgtuw_or,                          // llvm.hexagon.V6.vgtuw.or
    hexagon_V6_vgtuw_or_128B,                     // llvm.hexagon.V6.vgtuw.or.128B
    hexagon_V6_vgtuw_xor,                         // llvm.hexagon.V6.vgtuw.xor
    hexagon_V6_vgtuw_xor_128B,                    // llvm.hexagon.V6.vgtuw.xor.128B
    hexagon_V6_vgtw,                              // llvm.hexagon.V6.vgtw
    hexagon_V6_vgtw_128B,                         // llvm.hexagon.V6.vgtw.128B
    hexagon_V6_vgtw_and,                          // llvm.hexagon.V6.vgtw.and
    hexagon_V6_vgtw_and_128B,                     // llvm.hexagon.V6.vgtw.and.128B
    hexagon_V6_vgtw_or,                           // llvm.hexagon.V6.vgtw.or
    hexagon_V6_vgtw_or_128B,                      // llvm.hexagon.V6.vgtw.or.128B
    hexagon_V6_vgtw_xor,                          // llvm.hexagon.V6.vgtw.xor
    hexagon_V6_vgtw_xor_128B,                     // llvm.hexagon.V6.vgtw.xor.128B
    hexagon_V6_vinsertwr,                         // llvm.hexagon.V6.vinsertwr
    hexagon_V6_vinsertwr_128B,                    // llvm.hexagon.V6.vinsertwr.128B
    hexagon_V6_vlalignb,                          // llvm.hexagon.V6.vlalignb
    hexagon_V6_vlalignb_128B,                     // llvm.hexagon.V6.vlalignb.128B
    hexagon_V6_vlalignbi,                         // llvm.hexagon.V6.vlalignbi
    hexagon_V6_vlalignbi_128B,                    // llvm.hexagon.V6.vlalignbi.128B
    hexagon_V6_vlsrb,                             // llvm.hexagon.V6.vlsrb
    hexagon_V6_vlsrb_128B,                        // llvm.hexagon.V6.vlsrb.128B
    hexagon_V6_vlsrh,                             // llvm.hexagon.V6.vlsrh
    hexagon_V6_vlsrh_128B,                        // llvm.hexagon.V6.vlsrh.128B
    hexagon_V6_vlsrhv,                            // llvm.hexagon.V6.vlsrhv
    hexagon_V6_vlsrhv_128B,                       // llvm.hexagon.V6.vlsrhv.128B
    hexagon_V6_vlsrw,                             // llvm.hexagon.V6.vlsrw
    hexagon_V6_vlsrw_128B,                        // llvm.hexagon.V6.vlsrw.128B
    hexagon_V6_vlsrwv,                            // llvm.hexagon.V6.vlsrwv
    hexagon_V6_vlsrwv_128B,                       // llvm.hexagon.V6.vlsrwv.128B
    hexagon_V6_vlut4,                             // llvm.hexagon.V6.vlut4
    hexagon_V6_vlut4_128B,                        // llvm.hexagon.V6.vlut4.128B
    hexagon_V6_vlutvvb,                           // llvm.hexagon.V6.vlutvvb
    hexagon_V6_vlutvvb_128B,                      // llvm.hexagon.V6.vlutvvb.128B
    hexagon_V6_vlutvvb_nm,                        // llvm.hexagon.V6.vlutvvb.nm
    hexagon_V6_vlutvvb_nm_128B,                   // llvm.hexagon.V6.vlutvvb.nm.128B
    hexagon_V6_vlutvvb_oracc,                     // llvm.hexagon.V6.vlutvvb.oracc
    hexagon_V6_vlutvvb_oracc_128B,                // llvm.hexagon.V6.vlutvvb.oracc.128B
    hexagon_V6_vlutvvb_oracci,                    // llvm.hexagon.V6.vlutvvb.oracci
    hexagon_V6_vlutvvb_oracci_128B,               // llvm.hexagon.V6.vlutvvb.oracci.128B
    hexagon_V6_vlutvvbi,                          // llvm.hexagon.V6.vlutvvbi
    hexagon_V6_vlutvvbi_128B,                     // llvm.hexagon.V6.vlutvvbi.128B
    hexagon_V6_vlutvwh,                           // llvm.hexagon.V6.vlutvwh
    hexagon_V6_vlutvwh_128B,                      // llvm.hexagon.V6.vlutvwh.128B
    hexagon_V6_vlutvwh_nm,                        // llvm.hexagon.V6.vlutvwh.nm
    hexagon_V6_vlutvwh_nm_128B,                   // llvm.hexagon.V6.vlutvwh.nm.128B
    hexagon_V6_vlutvwh_oracc,                     // llvm.hexagon.V6.vlutvwh.oracc
    hexagon_V6_vlutvwh_oracc_128B,                // llvm.hexagon.V6.vlutvwh.oracc.128B
    hexagon_V6_vlutvwh_oracci,                    // llvm.hexagon.V6.vlutvwh.oracci
    hexagon_V6_vlutvwh_oracci_128B,               // llvm.hexagon.V6.vlutvwh.oracci.128B
    hexagon_V6_vlutvwhi,                          // llvm.hexagon.V6.vlutvwhi
    hexagon_V6_vlutvwhi_128B,                     // llvm.hexagon.V6.vlutvwhi.128B
    hexagon_V6_vmaskedstorenq,                    // llvm.hexagon.V6.vmaskedstorenq
    hexagon_V6_vmaskedstorenq_128B,               // llvm.hexagon.V6.vmaskedstorenq.128B
    hexagon_V6_vmaskedstorentnq,                  // llvm.hexagon.V6.vmaskedstorentnq
    hexagon_V6_vmaskedstorentnq_128B,             // llvm.hexagon.V6.vmaskedstorentnq.128B
    hexagon_V6_vmaskedstorentq,                   // llvm.hexagon.V6.vmaskedstorentq
    hexagon_V6_vmaskedstorentq_128B,              // llvm.hexagon.V6.vmaskedstorentq.128B
    hexagon_V6_vmaskedstoreq,                     // llvm.hexagon.V6.vmaskedstoreq
    hexagon_V6_vmaskedstoreq_128B,                // llvm.hexagon.V6.vmaskedstoreq.128B
    hexagon_V6_vmaxb,                             // llvm.hexagon.V6.vmaxb
    hexagon_V6_vmaxb_128B,                        // llvm.hexagon.V6.vmaxb.128B
    hexagon_V6_vmaxh,                             // llvm.hexagon.V6.vmaxh
    hexagon_V6_vmaxh_128B,                        // llvm.hexagon.V6.vmaxh.128B
    hexagon_V6_vmaxub,                            // llvm.hexagon.V6.vmaxub
    hexagon_V6_vmaxub_128B,                       // llvm.hexagon.V6.vmaxub.128B
    hexagon_V6_vmaxuh,                            // llvm.hexagon.V6.vmaxuh
    hexagon_V6_vmaxuh_128B,                       // llvm.hexagon.V6.vmaxuh.128B
    hexagon_V6_vmaxw,                             // llvm.hexagon.V6.vmaxw
    hexagon_V6_vmaxw_128B,                        // llvm.hexagon.V6.vmaxw.128B
    hexagon_V6_vminb,                             // llvm.hexagon.V6.vminb
    hexagon_V6_vminb_128B,                        // llvm.hexagon.V6.vminb.128B
    hexagon_V6_vminh,                             // llvm.hexagon.V6.vminh
    hexagon_V6_vminh_128B,                        // llvm.hexagon.V6.vminh.128B
    hexagon_V6_vminub,                            // llvm.hexagon.V6.vminub
    hexagon_V6_vminub_128B,                       // llvm.hexagon.V6.vminub.128B
    hexagon_V6_vminuh,                            // llvm.hexagon.V6.vminuh
    hexagon_V6_vminuh_128B,                       // llvm.hexagon.V6.vminuh.128B
    hexagon_V6_vminw,                             // llvm.hexagon.V6.vminw
    hexagon_V6_vminw_128B,                        // llvm.hexagon.V6.vminw.128B
    hexagon_V6_vmpabus,                           // llvm.hexagon.V6.vmpabus
    hexagon_V6_vmpabus_128B,                      // llvm.hexagon.V6.vmpabus.128B
    hexagon_V6_vmpabus_acc,                       // llvm.hexagon.V6.vmpabus.acc
    hexagon_V6_vmpabus_acc_128B,                  // llvm.hexagon.V6.vmpabus.acc.128B
    hexagon_V6_vmpabusv,                          // llvm.hexagon.V6.vmpabusv
    hexagon_V6_vmpabusv_128B,                     // llvm.hexagon.V6.vmpabusv.128B
    hexagon_V6_vmpabuu,                           // llvm.hexagon.V6.vmpabuu
    hexagon_V6_vmpabuu_128B,                      // llvm.hexagon.V6.vmpabuu.128B
    hexagon_V6_vmpabuu_acc,                       // llvm.hexagon.V6.vmpabuu.acc
    hexagon_V6_vmpabuu_acc_128B,                  // llvm.hexagon.V6.vmpabuu.acc.128B
    hexagon_V6_vmpabuuv,                          // llvm.hexagon.V6.vmpabuuv
    hexagon_V6_vmpabuuv_128B,                     // llvm.hexagon.V6.vmpabuuv.128B
    hexagon_V6_vmpahb,                            // llvm.hexagon.V6.vmpahb
    hexagon_V6_vmpahb_128B,                       // llvm.hexagon.V6.vmpahb.128B
    hexagon_V6_vmpahb_acc,                        // llvm.hexagon.V6.vmpahb.acc
    hexagon_V6_vmpahb_acc_128B,                   // llvm.hexagon.V6.vmpahb.acc.128B
    hexagon_V6_vmpahhsat,                         // llvm.hexagon.V6.vmpahhsat
    hexagon_V6_vmpahhsat_128B,                    // llvm.hexagon.V6.vmpahhsat.128B
    hexagon_V6_vmpauhb,                           // llvm.hexagon.V6.vmpauhb
    hexagon_V6_vmpauhb_128B,                      // llvm.hexagon.V6.vmpauhb.128B
    hexagon_V6_vmpauhb_acc,                       // llvm.hexagon.V6.vmpauhb.acc
    hexagon_V6_vmpauhb_acc_128B,                  // llvm.hexagon.V6.vmpauhb.acc.128B
    hexagon_V6_vmpauhuhsat,                       // llvm.hexagon.V6.vmpauhuhsat
    hexagon_V6_vmpauhuhsat_128B,                  // llvm.hexagon.V6.vmpauhuhsat.128B
    hexagon_V6_vmpsuhuhsat,                       // llvm.hexagon.V6.vmpsuhuhsat
    hexagon_V6_vmpsuhuhsat_128B,                  // llvm.hexagon.V6.vmpsuhuhsat.128B
    hexagon_V6_vmpybus,                           // llvm.hexagon.V6.vmpybus
    hexagon_V6_vmpybus_128B,                      // llvm.hexagon.V6.vmpybus.128B
    hexagon_V6_vmpybus_acc,                       // llvm.hexagon.V6.vmpybus.acc
    hexagon_V6_vmpybus_acc_128B,                  // llvm.hexagon.V6.vmpybus.acc.128B
    hexagon_V6_vmpybusv,                          // llvm.hexagon.V6.vmpybusv
    hexagon_V6_vmpybusv_128B,                     // llvm.hexagon.V6.vmpybusv.128B
    hexagon_V6_vmpybusv_acc,                      // llvm.hexagon.V6.vmpybusv.acc
    hexagon_V6_vmpybusv_acc_128B,                 // llvm.hexagon.V6.vmpybusv.acc.128B
    hexagon_V6_vmpybv,                            // llvm.hexagon.V6.vmpybv
    hexagon_V6_vmpybv_128B,                       // llvm.hexagon.V6.vmpybv.128B
    hexagon_V6_vmpybv_acc,                        // llvm.hexagon.V6.vmpybv.acc
    hexagon_V6_vmpybv_acc_128B,                   // llvm.hexagon.V6.vmpybv.acc.128B
    hexagon_V6_vmpyewuh,                          // llvm.hexagon.V6.vmpyewuh
    hexagon_V6_vmpyewuh_128B,                     // llvm.hexagon.V6.vmpyewuh.128B
    hexagon_V6_vmpyewuh_64,                       // llvm.hexagon.V6.vmpyewuh.64
    hexagon_V6_vmpyewuh_64_128B,                  // llvm.hexagon.V6.vmpyewuh.64.128B
    hexagon_V6_vmpyh,                             // llvm.hexagon.V6.vmpyh
    hexagon_V6_vmpyh_128B,                        // llvm.hexagon.V6.vmpyh.128B
    hexagon_V6_vmpyh_acc,                         // llvm.hexagon.V6.vmpyh.acc
    hexagon_V6_vmpyh_acc_128B,                    // llvm.hexagon.V6.vmpyh.acc.128B
    hexagon_V6_vmpyhsat_acc,                      // llvm.hexagon.V6.vmpyhsat.acc
    hexagon_V6_vmpyhsat_acc_128B,                 // llvm.hexagon.V6.vmpyhsat.acc.128B
    hexagon_V6_vmpyhsrs,                          // llvm.hexagon.V6.vmpyhsrs
    hexagon_V6_vmpyhsrs_128B,                     // llvm.hexagon.V6.vmpyhsrs.128B
    hexagon_V6_vmpyhss,                           // llvm.hexagon.V6.vmpyhss
    hexagon_V6_vmpyhss_128B,                      // llvm.hexagon.V6.vmpyhss.128B
    hexagon_V6_vmpyhus,                           // llvm.hexagon.V6.vmpyhus
    hexagon_V6_vmpyhus_128B,                      // llvm.hexagon.V6.vmpyhus.128B
    hexagon_V6_vmpyhus_acc,                       // llvm.hexagon.V6.vmpyhus.acc
    hexagon_V6_vmpyhus_acc_128B,                  // llvm.hexagon.V6.vmpyhus.acc.128B
    hexagon_V6_vmpyhv,                            // llvm.hexagon.V6.vmpyhv
    hexagon_V6_vmpyhv_128B,                       // llvm.hexagon.V6.vmpyhv.128B
    hexagon_V6_vmpyhv_acc,                        // llvm.hexagon.V6.vmpyhv.acc
    hexagon_V6_vmpyhv_acc_128B,                   // llvm.hexagon.V6.vmpyhv.acc.128B
    hexagon_V6_vmpyhvsrs,                         // llvm.hexagon.V6.vmpyhvsrs
    hexagon_V6_vmpyhvsrs_128B,                    // llvm.hexagon.V6.vmpyhvsrs.128B
    hexagon_V6_vmpyieoh,                          // llvm.hexagon.V6.vmpyieoh
    hexagon_V6_vmpyieoh_128B,                     // llvm.hexagon.V6.vmpyieoh.128B
    hexagon_V6_vmpyiewh_acc,                      // llvm.hexagon.V6.vmpyiewh.acc
    hexagon_V6_vmpyiewh_acc_128B,                 // llvm.hexagon.V6.vmpyiewh.acc.128B
    hexagon_V6_vmpyiewuh,                         // llvm.hexagon.V6.vmpyiewuh
    hexagon_V6_vmpyiewuh_128B,                    // llvm.hexagon.V6.vmpyiewuh.128B
    hexagon_V6_vmpyiewuh_acc,                     // llvm.hexagon.V6.vmpyiewuh.acc
    hexagon_V6_vmpyiewuh_acc_128B,                // llvm.hexagon.V6.vmpyiewuh.acc.128B
    hexagon_V6_vmpyih,                            // llvm.hexagon.V6.vmpyih
    hexagon_V6_vmpyih_128B,                       // llvm.hexagon.V6.vmpyih.128B
    hexagon_V6_vmpyih_acc,                        // llvm.hexagon.V6.vmpyih.acc
    hexagon_V6_vmpyih_acc_128B,                   // llvm.hexagon.V6.vmpyih.acc.128B
    hexagon_V6_vmpyihb,                           // llvm.hexagon.V6.vmpyihb
    hexagon_V6_vmpyihb_128B,                      // llvm.hexagon.V6.vmpyihb.128B
    hexagon_V6_vmpyihb_acc,                       // llvm.hexagon.V6.vmpyihb.acc
    hexagon_V6_vmpyihb_acc_128B,                  // llvm.hexagon.V6.vmpyihb.acc.128B
    hexagon_V6_vmpyiowh,                          // llvm.hexagon.V6.vmpyiowh
    hexagon_V6_vmpyiowh_128B,                     // llvm.hexagon.V6.vmpyiowh.128B
    hexagon_V6_vmpyiwb,                           // llvm.hexagon.V6.vmpyiwb
    hexagon_V6_vmpyiwb_128B,                      // llvm.hexagon.V6.vmpyiwb.128B
    hexagon_V6_vmpyiwb_acc,                       // llvm.hexagon.V6.vmpyiwb.acc
    hexagon_V6_vmpyiwb_acc_128B,                  // llvm.hexagon.V6.vmpyiwb.acc.128B
    hexagon_V6_vmpyiwh,                           // llvm.hexagon.V6.vmpyiwh
    hexagon_V6_vmpyiwh_128B,                      // llvm.hexagon.V6.vmpyiwh.128B
    hexagon_V6_vmpyiwh_acc,                       // llvm.hexagon.V6.vmpyiwh.acc
    hexagon_V6_vmpyiwh_acc_128B,                  // llvm.hexagon.V6.vmpyiwh.acc.128B
    hexagon_V6_vmpyiwub,                          // llvm.hexagon.V6.vmpyiwub
    hexagon_V6_vmpyiwub_128B,                     // llvm.hexagon.V6.vmpyiwub.128B
    hexagon_V6_vmpyiwub_acc,                      // llvm.hexagon.V6.vmpyiwub.acc
    hexagon_V6_vmpyiwub_acc_128B,                 // llvm.hexagon.V6.vmpyiwub.acc.128B
    hexagon_V6_vmpyowh,                           // llvm.hexagon.V6.vmpyowh
    hexagon_V6_vmpyowh_128B,                      // llvm.hexagon.V6.vmpyowh.128B
    hexagon_V6_vmpyowh_64_acc,                    // llvm.hexagon.V6.vmpyowh.64.acc
    hexagon_V6_vmpyowh_64_acc_128B,               // llvm.hexagon.V6.vmpyowh.64.acc.128B
    hexagon_V6_vmpyowh_rnd,                       // llvm.hexagon.V6.vmpyowh.rnd
    hexagon_V6_vmpyowh_rnd_128B,                  // llvm.hexagon.V6.vmpyowh.rnd.128B
    hexagon_V6_vmpyowh_rnd_sacc,                  // llvm.hexagon.V6.vmpyowh.rnd.sacc
    hexagon_V6_vmpyowh_rnd_sacc_128B,             // llvm.hexagon.V6.vmpyowh.rnd.sacc.128B
    hexagon_V6_vmpyowh_sacc,                      // llvm.hexagon.V6.vmpyowh.sacc
    hexagon_V6_vmpyowh_sacc_128B,                 // llvm.hexagon.V6.vmpyowh.sacc.128B
    hexagon_V6_vmpyub,                            // llvm.hexagon.V6.vmpyub
    hexagon_V6_vmpyub_128B,                       // llvm.hexagon.V6.vmpyub.128B
    hexagon_V6_vmpyub_acc,                        // llvm.hexagon.V6.vmpyub.acc
    hexagon_V6_vmpyub_acc_128B,                   // llvm.hexagon.V6.vmpyub.acc.128B
    hexagon_V6_vmpyubv,                           // llvm.hexagon.V6.vmpyubv
    hexagon_V6_vmpyubv_128B,                      // llvm.hexagon.V6.vmpyubv.128B
    hexagon_V6_vmpyubv_acc,                       // llvm.hexagon.V6.vmpyubv.acc
    hexagon_V6_vmpyubv_acc_128B,                  // llvm.hexagon.V6.vmpyubv.acc.128B
    hexagon_V6_vmpyuh,                            // llvm.hexagon.V6.vmpyuh
    hexagon_V6_vmpyuh_128B,                       // llvm.hexagon.V6.vmpyuh.128B
    hexagon_V6_vmpyuh_acc,                        // llvm.hexagon.V6.vmpyuh.acc
    hexagon_V6_vmpyuh_acc_128B,                   // llvm.hexagon.V6.vmpyuh.acc.128B
    hexagon_V6_vmpyuhe,                           // llvm.hexagon.V6.vmpyuhe
    hexagon_V6_vmpyuhe_128B,                      // llvm.hexagon.V6.vmpyuhe.128B
    hexagon_V6_vmpyuhe_acc,                       // llvm.hexagon.V6.vmpyuhe.acc
    hexagon_V6_vmpyuhe_acc_128B,                  // llvm.hexagon.V6.vmpyuhe.acc.128B
    hexagon_V6_vmpyuhv,                           // llvm.hexagon.V6.vmpyuhv
    hexagon_V6_vmpyuhv_128B,                      // llvm.hexagon.V6.vmpyuhv.128B
    hexagon_V6_vmpyuhv_acc,                       // llvm.hexagon.V6.vmpyuhv.acc
    hexagon_V6_vmpyuhv_acc_128B,                  // llvm.hexagon.V6.vmpyuhv.acc.128B
    hexagon_V6_vmux,                              // llvm.hexagon.V6.vmux
    hexagon_V6_vmux_128B,                         // llvm.hexagon.V6.vmux.128B
    hexagon_V6_vnavgb,                            // llvm.hexagon.V6.vnavgb
    hexagon_V6_vnavgb_128B,                       // llvm.hexagon.V6.vnavgb.128B
    hexagon_V6_vnavgh,                            // llvm.hexagon.V6.vnavgh
    hexagon_V6_vnavgh_128B,                       // llvm.hexagon.V6.vnavgh.128B
    hexagon_V6_vnavgub,                           // llvm.hexagon.V6.vnavgub
    hexagon_V6_vnavgub_128B,                      // llvm.hexagon.V6.vnavgub.128B
    hexagon_V6_vnavgw,                            // llvm.hexagon.V6.vnavgw
    hexagon_V6_vnavgw_128B,                       // llvm.hexagon.V6.vnavgw.128B
    hexagon_V6_vnormamth,                         // llvm.hexagon.V6.vnormamth
    hexagon_V6_vnormamth_128B,                    // llvm.hexagon.V6.vnormamth.128B
    hexagon_V6_vnormamtw,                         // llvm.hexagon.V6.vnormamtw
    hexagon_V6_vnormamtw_128B,                    // llvm.hexagon.V6.vnormamtw.128B
    hexagon_V6_vnot,                              // llvm.hexagon.V6.vnot
    hexagon_V6_vnot_128B,                         // llvm.hexagon.V6.vnot.128B
    hexagon_V6_vor,                               // llvm.hexagon.V6.vor
    hexagon_V6_vor_128B,                          // llvm.hexagon.V6.vor.128B
    hexagon_V6_vpackeb,                           // llvm.hexagon.V6.vpackeb
    hexagon_V6_vpackeb_128B,                      // llvm.hexagon.V6.vpackeb.128B
    hexagon_V6_vpackeh,                           // llvm.hexagon.V6.vpackeh
    hexagon_V6_vpackeh_128B,                      // llvm.hexagon.V6.vpackeh.128B
    hexagon_V6_vpackhb_sat,                       // llvm.hexagon.V6.vpackhb.sat
    hexagon_V6_vpackhb_sat_128B,                  // llvm.hexagon.V6.vpackhb.sat.128B
    hexagon_V6_vpackhub_sat,                      // llvm.hexagon.V6.vpackhub.sat
    hexagon_V6_vpackhub_sat_128B,                 // llvm.hexagon.V6.vpackhub.sat.128B
    hexagon_V6_vpackob,                           // llvm.hexagon.V6.vpackob
    hexagon_V6_vpackob_128B,                      // llvm.hexagon.V6.vpackob.128B
    hexagon_V6_vpackoh,                           // llvm.hexagon.V6.vpackoh
    hexagon_V6_vpackoh_128B,                      // llvm.hexagon.V6.vpackoh.128B
    hexagon_V6_vpackwh_sat,                       // llvm.hexagon.V6.vpackwh.sat
    hexagon_V6_vpackwh_sat_128B,                  // llvm.hexagon.V6.vpackwh.sat.128B
    hexagon_V6_vpackwuh_sat,                      // llvm.hexagon.V6.vpackwuh.sat
    hexagon_V6_vpackwuh_sat_128B,                 // llvm.hexagon.V6.vpackwuh.sat.128B
    hexagon_V6_vpopcounth,                        // llvm.hexagon.V6.vpopcounth
    hexagon_V6_vpopcounth_128B,                   // llvm.hexagon.V6.vpopcounth.128B
    hexagon_V6_vprefixqb,                         // llvm.hexagon.V6.vprefixqb
    hexagon_V6_vprefixqb_128B,                    // llvm.hexagon.V6.vprefixqb.128B
    hexagon_V6_vprefixqh,                         // llvm.hexagon.V6.vprefixqh
    hexagon_V6_vprefixqh_128B,                    // llvm.hexagon.V6.vprefixqh.128B
    hexagon_V6_vprefixqw,                         // llvm.hexagon.V6.vprefixqw
    hexagon_V6_vprefixqw_128B,                    // llvm.hexagon.V6.vprefixqw.128B
    hexagon_V6_vrdelta,                           // llvm.hexagon.V6.vrdelta
    hexagon_V6_vrdelta_128B,                      // llvm.hexagon.V6.vrdelta.128B
    hexagon_V6_vrmpybub_rtt,                      // llvm.hexagon.V6.vrmpybub.rtt
    hexagon_V6_vrmpybub_rtt_128B,                 // llvm.hexagon.V6.vrmpybub.rtt.128B
    hexagon_V6_vrmpybub_rtt_acc,                  // llvm.hexagon.V6.vrmpybub.rtt.acc
    hexagon_V6_vrmpybub_rtt_acc_128B,             // llvm.hexagon.V6.vrmpybub.rtt.acc.128B
    hexagon_V6_vrmpybus,                          // llvm.hexagon.V6.vrmpybus
    hexagon_V6_vrmpybus_128B,                     // llvm.hexagon.V6.vrmpybus.128B
    hexagon_V6_vrmpybus_acc,                      // llvm.hexagon.V6.vrmpybus.acc
    hexagon_V6_vrmpybus_acc_128B,                 // llvm.hexagon.V6.vrmpybus.acc.128B
    hexagon_V6_vrmpybusi,                         // llvm.hexagon.V6.vrmpybusi
    hexagon_V6_vrmpybusi_128B,                    // llvm.hexagon.V6.vrmpybusi.128B
    hexagon_V6_vrmpybusi_acc,                     // llvm.hexagon.V6.vrmpybusi.acc
    hexagon_V6_vrmpybusi_acc_128B,                // llvm.hexagon.V6.vrmpybusi.acc.128B
    hexagon_V6_vrmpybusv,                         // llvm.hexagon.V6.vrmpybusv
    hexagon_V6_vrmpybusv_128B,                    // llvm.hexagon.V6.vrmpybusv.128B
    hexagon_V6_vrmpybusv_acc,                     // llvm.hexagon.V6.vrmpybusv.acc
    hexagon_V6_vrmpybusv_acc_128B,                // llvm.hexagon.V6.vrmpybusv.acc.128B
    hexagon_V6_vrmpybv,                           // llvm.hexagon.V6.vrmpybv
    hexagon_V6_vrmpybv_128B,                      // llvm.hexagon.V6.vrmpybv.128B
    hexagon_V6_vrmpybv_acc,                       // llvm.hexagon.V6.vrmpybv.acc
    hexagon_V6_vrmpybv_acc_128B,                  // llvm.hexagon.V6.vrmpybv.acc.128B
    hexagon_V6_vrmpyub,                           // llvm.hexagon.V6.vrmpyub
    hexagon_V6_vrmpyub_128B,                      // llvm.hexagon.V6.vrmpyub.128B
    hexagon_V6_vrmpyub_acc,                       // llvm.hexagon.V6.vrmpyub.acc
    hexagon_V6_vrmpyub_acc_128B,                  // llvm.hexagon.V6.vrmpyub.acc.128B
    hexagon_V6_vrmpyub_rtt,                       // llvm.hexagon.V6.vrmpyub.rtt
    hexagon_V6_vrmpyub_rtt_128B,                  // llvm.hexagon.V6.vrmpyub.rtt.128B
    hexagon_V6_vrmpyub_rtt_acc,                   // llvm.hexagon.V6.vrmpyub.rtt.acc
    hexagon_V6_vrmpyub_rtt_acc_128B,              // llvm.hexagon.V6.vrmpyub.rtt.acc.128B
    hexagon_V6_vrmpyubi,                          // llvm.hexagon.V6.vrmpyubi
    hexagon_V6_vrmpyubi_128B,                     // llvm.hexagon.V6.vrmpyubi.128B
    hexagon_V6_vrmpyubi_acc,                      // llvm.hexagon.V6.vrmpyubi.acc
    hexagon_V6_vrmpyubi_acc_128B,                 // llvm.hexagon.V6.vrmpyubi.acc.128B
    hexagon_V6_vrmpyubv,                          // llvm.hexagon.V6.vrmpyubv
    hexagon_V6_vrmpyubv_128B,                     // llvm.hexagon.V6.vrmpyubv.128B
    hexagon_V6_vrmpyubv_acc,                      // llvm.hexagon.V6.vrmpyubv.acc
    hexagon_V6_vrmpyubv_acc_128B,                 // llvm.hexagon.V6.vrmpyubv.acc.128B
    hexagon_V6_vror,                              // llvm.hexagon.V6.vror
    hexagon_V6_vror_128B,                         // llvm.hexagon.V6.vror.128B
    hexagon_V6_vroundhb,                          // llvm.hexagon.V6.vroundhb
    hexagon_V6_vroundhb_128B,                     // llvm.hexagon.V6.vroundhb.128B
    hexagon_V6_vroundhub,                         // llvm.hexagon.V6.vroundhub
    hexagon_V6_vroundhub_128B,                    // llvm.hexagon.V6.vroundhub.128B
    hexagon_V6_vrounduhub,                        // llvm.hexagon.V6.vrounduhub
    hexagon_V6_vrounduhub_128B,                   // llvm.hexagon.V6.vrounduhub.128B
    hexagon_V6_vrounduwuh,                        // llvm.hexagon.V6.vrounduwuh
    hexagon_V6_vrounduwuh_128B,                   // llvm.hexagon.V6.vrounduwuh.128B
    hexagon_V6_vroundwh,                          // llvm.hexagon.V6.vroundwh
    hexagon_V6_vroundwh_128B,                     // llvm.hexagon.V6.vroundwh.128B
    hexagon_V6_vroundwuh,                         // llvm.hexagon.V6.vroundwuh
    hexagon_V6_vroundwuh_128B,                    // llvm.hexagon.V6.vroundwuh.128B
    hexagon_V6_vrsadubi,                          // llvm.hexagon.V6.vrsadubi
    hexagon_V6_vrsadubi_128B,                     // llvm.hexagon.V6.vrsadubi.128B
    hexagon_V6_vrsadubi_acc,                      // llvm.hexagon.V6.vrsadubi.acc
    hexagon_V6_vrsadubi_acc_128B,                 // llvm.hexagon.V6.vrsadubi.acc.128B
    hexagon_V6_vsathub,                           // llvm.hexagon.V6.vsathub
    hexagon_V6_vsathub_128B,                      // llvm.hexagon.V6.vsathub.128B
    hexagon_V6_vsatuwuh,                          // llvm.hexagon.V6.vsatuwuh
    hexagon_V6_vsatuwuh_128B,                     // llvm.hexagon.V6.vsatuwuh.128B
    hexagon_V6_vsatwh,                            // llvm.hexagon.V6.vsatwh
    hexagon_V6_vsatwh_128B,                       // llvm.hexagon.V6.vsatwh.128B
    hexagon_V6_vsb,                               // llvm.hexagon.V6.vsb
    hexagon_V6_vsb_128B,                          // llvm.hexagon.V6.vsb.128B
    hexagon_V6_vscattermh,                        // llvm.hexagon.V6.vscattermh
    hexagon_V6_vscattermh_128B,                   // llvm.hexagon.V6.vscattermh.128B
    hexagon_V6_vscattermh_add,                    // llvm.hexagon.V6.vscattermh.add
    hexagon_V6_vscattermh_add_128B,               // llvm.hexagon.V6.vscattermh.add.128B
    hexagon_V6_vscattermhq,                       // llvm.hexagon.V6.vscattermhq
    hexagon_V6_vscattermhq_128B,                  // llvm.hexagon.V6.vscattermhq.128B
    hexagon_V6_vscattermhw,                       // llvm.hexagon.V6.vscattermhw
    hexagon_V6_vscattermhw_128B,                  // llvm.hexagon.V6.vscattermhw.128B
    hexagon_V6_vscattermhw_add,                   // llvm.hexagon.V6.vscattermhw.add
    hexagon_V6_vscattermhw_add_128B,              // llvm.hexagon.V6.vscattermhw.add.128B
    hexagon_V6_vscattermhwq,                      // llvm.hexagon.V6.vscattermhwq
    hexagon_V6_vscattermhwq_128B,                 // llvm.hexagon.V6.vscattermhwq.128B
    hexagon_V6_vscattermw,                        // llvm.hexagon.V6.vscattermw
    hexagon_V6_vscattermw_128B,                   // llvm.hexagon.V6.vscattermw.128B
    hexagon_V6_vscattermw_add,                    // llvm.hexagon.V6.vscattermw.add
    hexagon_V6_vscattermw_add_128B,               // llvm.hexagon.V6.vscattermw.add.128B
    hexagon_V6_vscattermwq,                       // llvm.hexagon.V6.vscattermwq
    hexagon_V6_vscattermwq_128B,                  // llvm.hexagon.V6.vscattermwq.128B
    hexagon_V6_vsh,                               // llvm.hexagon.V6.vsh
    hexagon_V6_vsh_128B,                          // llvm.hexagon.V6.vsh.128B
    hexagon_V6_vshufeh,                           // llvm.hexagon.V6.vshufeh
    hexagon_V6_vshufeh_128B,                      // llvm.hexagon.V6.vshufeh.128B
    hexagon_V6_vshuffb,                           // llvm.hexagon.V6.vshuffb
    hexagon_V6_vshuffb_128B,                      // llvm.hexagon.V6.vshuffb.128B
    hexagon_V6_vshuffeb,                          // llvm.hexagon.V6.vshuffeb
    hexagon_V6_vshuffeb_128B,                     // llvm.hexagon.V6.vshuffeb.128B
    hexagon_V6_vshuffh,                           // llvm.hexagon.V6.vshuffh
    hexagon_V6_vshuffh_128B,                      // llvm.hexagon.V6.vshuffh.128B
    hexagon_V6_vshuffob,                          // llvm.hexagon.V6.vshuffob
    hexagon_V6_vshuffob_128B,                     // llvm.hexagon.V6.vshuffob.128B
    hexagon_V6_vshuffvdd,                         // llvm.hexagon.V6.vshuffvdd
    hexagon_V6_vshuffvdd_128B,                    // llvm.hexagon.V6.vshuffvdd.128B
    hexagon_V6_vshufoeb,                          // llvm.hexagon.V6.vshufoeb
    hexagon_V6_vshufoeb_128B,                     // llvm.hexagon.V6.vshufoeb.128B
    hexagon_V6_vshufoeh,                          // llvm.hexagon.V6.vshufoeh
    hexagon_V6_vshufoeh_128B,                     // llvm.hexagon.V6.vshufoeh.128B
    hexagon_V6_vshufoh,                           // llvm.hexagon.V6.vshufoh
    hexagon_V6_vshufoh_128B,                      // llvm.hexagon.V6.vshufoh.128B
    hexagon_V6_vsubb,                             // llvm.hexagon.V6.vsubb
    hexagon_V6_vsubb_128B,                        // llvm.hexagon.V6.vsubb.128B
    hexagon_V6_vsubb_dv,                          // llvm.hexagon.V6.vsubb.dv
    hexagon_V6_vsubb_dv_128B,                     // llvm.hexagon.V6.vsubb.dv.128B
    hexagon_V6_vsubbnq,                           // llvm.hexagon.V6.vsubbnq
    hexagon_V6_vsubbnq_128B,                      // llvm.hexagon.V6.vsubbnq.128B
    hexagon_V6_vsubbq,                            // llvm.hexagon.V6.vsubbq
    hexagon_V6_vsubbq_128B,                       // llvm.hexagon.V6.vsubbq.128B
    hexagon_V6_vsubbsat,                          // llvm.hexagon.V6.vsubbsat
    hexagon_V6_vsubbsat_128B,                     // llvm.hexagon.V6.vsubbsat.128B
    hexagon_V6_vsubbsat_dv,                       // llvm.hexagon.V6.vsubbsat.dv
    hexagon_V6_vsubbsat_dv_128B,                  // llvm.hexagon.V6.vsubbsat.dv.128B
    hexagon_V6_vsubcarry,                         // llvm.hexagon.V6.vsubcarry
    hexagon_V6_vsubcarry_128B,                    // llvm.hexagon.V6.vsubcarry.128B
    hexagon_V6_vsubh,                             // llvm.hexagon.V6.vsubh
    hexagon_V6_vsubh_128B,                        // llvm.hexagon.V6.vsubh.128B
    hexagon_V6_vsubh_dv,                          // llvm.hexagon.V6.vsubh.dv
    hexagon_V6_vsubh_dv_128B,                     // llvm.hexagon.V6.vsubh.dv.128B
    hexagon_V6_vsubhnq,                           // llvm.hexagon.V6.vsubhnq
    hexagon_V6_vsubhnq_128B,                      // llvm.hexagon.V6.vsubhnq.128B
    hexagon_V6_vsubhq,                            // llvm.hexagon.V6.vsubhq
    hexagon_V6_vsubhq_128B,                       // llvm.hexagon.V6.vsubhq.128B
    hexagon_V6_vsubhsat,                          // llvm.hexagon.V6.vsubhsat
    hexagon_V6_vsubhsat_128B,                     // llvm.hexagon.V6.vsubhsat.128B
    hexagon_V6_vsubhsat_dv,                       // llvm.hexagon.V6.vsubhsat.dv
    hexagon_V6_vsubhsat_dv_128B,                  // llvm.hexagon.V6.vsubhsat.dv.128B
    hexagon_V6_vsubhw,                            // llvm.hexagon.V6.vsubhw
    hexagon_V6_vsubhw_128B,                       // llvm.hexagon.V6.vsubhw.128B
    hexagon_V6_vsububh,                           // llvm.hexagon.V6.vsububh
    hexagon_V6_vsububh_128B,                      // llvm.hexagon.V6.vsububh.128B
    hexagon_V6_vsububsat,                         // llvm.hexagon.V6.vsububsat
    hexagon_V6_vsububsat_128B,                    // llvm.hexagon.V6.vsububsat.128B
    hexagon_V6_vsububsat_dv,                      // llvm.hexagon.V6.vsububsat.dv
    hexagon_V6_vsububsat_dv_128B,                 // llvm.hexagon.V6.vsububsat.dv.128B
    hexagon_V6_vsubububb_sat,                     // llvm.hexagon.V6.vsubububb.sat
    hexagon_V6_vsubububb_sat_128B,                // llvm.hexagon.V6.vsubububb.sat.128B
    hexagon_V6_vsubuhsat,                         // llvm.hexagon.V6.vsubuhsat
    hexagon_V6_vsubuhsat_128B,                    // llvm.hexagon.V6.vsubuhsat.128B
    hexagon_V6_vsubuhsat_dv,                      // llvm.hexagon.V6.vsubuhsat.dv
    hexagon_V6_vsubuhsat_dv_128B,                 // llvm.hexagon.V6.vsubuhsat.dv.128B
    hexagon_V6_vsubuhw,                           // llvm.hexagon.V6.vsubuhw
    hexagon_V6_vsubuhw_128B,                      // llvm.hexagon.V6.vsubuhw.128B
    hexagon_V6_vsubuwsat,                         // llvm.hexagon.V6.vsubuwsat
    hexagon_V6_vsubuwsat_128B,                    // llvm.hexagon.V6.vsubuwsat.128B
    hexagon_V6_vsubuwsat_dv,                      // llvm.hexagon.V6.vsubuwsat.dv
    hexagon_V6_vsubuwsat_dv_128B,                 // llvm.hexagon.V6.vsubuwsat.dv.128B
    hexagon_V6_vsubw,                             // llvm.hexagon.V6.vsubw
    hexagon_V6_vsubw_128B,                        // llvm.hexagon.V6.vsubw.128B
    hexagon_V6_vsubw_dv,                          // llvm.hexagon.V6.vsubw.dv
    hexagon_V6_vsubw_dv_128B,                     // llvm.hexagon.V6.vsubw.dv.128B
    hexagon_V6_vsubwnq,                           // llvm.hexagon.V6.vsubwnq
    hexagon_V6_vsubwnq_128B,                      // llvm.hexagon.V6.vsubwnq.128B
    hexagon_V6_vsubwq,                            // llvm.hexagon.V6.vsubwq
    hexagon_V6_vsubwq_128B,                       // llvm.hexagon.V6.vsubwq.128B
    hexagon_V6_vsubwsat,                          // llvm.hexagon.V6.vsubwsat
    hexagon_V6_vsubwsat_128B,                     // llvm.hexagon.V6.vsubwsat.128B
    hexagon_V6_vsubwsat_dv,                       // llvm.hexagon.V6.vsubwsat.dv
    hexagon_V6_vsubwsat_dv_128B,                  // llvm.hexagon.V6.vsubwsat.dv.128B
    hexagon_V6_vswap,                             // llvm.hexagon.V6.vswap
    hexagon_V6_vswap_128B,                        // llvm.hexagon.V6.vswap.128B
    hexagon_V6_vtmpyb,                            // llvm.hexagon.V6.vtmpyb
    hexagon_V6_vtmpyb_128B,                       // llvm.hexagon.V6.vtmpyb.128B
    hexagon_V6_vtmpyb_acc,                        // llvm.hexagon.V6.vtmpyb.acc
    hexagon_V6_vtmpyb_acc_128B,                   // llvm.hexagon.V6.vtmpyb.acc.128B
    hexagon_V6_vtmpybus,                          // llvm.hexagon.V6.vtmpybus
    hexagon_V6_vtmpybus_128B,                     // llvm.hexagon.V6.vtmpybus.128B
    hexagon_V6_vtmpybus_acc,                      // llvm.hexagon.V6.vtmpybus.acc
    hexagon_V6_vtmpybus_acc_128B,                 // llvm.hexagon.V6.vtmpybus.acc.128B
    hexagon_V6_vtmpyhb,                           // llvm.hexagon.V6.vtmpyhb
    hexagon_V6_vtmpyhb_128B,                      // llvm.hexagon.V6.vtmpyhb.128B
    hexagon_V6_vtmpyhb_acc,                       // llvm.hexagon.V6.vtmpyhb.acc
    hexagon_V6_vtmpyhb_acc_128B,                  // llvm.hexagon.V6.vtmpyhb.acc.128B
    hexagon_V6_vunpackb,                          // llvm.hexagon.V6.vunpackb
    hexagon_V6_vunpackb_128B,                     // llvm.hexagon.V6.vunpackb.128B
    hexagon_V6_vunpackh,                          // llvm.hexagon.V6.vunpackh
    hexagon_V6_vunpackh_128B,                     // llvm.hexagon.V6.vunpackh.128B
    hexagon_V6_vunpackob,                         // llvm.hexagon.V6.vunpackob
    hexagon_V6_vunpackob_128B,                    // llvm.hexagon.V6.vunpackob.128B
    hexagon_V6_vunpackoh,                         // llvm.hexagon.V6.vunpackoh
    hexagon_V6_vunpackoh_128B,                    // llvm.hexagon.V6.vunpackoh.128B
    hexagon_V6_vunpackub,                         // llvm.hexagon.V6.vunpackub
    hexagon_V6_vunpackub_128B,                    // llvm.hexagon.V6.vunpackub.128B
    hexagon_V6_vunpackuh,                         // llvm.hexagon.V6.vunpackuh
    hexagon_V6_vunpackuh_128B,                    // llvm.hexagon.V6.vunpackuh.128B
    hexagon_V6_vxor,                              // llvm.hexagon.V6.vxor
    hexagon_V6_vxor_128B,                         // llvm.hexagon.V6.vxor.128B
    hexagon_V6_vzb,                               // llvm.hexagon.V6.vzb
    hexagon_V6_vzb_128B,                          // llvm.hexagon.V6.vzb.128B
    hexagon_V6_vzh,                               // llvm.hexagon.V6.vzh
    hexagon_V6_vzh_128B,                          // llvm.hexagon.V6.vzh.128B
    hexagon_Y2_dccleana,                          // llvm.hexagon.Y2.dccleana
    hexagon_Y2_dccleaninva,                       // llvm.hexagon.Y2.dccleaninva
    hexagon_Y2_dcinva,                            // llvm.hexagon.Y2.dcinva
    hexagon_Y2_dczeroa,                           // llvm.hexagon.Y2.dczeroa
    hexagon_Y4_l2fetch,                           // llvm.hexagon.Y4.l2fetch
    hexagon_Y5_l2fetch,                           // llvm.hexagon.Y5.l2fetch
    hexagon_brev_ldb,                             // llvm.hexagon.brev.ldb
    hexagon_brev_ldd,                             // llvm.hexagon.brev.ldd
    hexagon_brev_ldh,                             // llvm.hexagon.brev.ldh
    hexagon_brev_ldub,                            // llvm.hexagon.brev.ldub
    hexagon_brev_lduh,                            // llvm.hexagon.brev.lduh
    hexagon_brev_ldw,                             // llvm.hexagon.brev.ldw
    hexagon_brev_stb,                             // llvm.hexagon.brev.stb
    hexagon_brev_std,                             // llvm.hexagon.brev.std
    hexagon_brev_sth,                             // llvm.hexagon.brev.sth
    hexagon_brev_sthhi,                           // llvm.hexagon.brev.sthhi
    hexagon_brev_stw,                             // llvm.hexagon.brev.stw
    hexagon_circ_ldb,                             // llvm.hexagon.circ.ldb
    hexagon_circ_ldd,                             // llvm.hexagon.circ.ldd
    hexagon_circ_ldh,                             // llvm.hexagon.circ.ldh
    hexagon_circ_ldub,                            // llvm.hexagon.circ.ldub
    hexagon_circ_lduh,                            // llvm.hexagon.circ.lduh
    hexagon_circ_ldw,                             // llvm.hexagon.circ.ldw
    hexagon_circ_stb,                             // llvm.hexagon.circ.stb
    hexagon_circ_std,                             // llvm.hexagon.circ.std
    hexagon_circ_sth,                             // llvm.hexagon.circ.sth
    hexagon_circ_sthhi,                           // llvm.hexagon.circ.sthhi
    hexagon_circ_stw,                             // llvm.hexagon.circ.stw
    hexagon_mm256i_vaddw,                         // llvm.hexagon.mm256i.vaddw
    hexagon_prefetch,                             // llvm.hexagon.prefetch
    mips_absq_s_ph,                               // llvm.mips.absq.s.ph
    mips_absq_s_qb,                               // llvm.mips.absq.s.qb
    mips_absq_s_w,                                // llvm.mips.absq.s.w
    mips_add_a_b,                                 // llvm.mips.add.a.b
    mips_add_a_d,                                 // llvm.mips.add.a.d
    mips_add_a_h,                                 // llvm.mips.add.a.h
    mips_add_a_w,                                 // llvm.mips.add.a.w
    mips_addq_ph,                                 // llvm.mips.addq.ph
    mips_addq_s_ph,                               // llvm.mips.addq.s.ph
    mips_addq_s_w,                                // llvm.mips.addq.s.w
    mips_addqh_ph,                                // llvm.mips.addqh.ph
    mips_addqh_r_ph,                              // llvm.mips.addqh.r.ph
    mips_addqh_r_w,                               // llvm.mips.addqh.r.w
    mips_addqh_w,                                 // llvm.mips.addqh.w
    mips_adds_a_b,                                // llvm.mips.adds.a.b
    mips_adds_a_d,                                // llvm.mips.adds.a.d
    mips_adds_a_h,                                // llvm.mips.adds.a.h
    mips_adds_a_w,                                // llvm.mips.adds.a.w
    mips_adds_s_b,                                // llvm.mips.adds.s.b
    mips_adds_s_d,                                // llvm.mips.adds.s.d
    mips_adds_s_h,                                // llvm.mips.adds.s.h
    mips_adds_s_w,                                // llvm.mips.adds.s.w
    mips_adds_u_b,                                // llvm.mips.adds.u.b
    mips_adds_u_d,                                // llvm.mips.adds.u.d
    mips_adds_u_h,                                // llvm.mips.adds.u.h
    mips_adds_u_w,                                // llvm.mips.adds.u.w
    mips_addsc,                                   // llvm.mips.addsc
    mips_addu_ph,                                 // llvm.mips.addu.ph
    mips_addu_qb,                                 // llvm.mips.addu.qb
    mips_addu_s_ph,                               // llvm.mips.addu.s.ph
    mips_addu_s_qb,                               // llvm.mips.addu.s.qb
    mips_adduh_qb,                                // llvm.mips.adduh.qb
    mips_adduh_r_qb,                              // llvm.mips.adduh.r.qb
    mips_addv_b,                                  // llvm.mips.addv.b
    mips_addv_d,                                  // llvm.mips.addv.d
    mips_addv_h,                                  // llvm.mips.addv.h
    mips_addv_w,                                  // llvm.mips.addv.w
    mips_addvi_b,                                 // llvm.mips.addvi.b
    mips_addvi_d,                                 // llvm.mips.addvi.d
    mips_addvi_h,                                 // llvm.mips.addvi.h
    mips_addvi_w,                                 // llvm.mips.addvi.w
    mips_addwc,                                   // llvm.mips.addwc
    mips_and_v,                                   // llvm.mips.and.v
    mips_andi_b,                                  // llvm.mips.andi.b
    mips_append,                                  // llvm.mips.append
    mips_asub_s_b,                                // llvm.mips.asub.s.b
    mips_asub_s_d,                                // llvm.mips.asub.s.d
    mips_asub_s_h,                                // llvm.mips.asub.s.h
    mips_asub_s_w,                                // llvm.mips.asub.s.w
    mips_asub_u_b,                                // llvm.mips.asub.u.b
    mips_asub_u_d,                                // llvm.mips.asub.u.d
    mips_asub_u_h,                                // llvm.mips.asub.u.h
    mips_asub_u_w,                                // llvm.mips.asub.u.w
    mips_ave_s_b,                                 // llvm.mips.ave.s.b
    mips_ave_s_d,                                 // llvm.mips.ave.s.d
    mips_ave_s_h,                                 // llvm.mips.ave.s.h
    mips_ave_s_w,                                 // llvm.mips.ave.s.w
    mips_ave_u_b,                                 // llvm.mips.ave.u.b
    mips_ave_u_d,                                 // llvm.mips.ave.u.d
    mips_ave_u_h,                                 // llvm.mips.ave.u.h
    mips_ave_u_w,                                 // llvm.mips.ave.u.w
    mips_aver_s_b,                                // llvm.mips.aver.s.b
    mips_aver_s_d,                                // llvm.mips.aver.s.d
    mips_aver_s_h,                                // llvm.mips.aver.s.h
    mips_aver_s_w,                                // llvm.mips.aver.s.w
    mips_aver_u_b,                                // llvm.mips.aver.u.b
    mips_aver_u_d,                                // llvm.mips.aver.u.d
    mips_aver_u_h,                                // llvm.mips.aver.u.h
    mips_aver_u_w,                                // llvm.mips.aver.u.w
    mips_balign,                                  // llvm.mips.balign
    mips_bclr_b,                                  // llvm.mips.bclr.b
    mips_bclr_d,                                  // llvm.mips.bclr.d
    mips_bclr_h,                                  // llvm.mips.bclr.h
    mips_bclr_w,                                  // llvm.mips.bclr.w
    mips_bclri_b,                                 // llvm.mips.bclri.b
    mips_bclri_d,                                 // llvm.mips.bclri.d
    mips_bclri_h,                                 // llvm.mips.bclri.h
    mips_bclri_w,                                 // llvm.mips.bclri.w
    mips_binsl_b,                                 // llvm.mips.binsl.b
    mips_binsl_d,                                 // llvm.mips.binsl.d
    mips_binsl_h,                                 // llvm.mips.binsl.h
    mips_binsl_w,                                 // llvm.mips.binsl.w
    mips_binsli_b,                                // llvm.mips.binsli.b
    mips_binsli_d,                                // llvm.mips.binsli.d
    mips_binsli_h,                                // llvm.mips.binsli.h
    mips_binsli_w,                                // llvm.mips.binsli.w
    mips_binsr_b,                                 // llvm.mips.binsr.b
    mips_binsr_d,                                 // llvm.mips.binsr.d
    mips_binsr_h,                                 // llvm.mips.binsr.h
    mips_binsr_w,                                 // llvm.mips.binsr.w
    mips_binsri_b,                                // llvm.mips.binsri.b
    mips_binsri_d,                                // llvm.mips.binsri.d
    mips_binsri_h,                                // llvm.mips.binsri.h
    mips_binsri_w,                                // llvm.mips.binsri.w
    mips_bitrev,                                  // llvm.mips.bitrev
    mips_bmnz_v,                                  // llvm.mips.bmnz.v
    mips_bmnzi_b,                                 // llvm.mips.bmnzi.b
    mips_bmz_v,                                   // llvm.mips.bmz.v
    mips_bmzi_b,                                  // llvm.mips.bmzi.b
    mips_bneg_b,                                  // llvm.mips.bneg.b
    mips_bneg_d,                                  // llvm.mips.bneg.d
    mips_bneg_h,                                  // llvm.mips.bneg.h
    mips_bneg_w,                                  // llvm.mips.bneg.w
    mips_bnegi_b,                                 // llvm.mips.bnegi.b
    mips_bnegi_d,                                 // llvm.mips.bnegi.d
    mips_bnegi_h,                                 // llvm.mips.bnegi.h
    mips_bnegi_w,                                 // llvm.mips.bnegi.w
    mips_bnz_b,                                   // llvm.mips.bnz.b
    mips_bnz_d,                                   // llvm.mips.bnz.d
    mips_bnz_h,                                   // llvm.mips.bnz.h
    mips_bnz_v,                                   // llvm.mips.bnz.v
    mips_bnz_w,                                   // llvm.mips.bnz.w
    mips_bposge32,                                // llvm.mips.bposge32
    mips_bsel_v,                                  // llvm.mips.bsel.v
    mips_bseli_b,                                 // llvm.mips.bseli.b
    mips_bset_b,                                  // llvm.mips.bset.b
    mips_bset_d,                                  // llvm.mips.bset.d
    mips_bset_h,                                  // llvm.mips.bset.h
    mips_bset_w,                                  // llvm.mips.bset.w
    mips_bseti_b,                                 // llvm.mips.bseti.b
    mips_bseti_d,                                 // llvm.mips.bseti.d
    mips_bseti_h,                                 // llvm.mips.bseti.h
    mips_bseti_w,                                 // llvm.mips.bseti.w
    mips_bz_b,                                    // llvm.mips.bz.b
    mips_bz_d,                                    // llvm.mips.bz.d
    mips_bz_h,                                    // llvm.mips.bz.h
    mips_bz_v,                                    // llvm.mips.bz.v
    mips_bz_w,                                    // llvm.mips.bz.w
    mips_ceq_b,                                   // llvm.mips.ceq.b
    mips_ceq_d,                                   // llvm.mips.ceq.d
    mips_ceq_h,                                   // llvm.mips.ceq.h
    mips_ceq_w,                                   // llvm.mips.ceq.w
    mips_ceqi_b,                                  // llvm.mips.ceqi.b
    mips_ceqi_d,                                  // llvm.mips.ceqi.d
    mips_ceqi_h,                                  // llvm.mips.ceqi.h
    mips_ceqi_w,                                  // llvm.mips.ceqi.w
    mips_cfcmsa,                                  // llvm.mips.cfcmsa
    mips_cle_s_b,                                 // llvm.mips.cle.s.b
    mips_cle_s_d,                                 // llvm.mips.cle.s.d
    mips_cle_s_h,                                 // llvm.mips.cle.s.h
    mips_cle_s_w,                                 // llvm.mips.cle.s.w
    mips_cle_u_b,                                 // llvm.mips.cle.u.b
    mips_cle_u_d,                                 // llvm.mips.cle.u.d
    mips_cle_u_h,                                 // llvm.mips.cle.u.h
    mips_cle_u_w,                                 // llvm.mips.cle.u.w
    mips_clei_s_b,                                // llvm.mips.clei.s.b
    mips_clei_s_d,                                // llvm.mips.clei.s.d
    mips_clei_s_h,                                // llvm.mips.clei.s.h
    mips_clei_s_w,                                // llvm.mips.clei.s.w
    mips_clei_u_b,                                // llvm.mips.clei.u.b
    mips_clei_u_d,                                // llvm.mips.clei.u.d
    mips_clei_u_h,                                // llvm.mips.clei.u.h
    mips_clei_u_w,                                // llvm.mips.clei.u.w
    mips_clt_s_b,                                 // llvm.mips.clt.s.b
    mips_clt_s_d,                                 // llvm.mips.clt.s.d
    mips_clt_s_h,                                 // llvm.mips.clt.s.h
    mips_clt_s_w,                                 // llvm.mips.clt.s.w
    mips_clt_u_b,                                 // llvm.mips.clt.u.b
    mips_clt_u_d,                                 // llvm.mips.clt.u.d
    mips_clt_u_h,                                 // llvm.mips.clt.u.h
    mips_clt_u_w,                                 // llvm.mips.clt.u.w
    mips_clti_s_b,                                // llvm.mips.clti.s.b
    mips_clti_s_d,                                // llvm.mips.clti.s.d
    mips_clti_s_h,                                // llvm.mips.clti.s.h
    mips_clti_s_w,                                // llvm.mips.clti.s.w
    mips_clti_u_b,                                // llvm.mips.clti.u.b
    mips_clti_u_d,                                // llvm.mips.clti.u.d
    mips_clti_u_h,                                // llvm.mips.clti.u.h
    mips_clti_u_w,                                // llvm.mips.clti.u.w
    mips_cmp_eq_ph,                               // llvm.mips.cmp.eq.ph
    mips_cmp_le_ph,                               // llvm.mips.cmp.le.ph
    mips_cmp_lt_ph,                               // llvm.mips.cmp.lt.ph
    mips_cmpgdu_eq_qb,                            // llvm.mips.cmpgdu.eq.qb
    mips_cmpgdu_le_qb,                            // llvm.mips.cmpgdu.le.qb
    mips_cmpgdu_lt_qb,                            // llvm.mips.cmpgdu.lt.qb
    mips_cmpgu_eq_qb,                             // llvm.mips.cmpgu.eq.qb
    mips_cmpgu_le_qb,                             // llvm.mips.cmpgu.le.qb
    mips_cmpgu_lt_qb,                             // llvm.mips.cmpgu.lt.qb
    mips_cmpu_eq_qb,                              // llvm.mips.cmpu.eq.qb
    mips_cmpu_le_qb,                              // llvm.mips.cmpu.le.qb
    mips_cmpu_lt_qb,                              // llvm.mips.cmpu.lt.qb
    mips_copy_s_b,                                // llvm.mips.copy.s.b
    mips_copy_s_d,                                // llvm.mips.copy.s.d
    mips_copy_s_h,                                // llvm.mips.copy.s.h
    mips_copy_s_w,                                // llvm.mips.copy.s.w
    mips_copy_u_b,                                // llvm.mips.copy.u.b
    mips_copy_u_d,                                // llvm.mips.copy.u.d
    mips_copy_u_h,                                // llvm.mips.copy.u.h
    mips_copy_u_w,                                // llvm.mips.copy.u.w
    mips_ctcmsa,                                  // llvm.mips.ctcmsa
    mips_div_s_b,                                 // llvm.mips.div.s.b
    mips_div_s_d,                                 // llvm.mips.div.s.d
    mips_div_s_h,                                 // llvm.mips.div.s.h
    mips_div_s_w,                                 // llvm.mips.div.s.w
    mips_div_u_b,                                 // llvm.mips.div.u.b
    mips_div_u_d,                                 // llvm.mips.div.u.d
    mips_div_u_h,                                 // llvm.mips.div.u.h
    mips_div_u_w,                                 // llvm.mips.div.u.w
    mips_dlsa,                                    // llvm.mips.dlsa
    mips_dotp_s_d,                                // llvm.mips.dotp.s.d
    mips_dotp_s_h,                                // llvm.mips.dotp.s.h
    mips_dotp_s_w,                                // llvm.mips.dotp.s.w
    mips_dotp_u_d,                                // llvm.mips.dotp.u.d
    mips_dotp_u_h,                                // llvm.mips.dotp.u.h
    mips_dotp_u_w,                                // llvm.mips.dotp.u.w
    mips_dpa_w_ph,                                // llvm.mips.dpa.w.ph
    mips_dpadd_s_d,                               // llvm.mips.dpadd.s.d
    mips_dpadd_s_h,                               // llvm.mips.dpadd.s.h
    mips_dpadd_s_w,                               // llvm.mips.dpadd.s.w
    mips_dpadd_u_d,                               // llvm.mips.dpadd.u.d
    mips_dpadd_u_h,                               // llvm.mips.dpadd.u.h
    mips_dpadd_u_w,                               // llvm.mips.dpadd.u.w
    mips_dpaq_s_w_ph,                             // llvm.mips.dpaq.s.w.ph
    mips_dpaq_sa_l_w,                             // llvm.mips.dpaq.sa.l.w
    mips_dpaqx_s_w_ph,                            // llvm.mips.dpaqx.s.w.ph
    mips_dpaqx_sa_w_ph,                           // llvm.mips.dpaqx.sa.w.ph
    mips_dpau_h_qbl,                              // llvm.mips.dpau.h.qbl
    mips_dpau_h_qbr,                              // llvm.mips.dpau.h.qbr
    mips_dpax_w_ph,                               // llvm.mips.dpax.w.ph
    mips_dps_w_ph,                                // llvm.mips.dps.w.ph
    mips_dpsq_s_w_ph,                             // llvm.mips.dpsq.s.w.ph
    mips_dpsq_sa_l_w,                             // llvm.mips.dpsq.sa.l.w
    mips_dpsqx_s_w_ph,                            // llvm.mips.dpsqx.s.w.ph
    mips_dpsqx_sa_w_ph,                           // llvm.mips.dpsqx.sa.w.ph
    mips_dpsu_h_qbl,                              // llvm.mips.dpsu.h.qbl
    mips_dpsu_h_qbr,                              // llvm.mips.dpsu.h.qbr
    mips_dpsub_s_d,                               // llvm.mips.dpsub.s.d
    mips_dpsub_s_h,                               // llvm.mips.dpsub.s.h
    mips_dpsub_s_w,                               // llvm.mips.dpsub.s.w
    mips_dpsub_u_d,                               // llvm.mips.dpsub.u.d
    mips_dpsub_u_h,                               // llvm.mips.dpsub.u.h
    mips_dpsub_u_w,                               // llvm.mips.dpsub.u.w
    mips_dpsx_w_ph,                               // llvm.mips.dpsx.w.ph
    mips_extp,                                    // llvm.mips.extp
    mips_extpdp,                                  // llvm.mips.extpdp
    mips_extr_r_w,                                // llvm.mips.extr.r.w
    mips_extr_rs_w,                               // llvm.mips.extr.rs.w
    mips_extr_s_h,                                // llvm.mips.extr.s.h
    mips_extr_w,                                  // llvm.mips.extr.w
    mips_fadd_d,                                  // llvm.mips.fadd.d
    mips_fadd_w,                                  // llvm.mips.fadd.w
    mips_fcaf_d,                                  // llvm.mips.fcaf.d
    mips_fcaf_w,                                  // llvm.mips.fcaf.w
    mips_fceq_d,                                  // llvm.mips.fceq.d
    mips_fceq_w,                                  // llvm.mips.fceq.w
    mips_fclass_d,                                // llvm.mips.fclass.d
    mips_fclass_w,                                // llvm.mips.fclass.w
    mips_fcle_d,                                  // llvm.mips.fcle.d
    mips_fcle_w,                                  // llvm.mips.fcle.w
    mips_fclt_d,                                  // llvm.mips.fclt.d
    mips_fclt_w,                                  // llvm.mips.fclt.w
    mips_fcne_d,                                  // llvm.mips.fcne.d
    mips_fcne_w,                                  // llvm.mips.fcne.w
    mips_fcor_d,                                  // llvm.mips.fcor.d
    mips_fcor_w,                                  // llvm.mips.fcor.w
    mips_fcueq_d,                                 // llvm.mips.fcueq.d
    mips_fcueq_w,                                 // llvm.mips.fcueq.w
    mips_fcule_d,                                 // llvm.mips.fcule.d
    mips_fcule_w,                                 // llvm.mips.fcule.w
    mips_fcult_d,                                 // llvm.mips.fcult.d
    mips_fcult_w,                                 // llvm.mips.fcult.w
    mips_fcun_d,                                  // llvm.mips.fcun.d
    mips_fcun_w,                                  // llvm.mips.fcun.w
    mips_fcune_d,                                 // llvm.mips.fcune.d
    mips_fcune_w,                                 // llvm.mips.fcune.w
    mips_fdiv_d,                                  // llvm.mips.fdiv.d
    mips_fdiv_w,                                  // llvm.mips.fdiv.w
    mips_fexdo_h,                                 // llvm.mips.fexdo.h
    mips_fexdo_w,                                 // llvm.mips.fexdo.w
    mips_fexp2_d,                                 // llvm.mips.fexp2.d
    mips_fexp2_w,                                 // llvm.mips.fexp2.w
    mips_fexupl_d,                                // llvm.mips.fexupl.d
    mips_fexupl_w,                                // llvm.mips.fexupl.w
    mips_fexupr_d,                                // llvm.mips.fexupr.d
    mips_fexupr_w,                                // llvm.mips.fexupr.w
    mips_ffint_s_d,                               // llvm.mips.ffint.s.d
    mips_ffint_s_w,                               // llvm.mips.ffint.s.w
    mips_ffint_u_d,                               // llvm.mips.ffint.u.d
    mips_ffint_u_w,                               // llvm.mips.ffint.u.w
    mips_ffql_d,                                  // llvm.mips.ffql.d
    mips_ffql_w,                                  // llvm.mips.ffql.w
    mips_ffqr_d,                                  // llvm.mips.ffqr.d
    mips_ffqr_w,                                  // llvm.mips.ffqr.w
    mips_fill_b,                                  // llvm.mips.fill.b
    mips_fill_d,                                  // llvm.mips.fill.d
    mips_fill_h,                                  // llvm.mips.fill.h
    mips_fill_w,                                  // llvm.mips.fill.w
    mips_flog2_d,                                 // llvm.mips.flog2.d
    mips_flog2_w,                                 // llvm.mips.flog2.w
    mips_fmadd_d,                                 // llvm.mips.fmadd.d
    mips_fmadd_w,                                 // llvm.mips.fmadd.w
    mips_fmax_a_d,                                // llvm.mips.fmax.a.d
    mips_fmax_a_w,                                // llvm.mips.fmax.a.w
    mips_fmax_d,                                  // llvm.mips.fmax.d
    mips_fmax_w,                                  // llvm.mips.fmax.w
    mips_fmin_a_d,                                // llvm.mips.fmin.a.d
    mips_fmin_a_w,                                // llvm.mips.fmin.a.w
    mips_fmin_d,                                  // llvm.mips.fmin.d
    mips_fmin_w,                                  // llvm.mips.fmin.w
    mips_fmsub_d,                                 // llvm.mips.fmsub.d
    mips_fmsub_w,                                 // llvm.mips.fmsub.w
    mips_fmul_d,                                  // llvm.mips.fmul.d
    mips_fmul_w,                                  // llvm.mips.fmul.w
    mips_frcp_d,                                  // llvm.mips.frcp.d
    mips_frcp_w,                                  // llvm.mips.frcp.w
    mips_frint_d,                                 // llvm.mips.frint.d
    mips_frint_w,                                 // llvm.mips.frint.w
    mips_frsqrt_d,                                // llvm.mips.frsqrt.d
    mips_frsqrt_w,                                // llvm.mips.frsqrt.w
    mips_fsaf_d,                                  // llvm.mips.fsaf.d
    mips_fsaf_w,                                  // llvm.mips.fsaf.w
    mips_fseq_d,                                  // llvm.mips.fseq.d
    mips_fseq_w,                                  // llvm.mips.fseq.w
    mips_fsle_d,                                  // llvm.mips.fsle.d
    mips_fsle_w,                                  // llvm.mips.fsle.w
    mips_fslt_d,                                  // llvm.mips.fslt.d
    mips_fslt_w,                                  // llvm.mips.fslt.w
    mips_fsne_d,                                  // llvm.mips.fsne.d
    mips_fsne_w,                                  // llvm.mips.fsne.w
    mips_fsor_d,                                  // llvm.mips.fsor.d
    mips_fsor_w,                                  // llvm.mips.fsor.w
    mips_fsqrt_d,                                 // llvm.mips.fsqrt.d
    mips_fsqrt_w,                                 // llvm.mips.fsqrt.w
    mips_fsub_d,                                  // llvm.mips.fsub.d
    mips_fsub_w,                                  // llvm.mips.fsub.w
    mips_fsueq_d,                                 // llvm.mips.fsueq.d
    mips_fsueq_w,                                 // llvm.mips.fsueq.w
    mips_fsule_d,                                 // llvm.mips.fsule.d
    mips_fsule_w,                                 // llvm.mips.fsule.w
    mips_fsult_d,                                 // llvm.mips.fsult.d
    mips_fsult_w,                                 // llvm.mips.fsult.w
    mips_fsun_d,                                  // llvm.mips.fsun.d
    mips_fsun_w,                                  // llvm.mips.fsun.w
    mips_fsune_d,                                 // llvm.mips.fsune.d
    mips_fsune_w,                                 // llvm.mips.fsune.w
    mips_ftint_s_d,                               // llvm.mips.ftint.s.d
    mips_ftint_s_w,                               // llvm.mips.ftint.s.w
    mips_ftint_u_d,                               // llvm.mips.ftint.u.d
    mips_ftint_u_w,                               // llvm.mips.ftint.u.w
    mips_ftq_h,                                   // llvm.mips.ftq.h
    mips_ftq_w,                                   // llvm.mips.ftq.w
    mips_ftrunc_s_d,                              // llvm.mips.ftrunc.s.d
    mips_ftrunc_s_w,                              // llvm.mips.ftrunc.s.w
    mips_ftrunc_u_d,                              // llvm.mips.ftrunc.u.d
    mips_ftrunc_u_w,                              // llvm.mips.ftrunc.u.w
    mips_hadd_s_d,                                // llvm.mips.hadd.s.d
    mips_hadd_s_h,                                // llvm.mips.hadd.s.h
    mips_hadd_s_w,                                // llvm.mips.hadd.s.w
    mips_hadd_u_d,                                // llvm.mips.hadd.u.d
    mips_hadd_u_h,                                // llvm.mips.hadd.u.h
    mips_hadd_u_w,                                // llvm.mips.hadd.u.w
    mips_hsub_s_d,                                // llvm.mips.hsub.s.d
    mips_hsub_s_h,                                // llvm.mips.hsub.s.h
    mips_hsub_s_w,                                // llvm.mips.hsub.s.w
    mips_hsub_u_d,                                // llvm.mips.hsub.u.d
    mips_hsub_u_h,                                // llvm.mips.hsub.u.h
    mips_hsub_u_w,                                // llvm.mips.hsub.u.w
    mips_ilvev_b,                                 // llvm.mips.ilvev.b
    mips_ilvev_d,                                 // llvm.mips.ilvev.d
    mips_ilvev_h,                                 // llvm.mips.ilvev.h
    mips_ilvev_w,                                 // llvm.mips.ilvev.w
    mips_ilvl_b,                                  // llvm.mips.ilvl.b
    mips_ilvl_d,                                  // llvm.mips.ilvl.d
    mips_ilvl_h,                                  // llvm.mips.ilvl.h
    mips_ilvl_w,                                  // llvm.mips.ilvl.w
    mips_ilvod_b,                                 // llvm.mips.ilvod.b
    mips_ilvod_d,                                 // llvm.mips.ilvod.d
    mips_ilvod_h,                                 // llvm.mips.ilvod.h
    mips_ilvod_w,                                 // llvm.mips.ilvod.w
    mips_ilvr_b,                                  // llvm.mips.ilvr.b
    mips_ilvr_d,                                  // llvm.mips.ilvr.d
    mips_ilvr_h,                                  // llvm.mips.ilvr.h
    mips_ilvr_w,                                  // llvm.mips.ilvr.w
    mips_insert_b,                                // llvm.mips.insert.b
    mips_insert_d,                                // llvm.mips.insert.d
    mips_insert_h,                                // llvm.mips.insert.h
    mips_insert_w,                                // llvm.mips.insert.w
    mips_insv,                                    // llvm.mips.insv
    mips_insve_b,                                 // llvm.mips.insve.b
    mips_insve_d,                                 // llvm.mips.insve.d
    mips_insve_h,                                 // llvm.mips.insve.h
    mips_insve_w,                                 // llvm.mips.insve.w
    mips_lbux,                                    // llvm.mips.lbux
    mips_ld_b,                                    // llvm.mips.ld.b
    mips_ld_d,                                    // llvm.mips.ld.d
    mips_ld_h,                                    // llvm.mips.ld.h
    mips_ld_w,                                    // llvm.mips.ld.w
    mips_ldi_b,                                   // llvm.mips.ldi.b
    mips_ldi_d,                                   // llvm.mips.ldi.d
    mips_ldi_h,                                   // llvm.mips.ldi.h
    mips_ldi_w,                                   // llvm.mips.ldi.w
    mips_lhx,                                     // llvm.mips.lhx
    mips_lsa,                                     // llvm.mips.lsa
    mips_lwx,                                     // llvm.mips.lwx
    mips_madd,                                    // llvm.mips.madd
    mips_madd_q_h,                                // llvm.mips.madd.q.h
    mips_madd_q_w,                                // llvm.mips.madd.q.w
    mips_maddr_q_h,                               // llvm.mips.maddr.q.h
    mips_maddr_q_w,                               // llvm.mips.maddr.q.w
    mips_maddu,                                   // llvm.mips.maddu
    mips_maddv_b,                                 // llvm.mips.maddv.b
    mips_maddv_d,                                 // llvm.mips.maddv.d
    mips_maddv_h,                                 // llvm.mips.maddv.h
    mips_maddv_w,                                 // llvm.mips.maddv.w
    mips_maq_s_w_phl,                             // llvm.mips.maq.s.w.phl
    mips_maq_s_w_phr,                             // llvm.mips.maq.s.w.phr
    mips_maq_sa_w_phl,                            // llvm.mips.maq.sa.w.phl
    mips_maq_sa_w_phr,                            // llvm.mips.maq.sa.w.phr
    mips_max_a_b,                                 // llvm.mips.max.a.b
    mips_max_a_d,                                 // llvm.mips.max.a.d
    mips_max_a_h,                                 // llvm.mips.max.a.h
    mips_max_a_w,                                 // llvm.mips.max.a.w
    mips_max_s_b,                                 // llvm.mips.max.s.b
    mips_max_s_d,                                 // llvm.mips.max.s.d
    mips_max_s_h,                                 // llvm.mips.max.s.h
    mips_max_s_w,                                 // llvm.mips.max.s.w
    mips_max_u_b,                                 // llvm.mips.max.u.b
    mips_max_u_d,                                 // llvm.mips.max.u.d
    mips_max_u_h,                                 // llvm.mips.max.u.h
    mips_max_u_w,                                 // llvm.mips.max.u.w
    mips_maxi_s_b,                                // llvm.mips.maxi.s.b
    mips_maxi_s_d,                                // llvm.mips.maxi.s.d
    mips_maxi_s_h,                                // llvm.mips.maxi.s.h
    mips_maxi_s_w,                                // llvm.mips.maxi.s.w
    mips_maxi_u_b,                                // llvm.mips.maxi.u.b
    mips_maxi_u_d,                                // llvm.mips.maxi.u.d
    mips_maxi_u_h,                                // llvm.mips.maxi.u.h
    mips_maxi_u_w,                                // llvm.mips.maxi.u.w
    mips_min_a_b,                                 // llvm.mips.min.a.b
    mips_min_a_d,                                 // llvm.mips.min.a.d
    mips_min_a_h,                                 // llvm.mips.min.a.h
    mips_min_a_w,                                 // llvm.mips.min.a.w
    mips_min_s_b,                                 // llvm.mips.min.s.b
    mips_min_s_d,                                 // llvm.mips.min.s.d
    mips_min_s_h,                                 // llvm.mips.min.s.h
    mips_min_s_w,                                 // llvm.mips.min.s.w
    mips_min_u_b,                                 // llvm.mips.min.u.b
    mips_min_u_d,                                 // llvm.mips.min.u.d
    mips_min_u_h,                                 // llvm.mips.min.u.h
    mips_min_u_w,                                 // llvm.mips.min.u.w
    mips_mini_s_b,                                // llvm.mips.mini.s.b
    mips_mini_s_d,                                // llvm.mips.mini.s.d
    mips_mini_s_h,                                // llvm.mips.mini.s.h
    mips_mini_s_w,                                // llvm.mips.mini.s.w
    mips_mini_u_b,                                // llvm.mips.mini.u.b
    mips_mini_u_d,                                // llvm.mips.mini.u.d
    mips_mini_u_h,                                // llvm.mips.mini.u.h
    mips_mini_u_w,                                // llvm.mips.mini.u.w
    mips_mod_s_b,                                 // llvm.mips.mod.s.b
    mips_mod_s_d,                                 // llvm.mips.mod.s.d
    mips_mod_s_h,                                 // llvm.mips.mod.s.h
    mips_mod_s_w,                                 // llvm.mips.mod.s.w
    mips_mod_u_b,                                 // llvm.mips.mod.u.b
    mips_mod_u_d,                                 // llvm.mips.mod.u.d
    mips_mod_u_h,                                 // llvm.mips.mod.u.h
    mips_mod_u_w,                                 // llvm.mips.mod.u.w
    mips_modsub,                                  // llvm.mips.modsub
    mips_move_v,                                  // llvm.mips.move.v
    mips_msub,                                    // llvm.mips.msub
    mips_msub_q_h,                                // llvm.mips.msub.q.h
    mips_msub_q_w,                                // llvm.mips.msub.q.w
    mips_msubr_q_h,                               // llvm.mips.msubr.q.h
    mips_msubr_q_w,                               // llvm.mips.msubr.q.w
    mips_msubu,                                   // llvm.mips.msubu
    mips_msubv_b,                                 // llvm.mips.msubv.b
    mips_msubv_d,                                 // llvm.mips.msubv.d
    mips_msubv_h,                                 // llvm.mips.msubv.h
    mips_msubv_w,                                 // llvm.mips.msubv.w
    mips_mthlip,                                  // llvm.mips.mthlip
    mips_mul_ph,                                  // llvm.mips.mul.ph
    mips_mul_q_h,                                 // llvm.mips.mul.q.h
    mips_mul_q_w,                                 // llvm.mips.mul.q.w
    mips_mul_s_ph,                                // llvm.mips.mul.s.ph
    mips_muleq_s_w_phl,                           // llvm.mips.muleq.s.w.phl
    mips_muleq_s_w_phr,                           // llvm.mips.muleq.s.w.phr
    mips_muleu_s_ph_qbl,                          // llvm.mips.muleu.s.ph.qbl
    mips_muleu_s_ph_qbr,                          // llvm.mips.muleu.s.ph.qbr
    mips_mulq_rs_ph,                              // llvm.mips.mulq.rs.ph
    mips_mulq_rs_w,                               // llvm.mips.mulq.rs.w
    mips_mulq_s_ph,                               // llvm.mips.mulq.s.ph
    mips_mulq_s_w,                                // llvm.mips.mulq.s.w
    mips_mulr_q_h,                                // llvm.mips.mulr.q.h
    mips_mulr_q_w,                                // llvm.mips.mulr.q.w
    mips_mulsa_w_ph,                              // llvm.mips.mulsa.w.ph
    mips_mulsaq_s_w_ph,                           // llvm.mips.mulsaq.s.w.ph
    mips_mult,                                    // llvm.mips.mult
    mips_multu,                                   // llvm.mips.multu
    mips_mulv_b,                                  // llvm.mips.mulv.b
    mips_mulv_d,                                  // llvm.mips.mulv.d
    mips_mulv_h,                                  // llvm.mips.mulv.h
    mips_mulv_w,                                  // llvm.mips.mulv.w
    mips_nloc_b,                                  // llvm.mips.nloc.b
    mips_nloc_d,                                  // llvm.mips.nloc.d
    mips_nloc_h,                                  // llvm.mips.nloc.h
    mips_nloc_w,                                  // llvm.mips.nloc.w
    mips_nlzc_b,                                  // llvm.mips.nlzc.b
    mips_nlzc_d,                                  // llvm.mips.nlzc.d
    mips_nlzc_h,                                  // llvm.mips.nlzc.h
    mips_nlzc_w,                                  // llvm.mips.nlzc.w
    mips_nor_v,                                   // llvm.mips.nor.v
    mips_nori_b,                                  // llvm.mips.nori.b
    mips_or_v,                                    // llvm.mips.or.v
    mips_ori_b,                                   // llvm.mips.ori.b
    mips_packrl_ph,                               // llvm.mips.packrl.ph
    mips_pckev_b,                                 // llvm.mips.pckev.b
    mips_pckev_d,                                 // llvm.mips.pckev.d
    mips_pckev_h,                                 // llvm.mips.pckev.h
    mips_pckev_w,                                 // llvm.mips.pckev.w
    mips_pckod_b,                                 // llvm.mips.pckod.b
    mips_pckod_d,                                 // llvm.mips.pckod.d
    mips_pckod_h,                                 // llvm.mips.pckod.h
    mips_pckod_w,                                 // llvm.mips.pckod.w
    mips_pcnt_b,                                  // llvm.mips.pcnt.b
    mips_pcnt_d,                                  // llvm.mips.pcnt.d
    mips_pcnt_h,                                  // llvm.mips.pcnt.h
    mips_pcnt_w,                                  // llvm.mips.pcnt.w
    mips_pick_ph,                                 // llvm.mips.pick.ph
    mips_pick_qb,                                 // llvm.mips.pick.qb
    mips_preceq_w_phl,                            // llvm.mips.preceq.w.phl
    mips_preceq_w_phr,                            // llvm.mips.preceq.w.phr
    mips_precequ_ph_qbl,                          // llvm.mips.precequ.ph.qbl
    mips_precequ_ph_qbla,                         // llvm.mips.precequ.ph.qbla
    mips_precequ_ph_qbr,                          // llvm.mips.precequ.ph.qbr
    mips_precequ_ph_qbra,                         // llvm.mips.precequ.ph.qbra
    mips_preceu_ph_qbl,                           // llvm.mips.preceu.ph.qbl
    mips_preceu_ph_qbla,                          // llvm.mips.preceu.ph.qbla
    mips_preceu_ph_qbr,                           // llvm.mips.preceu.ph.qbr
    mips_preceu_ph_qbra,                          // llvm.mips.preceu.ph.qbra
    mips_precr_qb_ph,                             // llvm.mips.precr.qb.ph
    mips_precr_sra_ph_w,                          // llvm.mips.precr.sra.ph.w
    mips_precr_sra_r_ph_w,                        // llvm.mips.precr.sra.r.ph.w
    mips_precrq_ph_w,                             // llvm.mips.precrq.ph.w
    mips_precrq_qb_ph,                            // llvm.mips.precrq.qb.ph
    mips_precrq_rs_ph_w,                          // llvm.mips.precrq.rs.ph.w
    mips_precrqu_s_qb_ph,                         // llvm.mips.precrqu.s.qb.ph
    mips_prepend,                                 // llvm.mips.prepend
    mips_raddu_w_qb,                              // llvm.mips.raddu.w.qb
    mips_rddsp,                                   // llvm.mips.rddsp
    mips_repl_ph,                                 // llvm.mips.repl.ph
    mips_repl_qb,                                 // llvm.mips.repl.qb
    mips_sat_s_b,                                 // llvm.mips.sat.s.b
    mips_sat_s_d,                                 // llvm.mips.sat.s.d
    mips_sat_s_h,                                 // llvm.mips.sat.s.h
    mips_sat_s_w,                                 // llvm.mips.sat.s.w
    mips_sat_u_b,                                 // llvm.mips.sat.u.b
    mips_sat_u_d,                                 // llvm.mips.sat.u.d
    mips_sat_u_h,                                 // llvm.mips.sat.u.h
    mips_sat_u_w,                                 // llvm.mips.sat.u.w
    mips_shf_b,                                   // llvm.mips.shf.b
    mips_shf_h,                                   // llvm.mips.shf.h
    mips_shf_w,                                   // llvm.mips.shf.w
    mips_shilo,                                   // llvm.mips.shilo
    mips_shll_ph,                                 // llvm.mips.shll.ph
    mips_shll_qb,                                 // llvm.mips.shll.qb
    mips_shll_s_ph,                               // llvm.mips.shll.s.ph
    mips_shll_s_w,                                // llvm.mips.shll.s.w
    mips_shra_ph,                                 // llvm.mips.shra.ph
    mips_shra_qb,                                 // llvm.mips.shra.qb
    mips_shra_r_ph,                               // llvm.mips.shra.r.ph
    mips_shra_r_qb,                               // llvm.mips.shra.r.qb
    mips_shra_r_w,                                // llvm.mips.shra.r.w
    mips_shrl_ph,                                 // llvm.mips.shrl.ph
    mips_shrl_qb,                                 // llvm.mips.shrl.qb
    mips_sld_b,                                   // llvm.mips.sld.b
    mips_sld_d,                                   // llvm.mips.sld.d
    mips_sld_h,                                   // llvm.mips.sld.h
    mips_sld_w,                                   // llvm.mips.sld.w
    mips_sldi_b,                                  // llvm.mips.sldi.b
    mips_sldi_d,                                  // llvm.mips.sldi.d
    mips_sldi_h,                                  // llvm.mips.sldi.h
    mips_sldi_w,                                  // llvm.mips.sldi.w
    mips_sll_b,                                   // llvm.mips.sll.b
    mips_sll_d,                                   // llvm.mips.sll.d
    mips_sll_h,                                   // llvm.mips.sll.h
    mips_sll_w,                                   // llvm.mips.sll.w
    mips_slli_b,                                  // llvm.mips.slli.b
    mips_slli_d,                                  // llvm.mips.slli.d
    mips_slli_h,                                  // llvm.mips.slli.h
    mips_slli_w,                                  // llvm.mips.slli.w
    mips_splat_b,                                 // llvm.mips.splat.b
    mips_splat_d,                                 // llvm.mips.splat.d
    mips_splat_h,                                 // llvm.mips.splat.h
    mips_splat_w,                                 // llvm.mips.splat.w
    mips_splati_b,                                // llvm.mips.splati.b
    mips_splati_d,                                // llvm.mips.splati.d
    mips_splati_h,                                // llvm.mips.splati.h
    mips_splati_w,                                // llvm.mips.splati.w
    mips_sra_b,                                   // llvm.mips.sra.b
    mips_sra_d,                                   // llvm.mips.sra.d
    mips_sra_h,                                   // llvm.mips.sra.h
    mips_sra_w,                                   // llvm.mips.sra.w
    mips_srai_b,                                  // llvm.mips.srai.b
    mips_srai_d,                                  // llvm.mips.srai.d
    mips_srai_h,                                  // llvm.mips.srai.h
    mips_srai_w,                                  // llvm.mips.srai.w
    mips_srar_b,                                  // llvm.mips.srar.b
    mips_srar_d,                                  // llvm.mips.srar.d
    mips_srar_h,                                  // llvm.mips.srar.h
    mips_srar_w,                                  // llvm.mips.srar.w
    mips_srari_b,                                 // llvm.mips.srari.b
    mips_srari_d,                                 // llvm.mips.srari.d
    mips_srari_h,                                 // llvm.mips.srari.h
    mips_srari_w,                                 // llvm.mips.srari.w
    mips_srl_b,                                   // llvm.mips.srl.b
    mips_srl_d,                                   // llvm.mips.srl.d
    mips_srl_h,                                   // llvm.mips.srl.h
    mips_srl_w,                                   // llvm.mips.srl.w
    mips_srli_b,                                  // llvm.mips.srli.b
    mips_srli_d,                                  // llvm.mips.srli.d
    mips_srli_h,                                  // llvm.mips.srli.h
    mips_srli_w,                                  // llvm.mips.srli.w
    mips_srlr_b,                                  // llvm.mips.srlr.b
    mips_srlr_d,                                  // llvm.mips.srlr.d
    mips_srlr_h,                                  // llvm.mips.srlr.h
    mips_srlr_w,                                  // llvm.mips.srlr.w
    mips_srlri_b,                                 // llvm.mips.srlri.b
    mips_srlri_d,                                 // llvm.mips.srlri.d
    mips_srlri_h,                                 // llvm.mips.srlri.h
    mips_srlri_w,                                 // llvm.mips.srlri.w
    mips_st_b,                                    // llvm.mips.st.b
    mips_st_d,                                    // llvm.mips.st.d
    mips_st_h,                                    // llvm.mips.st.h
    mips_st_w,                                    // llvm.mips.st.w
    mips_subq_ph,                                 // llvm.mips.subq.ph
    mips_subq_s_ph,                               // llvm.mips.subq.s.ph
    mips_subq_s_w,                                // llvm.mips.subq.s.w
    mips_subqh_ph,                                // llvm.mips.subqh.ph
    mips_subqh_r_ph,                              // llvm.mips.subqh.r.ph
    mips_subqh_r_w,                               // llvm.mips.subqh.r.w
    mips_subqh_w,                                 // llvm.mips.subqh.w
    mips_subs_s_b,                                // llvm.mips.subs.s.b
    mips_subs_s_d,                                // llvm.mips.subs.s.d
    mips_subs_s_h,                                // llvm.mips.subs.s.h
    mips_subs_s_w,                                // llvm.mips.subs.s.w
    mips_subs_u_b,                                // llvm.mips.subs.u.b
    mips_subs_u_d,                                // llvm.mips.subs.u.d
    mips_subs_u_h,                                // llvm.mips.subs.u.h
    mips_subs_u_w,                                // llvm.mips.subs.u.w
    mips_subsus_u_b,                              // llvm.mips.subsus.u.b
    mips_subsus_u_d,                              // llvm.mips.subsus.u.d
    mips_subsus_u_h,                              // llvm.mips.subsus.u.h
    mips_subsus_u_w,                              // llvm.mips.subsus.u.w
    mips_subsuu_s_b,                              // llvm.mips.subsuu.s.b
    mips_subsuu_s_d,                              // llvm.mips.subsuu.s.d
    mips_subsuu_s_h,                              // llvm.mips.subsuu.s.h
    mips_subsuu_s_w,                              // llvm.mips.subsuu.s.w
    mips_subu_ph,                                 // llvm.mips.subu.ph
    mips_subu_qb,                                 // llvm.mips.subu.qb
    mips_subu_s_ph,                               // llvm.mips.subu.s.ph
    mips_subu_s_qb,                               // llvm.mips.subu.s.qb
    mips_subuh_qb,                                // llvm.mips.subuh.qb
    mips_subuh_r_qb,                              // llvm.mips.subuh.r.qb
    mips_subv_b,                                  // llvm.mips.subv.b
    mips_subv_d,                                  // llvm.mips.subv.d
    mips_subv_h,                                  // llvm.mips.subv.h
    mips_subv_w,                                  // llvm.mips.subv.w
    mips_subvi_b,                                 // llvm.mips.subvi.b
    mips_subvi_d,                                 // llvm.mips.subvi.d
    mips_subvi_h,                                 // llvm.mips.subvi.h
    mips_subvi_w,                                 // llvm.mips.subvi.w
    mips_vshf_b,                                  // llvm.mips.vshf.b
    mips_vshf_d,                                  // llvm.mips.vshf.d
    mips_vshf_h,                                  // llvm.mips.vshf.h
    mips_vshf_w,                                  // llvm.mips.vshf.w
    mips_wrdsp,                                   // llvm.mips.wrdsp
    mips_xor_v,                                   // llvm.mips.xor.v
    mips_xori_b,                                  // llvm.mips.xori.b
    nvvm_add_rm_d,                                // llvm.nvvm.add.rm.d
    nvvm_add_rm_f,                                // llvm.nvvm.add.rm.f
    nvvm_add_rm_ftz_f,                            // llvm.nvvm.add.rm.ftz.f
    nvvm_add_rn_d,                                // llvm.nvvm.add.rn.d
    nvvm_add_rn_f,                                // llvm.nvvm.add.rn.f
    nvvm_add_rn_ftz_f,                            // llvm.nvvm.add.rn.ftz.f
    nvvm_add_rp_d,                                // llvm.nvvm.add.rp.d
    nvvm_add_rp_f,                                // llvm.nvvm.add.rp.f
    nvvm_add_rp_ftz_f,                            // llvm.nvvm.add.rp.ftz.f
    nvvm_add_rz_d,                                // llvm.nvvm.add.rz.d
    nvvm_add_rz_f,                                // llvm.nvvm.add.rz.f
    nvvm_add_rz_ftz_f,                            // llvm.nvvm.add.rz.ftz.f
    nvvm_atomic_add_gen_f_cta,                    // llvm.nvvm.atomic.add.gen.f.cta
    nvvm_atomic_add_gen_f_sys,                    // llvm.nvvm.atomic.add.gen.f.sys
    nvvm_atomic_add_gen_i_cta,                    // llvm.nvvm.atomic.add.gen.i.cta
    nvvm_atomic_add_gen_i_sys,                    // llvm.nvvm.atomic.add.gen.i.sys
    nvvm_atomic_and_gen_i_cta,                    // llvm.nvvm.atomic.and.gen.i.cta
    nvvm_atomic_and_gen_i_sys,                    // llvm.nvvm.atomic.and.gen.i.sys
    nvvm_atomic_cas_gen_i_cta,                    // llvm.nvvm.atomic.cas.gen.i.cta
    nvvm_atomic_cas_gen_i_sys,                    // llvm.nvvm.atomic.cas.gen.i.sys
    nvvm_atomic_dec_gen_i_cta,                    // llvm.nvvm.atomic.dec.gen.i.cta
    nvvm_atomic_dec_gen_i_sys,                    // llvm.nvvm.atomic.dec.gen.i.sys
    nvvm_atomic_exch_gen_i_cta,                   // llvm.nvvm.atomic.exch.gen.i.cta
    nvvm_atomic_exch_gen_i_sys,                   // llvm.nvvm.atomic.exch.gen.i.sys
    nvvm_atomic_inc_gen_i_cta,                    // llvm.nvvm.atomic.inc.gen.i.cta
    nvvm_atomic_inc_gen_i_sys,                    // llvm.nvvm.atomic.inc.gen.i.sys
    nvvm_atomic_load_add_f32,                     // llvm.nvvm.atomic.load.add.f32
    nvvm_atomic_load_add_f64,                     // llvm.nvvm.atomic.load.add.f64
    nvvm_atomic_load_dec_32,                      // llvm.nvvm.atomic.load.dec.32
    nvvm_atomic_load_inc_32,                      // llvm.nvvm.atomic.load.inc.32
    nvvm_atomic_max_gen_i_cta,                    // llvm.nvvm.atomic.max.gen.i.cta
    nvvm_atomic_max_gen_i_sys,                    // llvm.nvvm.atomic.max.gen.i.sys
    nvvm_atomic_min_gen_i_cta,                    // llvm.nvvm.atomic.min.gen.i.cta
    nvvm_atomic_min_gen_i_sys,                    // llvm.nvvm.atomic.min.gen.i.sys
    nvvm_atomic_or_gen_i_cta,                     // llvm.nvvm.atomic.or.gen.i.cta
    nvvm_atomic_or_gen_i_sys,                     // llvm.nvvm.atomic.or.gen.i.sys
    nvvm_atomic_xor_gen_i_cta,                    // llvm.nvvm.atomic.xor.gen.i.cta
    nvvm_atomic_xor_gen_i_sys,                    // llvm.nvvm.atomic.xor.gen.i.sys
    nvvm_bar_sync,                                // llvm.nvvm.bar.sync
    nvvm_bar_warp_sync,                           // llvm.nvvm.bar.warp.sync
    nvvm_barrier,                                 // llvm.nvvm.barrier
    nvvm_barrier_n,                               // llvm.nvvm.barrier.n
    nvvm_barrier_sync,                            // llvm.nvvm.barrier.sync
    nvvm_barrier_sync_cnt,                        // llvm.nvvm.barrier.sync.cnt
    nvvm_barrier0,                                // llvm.nvvm.barrier0
    nvvm_barrier0_and,                            // llvm.nvvm.barrier0.and
    nvvm_barrier0_or,                             // llvm.nvvm.barrier0.or
    nvvm_barrier0_popc,                           // llvm.nvvm.barrier0.popc
    nvvm_bitcast_d2ll,                            // llvm.nvvm.bitcast.d2ll
    nvvm_bitcast_f2i,                             // llvm.nvvm.bitcast.f2i
    nvvm_bitcast_i2f,                             // llvm.nvvm.bitcast.i2f
    nvvm_bitcast_ll2d,                            // llvm.nvvm.bitcast.ll2d
    nvvm_ceil_d,                                  // llvm.nvvm.ceil.d
    nvvm_ceil_f,                                  // llvm.nvvm.ceil.f
    nvvm_ceil_ftz_f,                              // llvm.nvvm.ceil.ftz.f
    nvvm_compiler_error,                          // llvm.nvvm.compiler.error
    nvvm_compiler_warn,                           // llvm.nvvm.compiler.warn
    nvvm_cos_approx_f,                            // llvm.nvvm.cos.approx.f
    nvvm_cos_approx_ftz_f,                        // llvm.nvvm.cos.approx.ftz.f
    nvvm_d2f_rm,                                  // llvm.nvvm.d2f.rm
    nvvm_d2f_rm_ftz,                              // llvm.nvvm.d2f.rm.ftz
    nvvm_d2f_rn,                                  // llvm.nvvm.d2f.rn
    nvvm_d2f_rn_ftz,                              // llvm.nvvm.d2f.rn.ftz
    nvvm_d2f_rp,                                  // llvm.nvvm.d2f.rp
    nvvm_d2f_rp_ftz,                              // llvm.nvvm.d2f.rp.ftz
    nvvm_d2f_rz,                                  // llvm.nvvm.d2f.rz
    nvvm_d2f_rz_ftz,                              // llvm.nvvm.d2f.rz.ftz
    nvvm_d2i_hi,                                  // llvm.nvvm.d2i.hi
    nvvm_d2i_lo,                                  // llvm.nvvm.d2i.lo
    nvvm_d2i_rm,                                  // llvm.nvvm.d2i.rm
    nvvm_d2i_rn,                                  // llvm.nvvm.d2i.rn
    nvvm_d2i_rp,                                  // llvm.nvvm.d2i.rp
    nvvm_d2i_rz,                                  // llvm.nvvm.d2i.rz
    nvvm_d2ll_rm,                                 // llvm.nvvm.d2ll.rm
    nvvm_d2ll_rn,                                 // llvm.nvvm.d2ll.rn
    nvvm_d2ll_rp,                                 // llvm.nvvm.d2ll.rp
    nvvm_d2ll_rz,                                 // llvm.nvvm.d2ll.rz
    nvvm_d2ui_rm,                                 // llvm.nvvm.d2ui.rm
    nvvm_d2ui_rn,                                 // llvm.nvvm.d2ui.rn
    nvvm_d2ui_rp,                                 // llvm.nvvm.d2ui.rp
    nvvm_d2ui_rz,                                 // llvm.nvvm.d2ui.rz
    nvvm_d2ull_rm,                                // llvm.nvvm.d2ull.rm
    nvvm_d2ull_rn,                                // llvm.nvvm.d2ull.rn
    nvvm_d2ull_rp,                                // llvm.nvvm.d2ull.rp
    nvvm_d2ull_rz,                                // llvm.nvvm.d2ull.rz
    nvvm_div_approx_f,                            // llvm.nvvm.div.approx.f
    nvvm_div_approx_ftz_f,                        // llvm.nvvm.div.approx.ftz.f
    nvvm_div_rm_d,                                // llvm.nvvm.div.rm.d
    nvvm_div_rm_f,                                // llvm.nvvm.div.rm.f
    nvvm_div_rm_ftz_f,                            // llvm.nvvm.div.rm.ftz.f
    nvvm_div_rn_d,                                // llvm.nvvm.div.rn.d
    nvvm_div_rn_f,                                // llvm.nvvm.div.rn.f
    nvvm_div_rn_ftz_f,                            // llvm.nvvm.div.rn.ftz.f
    nvvm_div_rp_d,                                // llvm.nvvm.div.rp.d
    nvvm_div_rp_f,                                // llvm.nvvm.div.rp.f
    nvvm_div_rp_ftz_f,                            // llvm.nvvm.div.rp.ftz.f
    nvvm_div_rz_d,                                // llvm.nvvm.div.rz.d
    nvvm_div_rz_f,                                // llvm.nvvm.div.rz.f
    nvvm_div_rz_ftz_f,                            // llvm.nvvm.div.rz.ftz.f
    nvvm_ex2_approx_d,                            // llvm.nvvm.ex2.approx.d
    nvvm_ex2_approx_f,                            // llvm.nvvm.ex2.approx.f
    nvvm_ex2_approx_ftz_f,                        // llvm.nvvm.ex2.approx.ftz.f
    nvvm_f2h_rn,                                  // llvm.nvvm.f2h.rn
    nvvm_f2h_rn_ftz,                              // llvm.nvvm.f2h.rn.ftz
    nvvm_f2i_rm,                                  // llvm.nvvm.f2i.rm
    nvvm_f2i_rm_ftz,                              // llvm.nvvm.f2i.rm.ftz
    nvvm_f2i_rn,                                  // llvm.nvvm.f2i.rn
    nvvm_f2i_rn_ftz,                              // llvm.nvvm.f2i.rn.ftz
    nvvm_f2i_rp,                                  // llvm.nvvm.f2i.rp
    nvvm_f2i_rp_ftz,                              // llvm.nvvm.f2i.rp.ftz
    nvvm_f2i_rz,                                  // llvm.nvvm.f2i.rz
    nvvm_f2i_rz_ftz,                              // llvm.nvvm.f2i.rz.ftz
    nvvm_f2ll_rm,                                 // llvm.nvvm.f2ll.rm
    nvvm_f2ll_rm_ftz,                             // llvm.nvvm.f2ll.rm.ftz
    nvvm_f2ll_rn,                                 // llvm.nvvm.f2ll.rn
    nvvm_f2ll_rn_ftz,                             // llvm.nvvm.f2ll.rn.ftz
    nvvm_f2ll_rp,                                 // llvm.nvvm.f2ll.rp
    nvvm_f2ll_rp_ftz,                             // llvm.nvvm.f2ll.rp.ftz
    nvvm_f2ll_rz,                                 // llvm.nvvm.f2ll.rz
    nvvm_f2ll_rz_ftz,                             // llvm.nvvm.f2ll.rz.ftz
    nvvm_f2ui_rm,                                 // llvm.nvvm.f2ui.rm
    nvvm_f2ui_rm_ftz,                             // llvm.nvvm.f2ui.rm.ftz
    nvvm_f2ui_rn,                                 // llvm.nvvm.f2ui.rn
    nvvm_f2ui_rn_ftz,                             // llvm.nvvm.f2ui.rn.ftz
    nvvm_f2ui_rp,                                 // llvm.nvvm.f2ui.rp
    nvvm_f2ui_rp_ftz,                             // llvm.nvvm.f2ui.rp.ftz
    nvvm_f2ui_rz,                                 // llvm.nvvm.f2ui.rz
    nvvm_f2ui_rz_ftz,                             // llvm.nvvm.f2ui.rz.ftz
    nvvm_f2ull_rm,                                // llvm.nvvm.f2ull.rm
    nvvm_f2ull_rm_ftz,                            // llvm.nvvm.f2ull.rm.ftz
    nvvm_f2ull_rn,                                // llvm.nvvm.f2ull.rn
    nvvm_f2ull_rn_ftz,                            // llvm.nvvm.f2ull.rn.ftz
    nvvm_f2ull_rp,                                // llvm.nvvm.f2ull.rp
    nvvm_f2ull_rp_ftz,                            // llvm.nvvm.f2ull.rp.ftz
    nvvm_f2ull_rz,                                // llvm.nvvm.f2ull.rz
    nvvm_f2ull_rz_ftz,                            // llvm.nvvm.f2ull.rz.ftz
    nvvm_fabs_d,                                  // llvm.nvvm.fabs.d
    nvvm_fabs_f,                                  // llvm.nvvm.fabs.f
    nvvm_fabs_ftz_f,                              // llvm.nvvm.fabs.ftz.f
    nvvm_floor_d,                                 // llvm.nvvm.floor.d
    nvvm_floor_f,                                 // llvm.nvvm.floor.f
    nvvm_floor_ftz_f,                             // llvm.nvvm.floor.ftz.f
    nvvm_fma_rm_d,                                // llvm.nvvm.fma.rm.d
    nvvm_fma_rm_f,                                // llvm.nvvm.fma.rm.f
    nvvm_fma_rm_ftz_f,                            // llvm.nvvm.fma.rm.ftz.f
    nvvm_fma_rn_d,                                // llvm.nvvm.fma.rn.d
    nvvm_fma_rn_f,                                // llvm.nvvm.fma.rn.f
    nvvm_fma_rn_ftz_f,                            // llvm.nvvm.fma.rn.ftz.f
    nvvm_fma_rp_d,                                // llvm.nvvm.fma.rp.d
    nvvm_fma_rp_f,                                // llvm.nvvm.fma.rp.f
    nvvm_fma_rp_ftz_f,                            // llvm.nvvm.fma.rp.ftz.f
    nvvm_fma_rz_d,                                // llvm.nvvm.fma.rz.d
    nvvm_fma_rz_f,                                // llvm.nvvm.fma.rz.f
    nvvm_fma_rz_ftz_f,                            // llvm.nvvm.fma.rz.ftz.f
    nvvm_fmax_d,                                  // llvm.nvvm.fmax.d
    nvvm_fmax_f,                                  // llvm.nvvm.fmax.f
    nvvm_fmax_ftz_f,                              // llvm.nvvm.fmax.ftz.f
    nvvm_fmin_d,                                  // llvm.nvvm.fmin.d
    nvvm_fmin_f,                                  // llvm.nvvm.fmin.f
    nvvm_fmin_ftz_f,                              // llvm.nvvm.fmin.ftz.f
    nvvm_fns,                                     // llvm.nvvm.fns
    nvvm_i2d_rm,                                  // llvm.nvvm.i2d.rm
    nvvm_i2d_rn,                                  // llvm.nvvm.i2d.rn
    nvvm_i2d_rp,                                  // llvm.nvvm.i2d.rp
    nvvm_i2d_rz,                                  // llvm.nvvm.i2d.rz
    nvvm_i2f_rm,                                  // llvm.nvvm.i2f.rm
    nvvm_i2f_rn,                                  // llvm.nvvm.i2f.rn
    nvvm_i2f_rp,                                  // llvm.nvvm.i2f.rp
    nvvm_i2f_rz,                                  // llvm.nvvm.i2f.rz
    nvvm_isspacep_const,                          // llvm.nvvm.isspacep.const
    nvvm_isspacep_global,                         // llvm.nvvm.isspacep.global
    nvvm_isspacep_local,                          // llvm.nvvm.isspacep.local
    nvvm_isspacep_shared,                         // llvm.nvvm.isspacep.shared
    nvvm_istypep_sampler,                         // llvm.nvvm.istypep.sampler
    nvvm_istypep_surface,                         // llvm.nvvm.istypep.surface
    nvvm_istypep_texture,                         // llvm.nvvm.istypep.texture
    nvvm_ldg_global_f,                            // llvm.nvvm.ldg.global.f
    nvvm_ldg_global_i,                            // llvm.nvvm.ldg.global.i
    nvvm_ldg_global_p,                            // llvm.nvvm.ldg.global.p
    nvvm_ldu_global_f,                            // llvm.nvvm.ldu.global.f
    nvvm_ldu_global_i,                            // llvm.nvvm.ldu.global.i
    nvvm_ldu_global_p,                            // llvm.nvvm.ldu.global.p
    nvvm_lg2_approx_d,                            // llvm.nvvm.lg2.approx.d
    nvvm_lg2_approx_f,                            // llvm.nvvm.lg2.approx.f
    nvvm_lg2_approx_ftz_f,                        // llvm.nvvm.lg2.approx.ftz.f
    nvvm_ll2d_rm,                                 // llvm.nvvm.ll2d.rm
    nvvm_ll2d_rn,                                 // llvm.nvvm.ll2d.rn
    nvvm_ll2d_rp,                                 // llvm.nvvm.ll2d.rp
    nvvm_ll2d_rz,                                 // llvm.nvvm.ll2d.rz
    nvvm_ll2f_rm,                                 // llvm.nvvm.ll2f.rm
    nvvm_ll2f_rn,                                 // llvm.nvvm.ll2f.rn
    nvvm_ll2f_rp,                                 // llvm.nvvm.ll2f.rp
    nvvm_ll2f_rz,                                 // llvm.nvvm.ll2f.rz
    nvvm_lohi_i2d,                                // llvm.nvvm.lohi.i2d
    nvvm_match_all_sync_i32p,                     // llvm.nvvm.match.all.sync.i32p
    nvvm_match_all_sync_i64p,                     // llvm.nvvm.match.all.sync.i64p
    nvvm_match_any_sync_i32,                      // llvm.nvvm.match.any.sync.i32
    nvvm_match_any_sync_i64,                      // llvm.nvvm.match.any.sync.i64
    nvvm_membar_cta,                              // llvm.nvvm.membar.cta
    nvvm_membar_gl,                               // llvm.nvvm.membar.gl
    nvvm_membar_sys,                              // llvm.nvvm.membar.sys
    nvvm_move_double,                             // llvm.nvvm.move.double
    nvvm_move_float,                              // llvm.nvvm.move.float
    nvvm_move_i16,                                // llvm.nvvm.move.i16
    nvvm_move_i32,                                // llvm.nvvm.move.i32
    nvvm_move_i64,                                // llvm.nvvm.move.i64
    nvvm_move_ptr,                                // llvm.nvvm.move.ptr
    nvvm_mul_rm_d,                                // llvm.nvvm.mul.rm.d
    nvvm_mul_rm_f,                                // llvm.nvvm.mul.rm.f
    nvvm_mul_rm_ftz_f,                            // llvm.nvvm.mul.rm.ftz.f
    nvvm_mul_rn_d,                                // llvm.nvvm.mul.rn.d
    nvvm_mul_rn_f,                                // llvm.nvvm.mul.rn.f
    nvvm_mul_rn_ftz_f,                            // llvm.nvvm.mul.rn.ftz.f
    nvvm_mul_rp_d,                                // llvm.nvvm.mul.rp.d
    nvvm_mul_rp_f,                                // llvm.nvvm.mul.rp.f
    nvvm_mul_rp_ftz_f,                            // llvm.nvvm.mul.rp.ftz.f
    nvvm_mul_rz_d,                                // llvm.nvvm.mul.rz.d
    nvvm_mul_rz_f,                                // llvm.nvvm.mul.rz.f
    nvvm_mul_rz_ftz_f,                            // llvm.nvvm.mul.rz.ftz.f
    nvvm_mul24_i,                                 // llvm.nvvm.mul24.i
    nvvm_mul24_ui,                                // llvm.nvvm.mul24.ui
    nvvm_mulhi_i,                                 // llvm.nvvm.mulhi.i
    nvvm_mulhi_ll,                                // llvm.nvvm.mulhi.ll
    nvvm_mulhi_ui,                                // llvm.nvvm.mulhi.ui
    nvvm_mulhi_ull,                               // llvm.nvvm.mulhi.ull
    nvvm_prmt,                                    // llvm.nvvm.prmt
    nvvm_ptr_constant_to_gen,                     // llvm.nvvm.ptr.constant.to.gen
    nvvm_ptr_gen_to_constant,                     // llvm.nvvm.ptr.gen.to.constant
    nvvm_ptr_gen_to_global,                       // llvm.nvvm.ptr.gen.to.global
    nvvm_ptr_gen_to_local,                        // llvm.nvvm.ptr.gen.to.local
    nvvm_ptr_gen_to_param,                        // llvm.nvvm.ptr.gen.to.param
    nvvm_ptr_gen_to_shared,                       // llvm.nvvm.ptr.gen.to.shared
    nvvm_ptr_global_to_gen,                       // llvm.nvvm.ptr.global.to.gen
    nvvm_ptr_local_to_gen,                        // llvm.nvvm.ptr.local.to.gen
    nvvm_ptr_shared_to_gen,                       // llvm.nvvm.ptr.shared.to.gen
    nvvm_rcp_approx_ftz_d,                        // llvm.nvvm.rcp.approx.ftz.d
    nvvm_rcp_rm_d,                                // llvm.nvvm.rcp.rm.d
    nvvm_rcp_rm_f,                                // llvm.nvvm.rcp.rm.f
    nvvm_rcp_rm_ftz_f,                            // llvm.nvvm.rcp.rm.ftz.f
    nvvm_rcp_rn_d,                                // llvm.nvvm.rcp.rn.d
    nvvm_rcp_rn_f,                                // llvm.nvvm.rcp.rn.f
    nvvm_rcp_rn_ftz_f,                            // llvm.nvvm.rcp.rn.ftz.f
    nvvm_rcp_rp_d,                                // llvm.nvvm.rcp.rp.d
    nvvm_rcp_rp_f,                                // llvm.nvvm.rcp.rp.f
    nvvm_rcp_rp_ftz_f,                            // llvm.nvvm.rcp.rp.ftz.f
    nvvm_rcp_rz_d,                                // llvm.nvvm.rcp.rz.d
    nvvm_rcp_rz_f,                                // llvm.nvvm.rcp.rz.f
    nvvm_rcp_rz_ftz_f,                            // llvm.nvvm.rcp.rz.ftz.f
    nvvm_read_ptx_sreg_clock,                     // llvm.nvvm.read.ptx.sreg.clock
    nvvm_read_ptx_sreg_clock64,                   // llvm.nvvm.read.ptx.sreg.clock64
    nvvm_read_ptx_sreg_ctaid_w,                   // llvm.nvvm.read.ptx.sreg.ctaid.w
    nvvm_read_ptx_sreg_ctaid_x,                   // llvm.nvvm.read.ptx.sreg.ctaid.x
    nvvm_read_ptx_sreg_ctaid_y,                   // llvm.nvvm.read.ptx.sreg.ctaid.y
    nvvm_read_ptx_sreg_ctaid_z,                   // llvm.nvvm.read.ptx.sreg.ctaid.z
    nvvm_read_ptx_sreg_envreg0,                   // llvm.nvvm.read.ptx.sreg.envreg0
    nvvm_read_ptx_sreg_envreg1,                   // llvm.nvvm.read.ptx.sreg.envreg1
    nvvm_read_ptx_sreg_envreg10,                  // llvm.nvvm.read.ptx.sreg.envreg10
    nvvm_read_ptx_sreg_envreg11,                  // llvm.nvvm.read.ptx.sreg.envreg11
    nvvm_read_ptx_sreg_envreg12,                  // llvm.nvvm.read.ptx.sreg.envreg12
    nvvm_read_ptx_sreg_envreg13,                  // llvm.nvvm.read.ptx.sreg.envreg13
    nvvm_read_ptx_sreg_envreg14,                  // llvm.nvvm.read.ptx.sreg.envreg14
    nvvm_read_ptx_sreg_envreg15,                  // llvm.nvvm.read.ptx.sreg.envreg15
    nvvm_read_ptx_sreg_envreg16,                  // llvm.nvvm.read.ptx.sreg.envreg16
    nvvm_read_ptx_sreg_envreg17,                  // llvm.nvvm.read.ptx.sreg.envreg17
    nvvm_read_ptx_sreg_envreg18,                  // llvm.nvvm.read.ptx.sreg.envreg18
    nvvm_read_ptx_sreg_envreg19,                  // llvm.nvvm.read.ptx.sreg.envreg19
    nvvm_read_ptx_sreg_envreg2,                   // llvm.nvvm.read.ptx.sreg.envreg2
    nvvm_read_ptx_sreg_envreg20,                  // llvm.nvvm.read.ptx.sreg.envreg20
    nvvm_read_ptx_sreg_envreg21,                  // llvm.nvvm.read.ptx.sreg.envreg21
    nvvm_read_ptx_sreg_envreg22,                  // llvm.nvvm.read.ptx.sreg.envreg22
    nvvm_read_ptx_sreg_envreg23,                  // llvm.nvvm.read.ptx.sreg.envreg23
    nvvm_read_ptx_sreg_envreg24,                  // llvm.nvvm.read.ptx.sreg.envreg24
    nvvm_read_ptx_sreg_envreg25,                  // llvm.nvvm.read.ptx.sreg.envreg25
    nvvm_read_ptx_sreg_envreg26,                  // llvm.nvvm.read.ptx.sreg.envreg26
    nvvm_read_ptx_sreg_envreg27,                  // llvm.nvvm.read.ptx.sreg.envreg27
    nvvm_read_ptx_sreg_envreg28,                  // llvm.nvvm.read.ptx.sreg.envreg28
    nvvm_read_ptx_sreg_envreg29,                  // llvm.nvvm.read.ptx.sreg.envreg29
    nvvm_read_ptx_sreg_envreg3,                   // llvm.nvvm.read.ptx.sreg.envreg3
    nvvm_read_ptx_sreg_envreg30,                  // llvm.nvvm.read.ptx.sreg.envreg30
    nvvm_read_ptx_sreg_envreg31,                  // llvm.nvvm.read.ptx.sreg.envreg31
    nvvm_read_ptx_sreg_envreg4,                   // llvm.nvvm.read.ptx.sreg.envreg4
    nvvm_read_ptx_sreg_envreg5,                   // llvm.nvvm.read.ptx.sreg.envreg5
    nvvm_read_ptx_sreg_envreg6,                   // llvm.nvvm.read.ptx.sreg.envreg6
    nvvm_read_ptx_sreg_envreg7,                   // llvm.nvvm.read.ptx.sreg.envreg7
    nvvm_read_ptx_sreg_envreg8,                   // llvm.nvvm.read.ptx.sreg.envreg8
    nvvm_read_ptx_sreg_envreg9,                   // llvm.nvvm.read.ptx.sreg.envreg9
    nvvm_read_ptx_sreg_gridid,                    // llvm.nvvm.read.ptx.sreg.gridid
    nvvm_read_ptx_sreg_laneid,                    // llvm.nvvm.read.ptx.sreg.laneid
    nvvm_read_ptx_sreg_lanemask_eq,               // llvm.nvvm.read.ptx.sreg.lanemask.eq
    nvvm_read_ptx_sreg_lanemask_ge,               // llvm.nvvm.read.ptx.sreg.lanemask.ge
    nvvm_read_ptx_sreg_lanemask_gt,               // llvm.nvvm.read.ptx.sreg.lanemask.gt
    nvvm_read_ptx_sreg_lanemask_le,               // llvm.nvvm.read.ptx.sreg.lanemask.le
    nvvm_read_ptx_sreg_lanemask_lt,               // llvm.nvvm.read.ptx.sreg.lanemask.lt
    nvvm_read_ptx_sreg_nctaid_w,                  // llvm.nvvm.read.ptx.sreg.nctaid.w
    nvvm_read_ptx_sreg_nctaid_x,                  // llvm.nvvm.read.ptx.sreg.nctaid.x
    nvvm_read_ptx_sreg_nctaid_y,                  // llvm.nvvm.read.ptx.sreg.nctaid.y
    nvvm_read_ptx_sreg_nctaid_z,                  // llvm.nvvm.read.ptx.sreg.nctaid.z
    nvvm_read_ptx_sreg_nsmid,                     // llvm.nvvm.read.ptx.sreg.nsmid
    nvvm_read_ptx_sreg_ntid_w,                    // llvm.nvvm.read.ptx.sreg.ntid.w
    nvvm_read_ptx_sreg_ntid_x,                    // llvm.nvvm.read.ptx.sreg.ntid.x
    nvvm_read_ptx_sreg_ntid_y,                    // llvm.nvvm.read.ptx.sreg.ntid.y
    nvvm_read_ptx_sreg_ntid_z,                    // llvm.nvvm.read.ptx.sreg.ntid.z
    nvvm_read_ptx_sreg_nwarpid,                   // llvm.nvvm.read.ptx.sreg.nwarpid
    nvvm_read_ptx_sreg_pm0,                       // llvm.nvvm.read.ptx.sreg.pm0
    nvvm_read_ptx_sreg_pm1,                       // llvm.nvvm.read.ptx.sreg.pm1
    nvvm_read_ptx_sreg_pm2,                       // llvm.nvvm.read.ptx.sreg.pm2
    nvvm_read_ptx_sreg_pm3,                       // llvm.nvvm.read.ptx.sreg.pm3
    nvvm_read_ptx_sreg_smid,                      // llvm.nvvm.read.ptx.sreg.smid
    nvvm_read_ptx_sreg_tid_w,                     // llvm.nvvm.read.ptx.sreg.tid.w
    nvvm_read_ptx_sreg_tid_x,                     // llvm.nvvm.read.ptx.sreg.tid.x
    nvvm_read_ptx_sreg_tid_y,                     // llvm.nvvm.read.ptx.sreg.tid.y
    nvvm_read_ptx_sreg_tid_z,                     // llvm.nvvm.read.ptx.sreg.tid.z
    nvvm_read_ptx_sreg_warpid,                    // llvm.nvvm.read.ptx.sreg.warpid
    nvvm_read_ptx_sreg_warpsize,                  // llvm.nvvm.read.ptx.sreg.warpsize
    nvvm_reflect,                                 // llvm.nvvm.reflect
    nvvm_rotate_b32,                              // llvm.nvvm.rotate.b32
    nvvm_rotate_b64,                              // llvm.nvvm.rotate.b64
    nvvm_rotate_right_b64,                        // llvm.nvvm.rotate.right.b64
    nvvm_round_d,                                 // llvm.nvvm.round.d
    nvvm_round_f,                                 // llvm.nvvm.round.f
    nvvm_round_ftz_f,                             // llvm.nvvm.round.ftz.f
    nvvm_rsqrt_approx_d,                          // llvm.nvvm.rsqrt.approx.d
    nvvm_rsqrt_approx_f,                          // llvm.nvvm.rsqrt.approx.f
    nvvm_rsqrt_approx_ftz_f,                      // llvm.nvvm.rsqrt.approx.ftz.f
    nvvm_sad_i,                                   // llvm.nvvm.sad.i
    nvvm_sad_ui,                                  // llvm.nvvm.sad.ui
    nvvm_saturate_d,                              // llvm.nvvm.saturate.d
    nvvm_saturate_f,                              // llvm.nvvm.saturate.f
    nvvm_saturate_ftz_f,                          // llvm.nvvm.saturate.ftz.f
    nvvm_shfl_bfly_f32,                           // llvm.nvvm.shfl.bfly.f32
    nvvm_shfl_bfly_i32,                           // llvm.nvvm.shfl.bfly.i32
    nvvm_shfl_down_f32,                           // llvm.nvvm.shfl.down.f32
    nvvm_shfl_down_i32,                           // llvm.nvvm.shfl.down.i32
    nvvm_shfl_idx_f32,                            // llvm.nvvm.shfl.idx.f32
    nvvm_shfl_idx_i32,                            // llvm.nvvm.shfl.idx.i32
    nvvm_shfl_sync_bfly_f32,                      // llvm.nvvm.shfl.sync.bfly.f32
    nvvm_shfl_sync_bfly_i32,                      // llvm.nvvm.shfl.sync.bfly.i32
    nvvm_shfl_sync_down_f32,                      // llvm.nvvm.shfl.sync.down.f32
    nvvm_shfl_sync_down_i32,                      // llvm.nvvm.shfl.sync.down.i32
    nvvm_shfl_sync_idx_f32,                       // llvm.nvvm.shfl.sync.idx.f32
    nvvm_shfl_sync_idx_i32,                       // llvm.nvvm.shfl.sync.idx.i32
    nvvm_shfl_sync_up_f32,                        // llvm.nvvm.shfl.sync.up.f32
    nvvm_shfl_sync_up_i32,                        // llvm.nvvm.shfl.sync.up.i32
    nvvm_shfl_up_f32,                             // llvm.nvvm.shfl.up.f32
    nvvm_shfl_up_i32,                             // llvm.nvvm.shfl.up.i32
    nvvm_sin_approx_f,                            // llvm.nvvm.sin.approx.f
    nvvm_sin_approx_ftz_f,                        // llvm.nvvm.sin.approx.ftz.f
    nvvm_sqrt_approx_f,                           // llvm.nvvm.sqrt.approx.f
    nvvm_sqrt_approx_ftz_f,                       // llvm.nvvm.sqrt.approx.ftz.f
    nvvm_sqrt_f,                                  // llvm.nvvm.sqrt.f
    nvvm_sqrt_rm_d,                               // llvm.nvvm.sqrt.rm.d
    nvvm_sqrt_rm_f,                               // llvm.nvvm.sqrt.rm.f
    nvvm_sqrt_rm_ftz_f,                           // llvm.nvvm.sqrt.rm.ftz.f
    nvvm_sqrt_rn_d,                               // llvm.nvvm.sqrt.rn.d
    nvvm_sqrt_rn_f,                               // llvm.nvvm.sqrt.rn.f
    nvvm_sqrt_rn_ftz_f,                           // llvm.nvvm.sqrt.rn.ftz.f
    nvvm_sqrt_rp_d,                               // llvm.nvvm.sqrt.rp.d
    nvvm_sqrt_rp_f,                               // llvm.nvvm.sqrt.rp.f
    nvvm_sqrt_rp_ftz_f,                           // llvm.nvvm.sqrt.rp.ftz.f
    nvvm_sqrt_rz_d,                               // llvm.nvvm.sqrt.rz.d
    nvvm_sqrt_rz_f,                               // llvm.nvvm.sqrt.rz.f
    nvvm_sqrt_rz_ftz_f,                           // llvm.nvvm.sqrt.rz.ftz.f
    nvvm_suld_1d_array_i16_clamp,                 // llvm.nvvm.suld.1d.array.i16.clamp
    nvvm_suld_1d_array_i16_trap,                  // llvm.nvvm.suld.1d.array.i16.trap
    nvvm_suld_1d_array_i16_zero,                  // llvm.nvvm.suld.1d.array.i16.zero
    nvvm_suld_1d_array_i32_clamp,                 // llvm.nvvm.suld.1d.array.i32.clamp
    nvvm_suld_1d_array_i32_trap,                  // llvm.nvvm.suld.1d.array.i32.trap
    nvvm_suld_1d_array_i32_zero,                  // llvm.nvvm.suld.1d.array.i32.zero
    nvvm_suld_1d_array_i64_clamp,                 // llvm.nvvm.suld.1d.array.i64.clamp
    nvvm_suld_1d_array_i64_trap,                  // llvm.nvvm.suld.1d.array.i64.trap
    nvvm_suld_1d_array_i64_zero,                  // llvm.nvvm.suld.1d.array.i64.zero
    nvvm_suld_1d_array_i8_clamp,                  // llvm.nvvm.suld.1d.array.i8.clamp
    nvvm_suld_1d_array_i8_trap,                   // llvm.nvvm.suld.1d.array.i8.trap
    nvvm_suld_1d_array_i8_zero,                   // llvm.nvvm.suld.1d.array.i8.zero
    nvvm_suld_1d_array_v2i16_clamp,               // llvm.nvvm.suld.1d.array.v2i16.clamp
    nvvm_suld_1d_array_v2i16_trap,                // llvm.nvvm.suld.1d.array.v2i16.trap
    nvvm_suld_1d_array_v2i16_zero,                // llvm.nvvm.suld.1d.array.v2i16.zero
    nvvm_suld_1d_array_v2i32_clamp,               // llvm.nvvm.suld.1d.array.v2i32.clamp
    nvvm_suld_1d_array_v2i32_trap,                // llvm.nvvm.suld.1d.array.v2i32.trap
    nvvm_suld_1d_array_v2i32_zero,                // llvm.nvvm.suld.1d.array.v2i32.zero
    nvvm_suld_1d_array_v2i64_clamp,               // llvm.nvvm.suld.1d.array.v2i64.clamp
    nvvm_suld_1d_array_v2i64_trap,                // llvm.nvvm.suld.1d.array.v2i64.trap
    nvvm_suld_1d_array_v2i64_zero,                // llvm.nvvm.suld.1d.array.v2i64.zero
    nvvm_suld_1d_array_v2i8_clamp,                // llvm.nvvm.suld.1d.array.v2i8.clamp
    nvvm_suld_1d_array_v2i8_trap,                 // llvm.nvvm.suld.1d.array.v2i8.trap
    nvvm_suld_1d_array_v2i8_zero,                 // llvm.nvvm.suld.1d.array.v2i8.zero
    nvvm_suld_1d_array_v4i16_clamp,               // llvm.nvvm.suld.1d.array.v4i16.clamp
    nvvm_suld_1d_array_v4i16_trap,                // llvm.nvvm.suld.1d.array.v4i16.trap
    nvvm_suld_1d_array_v4i16_zero,                // llvm.nvvm.suld.1d.array.v4i16.zero
    nvvm_suld_1d_array_v4i32_clamp,               // llvm.nvvm.suld.1d.array.v4i32.clamp
    nvvm_suld_1d_array_v4i32_trap,                // llvm.nvvm.suld.1d.array.v4i32.trap
    nvvm_suld_1d_array_v4i32_zero,                // llvm.nvvm.suld.1d.array.v4i32.zero
    nvvm_suld_1d_array_v4i8_clamp,                // llvm.nvvm.suld.1d.array.v4i8.clamp
    nvvm_suld_1d_array_v4i8_trap,                 // llvm.nvvm.suld.1d.array.v4i8.trap
    nvvm_suld_1d_array_v4i8_zero,                 // llvm.nvvm.suld.1d.array.v4i8.zero
    nvvm_suld_1d_i16_clamp,                       // llvm.nvvm.suld.1d.i16.clamp
    nvvm_suld_1d_i16_trap,                        // llvm.nvvm.suld.1d.i16.trap
    nvvm_suld_1d_i16_zero,                        // llvm.nvvm.suld.1d.i16.zero
    nvvm_suld_1d_i32_clamp,                       // llvm.nvvm.suld.1d.i32.clamp
    nvvm_suld_1d_i32_trap,                        // llvm.nvvm.suld.1d.i32.trap
    nvvm_suld_1d_i32_zero,                        // llvm.nvvm.suld.1d.i32.zero
    nvvm_suld_1d_i64_clamp,                       // llvm.nvvm.suld.1d.i64.clamp
    nvvm_suld_1d_i64_trap,                        // llvm.nvvm.suld.1d.i64.trap
    nvvm_suld_1d_i64_zero,                        // llvm.nvvm.suld.1d.i64.zero
    nvvm_suld_1d_i8_clamp,                        // llvm.nvvm.suld.1d.i8.clamp
    nvvm_suld_1d_i8_trap,                         // llvm.nvvm.suld.1d.i8.trap
    nvvm_suld_1d_i8_zero,                         // llvm.nvvm.suld.1d.i8.zero
    nvvm_suld_1d_v2i16_clamp,                     // llvm.nvvm.suld.1d.v2i16.clamp
    nvvm_suld_1d_v2i16_trap,                      // llvm.nvvm.suld.1d.v2i16.trap
    nvvm_suld_1d_v2i16_zero,                      // llvm.nvvm.suld.1d.v2i16.zero
    nvvm_suld_1d_v2i32_clamp,                     // llvm.nvvm.suld.1d.v2i32.clamp
    nvvm_suld_1d_v2i32_trap,                      // llvm.nvvm.suld.1d.v2i32.trap
    nvvm_suld_1d_v2i32_zero,                      // llvm.nvvm.suld.1d.v2i32.zero
    nvvm_suld_1d_v2i64_clamp,                     // llvm.nvvm.suld.1d.v2i64.clamp
    nvvm_suld_1d_v2i64_trap,                      // llvm.nvvm.suld.1d.v2i64.trap
    nvvm_suld_1d_v2i64_zero,                      // llvm.nvvm.suld.1d.v2i64.zero
    nvvm_suld_1d_v2i8_clamp,                      // llvm.nvvm.suld.1d.v2i8.clamp
    nvvm_suld_1d_v2i8_trap,                       // llvm.nvvm.suld.1d.v2i8.trap
    nvvm_suld_1d_v2i8_zero,                       // llvm.nvvm.suld.1d.v2i8.zero
    nvvm_suld_1d_v4i16_clamp,                     // llvm.nvvm.suld.1d.v4i16.clamp
    nvvm_suld_1d_v4i16_trap,                      // llvm.nvvm.suld.1d.v4i16.trap
    nvvm_suld_1d_v4i16_zero,                      // llvm.nvvm.suld.1d.v4i16.zero
    nvvm_suld_1d_v4i32_clamp,                     // llvm.nvvm.suld.1d.v4i32.clamp
    nvvm_suld_1d_v4i32_trap,                      // llvm.nvvm.suld.1d.v4i32.trap
    nvvm_suld_1d_v4i32_zero,                      // llvm.nvvm.suld.1d.v4i32.zero
    nvvm_suld_1d_v4i8_clamp,                      // llvm.nvvm.suld.1d.v4i8.clamp
    nvvm_suld_1d_v4i8_trap,                       // llvm.nvvm.suld.1d.v4i8.trap
    nvvm_suld_1d_v4i8_zero,                       // llvm.nvvm.suld.1d.v4i8.zero
    nvvm_suld_2d_array_i16_clamp,                 // llvm.nvvm.suld.2d.array.i16.clamp
    nvvm_suld_2d_array_i16_trap,                  // llvm.nvvm.suld.2d.array.i16.trap
    nvvm_suld_2d_array_i16_zero,                  // llvm.nvvm.suld.2d.array.i16.zero
    nvvm_suld_2d_array_i32_clamp,                 // llvm.nvvm.suld.2d.array.i32.clamp
    nvvm_suld_2d_array_i32_trap,                  // llvm.nvvm.suld.2d.array.i32.trap
    nvvm_suld_2d_array_i32_zero,                  // llvm.nvvm.suld.2d.array.i32.zero
    nvvm_suld_2d_array_i64_clamp,                 // llvm.nvvm.suld.2d.array.i64.clamp
    nvvm_suld_2d_array_i64_trap,                  // llvm.nvvm.suld.2d.array.i64.trap
    nvvm_suld_2d_array_i64_zero,                  // llvm.nvvm.suld.2d.array.i64.zero
    nvvm_suld_2d_array_i8_clamp,                  // llvm.nvvm.suld.2d.array.i8.clamp
    nvvm_suld_2d_array_i8_trap,                   // llvm.nvvm.suld.2d.array.i8.trap
    nvvm_suld_2d_array_i8_zero,                   // llvm.nvvm.suld.2d.array.i8.zero
    nvvm_suld_2d_array_v2i16_clamp,               // llvm.nvvm.suld.2d.array.v2i16.clamp
    nvvm_suld_2d_array_v2i16_trap,                // llvm.nvvm.suld.2d.array.v2i16.trap
    nvvm_suld_2d_array_v2i16_zero,                // llvm.nvvm.suld.2d.array.v2i16.zero
    nvvm_suld_2d_array_v2i32_clamp,               // llvm.nvvm.suld.2d.array.v2i32.clamp
    nvvm_suld_2d_array_v2i32_trap,                // llvm.nvvm.suld.2d.array.v2i32.trap
    nvvm_suld_2d_array_v2i32_zero,                // llvm.nvvm.suld.2d.array.v2i32.zero
    nvvm_suld_2d_array_v2i64_clamp,               // llvm.nvvm.suld.2d.array.v2i64.clamp
    nvvm_suld_2d_array_v2i64_trap,                // llvm.nvvm.suld.2d.array.v2i64.trap
    nvvm_suld_2d_array_v2i64_zero,                // llvm.nvvm.suld.2d.array.v2i64.zero
    nvvm_suld_2d_array_v2i8_clamp,                // llvm.nvvm.suld.2d.array.v2i8.clamp
    nvvm_suld_2d_array_v2i8_trap,                 // llvm.nvvm.suld.2d.array.v2i8.trap
    nvvm_suld_2d_array_v2i8_zero,                 // llvm.nvvm.suld.2d.array.v2i8.zero
    nvvm_suld_2d_array_v4i16_clamp,               // llvm.nvvm.suld.2d.array.v4i16.clamp
    nvvm_suld_2d_array_v4i16_trap,                // llvm.nvvm.suld.2d.array.v4i16.trap
    nvvm_suld_2d_array_v4i16_zero,                // llvm.nvvm.suld.2d.array.v4i16.zero
    nvvm_suld_2d_array_v4i32_clamp,               // llvm.nvvm.suld.2d.array.v4i32.clamp
    nvvm_suld_2d_array_v4i32_trap,                // llvm.nvvm.suld.2d.array.v4i32.trap
    nvvm_suld_2d_array_v4i32_zero,                // llvm.nvvm.suld.2d.array.v4i32.zero
    nvvm_suld_2d_array_v4i8_clamp,                // llvm.nvvm.suld.2d.array.v4i8.clamp
    nvvm_suld_2d_array_v4i8_trap,                 // llvm.nvvm.suld.2d.array.v4i8.trap
    nvvm_suld_2d_array_v4i8_zero,                 // llvm.nvvm.suld.2d.array.v4i8.zero
    nvvm_suld_2d_i16_clamp,                       // llvm.nvvm.suld.2d.i16.clamp
    nvvm_suld_2d_i16_trap,                        // llvm.nvvm.suld.2d.i16.trap
    nvvm_suld_2d_i16_zero,                        // llvm.nvvm.suld.2d.i16.zero
    nvvm_suld_2d_i32_clamp,                       // llvm.nvvm.suld.2d.i32.clamp
    nvvm_suld_2d_i32_trap,                        // llvm.nvvm.suld.2d.i32.trap
    nvvm_suld_2d_i32_zero,                        // llvm.nvvm.suld.2d.i32.zero
    nvvm_suld_2d_i64_clamp,                       // llvm.nvvm.suld.2d.i64.clamp
    nvvm_suld_2d_i64_trap,                        // llvm.nvvm.suld.2d.i64.trap
    nvvm_suld_2d_i64_zero,                        // llvm.nvvm.suld.2d.i64.zero
    nvvm_suld_2d_i8_clamp,                        // llvm.nvvm.suld.2d.i8.clamp
    nvvm_suld_2d_i8_trap,                         // llvm.nvvm.suld.2d.i8.trap
    nvvm_suld_2d_i8_zero,                         // llvm.nvvm.suld.2d.i8.zero
    nvvm_suld_2d_v2i16_clamp,                     // llvm.nvvm.suld.2d.v2i16.clamp
    nvvm_suld_2d_v2i16_trap,                      // llvm.nvvm.suld.2d.v2i16.trap
    nvvm_suld_2d_v2i16_zero,                      // llvm.nvvm.suld.2d.v2i16.zero
    nvvm_suld_2d_v2i32_clamp,                     // llvm.nvvm.suld.2d.v2i32.clamp
    nvvm_suld_2d_v2i32_trap,                      // llvm.nvvm.suld.2d.v2i32.trap
    nvvm_suld_2d_v2i32_zero,                      // llvm.nvvm.suld.2d.v2i32.zero
    nvvm_suld_2d_v2i64_clamp,                     // llvm.nvvm.suld.2d.v2i64.clamp
    nvvm_suld_2d_v2i64_trap,                      // llvm.nvvm.suld.2d.v2i64.trap
    nvvm_suld_2d_v2i64_zero,                      // llvm.nvvm.suld.2d.v2i64.zero
    nvvm_suld_2d_v2i8_clamp,                      // llvm.nvvm.suld.2d.v2i8.clamp
    nvvm_suld_2d_v2i8_trap,                       // llvm.nvvm.suld.2d.v2i8.trap
    nvvm_suld_2d_v2i8_zero,                       // llvm.nvvm.suld.2d.v2i8.zero
    nvvm_suld_2d_v4i16_clamp,                     // llvm.nvvm.suld.2d.v4i16.clamp
    nvvm_suld_2d_v4i16_trap,                      // llvm.nvvm.suld.2d.v4i16.trap
    nvvm_suld_2d_v4i16_zero,                      // llvm.nvvm.suld.2d.v4i16.zero
    nvvm_suld_2d_v4i32_clamp,                     // llvm.nvvm.suld.2d.v4i32.clamp
    nvvm_suld_2d_v4i32_trap,                      // llvm.nvvm.suld.2d.v4i32.trap
    nvvm_suld_2d_v4i32_zero,                      // llvm.nvvm.suld.2d.v4i32.zero
    nvvm_suld_2d_v4i8_clamp,                      // llvm.nvvm.suld.2d.v4i8.clamp
    nvvm_suld_2d_v4i8_trap,                       // llvm.nvvm.suld.2d.v4i8.trap
    nvvm_suld_2d_v4i8_zero,                       // llvm.nvvm.suld.2d.v4i8.zero
    nvvm_suld_3d_i16_clamp,                       // llvm.nvvm.suld.3d.i16.clamp
    nvvm_suld_3d_i16_trap,                        // llvm.nvvm.suld.3d.i16.trap
    nvvm_suld_3d_i16_zero,                        // llvm.nvvm.suld.3d.i16.zero
    nvvm_suld_3d_i32_clamp,                       // llvm.nvvm.suld.3d.i32.clamp
    nvvm_suld_3d_i32_trap,                        // llvm.nvvm.suld.3d.i32.trap
    nvvm_suld_3d_i32_zero,                        // llvm.nvvm.suld.3d.i32.zero
    nvvm_suld_3d_i64_clamp,                       // llvm.nvvm.suld.3d.i64.clamp
    nvvm_suld_3d_i64_trap,                        // llvm.nvvm.suld.3d.i64.trap
    nvvm_suld_3d_i64_zero,                        // llvm.nvvm.suld.3d.i64.zero
    nvvm_suld_3d_i8_clamp,                        // llvm.nvvm.suld.3d.i8.clamp
    nvvm_suld_3d_i8_trap,                         // llvm.nvvm.suld.3d.i8.trap
    nvvm_suld_3d_i8_zero,                         // llvm.nvvm.suld.3d.i8.zero
    nvvm_suld_3d_v2i16_clamp,                     // llvm.nvvm.suld.3d.v2i16.clamp
    nvvm_suld_3d_v2i16_trap,                      // llvm.nvvm.suld.3d.v2i16.trap
    nvvm_suld_3d_v2i16_zero,                      // llvm.nvvm.suld.3d.v2i16.zero
    nvvm_suld_3d_v2i32_clamp,                     // llvm.nvvm.suld.3d.v2i32.clamp
    nvvm_suld_3d_v2i32_trap,                      // llvm.nvvm.suld.3d.v2i32.trap
    nvvm_suld_3d_v2i32_zero,                      // llvm.nvvm.suld.3d.v2i32.zero
    nvvm_suld_3d_v2i64_clamp,                     // llvm.nvvm.suld.3d.v2i64.clamp
    nvvm_suld_3d_v2i64_trap,                      // llvm.nvvm.suld.3d.v2i64.trap
    nvvm_suld_3d_v2i64_zero,                      // llvm.nvvm.suld.3d.v2i64.zero
    nvvm_suld_3d_v2i8_clamp,                      // llvm.nvvm.suld.3d.v2i8.clamp
    nvvm_suld_3d_v2i8_trap,                       // llvm.nvvm.suld.3d.v2i8.trap
    nvvm_suld_3d_v2i8_zero,                       // llvm.nvvm.suld.3d.v2i8.zero
    nvvm_suld_3d_v4i16_clamp,                     // llvm.nvvm.suld.3d.v4i16.clamp
    nvvm_suld_3d_v4i16_trap,                      // llvm.nvvm.suld.3d.v4i16.trap
    nvvm_suld_3d_v4i16_zero,                      // llvm.nvvm.suld.3d.v4i16.zero
    nvvm_suld_3d_v4i32_clamp,                     // llvm.nvvm.suld.3d.v4i32.clamp
    nvvm_suld_3d_v4i32_trap,                      // llvm.nvvm.suld.3d.v4i32.trap
    nvvm_suld_3d_v4i32_zero,                      // llvm.nvvm.suld.3d.v4i32.zero
    nvvm_suld_3d_v4i8_clamp,                      // llvm.nvvm.suld.3d.v4i8.clamp
    nvvm_suld_3d_v4i8_trap,                       // llvm.nvvm.suld.3d.v4i8.trap
    nvvm_suld_3d_v4i8_zero,                       // llvm.nvvm.suld.3d.v4i8.zero
    nvvm_suq_array_size,                          // llvm.nvvm.suq.array.size
    nvvm_suq_channel_data_type,                   // llvm.nvvm.suq.channel.data.type
    nvvm_suq_channel_order,                       // llvm.nvvm.suq.channel.order
    nvvm_suq_depth,                               // llvm.nvvm.suq.depth
    nvvm_suq_height,                              // llvm.nvvm.suq.height
    nvvm_suq_width,                               // llvm.nvvm.suq.width
    nvvm_sust_b_1d_array_i16_clamp,               // llvm.nvvm.sust.b.1d.array.i16.clamp
    nvvm_sust_b_1d_array_i16_trap,                // llvm.nvvm.sust.b.1d.array.i16.trap
    nvvm_sust_b_1d_array_i16_zero,                // llvm.nvvm.sust.b.1d.array.i16.zero
    nvvm_sust_b_1d_array_i32_clamp,               // llvm.nvvm.sust.b.1d.array.i32.clamp
    nvvm_sust_b_1d_array_i32_trap,                // llvm.nvvm.sust.b.1d.array.i32.trap
    nvvm_sust_b_1d_array_i32_zero,                // llvm.nvvm.sust.b.1d.array.i32.zero
    nvvm_sust_b_1d_array_i64_clamp,               // llvm.nvvm.sust.b.1d.array.i64.clamp
    nvvm_sust_b_1d_array_i64_trap,                // llvm.nvvm.sust.b.1d.array.i64.trap
    nvvm_sust_b_1d_array_i64_zero,                // llvm.nvvm.sust.b.1d.array.i64.zero
    nvvm_sust_b_1d_array_i8_clamp,                // llvm.nvvm.sust.b.1d.array.i8.clamp
    nvvm_sust_b_1d_array_i8_trap,                 // llvm.nvvm.sust.b.1d.array.i8.trap
    nvvm_sust_b_1d_array_i8_zero,                 // llvm.nvvm.sust.b.1d.array.i8.zero
    nvvm_sust_b_1d_array_v2i16_clamp,             // llvm.nvvm.sust.b.1d.array.v2i16.clamp
    nvvm_sust_b_1d_array_v2i16_trap,              // llvm.nvvm.sust.b.1d.array.v2i16.trap
    nvvm_sust_b_1d_array_v2i16_zero,              // llvm.nvvm.sust.b.1d.array.v2i16.zero
    nvvm_sust_b_1d_array_v2i32_clamp,             // llvm.nvvm.sust.b.1d.array.v2i32.clamp
    nvvm_sust_b_1d_array_v2i32_trap,              // llvm.nvvm.sust.b.1d.array.v2i32.trap
    nvvm_sust_b_1d_array_v2i32_zero,              // llvm.nvvm.sust.b.1d.array.v2i32.zero
    nvvm_sust_b_1d_array_v2i64_clamp,             // llvm.nvvm.sust.b.1d.array.v2i64.clamp
    nvvm_sust_b_1d_array_v2i64_trap,              // llvm.nvvm.sust.b.1d.array.v2i64.trap
    nvvm_sust_b_1d_array_v2i64_zero,              // llvm.nvvm.sust.b.1d.array.v2i64.zero
    nvvm_sust_b_1d_array_v2i8_clamp,              // llvm.nvvm.sust.b.1d.array.v2i8.clamp
    nvvm_sust_b_1d_array_v2i8_trap,               // llvm.nvvm.sust.b.1d.array.v2i8.trap
    nvvm_sust_b_1d_array_v2i8_zero,               // llvm.nvvm.sust.b.1d.array.v2i8.zero
    nvvm_sust_b_1d_array_v4i16_clamp,             // llvm.nvvm.sust.b.1d.array.v4i16.clamp
    nvvm_sust_b_1d_array_v4i16_trap,              // llvm.nvvm.sust.b.1d.array.v4i16.trap
    nvvm_sust_b_1d_array_v4i16_zero,              // llvm.nvvm.sust.b.1d.array.v4i16.zero
    nvvm_sust_b_1d_array_v4i32_clamp,             // llvm.nvvm.sust.b.1d.array.v4i32.clamp
    nvvm_sust_b_1d_array_v4i32_trap,              // llvm.nvvm.sust.b.1d.array.v4i32.trap
    nvvm_sust_b_1d_array_v4i32_zero,              // llvm.nvvm.sust.b.1d.array.v4i32.zero
    nvvm_sust_b_1d_array_v4i8_clamp,              // llvm.nvvm.sust.b.1d.array.v4i8.clamp
    nvvm_sust_b_1d_array_v4i8_trap,               // llvm.nvvm.sust.b.1d.array.v4i8.trap
    nvvm_sust_b_1d_array_v4i8_zero,               // llvm.nvvm.sust.b.1d.array.v4i8.zero
    nvvm_sust_b_1d_i16_clamp,                     // llvm.nvvm.sust.b.1d.i16.clamp
    nvvm_sust_b_1d_i16_trap,                      // llvm.nvvm.sust.b.1d.i16.trap
    nvvm_sust_b_1d_i16_zero,                      // llvm.nvvm.sust.b.1d.i16.zero
    nvvm_sust_b_1d_i32_clamp,                     // llvm.nvvm.sust.b.1d.i32.clamp
    nvvm_sust_b_1d_i32_trap,                      // llvm.nvvm.sust.b.1d.i32.trap
    nvvm_sust_b_1d_i32_zero,                      // llvm.nvvm.sust.b.1d.i32.zero
    nvvm_sust_b_1d_i64_clamp,                     // llvm.nvvm.sust.b.1d.i64.clamp
    nvvm_sust_b_1d_i64_trap,                      // llvm.nvvm.sust.b.1d.i64.trap
    nvvm_sust_b_1d_i64_zero,                      // llvm.nvvm.sust.b.1d.i64.zero
    nvvm_sust_b_1d_i8_clamp,                      // llvm.nvvm.sust.b.1d.i8.clamp
    nvvm_sust_b_1d_i8_trap,                       // llvm.nvvm.sust.b.1d.i8.trap
    nvvm_sust_b_1d_i8_zero,                       // llvm.nvvm.sust.b.1d.i8.zero
    nvvm_sust_b_1d_v2i16_clamp,                   // llvm.nvvm.sust.b.1d.v2i16.clamp
    nvvm_sust_b_1d_v2i16_trap,                    // llvm.nvvm.sust.b.1d.v2i16.trap
    nvvm_sust_b_1d_v2i16_zero,                    // llvm.nvvm.sust.b.1d.v2i16.zero
    nvvm_sust_b_1d_v2i32_clamp,                   // llvm.nvvm.sust.b.1d.v2i32.clamp
    nvvm_sust_b_1d_v2i32_trap,                    // llvm.nvvm.sust.b.1d.v2i32.trap
    nvvm_sust_b_1d_v2i32_zero,                    // llvm.nvvm.sust.b.1d.v2i32.zero
    nvvm_sust_b_1d_v2i64_clamp,                   // llvm.nvvm.sust.b.1d.v2i64.clamp
    nvvm_sust_b_1d_v2i64_trap,                    // llvm.nvvm.sust.b.1d.v2i64.trap
    nvvm_sust_b_1d_v2i64_zero,                    // llvm.nvvm.sust.b.1d.v2i64.zero
    nvvm_sust_b_1d_v2i8_clamp,                    // llvm.nvvm.sust.b.1d.v2i8.clamp
    nvvm_sust_b_1d_v2i8_trap,                     // llvm.nvvm.sust.b.1d.v2i8.trap
    nvvm_sust_b_1d_v2i8_zero,                     // llvm.nvvm.sust.b.1d.v2i8.zero
    nvvm_sust_b_1d_v4i16_clamp,                   // llvm.nvvm.sust.b.1d.v4i16.clamp
    nvvm_sust_b_1d_v4i16_trap,                    // llvm.nvvm.sust.b.1d.v4i16.trap
    nvvm_sust_b_1d_v4i16_zero,                    // llvm.nvvm.sust.b.1d.v4i16.zero
    nvvm_sust_b_1d_v4i32_clamp,                   // llvm.nvvm.sust.b.1d.v4i32.clamp
    nvvm_sust_b_1d_v4i32_trap,                    // llvm.nvvm.sust.b.1d.v4i32.trap
    nvvm_sust_b_1d_v4i32_zero,                    // llvm.nvvm.sust.b.1d.v4i32.zero
    nvvm_sust_b_1d_v4i8_clamp,                    // llvm.nvvm.sust.b.1d.v4i8.clamp
    nvvm_sust_b_1d_v4i8_trap,                     // llvm.nvvm.sust.b.1d.v4i8.trap
    nvvm_sust_b_1d_v4i8_zero,                     // llvm.nvvm.sust.b.1d.v4i8.zero
    nvvm_sust_b_2d_array_i16_clamp,               // llvm.nvvm.sust.b.2d.array.i16.clamp
    nvvm_sust_b_2d_array_i16_trap,                // llvm.nvvm.sust.b.2d.array.i16.trap
    nvvm_sust_b_2d_array_i16_zero,                // llvm.nvvm.sust.b.2d.array.i16.zero
    nvvm_sust_b_2d_array_i32_clamp,               // llvm.nvvm.sust.b.2d.array.i32.clamp
    nvvm_sust_b_2d_array_i32_trap,                // llvm.nvvm.sust.b.2d.array.i32.trap
    nvvm_sust_b_2d_array_i32_zero,                // llvm.nvvm.sust.b.2d.array.i32.zero
    nvvm_sust_b_2d_array_i64_clamp,               // llvm.nvvm.sust.b.2d.array.i64.clamp
    nvvm_sust_b_2d_array_i64_trap,                // llvm.nvvm.sust.b.2d.array.i64.trap
    nvvm_sust_b_2d_array_i64_zero,                // llvm.nvvm.sust.b.2d.array.i64.zero
    nvvm_sust_b_2d_array_i8_clamp,                // llvm.nvvm.sust.b.2d.array.i8.clamp
    nvvm_sust_b_2d_array_i8_trap,                 // llvm.nvvm.sust.b.2d.array.i8.trap
    nvvm_sust_b_2d_array_i8_zero,                 // llvm.nvvm.sust.b.2d.array.i8.zero
    nvvm_sust_b_2d_array_v2i16_clamp,             // llvm.nvvm.sust.b.2d.array.v2i16.clamp
    nvvm_sust_b_2d_array_v2i16_trap,              // llvm.nvvm.sust.b.2d.array.v2i16.trap
    nvvm_sust_b_2d_array_v2i16_zero,              // llvm.nvvm.sust.b.2d.array.v2i16.zero
    nvvm_sust_b_2d_array_v2i32_clamp,             // llvm.nvvm.sust.b.2d.array.v2i32.clamp
    nvvm_sust_b_2d_array_v2i32_trap,              // llvm.nvvm.sust.b.2d.array.v2i32.trap
    nvvm_sust_b_2d_array_v2i32_zero,              // llvm.nvvm.sust.b.2d.array.v2i32.zero
    nvvm_sust_b_2d_array_v2i64_clamp,             // llvm.nvvm.sust.b.2d.array.v2i64.clamp
    nvvm_sust_b_2d_array_v2i64_trap,              // llvm.nvvm.sust.b.2d.array.v2i64.trap
    nvvm_sust_b_2d_array_v2i64_zero,              // llvm.nvvm.sust.b.2d.array.v2i64.zero
    nvvm_sust_b_2d_array_v2i8_clamp,              // llvm.nvvm.sust.b.2d.array.v2i8.clamp
    nvvm_sust_b_2d_array_v2i8_trap,               // llvm.nvvm.sust.b.2d.array.v2i8.trap
    nvvm_sust_b_2d_array_v2i8_zero,               // llvm.nvvm.sust.b.2d.array.v2i8.zero
    nvvm_sust_b_2d_array_v4i16_clamp,             // llvm.nvvm.sust.b.2d.array.v4i16.clamp
    nvvm_sust_b_2d_array_v4i16_trap,              // llvm.nvvm.sust.b.2d.array.v4i16.trap
    nvvm_sust_b_2d_array_v4i16_zero,              // llvm.nvvm.sust.b.2d.array.v4i16.zero
    nvvm_sust_b_2d_array_v4i32_clamp,             // llvm.nvvm.sust.b.2d.array.v4i32.clamp
    nvvm_sust_b_2d_array_v4i32_trap,              // llvm.nvvm.sust.b.2d.array.v4i32.trap
    nvvm_sust_b_2d_array_v4i32_zero,              // llvm.nvvm.sust.b.2d.array.v4i32.zero
    nvvm_sust_b_2d_array_v4i8_clamp,              // llvm.nvvm.sust.b.2d.array.v4i8.clamp
    nvvm_sust_b_2d_array_v4i8_trap,               // llvm.nvvm.sust.b.2d.array.v4i8.trap
    nvvm_sust_b_2d_array_v4i8_zero,               // llvm.nvvm.sust.b.2d.array.v4i8.zero
    nvvm_sust_b_2d_i16_clamp,                     // llvm.nvvm.sust.b.2d.i16.clamp
    nvvm_sust_b_2d_i16_trap,                      // llvm.nvvm.sust.b.2d.i16.trap
    nvvm_sust_b_2d_i16_zero,                      // llvm.nvvm.sust.b.2d.i16.zero
    nvvm_sust_b_2d_i32_clamp,                     // llvm.nvvm.sust.b.2d.i32.clamp
    nvvm_sust_b_2d_i32_trap,                      // llvm.nvvm.sust.b.2d.i32.trap
    nvvm_sust_b_2d_i32_zero,                      // llvm.nvvm.sust.b.2d.i32.zero
    nvvm_sust_b_2d_i64_clamp,                     // llvm.nvvm.sust.b.2d.i64.clamp
    nvvm_sust_b_2d_i64_trap,                      // llvm.nvvm.sust.b.2d.i64.trap
    nvvm_sust_b_2d_i64_zero,                      // llvm.nvvm.sust.b.2d.i64.zero
    nvvm_sust_b_2d_i8_clamp,                      // llvm.nvvm.sust.b.2d.i8.clamp
    nvvm_sust_b_2d_i8_trap,                       // llvm.nvvm.sust.b.2d.i8.trap
    nvvm_sust_b_2d_i8_zero,                       // llvm.nvvm.sust.b.2d.i8.zero
    nvvm_sust_b_2d_v2i16_clamp,                   // llvm.nvvm.sust.b.2d.v2i16.clamp
    nvvm_sust_b_2d_v2i16_trap,                    // llvm.nvvm.sust.b.2d.v2i16.trap
    nvvm_sust_b_2d_v2i16_zero,                    // llvm.nvvm.sust.b.2d.v2i16.zero
    nvvm_sust_b_2d_v2i32_clamp,                   // llvm.nvvm.sust.b.2d.v2i32.clamp
    nvvm_sust_b_2d_v2i32_trap,                    // llvm.nvvm.sust.b.2d.v2i32.trap
    nvvm_sust_b_2d_v2i32_zero,                    // llvm.nvvm.sust.b.2d.v2i32.zero
    nvvm_sust_b_2d_v2i64_clamp,                   // llvm.nvvm.sust.b.2d.v2i64.clamp
    nvvm_sust_b_2d_v2i64_trap,                    // llvm.nvvm.sust.b.2d.v2i64.trap
    nvvm_sust_b_2d_v2i64_zero,                    // llvm.nvvm.sust.b.2d.v2i64.zero
    nvvm_sust_b_2d_v2i8_clamp,                    // llvm.nvvm.sust.b.2d.v2i8.clamp
    nvvm_sust_b_2d_v2i8_trap,                     // llvm.nvvm.sust.b.2d.v2i8.trap
    nvvm_sust_b_2d_v2i8_zero,                     // llvm.nvvm.sust.b.2d.v2i8.zero
    nvvm_sust_b_2d_v4i16_clamp,                   // llvm.nvvm.sust.b.2d.v4i16.clamp
    nvvm_sust_b_2d_v4i16_trap,                    // llvm.nvvm.sust.b.2d.v4i16.trap
    nvvm_sust_b_2d_v4i16_zero,                    // llvm.nvvm.sust.b.2d.v4i16.zero
    nvvm_sust_b_2d_v4i32_clamp,                   // llvm.nvvm.sust.b.2d.v4i32.clamp
    nvvm_sust_b_2d_v4i32_trap,                    // llvm.nvvm.sust.b.2d.v4i32.trap
    nvvm_sust_b_2d_v4i32_zero,                    // llvm.nvvm.sust.b.2d.v4i32.zero
    nvvm_sust_b_2d_v4i8_clamp,                    // llvm.nvvm.sust.b.2d.v4i8.clamp
    nvvm_sust_b_2d_v4i8_trap,                     // llvm.nvvm.sust.b.2d.v4i8.trap
    nvvm_sust_b_2d_v4i8_zero,                     // llvm.nvvm.sust.b.2d.v4i8.zero
    nvvm_sust_b_3d_i16_clamp,                     // llvm.nvvm.sust.b.3d.i16.clamp
    nvvm_sust_b_3d_i16_trap,                      // llvm.nvvm.sust.b.3d.i16.trap
    nvvm_sust_b_3d_i16_zero,                      // llvm.nvvm.sust.b.3d.i16.zero
    nvvm_sust_b_3d_i32_clamp,                     // llvm.nvvm.sust.b.3d.i32.clamp
    nvvm_sust_b_3d_i32_trap,                      // llvm.nvvm.sust.b.3d.i32.trap
    nvvm_sust_b_3d_i32_zero,                      // llvm.nvvm.sust.b.3d.i32.zero
    nvvm_sust_b_3d_i64_clamp,                     // llvm.nvvm.sust.b.3d.i64.clamp
    nvvm_sust_b_3d_i64_trap,                      // llvm.nvvm.sust.b.3d.i64.trap
    nvvm_sust_b_3d_i64_zero,                      // llvm.nvvm.sust.b.3d.i64.zero
    nvvm_sust_b_3d_i8_clamp,                      // llvm.nvvm.sust.b.3d.i8.clamp
    nvvm_sust_b_3d_i8_trap,                       // llvm.nvvm.sust.b.3d.i8.trap
    nvvm_sust_b_3d_i8_zero,                       // llvm.nvvm.sust.b.3d.i8.zero
    nvvm_sust_b_3d_v2i16_clamp,                   // llvm.nvvm.sust.b.3d.v2i16.clamp
    nvvm_sust_b_3d_v2i16_trap,                    // llvm.nvvm.sust.b.3d.v2i16.trap
    nvvm_sust_b_3d_v2i16_zero,                    // llvm.nvvm.sust.b.3d.v2i16.zero
    nvvm_sust_b_3d_v2i32_clamp,                   // llvm.nvvm.sust.b.3d.v2i32.clamp
    nvvm_sust_b_3d_v2i32_trap,                    // llvm.nvvm.sust.b.3d.v2i32.trap
    nvvm_sust_b_3d_v2i32_zero,                    // llvm.nvvm.sust.b.3d.v2i32.zero
    nvvm_sust_b_3d_v2i64_clamp,                   // llvm.nvvm.sust.b.3d.v2i64.clamp
    nvvm_sust_b_3d_v2i64_trap,                    // llvm.nvvm.sust.b.3d.v2i64.trap
    nvvm_sust_b_3d_v2i64_zero,                    // llvm.nvvm.sust.b.3d.v2i64.zero
    nvvm_sust_b_3d_v2i8_clamp,                    // llvm.nvvm.sust.b.3d.v2i8.clamp
    nvvm_sust_b_3d_v2i8_trap,                     // llvm.nvvm.sust.b.3d.v2i8.trap
    nvvm_sust_b_3d_v2i8_zero,                     // llvm.nvvm.sust.b.3d.v2i8.zero
    nvvm_sust_b_3d_v4i16_clamp,                   // llvm.nvvm.sust.b.3d.v4i16.clamp
    nvvm_sust_b_3d_v4i16_trap,                    // llvm.nvvm.sust.b.3d.v4i16.trap
    nvvm_sust_b_3d_v4i16_zero,                    // llvm.nvvm.sust.b.3d.v4i16.zero
    nvvm_sust_b_3d_v4i32_clamp,                   // llvm.nvvm.sust.b.3d.v4i32.clamp
    nvvm_sust_b_3d_v4i32_trap,                    // llvm.nvvm.sust.b.3d.v4i32.trap
    nvvm_sust_b_3d_v4i32_zero,                    // llvm.nvvm.sust.b.3d.v4i32.zero
    nvvm_sust_b_3d_v4i8_clamp,                    // llvm.nvvm.sust.b.3d.v4i8.clamp
    nvvm_sust_b_3d_v4i8_trap,                     // llvm.nvvm.sust.b.3d.v4i8.trap
    nvvm_sust_b_3d_v4i8_zero,                     // llvm.nvvm.sust.b.3d.v4i8.zero
    nvvm_sust_p_1d_array_i16_trap,                // llvm.nvvm.sust.p.1d.array.i16.trap
    nvvm_sust_p_1d_array_i32_trap,                // llvm.nvvm.sust.p.1d.array.i32.trap
    nvvm_sust_p_1d_array_i8_trap,                 // llvm.nvvm.sust.p.1d.array.i8.trap
    nvvm_sust_p_1d_array_v2i16_trap,              // llvm.nvvm.sust.p.1d.array.v2i16.trap
    nvvm_sust_p_1d_array_v2i32_trap,              // llvm.nvvm.sust.p.1d.array.v2i32.trap
    nvvm_sust_p_1d_array_v2i8_trap,               // llvm.nvvm.sust.p.1d.array.v2i8.trap
    nvvm_sust_p_1d_array_v4i16_trap,              // llvm.nvvm.sust.p.1d.array.v4i16.trap
    nvvm_sust_p_1d_array_v4i32_trap,              // llvm.nvvm.sust.p.1d.array.v4i32.trap
    nvvm_sust_p_1d_array_v4i8_trap,               // llvm.nvvm.sust.p.1d.array.v4i8.trap
    nvvm_sust_p_1d_i16_trap,                      // llvm.nvvm.sust.p.1d.i16.trap
    nvvm_sust_p_1d_i32_trap,                      // llvm.nvvm.sust.p.1d.i32.trap
    nvvm_sust_p_1d_i8_trap,                       // llvm.nvvm.sust.p.1d.i8.trap
    nvvm_sust_p_1d_v2i16_trap,                    // llvm.nvvm.sust.p.1d.v2i16.trap
    nvvm_sust_p_1d_v2i32_trap,                    // llvm.nvvm.sust.p.1d.v2i32.trap
    nvvm_sust_p_1d_v2i8_trap,                     // llvm.nvvm.sust.p.1d.v2i8.trap
    nvvm_sust_p_1d_v4i16_trap,                    // llvm.nvvm.sust.p.1d.v4i16.trap
    nvvm_sust_p_1d_v4i32_trap,                    // llvm.nvvm.sust.p.1d.v4i32.trap
    nvvm_sust_p_1d_v4i8_trap,                     // llvm.nvvm.sust.p.1d.v4i8.trap
    nvvm_sust_p_2d_array_i16_trap,                // llvm.nvvm.sust.p.2d.array.i16.trap
    nvvm_sust_p_2d_array_i32_trap,                // llvm.nvvm.sust.p.2d.array.i32.trap
    nvvm_sust_p_2d_array_i8_trap,                 // llvm.nvvm.sust.p.2d.array.i8.trap
    nvvm_sust_p_2d_array_v2i16_trap,              // llvm.nvvm.sust.p.2d.array.v2i16.trap
    nvvm_sust_p_2d_array_v2i32_trap,              // llvm.nvvm.sust.p.2d.array.v2i32.trap
    nvvm_sust_p_2d_array_v2i8_trap,               // llvm.nvvm.sust.p.2d.array.v2i8.trap
    nvvm_sust_p_2d_array_v4i16_trap,              // llvm.nvvm.sust.p.2d.array.v4i16.trap
    nvvm_sust_p_2d_array_v4i32_trap,              // llvm.nvvm.sust.p.2d.array.v4i32.trap
    nvvm_sust_p_2d_array_v4i8_trap,               // llvm.nvvm.sust.p.2d.array.v4i8.trap
    nvvm_sust_p_2d_i16_trap,                      // llvm.nvvm.sust.p.2d.i16.trap
    nvvm_sust_p_2d_i32_trap,                      // llvm.nvvm.sust.p.2d.i32.trap
    nvvm_sust_p_2d_i8_trap,                       // llvm.nvvm.sust.p.2d.i8.trap
    nvvm_sust_p_2d_v2i16_trap,                    // llvm.nvvm.sust.p.2d.v2i16.trap
    nvvm_sust_p_2d_v2i32_trap,                    // llvm.nvvm.sust.p.2d.v2i32.trap
    nvvm_sust_p_2d_v2i8_trap,                     // llvm.nvvm.sust.p.2d.v2i8.trap
    nvvm_sust_p_2d_v4i16_trap,                    // llvm.nvvm.sust.p.2d.v4i16.trap
    nvvm_sust_p_2d_v4i32_trap,                    // llvm.nvvm.sust.p.2d.v4i32.trap
    nvvm_sust_p_2d_v4i8_trap,                     // llvm.nvvm.sust.p.2d.v4i8.trap
    nvvm_sust_p_3d_i16_trap,                      // llvm.nvvm.sust.p.3d.i16.trap
    nvvm_sust_p_3d_i32_trap,                      // llvm.nvvm.sust.p.3d.i32.trap
    nvvm_sust_p_3d_i8_trap,                       // llvm.nvvm.sust.p.3d.i8.trap
    nvvm_sust_p_3d_v2i16_trap,                    // llvm.nvvm.sust.p.3d.v2i16.trap
    nvvm_sust_p_3d_v2i32_trap,                    // llvm.nvvm.sust.p.3d.v2i32.trap
    nvvm_sust_p_3d_v2i8_trap,                     // llvm.nvvm.sust.p.3d.v2i8.trap
    nvvm_sust_p_3d_v4i16_trap,                    // llvm.nvvm.sust.p.3d.v4i16.trap
    nvvm_sust_p_3d_v4i32_trap,                    // llvm.nvvm.sust.p.3d.v4i32.trap
    nvvm_sust_p_3d_v4i8_trap,                     // llvm.nvvm.sust.p.3d.v4i8.trap
    nvvm_swap_lo_hi_b64,                          // llvm.nvvm.swap.lo.hi.b64
    nvvm_tex_1d_array_grad_v4f32_f32,             // llvm.nvvm.tex.1d.array.grad.v4f32.f32
    nvvm_tex_1d_array_grad_v4s32_f32,             // llvm.nvvm.tex.1d.array.grad.v4s32.f32
    nvvm_tex_1d_array_grad_v4u32_f32,             // llvm.nvvm.tex.1d.array.grad.v4u32.f32
    nvvm_tex_1d_array_level_v4f32_f32,            // llvm.nvvm.tex.1d.array.level.v4f32.f32
    nvvm_tex_1d_array_level_v4s32_f32,            // llvm.nvvm.tex.1d.array.level.v4s32.f32
    nvvm_tex_1d_array_level_v4u32_f32,            // llvm.nvvm.tex.1d.array.level.v4u32.f32
    nvvm_tex_1d_array_v4f32_f32,                  // llvm.nvvm.tex.1d.array.v4f32.f32
    nvvm_tex_1d_array_v4f32_s32,                  // llvm.nvvm.tex.1d.array.v4f32.s32
    nvvm_tex_1d_array_v4s32_f32,                  // llvm.nvvm.tex.1d.array.v4s32.f32
    nvvm_tex_1d_array_v4s32_s32,                  // llvm.nvvm.tex.1d.array.v4s32.s32
    nvvm_tex_1d_array_v4u32_f32,                  // llvm.nvvm.tex.1d.array.v4u32.f32
    nvvm_tex_1d_array_v4u32_s32,                  // llvm.nvvm.tex.1d.array.v4u32.s32
    nvvm_tex_1d_grad_v4f32_f32,                   // llvm.nvvm.tex.1d.grad.v4f32.f32
    nvvm_tex_1d_grad_v4s32_f32,                   // llvm.nvvm.tex.1d.grad.v4s32.f32
    nvvm_tex_1d_grad_v4u32_f32,                   // llvm.nvvm.tex.1d.grad.v4u32.f32
    nvvm_tex_1d_level_v4f32_f32,                  // llvm.nvvm.tex.1d.level.v4f32.f32
    nvvm_tex_1d_level_v4s32_f32,                  // llvm.nvvm.tex.1d.level.v4s32.f32
    nvvm_tex_1d_level_v4u32_f32,                  // llvm.nvvm.tex.1d.level.v4u32.f32
    nvvm_tex_1d_v4f32_f32,                        // llvm.nvvm.tex.1d.v4f32.f32
    nvvm_tex_1d_v4f32_s32,                        // llvm.nvvm.tex.1d.v4f32.s32
    nvvm_tex_1d_v4s32_f32,                        // llvm.nvvm.tex.1d.v4s32.f32
    nvvm_tex_1d_v4s32_s32,                        // llvm.nvvm.tex.1d.v4s32.s32
    nvvm_tex_1d_v4u32_f32,                        // llvm.nvvm.tex.1d.v4u32.f32
    nvvm_tex_1d_v4u32_s32,                        // llvm.nvvm.tex.1d.v4u32.s32
    nvvm_tex_2d_array_grad_v4f32_f32,             // llvm.nvvm.tex.2d.array.grad.v4f32.f32
    nvvm_tex_2d_array_grad_v4s32_f32,             // llvm.nvvm.tex.2d.array.grad.v4s32.f32
    nvvm_tex_2d_array_grad_v4u32_f32,             // llvm.nvvm.tex.2d.array.grad.v4u32.f32
    nvvm_tex_2d_array_level_v4f32_f32,            // llvm.nvvm.tex.2d.array.level.v4f32.f32
    nvvm_tex_2d_array_level_v4s32_f32,            // llvm.nvvm.tex.2d.array.level.v4s32.f32
    nvvm_tex_2d_array_level_v4u32_f32,            // llvm.nvvm.tex.2d.array.level.v4u32.f32
    nvvm_tex_2d_array_v4f32_f32,                  // llvm.nvvm.tex.2d.array.v4f32.f32
    nvvm_tex_2d_array_v4f32_s32,                  // llvm.nvvm.tex.2d.array.v4f32.s32
    nvvm_tex_2d_array_v4s32_f32,                  // llvm.nvvm.tex.2d.array.v4s32.f32
    nvvm_tex_2d_array_v4s32_s32,                  // llvm.nvvm.tex.2d.array.v4s32.s32
    nvvm_tex_2d_array_v4u32_f32,                  // llvm.nvvm.tex.2d.array.v4u32.f32
    nvvm_tex_2d_array_v4u32_s32,                  // llvm.nvvm.tex.2d.array.v4u32.s32
    nvvm_tex_2d_grad_v4f32_f32,                   // llvm.nvvm.tex.2d.grad.v4f32.f32
    nvvm_tex_2d_grad_v4s32_f32,                   // llvm.nvvm.tex.2d.grad.v4s32.f32
    nvvm_tex_2d_grad_v4u32_f32,                   // llvm.nvvm.tex.2d.grad.v4u32.f32
    nvvm_tex_2d_level_v4f32_f32,                  // llvm.nvvm.tex.2d.level.v4f32.f32
    nvvm_tex_2d_level_v4s32_f32,                  // llvm.nvvm.tex.2d.level.v4s32.f32
    nvvm_tex_2d_level_v4u32_f32,                  // llvm.nvvm.tex.2d.level.v4u32.f32
    nvvm_tex_2d_v4f32_f32,                        // llvm.nvvm.tex.2d.v4f32.f32
    nvvm_tex_2d_v4f32_s32,                        // llvm.nvvm.tex.2d.v4f32.s32
    nvvm_tex_2d_v4s32_f32,                        // llvm.nvvm.tex.2d.v4s32.f32
    nvvm_tex_2d_v4s32_s32,                        // llvm.nvvm.tex.2d.v4s32.s32
    nvvm_tex_2d_v4u32_f32,                        // llvm.nvvm.tex.2d.v4u32.f32
    nvvm_tex_2d_v4u32_s32,                        // llvm.nvvm.tex.2d.v4u32.s32
    nvvm_tex_3d_grad_v4f32_f32,                   // llvm.nvvm.tex.3d.grad.v4f32.f32
    nvvm_tex_3d_grad_v4s32_f32,                   // llvm.nvvm.tex.3d.grad.v4s32.f32
    nvvm_tex_3d_grad_v4u32_f32,                   // llvm.nvvm.tex.3d.grad.v4u32.f32
    nvvm_tex_3d_level_v4f32_f32,                  // llvm.nvvm.tex.3d.level.v4f32.f32
    nvvm_tex_3d_level_v4s32_f32,                  // llvm.nvvm.tex.3d.level.v4s32.f32
    nvvm_tex_3d_level_v4u32_f32,                  // llvm.nvvm.tex.3d.level.v4u32.f32
    nvvm_tex_3d_v4f32_f32,                        // llvm.nvvm.tex.3d.v4f32.f32
    nvvm_tex_3d_v4f32_s32,                        // llvm.nvvm.tex.3d.v4f32.s32
    nvvm_tex_3d_v4s32_f32,                        // llvm.nvvm.tex.3d.v4s32.f32
    nvvm_tex_3d_v4s32_s32,                        // llvm.nvvm.tex.3d.v4s32.s32
    nvvm_tex_3d_v4u32_f32,                        // llvm.nvvm.tex.3d.v4u32.f32
    nvvm_tex_3d_v4u32_s32,                        // llvm.nvvm.tex.3d.v4u32.s32
    nvvm_tex_cube_array_level_v4f32_f32,          // llvm.nvvm.tex.cube.array.level.v4f32.f32
    nvvm_tex_cube_array_level_v4s32_f32,          // llvm.nvvm.tex.cube.array.level.v4s32.f32
    nvvm_tex_cube_array_level_v4u32_f32,          // llvm.nvvm.tex.cube.array.level.v4u32.f32
    nvvm_tex_cube_array_v4f32_f32,                // llvm.nvvm.tex.cube.array.v4f32.f32
    nvvm_tex_cube_array_v4s32_f32,                // llvm.nvvm.tex.cube.array.v4s32.f32
    nvvm_tex_cube_array_v4u32_f32,                // llvm.nvvm.tex.cube.array.v4u32.f32
    nvvm_tex_cube_level_v4f32_f32,                // llvm.nvvm.tex.cube.level.v4f32.f32
    nvvm_tex_cube_level_v4s32_f32,                // llvm.nvvm.tex.cube.level.v4s32.f32
    nvvm_tex_cube_level_v4u32_f32,                // llvm.nvvm.tex.cube.level.v4u32.f32
    nvvm_tex_cube_v4f32_f32,                      // llvm.nvvm.tex.cube.v4f32.f32
    nvvm_tex_cube_v4s32_f32,                      // llvm.nvvm.tex.cube.v4s32.f32
    nvvm_tex_cube_v4u32_f32,                      // llvm.nvvm.tex.cube.v4u32.f32
    nvvm_tex_unified_1d_array_grad_v4f32_f32,     // llvm.nvvm.tex.unified.1d.array.grad.v4f32.f32
    nvvm_tex_unified_1d_array_grad_v4s32_f32,     // llvm.nvvm.tex.unified.1d.array.grad.v4s32.f32
    nvvm_tex_unified_1d_array_grad_v4u32_f32,     // llvm.nvvm.tex.unified.1d.array.grad.v4u32.f32
    nvvm_tex_unified_1d_array_level_v4f32_f32,    // llvm.nvvm.tex.unified.1d.array.level.v4f32.f32
    nvvm_tex_unified_1d_array_level_v4s32_f32,    // llvm.nvvm.tex.unified.1d.array.level.v4s32.f32
    nvvm_tex_unified_1d_array_level_v4u32_f32,    // llvm.nvvm.tex.unified.1d.array.level.v4u32.f32
    nvvm_tex_unified_1d_array_v4f32_f32,          // llvm.nvvm.tex.unified.1d.array.v4f32.f32
    nvvm_tex_unified_1d_array_v4f32_s32,          // llvm.nvvm.tex.unified.1d.array.v4f32.s32
    nvvm_tex_unified_1d_array_v4s32_f32,          // llvm.nvvm.tex.unified.1d.array.v4s32.f32
    nvvm_tex_unified_1d_array_v4s32_s32,          // llvm.nvvm.tex.unified.1d.array.v4s32.s32
    nvvm_tex_unified_1d_array_v4u32_f32,          // llvm.nvvm.tex.unified.1d.array.v4u32.f32
    nvvm_tex_unified_1d_array_v4u32_s32,          // llvm.nvvm.tex.unified.1d.array.v4u32.s32
    nvvm_tex_unified_1d_grad_v4f32_f32,           // llvm.nvvm.tex.unified.1d.grad.v4f32.f32
    nvvm_tex_unified_1d_grad_v4s32_f32,           // llvm.nvvm.tex.unified.1d.grad.v4s32.f32
    nvvm_tex_unified_1d_grad_v4u32_f32,           // llvm.nvvm.tex.unified.1d.grad.v4u32.f32
    nvvm_tex_unified_1d_level_v4f32_f32,          // llvm.nvvm.tex.unified.1d.level.v4f32.f32
    nvvm_tex_unified_1d_level_v4s32_f32,          // llvm.nvvm.tex.unified.1d.level.v4s32.f32
    nvvm_tex_unified_1d_level_v4u32_f32,          // llvm.nvvm.tex.unified.1d.level.v4u32.f32
    nvvm_tex_unified_1d_v4f32_f32,                // llvm.nvvm.tex.unified.1d.v4f32.f32
    nvvm_tex_unified_1d_v4f32_s32,                // llvm.nvvm.tex.unified.1d.v4f32.s32
    nvvm_tex_unified_1d_v4s32_f32,                // llvm.nvvm.tex.unified.1d.v4s32.f32
    nvvm_tex_unified_1d_v4s32_s32,                // llvm.nvvm.tex.unified.1d.v4s32.s32
    nvvm_tex_unified_1d_v4u32_f32,                // llvm.nvvm.tex.unified.1d.v4u32.f32
    nvvm_tex_unified_1d_v4u32_s32,                // llvm.nvvm.tex.unified.1d.v4u32.s32
    nvvm_tex_unified_2d_array_grad_v4f32_f32,     // llvm.nvvm.tex.unified.2d.array.grad.v4f32.f32
    nvvm_tex_unified_2d_array_grad_v4s32_f32,     // llvm.nvvm.tex.unified.2d.array.grad.v4s32.f32
    nvvm_tex_unified_2d_array_grad_v4u32_f32,     // llvm.nvvm.tex.unified.2d.array.grad.v4u32.f32
    nvvm_tex_unified_2d_array_level_v4f32_f32,    // llvm.nvvm.tex.unified.2d.array.level.v4f32.f32
    nvvm_tex_unified_2d_array_level_v4s32_f32,    // llvm.nvvm.tex.unified.2d.array.level.v4s32.f32
    nvvm_tex_unified_2d_array_level_v4u32_f32,    // llvm.nvvm.tex.unified.2d.array.level.v4u32.f32
    nvvm_tex_unified_2d_array_v4f32_f32,          // llvm.nvvm.tex.unified.2d.array.v4f32.f32
    nvvm_tex_unified_2d_array_v4f32_s32,          // llvm.nvvm.tex.unified.2d.array.v4f32.s32
    nvvm_tex_unified_2d_array_v4s32_f32,          // llvm.nvvm.tex.unified.2d.array.v4s32.f32
    nvvm_tex_unified_2d_array_v4s32_s32,          // llvm.nvvm.tex.unified.2d.array.v4s32.s32
    nvvm_tex_unified_2d_array_v4u32_f32,          // llvm.nvvm.tex.unified.2d.array.v4u32.f32
    nvvm_tex_unified_2d_array_v4u32_s32,          // llvm.nvvm.tex.unified.2d.array.v4u32.s32
    nvvm_tex_unified_2d_grad_v4f32_f32,           // llvm.nvvm.tex.unified.2d.grad.v4f32.f32
    nvvm_tex_unified_2d_grad_v4s32_f32,           // llvm.nvvm.tex.unified.2d.grad.v4s32.f32
    nvvm_tex_unified_2d_grad_v4u32_f32,           // llvm.nvvm.tex.unified.2d.grad.v4u32.f32
    nvvm_tex_unified_2d_level_v4f32_f32,          // llvm.nvvm.tex.unified.2d.level.v4f32.f32
    nvvm_tex_unified_2d_level_v4s32_f32,          // llvm.nvvm.tex.unified.2d.level.v4s32.f32
    nvvm_tex_unified_2d_level_v4u32_f32,          // llvm.nvvm.tex.unified.2d.level.v4u32.f32
    nvvm_tex_unified_2d_v4f32_f32,                // llvm.nvvm.tex.unified.2d.v4f32.f32
    nvvm_tex_unified_2d_v4f32_s32,                // llvm.nvvm.tex.unified.2d.v4f32.s32
    nvvm_tex_unified_2d_v4s32_f32,                // llvm.nvvm.tex.unified.2d.v4s32.f32
    nvvm_tex_unified_2d_v4s32_s32,                // llvm.nvvm.tex.unified.2d.v4s32.s32
    nvvm_tex_unified_2d_v4u32_f32,                // llvm.nvvm.tex.unified.2d.v4u32.f32
    nvvm_tex_unified_2d_v4u32_s32,                // llvm.nvvm.tex.unified.2d.v4u32.s32
    nvvm_tex_unified_3d_grad_v4f32_f32,           // llvm.nvvm.tex.unified.3d.grad.v4f32.f32
    nvvm_tex_unified_3d_grad_v4s32_f32,           // llvm.nvvm.tex.unified.3d.grad.v4s32.f32
    nvvm_tex_unified_3d_grad_v4u32_f32,           // llvm.nvvm.tex.unified.3d.grad.v4u32.f32
    nvvm_tex_unified_3d_level_v4f32_f32,          // llvm.nvvm.tex.unified.3d.level.v4f32.f32
    nvvm_tex_unified_3d_level_v4s32_f32,          // llvm.nvvm.tex.unified.3d.level.v4s32.f32
    nvvm_tex_unified_3d_level_v4u32_f32,          // llvm.nvvm.tex.unified.3d.level.v4u32.f32
    nvvm_tex_unified_3d_v4f32_f32,                // llvm.nvvm.tex.unified.3d.v4f32.f32
    nvvm_tex_unified_3d_v4f32_s32,                // llvm.nvvm.tex.unified.3d.v4f32.s32
    nvvm_tex_unified_3d_v4s32_f32,                // llvm.nvvm.tex.unified.3d.v4s32.f32
    nvvm_tex_unified_3d_v4s32_s32,                // llvm.nvvm.tex.unified.3d.v4s32.s32
    nvvm_tex_unified_3d_v4u32_f32,                // llvm.nvvm.tex.unified.3d.v4u32.f32
    nvvm_tex_unified_3d_v4u32_s32,                // llvm.nvvm.tex.unified.3d.v4u32.s32
    nvvm_tex_unified_cube_array_level_v4f32_f32,  // llvm.nvvm.tex.unified.cube.array.level.v4f32.f32
    nvvm_tex_unified_cube_array_level_v4s32_f32,  // llvm.nvvm.tex.unified.cube.array.level.v4s32.f32
    nvvm_tex_unified_cube_array_level_v4u32_f32,  // llvm.nvvm.tex.unified.cube.array.level.v4u32.f32
    nvvm_tex_unified_cube_array_v4f32_f32,        // llvm.nvvm.tex.unified.cube.array.v4f32.f32
    nvvm_tex_unified_cube_array_v4s32_f32,        // llvm.nvvm.tex.unified.cube.array.v4s32.f32
    nvvm_tex_unified_cube_array_v4u32_f32,        // llvm.nvvm.tex.unified.cube.array.v4u32.f32
    nvvm_tex_unified_cube_level_v4f32_f32,        // llvm.nvvm.tex.unified.cube.level.v4f32.f32
    nvvm_tex_unified_cube_level_v4s32_f32,        // llvm.nvvm.tex.unified.cube.level.v4s32.f32
    nvvm_tex_unified_cube_level_v4u32_f32,        // llvm.nvvm.tex.unified.cube.level.v4u32.f32
    nvvm_tex_unified_cube_v4f32_f32,              // llvm.nvvm.tex.unified.cube.v4f32.f32
    nvvm_tex_unified_cube_v4s32_f32,              // llvm.nvvm.tex.unified.cube.v4s32.f32
    nvvm_tex_unified_cube_v4u32_f32,              // llvm.nvvm.tex.unified.cube.v4u32.f32
    nvvm_texsurf_handle,                          // llvm.nvvm.texsurf.handle
    nvvm_texsurf_handle_internal,                 // llvm.nvvm.texsurf.handle.internal
    nvvm_tld4_a_2d_v4f32_f32,                     // llvm.nvvm.tld4.a.2d.v4f32.f32
    nvvm_tld4_a_2d_v4s32_f32,                     // llvm.nvvm.tld4.a.2d.v4s32.f32
    nvvm_tld4_a_2d_v4u32_f32,                     // llvm.nvvm.tld4.a.2d.v4u32.f32
    nvvm_tld4_b_2d_v4f32_f32,                     // llvm.nvvm.tld4.b.2d.v4f32.f32
    nvvm_tld4_b_2d_v4s32_f32,                     // llvm.nvvm.tld4.b.2d.v4s32.f32
    nvvm_tld4_b_2d_v4u32_f32,                     // llvm.nvvm.tld4.b.2d.v4u32.f32
    nvvm_tld4_g_2d_v4f32_f32,                     // llvm.nvvm.tld4.g.2d.v4f32.f32
    nvvm_tld4_g_2d_v4s32_f32,                     // llvm.nvvm.tld4.g.2d.v4s32.f32
    nvvm_tld4_g_2d_v4u32_f32,                     // llvm.nvvm.tld4.g.2d.v4u32.f32
    nvvm_tld4_r_2d_v4f32_f32,                     // llvm.nvvm.tld4.r.2d.v4f32.f32
    nvvm_tld4_r_2d_v4s32_f32,                     // llvm.nvvm.tld4.r.2d.v4s32.f32
    nvvm_tld4_r_2d_v4u32_f32,                     // llvm.nvvm.tld4.r.2d.v4u32.f32
    nvvm_tld4_unified_a_2d_v4f32_f32,             // llvm.nvvm.tld4.unified.a.2d.v4f32.f32
    nvvm_tld4_unified_a_2d_v4s32_f32,             // llvm.nvvm.tld4.unified.a.2d.v4s32.f32
    nvvm_tld4_unified_a_2d_v4u32_f32,             // llvm.nvvm.tld4.unified.a.2d.v4u32.f32
    nvvm_tld4_unified_b_2d_v4f32_f32,             // llvm.nvvm.tld4.unified.b.2d.v4f32.f32
    nvvm_tld4_unified_b_2d_v4s32_f32,             // llvm.nvvm.tld4.unified.b.2d.v4s32.f32
    nvvm_tld4_unified_b_2d_v4u32_f32,             // llvm.nvvm.tld4.unified.b.2d.v4u32.f32
    nvvm_tld4_unified_g_2d_v4f32_f32,             // llvm.nvvm.tld4.unified.g.2d.v4f32.f32
    nvvm_tld4_unified_g_2d_v4s32_f32,             // llvm.nvvm.tld4.unified.g.2d.v4s32.f32
    nvvm_tld4_unified_g_2d_v4u32_f32,             // llvm.nvvm.tld4.unified.g.2d.v4u32.f32
    nvvm_tld4_unified_r_2d_v4f32_f32,             // llvm.nvvm.tld4.unified.r.2d.v4f32.f32
    nvvm_tld4_unified_r_2d_v4s32_f32,             // llvm.nvvm.tld4.unified.r.2d.v4s32.f32
    nvvm_tld4_unified_r_2d_v4u32_f32,             // llvm.nvvm.tld4.unified.r.2d.v4u32.f32
    nvvm_trunc_d,                                 // llvm.nvvm.trunc.d
    nvvm_trunc_f,                                 // llvm.nvvm.trunc.f
    nvvm_trunc_ftz_f,                             // llvm.nvvm.trunc.ftz.f
    nvvm_txq_array_size,                          // llvm.nvvm.txq.array.size
    nvvm_txq_channel_data_type,                   // llvm.nvvm.txq.channel.data.type
    nvvm_txq_channel_order,                       // llvm.nvvm.txq.channel.order
    nvvm_txq_depth,                               // llvm.nvvm.txq.depth
    nvvm_txq_height,                              // llvm.nvvm.txq.height
    nvvm_txq_num_mipmap_levels,                   // llvm.nvvm.txq.num.mipmap.levels
    nvvm_txq_num_samples,                         // llvm.nvvm.txq.num.samples
    nvvm_txq_width,                               // llvm.nvvm.txq.width
    nvvm_ui2d_rm,                                 // llvm.nvvm.ui2d.rm
    nvvm_ui2d_rn,                                 // llvm.nvvm.ui2d.rn
    nvvm_ui2d_rp,                                 // llvm.nvvm.ui2d.rp
    nvvm_ui2d_rz,                                 // llvm.nvvm.ui2d.rz
    nvvm_ui2f_rm,                                 // llvm.nvvm.ui2f.rm
    nvvm_ui2f_rn,                                 // llvm.nvvm.ui2f.rn
    nvvm_ui2f_rp,                                 // llvm.nvvm.ui2f.rp
    nvvm_ui2f_rz,                                 // llvm.nvvm.ui2f.rz
    nvvm_ull2d_rm,                                // llvm.nvvm.ull2d.rm
    nvvm_ull2d_rn,                                // llvm.nvvm.ull2d.rn
    nvvm_ull2d_rp,                                // llvm.nvvm.ull2d.rp
    nvvm_ull2d_rz,                                // llvm.nvvm.ull2d.rz
    nvvm_ull2f_rm,                                // llvm.nvvm.ull2f.rm
    nvvm_ull2f_rn,                                // llvm.nvvm.ull2f.rn
    nvvm_ull2f_rp,                                // llvm.nvvm.ull2f.rp
    nvvm_ull2f_rz,                                // llvm.nvvm.ull2f.rz
    nvvm_vote_all,                                // llvm.nvvm.vote.all
    nvvm_vote_all_sync,                           // llvm.nvvm.vote.all.sync
    nvvm_vote_any,                                // llvm.nvvm.vote.any
    nvvm_vote_any_sync,                           // llvm.nvvm.vote.any.sync
    nvvm_vote_ballot,                             // llvm.nvvm.vote.ballot
    nvvm_vote_ballot_sync,                        // llvm.nvvm.vote.ballot.sync
    nvvm_vote_uni,                                // llvm.nvvm.vote.uni
    nvvm_vote_uni_sync,                           // llvm.nvvm.vote.uni.sync
    nvvm_wmma_load_a_f16_col,                     // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.f16
    nvvm_wmma_load_a_f16_col_global,              // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.global.f16
    nvvm_wmma_load_a_f16_col_global_stride,       // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.global.stride.f16
    nvvm_wmma_load_a_f16_col_shared,              // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.shared.f16
    nvvm_wmma_load_a_f16_col_shared_stride,       // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.shared.stride.f16
    nvvm_wmma_load_a_f16_col_stride,              // llvm.nvvm.wmma.load.a.sync.col.m16n16k16.stride.f16
    nvvm_wmma_load_a_f16_row,                     // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.f16
    nvvm_wmma_load_a_f16_row_global,              // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.global.f16
    nvvm_wmma_load_a_f16_row_global_stride,       // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.global.stride.f16
    nvvm_wmma_load_a_f16_row_shared,              // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.shared.f16
    nvvm_wmma_load_a_f16_row_shared_stride,       // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.shared.stride.f16
    nvvm_wmma_load_a_f16_row_stride,              // llvm.nvvm.wmma.load.a.sync.row.m16n16k16.stride.f16
    nvvm_wmma_load_b_f16_col,                     // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.f16
    nvvm_wmma_load_b_f16_col_global,              // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.global.f16
    nvvm_wmma_load_b_f16_col_global_stride,       // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.global.stride.f16
    nvvm_wmma_load_b_f16_col_shared,              // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.shared.f16
    nvvm_wmma_load_b_f16_col_shared_stride,       // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.shared.stride.f16
    nvvm_wmma_load_b_f16_col_stride,              // llvm.nvvm.wmma.load.b.sync.col.m16n16k16.stride.f16
    nvvm_wmma_load_b_f16_row,                     // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.f16
    nvvm_wmma_load_b_f16_row_global,              // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.global.f16
    nvvm_wmma_load_b_f16_row_global_stride,       // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.global.stride.f16
    nvvm_wmma_load_b_f16_row_shared,              // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.shared.f16
    nvvm_wmma_load_b_f16_row_shared_stride,       // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.shared.stride.f16
    nvvm_wmma_load_b_f16_row_stride,              // llvm.nvvm.wmma.load.b.sync.row.m16n16k16.stride.f16
    nvvm_wmma_load_c_f16_col,                     // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.f16
    nvvm_wmma_load_c_f32_col,                     // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.f32
    nvvm_wmma_load_c_f16_col_global,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.f16
    nvvm_wmma_load_c_f32_col_global,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.f32
    nvvm_wmma_load_c_f16_col_global_stride,       // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.stride.f16
    nvvm_wmma_load_c_f32_col_global_stride,       // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.stride.f32
    nvvm_wmma_load_c_f16_col_shared,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.f16
    nvvm_wmma_load_c_f32_col_shared,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.f32
    nvvm_wmma_load_c_f16_col_shared_stride,       // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.stride.f16
    nvvm_wmma_load_c_f32_col_shared_stride,       // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.stride.f32
    nvvm_wmma_load_c_f16_col_stride,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.stride.f16
    nvvm_wmma_load_c_f32_col_stride,              // llvm.nvvm.wmma.load.c.sync.col.m16n16k16.stride.f32
    nvvm_wmma_load_c_f16_row,                     // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.f16
    nvvm_wmma_load_c_f32_row,                     // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.f32
    nvvm_wmma_load_c_f16_row_global,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.f16
    nvvm_wmma_load_c_f32_row_global,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.f32
    nvvm_wmma_load_c_f16_row_global_stride,       // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.stride.f16
    nvvm_wmma_load_c_f32_row_global_stride,       // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.stride.f32
    nvvm_wmma_load_c_f16_row_shared,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.f16
    nvvm_wmma_load_c_f32_row_shared,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.f32
    nvvm_wmma_load_c_f16_row_shared_stride,       // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.stride.f16
    nvvm_wmma_load_c_f32_row_shared_stride,       // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.stride.f32
    nvvm_wmma_load_c_f16_row_stride,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.stride.f16
    nvvm_wmma_load_c_f32_row_stride,              // llvm.nvvm.wmma.load.c.sync.row.m16n16k16.stride.f32
    nvvm_wmma_mma_sync_col_col_f16_f16,           // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f16
    nvvm_wmma_mma_sync_col_col_f16_f16_satfinite, // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f16.satfinite
    nvvm_wmma_mma_sync_col_col_f16_f32,           // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f32
    nvvm_wmma_mma_sync_col_col_f16_f32_satfinite, // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f32.satfinite
    nvvm_wmma_mma_sync_col_col_f32_f16,           // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f16
    nvvm_wmma_mma_sync_col_col_f32_f16_satfinite, // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f16.satfinite
    nvvm_wmma_mma_sync_col_col_f32_f32,           // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f32
    nvvm_wmma_mma_sync_col_col_f32_f32_satfinite, // llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f32.satfinite
    nvvm_wmma_mma_sync_col_row_f16_f16,           // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f16
    nvvm_wmma_mma_sync_col_row_f16_f16_satfinite, // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f16.satfinite
    nvvm_wmma_mma_sync_col_row_f16_f32,           // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f32
    nvvm_wmma_mma_sync_col_row_f16_f32_satfinite, // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f32.satfinite
    nvvm_wmma_mma_sync_col_row_f32_f16,           // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f16
    nvvm_wmma_mma_sync_col_row_f32_f16_satfinite, // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f16.satfinite
    nvvm_wmma_mma_sync_col_row_f32_f32,           // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f32
    nvvm_wmma_mma_sync_col_row_f32_f32_satfinite, // llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f32.satfinite
    nvvm_wmma_mma_sync_row_col_f16_f16,           // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f16
    nvvm_wmma_mma_sync_row_col_f16_f16_satfinite, // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f16.satfinite
    nvvm_wmma_mma_sync_row_col_f16_f32,           // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f32
    nvvm_wmma_mma_sync_row_col_f16_f32_satfinite, // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f32.satfinite
    nvvm_wmma_mma_sync_row_col_f32_f16,           // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f16
    nvvm_wmma_mma_sync_row_col_f32_f16_satfinite, // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f16.satfinite
    nvvm_wmma_mma_sync_row_col_f32_f32,           // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f32
    nvvm_wmma_mma_sync_row_col_f32_f32_satfinite, // llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f32.satfinite
    nvvm_wmma_mma_sync_row_row_f16_f16,           // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f16
    nvvm_wmma_mma_sync_row_row_f16_f16_satfinite, // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f16.satfinite
    nvvm_wmma_mma_sync_row_row_f16_f32,           // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f32
    nvvm_wmma_mma_sync_row_row_f16_f32_satfinite, // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f32.satfinite
    nvvm_wmma_mma_sync_row_row_f32_f16,           // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f16
    nvvm_wmma_mma_sync_row_row_f32_f16_satfinite, // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f16.satfinite
    nvvm_wmma_mma_sync_row_row_f32_f32,           // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f32
    nvvm_wmma_mma_sync_row_row_f32_f32_satfinite, // llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f32.satfinite
    nvvm_wmma_store_d_f16_col,                    // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.f16
    nvvm_wmma_store_d_f32_col,                    // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.f32
    nvvm_wmma_store_d_f16_col_global,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.f16
    nvvm_wmma_store_d_f32_col_global,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.f32
    nvvm_wmma_store_d_f16_col_global_stride,      // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.stride.f16
    nvvm_wmma_store_d_f32_col_global_stride,      // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.stride.f32
    nvvm_wmma_store_d_f16_col_shared,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.f16
    nvvm_wmma_store_d_f32_col_shared,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.f32
    nvvm_wmma_store_d_f16_col_shared_stride,      // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.stride.f16
    nvvm_wmma_store_d_f32_col_shared_stride,      // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.stride.f32
    nvvm_wmma_store_d_f16_col_stride,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.stride.f16
    nvvm_wmma_store_d_f32_col_stride,             // llvm.nvvm.wmma.store.d.sync.col.m16n16k16.stride.f32
    nvvm_wmma_store_d_f16_row,                    // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.f16
    nvvm_wmma_store_d_f32_row,                    // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.f32
    nvvm_wmma_store_d_f16_row_global,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.f16
    nvvm_wmma_store_d_f32_row_global,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.f32
    nvvm_wmma_store_d_f16_row_global_stride,      // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.stride.f16
    nvvm_wmma_store_d_f32_row_global_stride,      // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.stride.f32
    nvvm_wmma_store_d_f16_row_shared,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.f16
    nvvm_wmma_store_d_f32_row_shared,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.f32
    nvvm_wmma_store_d_f16_row_shared_stride,      // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.stride.f16
    nvvm_wmma_store_d_f32_row_shared_stride,      // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.stride.f32
    nvvm_wmma_store_d_f16_row_stride,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.stride.f16
    nvvm_wmma_store_d_f32_row_stride,             // llvm.nvvm.wmma.store.d.sync.row.m16n16k16.stride.f32
    ppc_altivec_crypto_vcipher,                   // llvm.ppc.altivec.crypto.vcipher
    ppc_altivec_crypto_vcipherlast,               // llvm.ppc.altivec.crypto.vcipherlast
    ppc_altivec_crypto_vncipher,                  // llvm.ppc.altivec.crypto.vncipher
    ppc_altivec_crypto_vncipherlast,              // llvm.ppc.altivec.crypto.vncipherlast
    ppc_altivec_crypto_vpermxor,                  // llvm.ppc.altivec.crypto.vpermxor
    ppc_altivec_crypto_vpmsumb,                   // llvm.ppc.altivec.crypto.vpmsumb
    ppc_altivec_crypto_vpmsumd,                   // llvm.ppc.altivec.crypto.vpmsumd
    ppc_altivec_crypto_vpmsumh,                   // llvm.ppc.altivec.crypto.vpmsumh
    ppc_altivec_crypto_vpmsumw,                   // llvm.ppc.altivec.crypto.vpmsumw
    ppc_altivec_crypto_vsbox,                     // llvm.ppc.altivec.crypto.vsbox
    ppc_altivec_crypto_vshasigmad,                // llvm.ppc.altivec.crypto.vshasigmad
    ppc_altivec_crypto_vshasigmaw,                // llvm.ppc.altivec.crypto.vshasigmaw
    ppc_altivec_dss,                              // llvm.ppc.altivec.dss
    ppc_altivec_dssall,                           // llvm.ppc.altivec.dssall
    ppc_altivec_dst,                              // llvm.ppc.altivec.dst
    ppc_altivec_dstst,                            // llvm.ppc.altivec.dstst
    ppc_altivec_dststt,                           // llvm.ppc.altivec.dststt
    ppc_altivec_dstt,                             // llvm.ppc.altivec.dstt
    ppc_altivec_lvebx,                            // llvm.ppc.altivec.lvebx
    ppc_altivec_lvehx,                            // llvm.ppc.altivec.lvehx
    ppc_altivec_lvewx,                            // llvm.ppc.altivec.lvewx
    ppc_altivec_lvsl,                             // llvm.ppc.altivec.lvsl
    ppc_altivec_lvsr,                             // llvm.ppc.altivec.lvsr
    ppc_altivec_lvx,                              // llvm.ppc.altivec.lvx
    ppc_altivec_lvxl,                             // llvm.ppc.altivec.lvxl
    ppc_altivec_mfvscr,                           // llvm.ppc.altivec.mfvscr
    ppc_altivec_mtvscr,                           // llvm.ppc.altivec.mtvscr
    ppc_altivec_stvebx,                           // llvm.ppc.altivec.stvebx
    ppc_altivec_stvehx,                           // llvm.ppc.altivec.stvehx
    ppc_altivec_stvewx,                           // llvm.ppc.altivec.stvewx
    ppc_altivec_stvx,                             // llvm.ppc.altivec.stvx
    ppc_altivec_stvxl,                            // llvm.ppc.altivec.stvxl
    ppc_altivec_vabsdub,                          // llvm.ppc.altivec.vabsdub
    ppc_altivec_vabsduh,                          // llvm.ppc.altivec.vabsduh
    ppc_altivec_vabsduw,                          // llvm.ppc.altivec.vabsduw
    ppc_altivec_vaddcuq,                          // llvm.ppc.altivec.vaddcuq
    ppc_altivec_vaddcuw,                          // llvm.ppc.altivec.vaddcuw
    ppc_altivec_vaddecuq,                         // llvm.ppc.altivec.vaddecuq
    ppc_altivec_vaddeuqm,                         // llvm.ppc.altivec.vaddeuqm
    ppc_altivec_vaddsbs,                          // llvm.ppc.altivec.vaddsbs
    ppc_altivec_vaddshs,                          // llvm.ppc.altivec.vaddshs
    ppc_altivec_vaddsws,                          // llvm.ppc.altivec.vaddsws
    ppc_altivec_vaddubs,                          // llvm.ppc.altivec.vaddubs
    ppc_altivec_vadduhs,                          // llvm.ppc.altivec.vadduhs
    ppc_altivec_vadduws,                          // llvm.ppc.altivec.vadduws
    ppc_altivec_vavgsb,                           // llvm.ppc.altivec.vavgsb
    ppc_altivec_vavgsh,                           // llvm.ppc.altivec.vavgsh
    ppc_altivec_vavgsw,                           // llvm.ppc.altivec.vavgsw
    ppc_altivec_vavgub,                           // llvm.ppc.altivec.vavgub
    ppc_altivec_vavguh,                           // llvm.ppc.altivec.vavguh
    ppc_altivec_vavguw,                           // llvm.ppc.altivec.vavguw
    ppc_altivec_vbpermq,                          // llvm.ppc.altivec.vbpermq
    ppc_altivec_vcfsx,                            // llvm.ppc.altivec.vcfsx
    ppc_altivec_vcfux,                            // llvm.ppc.altivec.vcfux
    ppc_altivec_vclzlsbb,                         // llvm.ppc.altivec.vclzlsbb
    ppc_altivec_vcmpbfp,                          // llvm.ppc.altivec.vcmpbfp
    ppc_altivec_vcmpbfp_p,                        // llvm.ppc.altivec.vcmpbfp.p
    ppc_altivec_vcmpeqfp,                         // llvm.ppc.altivec.vcmpeqfp
    ppc_altivec_vcmpeqfp_p,                       // llvm.ppc.altivec.vcmpeqfp.p
    ppc_altivec_vcmpequb,                         // llvm.ppc.altivec.vcmpequb
    ppc_altivec_vcmpequb_p,                       // llvm.ppc.altivec.vcmpequb.p
    ppc_altivec_vcmpequd,                         // llvm.ppc.altivec.vcmpequd
    ppc_altivec_vcmpequd_p,                       // llvm.ppc.altivec.vcmpequd.p
    ppc_altivec_vcmpequh,                         // llvm.ppc.altivec.vcmpequh
    ppc_altivec_vcmpequh_p,                       // llvm.ppc.altivec.vcmpequh.p
    ppc_altivec_vcmpequw,                         // llvm.ppc.altivec.vcmpequw
    ppc_altivec_vcmpequw_p,                       // llvm.ppc.altivec.vcmpequw.p
    ppc_altivec_vcmpgefp,                         // llvm.ppc.altivec.vcmpgefp
    ppc_altivec_vcmpgefp_p,                       // llvm.ppc.altivec.vcmpgefp.p
    ppc_altivec_vcmpgtfp,                         // llvm.ppc.altivec.vcmpgtfp
    ppc_altivec_vcmpgtfp_p,                       // llvm.ppc.altivec.vcmpgtfp.p
    ppc_altivec_vcmpgtsb,                         // llvm.ppc.altivec.vcmpgtsb
    ppc_altivec_vcmpgtsb_p,                       // llvm.ppc.altivec.vcmpgtsb.p
    ppc_altivec_vcmpgtsd,                         // llvm.ppc.altivec.vcmpgtsd
    ppc_altivec_vcmpgtsd_p,                       // llvm.ppc.altivec.vcmpgtsd.p
    ppc_altivec_vcmpgtsh,                         // llvm.ppc.altivec.vcmpgtsh
    ppc_altivec_vcmpgtsh_p,                       // llvm.ppc.altivec.vcmpgtsh.p
    ppc_altivec_vcmpgtsw,                         // llvm.ppc.altivec.vcmpgtsw
    ppc_altivec_vcmpgtsw_p,                       // llvm.ppc.altivec.vcmpgtsw.p
    ppc_altivec_vcmpgtub,                         // llvm.ppc.altivec.vcmpgtub
    ppc_altivec_vcmpgtub_p,                       // llvm.ppc.altivec.vcmpgtub.p
    ppc_altivec_vcmpgtud,                         // llvm.ppc.altivec.vcmpgtud
    ppc_altivec_vcmpgtud_p,                       // llvm.ppc.altivec.vcmpgtud.p
    ppc_altivec_vcmpgtuh,                         // llvm.ppc.altivec.vcmpgtuh
    ppc_altivec_vcmpgtuh_p,                       // llvm.ppc.altivec.vcmpgtuh.p
    ppc_altivec_vcmpgtuw,                         // llvm.ppc.altivec.vcmpgtuw
    ppc_altivec_vcmpgtuw_p,                       // llvm.ppc.altivec.vcmpgtuw.p
    ppc_altivec_vcmpneb,                          // llvm.ppc.altivec.vcmpneb
    ppc_altivec_vcmpneb_p,                        // llvm.ppc.altivec.vcmpneb.p
    ppc_altivec_vcmpneh,                          // llvm.ppc.altivec.vcmpneh
    ppc_altivec_vcmpneh_p,                        // llvm.ppc.altivec.vcmpneh.p
    ppc_altivec_vcmpnew,                          // llvm.ppc.altivec.vcmpnew
    ppc_altivec_vcmpnew_p,                        // llvm.ppc.altivec.vcmpnew.p
    ppc_altivec_vcmpnezb,                         // llvm.ppc.altivec.vcmpnezb
    ppc_altivec_vcmpnezb_p,                       // llvm.ppc.altivec.vcmpnezb.p
    ppc_altivec_vcmpnezh,                         // llvm.ppc.altivec.vcmpnezh
    ppc_altivec_vcmpnezh_p,                       // llvm.ppc.altivec.vcmpnezh.p
    ppc_altivec_vcmpnezw,                         // llvm.ppc.altivec.vcmpnezw
    ppc_altivec_vcmpnezw_p,                       // llvm.ppc.altivec.vcmpnezw.p
    ppc_altivec_vctsxs,                           // llvm.ppc.altivec.vctsxs
    ppc_altivec_vctuxs,                           // llvm.ppc.altivec.vctuxs
    ppc_altivec_vctzlsbb,                         // llvm.ppc.altivec.vctzlsbb
    ppc_altivec_vexptefp,                         // llvm.ppc.altivec.vexptefp
    ppc_altivec_vgbbd,                            // llvm.ppc.altivec.vgbbd
    ppc_altivec_vlogefp,                          // llvm.ppc.altivec.vlogefp
    ppc_altivec_vmaddfp,                          // llvm.ppc.altivec.vmaddfp
    ppc_altivec_vmaxfp,                           // llvm.ppc.altivec.vmaxfp
    ppc_altivec_vmaxsb,                           // llvm.ppc.altivec.vmaxsb
    ppc_altivec_vmaxsd,                           // llvm.ppc.altivec.vmaxsd
    ppc_altivec_vmaxsh,                           // llvm.ppc.altivec.vmaxsh
    ppc_altivec_vmaxsw,                           // llvm.ppc.altivec.vmaxsw
    ppc_altivec_vmaxub,                           // llvm.ppc.altivec.vmaxub
    ppc_altivec_vmaxud,                           // llvm.ppc.altivec.vmaxud
    ppc_altivec_vmaxuh,                           // llvm.ppc.altivec.vmaxuh
    ppc_altivec_vmaxuw,                           // llvm.ppc.altivec.vmaxuw
    ppc_altivec_vmhaddshs,                        // llvm.ppc.altivec.vmhaddshs
    ppc_altivec_vmhraddshs,                       // llvm.ppc.altivec.vmhraddshs
    ppc_altivec_vminfp,                           // llvm.ppc.altivec.vminfp
    ppc_altivec_vminsb,                           // llvm.ppc.altivec.vminsb
    ppc_altivec_vminsd,                           // llvm.ppc.altivec.vminsd
    ppc_altivec_vminsh,                           // llvm.ppc.altivec.vminsh
    ppc_altivec_vminsw,                           // llvm.ppc.altivec.vminsw
    ppc_altivec_vminub,                           // llvm.ppc.altivec.vminub
    ppc_altivec_vminud,                           // llvm.ppc.altivec.vminud
    ppc_altivec_vminuh,                           // llvm.ppc.altivec.vminuh
    ppc_altivec_vminuw,                           // llvm.ppc.altivec.vminuw
    ppc_altivec_vmladduhm,                        // llvm.ppc.altivec.vmladduhm
    ppc_altivec_vmsummbm,                         // llvm.ppc.altivec.vmsummbm
    ppc_altivec_vmsumshm,                         // llvm.ppc.altivec.vmsumshm
    ppc_altivec_vmsumshs,                         // llvm.ppc.altivec.vmsumshs
    ppc_altivec_vmsumubm,                         // llvm.ppc.altivec.vmsumubm
    ppc_altivec_vmsumuhm,                         // llvm.ppc.altivec.vmsumuhm
    ppc_altivec_vmsumuhs,                         // llvm.ppc.altivec.vmsumuhs
    ppc_altivec_vmulesb,                          // llvm.ppc.altivec.vmulesb
    ppc_altivec_vmulesh,                          // llvm.ppc.altivec.vmulesh
    ppc_altivec_vmulesw,                          // llvm.ppc.altivec.vmulesw
    ppc_altivec_vmuleub,                          // llvm.ppc.altivec.vmuleub
    ppc_altivec_vmuleuh,                          // llvm.ppc.altivec.vmuleuh
    ppc_altivec_vmuleuw,                          // llvm.ppc.altivec.vmuleuw
    ppc_altivec_vmulosb,                          // llvm.ppc.altivec.vmulosb
    ppc_altivec_vmulosh,                          // llvm.ppc.altivec.vmulosh
    ppc_altivec_vmulosw,                          // llvm.ppc.altivec.vmulosw
    ppc_altivec_vmuloub,                          // llvm.ppc.altivec.vmuloub
    ppc_altivec_vmulouh,                          // llvm.ppc.altivec.vmulouh
    ppc_altivec_vmulouw,                          // llvm.ppc.altivec.vmulouw
    ppc_altivec_vnmsubfp,                         // llvm.ppc.altivec.vnmsubfp
    ppc_altivec_vperm,                            // llvm.ppc.altivec.vperm
    ppc_altivec_vpkpx,                            // llvm.ppc.altivec.vpkpx
    ppc_altivec_vpksdss,                          // llvm.ppc.altivec.vpksdss
    ppc_altivec_vpksdus,                          // llvm.ppc.altivec.vpksdus
    ppc_altivec_vpkshss,                          // llvm.ppc.altivec.vpkshss
    ppc_altivec_vpkshus,                          // llvm.ppc.altivec.vpkshus
    ppc_altivec_vpkswss,                          // llvm.ppc.altivec.vpkswss
    ppc_altivec_vpkswus,                          // llvm.ppc.altivec.vpkswus
    ppc_altivec_vpkudus,                          // llvm.ppc.altivec.vpkudus
    ppc_altivec_vpkuhus,                          // llvm.ppc.altivec.vpkuhus
    ppc_altivec_vpkuwus,                          // llvm.ppc.altivec.vpkuwus
    ppc_altivec_vprtybd,                          // llvm.ppc.altivec.vprtybd
    ppc_altivec_vprtybq,                          // llvm.ppc.altivec.vprtybq
    ppc_altivec_vprtybw,                          // llvm.ppc.altivec.vprtybw
    ppc_altivec_vrefp,                            // llvm.ppc.altivec.vrefp
    ppc_altivec_vrfim,                            // llvm.ppc.altivec.vrfim
    ppc_altivec_vrfin,                            // llvm.ppc.altivec.vrfin
    ppc_altivec_vrfip,                            // llvm.ppc.altivec.vrfip
    ppc_altivec_vrfiz,                            // llvm.ppc.altivec.vrfiz
    ppc_altivec_vrlb,                             // llvm.ppc.altivec.vrlb
    ppc_altivec_vrld,                             // llvm.ppc.altivec.vrld
    ppc_altivec_vrldmi,                           // llvm.ppc.altivec.vrldmi
    ppc_altivec_vrldnm,                           // llvm.ppc.altivec.vrldnm
    ppc_altivec_vrlh,                             // llvm.ppc.altivec.vrlh
    ppc_altivec_vrlw,                             // llvm.ppc.altivec.vrlw
    ppc_altivec_vrlwmi,                           // llvm.ppc.altivec.vrlwmi
    ppc_altivec_vrlwnm,                           // llvm.ppc.altivec.vrlwnm
    ppc_altivec_vrsqrtefp,                        // llvm.ppc.altivec.vrsqrtefp
    ppc_altivec_vsel,                             // llvm.ppc.altivec.vsel
    ppc_altivec_vsl,                              // llvm.ppc.altivec.vsl
    ppc_altivec_vslb,                             // llvm.ppc.altivec.vslb
    ppc_altivec_vslh,                             // llvm.ppc.altivec.vslh
    ppc_altivec_vslo,                             // llvm.ppc.altivec.vslo
    ppc_altivec_vslv,                             // llvm.ppc.altivec.vslv
    ppc_altivec_vslw,                             // llvm.ppc.altivec.vslw
    ppc_altivec_vsr,                              // llvm.ppc.altivec.vsr
    ppc_altivec_vsrab,                            // llvm.ppc.altivec.vsrab
    ppc_altivec_vsrah,                            // llvm.ppc.altivec.vsrah
    ppc_altivec_vsraw,                            // llvm.ppc.altivec.vsraw
    ppc_altivec_vsrb,                             // llvm.ppc.altivec.vsrb
    ppc_altivec_vsrh,                             // llvm.ppc.altivec.vsrh
    ppc_altivec_vsro,                             // llvm.ppc.altivec.vsro
    ppc_altivec_vsrv,                             // llvm.ppc.altivec.vsrv
    ppc_altivec_vsrw,                             // llvm.ppc.altivec.vsrw
    ppc_altivec_vsubcuq,                          // llvm.ppc.altivec.vsubcuq
    ppc_altivec_vsubcuw,                          // llvm.ppc.altivec.vsubcuw
    ppc_altivec_vsubecuq,                         // llvm.ppc.altivec.vsubecuq
    ppc_altivec_vsubeuqm,                         // llvm.ppc.altivec.vsubeuqm
    ppc_altivec_vsubsbs,                          // llvm.ppc.altivec.vsubsbs
    ppc_altivec_vsubshs,                          // llvm.ppc.altivec.vsubshs
    ppc_altivec_vsubsws,                          // llvm.ppc.altivec.vsubsws
    ppc_altivec_vsububs,                          // llvm.ppc.altivec.vsububs
    ppc_altivec_vsubuhs,                          // llvm.ppc.altivec.vsubuhs
    ppc_altivec_vsubuws,                          // llvm.ppc.altivec.vsubuws
    ppc_altivec_vsum2sws,                         // llvm.ppc.altivec.vsum2sws
    ppc_altivec_vsum4sbs,                         // llvm.ppc.altivec.vsum4sbs
    ppc_altivec_vsum4shs,                         // llvm.ppc.altivec.vsum4shs
    ppc_altivec_vsum4ubs,                         // llvm.ppc.altivec.vsum4ubs
    ppc_altivec_vsumsws,                          // llvm.ppc.altivec.vsumsws
    ppc_altivec_vupkhpx,                          // llvm.ppc.altivec.vupkhpx
    ppc_altivec_vupkhsb,                          // llvm.ppc.altivec.vupkhsb
    ppc_altivec_vupkhsh,                          // llvm.ppc.altivec.vupkhsh
    ppc_altivec_vupkhsw,                          // llvm.ppc.altivec.vupkhsw
    ppc_altivec_vupklpx,                          // llvm.ppc.altivec.vupklpx
    ppc_altivec_vupklsb,                          // llvm.ppc.altivec.vupklsb
    ppc_altivec_vupklsh,                          // llvm.ppc.altivec.vupklsh
    ppc_altivec_vupklsw,                          // llvm.ppc.altivec.vupklsw
    ppc_bpermd,                                   // llvm.ppc.bpermd
    ppc_cfence,                                   // llvm.ppc.cfence
    ppc_dcba,                                     // llvm.ppc.dcba
    ppc_dcbf,                                     // llvm.ppc.dcbf
    ppc_dcbi,                                     // llvm.ppc.dcbi
    ppc_dcbst,                                    // llvm.ppc.dcbst
    ppc_dcbt,                                     // llvm.ppc.dcbt
    ppc_dcbtst,                                   // llvm.ppc.dcbtst
    ppc_dcbz,                                     // llvm.ppc.dcbz
    ppc_dcbzl,                                    // llvm.ppc.dcbzl
    ppc_divde,                                    // llvm.ppc.divde
    ppc_divdeu,                                   // llvm.ppc.divdeu
    ppc_divwe,                                    // llvm.ppc.divwe
    ppc_divweu,                                   // llvm.ppc.divweu
    ppc_get_texasr,                               // llvm.ppc.get.texasr
    ppc_get_texasru,                              // llvm.ppc.get.texasru
    ppc_get_tfhar,                                // llvm.ppc.get.tfhar
    ppc_get_tfiar,                                // llvm.ppc.get.tfiar
    ppc_is_decremented_ctr_nonzero,               // llvm.ppc.is.decremented.ctr.nonzero
    ppc_lwsync,                                   // llvm.ppc.lwsync
    ppc_mtctr,                                    // llvm.ppc.mtctr
    ppc_qpx_qvfabs,                               // llvm.ppc.qpx.qvfabs
    ppc_qpx_qvfadd,                               // llvm.ppc.qpx.qvfadd
    ppc_qpx_qvfadds,                              // llvm.ppc.qpx.qvfadds
    ppc_qpx_qvfcfid,                              // llvm.ppc.qpx.qvfcfid
    ppc_qpx_qvfcfids,                             // llvm.ppc.qpx.qvfcfids
    ppc_qpx_qvfcfidu,                             // llvm.ppc.qpx.qvfcfidu
    ppc_qpx_qvfcfidus,                            // llvm.ppc.qpx.qvfcfidus
    ppc_qpx_qvfcmpeq,                             // llvm.ppc.qpx.qvfcmpeq
    ppc_qpx_qvfcmpgt,                             // llvm.ppc.qpx.qvfcmpgt
    ppc_qpx_qvfcmplt,                             // llvm.ppc.qpx.qvfcmplt
    ppc_qpx_qvfcpsgn,                             // llvm.ppc.qpx.qvfcpsgn
    ppc_qpx_qvfctid,                              // llvm.ppc.qpx.qvfctid
    ppc_qpx_qvfctidu,                             // llvm.ppc.qpx.qvfctidu
    ppc_qpx_qvfctiduz,                            // llvm.ppc.qpx.qvfctiduz
    ppc_qpx_qvfctidz,                             // llvm.ppc.qpx.qvfctidz
    ppc_qpx_qvfctiw,                              // llvm.ppc.qpx.qvfctiw
    ppc_qpx_qvfctiwu,                             // llvm.ppc.qpx.qvfctiwu
    ppc_qpx_qvfctiwuz,                            // llvm.ppc.qpx.qvfctiwuz
    ppc_qpx_qvfctiwz,                             // llvm.ppc.qpx.qvfctiwz
    ppc_qpx_qvflogical,                           // llvm.ppc.qpx.qvflogical
    ppc_qpx_qvfmadd,                              // llvm.ppc.qpx.qvfmadd
    ppc_qpx_qvfmadds,                             // llvm.ppc.qpx.qvfmadds
    ppc_qpx_qvfmsub,                              // llvm.ppc.qpx.qvfmsub
    ppc_qpx_qvfmsubs,                             // llvm.ppc.qpx.qvfmsubs
    ppc_qpx_qvfmul,                               // llvm.ppc.qpx.qvfmul
    ppc_qpx_qvfmuls,                              // llvm.ppc.qpx.qvfmuls
    ppc_qpx_qvfnabs,                              // llvm.ppc.qpx.qvfnabs
    ppc_qpx_qvfneg,                               // llvm.ppc.qpx.qvfneg
    ppc_qpx_qvfnmadd,                             // llvm.ppc.qpx.qvfnmadd
    ppc_qpx_qvfnmadds,                            // llvm.ppc.qpx.qvfnmadds
    ppc_qpx_qvfnmsub,                             // llvm.ppc.qpx.qvfnmsub
    ppc_qpx_qvfnmsubs,                            // llvm.ppc.qpx.qvfnmsubs
    ppc_qpx_qvfperm,                              // llvm.ppc.qpx.qvfperm
    ppc_qpx_qvfre,                                // llvm.ppc.qpx.qvfre
    ppc_qpx_qvfres,                               // llvm.ppc.qpx.qvfres
    ppc_qpx_qvfrim,                               // llvm.ppc.qpx.qvfrim
    ppc_qpx_qvfrin,                               // llvm.ppc.qpx.qvfrin
    ppc_qpx_qvfrip,                               // llvm.ppc.qpx.qvfrip
    ppc_qpx_qvfriz,                               // llvm.ppc.qpx.qvfriz
    ppc_qpx_qvfrsp,                               // llvm.ppc.qpx.qvfrsp
    ppc_qpx_qvfrsqrte,                            // llvm.ppc.qpx.qvfrsqrte
    ppc_qpx_qvfrsqrtes,                           // llvm.ppc.qpx.qvfrsqrtes
    ppc_qpx_qvfsel,                               // llvm.ppc.qpx.qvfsel
    ppc_qpx_qvfsub,                               // llvm.ppc.qpx.qvfsub
    ppc_qpx_qvfsubs,                              // llvm.ppc.qpx.qvfsubs
    ppc_qpx_qvftstnan,                            // llvm.ppc.qpx.qvftstnan
    ppc_qpx_qvfxmadd,                             // llvm.ppc.qpx.qvfxmadd
    ppc_qpx_qvfxmadds,                            // llvm.ppc.qpx.qvfxmadds
    ppc_qpx_qvfxmul,                              // llvm.ppc.qpx.qvfxmul
    ppc_qpx_qvfxmuls,                             // llvm.ppc.qpx.qvfxmuls
    ppc_qpx_qvfxxcpnmadd,                         // llvm.ppc.qpx.qvfxxcpnmadd
    ppc_qpx_qvfxxcpnmadds,                        // llvm.ppc.qpx.qvfxxcpnmadds
    ppc_qpx_qvfxxmadd,                            // llvm.ppc.qpx.qvfxxmadd
    ppc_qpx_qvfxxmadds,                           // llvm.ppc.qpx.qvfxxmadds
    ppc_qpx_qvfxxnpmadd,                          // llvm.ppc.qpx.qvfxxnpmadd
    ppc_qpx_qvfxxnpmadds,                         // llvm.ppc.qpx.qvfxxnpmadds
    ppc_qpx_qvgpci,                               // llvm.ppc.qpx.qvgpci
    ppc_qpx_qvlfcd,                               // llvm.ppc.qpx.qvlfcd
    ppc_qpx_qvlfcda,                              // llvm.ppc.qpx.qvlfcda
    ppc_qpx_qvlfcs,                               // llvm.ppc.qpx.qvlfcs
    ppc_qpx_qvlfcsa,                              // llvm.ppc.qpx.qvlfcsa
    ppc_qpx_qvlfd,                                // llvm.ppc.qpx.qvlfd
    ppc_qpx_qvlfda,                               // llvm.ppc.qpx.qvlfda
    ppc_qpx_qvlfiwa,                              // llvm.ppc.qpx.qvlfiwa
    ppc_qpx_qvlfiwaa,                             // llvm.ppc.qpx.qvlfiwaa
    ppc_qpx_qvlfiwz,                              // llvm.ppc.qpx.qvlfiwz
    ppc_qpx_qvlfiwza,                             // llvm.ppc.qpx.qvlfiwza
    ppc_qpx_qvlfs,                                // llvm.ppc.qpx.qvlfs
    ppc_qpx_qvlfsa,                               // llvm.ppc.qpx.qvlfsa
    ppc_qpx_qvlpcld,                              // llvm.ppc.qpx.qvlpcld
    ppc_qpx_qvlpcls,                              // llvm.ppc.qpx.qvlpcls
    ppc_qpx_qvlpcrd,                              // llvm.ppc.qpx.qvlpcrd
    ppc_qpx_qvlpcrs,                              // llvm.ppc.qpx.qvlpcrs
    ppc_qpx_qvstfcd,                              // llvm.ppc.qpx.qvstfcd
    ppc_qpx_qvstfcda,                             // llvm.ppc.qpx.qvstfcda
    ppc_qpx_qvstfcs,                              // llvm.ppc.qpx.qvstfcs
    ppc_qpx_qvstfcsa,                             // llvm.ppc.qpx.qvstfcsa
    ppc_qpx_qvstfd,                               // llvm.ppc.qpx.qvstfd
    ppc_qpx_qvstfda,                              // llvm.ppc.qpx.qvstfda
    ppc_qpx_qvstfiw,                              // llvm.ppc.qpx.qvstfiw
    ppc_qpx_qvstfiwa,                             // llvm.ppc.qpx.qvstfiwa
    ppc_qpx_qvstfs,                               // llvm.ppc.qpx.qvstfs
    ppc_qpx_qvstfsa,                              // llvm.ppc.qpx.qvstfsa
    ppc_set_texasr,                               // llvm.ppc.set.texasr
    ppc_set_texasru,                              // llvm.ppc.set.texasru
    ppc_set_tfhar,                                // llvm.ppc.set.tfhar
    ppc_set_tfiar,                                // llvm.ppc.set.tfiar
    ppc_sync,                                     // llvm.ppc.sync
    ppc_tabort,                                   // llvm.ppc.tabort
    ppc_tabortdc,                                 // llvm.ppc.tabortdc
    ppc_tabortdci,                                // llvm.ppc.tabortdci
    ppc_tabortwc,                                 // llvm.ppc.tabortwc
    ppc_tabortwci,                                // llvm.ppc.tabortwci
    ppc_tbegin,                                   // llvm.ppc.tbegin
    ppc_tcheck,                                   // llvm.ppc.tcheck
    ppc_tend,                                     // llvm.ppc.tend
    ppc_tendall,                                  // llvm.ppc.tendall
    ppc_trechkpt,                                 // llvm.ppc.trechkpt
    ppc_treclaim,                                 // llvm.ppc.treclaim
    ppc_tresume,                                  // llvm.ppc.tresume
    ppc_tsr,                                      // llvm.ppc.tsr
    ppc_tsuspend,                                 // llvm.ppc.tsuspend
    ppc_ttest,                                    // llvm.ppc.ttest
    ppc_vsx_lxvd2x,                               // llvm.ppc.vsx.lxvd2x
    ppc_vsx_lxvd2x_be,                            // llvm.ppc.vsx.lxvd2x.be
    ppc_vsx_lxvl,                                 // llvm.ppc.vsx.lxvl
    ppc_vsx_lxvll,                                // llvm.ppc.vsx.lxvll
    ppc_vsx_lxvw4x,                               // llvm.ppc.vsx.lxvw4x
    ppc_vsx_lxvw4x_be,                            // llvm.ppc.vsx.lxvw4x.be
    ppc_vsx_stxvd2x,                              // llvm.ppc.vsx.stxvd2x
    ppc_vsx_stxvd2x_be,                           // llvm.ppc.vsx.stxvd2x.be
    ppc_vsx_stxvl,                                // llvm.ppc.vsx.stxvl
    ppc_vsx_stxvll,                               // llvm.ppc.vsx.stxvll
    ppc_vsx_stxvw4x,                              // llvm.ppc.vsx.stxvw4x
    ppc_vsx_stxvw4x_be,                           // llvm.ppc.vsx.stxvw4x.be
    ppc_vsx_xsmaxdp,                              // llvm.ppc.vsx.xsmaxdp
    ppc_vsx_xsmindp,                              // llvm.ppc.vsx.xsmindp
    ppc_vsx_xvcmpeqdp,                            // llvm.ppc.vsx.xvcmpeqdp
    ppc_vsx_xvcmpeqdp_p,                          // llvm.ppc.vsx.xvcmpeqdp.p
    ppc_vsx_xvcmpeqsp,                            // llvm.ppc.vsx.xvcmpeqsp
    ppc_vsx_xvcmpeqsp_p,                          // llvm.ppc.vsx.xvcmpeqsp.p
    ppc_vsx_xvcmpgedp,                            // llvm.ppc.vsx.xvcmpgedp
    ppc_vsx_xvcmpgedp_p,                          // llvm.ppc.vsx.xvcmpgedp.p
    ppc_vsx_xvcmpgesp,                            // llvm.ppc.vsx.xvcmpgesp
    ppc_vsx_xvcmpgesp_p,                          // llvm.ppc.vsx.xvcmpgesp.p
    ppc_vsx_xvcmpgtdp,                            // llvm.ppc.vsx.xvcmpgtdp
    ppc_vsx_xvcmpgtdp_p,                          // llvm.ppc.vsx.xvcmpgtdp.p
    ppc_vsx_xvcmpgtsp,                            // llvm.ppc.vsx.xvcmpgtsp
    ppc_vsx_xvcmpgtsp_p,                          // llvm.ppc.vsx.xvcmpgtsp.p
    ppc_vsx_xvcvdpsp,                             // llvm.ppc.vsx.xvcvdpsp
    ppc_vsx_xvcvdpsxws,                           // llvm.ppc.vsx.xvcvdpsxws
    ppc_vsx_xvcvdpuxws,                           // llvm.ppc.vsx.xvcvdpuxws
    ppc_vsx_xvcvhpsp,                             // llvm.ppc.vsx.xvcvhpsp
    ppc_vsx_xvcvspdp,                             // llvm.ppc.vsx.xvcvspdp
    ppc_vsx_xvcvsphp,                             // llvm.ppc.vsx.xvcvsphp
    ppc_vsx_xvcvsxdsp,                            // llvm.ppc.vsx.xvcvsxdsp
    ppc_vsx_xvcvsxwdp,                            // llvm.ppc.vsx.xvcvsxwdp
    ppc_vsx_xvcvuxdsp,                            // llvm.ppc.vsx.xvcvuxdsp
    ppc_vsx_xvcvuxwdp,                            // llvm.ppc.vsx.xvcvuxwdp
    ppc_vsx_xvdivdp,                              // llvm.ppc.vsx.xvdivdp
    ppc_vsx_xvdivsp,                              // llvm.ppc.vsx.xvdivsp
    ppc_vsx_xviexpdp,                             // llvm.ppc.vsx.xviexpdp
    ppc_vsx_xviexpsp,                             // llvm.ppc.vsx.xviexpsp
    ppc_vsx_xvmaxdp,                              // llvm.ppc.vsx.xvmaxdp
    ppc_vsx_xvmaxsp,                              // llvm.ppc.vsx.xvmaxsp
    ppc_vsx_xvmindp,                              // llvm.ppc.vsx.xvmindp
    ppc_vsx_xvminsp,                              // llvm.ppc.vsx.xvminsp
    ppc_vsx_xvrdpip,                              // llvm.ppc.vsx.xvrdpip
    ppc_vsx_xvredp,                               // llvm.ppc.vsx.xvredp
    ppc_vsx_xvresp,                               // llvm.ppc.vsx.xvresp
    ppc_vsx_xvrspip,                              // llvm.ppc.vsx.xvrspip
    ppc_vsx_xvrsqrtedp,                           // llvm.ppc.vsx.xvrsqrtedp
    ppc_vsx_xvrsqrtesp,                           // llvm.ppc.vsx.xvrsqrtesp
    ppc_vsx_xvtstdcdp,                            // llvm.ppc.vsx.xvtstdcdp
    ppc_vsx_xvtstdcsp,                            // llvm.ppc.vsx.xvtstdcsp
    ppc_vsx_xvxexpdp,                             // llvm.ppc.vsx.xvxexpdp
    ppc_vsx_xvxexpsp,                             // llvm.ppc.vsx.xvxexpsp
    ppc_vsx_xvxsigdp,                             // llvm.ppc.vsx.xvxsigdp
    ppc_vsx_xvxsigsp,                             // llvm.ppc.vsx.xvxsigsp
    ppc_vsx_xxextractuw,                          // llvm.ppc.vsx.xxextractuw
    ppc_vsx_xxinsertw,                            // llvm.ppc.vsx.xxinsertw
    ppc_vsx_xxleqv,                               // llvm.ppc.vsx.xxleqv
    r600_cube,                                    // llvm.r600.cube
    r600_group_barrier,                           // llvm.r600.group.barrier
    r600_implicitarg_ptr,                         // llvm.r600.implicitarg.ptr
    r600_rat_store_typed,                         // llvm.r600.rat.store.typed
    r600_read_global_size_x,                      // llvm.r600.read.global.size.x
    r600_read_global_size_y,                      // llvm.r600.read.global.size.y
    r600_read_global_size_z,                      // llvm.r600.read.global.size.z
    r600_read_local_size_x,                       // llvm.r600.read.local.size.x
    r600_read_local_size_y,                       // llvm.r600.read.local.size.y
    r600_read_local_size_z,                       // llvm.r600.read.local.size.z
    r600_read_ngroups_x,                          // llvm.r600.read.ngroups.x
    r600_read_ngroups_y,                          // llvm.r600.read.ngroups.y
    r600_read_ngroups_z,                          // llvm.r600.read.ngroups.z
    r600_read_tgid_x,                             // llvm.r600.read.tgid.x
    r600_read_tgid_y,                             // llvm.r600.read.tgid.y
    r600_read_tgid_z,                             // llvm.r600.read.tgid.z
    r600_read_tidig_x,                            // llvm.r600.read.tidig.x
    r600_read_tidig_y,                            // llvm.r600.read.tidig.y
    r600_read_tidig_z,                            // llvm.r600.read.tidig.z
    r600_recipsqrt_clamped,                       // llvm.r600.recipsqrt.clamped
    r600_recipsqrt_ieee,                          // llvm.r600.recipsqrt.ieee
    s390_efpc,                                    // llvm.s390.efpc
    s390_etnd,                                    // llvm.s390.etnd
    s390_lcbb,                                    // llvm.s390.lcbb
    s390_ntstg,                                   // llvm.s390.ntstg
    s390_ppa_txassist,                            // llvm.s390.ppa.txassist
    s390_sfpc,                                    // llvm.s390.sfpc
    s390_tabort,                                  // llvm.s390.tabort
    s390_tbegin,                                  // llvm.s390.tbegin
    s390_tbegin_nofloat,                          // llvm.s390.tbegin.nofloat
    s390_tbeginc,                                 // llvm.s390.tbeginc
    s390_tdc,                                     // llvm.s390.tdc
    s390_tend,                                    // llvm.s390.tend
    s390_vaccb,                                   // llvm.s390.vaccb
    s390_vacccq,                                  // llvm.s390.vacccq
    s390_vaccf,                                   // llvm.s390.vaccf
    s390_vaccg,                                   // llvm.s390.vaccg
    s390_vacch,                                   // llvm.s390.vacch
    s390_vaccq,                                   // llvm.s390.vaccq
    s390_vacq,                                    // llvm.s390.vacq
    s390_vaq,                                     // llvm.s390.vaq
    s390_vavgb,                                   // llvm.s390.vavgb
    s390_vavgf,                                   // llvm.s390.vavgf
    s390_vavgg,                                   // llvm.s390.vavgg
    s390_vavgh,                                   // llvm.s390.vavgh
    s390_vavglb,                                  // llvm.s390.vavglb
    s390_vavglf,                                  // llvm.s390.vavglf
    s390_vavglg,                                  // llvm.s390.vavglg
    s390_vavglh,                                  // llvm.s390.vavglh
    s390_vbperm,                                  // llvm.s390.vbperm
    s390_vceqbs,                                  // llvm.s390.vceqbs
    s390_vceqfs,                                  // llvm.s390.vceqfs
    s390_vceqgs,                                  // llvm.s390.vceqgs
    s390_vceqhs,                                  // llvm.s390.vceqhs
    s390_vchbs,                                   // llvm.s390.vchbs
    s390_vchfs,                                   // llvm.s390.vchfs
    s390_vchgs,                                   // llvm.s390.vchgs
    s390_vchhs,                                   // llvm.s390.vchhs
    s390_vchlbs,                                  // llvm.s390.vchlbs
    s390_vchlfs,                                  // llvm.s390.vchlfs
    s390_vchlgs,                                  // llvm.s390.vchlgs
    s390_vchlhs,                                  // llvm.s390.vchlhs
    s390_vcksm,                                   // llvm.s390.vcksm
    s390_verimb,                                  // llvm.s390.verimb
    s390_verimf,                                  // llvm.s390.verimf
    s390_verimg,                                  // llvm.s390.verimg
    s390_verimh,                                  // llvm.s390.verimh
    s390_verllb,                                  // llvm.s390.verllb
    s390_verllf,                                  // llvm.s390.verllf
    s390_verllg,                                  // llvm.s390.verllg
    s390_verllh,                                  // llvm.s390.verllh
    s390_verllvb,                                 // llvm.s390.verllvb
    s390_verllvf,                                 // llvm.s390.verllvf
    s390_verllvg,                                 // llvm.s390.verllvg
    s390_verllvh,                                 // llvm.s390.verllvh
    s390_vfaeb,                                   // llvm.s390.vfaeb
    s390_vfaebs,                                  // llvm.s390.vfaebs
    s390_vfaef,                                   // llvm.s390.vfaef
    s390_vfaefs,                                  // llvm.s390.vfaefs
    s390_vfaeh,                                   // llvm.s390.vfaeh
    s390_vfaehs,                                  // llvm.s390.vfaehs
    s390_vfaezb,                                  // llvm.s390.vfaezb
    s390_vfaezbs,                                 // llvm.s390.vfaezbs
    s390_vfaezf,                                  // llvm.s390.vfaezf
    s390_vfaezfs,                                 // llvm.s390.vfaezfs
    s390_vfaezh,                                  // llvm.s390.vfaezh
    s390_vfaezhs,                                 // llvm.s390.vfaezhs
    s390_vfcedbs,                                 // llvm.s390.vfcedbs
    s390_vfcesbs,                                 // llvm.s390.vfcesbs
    s390_vfchdbs,                                 // llvm.s390.vfchdbs
    s390_vfchedbs,                                // llvm.s390.vfchedbs
    s390_vfchesbs,                                // llvm.s390.vfchesbs
    s390_vfchsbs,                                 // llvm.s390.vfchsbs
    s390_vfeeb,                                   // llvm.s390.vfeeb
    s390_vfeebs,                                  // llvm.s390.vfeebs
    s390_vfeef,                                   // llvm.s390.vfeef
    s390_vfeefs,                                  // llvm.s390.vfeefs
    s390_vfeeh,                                   // llvm.s390.vfeeh
    s390_vfeehs,                                  // llvm.s390.vfeehs
    s390_vfeezb,                                  // llvm.s390.vfeezb
    s390_vfeezbs,                                 // llvm.s390.vfeezbs
    s390_vfeezf,                                  // llvm.s390.vfeezf
    s390_vfeezfs,                                 // llvm.s390.vfeezfs
    s390_vfeezh,                                  // llvm.s390.vfeezh
    s390_vfeezhs,                                 // llvm.s390.vfeezhs
    s390_vfeneb,                                  // llvm.s390.vfeneb
    s390_vfenebs,                                 // llvm.s390.vfenebs
    s390_vfenef,                                  // llvm.s390.vfenef
    s390_vfenefs,                                 // llvm.s390.vfenefs
    s390_vfeneh,                                  // llvm.s390.vfeneh
    s390_vfenehs,                                 // llvm.s390.vfenehs
    s390_vfenezb,                                 // llvm.s390.vfenezb
    s390_vfenezbs,                                // llvm.s390.vfenezbs
    s390_vfenezf,                                 // llvm.s390.vfenezf
    s390_vfenezfs,                                // llvm.s390.vfenezfs
    s390_vfenezh,                                 // llvm.s390.vfenezh
    s390_vfenezhs,                                // llvm.s390.vfenezhs
    s390_vfidb,                                   // llvm.s390.vfidb
    s390_vfisb,                                   // llvm.s390.vfisb
    s390_vfmaxdb,                                 // llvm.s390.vfmaxdb
    s390_vfmaxsb,                                 // llvm.s390.vfmaxsb
    s390_vfmindb,                                 // llvm.s390.vfmindb
    s390_vfminsb,                                 // llvm.s390.vfminsb
    s390_vftcidb,                                 // llvm.s390.vftcidb
    s390_vftcisb,                                 // llvm.s390.vftcisb
    s390_vgfmab,                                  // llvm.s390.vgfmab
    s390_vgfmaf,                                  // llvm.s390.vgfmaf
    s390_vgfmag,                                  // llvm.s390.vgfmag
    s390_vgfmah,                                  // llvm.s390.vgfmah
    s390_vgfmb,                                   // llvm.s390.vgfmb
    s390_vgfmf,                                   // llvm.s390.vgfmf
    s390_vgfmg,                                   // llvm.s390.vgfmg
    s390_vgfmh,                                   // llvm.s390.vgfmh
    s390_vistrb,                                  // llvm.s390.vistrb
    s390_vistrbs,                                 // llvm.s390.vistrbs
    s390_vistrf,                                  // llvm.s390.vistrf
    s390_vistrfs,                                 // llvm.s390.vistrfs
    s390_vistrh,                                  // llvm.s390.vistrh
    s390_vistrhs,                                 // llvm.s390.vistrhs
    s390_vlbb,                                    // llvm.s390.vlbb
    s390_vll,                                     // llvm.s390.vll
    s390_vlrl,                                    // llvm.s390.vlrl
    s390_vmaeb,                                   // llvm.s390.vmaeb
    s390_vmaef,                                   // llvm.s390.vmaef
    s390_vmaeh,                                   // llvm.s390.vmaeh
    s390_vmahb,                                   // llvm.s390.vmahb
    s390_vmahf,                                   // llvm.s390.vmahf
    s390_vmahh,                                   // llvm.s390.vmahh
    s390_vmaleb,                                  // llvm.s390.vmaleb
    s390_vmalef,                                  // llvm.s390.vmalef
    s390_vmaleh,                                  // llvm.s390.vmaleh
    s390_vmalhb,                                  // llvm.s390.vmalhb
    s390_vmalhf,                                  // llvm.s390.vmalhf
    s390_vmalhh,                                  // llvm.s390.vmalhh
    s390_vmalob,                                  // llvm.s390.vmalob
    s390_vmalof,                                  // llvm.s390.vmalof
    s390_vmaloh,                                  // llvm.s390.vmaloh
    s390_vmaob,                                   // llvm.s390.vmaob
    s390_vmaof,                                   // llvm.s390.vmaof
    s390_vmaoh,                                   // llvm.s390.vmaoh
    s390_vmeb,                                    // llvm.s390.vmeb
    s390_vmef,                                    // llvm.s390.vmef
    s390_vmeh,                                    // llvm.s390.vmeh
    s390_vmhb,                                    // llvm.s390.vmhb
    s390_vmhf,                                    // llvm.s390.vmhf
    s390_vmhh,                                    // llvm.s390.vmhh
    s390_vmleb,                                   // llvm.s390.vmleb
    s390_vmlef,                                   // llvm.s390.vmlef
    s390_vmleh,                                   // llvm.s390.vmleh
    s390_vmlhb,                                   // llvm.s390.vmlhb
    s390_vmlhf,                                   // llvm.s390.vmlhf
    s390_vmlhh,                                   // llvm.s390.vmlhh
    s390_vmlob,                                   // llvm.s390.vmlob
    s390_vmlof,                                   // llvm.s390.vmlof
    s390_vmloh,                                   // llvm.s390.vmloh
    s390_vmob,                                    // llvm.s390.vmob
    s390_vmof,                                    // llvm.s390.vmof
    s390_vmoh,                                    // llvm.s390.vmoh
    s390_vmslg,                                   // llvm.s390.vmslg
    s390_vpdi,                                    // llvm.s390.vpdi
    s390_vperm,                                   // llvm.s390.vperm
    s390_vpklsf,                                  // llvm.s390.vpklsf
    s390_vpklsfs,                                 // llvm.s390.vpklsfs
    s390_vpklsg,                                  // llvm.s390.vpklsg
    s390_vpklsgs,                                 // llvm.s390.vpklsgs
    s390_vpklsh,                                  // llvm.s390.vpklsh
    s390_vpklshs,                                 // llvm.s390.vpklshs
    s390_vpksf,                                   // llvm.s390.vpksf
    s390_vpksfs,                                  // llvm.s390.vpksfs
    s390_vpksg,                                   // llvm.s390.vpksg
    s390_vpksgs,                                  // llvm.s390.vpksgs
    s390_vpksh,                                   // llvm.s390.vpksh
    s390_vpkshs,                                  // llvm.s390.vpkshs
    s390_vsbcbiq,                                 // llvm.s390.vsbcbiq
    s390_vsbiq,                                   // llvm.s390.vsbiq
    s390_vscbib,                                  // llvm.s390.vscbib
    s390_vscbif,                                  // llvm.s390.vscbif
    s390_vscbig,                                  // llvm.s390.vscbig
    s390_vscbih,                                  // llvm.s390.vscbih
    s390_vscbiq,                                  // llvm.s390.vscbiq
    s390_vsl,                                     // llvm.s390.vsl
    s390_vslb,                                    // llvm.s390.vslb
    s390_vsldb,                                   // llvm.s390.vsldb
    s390_vsq,                                     // llvm.s390.vsq
    s390_vsra,                                    // llvm.s390.vsra
    s390_vsrab,                                   // llvm.s390.vsrab
    s390_vsrl,                                    // llvm.s390.vsrl
    s390_vsrlb,                                   // llvm.s390.vsrlb
    s390_vstl,                                    // llvm.s390.vstl
    s390_vstrcb,                                  // llvm.s390.vstrcb
    s390_vstrcbs,                                 // llvm.s390.vstrcbs
    s390_vstrcf,                                  // llvm.s390.vstrcf
    s390_vstrcfs,                                 // llvm.s390.vstrcfs
    s390_vstrch,                                  // llvm.s390.vstrch
    s390_vstrchs,                                 // llvm.s390.vstrchs
    s390_vstrczb,                                 // llvm.s390.vstrczb
    s390_vstrczbs,                                // llvm.s390.vstrczbs
    s390_vstrczf,                                 // llvm.s390.vstrczf
    s390_vstrczfs,                                // llvm.s390.vstrczfs
    s390_vstrczh,                                 // llvm.s390.vstrczh
    s390_vstrczhs,                                // llvm.s390.vstrczhs
    s390_vstrl,                                   // llvm.s390.vstrl
    s390_vsumb,                                   // llvm.s390.vsumb
    s390_vsumgf,                                  // llvm.s390.vsumgf
    s390_vsumgh,                                  // llvm.s390.vsumgh
    s390_vsumh,                                   // llvm.s390.vsumh
    s390_vsumqf,                                  // llvm.s390.vsumqf
    s390_vsumqg,                                  // llvm.s390.vsumqg
    s390_vtm,                                     // llvm.s390.vtm
    s390_vuphb,                                   // llvm.s390.vuphb
    s390_vuphf,                                   // llvm.s390.vuphf
    s390_vuphh,                                   // llvm.s390.vuphh
    s390_vuplb,                                   // llvm.s390.vuplb
    s390_vuplf,                                   // llvm.s390.vuplf
    s390_vuplhb,                                  // llvm.s390.vuplhb
    s390_vuplhf,                                  // llvm.s390.vuplhf
    s390_vuplhh,                                  // llvm.s390.vuplhh
    s390_vuplhw,                                  // llvm.s390.vuplhw
    s390_vupllb,                                  // llvm.s390.vupllb
    s390_vupllf,                                  // llvm.s390.vupllf
    s390_vupllh,                                  // llvm.s390.vupllh
    wasm_current_memory,                          // llvm.wasm.current.memory
    wasm_grow_memory,                             // llvm.wasm.grow.memory
    wasm_rethrow,                                 // llvm.wasm.rethrow
    wasm_throw,                                   // llvm.wasm.throw
    x86_3dnow_pavgusb,                            // llvm.x86.3dnow.pavgusb
    x86_3dnow_pf2id,                              // llvm.x86.3dnow.pf2id
    x86_3dnow_pfacc,                              // llvm.x86.3dnow.pfacc
    x86_3dnow_pfadd,                              // llvm.x86.3dnow.pfadd
    x86_3dnow_pfcmpeq,                            // llvm.x86.3dnow.pfcmpeq
    x86_3dnow_pfcmpge,                            // llvm.x86.3dnow.pfcmpge
    x86_3dnow_pfcmpgt,                            // llvm.x86.3dnow.pfcmpgt
    x86_3dnow_pfmax,                              // llvm.x86.3dnow.pfmax
    x86_3dnow_pfmin,                              // llvm.x86.3dnow.pfmin
    x86_3dnow_pfmul,                              // llvm.x86.3dnow.pfmul
    x86_3dnow_pfrcp,                              // llvm.x86.3dnow.pfrcp
    x86_3dnow_pfrcpit1,                           // llvm.x86.3dnow.pfrcpit1
    x86_3dnow_pfrcpit2,                           // llvm.x86.3dnow.pfrcpit2
    x86_3dnow_pfrsqit1,                           // llvm.x86.3dnow.pfrsqit1
    x86_3dnow_pfrsqrt,                            // llvm.x86.3dnow.pfrsqrt
    x86_3dnow_pfsub,                              // llvm.x86.3dnow.pfsub
    x86_3dnow_pfsubr,                             // llvm.x86.3dnow.pfsubr
    x86_3dnow_pi2fd,                              // llvm.x86.3dnow.pi2fd
    x86_3dnow_pmulhrw,                            // llvm.x86.3dnow.pmulhrw
    x86_3dnowa_pf2iw,                             // llvm.x86.3dnowa.pf2iw
    x86_3dnowa_pfnacc,                            // llvm.x86.3dnowa.pfnacc
    x86_3dnowa_pfpnacc,                           // llvm.x86.3dnowa.pfpnacc
    x86_3dnowa_pi2fw,                             // llvm.x86.3dnowa.pi2fw
    x86_3dnowa_pswapd,                            // llvm.x86.3dnowa.pswapd
    x86_addcarry_u32,                             // llvm.x86.addcarry.u32
    x86_addcarry_u64,                             // llvm.x86.addcarry.u64
    x86_addcarryx_u32,                            // llvm.x86.addcarryx.u32
    x86_addcarryx_u64,                            // llvm.x86.addcarryx.u64
    x86_aesni_aesdec,                             // llvm.x86.aesni.aesdec
    x86_aesni_aesdec_256,                         // llvm.x86.aesni.aesdec.256
    x86_aesni_aesdec_512,                         // llvm.x86.aesni.aesdec.512
    x86_aesni_aesdeclast,                         // llvm.x86.aesni.aesdeclast
    x86_aesni_aesdeclast_256,                     // llvm.x86.aesni.aesdeclast.256
    x86_aesni_aesdeclast_512,                     // llvm.x86.aesni.aesdeclast.512
    x86_aesni_aesenc,                             // llvm.x86.aesni.aesenc
    x86_aesni_aesenc_256,                         // llvm.x86.aesni.aesenc.256
    x86_aesni_aesenc_512,                         // llvm.x86.aesni.aesenc.512
    x86_aesni_aesenclast,                         // llvm.x86.aesni.aesenclast
    x86_aesni_aesenclast_256,                     // llvm.x86.aesni.aesenclast.256
    x86_aesni_aesenclast_512,                     // llvm.x86.aesni.aesenclast.512
    x86_aesni_aesimc,                             // llvm.x86.aesni.aesimc
    x86_aesni_aeskeygenassist,                    // llvm.x86.aesni.aeskeygenassist
    x86_avx_addsub_pd_256,                        // llvm.x86.avx.addsub.pd.256
    x86_avx_addsub_ps_256,                        // llvm.x86.avx.addsub.ps.256
    x86_avx_blendv_pd_256,                        // llvm.x86.avx.blendv.pd.256
    x86_avx_blendv_ps_256,                        // llvm.x86.avx.blendv.ps.256
    x86_avx_cmp_pd_256,                           // llvm.x86.avx.cmp.pd.256
    x86_avx_cmp_ps_256,                           // llvm.x86.avx.cmp.ps.256
    x86_avx_cvt_pd2_ps_256,                       // llvm.x86.avx.cvt.pd2.ps.256
    x86_avx_cvt_pd2dq_256,                        // llvm.x86.avx.cvt.pd2dq.256
    x86_avx_cvt_ps2dq_256,                        // llvm.x86.avx.cvt.ps2dq.256
    x86_avx_cvtdq2_ps_256,                        // llvm.x86.avx.cvtdq2.ps.256
    x86_avx_cvtt_pd2dq_256,                       // llvm.x86.avx.cvtt.pd2dq.256
    x86_avx_cvtt_ps2dq_256,                       // llvm.x86.avx.cvtt.ps2dq.256
    x86_avx_dp_ps_256,                            // llvm.x86.avx.dp.ps.256
    x86_avx_hadd_pd_256,                          // llvm.x86.avx.hadd.pd.256
    x86_avx_hadd_ps_256,                          // llvm.x86.avx.hadd.ps.256
    x86_avx_hsub_pd_256,                          // llvm.x86.avx.hsub.pd.256
    x86_avx_hsub_ps_256,                          // llvm.x86.avx.hsub.ps.256
    x86_avx_ldu_dq_256,                           // llvm.x86.avx.ldu.dq.256
    x86_avx_maskload_pd,                          // llvm.x86.avx.maskload.pd
    x86_avx_maskload_pd_256,                      // llvm.x86.avx.maskload.pd.256
    x86_avx_maskload_ps,                          // llvm.x86.avx.maskload.ps
    x86_avx_maskload_ps_256,                      // llvm.x86.avx.maskload.ps.256
    x86_avx_maskstore_pd,                         // llvm.x86.avx.maskstore.pd
    x86_avx_maskstore_pd_256,                     // llvm.x86.avx.maskstore.pd.256
    x86_avx_maskstore_ps,                         // llvm.x86.avx.maskstore.ps
    x86_avx_maskstore_ps_256,                     // llvm.x86.avx.maskstore.ps.256
    x86_avx_max_pd_256,                           // llvm.x86.avx.max.pd.256
    x86_avx_max_ps_256,                           // llvm.x86.avx.max.ps.256
    x86_avx_min_pd_256,                           // llvm.x86.avx.min.pd.256
    x86_avx_min_ps_256,                           // llvm.x86.avx.min.ps.256
    x86_avx_movmsk_pd_256,                        // llvm.x86.avx.movmsk.pd.256
    x86_avx_movmsk_ps_256,                        // llvm.x86.avx.movmsk.ps.256
    x86_avx_ptestc_256,                           // llvm.x86.avx.ptestc.256
    x86_avx_ptestnzc_256,                         // llvm.x86.avx.ptestnzc.256
    x86_avx_ptestz_256,                           // llvm.x86.avx.ptestz.256
    x86_avx_rcp_ps_256,                           // llvm.x86.avx.rcp.ps.256
    x86_avx_round_pd_256,                         // llvm.x86.avx.round.pd.256
    x86_avx_round_ps_256,                         // llvm.x86.avx.round.ps.256
    x86_avx_rsqrt_ps_256,                         // llvm.x86.avx.rsqrt.ps.256
    x86_avx_sqrt_pd_256,                          // llvm.x86.avx.sqrt.pd.256
    x86_avx_sqrt_ps_256,                          // llvm.x86.avx.sqrt.ps.256
    x86_avx_vpermilvar_pd,                        // llvm.x86.avx.vpermilvar.pd
    x86_avx_vpermilvar_pd_256,                    // llvm.x86.avx.vpermilvar.pd.256
    x86_avx_vpermilvar_ps,                        // llvm.x86.avx.vpermilvar.ps
    x86_avx_vpermilvar_ps_256,                    // llvm.x86.avx.vpermilvar.ps.256
    x86_avx_vtestc_pd,                            // llvm.x86.avx.vtestc.pd
    x86_avx_vtestc_pd_256,                        // llvm.x86.avx.vtestc.pd.256
    x86_avx_vtestc_ps,                            // llvm.x86.avx.vtestc.ps
    x86_avx_vtestc_ps_256,                        // llvm.x86.avx.vtestc.ps.256
    x86_avx_vtestnzc_pd,                          // llvm.x86.avx.vtestnzc.pd
    x86_avx_vtestnzc_pd_256,                      // llvm.x86.avx.vtestnzc.pd.256
    x86_avx_vtestnzc_ps,                          // llvm.x86.avx.vtestnzc.ps
    x86_avx_vtestnzc_ps_256,                      // llvm.x86.avx.vtestnzc.ps.256
    x86_avx_vtestz_pd,                            // llvm.x86.avx.vtestz.pd
    x86_avx_vtestz_pd_256,                        // llvm.x86.avx.vtestz.pd.256
    x86_avx_vtestz_ps,                            // llvm.x86.avx.vtestz.ps
    x86_avx_vtestz_ps_256,                        // llvm.x86.avx.vtestz.ps.256
    x86_avx_vzeroall,                             // llvm.x86.avx.vzeroall
    x86_avx_vzeroupper,                           // llvm.x86.avx.vzeroupper
    x86_avx2_gather_d_d,                          // llvm.x86.avx2.gather.d.d
    x86_avx2_gather_d_d_256,                      // llvm.x86.avx2.gather.d.d.256
    x86_avx2_gather_d_pd,                         // llvm.x86.avx2.gather.d.pd
    x86_avx2_gather_d_pd_256,                     // llvm.x86.avx2.gather.d.pd.256
    x86_avx2_gather_d_ps,                         // llvm.x86.avx2.gather.d.ps
    x86_avx2_gather_d_ps_256,                     // llvm.x86.avx2.gather.d.ps.256
    x86_avx2_gather_d_q,                          // llvm.x86.avx2.gather.d.q
    x86_avx2_gather_d_q_256,                      // llvm.x86.avx2.gather.d.q.256
    x86_avx2_gather_q_d,                          // llvm.x86.avx2.gather.q.d
    x86_avx2_gather_q_d_256,                      // llvm.x86.avx2.gather.q.d.256
    x86_avx2_gather_q_pd,                         // llvm.x86.avx2.gather.q.pd
    x86_avx2_gather_q_pd_256,                     // llvm.x86.avx2.gather.q.pd.256
    x86_avx2_gather_q_ps,                         // llvm.x86.avx2.gather.q.ps
    x86_avx2_gather_q_ps_256,                     // llvm.x86.avx2.gather.q.ps.256
    x86_avx2_gather_q_q,                          // llvm.x86.avx2.gather.q.q
    x86_avx2_gather_q_q_256,                      // llvm.x86.avx2.gather.q.q.256
    x86_avx2_maskload_d,                          // llvm.x86.avx2.maskload.d
    x86_avx2_maskload_d_256,                      // llvm.x86.avx2.maskload.d.256
    x86_avx2_maskload_q,                          // llvm.x86.avx2.maskload.q
    x86_avx2_maskload_q_256,                      // llvm.x86.avx2.maskload.q.256
    x86_avx2_maskstore_d,                         // llvm.x86.avx2.maskstore.d
    x86_avx2_maskstore_d_256,                     // llvm.x86.avx2.maskstore.d.256
    x86_avx2_maskstore_q,                         // llvm.x86.avx2.maskstore.q
    x86_avx2_maskstore_q_256,                     // llvm.x86.avx2.maskstore.q.256
    x86_avx2_mpsadbw,                             // llvm.x86.avx2.mpsadbw
    x86_avx2_packssdw,                            // llvm.x86.avx2.packssdw
    x86_avx2_packsswb,                            // llvm.x86.avx2.packsswb
    x86_avx2_packusdw,                            // llvm.x86.avx2.packusdw
    x86_avx2_packuswb,                            // llvm.x86.avx2.packuswb
    x86_avx2_padds_b,                             // llvm.x86.avx2.padds.b
    x86_avx2_padds_w,                             // llvm.x86.avx2.padds.w
    x86_avx2_paddus_b,                            // llvm.x86.avx2.paddus.b
    x86_avx2_paddus_w,                            // llvm.x86.avx2.paddus.w
    x86_avx2_pblendvb,                            // llvm.x86.avx2.pblendvb
    x86_avx2_permd,                               // llvm.x86.avx2.permd
    x86_avx2_permps,                              // llvm.x86.avx2.permps
    x86_avx2_phadd_d,                             // llvm.x86.avx2.phadd.d
    x86_avx2_phadd_sw,                            // llvm.x86.avx2.phadd.sw
    x86_avx2_phadd_w,                             // llvm.x86.avx2.phadd.w
    x86_avx2_phsub_d,                             // llvm.x86.avx2.phsub.d
    x86_avx2_phsub_sw,                            // llvm.x86.avx2.phsub.sw
    x86_avx2_phsub_w,                             // llvm.x86.avx2.phsub.w
    x86_avx2_pmadd_ub_sw,                         // llvm.x86.avx2.pmadd.ub.sw
    x86_avx2_pmadd_wd,                            // llvm.x86.avx2.pmadd.wd
    x86_avx2_pmovmskb,                            // llvm.x86.avx2.pmovmskb
    x86_avx2_pmul_dq,                             // llvm.x86.avx2.pmul.dq
    x86_avx2_pmul_hr_sw,                          // llvm.x86.avx2.pmul.hr.sw
    x86_avx2_pmulh_w,                             // llvm.x86.avx2.pmulh.w
    x86_avx2_pmulhu_w,                            // llvm.x86.avx2.pmulhu.w
    x86_avx2_pmulu_dq,                            // llvm.x86.avx2.pmulu.dq
    x86_avx2_psad_bw,                             // llvm.x86.avx2.psad.bw
    x86_avx2_pshuf_b,                             // llvm.x86.avx2.pshuf.b
    x86_avx2_psign_b,                             // llvm.x86.avx2.psign.b
    x86_avx2_psign_d,                             // llvm.x86.avx2.psign.d
    x86_avx2_psign_w,                             // llvm.x86.avx2.psign.w
    x86_avx2_psll_d,                              // llvm.x86.avx2.psll.d
    x86_avx2_psll_q,                              // llvm.x86.avx2.psll.q
    x86_avx2_psll_w,                              // llvm.x86.avx2.psll.w
    x86_avx2_pslli_d,                             // llvm.x86.avx2.pslli.d
    x86_avx2_pslli_q,                             // llvm.x86.avx2.pslli.q
    x86_avx2_pslli_w,                             // llvm.x86.avx2.pslli.w
    x86_avx2_psllv_d,                             // llvm.x86.avx2.psllv.d
    x86_avx2_psllv_d_256,                         // llvm.x86.avx2.psllv.d.256
    x86_avx2_psllv_q,                             // llvm.x86.avx2.psllv.q
    x86_avx2_psllv_q_256,                         // llvm.x86.avx2.psllv.q.256
    x86_avx2_psra_d,                              // llvm.x86.avx2.psra.d
    x86_avx2_psra_w,                              // llvm.x86.avx2.psra.w
    x86_avx2_psrai_d,                             // llvm.x86.avx2.psrai.d
    x86_avx2_psrai_w,                             // llvm.x86.avx2.psrai.w
    x86_avx2_psrav_d,                             // llvm.x86.avx2.psrav.d
    x86_avx2_psrav_d_256,                         // llvm.x86.avx2.psrav.d.256
    x86_avx2_psrl_d,                              // llvm.x86.avx2.psrl.d
    x86_avx2_psrl_q,                              // llvm.x86.avx2.psrl.q
    x86_avx2_psrl_w,                              // llvm.x86.avx2.psrl.w
    x86_avx2_psrli_d,                             // llvm.x86.avx2.psrli.d
    x86_avx2_psrli_q,                             // llvm.x86.avx2.psrli.q
    x86_avx2_psrli_w,                             // llvm.x86.avx2.psrli.w
    x86_avx2_psrlv_d,                             // llvm.x86.avx2.psrlv.d
    x86_avx2_psrlv_d_256,                         // llvm.x86.avx2.psrlv.d.256
    x86_avx2_psrlv_q,                             // llvm.x86.avx2.psrlv.q
    x86_avx2_psrlv_q_256,                         // llvm.x86.avx2.psrlv.q.256
    x86_avx2_psubs_b,                             // llvm.x86.avx2.psubs.b
    x86_avx2_psubs_w,                             // llvm.x86.avx2.psubs.w
    x86_avx2_psubus_b,                            // llvm.x86.avx2.psubus.b
    x86_avx2_psubus_w,                            // llvm.x86.avx2.psubus.w
    x86_avx512_broadcastmb_128,                   // llvm.x86.avx512.broadcastmb.128
    x86_avx512_broadcastmb_256,                   // llvm.x86.avx512.broadcastmb.256
    x86_avx512_broadcastmb_512,                   // llvm.x86.avx512.broadcastmb.512
    x86_avx512_broadcastmw_128,                   // llvm.x86.avx512.broadcastmw.128
    x86_avx512_broadcastmw_256,                   // llvm.x86.avx512.broadcastmw.256
    x86_avx512_broadcastmw_512,                   // llvm.x86.avx512.broadcastmw.512
    x86_avx512_cvtb2mask_128,                     // llvm.x86.avx512.cvtb2mask.128
    x86_avx512_cvtb2mask_256,                     // llvm.x86.avx512.cvtb2mask.256
    x86_avx512_cvtb2mask_512,                     // llvm.x86.avx512.cvtb2mask.512
    x86_avx512_cvtd2mask_128,                     // llvm.x86.avx512.cvtd2mask.128
    x86_avx512_cvtd2mask_256,                     // llvm.x86.avx512.cvtd2mask.256
    x86_avx512_cvtd2mask_512,                     // llvm.x86.avx512.cvtd2mask.512
    x86_avx512_cvtq2mask_128,                     // llvm.x86.avx512.cvtq2mask.128
    x86_avx512_cvtq2mask_256,                     // llvm.x86.avx512.cvtq2mask.256
    x86_avx512_cvtq2mask_512,                     // llvm.x86.avx512.cvtq2mask.512
    x86_avx512_cvtsi2sd64,                        // llvm.x86.avx512.cvtsi2sd64
    x86_avx512_cvtsi2ss32,                        // llvm.x86.avx512.cvtsi2ss32
    x86_avx512_cvtsi2ss64,                        // llvm.x86.avx512.cvtsi2ss64
    x86_avx512_cvttsd2si,                         // llvm.x86.avx512.cvttsd2si
    x86_avx512_cvttsd2si64,                       // llvm.x86.avx512.cvttsd2si64
    x86_avx512_cvttsd2usi,                        // llvm.x86.avx512.cvttsd2usi
    x86_avx512_cvttsd2usi64,                      // llvm.x86.avx512.cvttsd2usi64
    x86_avx512_cvttss2si,                         // llvm.x86.avx512.cvttss2si
    x86_avx512_cvttss2si64,                       // llvm.x86.avx512.cvttss2si64
    x86_avx512_cvttss2usi,                        // llvm.x86.avx512.cvttss2usi
    x86_avx512_cvttss2usi64,                      // llvm.x86.avx512.cvttss2usi64
    x86_avx512_cvtusi2sd,                         // llvm.x86.avx512.cvtusi2sd
    x86_avx512_cvtusi2ss,                         // llvm.x86.avx512.cvtusi2ss
    x86_avx512_cvtusi642sd,                       // llvm.x86.avx512.cvtusi642sd
    x86_avx512_cvtusi642ss,                       // llvm.x86.avx512.cvtusi642ss
    x86_avx512_cvtw2mask_128,                     // llvm.x86.avx512.cvtw2mask.128
    x86_avx512_cvtw2mask_256,                     // llvm.x86.avx512.cvtw2mask.256
    x86_avx512_cvtw2mask_512,                     // llvm.x86.avx512.cvtw2mask.512
    x86_avx512_exp2_pd,                           // llvm.x86.avx512.exp2.pd
    x86_avx512_exp2_ps,                           // llvm.x86.avx512.exp2.ps
    x86_avx512_gather_dpd_512,                    // llvm.x86.avx512.gather.dpd.512
    x86_avx512_gather_dpi_512,                    // llvm.x86.avx512.gather.dpi.512
    x86_avx512_gather_dpq_512,                    // llvm.x86.avx512.gather.dpq.512
    x86_avx512_gather_dps_512,                    // llvm.x86.avx512.gather.dps.512
    x86_avx512_gather_qpd_512,                    // llvm.x86.avx512.gather.qpd.512
    x86_avx512_gather_qpi_512,                    // llvm.x86.avx512.gather.qpi.512
    x86_avx512_gather_qpq_512,                    // llvm.x86.avx512.gather.qpq.512
    x86_avx512_gather_qps_512,                    // llvm.x86.avx512.gather.qps.512
    x86_avx512_gather3div2_df,                    // llvm.x86.avx512.gather3div2.df
    x86_avx512_gather3div2_di,                    // llvm.x86.avx512.gather3div2.di
    x86_avx512_gather3div4_df,                    // llvm.x86.avx512.gather3div4.df
    x86_avx512_gather3div4_di,                    // llvm.x86.avx512.gather3div4.di
    x86_avx512_gather3div4_sf,                    // llvm.x86.avx512.gather3div4.sf
    x86_avx512_gather3div4_si,                    // llvm.x86.avx512.gather3div4.si
    x86_avx512_gather3div8_sf,                    // llvm.x86.avx512.gather3div8.sf
    x86_avx512_gather3div8_si,                    // llvm.x86.avx512.gather3div8.si
    x86_avx512_gather3siv2_df,                    // llvm.x86.avx512.gather3siv2.df
    x86_avx512_gather3siv2_di,                    // llvm.x86.avx512.gather3siv2.di
    x86_avx512_gather3siv4_df,                    // llvm.x86.avx512.gather3siv4.df
    x86_avx512_gather3siv4_di,                    // llvm.x86.avx512.gather3siv4.di
    x86_avx512_gather3siv4_sf,                    // llvm.x86.avx512.gather3siv4.sf
    x86_avx512_gather3siv4_si,                    // llvm.x86.avx512.gather3siv4.si
    x86_avx512_gather3siv8_sf,                    // llvm.x86.avx512.gather3siv8.sf
    x86_avx512_gather3siv8_si,                    // llvm.x86.avx512.gather3siv8.si
    x86_avx512_gatherpf_dpd_512,                  // llvm.x86.avx512.gatherpf.dpd.512
    x86_avx512_gatherpf_dps_512,                  // llvm.x86.avx512.gatherpf.dps.512
    x86_avx512_gatherpf_qpd_512,                  // llvm.x86.avx512.gatherpf.qpd.512
    x86_avx512_gatherpf_qps_512,                  // llvm.x86.avx512.gatherpf.qps.512
    x86_avx512_kand_w,                            // llvm.x86.avx512.kand.w
    x86_avx512_kandn_w,                           // llvm.x86.avx512.kandn.w
    x86_avx512_knot_w,                            // llvm.x86.avx512.knot.w
    x86_avx512_kor_w,                             // llvm.x86.avx512.kor.w
    x86_avx512_kortestc_w,                        // llvm.x86.avx512.kortestc.w
    x86_avx512_kortestz_w,                        // llvm.x86.avx512.kortestz.w
    x86_avx512_kunpck_bw,                         // llvm.x86.avx512.kunpck.bw
    x86_avx512_kunpck_dq,                         // llvm.x86.avx512.kunpck.dq
    x86_avx512_kunpck_wd,                         // llvm.x86.avx512.kunpck.wd
    x86_avx512_kxnor_w,                           // llvm.x86.avx512.kxnor.w
    x86_avx512_kxor_w,                            // llvm.x86.avx512.kxor.w
    x86_avx512_mask_add_pd_512,                   // llvm.x86.avx512.mask.add.pd.512
    x86_avx512_mask_add_ps_512,                   // llvm.x86.avx512.mask.add.ps.512
    x86_avx512_mask_add_sd_round,                 // llvm.x86.avx512.mask.add.sd.round
    x86_avx512_mask_add_ss_round,                 // llvm.x86.avx512.mask.add.ss.round
    x86_avx512_mask_cmp_pd_128,                   // llvm.x86.avx512.mask.cmp.pd.128
    x86_avx512_mask_cmp_pd_256,                   // llvm.x86.avx512.mask.cmp.pd.256
    x86_avx512_mask_cmp_pd_512,                   // llvm.x86.avx512.mask.cmp.pd.512
    x86_avx512_mask_cmp_ps_128,                   // llvm.x86.avx512.mask.cmp.ps.128
    x86_avx512_mask_cmp_ps_256,                   // llvm.x86.avx512.mask.cmp.ps.256
    x86_avx512_mask_cmp_ps_512,                   // llvm.x86.avx512.mask.cmp.ps.512
    x86_avx512_mask_cmp_sd,                       // llvm.x86.avx512.mask.cmp.sd
    x86_avx512_mask_cmp_ss,                       // llvm.x86.avx512.mask.cmp.ss
    x86_avx512_mask_compress_b_128,               // llvm.x86.avx512.mask.compress.b.128
    x86_avx512_mask_compress_b_256,               // llvm.x86.avx512.mask.compress.b.256
    x86_avx512_mask_compress_b_512,               // llvm.x86.avx512.mask.compress.b.512
    x86_avx512_mask_compress_d_128,               // llvm.x86.avx512.mask.compress.d.128
    x86_avx512_mask_compress_d_256,               // llvm.x86.avx512.mask.compress.d.256
    x86_avx512_mask_compress_d_512,               // llvm.x86.avx512.mask.compress.d.512
    x86_avx512_mask_compress_pd_128,              // llvm.x86.avx512.mask.compress.pd.128
    x86_avx512_mask_compress_pd_256,              // llvm.x86.avx512.mask.compress.pd.256
    x86_avx512_mask_compress_pd_512,              // llvm.x86.avx512.mask.compress.pd.512
    x86_avx512_mask_compress_ps_128,              // llvm.x86.avx512.mask.compress.ps.128
    x86_avx512_mask_compress_ps_256,              // llvm.x86.avx512.mask.compress.ps.256
    x86_avx512_mask_compress_ps_512,              // llvm.x86.avx512.mask.compress.ps.512
    x86_avx512_mask_compress_q_128,               // llvm.x86.avx512.mask.compress.q.128
    x86_avx512_mask_compress_q_256,               // llvm.x86.avx512.mask.compress.q.256
    x86_avx512_mask_compress_q_512,               // llvm.x86.avx512.mask.compress.q.512
    x86_avx512_mask_compress_store_b_128,         // llvm.x86.avx512.mask.compress.store.b.128
    x86_avx512_mask_compress_store_b_256,         // llvm.x86.avx512.mask.compress.store.b.256
    x86_avx512_mask_compress_store_b_512,         // llvm.x86.avx512.mask.compress.store.b.512
    x86_avx512_mask_compress_store_d_128,         // llvm.x86.avx512.mask.compress.store.d.128
    x86_avx512_mask_compress_store_d_256,         // llvm.x86.avx512.mask.compress.store.d.256
    x86_avx512_mask_compress_store_d_512,         // llvm.x86.avx512.mask.compress.store.d.512
    x86_avx512_mask_compress_store_pd_128,        // llvm.x86.avx512.mask.compress.store.pd.128
    x86_avx512_mask_compress_store_pd_256,        // llvm.x86.avx512.mask.compress.store.pd.256
    x86_avx512_mask_compress_store_pd_512,        // llvm.x86.avx512.mask.compress.store.pd.512
    x86_avx512_mask_compress_store_ps_128,        // llvm.x86.avx512.mask.compress.store.ps.128
    x86_avx512_mask_compress_store_ps_256,        // llvm.x86.avx512.mask.compress.store.ps.256
    x86_avx512_mask_compress_store_ps_512,        // llvm.x86.avx512.mask.compress.store.ps.512
    x86_avx512_mask_compress_store_q_128,         // llvm.x86.avx512.mask.compress.store.q.128
    x86_avx512_mask_compress_store_q_256,         // llvm.x86.avx512.mask.compress.store.q.256
    x86_avx512_mask_compress_store_q_512,         // llvm.x86.avx512.mask.compress.store.q.512
    x86_avx512_mask_compress_store_w_128,         // llvm.x86.avx512.mask.compress.store.w.128
    x86_avx512_mask_compress_store_w_256,         // llvm.x86.avx512.mask.compress.store.w.256
    x86_avx512_mask_compress_store_w_512,         // llvm.x86.avx512.mask.compress.store.w.512
    x86_avx512_mask_compress_w_128,               // llvm.x86.avx512.mask.compress.w.128
    x86_avx512_mask_compress_w_256,               // llvm.x86.avx512.mask.compress.w.256
    x86_avx512_mask_compress_w_512,               // llvm.x86.avx512.mask.compress.w.512
    x86_avx512_mask_conflict_d_128,               // llvm.x86.avx512.mask.conflict.d.128
    x86_avx512_mask_conflict_d_256,               // llvm.x86.avx512.mask.conflict.d.256
    x86_avx512_mask_conflict_d_512,               // llvm.x86.avx512.mask.conflict.d.512
    x86_avx512_mask_conflict_q_128,               // llvm.x86.avx512.mask.conflict.q.128
    x86_avx512_mask_conflict_q_256,               // llvm.x86.avx512.mask.conflict.q.256
    x86_avx512_mask_conflict_q_512,               // llvm.x86.avx512.mask.conflict.q.512
    x86_avx512_mask_cvtdq2ps_128,                 // llvm.x86.avx512.mask.cvtdq2ps.128
    x86_avx512_mask_cvtdq2ps_256,                 // llvm.x86.avx512.mask.cvtdq2ps.256
    x86_avx512_mask_cvtdq2ps_512,                 // llvm.x86.avx512.mask.cvtdq2ps.512
    x86_avx512_mask_cvtpd2dq_128,                 // llvm.x86.avx512.mask.cvtpd2dq.128
    x86_avx512_mask_cvtpd2dq_256,                 // llvm.x86.avx512.mask.cvtpd2dq.256
    x86_avx512_mask_cvtpd2dq_512,                 // llvm.x86.avx512.mask.cvtpd2dq.512
    x86_avx512_mask_cvtpd2ps,                     // llvm.x86.avx512.mask.cvtpd2ps
    x86_avx512_mask_cvtpd2ps_256,                 // llvm.x86.avx512.mask.cvtpd2ps.256
    x86_avx512_mask_cvtpd2ps_512,                 // llvm.x86.avx512.mask.cvtpd2ps.512
    x86_avx512_mask_cvtpd2qq_128,                 // llvm.x86.avx512.mask.cvtpd2qq.128
    x86_avx512_mask_cvtpd2qq_256,                 // llvm.x86.avx512.mask.cvtpd2qq.256
    x86_avx512_mask_cvtpd2qq_512,                 // llvm.x86.avx512.mask.cvtpd2qq.512
    x86_avx512_mask_cvtpd2udq_128,                // llvm.x86.avx512.mask.cvtpd2udq.128
    x86_avx512_mask_cvtpd2udq_256,                // llvm.x86.avx512.mask.cvtpd2udq.256
    x86_avx512_mask_cvtpd2udq_512,                // llvm.x86.avx512.mask.cvtpd2udq.512
    x86_avx512_mask_cvtpd2uqq_128,                // llvm.x86.avx512.mask.cvtpd2uqq.128
    x86_avx512_mask_cvtpd2uqq_256,                // llvm.x86.avx512.mask.cvtpd2uqq.256
    x86_avx512_mask_cvtpd2uqq_512,                // llvm.x86.avx512.mask.cvtpd2uqq.512
    x86_avx512_mask_cvtps2dq_128,                 // llvm.x86.avx512.mask.cvtps2dq.128
    x86_avx512_mask_cvtps2dq_256,                 // llvm.x86.avx512.mask.cvtps2dq.256
    x86_avx512_mask_cvtps2dq_512,                 // llvm.x86.avx512.mask.cvtps2dq.512
    x86_avx512_mask_cvtps2pd_128,                 // llvm.x86.avx512.mask.cvtps2pd.128
    x86_avx512_mask_cvtps2pd_256,                 // llvm.x86.avx512.mask.cvtps2pd.256
    x86_avx512_mask_cvtps2pd_512,                 // llvm.x86.avx512.mask.cvtps2pd.512
    x86_avx512_mask_cvtps2qq_128,                 // llvm.x86.avx512.mask.cvtps2qq.128
    x86_avx512_mask_cvtps2qq_256,                 // llvm.x86.avx512.mask.cvtps2qq.256
    x86_avx512_mask_cvtps2qq_512,                 // llvm.x86.avx512.mask.cvtps2qq.512
    x86_avx512_mask_cvtps2udq_128,                // llvm.x86.avx512.mask.cvtps2udq.128
    x86_avx512_mask_cvtps2udq_256,                // llvm.x86.avx512.mask.cvtps2udq.256
    x86_avx512_mask_cvtps2udq_512,                // llvm.x86.avx512.mask.cvtps2udq.512
    x86_avx512_mask_cvtps2uqq_128,                // llvm.x86.avx512.mask.cvtps2uqq.128
    x86_avx512_mask_cvtps2uqq_256,                // llvm.x86.avx512.mask.cvtps2uqq.256
    x86_avx512_mask_cvtps2uqq_512,                // llvm.x86.avx512.mask.cvtps2uqq.512
    x86_avx512_mask_cvtqq2pd_128,                 // llvm.x86.avx512.mask.cvtqq2pd.128
    x86_avx512_mask_cvtqq2pd_256,                 // llvm.x86.avx512.mask.cvtqq2pd.256
    x86_avx512_mask_cvtqq2pd_512,                 // llvm.x86.avx512.mask.cvtqq2pd.512
    x86_avx512_mask_cvtqq2ps_128,                 // llvm.x86.avx512.mask.cvtqq2ps.128
    x86_avx512_mask_cvtqq2ps_256,                 // llvm.x86.avx512.mask.cvtqq2ps.256
    x86_avx512_mask_cvtqq2ps_512,                 // llvm.x86.avx512.mask.cvtqq2ps.512
    x86_avx512_mask_cvtsd2ss_round,               // llvm.x86.avx512.mask.cvtsd2ss.round
    x86_avx512_mask_cvtss2sd_round,               // llvm.x86.avx512.mask.cvtss2sd.round
    x86_avx512_mask_cvttpd2dq_128,                // llvm.x86.avx512.mask.cvttpd2dq.128
    x86_avx512_mask_cvttpd2dq_256,                // llvm.x86.avx512.mask.cvttpd2dq.256
    x86_avx512_mask_cvttpd2dq_512,                // llvm.x86.avx512.mask.cvttpd2dq.512
    x86_avx512_mask_cvttpd2qq_128,                // llvm.x86.avx512.mask.cvttpd2qq.128
    x86_avx512_mask_cvttpd2qq_256,                // llvm.x86.avx512.mask.cvttpd2qq.256
    x86_avx512_mask_cvttpd2qq_512,                // llvm.x86.avx512.mask.cvttpd2qq.512
    x86_avx512_mask_cvttpd2udq_128,               // llvm.x86.avx512.mask.cvttpd2udq.128
    x86_avx512_mask_cvttpd2udq_256,               // llvm.x86.avx512.mask.cvttpd2udq.256
    x86_avx512_mask_cvttpd2udq_512,               // llvm.x86.avx512.mask.cvttpd2udq.512
    x86_avx512_mask_cvttpd2uqq_128,               // llvm.x86.avx512.mask.cvttpd2uqq.128
    x86_avx512_mask_cvttpd2uqq_256,               // llvm.x86.avx512.mask.cvttpd2uqq.256
    x86_avx512_mask_cvttpd2uqq_512,               // llvm.x86.avx512.mask.cvttpd2uqq.512
    x86_avx512_mask_cvttps2dq_128,                // llvm.x86.avx512.mask.cvttps2dq.128
    x86_avx512_mask_cvttps2dq_256,                // llvm.x86.avx512.mask.cvttps2dq.256
    x86_avx512_mask_cvttps2dq_512,                // llvm.x86.avx512.mask.cvttps2dq.512
    x86_avx512_mask_cvttps2qq_128,                // llvm.x86.avx512.mask.cvttps2qq.128
    x86_avx512_mask_cvttps2qq_256,                // llvm.x86.avx512.mask.cvttps2qq.256
    x86_avx512_mask_cvttps2qq_512,                // llvm.x86.avx512.mask.cvttps2qq.512
    x86_avx512_mask_cvttps2udq_128,               // llvm.x86.avx512.mask.cvttps2udq.128
    x86_avx512_mask_cvttps2udq_256,               // llvm.x86.avx512.mask.cvttps2udq.256
    x86_avx512_mask_cvttps2udq_512,               // llvm.x86.avx512.mask.cvttps2udq.512
    x86_avx512_mask_cvttps2uqq_128,               // llvm.x86.avx512.mask.cvttps2uqq.128
    x86_avx512_mask_cvttps2uqq_256,               // llvm.x86.avx512.mask.cvttps2uqq.256
    x86_avx512_mask_cvttps2uqq_512,               // llvm.x86.avx512.mask.cvttps2uqq.512
    x86_avx512_mask_cvtudq2ps_128,                // llvm.x86.avx512.mask.cvtudq2ps.128
    x86_avx512_mask_cvtudq2ps_256,                // llvm.x86.avx512.mask.cvtudq2ps.256
    x86_avx512_mask_cvtudq2ps_512,                // llvm.x86.avx512.mask.cvtudq2ps.512
    x86_avx512_mask_cvtuqq2pd_128,                // llvm.x86.avx512.mask.cvtuqq2pd.128
    x86_avx512_mask_cvtuqq2pd_256,                // llvm.x86.avx512.mask.cvtuqq2pd.256
    x86_avx512_mask_cvtuqq2pd_512,                // llvm.x86.avx512.mask.cvtuqq2pd.512
    x86_avx512_mask_cvtuqq2ps_128,                // llvm.x86.avx512.mask.cvtuqq2ps.128
    x86_avx512_mask_cvtuqq2ps_256,                // llvm.x86.avx512.mask.cvtuqq2ps.256
    x86_avx512_mask_cvtuqq2ps_512,                // llvm.x86.avx512.mask.cvtuqq2ps.512
    x86_avx512_mask_dbpsadbw_128,                 // llvm.x86.avx512.mask.dbpsadbw.128
    x86_avx512_mask_dbpsadbw_256,                 // llvm.x86.avx512.mask.dbpsadbw.256
    x86_avx512_mask_dbpsadbw_512,                 // llvm.x86.avx512.mask.dbpsadbw.512
    x86_avx512_mask_div_pd_512,                   // llvm.x86.avx512.mask.div.pd.512
    x86_avx512_mask_div_ps_512,                   // llvm.x86.avx512.mask.div.ps.512
    x86_avx512_mask_div_sd_round,                 // llvm.x86.avx512.mask.div.sd.round
    x86_avx512_mask_div_ss_round,                 // llvm.x86.avx512.mask.div.ss.round
    x86_avx512_mask_expand_b_128,                 // llvm.x86.avx512.mask.expand.b.128
    x86_avx512_mask_expand_b_256,                 // llvm.x86.avx512.mask.expand.b.256
    x86_avx512_mask_expand_b_512,                 // llvm.x86.avx512.mask.expand.b.512
    x86_avx512_mask_expand_d_128,                 // llvm.x86.avx512.mask.expand.d.128
    x86_avx512_mask_expand_d_256,                 // llvm.x86.avx512.mask.expand.d.256
    x86_avx512_mask_expand_d_512,                 // llvm.x86.avx512.mask.expand.d.512
    x86_avx512_mask_expand_load_b_128,            // llvm.x86.avx512.mask.expand.load.b.128
    x86_avx512_mask_expand_load_b_256,            // llvm.x86.avx512.mask.expand.load.b.256
    x86_avx512_mask_expand_load_b_512,            // llvm.x86.avx512.mask.expand.load.b.512
    x86_avx512_mask_expand_load_d_128,            // llvm.x86.avx512.mask.expand.load.d.128
    x86_avx512_mask_expand_load_d_256,            // llvm.x86.avx512.mask.expand.load.d.256
    x86_avx512_mask_expand_load_d_512,            // llvm.x86.avx512.mask.expand.load.d.512
    x86_avx512_mask_expand_load_pd_128,           // llvm.x86.avx512.mask.expand.load.pd.128
    x86_avx512_mask_expand_load_pd_256,           // llvm.x86.avx512.mask.expand.load.pd.256
    x86_avx512_mask_expand_load_pd_512,           // llvm.x86.avx512.mask.expand.load.pd.512
    x86_avx512_mask_expand_load_ps_128,           // llvm.x86.avx512.mask.expand.load.ps.128
    x86_avx512_mask_expand_load_ps_256,           // llvm.x86.avx512.mask.expand.load.ps.256
    x86_avx512_mask_expand_load_ps_512,           // llvm.x86.avx512.mask.expand.load.ps.512
    x86_avx512_mask_expand_load_q_128,            // llvm.x86.avx512.mask.expand.load.q.128
    x86_avx512_mask_expand_load_q_256,            // llvm.x86.avx512.mask.expand.load.q.256
    x86_avx512_mask_expand_load_q_512,            // llvm.x86.avx512.mask.expand.load.q.512
    x86_avx512_mask_expand_load_w_128,            // llvm.x86.avx512.mask.expand.load.w.128
    x86_avx512_mask_expand_load_w_256,            // llvm.x86.avx512.mask.expand.load.w.256
    x86_avx512_mask_expand_load_w_512,            // llvm.x86.avx512.mask.expand.load.w.512
    x86_avx512_mask_expand_pd_128,                // llvm.x86.avx512.mask.expand.pd.128
    x86_avx512_mask_expand_pd_256,                // llvm.x86.avx512.mask.expand.pd.256
    x86_avx512_mask_expand_pd_512,                // llvm.x86.avx512.mask.expand.pd.512
    x86_avx512_mask_expand_ps_128,                // llvm.x86.avx512.mask.expand.ps.128
    x86_avx512_mask_expand_ps_256,                // llvm.x86.avx512.mask.expand.ps.256
    x86_avx512_mask_expand_ps_512,                // llvm.x86.avx512.mask.expand.ps.512
    x86_avx512_mask_expand_q_128,                 // llvm.x86.avx512.mask.expand.q.128
    x86_avx512_mask_expand_q_256,                 // llvm.x86.avx512.mask.expand.q.256
    x86_avx512_mask_expand_q_512,                 // llvm.x86.avx512.mask.expand.q.512
    x86_avx512_mask_expand_w_128,                 // llvm.x86.avx512.mask.expand.w.128
    x86_avx512_mask_expand_w_256,                 // llvm.x86.avx512.mask.expand.w.256
    x86_avx512_mask_expand_w_512,                 // llvm.x86.avx512.mask.expand.w.512
    x86_avx512_mask_fixupimm_pd_128,              // llvm.x86.avx512.mask.fixupimm.pd.128
    x86_avx512_mask_fixupimm_pd_256,              // llvm.x86.avx512.mask.fixupimm.pd.256
    x86_avx512_mask_fixupimm_pd_512,              // llvm.x86.avx512.mask.fixupimm.pd.512
    x86_avx512_mask_fixupimm_ps_128,              // llvm.x86.avx512.mask.fixupimm.ps.128
    x86_avx512_mask_fixupimm_ps_256,              // llvm.x86.avx512.mask.fixupimm.ps.256
    x86_avx512_mask_fixupimm_ps_512,              // llvm.x86.avx512.mask.fixupimm.ps.512
    x86_avx512_mask_fixupimm_sd,                  // llvm.x86.avx512.mask.fixupimm.sd
    x86_avx512_mask_fixupimm_ss,                  // llvm.x86.avx512.mask.fixupimm.ss
    x86_avx512_mask_fpclass_pd_128,               // llvm.x86.avx512.mask.fpclass.pd.128
    x86_avx512_mask_fpclass_pd_256,               // llvm.x86.avx512.mask.fpclass.pd.256
    x86_avx512_mask_fpclass_pd_512,               // llvm.x86.avx512.mask.fpclass.pd.512
    x86_avx512_mask_fpclass_ps_128,               // llvm.x86.avx512.mask.fpclass.ps.128
    x86_avx512_mask_fpclass_ps_256,               // llvm.x86.avx512.mask.fpclass.ps.256
    x86_avx512_mask_fpclass_ps_512,               // llvm.x86.avx512.mask.fpclass.ps.512
    x86_avx512_mask_fpclass_sd,                   // llvm.x86.avx512.mask.fpclass.sd
    x86_avx512_mask_fpclass_ss,                   // llvm.x86.avx512.mask.fpclass.ss
    x86_avx512_mask_getexp_pd_128,                // llvm.x86.avx512.mask.getexp.pd.128
    x86_avx512_mask_getexp_pd_256,                // llvm.x86.avx512.mask.getexp.pd.256
    x86_avx512_mask_getexp_pd_512,                // llvm.x86.avx512.mask.getexp.pd.512
    x86_avx512_mask_getexp_ps_128,                // llvm.x86.avx512.mask.getexp.ps.128
    x86_avx512_mask_getexp_ps_256,                // llvm.x86.avx512.mask.getexp.ps.256
    x86_avx512_mask_getexp_ps_512,                // llvm.x86.avx512.mask.getexp.ps.512
    x86_avx512_mask_getexp_sd,                    // llvm.x86.avx512.mask.getexp.sd
    x86_avx512_mask_getexp_ss,                    // llvm.x86.avx512.mask.getexp.ss
    x86_avx512_mask_getmant_pd_128,               // llvm.x86.avx512.mask.getmant.pd.128
    x86_avx512_mask_getmant_pd_256,               // llvm.x86.avx512.mask.getmant.pd.256
    x86_avx512_mask_getmant_pd_512,               // llvm.x86.avx512.mask.getmant.pd.512
    x86_avx512_mask_getmant_ps_128,               // llvm.x86.avx512.mask.getmant.ps.128
    x86_avx512_mask_getmant_ps_256,               // llvm.x86.avx512.mask.getmant.ps.256
    x86_avx512_mask_getmant_ps_512,               // llvm.x86.avx512.mask.getmant.ps.512
    x86_avx512_mask_getmant_sd,                   // llvm.x86.avx512.mask.getmant.sd
    x86_avx512_mask_getmant_ss,                   // llvm.x86.avx512.mask.getmant.ss
    x86_avx512_mask_max_pd_512,                   // llvm.x86.avx512.mask.max.pd.512
    x86_avx512_mask_max_ps_512,                   // llvm.x86.avx512.mask.max.ps.512
    x86_avx512_mask_max_sd_round,                 // llvm.x86.avx512.mask.max.sd.round
    x86_avx512_mask_max_ss_round,                 // llvm.x86.avx512.mask.max.ss.round
    x86_avx512_mask_min_pd_512,                   // llvm.x86.avx512.mask.min.pd.512
    x86_avx512_mask_min_ps_512,                   // llvm.x86.avx512.mask.min.ps.512
    x86_avx512_mask_min_sd_round,                 // llvm.x86.avx512.mask.min.sd.round
    x86_avx512_mask_min_ss_round,                 // llvm.x86.avx512.mask.min.ss.round
    x86_avx512_mask_mul_pd_512,                   // llvm.x86.avx512.mask.mul.pd.512
    x86_avx512_mask_mul_ps_512,                   // llvm.x86.avx512.mask.mul.ps.512
    x86_avx512_mask_mul_sd_round,                 // llvm.x86.avx512.mask.mul.sd.round
    x86_avx512_mask_mul_ss_round,                 // llvm.x86.avx512.mask.mul.ss.round
    x86_avx512_mask_padds_b_128,                  // llvm.x86.avx512.mask.padds.b.128
    x86_avx512_mask_padds_b_256,                  // llvm.x86.avx512.mask.padds.b.256
    x86_avx512_mask_padds_b_512,                  // llvm.x86.avx512.mask.padds.b.512
    x86_avx512_mask_padds_w_128,                  // llvm.x86.avx512.mask.padds.w.128
    x86_avx512_mask_padds_w_256,                  // llvm.x86.avx512.mask.padds.w.256
    x86_avx512_mask_padds_w_512,                  // llvm.x86.avx512.mask.padds.w.512
    x86_avx512_mask_paddus_b_128,                 // llvm.x86.avx512.mask.paddus.b.128
    x86_avx512_mask_paddus_b_256,                 // llvm.x86.avx512.mask.paddus.b.256
    x86_avx512_mask_paddus_b_512,                 // llvm.x86.avx512.mask.paddus.b.512
    x86_avx512_mask_paddus_w_128,                 // llvm.x86.avx512.mask.paddus.w.128
    x86_avx512_mask_paddus_w_256,                 // llvm.x86.avx512.mask.paddus.w.256
    x86_avx512_mask_paddus_w_512,                 // llvm.x86.avx512.mask.paddus.w.512
    x86_avx512_mask_permvar_df_256,               // llvm.x86.avx512.mask.permvar.df.256
    x86_avx512_mask_permvar_df_512,               // llvm.x86.avx512.mask.permvar.df.512
    x86_avx512_mask_permvar_di_256,               // llvm.x86.avx512.mask.permvar.di.256
    x86_avx512_mask_permvar_di_512,               // llvm.x86.avx512.mask.permvar.di.512
    x86_avx512_mask_permvar_hi_128,               // llvm.x86.avx512.mask.permvar.hi.128
    x86_avx512_mask_permvar_hi_256,               // llvm.x86.avx512.mask.permvar.hi.256
    x86_avx512_mask_permvar_hi_512,               // llvm.x86.avx512.mask.permvar.hi.512
    x86_avx512_mask_permvar_qi_128,               // llvm.x86.avx512.mask.permvar.qi.128
    x86_avx512_mask_permvar_qi_256,               // llvm.x86.avx512.mask.permvar.qi.256
    x86_avx512_mask_permvar_qi_512,               // llvm.x86.avx512.mask.permvar.qi.512
    x86_avx512_mask_permvar_sf_256,               // llvm.x86.avx512.mask.permvar.sf.256
    x86_avx512_mask_permvar_sf_512,               // llvm.x86.avx512.mask.permvar.sf.512
    x86_avx512_mask_permvar_si_256,               // llvm.x86.avx512.mask.permvar.si.256
    x86_avx512_mask_permvar_si_512,               // llvm.x86.avx512.mask.permvar.si.512
    x86_avx512_mask_pmaddubs_w_128,               // llvm.x86.avx512.mask.pmaddubs.w.128
    x86_avx512_mask_pmaddubs_w_256,               // llvm.x86.avx512.mask.pmaddubs.w.256
    x86_avx512_mask_pmaddubs_w_512,               // llvm.x86.avx512.mask.pmaddubs.w.512
    x86_avx512_mask_pmaddw_d_128,                 // llvm.x86.avx512.mask.pmaddw.d.128
    x86_avx512_mask_pmaddw_d_256,                 // llvm.x86.avx512.mask.pmaddw.d.256
    x86_avx512_mask_pmaddw_d_512,                 // llvm.x86.avx512.mask.pmaddw.d.512
    x86_avx512_mask_pmov_db_128,                  // llvm.x86.avx512.mask.pmov.db.128
    x86_avx512_mask_pmov_db_256,                  // llvm.x86.avx512.mask.pmov.db.256
    x86_avx512_mask_pmov_db_512,                  // llvm.x86.avx512.mask.pmov.db.512
    x86_avx512_mask_pmov_db_mem_128,              // llvm.x86.avx512.mask.pmov.db.mem.128
    x86_avx512_mask_pmov_db_mem_256,              // llvm.x86.avx512.mask.pmov.db.mem.256
    x86_avx512_mask_pmov_db_mem_512,              // llvm.x86.avx512.mask.pmov.db.mem.512
    x86_avx512_mask_pmov_dw_128,                  // llvm.x86.avx512.mask.pmov.dw.128
    x86_avx512_mask_pmov_dw_256,                  // llvm.x86.avx512.mask.pmov.dw.256
    x86_avx512_mask_pmov_dw_512,                  // llvm.x86.avx512.mask.pmov.dw.512
    x86_avx512_mask_pmov_dw_mem_128,              // llvm.x86.avx512.mask.pmov.dw.mem.128
    x86_avx512_mask_pmov_dw_mem_256,              // llvm.x86.avx512.mask.pmov.dw.mem.256
    x86_avx512_mask_pmov_dw_mem_512,              // llvm.x86.avx512.mask.pmov.dw.mem.512
    x86_avx512_mask_pmov_qb_128,                  // llvm.x86.avx512.mask.pmov.qb.128
    x86_avx512_mask_pmov_qb_256,                  // llvm.x86.avx512.mask.pmov.qb.256
    x86_avx512_mask_pmov_qb_512,                  // llvm.x86.avx512.mask.pmov.qb.512
    x86_avx512_mask_pmov_qb_mem_128,              // llvm.x86.avx512.mask.pmov.qb.mem.128
    x86_avx512_mask_pmov_qb_mem_256,              // llvm.x86.avx512.mask.pmov.qb.mem.256
    x86_avx512_mask_pmov_qb_mem_512,              // llvm.x86.avx512.mask.pmov.qb.mem.512
    x86_avx512_mask_pmov_qd_128,                  // llvm.x86.avx512.mask.pmov.qd.128
    x86_avx512_mask_pmov_qd_256,                  // llvm.x86.avx512.mask.pmov.qd.256
    x86_avx512_mask_pmov_qd_512,                  // llvm.x86.avx512.mask.pmov.qd.512
    x86_avx512_mask_pmov_qd_mem_128,              // llvm.x86.avx512.mask.pmov.qd.mem.128
    x86_avx512_mask_pmov_qd_mem_256,              // llvm.x86.avx512.mask.pmov.qd.mem.256
    x86_avx512_mask_pmov_qd_mem_512,              // llvm.x86.avx512.mask.pmov.qd.mem.512
    x86_avx512_mask_pmov_qw_128,                  // llvm.x86.avx512.mask.pmov.qw.128
    x86_avx512_mask_pmov_qw_256,                  // llvm.x86.avx512.mask.pmov.qw.256
    x86_avx512_mask_pmov_qw_512,                  // llvm.x86.avx512.mask.pmov.qw.512
    x86_avx512_mask_pmov_qw_mem_128,              // llvm.x86.avx512.mask.pmov.qw.mem.128
    x86_avx512_mask_pmov_qw_mem_256,              // llvm.x86.avx512.mask.pmov.qw.mem.256
    x86_avx512_mask_pmov_qw_mem_512,              // llvm.x86.avx512.mask.pmov.qw.mem.512
    x86_avx512_mask_pmov_wb_128,                  // llvm.x86.avx512.mask.pmov.wb.128
    x86_avx512_mask_pmov_wb_256,                  // llvm.x86.avx512.mask.pmov.wb.256
    x86_avx512_mask_pmov_wb_512,                  // llvm.x86.avx512.mask.pmov.wb.512
    x86_avx512_mask_pmov_wb_mem_128,              // llvm.x86.avx512.mask.pmov.wb.mem.128
    x86_avx512_mask_pmov_wb_mem_256,              // llvm.x86.avx512.mask.pmov.wb.mem.256
    x86_avx512_mask_pmov_wb_mem_512,              // llvm.x86.avx512.mask.pmov.wb.mem.512
    x86_avx512_mask_pmovs_db_128,                 // llvm.x86.avx512.mask.pmovs.db.128
    x86_avx512_mask_pmovs_db_256,                 // llvm.x86.avx512.mask.pmovs.db.256
    x86_avx512_mask_pmovs_db_512,                 // llvm.x86.avx512.mask.pmovs.db.512
    x86_avx512_mask_pmovs_db_mem_128,             // llvm.x86.avx512.mask.pmovs.db.mem.128
    x86_avx512_mask_pmovs_db_mem_256,             // llvm.x86.avx512.mask.pmovs.db.mem.256
    x86_avx512_mask_pmovs_db_mem_512,             // llvm.x86.avx512.mask.pmovs.db.mem.512
    x86_avx512_mask_pmovs_dw_128,                 // llvm.x86.avx512.mask.pmovs.dw.128
    x86_avx512_mask_pmovs_dw_256,                 // llvm.x86.avx512.mask.pmovs.dw.256
    x86_avx512_mask_pmovs_dw_512,                 // llvm.x86.avx512.mask.pmovs.dw.512
    x86_avx512_mask_pmovs_dw_mem_128,             // llvm.x86.avx512.mask.pmovs.dw.mem.128
    x86_avx512_mask_pmovs_dw_mem_256,             // llvm.x86.avx512.mask.pmovs.dw.mem.256
    x86_avx512_mask_pmovs_dw_mem_512,             // llvm.x86.avx512.mask.pmovs.dw.mem.512
    x86_avx512_mask_pmovs_qb_128,                 // llvm.x86.avx512.mask.pmovs.qb.128
    x86_avx512_mask_pmovs_qb_256,                 // llvm.x86.avx512.mask.pmovs.qb.256
    x86_avx512_mask_pmovs_qb_512,                 // llvm.x86.avx512.mask.pmovs.qb.512
    x86_avx512_mask_pmovs_qb_mem_128,             // llvm.x86.avx512.mask.pmovs.qb.mem.128
    x86_avx512_mask_pmovs_qb_mem_256,             // llvm.x86.avx512.mask.pmovs.qb.mem.256
    x86_avx512_mask_pmovs_qb_mem_512,             // llvm.x86.avx512.mask.pmovs.qb.mem.512
    x86_avx512_mask_pmovs_qd_128,                 // llvm.x86.avx512.mask.pmovs.qd.128
    x86_avx512_mask_pmovs_qd_256,                 // llvm.x86.avx512.mask.pmovs.qd.256
    x86_avx512_mask_pmovs_qd_512,                 // llvm.x86.avx512.mask.pmovs.qd.512
    x86_avx512_mask_pmovs_qd_mem_128,             // llvm.x86.avx512.mask.pmovs.qd.mem.128
    x86_avx512_mask_pmovs_qd_mem_256,             // llvm.x86.avx512.mask.pmovs.qd.mem.256
    x86_avx512_mask_pmovs_qd_mem_512,             // llvm.x86.avx512.mask.pmovs.qd.mem.512
    x86_avx512_mask_pmovs_qw_128,                 // llvm.x86.avx512.mask.pmovs.qw.128
    x86_avx512_mask_pmovs_qw_256,                 // llvm.x86.avx512.mask.pmovs.qw.256
    x86_avx512_mask_pmovs_qw_512,                 // llvm.x86.avx512.mask.pmovs.qw.512
    x86_avx512_mask_pmovs_qw_mem_128,             // llvm.x86.avx512.mask.pmovs.qw.mem.128
    x86_avx512_mask_pmovs_qw_mem_256,             // llvm.x86.avx512.mask.pmovs.qw.mem.256
    x86_avx512_mask_pmovs_qw_mem_512,             // llvm.x86.avx512.mask.pmovs.qw.mem.512
    x86_avx512_mask_pmovs_wb_128,                 // llvm.x86.avx512.mask.pmovs.wb.128
    x86_avx512_mask_pmovs_wb_256,                 // llvm.x86.avx512.mask.pmovs.wb.256
    x86_avx512_mask_pmovs_wb_512,                 // llvm.x86.avx512.mask.pmovs.wb.512
    x86_avx512_mask_pmovs_wb_mem_128,             // llvm.x86.avx512.mask.pmovs.wb.mem.128
    x86_avx512_mask_pmovs_wb_mem_256,             // llvm.x86.avx512.mask.pmovs.wb.mem.256
    x86_avx512_mask_pmovs_wb_mem_512,             // llvm.x86.avx512.mask.pmovs.wb.mem.512
    x86_avx512_mask_pmovus_db_128,                // llvm.x86.avx512.mask.pmovus.db.128
    x86_avx512_mask_pmovus_db_256,                // llvm.x86.avx512.mask.pmovus.db.256
    x86_avx512_mask_pmovus_db_512,                // llvm.x86.avx512.mask.pmovus.db.512
    x86_avx512_mask_pmovus_db_mem_128,            // llvm.x86.avx512.mask.pmovus.db.mem.128
    x86_avx512_mask_pmovus_db_mem_256,            // llvm.x86.avx512.mask.pmovus.db.mem.256
    x86_avx512_mask_pmovus_db_mem_512,            // llvm.x86.avx512.mask.pmovus.db.mem.512
    x86_avx512_mask_pmovus_dw_128,                // llvm.x86.avx512.mask.pmovus.dw.128
    x86_avx512_mask_pmovus_dw_256,                // llvm.x86.avx512.mask.pmovus.dw.256
    x86_avx512_mask_pmovus_dw_512,                // llvm.x86.avx512.mask.pmovus.dw.512
    x86_avx512_mask_pmovus_dw_mem_128,            // llvm.x86.avx512.mask.pmovus.dw.mem.128
    x86_avx512_mask_pmovus_dw_mem_256,            // llvm.x86.avx512.mask.pmovus.dw.mem.256
    x86_avx512_mask_pmovus_dw_mem_512,            // llvm.x86.avx512.mask.pmovus.dw.mem.512
    x86_avx512_mask_pmovus_qb_128,                // llvm.x86.avx512.mask.pmovus.qb.128
    x86_avx512_mask_pmovus_qb_256,                // llvm.x86.avx512.mask.pmovus.qb.256
    x86_avx512_mask_pmovus_qb_512,                // llvm.x86.avx512.mask.pmovus.qb.512
    x86_avx512_mask_pmovus_qb_mem_128,            // llvm.x86.avx512.mask.pmovus.qb.mem.128
    x86_avx512_mask_pmovus_qb_mem_256,            // llvm.x86.avx512.mask.pmovus.qb.mem.256
    x86_avx512_mask_pmovus_qb_mem_512,            // llvm.x86.avx512.mask.pmovus.qb.mem.512
    x86_avx512_mask_pmovus_qd_128,                // llvm.x86.avx512.mask.pmovus.qd.128
    x86_avx512_mask_pmovus_qd_256,                // llvm.x86.avx512.mask.pmovus.qd.256
    x86_avx512_mask_pmovus_qd_512,                // llvm.x86.avx512.mask.pmovus.qd.512
    x86_avx512_mask_pmovus_qd_mem_128,            // llvm.x86.avx512.mask.pmovus.qd.mem.128
    x86_avx512_mask_pmovus_qd_mem_256,            // llvm.x86.avx512.mask.pmovus.qd.mem.256
    x86_avx512_mask_pmovus_qd_mem_512,            // llvm.x86.avx512.mask.pmovus.qd.mem.512
    x86_avx512_mask_pmovus_qw_128,                // llvm.x86.avx512.mask.pmovus.qw.128
    x86_avx512_mask_pmovus_qw_256,                // llvm.x86.avx512.mask.pmovus.qw.256
    x86_avx512_mask_pmovus_qw_512,                // llvm.x86.avx512.mask.pmovus.qw.512
    x86_avx512_mask_pmovus_qw_mem_128,            // llvm.x86.avx512.mask.pmovus.qw.mem.128
    x86_avx512_mask_pmovus_qw_mem_256,            // llvm.x86.avx512.mask.pmovus.qw.mem.256
    x86_avx512_mask_pmovus_qw_mem_512,            // llvm.x86.avx512.mask.pmovus.qw.mem.512
    x86_avx512_mask_pmovus_wb_128,                // llvm.x86.avx512.mask.pmovus.wb.128
    x86_avx512_mask_pmovus_wb_256,                // llvm.x86.avx512.mask.pmovus.wb.256
    x86_avx512_mask_pmovus_wb_512,                // llvm.x86.avx512.mask.pmovus.wb.512
    x86_avx512_mask_pmovus_wb_mem_128,            // llvm.x86.avx512.mask.pmovus.wb.mem.128
    x86_avx512_mask_pmovus_wb_mem_256,            // llvm.x86.avx512.mask.pmovus.wb.mem.256
    x86_avx512_mask_pmovus_wb_mem_512,            // llvm.x86.avx512.mask.pmovus.wb.mem.512
    x86_avx512_mask_pmul_hr_sw_128,               // llvm.x86.avx512.mask.pmul.hr.sw.128
    x86_avx512_mask_pmul_hr_sw_256,               // llvm.x86.avx512.mask.pmul.hr.sw.256
    x86_avx512_mask_pmul_hr_sw_512,               // llvm.x86.avx512.mask.pmul.hr.sw.512
    x86_avx512_mask_pmulh_w_128,                  // llvm.x86.avx512.mask.pmulh.w.128
    x86_avx512_mask_pmulh_w_256,                  // llvm.x86.avx512.mask.pmulh.w.256
    x86_avx512_mask_pmulh_w_512,                  // llvm.x86.avx512.mask.pmulh.w.512
    x86_avx512_mask_pmulhu_w_128,                 // llvm.x86.avx512.mask.pmulhu.w.128
    x86_avx512_mask_pmulhu_w_256,                 // llvm.x86.avx512.mask.pmulhu.w.256
    x86_avx512_mask_pmulhu_w_512,                 // llvm.x86.avx512.mask.pmulhu.w.512
    x86_avx512_mask_pmultishift_qb_128,           // llvm.x86.avx512.mask.pmultishift.qb.128
    x86_avx512_mask_pmultishift_qb_256,           // llvm.x86.avx512.mask.pmultishift.qb.256
    x86_avx512_mask_pmultishift_qb_512,           // llvm.x86.avx512.mask.pmultishift.qb.512
    x86_avx512_mask_prol_d_128,                   // llvm.x86.avx512.mask.prol.d.128
    x86_avx512_mask_prol_d_256,                   // llvm.x86.avx512.mask.prol.d.256
    x86_avx512_mask_prol_d_512,                   // llvm.x86.avx512.mask.prol.d.512
    x86_avx512_mask_prol_q_128,                   // llvm.x86.avx512.mask.prol.q.128
    x86_avx512_mask_prol_q_256,                   // llvm.x86.avx512.mask.prol.q.256
    x86_avx512_mask_prol_q_512,                   // llvm.x86.avx512.mask.prol.q.512
    x86_avx512_mask_prolv_d_128,                  // llvm.x86.avx512.mask.prolv.d.128
    x86_avx512_mask_prolv_d_256,                  // llvm.x86.avx512.mask.prolv.d.256
    x86_avx512_mask_prolv_d_512,                  // llvm.x86.avx512.mask.prolv.d.512
    x86_avx512_mask_prolv_q_128,                  // llvm.x86.avx512.mask.prolv.q.128
    x86_avx512_mask_prolv_q_256,                  // llvm.x86.avx512.mask.prolv.q.256
    x86_avx512_mask_prolv_q_512,                  // llvm.x86.avx512.mask.prolv.q.512
    x86_avx512_mask_pror_d_128,                   // llvm.x86.avx512.mask.pror.d.128
    x86_avx512_mask_pror_d_256,                   // llvm.x86.avx512.mask.pror.d.256
    x86_avx512_mask_pror_d_512,                   // llvm.x86.avx512.mask.pror.d.512
    x86_avx512_mask_pror_q_128,                   // llvm.x86.avx512.mask.pror.q.128
    x86_avx512_mask_pror_q_256,                   // llvm.x86.avx512.mask.pror.q.256
    x86_avx512_mask_pror_q_512,                   // llvm.x86.avx512.mask.pror.q.512
    x86_avx512_mask_prorv_d_128,                  // llvm.x86.avx512.mask.prorv.d.128
    x86_avx512_mask_prorv_d_256,                  // llvm.x86.avx512.mask.prorv.d.256
    x86_avx512_mask_prorv_d_512,                  // llvm.x86.avx512.mask.prorv.d.512
    x86_avx512_mask_prorv_q_128,                  // llvm.x86.avx512.mask.prorv.q.128
    x86_avx512_mask_prorv_q_256,                  // llvm.x86.avx512.mask.prorv.q.256
    x86_avx512_mask_prorv_q_512,                  // llvm.x86.avx512.mask.prorv.q.512
    x86_avx512_mask_psubs_b_128,                  // llvm.x86.avx512.mask.psubs.b.128
    x86_avx512_mask_psubs_b_256,                  // llvm.x86.avx512.mask.psubs.b.256
    x86_avx512_mask_psubs_b_512,                  // llvm.x86.avx512.mask.psubs.b.512
    x86_avx512_mask_psubs_w_128,                  // llvm.x86.avx512.mask.psubs.w.128
    x86_avx512_mask_psubs_w_256,                  // llvm.x86.avx512.mask.psubs.w.256
    x86_avx512_mask_psubs_w_512,                  // llvm.x86.avx512.mask.psubs.w.512
    x86_avx512_mask_psubus_b_128,                 // llvm.x86.avx512.mask.psubus.b.128
    x86_avx512_mask_psubus_b_256,                 // llvm.x86.avx512.mask.psubus.b.256
    x86_avx512_mask_psubus_b_512,                 // llvm.x86.avx512.mask.psubus.b.512
    x86_avx512_mask_psubus_w_128,                 // llvm.x86.avx512.mask.psubus.w.128
    x86_avx512_mask_psubus_w_256,                 // llvm.x86.avx512.mask.psubus.w.256
    x86_avx512_mask_psubus_w_512,                 // llvm.x86.avx512.mask.psubus.w.512
    x86_avx512_mask_pternlog_d_128,               // llvm.x86.avx512.mask.pternlog.d.128
    x86_avx512_mask_pternlog_d_256,               // llvm.x86.avx512.mask.pternlog.d.256
    x86_avx512_mask_pternlog_d_512,               // llvm.x86.avx512.mask.pternlog.d.512
    x86_avx512_mask_pternlog_q_128,               // llvm.x86.avx512.mask.pternlog.q.128
    x86_avx512_mask_pternlog_q_256,               // llvm.x86.avx512.mask.pternlog.q.256
    x86_avx512_mask_pternlog_q_512,               // llvm.x86.avx512.mask.pternlog.q.512
    x86_avx512_mask_range_pd_128,                 // llvm.x86.avx512.mask.range.pd.128
    x86_avx512_mask_range_pd_256,                 // llvm.x86.avx512.mask.range.pd.256
    x86_avx512_mask_range_pd_512,                 // llvm.x86.avx512.mask.range.pd.512
    x86_avx512_mask_range_ps_128,                 // llvm.x86.avx512.mask.range.ps.128
    x86_avx512_mask_range_ps_256,                 // llvm.x86.avx512.mask.range.ps.256
    x86_avx512_mask_range_ps_512,                 // llvm.x86.avx512.mask.range.ps.512
    x86_avx512_mask_range_sd,                     // llvm.x86.avx512.mask.range.sd
    x86_avx512_mask_range_ss,                     // llvm.x86.avx512.mask.range.ss
    x86_avx512_mask_reduce_pd_128,                // llvm.x86.avx512.mask.reduce.pd.128
    x86_avx512_mask_reduce_pd_256,                // llvm.x86.avx512.mask.reduce.pd.256
    x86_avx512_mask_reduce_pd_512,                // llvm.x86.avx512.mask.reduce.pd.512
    x86_avx512_mask_reduce_ps_128,                // llvm.x86.avx512.mask.reduce.ps.128
    x86_avx512_mask_reduce_ps_256,                // llvm.x86.avx512.mask.reduce.ps.256
    x86_avx512_mask_reduce_ps_512,                // llvm.x86.avx512.mask.reduce.ps.512
    x86_avx512_mask_reduce_sd,                    // llvm.x86.avx512.mask.reduce.sd
    x86_avx512_mask_reduce_ss,                    // llvm.x86.avx512.mask.reduce.ss
    x86_avx512_mask_rndscale_pd_128,              // llvm.x86.avx512.mask.rndscale.pd.128
    x86_avx512_mask_rndscale_pd_256,              // llvm.x86.avx512.mask.rndscale.pd.256
    x86_avx512_mask_rndscale_pd_512,              // llvm.x86.avx512.mask.rndscale.pd.512
    x86_avx512_mask_rndscale_ps_128,              // llvm.x86.avx512.mask.rndscale.ps.128
    x86_avx512_mask_rndscale_ps_256,              // llvm.x86.avx512.mask.rndscale.ps.256
    x86_avx512_mask_rndscale_ps_512,              // llvm.x86.avx512.mask.rndscale.ps.512
    x86_avx512_mask_rndscale_sd,                  // llvm.x86.avx512.mask.rndscale.sd
    x86_avx512_mask_rndscale_ss,                  // llvm.x86.avx512.mask.rndscale.ss
    x86_avx512_mask_scalef_pd_128,                // llvm.x86.avx512.mask.scalef.pd.128
    x86_avx512_mask_scalef_pd_256,                // llvm.x86.avx512.mask.scalef.pd.256
    x86_avx512_mask_scalef_pd_512,                // llvm.x86.avx512.mask.scalef.pd.512
    x86_avx512_mask_scalef_ps_128,                // llvm.x86.avx512.mask.scalef.ps.128
    x86_avx512_mask_scalef_ps_256,                // llvm.x86.avx512.mask.scalef.ps.256
    x86_avx512_mask_scalef_ps_512,                // llvm.x86.avx512.mask.scalef.ps.512
    x86_avx512_mask_scalef_sd,                    // llvm.x86.avx512.mask.scalef.sd
    x86_avx512_mask_scalef_ss,                    // llvm.x86.avx512.mask.scalef.ss
    x86_avx512_mask_sqrt_pd_128,                  // llvm.x86.avx512.mask.sqrt.pd.128
    x86_avx512_mask_sqrt_pd_256,                  // llvm.x86.avx512.mask.sqrt.pd.256
    x86_avx512_mask_sqrt_pd_512,                  // llvm.x86.avx512.mask.sqrt.pd.512
    x86_avx512_mask_sqrt_ps_128,                  // llvm.x86.avx512.mask.sqrt.ps.128
    x86_avx512_mask_sqrt_ps_256,                  // llvm.x86.avx512.mask.sqrt.ps.256
    x86_avx512_mask_sqrt_ps_512,                  // llvm.x86.avx512.mask.sqrt.ps.512
    x86_avx512_mask_sqrt_sd,                      // llvm.x86.avx512.mask.sqrt.sd
    x86_avx512_mask_sqrt_ss,                      // llvm.x86.avx512.mask.sqrt.ss
    x86_avx512_mask_store_ss,                     // llvm.x86.avx512.mask.store.ss
    x86_avx512_mask_sub_pd_512,                   // llvm.x86.avx512.mask.sub.pd.512
    x86_avx512_mask_sub_ps_512,                   // llvm.x86.avx512.mask.sub.ps.512
    x86_avx512_mask_sub_sd_round,                 // llvm.x86.avx512.mask.sub.sd.round
    x86_avx512_mask_sub_ss_round,                 // llvm.x86.avx512.mask.sub.ss.round
    x86_avx512_mask_vcvtph2ps_128,                // llvm.x86.avx512.mask.vcvtph2ps.128
    x86_avx512_mask_vcvtph2ps_256,                // llvm.x86.avx512.mask.vcvtph2ps.256
    x86_avx512_mask_vcvtph2ps_512,                // llvm.x86.avx512.mask.vcvtph2ps.512
    x86_avx512_mask_vcvtps2ph_128,                // llvm.x86.avx512.mask.vcvtps2ph.128
    x86_avx512_mask_vcvtps2ph_256,                // llvm.x86.avx512.mask.vcvtps2ph.256
    x86_avx512_mask_vcvtps2ph_512,                // llvm.x86.avx512.mask.vcvtps2ph.512
    x86_avx512_mask_vfmadd_pd_128,                // llvm.x86.avx512.mask.vfmadd.pd.128
    x86_avx512_mask_vfmadd_pd_256,                // llvm.x86.avx512.mask.vfmadd.pd.256
    x86_avx512_mask_vfmadd_pd_512,                // llvm.x86.avx512.mask.vfmadd.pd.512
    x86_avx512_mask_vfmadd_ps_128,                // llvm.x86.avx512.mask.vfmadd.ps.128
    x86_avx512_mask_vfmadd_ps_256,                // llvm.x86.avx512.mask.vfmadd.ps.256
    x86_avx512_mask_vfmadd_ps_512,                // llvm.x86.avx512.mask.vfmadd.ps.512
    x86_avx512_mask_vfmadd_sd,                    // llvm.x86.avx512.mask.vfmadd.sd
    x86_avx512_mask_vfmadd_ss,                    // llvm.x86.avx512.mask.vfmadd.ss
    x86_avx512_mask_vfmaddsub_pd_128,             // llvm.x86.avx512.mask.vfmaddsub.pd.128
    x86_avx512_mask_vfmaddsub_pd_256,             // llvm.x86.avx512.mask.vfmaddsub.pd.256
    x86_avx512_mask_vfmaddsub_pd_512,             // llvm.x86.avx512.mask.vfmaddsub.pd.512
    x86_avx512_mask_vfmaddsub_ps_128,             // llvm.x86.avx512.mask.vfmaddsub.ps.128
    x86_avx512_mask_vfmaddsub_ps_256,             // llvm.x86.avx512.mask.vfmaddsub.ps.256
    x86_avx512_mask_vfmaddsub_ps_512,             // llvm.x86.avx512.mask.vfmaddsub.ps.512
    x86_avx512_mask_vfnmadd_pd_128,               // llvm.x86.avx512.mask.vfnmadd.pd.128
    x86_avx512_mask_vfnmadd_pd_256,               // llvm.x86.avx512.mask.vfnmadd.pd.256
    x86_avx512_mask_vfnmadd_pd_512,               // llvm.x86.avx512.mask.vfnmadd.pd.512
    x86_avx512_mask_vfnmadd_ps_128,               // llvm.x86.avx512.mask.vfnmadd.ps.128
    x86_avx512_mask_vfnmadd_ps_256,               // llvm.x86.avx512.mask.vfnmadd.ps.256
    x86_avx512_mask_vfnmadd_ps_512,               // llvm.x86.avx512.mask.vfnmadd.ps.512
    x86_avx512_mask_vfnmsub_pd_128,               // llvm.x86.avx512.mask.vfnmsub.pd.128
    x86_avx512_mask_vfnmsub_pd_256,               // llvm.x86.avx512.mask.vfnmsub.pd.256
    x86_avx512_mask_vfnmsub_pd_512,               // llvm.x86.avx512.mask.vfnmsub.pd.512
    x86_avx512_mask_vfnmsub_ps_128,               // llvm.x86.avx512.mask.vfnmsub.ps.128
    x86_avx512_mask_vfnmsub_ps_256,               // llvm.x86.avx512.mask.vfnmsub.ps.256
    x86_avx512_mask_vfnmsub_ps_512,               // llvm.x86.avx512.mask.vfnmsub.ps.512
    x86_avx512_mask_vpdpbusd_128,                 // llvm.x86.avx512.mask.vpdpbusd.128
    x86_avx512_mask_vpdpbusd_256,                 // llvm.x86.avx512.mask.vpdpbusd.256
    x86_avx512_mask_vpdpbusd_512,                 // llvm.x86.avx512.mask.vpdpbusd.512
    x86_avx512_mask_vpdpbusds_128,                // llvm.x86.avx512.mask.vpdpbusds.128
    x86_avx512_mask_vpdpbusds_256,                // llvm.x86.avx512.mask.vpdpbusds.256
    x86_avx512_mask_vpdpbusds_512,                // llvm.x86.avx512.mask.vpdpbusds.512
    x86_avx512_mask_vpdpwssd_128,                 // llvm.x86.avx512.mask.vpdpwssd.128
    x86_avx512_mask_vpdpwssd_256,                 // llvm.x86.avx512.mask.vpdpwssd.256
    x86_avx512_mask_vpdpwssd_512,                 // llvm.x86.avx512.mask.vpdpwssd.512
    x86_avx512_mask_vpdpwssds_128,                // llvm.x86.avx512.mask.vpdpwssds.128
    x86_avx512_mask_vpdpwssds_256,                // llvm.x86.avx512.mask.vpdpwssds.256
    x86_avx512_mask_vpdpwssds_512,                // llvm.x86.avx512.mask.vpdpwssds.512
    x86_avx512_mask_vpermi2var_d_128,             // llvm.x86.avx512.mask.vpermi2var.d.128
    x86_avx512_mask_vpermi2var_d_256,             // llvm.x86.avx512.mask.vpermi2var.d.256
    x86_avx512_mask_vpermi2var_d_512,             // llvm.x86.avx512.mask.vpermi2var.d.512
    x86_avx512_mask_vpermi2var_hi_128,            // llvm.x86.avx512.mask.vpermi2var.hi.128
    x86_avx512_mask_vpermi2var_hi_256,            // llvm.x86.avx512.mask.vpermi2var.hi.256
    x86_avx512_mask_vpermi2var_hi_512,            // llvm.x86.avx512.mask.vpermi2var.hi.512
    x86_avx512_mask_vpermi2var_pd_128,            // llvm.x86.avx512.mask.vpermi2var.pd.128
    x86_avx512_mask_vpermi2var_pd_256,            // llvm.x86.avx512.mask.vpermi2var.pd.256
    x86_avx512_mask_vpermi2var_pd_512,            // llvm.x86.avx512.mask.vpermi2var.pd.512
    x86_avx512_mask_vpermi2var_ps_128,            // llvm.x86.avx512.mask.vpermi2var.ps.128
    x86_avx512_mask_vpermi2var_ps_256,            // llvm.x86.avx512.mask.vpermi2var.ps.256
    x86_avx512_mask_vpermi2var_ps_512,            // llvm.x86.avx512.mask.vpermi2var.ps.512
    x86_avx512_mask_vpermi2var_q_128,             // llvm.x86.avx512.mask.vpermi2var.q.128
    x86_avx512_mask_vpermi2var_q_256,             // llvm.x86.avx512.mask.vpermi2var.q.256
    x86_avx512_mask_vpermi2var_q_512,             // llvm.x86.avx512.mask.vpermi2var.q.512
    x86_avx512_mask_vpermi2var_qi_128,            // llvm.x86.avx512.mask.vpermi2var.qi.128
    x86_avx512_mask_vpermi2var_qi_256,            // llvm.x86.avx512.mask.vpermi2var.qi.256
    x86_avx512_mask_vpermi2var_qi_512,            // llvm.x86.avx512.mask.vpermi2var.qi.512
    x86_avx512_mask_vpermt2var_d_128,             // llvm.x86.avx512.mask.vpermt2var.d.128
    x86_avx512_mask_vpermt2var_d_256,             // llvm.x86.avx512.mask.vpermt2var.d.256
    x86_avx512_mask_vpermt2var_d_512,             // llvm.x86.avx512.mask.vpermt2var.d.512
    x86_avx512_mask_vpermt2var_hi_128,            // llvm.x86.avx512.mask.vpermt2var.hi.128
    x86_avx512_mask_vpermt2var_hi_256,            // llvm.x86.avx512.mask.vpermt2var.hi.256
    x86_avx512_mask_vpermt2var_hi_512,            // llvm.x86.avx512.mask.vpermt2var.hi.512
    x86_avx512_mask_vpermt2var_pd_128,            // llvm.x86.avx512.mask.vpermt2var.pd.128
    x86_avx512_mask_vpermt2var_pd_256,            // llvm.x86.avx512.mask.vpermt2var.pd.256
    x86_avx512_mask_vpermt2var_pd_512,            // llvm.x86.avx512.mask.vpermt2var.pd.512
    x86_avx512_mask_vpermt2var_ps_128,            // llvm.x86.avx512.mask.vpermt2var.ps.128
    x86_avx512_mask_vpermt2var_ps_256,            // llvm.x86.avx512.mask.vpermt2var.ps.256
    x86_avx512_mask_vpermt2var_ps_512,            // llvm.x86.avx512.mask.vpermt2var.ps.512
    x86_avx512_mask_vpermt2var_q_128,             // llvm.x86.avx512.mask.vpermt2var.q.128
    x86_avx512_mask_vpermt2var_q_256,             // llvm.x86.avx512.mask.vpermt2var.q.256
    x86_avx512_mask_vpermt2var_q_512,             // llvm.x86.avx512.mask.vpermt2var.q.512
    x86_avx512_mask_vpermt2var_qi_128,            // llvm.x86.avx512.mask.vpermt2var.qi.128
    x86_avx512_mask_vpermt2var_qi_256,            // llvm.x86.avx512.mask.vpermt2var.qi.256
    x86_avx512_mask_vpermt2var_qi_512,            // llvm.x86.avx512.mask.vpermt2var.qi.512
    x86_avx512_mask_vpmadd52h_uq_128,             // llvm.x86.avx512.mask.vpmadd52h.uq.128
    x86_avx512_mask_vpmadd52h_uq_256,             // llvm.x86.avx512.mask.vpmadd52h.uq.256
    x86_avx512_mask_vpmadd52h_uq_512,             // llvm.x86.avx512.mask.vpmadd52h.uq.512
    x86_avx512_mask_vpmadd52l_uq_128,             // llvm.x86.avx512.mask.vpmadd52l.uq.128
    x86_avx512_mask_vpmadd52l_uq_256,             // llvm.x86.avx512.mask.vpmadd52l.uq.256
    x86_avx512_mask_vpmadd52l_uq_512,             // llvm.x86.avx512.mask.vpmadd52l.uq.512
    x86_avx512_mask_vpshld_d_128,                 // llvm.x86.avx512.mask.vpshld.d.128
    x86_avx512_mask_vpshld_d_256,                 // llvm.x86.avx512.mask.vpshld.d.256
    x86_avx512_mask_vpshld_d_512,                 // llvm.x86.avx512.mask.vpshld.d.512
    x86_avx512_mask_vpshld_q_128,                 // llvm.x86.avx512.mask.vpshld.q.128
    x86_avx512_mask_vpshld_q_256,                 // llvm.x86.avx512.mask.vpshld.q.256
    x86_avx512_mask_vpshld_q_512,                 // llvm.x86.avx512.mask.vpshld.q.512
    x86_avx512_mask_vpshld_w_128,                 // llvm.x86.avx512.mask.vpshld.w.128
    x86_avx512_mask_vpshld_w_256,                 // llvm.x86.avx512.mask.vpshld.w.256
    x86_avx512_mask_vpshld_w_512,                 // llvm.x86.avx512.mask.vpshld.w.512
    x86_avx512_mask_vpshldv_d_128,                // llvm.x86.avx512.mask.vpshldv.d.128
    x86_avx512_mask_vpshldv_d_256,                // llvm.x86.avx512.mask.vpshldv.d.256
    x86_avx512_mask_vpshldv_d_512,                // llvm.x86.avx512.mask.vpshldv.d.512
    x86_avx512_mask_vpshldv_q_128,                // llvm.x86.avx512.mask.vpshldv.q.128
    x86_avx512_mask_vpshldv_q_256,                // llvm.x86.avx512.mask.vpshldv.q.256
    x86_avx512_mask_vpshldv_q_512,                // llvm.x86.avx512.mask.vpshldv.q.512
    x86_avx512_mask_vpshldv_w_128,                // llvm.x86.avx512.mask.vpshldv.w.128
    x86_avx512_mask_vpshldv_w_256,                // llvm.x86.avx512.mask.vpshldv.w.256
    x86_avx512_mask_vpshldv_w_512,                // llvm.x86.avx512.mask.vpshldv.w.512
    x86_avx512_mask_vpshrd_d_128,                 // llvm.x86.avx512.mask.vpshrd.d.128
    x86_avx512_mask_vpshrd_d_256,                 // llvm.x86.avx512.mask.vpshrd.d.256
    x86_avx512_mask_vpshrd_d_512,                 // llvm.x86.avx512.mask.vpshrd.d.512
    x86_avx512_mask_vpshrd_q_128,                 // llvm.x86.avx512.mask.vpshrd.q.128
    x86_avx512_mask_vpshrd_q_256,                 // llvm.x86.avx512.mask.vpshrd.q.256
    x86_avx512_mask_vpshrd_q_512,                 // llvm.x86.avx512.mask.vpshrd.q.512
    x86_avx512_mask_vpshrd_w_128,                 // llvm.x86.avx512.mask.vpshrd.w.128
    x86_avx512_mask_vpshrd_w_256,                 // llvm.x86.avx512.mask.vpshrd.w.256
    x86_avx512_mask_vpshrd_w_512,                 // llvm.x86.avx512.mask.vpshrd.w.512
    x86_avx512_mask_vpshrdv_d_128,                // llvm.x86.avx512.mask.vpshrdv.d.128
    x86_avx512_mask_vpshrdv_d_256,                // llvm.x86.avx512.mask.vpshrdv.d.256
    x86_avx512_mask_vpshrdv_d_512,                // llvm.x86.avx512.mask.vpshrdv.d.512
    x86_avx512_mask_vpshrdv_q_128,                // llvm.x86.avx512.mask.vpshrdv.q.128
    x86_avx512_mask_vpshrdv_q_256,                // llvm.x86.avx512.mask.vpshrdv.q.256
    x86_avx512_mask_vpshrdv_q_512,                // llvm.x86.avx512.mask.vpshrdv.q.512
    x86_avx512_mask_vpshrdv_w_128,                // llvm.x86.avx512.mask.vpshrdv.w.128
    x86_avx512_mask_vpshrdv_w_256,                // llvm.x86.avx512.mask.vpshrdv.w.256
    x86_avx512_mask_vpshrdv_w_512,                // llvm.x86.avx512.mask.vpshrdv.w.512
    x86_avx512_mask_vpshufbitqmb_128,             // llvm.x86.avx512.mask.vpshufbitqmb.128
    x86_avx512_mask_vpshufbitqmb_256,             // llvm.x86.avx512.mask.vpshufbitqmb.256
    x86_avx512_mask_vpshufbitqmb_512,             // llvm.x86.avx512.mask.vpshufbitqmb.512
    x86_avx512_mask3_vfmadd_pd_128,               // llvm.x86.avx512.mask3.vfmadd.pd.128
    x86_avx512_mask3_vfmadd_pd_256,               // llvm.x86.avx512.mask3.vfmadd.pd.256
    x86_avx512_mask3_vfmadd_pd_512,               // llvm.x86.avx512.mask3.vfmadd.pd.512
    x86_avx512_mask3_vfmadd_ps_128,               // llvm.x86.avx512.mask3.vfmadd.ps.128
    x86_avx512_mask3_vfmadd_ps_256,               // llvm.x86.avx512.mask3.vfmadd.ps.256
    x86_avx512_mask3_vfmadd_ps_512,               // llvm.x86.avx512.mask3.vfmadd.ps.512
    x86_avx512_mask3_vfmadd_sd,                   // llvm.x86.avx512.mask3.vfmadd.sd
    x86_avx512_mask3_vfmadd_ss,                   // llvm.x86.avx512.mask3.vfmadd.ss
    x86_avx512_mask3_vfmaddsub_pd_128,            // llvm.x86.avx512.mask3.vfmaddsub.pd.128
    x86_avx512_mask3_vfmaddsub_pd_256,            // llvm.x86.avx512.mask3.vfmaddsub.pd.256
    x86_avx512_mask3_vfmaddsub_pd_512,            // llvm.x86.avx512.mask3.vfmaddsub.pd.512
    x86_avx512_mask3_vfmaddsub_ps_128,            // llvm.x86.avx512.mask3.vfmaddsub.ps.128
    x86_avx512_mask3_vfmaddsub_ps_256,            // llvm.x86.avx512.mask3.vfmaddsub.ps.256
    x86_avx512_mask3_vfmaddsub_ps_512,            // llvm.x86.avx512.mask3.vfmaddsub.ps.512
    x86_avx512_mask3_vfmsub_pd_128,               // llvm.x86.avx512.mask3.vfmsub.pd.128
    x86_avx512_mask3_vfmsub_pd_256,               // llvm.x86.avx512.mask3.vfmsub.pd.256
    x86_avx512_mask3_vfmsub_pd_512,               // llvm.x86.avx512.mask3.vfmsub.pd.512
    x86_avx512_mask3_vfmsub_ps_128,               // llvm.x86.avx512.mask3.vfmsub.ps.128
    x86_avx512_mask3_vfmsub_ps_256,               // llvm.x86.avx512.mask3.vfmsub.ps.256
    x86_avx512_mask3_vfmsub_ps_512,               // llvm.x86.avx512.mask3.vfmsub.ps.512
    x86_avx512_mask3_vfmsub_sd,                   // llvm.x86.avx512.mask3.vfmsub.sd
    x86_avx512_mask3_vfmsub_ss,                   // llvm.x86.avx512.mask3.vfmsub.ss
    x86_avx512_mask3_vfmsubadd_pd_128,            // llvm.x86.avx512.mask3.vfmsubadd.pd.128
    x86_avx512_mask3_vfmsubadd_pd_256,            // llvm.x86.avx512.mask3.vfmsubadd.pd.256
    x86_avx512_mask3_vfmsubadd_pd_512,            // llvm.x86.avx512.mask3.vfmsubadd.pd.512
    x86_avx512_mask3_vfmsubadd_ps_128,            // llvm.x86.avx512.mask3.vfmsubadd.ps.128
    x86_avx512_mask3_vfmsubadd_ps_256,            // llvm.x86.avx512.mask3.vfmsubadd.ps.256
    x86_avx512_mask3_vfmsubadd_ps_512,            // llvm.x86.avx512.mask3.vfmsubadd.ps.512
    x86_avx512_mask3_vfnmsub_pd_128,              // llvm.x86.avx512.mask3.vfnmsub.pd.128
    x86_avx512_mask3_vfnmsub_pd_256,              // llvm.x86.avx512.mask3.vfnmsub.pd.256
    x86_avx512_mask3_vfnmsub_pd_512,              // llvm.x86.avx512.mask3.vfnmsub.pd.512
    x86_avx512_mask3_vfnmsub_ps_128,              // llvm.x86.avx512.mask3.vfnmsub.ps.128
    x86_avx512_mask3_vfnmsub_ps_256,              // llvm.x86.avx512.mask3.vfnmsub.ps.256
    x86_avx512_mask3_vfnmsub_ps_512,              // llvm.x86.avx512.mask3.vfnmsub.ps.512
    x86_avx512_mask3_vfnmsub_sd,                  // llvm.x86.avx512.mask3.vfnmsub.sd
    x86_avx512_mask3_vfnmsub_ss,                  // llvm.x86.avx512.mask3.vfnmsub.ss
    x86_avx512_maskz_fixupimm_pd_128,             // llvm.x86.avx512.maskz.fixupimm.pd.128
    x86_avx512_maskz_fixupimm_pd_256,             // llvm.x86.avx512.maskz.fixupimm.pd.256
    x86_avx512_maskz_fixupimm_pd_512,             // llvm.x86.avx512.maskz.fixupimm.pd.512
    x86_avx512_maskz_fixupimm_ps_128,             // llvm.x86.avx512.maskz.fixupimm.ps.128
    x86_avx512_maskz_fixupimm_ps_256,             // llvm.x86.avx512.maskz.fixupimm.ps.256
    x86_avx512_maskz_fixupimm_ps_512,             // llvm.x86.avx512.maskz.fixupimm.ps.512
    x86_avx512_maskz_fixupimm_sd,                 // llvm.x86.avx512.maskz.fixupimm.sd
    x86_avx512_maskz_fixupimm_ss,                 // llvm.x86.avx512.maskz.fixupimm.ss
    x86_avx512_maskz_pternlog_d_128,              // llvm.x86.avx512.maskz.pternlog.d.128
    x86_avx512_maskz_pternlog_d_256,              // llvm.x86.avx512.maskz.pternlog.d.256
    x86_avx512_maskz_pternlog_d_512,              // llvm.x86.avx512.maskz.pternlog.d.512
    x86_avx512_maskz_pternlog_q_128,              // llvm.x86.avx512.maskz.pternlog.q.128
    x86_avx512_maskz_pternlog_q_256,              // llvm.x86.avx512.maskz.pternlog.q.256
    x86_avx512_maskz_pternlog_q_512,              // llvm.x86.avx512.maskz.pternlog.q.512
    x86_avx512_maskz_vfmadd_pd_128,               // llvm.x86.avx512.maskz.vfmadd.pd.128
    x86_avx512_maskz_vfmadd_pd_256,               // llvm.x86.avx512.maskz.vfmadd.pd.256
    x86_avx512_maskz_vfmadd_pd_512,               // llvm.x86.avx512.maskz.vfmadd.pd.512
    x86_avx512_maskz_vfmadd_ps_128,               // llvm.x86.avx512.maskz.vfmadd.ps.128
    x86_avx512_maskz_vfmadd_ps_256,               // llvm.x86.avx512.maskz.vfmadd.ps.256
    x86_avx512_maskz_vfmadd_ps_512,               // llvm.x86.avx512.maskz.vfmadd.ps.512
    x86_avx512_maskz_vfmadd_sd,                   // llvm.x86.avx512.maskz.vfmadd.sd
    x86_avx512_maskz_vfmadd_ss,                   // llvm.x86.avx512.maskz.vfmadd.ss
    x86_avx512_maskz_vfmaddsub_pd_128,            // llvm.x86.avx512.maskz.vfmaddsub.pd.128
    x86_avx512_maskz_vfmaddsub_pd_256,            // llvm.x86.avx512.maskz.vfmaddsub.pd.256
    x86_avx512_maskz_vfmaddsub_pd_512,            // llvm.x86.avx512.maskz.vfmaddsub.pd.512
    x86_avx512_maskz_vfmaddsub_ps_128,            // llvm.x86.avx512.maskz.vfmaddsub.ps.128
    x86_avx512_maskz_vfmaddsub_ps_256,            // llvm.x86.avx512.maskz.vfmaddsub.ps.256
    x86_avx512_maskz_vfmaddsub_ps_512,            // llvm.x86.avx512.maskz.vfmaddsub.ps.512
    x86_avx512_maskz_vpdpbusd_128,                // llvm.x86.avx512.maskz.vpdpbusd.128
    x86_avx512_maskz_vpdpbusd_256,                // llvm.x86.avx512.maskz.vpdpbusd.256
    x86_avx512_maskz_vpdpbusd_512,                // llvm.x86.avx512.maskz.vpdpbusd.512
    x86_avx512_maskz_vpdpbusds_128,               // llvm.x86.avx512.maskz.vpdpbusds.128
    x86_avx512_maskz_vpdpbusds_256,               // llvm.x86.avx512.maskz.vpdpbusds.256
    x86_avx512_maskz_vpdpbusds_512,               // llvm.x86.avx512.maskz.vpdpbusds.512
    x86_avx512_maskz_vpdpwssd_128,                // llvm.x86.avx512.maskz.vpdpwssd.128
    x86_avx512_maskz_vpdpwssd_256,                // llvm.x86.avx512.maskz.vpdpwssd.256
    x86_avx512_maskz_vpdpwssd_512,                // llvm.x86.avx512.maskz.vpdpwssd.512
    x86_avx512_maskz_vpdpwssds_128,               // llvm.x86.avx512.maskz.vpdpwssds.128
    x86_avx512_maskz_vpdpwssds_256,               // llvm.x86.avx512.maskz.vpdpwssds.256
    x86_avx512_maskz_vpdpwssds_512,               // llvm.x86.avx512.maskz.vpdpwssds.512
    x86_avx512_maskz_vpermt2var_d_128,            // llvm.x86.avx512.maskz.vpermt2var.d.128
    x86_avx512_maskz_vpermt2var_d_256,            // llvm.x86.avx512.maskz.vpermt2var.d.256
    x86_avx512_maskz_vpermt2var_d_512,            // llvm.x86.avx512.maskz.vpermt2var.d.512
    x86_avx512_maskz_vpermt2var_hi_128,           // llvm.x86.avx512.maskz.vpermt2var.hi.128
    x86_avx512_maskz_vpermt2var_hi_256,           // llvm.x86.avx512.maskz.vpermt2var.hi.256
    x86_avx512_maskz_vpermt2var_hi_512,           // llvm.x86.avx512.maskz.vpermt2var.hi.512
    x86_avx512_maskz_vpermt2var_pd_128,           // llvm.x86.avx512.maskz.vpermt2var.pd.128
    x86_avx512_maskz_vpermt2var_pd_256,           // llvm.x86.avx512.maskz.vpermt2var.pd.256
    x86_avx512_maskz_vpermt2var_pd_512,           // llvm.x86.avx512.maskz.vpermt2var.pd.512
    x86_avx512_maskz_vpermt2var_ps_128,           // llvm.x86.avx512.maskz.vpermt2var.ps.128
    x86_avx512_maskz_vpermt2var_ps_256,           // llvm.x86.avx512.maskz.vpermt2var.ps.256
    x86_avx512_maskz_vpermt2var_ps_512,           // llvm.x86.avx512.maskz.vpermt2var.ps.512
    x86_avx512_maskz_vpermt2var_q_128,            // llvm.x86.avx512.maskz.vpermt2var.q.128
    x86_avx512_maskz_vpermt2var_q_256,            // llvm.x86.avx512.maskz.vpermt2var.q.256
    x86_avx512_maskz_vpermt2var_q_512,            // llvm.x86.avx512.maskz.vpermt2var.q.512
    x86_avx512_maskz_vpermt2var_qi_128,           // llvm.x86.avx512.maskz.vpermt2var.qi.128
    x86_avx512_maskz_vpermt2var_qi_256,           // llvm.x86.avx512.maskz.vpermt2var.qi.256
    x86_avx512_maskz_vpermt2var_qi_512,           // llvm.x86.avx512.maskz.vpermt2var.qi.512
    x86_avx512_maskz_vpmadd52h_uq_128,            // llvm.x86.avx512.maskz.vpmadd52h.uq.128
    x86_avx512_maskz_vpmadd52h_uq_256,            // llvm.x86.avx512.maskz.vpmadd52h.uq.256
    x86_avx512_maskz_vpmadd52h_uq_512,            // llvm.x86.avx512.maskz.vpmadd52h.uq.512
    x86_avx512_maskz_vpmadd52l_uq_128,            // llvm.x86.avx512.maskz.vpmadd52l.uq.128
    x86_avx512_maskz_vpmadd52l_uq_256,            // llvm.x86.avx512.maskz.vpmadd52l.uq.256
    x86_avx512_maskz_vpmadd52l_uq_512,            // llvm.x86.avx512.maskz.vpmadd52l.uq.512
    x86_avx512_maskz_vpshldv_d_128,               // llvm.x86.avx512.maskz.vpshldv.d.128
    x86_avx512_maskz_vpshldv_d_256,               // llvm.x86.avx512.maskz.vpshldv.d.256
    x86_avx512_maskz_vpshldv_d_512,               // llvm.x86.avx512.maskz.vpshldv.d.512
    x86_avx512_maskz_vpshldv_q_128,               // llvm.x86.avx512.maskz.vpshldv.q.128
    x86_avx512_maskz_vpshldv_q_256,               // llvm.x86.avx512.maskz.vpshldv.q.256
    x86_avx512_maskz_vpshldv_q_512,               // llvm.x86.avx512.maskz.vpshldv.q.512
    x86_avx512_maskz_vpshldv_w_128,               // llvm.x86.avx512.maskz.vpshldv.w.128
    x86_avx512_maskz_vpshldv_w_256,               // llvm.x86.avx512.maskz.vpshldv.w.256
    x86_avx512_maskz_vpshldv_w_512,               // llvm.x86.avx512.maskz.vpshldv.w.512
    x86_avx512_maskz_vpshrdv_d_128,               // llvm.x86.avx512.maskz.vpshrdv.d.128
    x86_avx512_maskz_vpshrdv_d_256,               // llvm.x86.avx512.maskz.vpshrdv.d.256
    x86_avx512_maskz_vpshrdv_d_512,               // llvm.x86.avx512.maskz.vpshrdv.d.512
    x86_avx512_maskz_vpshrdv_q_128,               // llvm.x86.avx512.maskz.vpshrdv.q.128
    x86_avx512_maskz_vpshrdv_q_256,               // llvm.x86.avx512.maskz.vpshrdv.q.256
    x86_avx512_maskz_vpshrdv_q_512,               // llvm.x86.avx512.maskz.vpshrdv.q.512
    x86_avx512_maskz_vpshrdv_w_128,               // llvm.x86.avx512.maskz.vpshrdv.w.128
    x86_avx512_maskz_vpshrdv_w_256,               // llvm.x86.avx512.maskz.vpshrdv.w.256
    x86_avx512_maskz_vpshrdv_w_512,               // llvm.x86.avx512.maskz.vpshrdv.w.512
    x86_avx512_packssdw_512,                      // llvm.x86.avx512.packssdw.512
    x86_avx512_packsswb_512,                      // llvm.x86.avx512.packsswb.512
    x86_avx512_packusdw_512,                      // llvm.x86.avx512.packusdw.512
    x86_avx512_packuswb_512,                      // llvm.x86.avx512.packuswb.512
    x86_avx512_pmul_dq_512,                       // llvm.x86.avx512.pmul.dq.512
    x86_avx512_pmulu_dq_512,                      // llvm.x86.avx512.pmulu.dq.512
    x86_avx512_psad_bw_512,                       // llvm.x86.avx512.psad.bw.512
    x86_avx512_pshuf_b_512,                       // llvm.x86.avx512.pshuf.b.512
    x86_avx512_psll_d_512,                        // llvm.x86.avx512.psll.d.512
    x86_avx512_psll_q_512,                        // llvm.x86.avx512.psll.q.512
    x86_avx512_psll_w_512,                        // llvm.x86.avx512.psll.w.512
    x86_avx512_pslli_d_512,                       // llvm.x86.avx512.pslli.d.512
    x86_avx512_pslli_q_512,                       // llvm.x86.avx512.pslli.q.512
    x86_avx512_pslli_w_512,                       // llvm.x86.avx512.pslli.w.512
    x86_avx512_psllv_d_512,                       // llvm.x86.avx512.psllv.d.512
    x86_avx512_psllv_q_512,                       // llvm.x86.avx512.psllv.q.512
    x86_avx512_psllv_w_128,                       // llvm.x86.avx512.psllv.w.128
    x86_avx512_psllv_w_256,                       // llvm.x86.avx512.psllv.w.256
    x86_avx512_psllv_w_512,                       // llvm.x86.avx512.psllv.w.512
    x86_avx512_psra_d_512,                        // llvm.x86.avx512.psra.d.512
    x86_avx512_psra_q_128,                        // llvm.x86.avx512.psra.q.128
    x86_avx512_psra_q_256,                        // llvm.x86.avx512.psra.q.256
    x86_avx512_psra_q_512,                        // llvm.x86.avx512.psra.q.512
    x86_avx512_psra_w_512,                        // llvm.x86.avx512.psra.w.512
    x86_avx512_psrai_d_512,                       // llvm.x86.avx512.psrai.d.512
    x86_avx512_psrai_q_128,                       // llvm.x86.avx512.psrai.q.128
    x86_avx512_psrai_q_256,                       // llvm.x86.avx512.psrai.q.256
    x86_avx512_psrai_q_512,                       // llvm.x86.avx512.psrai.q.512
    x86_avx512_psrai_w_512,                       // llvm.x86.avx512.psrai.w.512
    x86_avx512_psrav_d_512,                       // llvm.x86.avx512.psrav.d.512
    x86_avx512_psrav_q_128,                       // llvm.x86.avx512.psrav.q.128
    x86_avx512_psrav_q_256,                       // llvm.x86.avx512.psrav.q.256
    x86_avx512_psrav_q_512,                       // llvm.x86.avx512.psrav.q.512
    x86_avx512_psrav_w_128,                       // llvm.x86.avx512.psrav.w.128
    x86_avx512_psrav_w_256,                       // llvm.x86.avx512.psrav.w.256
    x86_avx512_psrav_w_512,                       // llvm.x86.avx512.psrav.w.512
    x86_avx512_psrl_d_512,                        // llvm.x86.avx512.psrl.d.512
    x86_avx512_psrl_q_512,                        // llvm.x86.avx512.psrl.q.512
    x86_avx512_psrl_w_512,                        // llvm.x86.avx512.psrl.w.512
    x86_avx512_psrli_d_512,                       // llvm.x86.avx512.psrli.d.512
    x86_avx512_psrli_q_512,                       // llvm.x86.avx512.psrli.q.512
    x86_avx512_psrli_w_512,                       // llvm.x86.avx512.psrli.w.512
    x86_avx512_psrlv_d_512,                       // llvm.x86.avx512.psrlv.d.512
    x86_avx512_psrlv_q_512,                       // llvm.x86.avx512.psrlv.q.512
    x86_avx512_psrlv_w_128,                       // llvm.x86.avx512.psrlv.w.128
    x86_avx512_psrlv_w_256,                       // llvm.x86.avx512.psrlv.w.256
    x86_avx512_psrlv_w_512,                       // llvm.x86.avx512.psrlv.w.512
    x86_avx512_rcp14_pd_128,                      // llvm.x86.avx512.rcp14.pd.128
    x86_avx512_rcp14_pd_256,                      // llvm.x86.avx512.rcp14.pd.256
    x86_avx512_rcp14_pd_512,                      // llvm.x86.avx512.rcp14.pd.512
    x86_avx512_rcp14_ps_128,                      // llvm.x86.avx512.rcp14.ps.128
    x86_avx512_rcp14_ps_256,                      // llvm.x86.avx512.rcp14.ps.256
    x86_avx512_rcp14_ps_512,                      // llvm.x86.avx512.rcp14.ps.512
    x86_avx512_rcp14_sd,                          // llvm.x86.avx512.rcp14.sd
    x86_avx512_rcp14_ss,                          // llvm.x86.avx512.rcp14.ss
    x86_avx512_rcp28_pd,                          // llvm.x86.avx512.rcp28.pd
    x86_avx512_rcp28_ps,                          // llvm.x86.avx512.rcp28.ps
    x86_avx512_rcp28_sd,                          // llvm.x86.avx512.rcp28.sd
    x86_avx512_rcp28_ss,                          // llvm.x86.avx512.rcp28.ss
    x86_avx512_rsqrt14_pd_128,                    // llvm.x86.avx512.rsqrt14.pd.128
    x86_avx512_rsqrt14_pd_256,                    // llvm.x86.avx512.rsqrt14.pd.256
    x86_avx512_rsqrt14_pd_512,                    // llvm.x86.avx512.rsqrt14.pd.512
    x86_avx512_rsqrt14_ps_128,                    // llvm.x86.avx512.rsqrt14.ps.128
    x86_avx512_rsqrt14_ps_256,                    // llvm.x86.avx512.rsqrt14.ps.256
    x86_avx512_rsqrt14_ps_512,                    // llvm.x86.avx512.rsqrt14.ps.512
    x86_avx512_rsqrt14_sd,                        // llvm.x86.avx512.rsqrt14.sd
    x86_avx512_rsqrt14_ss,                        // llvm.x86.avx512.rsqrt14.ss
    x86_avx512_rsqrt28_pd,                        // llvm.x86.avx512.rsqrt28.pd
    x86_avx512_rsqrt28_ps,                        // llvm.x86.avx512.rsqrt28.ps
    x86_avx512_rsqrt28_sd,                        // llvm.x86.avx512.rsqrt28.sd
    x86_avx512_rsqrt28_ss,                        // llvm.x86.avx512.rsqrt28.ss
    x86_avx512_scatter_dpd_512,                   // llvm.x86.avx512.scatter.dpd.512
    x86_avx512_scatter_dpi_512,                   // llvm.x86.avx512.scatter.dpi.512
    x86_avx512_scatter_dpq_512,                   // llvm.x86.avx512.scatter.dpq.512
    x86_avx512_scatter_dps_512,                   // llvm.x86.avx512.scatter.dps.512
    x86_avx512_scatter_qpd_512,                   // llvm.x86.avx512.scatter.qpd.512
    x86_avx512_scatter_qpi_512,                   // llvm.x86.avx512.scatter.qpi.512
    x86_avx512_scatter_qpq_512,                   // llvm.x86.avx512.scatter.qpq.512
    x86_avx512_scatter_qps_512,                   // llvm.x86.avx512.scatter.qps.512
    x86_avx512_scatterdiv2_df,                    // llvm.x86.avx512.scatterdiv2.df
    x86_avx512_scatterdiv2_di,                    // llvm.x86.avx512.scatterdiv2.di
    x86_avx512_scatterdiv4_df,                    // llvm.x86.avx512.scatterdiv4.df
    x86_avx512_scatterdiv4_di,                    // llvm.x86.avx512.scatterdiv4.di
    x86_avx512_scatterdiv4_sf,                    // llvm.x86.avx512.scatterdiv4.sf
    x86_avx512_scatterdiv4_si,                    // llvm.x86.avx512.scatterdiv4.si
    x86_avx512_scatterdiv8_sf,                    // llvm.x86.avx512.scatterdiv8.sf
    x86_avx512_scatterdiv8_si,                    // llvm.x86.avx512.scatterdiv8.si
    x86_avx512_scatterpf_dpd_512,                 // llvm.x86.avx512.scatterpf.dpd.512
    x86_avx512_scatterpf_dps_512,                 // llvm.x86.avx512.scatterpf.dps.512
    x86_avx512_scatterpf_qpd_512,                 // llvm.x86.avx512.scatterpf.qpd.512
    x86_avx512_scatterpf_qps_512,                 // llvm.x86.avx512.scatterpf.qps.512
    x86_avx512_scattersiv2_df,                    // llvm.x86.avx512.scattersiv2.df
    x86_avx512_scattersiv2_di,                    // llvm.x86.avx512.scattersiv2.di
    x86_avx512_scattersiv4_df,                    // llvm.x86.avx512.scattersiv4.df
    x86_avx512_scattersiv4_di,                    // llvm.x86.avx512.scattersiv4.di
    x86_avx512_scattersiv4_sf,                    // llvm.x86.avx512.scattersiv4.sf
    x86_avx512_scattersiv4_si,                    // llvm.x86.avx512.scattersiv4.si
    x86_avx512_scattersiv8_sf,                    // llvm.x86.avx512.scattersiv8.sf
    x86_avx512_scattersiv8_si,                    // llvm.x86.avx512.scattersiv8.si
    x86_avx512_vbroadcast_sd_512,                 // llvm.x86.avx512.vbroadcast.sd.512
    x86_avx512_vbroadcast_ss_512,                 // llvm.x86.avx512.vbroadcast.ss.512
    x86_avx512_vcomi_sd,                          // llvm.x86.avx512.vcomi.sd
    x86_avx512_vcomi_ss,                          // llvm.x86.avx512.vcomi.ss
    x86_avx512_vcvtsd2si32,                       // llvm.x86.avx512.vcvtsd2si32
    x86_avx512_vcvtsd2si64,                       // llvm.x86.avx512.vcvtsd2si64
    x86_avx512_vcvtsd2usi32,                      // llvm.x86.avx512.vcvtsd2usi32
    x86_avx512_vcvtsd2usi64,                      // llvm.x86.avx512.vcvtsd2usi64
    x86_avx512_vcvtss2si32,                       // llvm.x86.avx512.vcvtss2si32
    x86_avx512_vcvtss2si64,                       // llvm.x86.avx512.vcvtss2si64
    x86_avx512_vcvtss2usi32,                      // llvm.x86.avx512.vcvtss2usi32
    x86_avx512_vcvtss2usi64,                      // llvm.x86.avx512.vcvtss2usi64
    x86_avx512_vpermilvar_pd_512,                 // llvm.x86.avx512.vpermilvar.pd.512
    x86_avx512_vpermilvar_ps_512,                 // llvm.x86.avx512.vpermilvar.ps.512
    x86_bmi_bextr_32,                             // llvm.x86.bmi.bextr.32
    x86_bmi_bextr_64,                             // llvm.x86.bmi.bextr.64
    x86_bmi_bzhi_32,                              // llvm.x86.bmi.bzhi.32
    x86_bmi_bzhi_64,                              // llvm.x86.bmi.bzhi.64
    x86_bmi_pdep_32,                              // llvm.x86.bmi.pdep.32
    x86_bmi_pdep_64,                              // llvm.x86.bmi.pdep.64
    x86_bmi_pext_32,                              // llvm.x86.bmi.pext.32
    x86_bmi_pext_64,                              // llvm.x86.bmi.pext.64
    x86_clflushopt,                               // llvm.x86.clflushopt
    x86_clrssbsy,                                 // llvm.x86.clrssbsy
    x86_clwb,                                     // llvm.x86.clwb
    x86_clzero,                                   // llvm.x86.clzero
    x86_flags_read_u32,                           // llvm.x86.flags.read.u32
    x86_flags_read_u64,                           // llvm.x86.flags.read.u64
    x86_flags_write_u32,                          // llvm.x86.flags.write.u32
    x86_flags_write_u64,                          // llvm.x86.flags.write.u64
    x86_fma_vfmadd_pd,                            // llvm.x86.fma.vfmadd.pd
    x86_fma_vfmadd_pd_256,                        // llvm.x86.fma.vfmadd.pd.256
    x86_fma_vfmadd_ps,                            // llvm.x86.fma.vfmadd.ps
    x86_fma_vfmadd_ps_256,                        // llvm.x86.fma.vfmadd.ps.256
    x86_fma_vfmadd_sd,                            // llvm.x86.fma.vfmadd.sd
    x86_fma_vfmadd_ss,                            // llvm.x86.fma.vfmadd.ss
    x86_fma_vfmaddsub_pd,                         // llvm.x86.fma.vfmaddsub.pd
    x86_fma_vfmaddsub_pd_256,                     // llvm.x86.fma.vfmaddsub.pd.256
    x86_fma_vfmaddsub_ps,                         // llvm.x86.fma.vfmaddsub.ps
    x86_fma_vfmaddsub_ps_256,                     // llvm.x86.fma.vfmaddsub.ps.256
    x86_fma_vfmsub_pd,                            // llvm.x86.fma.vfmsub.pd
    x86_fma_vfmsub_pd_256,                        // llvm.x86.fma.vfmsub.pd.256
    x86_fma_vfmsub_ps,                            // llvm.x86.fma.vfmsub.ps
    x86_fma_vfmsub_ps_256,                        // llvm.x86.fma.vfmsub.ps.256
    x86_fma_vfmsub_sd,                            // llvm.x86.fma.vfmsub.sd
    x86_fma_vfmsub_ss,                            // llvm.x86.fma.vfmsub.ss
    x86_fma_vfmsubadd_pd,                         // llvm.x86.fma.vfmsubadd.pd
    x86_fma_vfmsubadd_pd_256,                     // llvm.x86.fma.vfmsubadd.pd.256
    x86_fma_vfmsubadd_ps,                         // llvm.x86.fma.vfmsubadd.ps
    x86_fma_vfmsubadd_ps_256,                     // llvm.x86.fma.vfmsubadd.ps.256
    x86_fma_vfnmadd_pd,                           // llvm.x86.fma.vfnmadd.pd
    x86_fma_vfnmadd_pd_256,                       // llvm.x86.fma.vfnmadd.pd.256
    x86_fma_vfnmadd_ps,                           // llvm.x86.fma.vfnmadd.ps
    x86_fma_vfnmadd_ps_256,                       // llvm.x86.fma.vfnmadd.ps.256
    x86_fma_vfnmadd_sd,                           // llvm.x86.fma.vfnmadd.sd
    x86_fma_vfnmadd_ss,                           // llvm.x86.fma.vfnmadd.ss
    x86_fma_vfnmsub_pd,                           // llvm.x86.fma.vfnmsub.pd
    x86_fma_vfnmsub_pd_256,                       // llvm.x86.fma.vfnmsub.pd.256
    x86_fma_vfnmsub_ps,                           // llvm.x86.fma.vfnmsub.ps
    x86_fma_vfnmsub_ps_256,                       // llvm.x86.fma.vfnmsub.ps.256
    x86_fma_vfnmsub_sd,                           // llvm.x86.fma.vfnmsub.sd
    x86_fma_vfnmsub_ss,                           // llvm.x86.fma.vfnmsub.ss
    x86_fma4_vfmadd_sd,                           // llvm.x86.fma4.vfmadd.sd
    x86_fma4_vfmadd_ss,                           // llvm.x86.fma4.vfmadd.ss
    x86_fxrstor,                                  // llvm.x86.fxrstor
    x86_fxrstor64,                                // llvm.x86.fxrstor64
    x86_fxsave,                                   // llvm.x86.fxsave
    x86_fxsave64,                                 // llvm.x86.fxsave64
    x86_incsspd,                                  // llvm.x86.incsspd
    x86_incsspq,                                  // llvm.x86.incsspq
    x86_int,                                      // llvm.x86.int
    x86_llwpcb,                                   // llvm.x86.llwpcb
    x86_lwpins32,                                 // llvm.x86.lwpins32
    x86_lwpins64,                                 // llvm.x86.lwpins64
    x86_lwpval32,                                 // llvm.x86.lwpval32
    x86_lwpval64,                                 // llvm.x86.lwpval64
    x86_mmx_emms,                                 // llvm.x86.mmx.emms
    x86_mmx_femms,                                // llvm.x86.mmx.femms
    x86_mmx_maskmovq,                             // llvm.x86.mmx.maskmovq
    x86_mmx_movnt_dq,                             // llvm.x86.mmx.movnt.dq
    x86_mmx_packssdw,                             // llvm.x86.mmx.packssdw
    x86_mmx_packsswb,                             // llvm.x86.mmx.packsswb
    x86_mmx_packuswb,                             // llvm.x86.mmx.packuswb
    x86_mmx_padd_b,                               // llvm.x86.mmx.padd.b
    x86_mmx_padd_d,                               // llvm.x86.mmx.padd.d
    x86_mmx_padd_q,                               // llvm.x86.mmx.padd.q
    x86_mmx_padd_w,                               // llvm.x86.mmx.padd.w
    x86_mmx_padds_b,                              // llvm.x86.mmx.padds.b
    x86_mmx_padds_w,                              // llvm.x86.mmx.padds.w
    x86_mmx_paddus_b,                             // llvm.x86.mmx.paddus.b
    x86_mmx_paddus_w,                             // llvm.x86.mmx.paddus.w
    x86_mmx_palignr_b,                            // llvm.x86.mmx.palignr.b
    x86_mmx_pand,                                 // llvm.x86.mmx.pand
    x86_mmx_pandn,                                // llvm.x86.mmx.pandn
    x86_mmx_pavg_b,                               // llvm.x86.mmx.pavg.b
    x86_mmx_pavg_w,                               // llvm.x86.mmx.pavg.w
    x86_mmx_pcmpeq_b,                             // llvm.x86.mmx.pcmpeq.b
    x86_mmx_pcmpeq_d,                             // llvm.x86.mmx.pcmpeq.d
    x86_mmx_pcmpeq_w,                             // llvm.x86.mmx.pcmpeq.w
    x86_mmx_pcmpgt_b,                             // llvm.x86.mmx.pcmpgt.b
    x86_mmx_pcmpgt_d,                             // llvm.x86.mmx.pcmpgt.d
    x86_mmx_pcmpgt_w,                             // llvm.x86.mmx.pcmpgt.w
    x86_mmx_pextr_w,                              // llvm.x86.mmx.pextr.w
    x86_mmx_pinsr_w,                              // llvm.x86.mmx.pinsr.w
    x86_mmx_pmadd_wd,                             // llvm.x86.mmx.pmadd.wd
    x86_mmx_pmaxs_w,                              // llvm.x86.mmx.pmaxs.w
    x86_mmx_pmaxu_b,                              // llvm.x86.mmx.pmaxu.b
    x86_mmx_pmins_w,                              // llvm.x86.mmx.pmins.w
    x86_mmx_pminu_b,                              // llvm.x86.mmx.pminu.b
    x86_mmx_pmovmskb,                             // llvm.x86.mmx.pmovmskb
    x86_mmx_pmulh_w,                              // llvm.x86.mmx.pmulh.w
    x86_mmx_pmulhu_w,                             // llvm.x86.mmx.pmulhu.w
    x86_mmx_pmull_w,                              // llvm.x86.mmx.pmull.w
    x86_mmx_pmulu_dq,                             // llvm.x86.mmx.pmulu.dq
    x86_mmx_por,                                  // llvm.x86.mmx.por
    x86_mmx_psad_bw,                              // llvm.x86.mmx.psad.bw
    x86_mmx_psll_d,                               // llvm.x86.mmx.psll.d
    x86_mmx_psll_q,                               // llvm.x86.mmx.psll.q
    x86_mmx_psll_w,                               // llvm.x86.mmx.psll.w
    x86_mmx_pslli_d,                              // llvm.x86.mmx.pslli.d
    x86_mmx_pslli_q,                              // llvm.x86.mmx.pslli.q
    x86_mmx_pslli_w,                              // llvm.x86.mmx.pslli.w
    x86_mmx_psra_d,                               // llvm.x86.mmx.psra.d
    x86_mmx_psra_w,                               // llvm.x86.mmx.psra.w
    x86_mmx_psrai_d,                              // llvm.x86.mmx.psrai.d
    x86_mmx_psrai_w,                              // llvm.x86.mmx.psrai.w
    x86_mmx_psrl_d,                               // llvm.x86.mmx.psrl.d
    x86_mmx_psrl_q,                               // llvm.x86.mmx.psrl.q
    x86_mmx_psrl_w,                               // llvm.x86.mmx.psrl.w
    x86_mmx_psrli_d,                              // llvm.x86.mmx.psrli.d
    x86_mmx_psrli_q,                              // llvm.x86.mmx.psrli.q
    x86_mmx_psrli_w,                              // llvm.x86.mmx.psrli.w
    x86_mmx_psub_b,                               // llvm.x86.mmx.psub.b
    x86_mmx_psub_d,                               // llvm.x86.mmx.psub.d
    x86_mmx_psub_q,                               // llvm.x86.mmx.psub.q
    x86_mmx_psub_w,                               // llvm.x86.mmx.psub.w
    x86_mmx_psubs_b,                              // llvm.x86.mmx.psubs.b
    x86_mmx_psubs_w,                              // llvm.x86.mmx.psubs.w
    x86_mmx_psubus_b,                             // llvm.x86.mmx.psubus.b
    x86_mmx_psubus_w,                             // llvm.x86.mmx.psubus.w
    x86_mmx_punpckhbw,                            // llvm.x86.mmx.punpckhbw
    x86_mmx_punpckhdq,                            // llvm.x86.mmx.punpckhdq
    x86_mmx_punpckhwd,                            // llvm.x86.mmx.punpckhwd
    x86_mmx_punpcklbw,                            // llvm.x86.mmx.punpcklbw
    x86_mmx_punpckldq,                            // llvm.x86.mmx.punpckldq
    x86_mmx_punpcklwd,                            // llvm.x86.mmx.punpcklwd
    x86_mmx_pxor,                                 // llvm.x86.mmx.pxor
    x86_monitorx,                                 // llvm.x86.monitorx
    x86_mwaitx,                                   // llvm.x86.mwaitx
    x86_pclmulqdq,                                // llvm.x86.pclmulqdq
    x86_pclmulqdq_256,                            // llvm.x86.pclmulqdq.256
    x86_pclmulqdq_512,                            // llvm.x86.pclmulqdq.512
    x86_rdfsbase_32,                              // llvm.x86.rdfsbase.32
    x86_rdfsbase_64,                              // llvm.x86.rdfsbase.64
    x86_rdgsbase_32,                              // llvm.x86.rdgsbase.32
    x86_rdgsbase_64,                              // llvm.x86.rdgsbase.64
    x86_rdpkru,                                   // llvm.x86.rdpkru
    x86_rdpmc,                                    // llvm.x86.rdpmc
    x86_rdrand_16,                                // llvm.x86.rdrand.16
    x86_rdrand_32,                                // llvm.x86.rdrand.32
    x86_rdrand_64,                                // llvm.x86.rdrand.64
    x86_rdseed_16,                                // llvm.x86.rdseed.16
    x86_rdseed_32,                                // llvm.x86.rdseed.32
    x86_rdseed_64,                                // llvm.x86.rdseed.64
    x86_rdsspd,                                   // llvm.x86.rdsspd
    x86_rdsspq,                                   // llvm.x86.rdsspq
    x86_rdtsc,                                    // llvm.x86.rdtsc
    x86_rdtscp,                                   // llvm.x86.rdtscp
    x86_rstorssp,                                 // llvm.x86.rstorssp
    x86_saveprevssp,                              // llvm.x86.saveprevssp
    x86_seh_ehguard,                              // llvm.x86.seh.ehguard
    x86_seh_ehregnode,                            // llvm.x86.seh.ehregnode
    x86_seh_lsda,                                 // llvm.x86.seh.lsda
    x86_seh_recoverfp,                            // llvm.x86.seh.recoverfp
    x86_setssbsy,                                 // llvm.x86.setssbsy
    x86_sha1msg1,                                 // llvm.x86.sha1msg1
    x86_sha1msg2,                                 // llvm.x86.sha1msg2
    x86_sha1nexte,                                // llvm.x86.sha1nexte
    x86_sha1rnds4,                                // llvm.x86.sha1rnds4
    x86_sha256msg1,                               // llvm.x86.sha256msg1
    x86_sha256msg2,                               // llvm.x86.sha256msg2
    x86_sha256rnds2,                              // llvm.x86.sha256rnds2
    x86_slwpcb,                                   // llvm.x86.slwpcb
    x86_sse_cmp_ps,                               // llvm.x86.sse.cmp.ps
    x86_sse_cmp_ss,                               // llvm.x86.sse.cmp.ss
    x86_sse_comieq_ss,                            // llvm.x86.sse.comieq.ss
    x86_sse_comige_ss,                            // llvm.x86.sse.comige.ss
    x86_sse_comigt_ss,                            // llvm.x86.sse.comigt.ss
    x86_sse_comile_ss,                            // llvm.x86.sse.comile.ss
    x86_sse_comilt_ss,                            // llvm.x86.sse.comilt.ss
    x86_sse_comineq_ss,                           // llvm.x86.sse.comineq.ss
    x86_sse_cvtpd2pi,                             // llvm.x86.sse.cvtpd2pi
    x86_sse_cvtpi2pd,                             // llvm.x86.sse.cvtpi2pd
    x86_sse_cvtpi2ps,                             // llvm.x86.sse.cvtpi2ps
    x86_sse_cvtps2pi,                             // llvm.x86.sse.cvtps2pi
    x86_sse_cvtsi2ss,                             // llvm.x86.sse.cvtsi2ss
    x86_sse_cvtsi642ss,                           // llvm.x86.sse.cvtsi642ss
    x86_sse_cvtss2si,                             // llvm.x86.sse.cvtss2si
    x86_sse_cvtss2si64,                           // llvm.x86.sse.cvtss2si64
    x86_sse_cvttpd2pi,                            // llvm.x86.sse.cvttpd2pi
    x86_sse_cvttps2pi,                            // llvm.x86.sse.cvttps2pi
    x86_sse_cvttss2si,                            // llvm.x86.sse.cvttss2si
    x86_sse_cvttss2si64,                          // llvm.x86.sse.cvttss2si64
    x86_sse_ldmxcsr,                              // llvm.x86.sse.ldmxcsr
    x86_sse_max_ps,                               // llvm.x86.sse.max.ps
    x86_sse_max_ss,                               // llvm.x86.sse.max.ss
    x86_sse_min_ps,                               // llvm.x86.sse.min.ps
    x86_sse_min_ss,                               // llvm.x86.sse.min.ss
    x86_sse_movmsk_ps,                            // llvm.x86.sse.movmsk.ps
    x86_sse_pshuf_w,                              // llvm.x86.sse.pshuf.w
    x86_sse_rcp_ps,                               // llvm.x86.sse.rcp.ps
    x86_sse_rcp_ss,                               // llvm.x86.sse.rcp.ss
    x86_sse_rsqrt_ps,                             // llvm.x86.sse.rsqrt.ps
    x86_sse_rsqrt_ss,                             // llvm.x86.sse.rsqrt.ss
    x86_sse_sfence,                               // llvm.x86.sse.sfence
    x86_sse_sqrt_ps,                              // llvm.x86.sse.sqrt.ps
    x86_sse_sqrt_ss,                              // llvm.x86.sse.sqrt.ss
    x86_sse_stmxcsr,                              // llvm.x86.sse.stmxcsr
    x86_sse_ucomieq_ss,                           // llvm.x86.sse.ucomieq.ss
    x86_sse_ucomige_ss,                           // llvm.x86.sse.ucomige.ss
    x86_sse_ucomigt_ss,                           // llvm.x86.sse.ucomigt.ss
    x86_sse_ucomile_ss,                           // llvm.x86.sse.ucomile.ss
    x86_sse_ucomilt_ss,                           // llvm.x86.sse.ucomilt.ss
    x86_sse_ucomineq_ss,                          // llvm.x86.sse.ucomineq.ss
    x86_sse2_clflush,                             // llvm.x86.sse2.clflush
    x86_sse2_cmp_pd,                              // llvm.x86.sse2.cmp.pd
    x86_sse2_cmp_sd,                              // llvm.x86.sse2.cmp.sd
    x86_sse2_comieq_sd,                           // llvm.x86.sse2.comieq.sd
    x86_sse2_comige_sd,                           // llvm.x86.sse2.comige.sd
    x86_sse2_comigt_sd,                           // llvm.x86.sse2.comigt.sd
    x86_sse2_comile_sd,                           // llvm.x86.sse2.comile.sd
    x86_sse2_comilt_sd,                           // llvm.x86.sse2.comilt.sd
    x86_sse2_comineq_sd,                          // llvm.x86.sse2.comineq.sd
    x86_sse2_cvtdq2ps,                            // llvm.x86.sse2.cvtdq2ps
    x86_sse2_cvtpd2dq,                            // llvm.x86.sse2.cvtpd2dq
    x86_sse2_cvtpd2ps,                            // llvm.x86.sse2.cvtpd2ps
    x86_sse2_cvtps2dq,                            // llvm.x86.sse2.cvtps2dq
    x86_sse2_cvtsd2si,                            // llvm.x86.sse2.cvtsd2si
    x86_sse2_cvtsd2si64,                          // llvm.x86.sse2.cvtsd2si64
    x86_sse2_cvtsd2ss,                            // llvm.x86.sse2.cvtsd2ss
    x86_sse2_cvtsi2sd,                            // llvm.x86.sse2.cvtsi2sd
    x86_sse2_cvtsi642sd,                          // llvm.x86.sse2.cvtsi642sd
    x86_sse2_cvtss2sd,                            // llvm.x86.sse2.cvtss2sd
    x86_sse2_cvttpd2dq,                           // llvm.x86.sse2.cvttpd2dq
    x86_sse2_cvttps2dq,                           // llvm.x86.sse2.cvttps2dq
    x86_sse2_cvttsd2si,                           // llvm.x86.sse2.cvttsd2si
    x86_sse2_cvttsd2si64,                         // llvm.x86.sse2.cvttsd2si64
    x86_sse2_lfence,                              // llvm.x86.sse2.lfence
    x86_sse2_maskmov_dqu,                         // llvm.x86.sse2.maskmov.dqu
    x86_sse2_max_pd,                              // llvm.x86.sse2.max.pd
    x86_sse2_max_sd,                              // llvm.x86.sse2.max.sd
    x86_sse2_mfence,                              // llvm.x86.sse2.mfence
    x86_sse2_min_pd,                              // llvm.x86.sse2.min.pd
    x86_sse2_min_sd,                              // llvm.x86.sse2.min.sd
    x86_sse2_movmsk_pd,                           // llvm.x86.sse2.movmsk.pd
    x86_sse2_packssdw_128,                        // llvm.x86.sse2.packssdw.128
    x86_sse2_packsswb_128,                        // llvm.x86.sse2.packsswb.128
    x86_sse2_packuswb_128,                        // llvm.x86.sse2.packuswb.128
    x86_sse2_padds_b,                             // llvm.x86.sse2.padds.b
    x86_sse2_padds_w,                             // llvm.x86.sse2.padds.w
    x86_sse2_paddus_b,                            // llvm.x86.sse2.paddus.b
    x86_sse2_paddus_w,                            // llvm.x86.sse2.paddus.w
    x86_sse2_pause,                               // llvm.x86.sse2.pause
    x86_sse2_pmadd_wd,                            // llvm.x86.sse2.pmadd.wd
    x86_sse2_pmovmskb_128,                        // llvm.x86.sse2.pmovmskb.128
    x86_sse2_pmulh_w,                             // llvm.x86.sse2.pmulh.w
    x86_sse2_pmulhu_w,                            // llvm.x86.sse2.pmulhu.w
    x86_sse2_pmulu_dq,                            // llvm.x86.sse2.pmulu.dq
    x86_sse2_psad_bw,                             // llvm.x86.sse2.psad.bw
    x86_sse2_psll_d,                              // llvm.x86.sse2.psll.d
    x86_sse2_psll_q,                              // llvm.x86.sse2.psll.q
    x86_sse2_psll_w,                              // llvm.x86.sse2.psll.w
    x86_sse2_pslli_d,                             // llvm.x86.sse2.pslli.d
    x86_sse2_pslli_q,                             // llvm.x86.sse2.pslli.q
    x86_sse2_pslli_w,                             // llvm.x86.sse2.pslli.w
    x86_sse2_psra_d,                              // llvm.x86.sse2.psra.d
    x86_sse2_psra_w,                              // llvm.x86.sse2.psra.w
    x86_sse2_psrai_d,                             // llvm.x86.sse2.psrai.d
    x86_sse2_psrai_w,                             // llvm.x86.sse2.psrai.w
    x86_sse2_psrl_d,                              // llvm.x86.sse2.psrl.d
    x86_sse2_psrl_q,                              // llvm.x86.sse2.psrl.q
    x86_sse2_psrl_w,                              // llvm.x86.sse2.psrl.w
    x86_sse2_psrli_d,                             // llvm.x86.sse2.psrli.d
    x86_sse2_psrli_q,                             // llvm.x86.sse2.psrli.q
    x86_sse2_psrli_w,                             // llvm.x86.sse2.psrli.w
    x86_sse2_psubs_b,                             // llvm.x86.sse2.psubs.b
    x86_sse2_psubs_w,                             // llvm.x86.sse2.psubs.w
    x86_sse2_psubus_b,                            // llvm.x86.sse2.psubus.b
    x86_sse2_psubus_w,                            // llvm.x86.sse2.psubus.w
    x86_sse2_sqrt_pd,                             // llvm.x86.sse2.sqrt.pd
    x86_sse2_sqrt_sd,                             // llvm.x86.sse2.sqrt.sd
    x86_sse2_ucomieq_sd,                          // llvm.x86.sse2.ucomieq.sd
    x86_sse2_ucomige_sd,                          // llvm.x86.sse2.ucomige.sd
    x86_sse2_ucomigt_sd,                          // llvm.x86.sse2.ucomigt.sd
    x86_sse2_ucomile_sd,                          // llvm.x86.sse2.ucomile.sd
    x86_sse2_ucomilt_sd,                          // llvm.x86.sse2.ucomilt.sd
    x86_sse2_ucomineq_sd,                         // llvm.x86.sse2.ucomineq.sd
    x86_sse3_addsub_pd,                           // llvm.x86.sse3.addsub.pd
    x86_sse3_addsub_ps,                           // llvm.x86.sse3.addsub.ps
    x86_sse3_hadd_pd,                             // llvm.x86.sse3.hadd.pd
    x86_sse3_hadd_ps,                             // llvm.x86.sse3.hadd.ps
    x86_sse3_hsub_pd,                             // llvm.x86.sse3.hsub.pd
    x86_sse3_hsub_ps,                             // llvm.x86.sse3.hsub.ps
    x86_sse3_ldu_dq,                              // llvm.x86.sse3.ldu.dq
    x86_sse3_monitor,                             // llvm.x86.sse3.monitor
    x86_sse3_mwait,                               // llvm.x86.sse3.mwait
    x86_sse41_blendvpd,                           // llvm.x86.sse41.blendvpd
    x86_sse41_blendvps,                           // llvm.x86.sse41.blendvps
    x86_sse41_dppd,                               // llvm.x86.sse41.dppd
    x86_sse41_dpps,                               // llvm.x86.sse41.dpps
    x86_sse41_insertps,                           // llvm.x86.sse41.insertps
    x86_sse41_mpsadbw,                            // llvm.x86.sse41.mpsadbw
    x86_sse41_packusdw,                           // llvm.x86.sse41.packusdw
    x86_sse41_pblendvb,                           // llvm.x86.sse41.pblendvb
    x86_sse41_phminposuw,                         // llvm.x86.sse41.phminposuw
    x86_sse41_pmuldq,                             // llvm.x86.sse41.pmuldq
    x86_sse41_ptestc,                             // llvm.x86.sse41.ptestc
    x86_sse41_ptestnzc,                           // llvm.x86.sse41.ptestnzc
    x86_sse41_ptestz,                             // llvm.x86.sse41.ptestz
    x86_sse41_round_pd,                           // llvm.x86.sse41.round.pd
    x86_sse41_round_ps,                           // llvm.x86.sse41.round.ps
    x86_sse41_round_sd,                           // llvm.x86.sse41.round.sd
    x86_sse41_round_ss,                           // llvm.x86.sse41.round.ss
    x86_sse42_crc32_32_16,                        // llvm.x86.sse42.crc32.32.16
    x86_sse42_crc32_32_32,                        // llvm.x86.sse42.crc32.32.32
    x86_sse42_crc32_32_8,                         // llvm.x86.sse42.crc32.32.8
    x86_sse42_crc32_64_64,                        // llvm.x86.sse42.crc32.64.64
    x86_sse42_pcmpestri128,                       // llvm.x86.sse42.pcmpestri128
    x86_sse42_pcmpestria128,                      // llvm.x86.sse42.pcmpestria128
    x86_sse42_pcmpestric128,                      // llvm.x86.sse42.pcmpestric128
    x86_sse42_pcmpestrio128,                      // llvm.x86.sse42.pcmpestrio128
    x86_sse42_pcmpestris128,                      // llvm.x86.sse42.pcmpestris128
    x86_sse42_pcmpestriz128,                      // llvm.x86.sse42.pcmpestriz128
    x86_sse42_pcmpestrm128,                       // llvm.x86.sse42.pcmpestrm128
    x86_sse42_pcmpistri128,                       // llvm.x86.sse42.pcmpistri128
    x86_sse42_pcmpistria128,                      // llvm.x86.sse42.pcmpistria128
    x86_sse42_pcmpistric128,                      // llvm.x86.sse42.pcmpistric128
    x86_sse42_pcmpistrio128,                      // llvm.x86.sse42.pcmpistrio128
    x86_sse42_pcmpistris128,                      // llvm.x86.sse42.pcmpistris128
    x86_sse42_pcmpistriz128,                      // llvm.x86.sse42.pcmpistriz128
    x86_sse42_pcmpistrm128,                       // llvm.x86.sse42.pcmpistrm128
    x86_sse4a_extrq,                              // llvm.x86.sse4a.extrq
    x86_sse4a_extrqi,                             // llvm.x86.sse4a.extrqi
    x86_sse4a_insertq,                            // llvm.x86.sse4a.insertq
    x86_sse4a_insertqi,                           // llvm.x86.sse4a.insertqi
    x86_ssse3_pabs_b,                             // llvm.x86.ssse3.pabs.b
    x86_ssse3_pabs_d,                             // llvm.x86.ssse3.pabs.d
    x86_ssse3_pabs_w,                             // llvm.x86.ssse3.pabs.w
    x86_ssse3_phadd_d,                            // llvm.x86.ssse3.phadd.d
    x86_ssse3_phadd_d_128,                        // llvm.x86.ssse3.phadd.d.128
    x86_ssse3_phadd_sw,                           // llvm.x86.ssse3.phadd.sw
    x86_ssse3_phadd_sw_128,                       // llvm.x86.ssse3.phadd.sw.128
    x86_ssse3_phadd_w,                            // llvm.x86.ssse3.phadd.w
    x86_ssse3_phadd_w_128,                        // llvm.x86.ssse3.phadd.w.128
    x86_ssse3_phsub_d,                            // llvm.x86.ssse3.phsub.d
    x86_ssse3_phsub_d_128,                        // llvm.x86.ssse3.phsub.d.128
    x86_ssse3_phsub_sw,                           // llvm.x86.ssse3.phsub.sw
    x86_ssse3_phsub_sw_128,                       // llvm.x86.ssse3.phsub.sw.128
    x86_ssse3_phsub_w,                            // llvm.x86.ssse3.phsub.w
    x86_ssse3_phsub_w_128,                        // llvm.x86.ssse3.phsub.w.128
    x86_ssse3_pmadd_ub_sw,                        // llvm.x86.ssse3.pmadd.ub.sw
    x86_ssse3_pmadd_ub_sw_128,                    // llvm.x86.ssse3.pmadd.ub.sw.128
    x86_ssse3_pmul_hr_sw,                         // llvm.x86.ssse3.pmul.hr.sw
    x86_ssse3_pmul_hr_sw_128,                     // llvm.x86.ssse3.pmul.hr.sw.128
    x86_ssse3_pshuf_b,                            // llvm.x86.ssse3.pshuf.b
    x86_ssse3_pshuf_b_128,                        // llvm.x86.ssse3.pshuf.b.128
    x86_ssse3_psign_b,                            // llvm.x86.ssse3.psign.b
    x86_ssse3_psign_b_128,                        // llvm.x86.ssse3.psign.b.128
    x86_ssse3_psign_d,                            // llvm.x86.ssse3.psign.d
    x86_ssse3_psign_d_128,                        // llvm.x86.ssse3.psign.d.128
    x86_ssse3_psign_w,                            // llvm.x86.ssse3.psign.w
    x86_ssse3_psign_w_128,                        // llvm.x86.ssse3.psign.w.128
    x86_subborrow_u32,                            // llvm.x86.subborrow.u32
    x86_subborrow_u64,                            // llvm.x86.subborrow.u64
    x86_tbm_bextri_u32,                           // llvm.x86.tbm.bextri.u32
    x86_tbm_bextri_u64,                           // llvm.x86.tbm.bextri.u64
    x86_vcvtph2ps_128,                            // llvm.x86.vcvtph2ps.128
    x86_vcvtph2ps_256,                            // llvm.x86.vcvtph2ps.256
    x86_vcvtps2ph_128,                            // llvm.x86.vcvtps2ph.128
    x86_vcvtps2ph_256,                            // llvm.x86.vcvtps2ph.256
    x86_vgf2p8affineinvqb_128,                    // llvm.x86.vgf2p8affineinvqb.128
    x86_vgf2p8affineinvqb_256,                    // llvm.x86.vgf2p8affineinvqb.256
    x86_vgf2p8affineinvqb_512,                    // llvm.x86.vgf2p8affineinvqb.512
    x86_vgf2p8affineqb_128,                       // llvm.x86.vgf2p8affineqb.128
    x86_vgf2p8affineqb_256,                       // llvm.x86.vgf2p8affineqb.256
    x86_vgf2p8affineqb_512,                       // llvm.x86.vgf2p8affineqb.512
    x86_vgf2p8mulb_128,                           // llvm.x86.vgf2p8mulb.128
    x86_vgf2p8mulb_256,                           // llvm.x86.vgf2p8mulb.256
    x86_vgf2p8mulb_512,                           // llvm.x86.vgf2p8mulb.512
    x86_wrfsbase_32,                              // llvm.x86.wrfsbase.32
    x86_wrfsbase_64,                              // llvm.x86.wrfsbase.64
    x86_wrgsbase_32,                              // llvm.x86.wrgsbase.32
    x86_wrgsbase_64,                              // llvm.x86.wrgsbase.64
    x86_wrpkru,                                   // llvm.x86.wrpkru
    x86_wrssd,                                    // llvm.x86.wrssd
    x86_wrssq,                                    // llvm.x86.wrssq
    x86_wrussd,                                   // llvm.x86.wrussd
    x86_wrussq,                                   // llvm.x86.wrussq
    x86_xabort,                                   // llvm.x86.xabort
    x86_xbegin,                                   // llvm.x86.xbegin
    x86_xend,                                     // llvm.x86.xend
    x86_xgetbv,                                   // llvm.x86.xgetbv
    x86_xop_vfrcz_pd,                             // llvm.x86.xop.vfrcz.pd
    x86_xop_vfrcz_pd_256,                         // llvm.x86.xop.vfrcz.pd.256
    x86_xop_vfrcz_ps,                             // llvm.x86.xop.vfrcz.ps
    x86_xop_vfrcz_ps_256,                         // llvm.x86.xop.vfrcz.ps.256
    x86_xop_vfrcz_sd,                             // llvm.x86.xop.vfrcz.sd
    x86_xop_vfrcz_ss,                             // llvm.x86.xop.vfrcz.ss
    x86_xop_vpcomb,                               // llvm.x86.xop.vpcomb
    x86_xop_vpcomd,                               // llvm.x86.xop.vpcomd
    x86_xop_vpcomq,                               // llvm.x86.xop.vpcomq
    x86_xop_vpcomub,                              // llvm.x86.xop.vpcomub
    x86_xop_vpcomud,                              // llvm.x86.xop.vpcomud
    x86_xop_vpcomuq,                              // llvm.x86.xop.vpcomuq
    x86_xop_vpcomuw,                              // llvm.x86.xop.vpcomuw
    x86_xop_vpcomw,                               // llvm.x86.xop.vpcomw
    x86_xop_vpermil2pd,                           // llvm.x86.xop.vpermil2pd
    x86_xop_vpermil2pd_256,                       // llvm.x86.xop.vpermil2pd.256
    x86_xop_vpermil2ps,                           // llvm.x86.xop.vpermil2ps
    x86_xop_vpermil2ps_256,                       // llvm.x86.xop.vpermil2ps.256
    x86_xop_vphaddbd,                             // llvm.x86.xop.vphaddbd
    x86_xop_vphaddbq,                             // llvm.x86.xop.vphaddbq
    x86_xop_vphaddbw,                             // llvm.x86.xop.vphaddbw
    x86_xop_vphadddq,                             // llvm.x86.xop.vphadddq
    x86_xop_vphaddubd,                            // llvm.x86.xop.vphaddubd
    x86_xop_vphaddubq,                            // llvm.x86.xop.vphaddubq
    x86_xop_vphaddubw,                            // llvm.x86.xop.vphaddubw
    x86_xop_vphaddudq,                            // llvm.x86.xop.vphaddudq
    x86_xop_vphadduwd,                            // llvm.x86.xop.vphadduwd
    x86_xop_vphadduwq,                            // llvm.x86.xop.vphadduwq
    x86_xop_vphaddwd,                             // llvm.x86.xop.vphaddwd
    x86_xop_vphaddwq,                             // llvm.x86.xop.vphaddwq
    x86_xop_vphsubbw,                             // llvm.x86.xop.vphsubbw
    x86_xop_vphsubdq,                             // llvm.x86.xop.vphsubdq
    x86_xop_vphsubwd,                             // llvm.x86.xop.vphsubwd
    x86_xop_vpmacsdd,                             // llvm.x86.xop.vpmacsdd
    x86_xop_vpmacsdqh,                            // llvm.x86.xop.vpmacsdqh
    x86_xop_vpmacsdql,                            // llvm.x86.xop.vpmacsdql
    x86_xop_vpmacssdd,                            // llvm.x86.xop.vpmacssdd
    x86_xop_vpmacssdqh,                           // llvm.x86.xop.vpmacssdqh
    x86_xop_vpmacssdql,                           // llvm.x86.xop.vpmacssdql
    x86_xop_vpmacsswd,                            // llvm.x86.xop.vpmacsswd
    x86_xop_vpmacssww,                            // llvm.x86.xop.vpmacssww
    x86_xop_vpmacswd,                             // llvm.x86.xop.vpmacswd
    x86_xop_vpmacsww,                             // llvm.x86.xop.vpmacsww
    x86_xop_vpmadcsswd,                           // llvm.x86.xop.vpmadcsswd
    x86_xop_vpmadcswd,                            // llvm.x86.xop.vpmadcswd
    x86_xop_vpperm,                               // llvm.x86.xop.vpperm
    x86_xop_vprotb,                               // llvm.x86.xop.vprotb
    x86_xop_vprotbi,                              // llvm.x86.xop.vprotbi
    x86_xop_vprotd,                               // llvm.x86.xop.vprotd
    x86_xop_vprotdi,                              // llvm.x86.xop.vprotdi
    x86_xop_vprotq,                               // llvm.x86.xop.vprotq
    x86_xop_vprotqi,                              // llvm.x86.xop.vprotqi
    x86_xop_vprotw,                               // llvm.x86.xop.vprotw
    x86_xop_vprotwi,                              // llvm.x86.xop.vprotwi
    x86_xop_vpshab,                               // llvm.x86.xop.vpshab
    x86_xop_vpshad,                               // llvm.x86.xop.vpshad
    x86_xop_vpshaq,                               // llvm.x86.xop.vpshaq
    x86_xop_vpshaw,                               // llvm.x86.xop.vpshaw
    x86_xop_vpshlb,                               // llvm.x86.xop.vpshlb
    x86_xop_vpshld,                               // llvm.x86.xop.vpshld
    x86_xop_vpshlq,                               // llvm.x86.xop.vpshlq
    x86_xop_vpshlw,                               // llvm.x86.xop.vpshlw
    x86_xrstor,                                   // llvm.x86.xrstor
    x86_xrstor64,                                 // llvm.x86.xrstor64
    x86_xrstors,                                  // llvm.x86.xrstors
    x86_xrstors64,                                // llvm.x86.xrstors64
    x86_xsave,                                    // llvm.x86.xsave
    x86_xsave64,                                  // llvm.x86.xsave64
    x86_xsavec,                                   // llvm.x86.xsavec
    x86_xsavec64,                                 // llvm.x86.xsavec64
    x86_xsaveopt,                                 // llvm.x86.xsaveopt
    x86_xsaveopt64,                               // llvm.x86.xsaveopt64
    x86_xsaves,                                   // llvm.x86.xsaves
    x86_xsaves64,                                 // llvm.x86.xsaves64
    x86_xsetbv,                                   // llvm.x86.xsetbv
    x86_xtest,                                    // llvm.x86.xtest
    xcore_bitrev,                                 // llvm.xcore.bitrev
    xcore_checkevent,                             // llvm.xcore.checkevent
    xcore_chkct,                                  // llvm.xcore.chkct
    xcore_clre,                                   // llvm.xcore.clre
    xcore_clrpt,                                  // llvm.xcore.clrpt
    xcore_clrsr,                                  // llvm.xcore.clrsr
    xcore_crc32,                                  // llvm.xcore.crc32
    xcore_crc8,                                   // llvm.xcore.crc8
    xcore_edu,                                    // llvm.xcore.edu
    xcore_eeu,                                    // llvm.xcore.eeu
    xcore_endin,                                  // llvm.xcore.endin
    xcore_freer,                                  // llvm.xcore.freer
    xcore_geted,                                  // llvm.xcore.geted
    xcore_getet,                                  // llvm.xcore.getet
    xcore_getid,                                  // llvm.xcore.getid
    xcore_getps,                                  // llvm.xcore.getps
    xcore_getr,                                   // llvm.xcore.getr
    xcore_getst,                                  // llvm.xcore.getst
    xcore_getts,                                  // llvm.xcore.getts
    xcore_in,                                     // llvm.xcore.in
    xcore_inct,                                   // llvm.xcore.inct
    xcore_initcp,                                 // llvm.xcore.initcp
    xcore_initdp,                                 // llvm.xcore.initdp
    xcore_initlr,                                 // llvm.xcore.initlr
    xcore_initpc,                                 // llvm.xcore.initpc
    xcore_initsp,                                 // llvm.xcore.initsp
    xcore_inshr,                                  // llvm.xcore.inshr
    xcore_int,                                    // llvm.xcore.int
    xcore_mjoin,                                  // llvm.xcore.mjoin
    xcore_msync,                                  // llvm.xcore.msync
    xcore_out,                                    // llvm.xcore.out
    xcore_outct,                                  // llvm.xcore.outct
    xcore_outshr,                                 // llvm.xcore.outshr
    xcore_outt,                                   // llvm.xcore.outt
    xcore_peek,                                   // llvm.xcore.peek
    xcore_setc,                                   // llvm.xcore.setc
    xcore_setclk,                                 // llvm.xcore.setclk
    xcore_setd,                                   // llvm.xcore.setd
    xcore_setev,                                  // llvm.xcore.setev
    xcore_setps,                                  // llvm.xcore.setps
    xcore_setpsc,                                 // llvm.xcore.setpsc
    xcore_setpt,                                  // llvm.xcore.setpt
    xcore_setrdy,                                 // llvm.xcore.setrdy
    xcore_setsr,                                  // llvm.xcore.setsr
    xcore_settw,                                  // llvm.xcore.settw
    xcore_setv,                                   // llvm.xcore.setv
    xcore_sext,                                   // llvm.xcore.sext
    xcore_ssync,                                  // llvm.xcore.ssync
    xcore_syncr,                                  // llvm.xcore.syncr
    xcore_testct,                                 // llvm.xcore.testct
    xcore_testwct,                                // llvm.xcore.testwct
    xcore_waitevent,                              // llvm.xcore.waitevent
    xcore_zext,                                   // llvm.xcore.zext
}

pub const INTRINSIC_NAMES: &[&str] = &[
    // Note that entry #0 is the invalid intrinsic!
    "llvm.addressofreturnaddress",
    "llvm.adjust.trampoline",
    "llvm.annotation",
    "llvm.assume",
    "llvm.bitreverse",
    "llvm.bswap",
    "llvm.canonicalize",
    "llvm.ceil",
    "llvm.clear_cache",
    "llvm.codeview.annotation",
    "llvm.convert.from.fp16",
    "llvm.convert.to.fp16",
    "llvm.copysign",
    "llvm.coro.alloc",
    "llvm.coro.begin",
    "llvm.coro.destroy",
    "llvm.coro.done",
    "llvm.coro.end",
    "llvm.coro.frame",
    "llvm.coro.free",
    "llvm.coro.id",
    "llvm.coro.param",
    "llvm.coro.promise",
    "llvm.coro.resume",
    "llvm.coro.save",
    "llvm.coro.size",
    "llvm.coro.subfn.addr",
    "llvm.coro.suspend",
    "llvm.cos",
    "llvm.ctlz",
    "llvm.ctpop",
    "llvm.cttz",
    "llvm.dbg.addr",
    "llvm.dbg.declare",
    "llvm.dbg.value",
    "llvm.debugtrap",
    "llvm.donothing",
    "llvm.eh.dwarf.cfa",
    "llvm.eh.exceptioncode",
    "llvm.eh.exceptionpointer",
    "llvm.eh.return.i32",
    "llvm.eh.return.i64",
    "llvm.eh.sjlj.callsite",
    "llvm.eh.sjlj.functioncontext",
    "llvm.eh.sjlj.longjmp",
    "llvm.eh.sjlj.lsda",
    "llvm.eh.sjlj.setjmp",
    "llvm.eh.sjlj.setup.dispatch",
    "llvm.eh.typeid.for",
    "llvm.eh.unwind.init",
    "llvm.exp",
    "llvm.exp2",
    "llvm.expect",
    "llvm.experimental.constrained.cos",
    "llvm.experimental.constrained.exp",
    "llvm.experimental.constrained.exp2",
    "llvm.experimental.constrained.fadd",
    "llvm.experimental.constrained.fdiv",
    "llvm.experimental.constrained.fma",
    "llvm.experimental.constrained.fmul",
    "llvm.experimental.constrained.frem",
    "llvm.experimental.constrained.fsub",
    "llvm.experimental.constrained.log",
    "llvm.experimental.constrained.log10",
    "llvm.experimental.constrained.log2",
    "llvm.experimental.constrained.nearbyint",
    "llvm.experimental.constrained.pow",
    "llvm.experimental.constrained.powi",
    "llvm.experimental.constrained.rint",
    "llvm.experimental.constrained.sin",
    "llvm.experimental.constrained.sqrt",
    "llvm.experimental.deoptimize",
    "llvm.experimental.gc.relocate",
    "llvm.experimental.gc.result",
    "llvm.experimental.gc.statepoint",
    "llvm.experimental.guard",
    "llvm.experimental.patchpoint.i64",
    "llvm.experimental.patchpoint.void",
    "llvm.experimental.stackmap",
    "llvm.experimental.vector.reduce.add",
    "llvm.experimental.vector.reduce.and",
    "llvm.experimental.vector.reduce.fadd",
    "llvm.experimental.vector.reduce.fmax",
    "llvm.experimental.vector.reduce.fmin",
    "llvm.experimental.vector.reduce.fmul",
    "llvm.experimental.vector.reduce.mul",
    "llvm.experimental.vector.reduce.or",
    "llvm.experimental.vector.reduce.smax",
    "llvm.experimental.vector.reduce.smin",
    "llvm.experimental.vector.reduce.umax",
    "llvm.experimental.vector.reduce.umin",
    "llvm.experimental.vector.reduce.xor",
    "llvm.fabs",
    "llvm.floor",
    "llvm.flt.rounds",
    "llvm.fma",
    "llvm.fmuladd",
    "llvm.frameaddress",
    "llvm.gcread",
    "llvm.gcroot",
    "llvm.gcwrite",
    "llvm.get.dynamic.area.offset",
    "llvm.init.trampoline",
    "llvm.instrprof.increment",
    "llvm.instrprof.increment.step",
    "llvm.instrprof.value.profile",
    "llvm.invariant.end",
    "llvm.invariant.group.barrier",
    "llvm.invariant.start",
    "llvm.lifetime.end",
    "llvm.lifetime.start",
    "llvm.load.relative",
    "llvm.localaddress",
    "llvm.localescape",
    "llvm.localrecover",
    "llvm.log",
    "llvm.log10",
    "llvm.log2",
    "llvm.longjmp",
    "llvm.masked.compressstore",
    "llvm.masked.expandload",
    "llvm.masked.gather",
    "llvm.masked.load",
    "llvm.masked.scatter",
    "llvm.masked.store",
    "llvm.maxnum",
    "llvm.memcpy",
    "llvm.memcpy.element.unordered.atomic",
    "llvm.memmove",
    "llvm.memmove.element.unordered.atomic",
    "llvm.memset",
    "llvm.memset.element.unordered.atomic",
    "llvm.minnum",
    "llvm.nearbyint",
    "llvm.objectsize",
    "llvm.pcmarker",
    "llvm.pow",
    "llvm.powi",
    "llvm.prefetch",
    "llvm.ptr.annotation",
    "llvm.read_register",
    "llvm.readcyclecounter",
    "llvm.returnaddress",
    "llvm.rint",
    "llvm.round",
    "llvm.sadd.with.overflow",
    "llvm.setjmp",
    "llvm.sideeffect",
    "llvm.siglongjmp",
    "llvm.sigsetjmp",
    "llvm.sin",
    "llvm.smul.with.overflow",
    "llvm.sqrt",
    "llvm.ssa.copy",
    "llvm.ssub.with.overflow",
    "llvm.stackguard",
    "llvm.stackprotector",
    "llvm.stackrestore",
    "llvm.stacksave",
    "llvm.thread.pointer",
    "llvm.trap",
    "llvm.trunc",
    "llvm.type.checked.load",
    "llvm.type.test",
    "llvm.uadd.with.overflow",
    "llvm.umul.with.overflow",
    "llvm.usub.with.overflow",
    "llvm.va_copy",
    "llvm.va_end",
    "llvm.va_start",
    "llvm.var.annotation",
    "llvm.write_register",
    "llvm.xray.customevent",
    "llvm.aarch64.clrex",
    "llvm.aarch64.crc32b",
    "llvm.aarch64.crc32cb",
    "llvm.aarch64.crc32ch",
    "llvm.aarch64.crc32cw",
    "llvm.aarch64.crc32cx",
    "llvm.aarch64.crc32h",
    "llvm.aarch64.crc32w",
    "llvm.aarch64.crc32x",
    "llvm.aarch64.crypto.aesd",
    "llvm.aarch64.crypto.aese",
    "llvm.aarch64.crypto.aesimc",
    "llvm.aarch64.crypto.aesmc",
    "llvm.aarch64.crypto.sha1c",
    "llvm.aarch64.crypto.sha1h",
    "llvm.aarch64.crypto.sha1m",
    "llvm.aarch64.crypto.sha1p",
    "llvm.aarch64.crypto.sha1su0",
    "llvm.aarch64.crypto.sha1su1",
    "llvm.aarch64.crypto.sha256h",
    "llvm.aarch64.crypto.sha256h2",
    "llvm.aarch64.crypto.sha256su0",
    "llvm.aarch64.crypto.sha256su1",
    "llvm.aarch64.dmb",
    "llvm.aarch64.dsb",
    "llvm.aarch64.hint",
    "llvm.aarch64.isb",
    "llvm.aarch64.ldaxp",
    "llvm.aarch64.ldaxr",
    "llvm.aarch64.ldxp",
    "llvm.aarch64.ldxr",
    "llvm.aarch64.neon.abs",
    "llvm.aarch64.neon.addhn",
    "llvm.aarch64.neon.addp",
    "llvm.aarch64.neon.cls",
    "llvm.aarch64.neon.fabd",
    "llvm.aarch64.neon.facge",
    "llvm.aarch64.neon.facgt",
    "llvm.aarch64.neon.faddv",
    "llvm.aarch64.neon.fcvtas",
    "llvm.aarch64.neon.fcvtau",
    "llvm.aarch64.neon.fcvtms",
    "llvm.aarch64.neon.fcvtmu",
    "llvm.aarch64.neon.fcvtns",
    "llvm.aarch64.neon.fcvtnu",
    "llvm.aarch64.neon.fcvtps",
    "llvm.aarch64.neon.fcvtpu",
    "llvm.aarch64.neon.fcvtxn",
    "llvm.aarch64.neon.fcvtzs",
    "llvm.aarch64.neon.fcvtzu",
    "llvm.aarch64.neon.fmax",
    "llvm.aarch64.neon.fmaxnm",
    "llvm.aarch64.neon.fmaxnmp",
    "llvm.aarch64.neon.fmaxnmv",
    "llvm.aarch64.neon.fmaxp",
    "llvm.aarch64.neon.fmaxv",
    "llvm.aarch64.neon.fmin",
    "llvm.aarch64.neon.fminnm",
    "llvm.aarch64.neon.fminnmp",
    "llvm.aarch64.neon.fminnmv",
    "llvm.aarch64.neon.fminp",
    "llvm.aarch64.neon.fminv",
    "llvm.aarch64.neon.fmulx",
    "llvm.aarch64.neon.frecpe",
    "llvm.aarch64.neon.frecps",
    "llvm.aarch64.neon.frecpx",
    "llvm.aarch64.neon.frintn",
    "llvm.aarch64.neon.frsqrte",
    "llvm.aarch64.neon.frsqrts",
    "llvm.aarch64.neon.ld1x2",
    "llvm.aarch64.neon.ld1x3",
    "llvm.aarch64.neon.ld1x4",
    "llvm.aarch64.neon.ld2",
    "llvm.aarch64.neon.ld2lane",
    "llvm.aarch64.neon.ld2r",
    "llvm.aarch64.neon.ld3",
    "llvm.aarch64.neon.ld3lane",
    "llvm.aarch64.neon.ld3r",
    "llvm.aarch64.neon.ld4",
    "llvm.aarch64.neon.ld4lane",
    "llvm.aarch64.neon.ld4r",
    "llvm.aarch64.neon.pmul",
    "llvm.aarch64.neon.pmull",
    "llvm.aarch64.neon.pmull64",
    "llvm.aarch64.neon.raddhn",
    "llvm.aarch64.neon.rbit",
    "llvm.aarch64.neon.rshrn",
    "llvm.aarch64.neon.rsubhn",
    "llvm.aarch64.neon.sabd",
    "llvm.aarch64.neon.saddlp",
    "llvm.aarch64.neon.saddlv",
    "llvm.aarch64.neon.saddv",
    "llvm.aarch64.neon.scalar.sqxtn",
    "llvm.aarch64.neon.scalar.sqxtun",
    "llvm.aarch64.neon.scalar.uqxtn",
    "llvm.aarch64.neon.shadd",
    "llvm.aarch64.neon.shll",
    "llvm.aarch64.neon.shsub",
    "llvm.aarch64.neon.smax",
    "llvm.aarch64.neon.smaxp",
    "llvm.aarch64.neon.smaxv",
    "llvm.aarch64.neon.smin",
    "llvm.aarch64.neon.sminp",
    "llvm.aarch64.neon.sminv",
    "llvm.aarch64.neon.smull",
    "llvm.aarch64.neon.sqabs",
    "llvm.aarch64.neon.sqadd",
    "llvm.aarch64.neon.sqdmulh",
    "llvm.aarch64.neon.sqdmull",
    "llvm.aarch64.neon.sqdmulls.scalar",
    "llvm.aarch64.neon.sqneg",
    "llvm.aarch64.neon.sqrdmulh",
    "llvm.aarch64.neon.sqrshl",
    "llvm.aarch64.neon.sqrshrn",
    "llvm.aarch64.neon.sqrshrun",
    "llvm.aarch64.neon.sqshl",
    "llvm.aarch64.neon.sqshlu",
    "llvm.aarch64.neon.sqshrn",
    "llvm.aarch64.neon.sqshrun",
    "llvm.aarch64.neon.sqsub",
    "llvm.aarch64.neon.sqxtn",
    "llvm.aarch64.neon.sqxtun",
    "llvm.aarch64.neon.srhadd",
    "llvm.aarch64.neon.srshl",
    "llvm.aarch64.neon.sshl",
    "llvm.aarch64.neon.sshll",
    "llvm.aarch64.neon.st1x2",
    "llvm.aarch64.neon.st1x3",
    "llvm.aarch64.neon.st1x4",
    "llvm.aarch64.neon.st2",
    "llvm.aarch64.neon.st2lane",
    "llvm.aarch64.neon.st3",
    "llvm.aarch64.neon.st3lane",
    "llvm.aarch64.neon.st4",
    "llvm.aarch64.neon.st4lane",
    "llvm.aarch64.neon.subhn",
    "llvm.aarch64.neon.suqadd",
    "llvm.aarch64.neon.tbl1",
    "llvm.aarch64.neon.tbl2",
    "llvm.aarch64.neon.tbl3",
    "llvm.aarch64.neon.tbl4",
    "llvm.aarch64.neon.tbx1",
    "llvm.aarch64.neon.tbx2",
    "llvm.aarch64.neon.tbx3",
    "llvm.aarch64.neon.tbx4",
    "llvm.aarch64.neon.uabd",
    "llvm.aarch64.neon.uaddlp",
    "llvm.aarch64.neon.uaddlv",
    "llvm.aarch64.neon.uaddv",
    "llvm.aarch64.neon.uhadd",
    "llvm.aarch64.neon.uhsub",
    "llvm.aarch64.neon.umax",
    "llvm.aarch64.neon.umaxp",
    "llvm.aarch64.neon.umaxv",
    "llvm.aarch64.neon.umin",
    "llvm.aarch64.neon.uminp",
    "llvm.aarch64.neon.uminv",
    "llvm.aarch64.neon.umull",
    "llvm.aarch64.neon.uqadd",
    "llvm.aarch64.neon.uqrshl",
    "llvm.aarch64.neon.uqrshrn",
    "llvm.aarch64.neon.uqshl",
    "llvm.aarch64.neon.uqshrn",
    "llvm.aarch64.neon.uqsub",
    "llvm.aarch64.neon.uqxtn",
    "llvm.aarch64.neon.urecpe",
    "llvm.aarch64.neon.urhadd",
    "llvm.aarch64.neon.urshl",
    "llvm.aarch64.neon.ursqrte",
    "llvm.aarch64.neon.ushl",
    "llvm.aarch64.neon.ushll",
    "llvm.aarch64.neon.usqadd",
    "llvm.aarch64.neon.vcopy.lane",
    "llvm.aarch64.neon.vcvtfp2fxs",
    "llvm.aarch64.neon.vcvtfp2fxu",
    "llvm.aarch64.neon.vcvtfp2hf",
    "llvm.aarch64.neon.vcvtfxs2fp",
    "llvm.aarch64.neon.vcvtfxu2fp",
    "llvm.aarch64.neon.vcvthf2fp",
    "llvm.aarch64.neon.vsli",
    "llvm.aarch64.neon.vsri",
    "llvm.aarch64.sdiv",
    "llvm.aarch64.sisd.fabd",
    "llvm.aarch64.sisd.fcvtxn",
    "llvm.aarch64.stlxp",
    "llvm.aarch64.stlxr",
    "llvm.aarch64.stxp",
    "llvm.aarch64.stxr",
    "llvm.aarch64.udiv",
    "llvm.amdgcn.alignbit",
    "llvm.amdgcn.alignbyte",
    "llvm.amdgcn.atomic.dec",
    "llvm.amdgcn.atomic.inc",
    "llvm.amdgcn.break",
    "llvm.amdgcn.buffer.atomic.add",
    "llvm.amdgcn.buffer.atomic.and",
    "llvm.amdgcn.buffer.atomic.cmpswap",
    "llvm.amdgcn.buffer.atomic.or",
    "llvm.amdgcn.buffer.atomic.smax",
    "llvm.amdgcn.buffer.atomic.smin",
    "llvm.amdgcn.buffer.atomic.sub",
    "llvm.amdgcn.buffer.atomic.swap",
    "llvm.amdgcn.buffer.atomic.umax",
    "llvm.amdgcn.buffer.atomic.umin",
    "llvm.amdgcn.buffer.atomic.xor",
    "llvm.amdgcn.buffer.load",
    "llvm.amdgcn.buffer.load.format",
    "llvm.amdgcn.buffer.store",
    "llvm.amdgcn.buffer.store.format",
    "llvm.amdgcn.buffer.wbinvl1",
    "llvm.amdgcn.buffer.wbinvl1.sc",
    "llvm.amdgcn.buffer.wbinvl1.vol",
    "llvm.amdgcn.class",
    "llvm.amdgcn.cos",
    "llvm.amdgcn.cubeid",
    "llvm.amdgcn.cubema",
    "llvm.amdgcn.cubesc",
    "llvm.amdgcn.cubetc",
    "llvm.amdgcn.cvt.pk.i16",
    "llvm.amdgcn.cvt.pk.u16",
    "llvm.amdgcn.cvt.pk.u8.f32",
    "llvm.amdgcn.cvt.pknorm.i16",
    "llvm.amdgcn.cvt.pknorm.u16",
    "llvm.amdgcn.cvt.pkrtz",
    "llvm.amdgcn.dispatch.id",
    "llvm.amdgcn.dispatch.ptr",
    "llvm.amdgcn.div.fixup",
    "llvm.amdgcn.div.fmas",
    "llvm.amdgcn.div.scale",
    "llvm.amdgcn.ds.bpermute",
    "llvm.amdgcn.ds.permute",
    "llvm.amdgcn.ds.swizzle",
    "llvm.amdgcn.else",
    "llvm.amdgcn.else.break",
    "llvm.amdgcn.end.cf",
    "llvm.amdgcn.exp",
    "llvm.amdgcn.exp.compr",
    "llvm.amdgcn.fcmp",
    "llvm.amdgcn.fdiv.fast",
    "llvm.amdgcn.fmed3",
    "llvm.amdgcn.fmul.legacy",
    "llvm.amdgcn.fract",
    "llvm.amdgcn.frexp.exp",
    "llvm.amdgcn.frexp.mant",
    "llvm.amdgcn.groupstaticsize",
    "llvm.amdgcn.icmp",
    "llvm.amdgcn.if",
    "llvm.amdgcn.if.break",
    "llvm.amdgcn.image.atomic.add",
    "llvm.amdgcn.image.atomic.and",
    "llvm.amdgcn.image.atomic.cmpswap",
    "llvm.amdgcn.image.atomic.dec",
    "llvm.amdgcn.image.atomic.inc",
    "llvm.amdgcn.image.atomic.or",
    "llvm.amdgcn.image.atomic.smax",
    "llvm.amdgcn.image.atomic.smin",
    "llvm.amdgcn.image.atomic.sub",
    "llvm.amdgcn.image.atomic.swap",
    "llvm.amdgcn.image.atomic.umax",
    "llvm.amdgcn.image.atomic.umin",
    "llvm.amdgcn.image.atomic.xor",
    "llvm.amdgcn.image.gather4",
    "llvm.amdgcn.image.gather4.b",
    "llvm.amdgcn.image.gather4.b.cl",
    "llvm.amdgcn.image.gather4.b.cl.o",
    "llvm.amdgcn.image.gather4.b.o",
    "llvm.amdgcn.image.gather4.c",
    "llvm.amdgcn.image.gather4.c.b",
    "llvm.amdgcn.image.gather4.c.b.cl",
    "llvm.amdgcn.image.gather4.c.b.cl.o",
    "llvm.amdgcn.image.gather4.c.b.o",
    "llvm.amdgcn.image.gather4.c.cl",
    "llvm.amdgcn.image.gather4.c.cl.o",
    "llvm.amdgcn.image.gather4.c.l",
    "llvm.amdgcn.image.gather4.c.l.o",
    "llvm.amdgcn.image.gather4.c.lz",
    "llvm.amdgcn.image.gather4.c.lz.o",
    "llvm.amdgcn.image.gather4.c.o",
    "llvm.amdgcn.image.gather4.cl",
    "llvm.amdgcn.image.gather4.cl.o",
    "llvm.amdgcn.image.gather4.l",
    "llvm.amdgcn.image.gather4.l.o",
    "llvm.amdgcn.image.gather4.lz",
    "llvm.amdgcn.image.gather4.lz.o",
    "llvm.amdgcn.image.gather4.o",
    "llvm.amdgcn.image.getlod",
    "llvm.amdgcn.image.getresinfo",
    "llvm.amdgcn.image.load",
    "llvm.amdgcn.image.load.mip",
    "llvm.amdgcn.image.sample",
    "llvm.amdgcn.image.sample.b",
    "llvm.amdgcn.image.sample.b.cl",
    "llvm.amdgcn.image.sample.b.cl.o",
    "llvm.amdgcn.image.sample.b.o",
    "llvm.amdgcn.image.sample.c",
    "llvm.amdgcn.image.sample.c.b",
    "llvm.amdgcn.image.sample.c.b.cl",
    "llvm.amdgcn.image.sample.c.b.cl.o",
    "llvm.amdgcn.image.sample.c.b.o",
    "llvm.amdgcn.image.sample.c.cd",
    "llvm.amdgcn.image.sample.c.cd.cl",
    "llvm.amdgcn.image.sample.c.cd.cl.o",
    "llvm.amdgcn.image.sample.c.cd.o",
    "llvm.amdgcn.image.sample.c.cl",
    "llvm.amdgcn.image.sample.c.cl.o",
    "llvm.amdgcn.image.sample.c.d",
    "llvm.amdgcn.image.sample.c.d.cl",
    "llvm.amdgcn.image.sample.c.d.cl.o",
    "llvm.amdgcn.image.sample.c.d.o",
    "llvm.amdgcn.image.sample.c.l",
    "llvm.amdgcn.image.sample.c.l.o",
    "llvm.amdgcn.image.sample.c.lz",
    "llvm.amdgcn.image.sample.c.lz.o",
    "llvm.amdgcn.image.sample.c.o",
    "llvm.amdgcn.image.sample.cd",
    "llvm.amdgcn.image.sample.cd.cl",
    "llvm.amdgcn.image.sample.cd.cl.o",
    "llvm.amdgcn.image.sample.cd.o",
    "llvm.amdgcn.image.sample.cl",
    "llvm.amdgcn.image.sample.cl.o",
    "llvm.amdgcn.image.sample.d",
    "llvm.amdgcn.image.sample.d.cl",
    "llvm.amdgcn.image.sample.d.cl.o",
    "llvm.amdgcn.image.sample.d.o",
    "llvm.amdgcn.image.sample.l",
    "llvm.amdgcn.image.sample.l.o",
    "llvm.amdgcn.image.sample.lz",
    "llvm.amdgcn.image.sample.lz.o",
    "llvm.amdgcn.image.sample.o",
    "llvm.amdgcn.image.store",
    "llvm.amdgcn.image.store.mip",
    "llvm.amdgcn.implicit.buffer.ptr",
    "llvm.amdgcn.implicitarg.ptr",
    "llvm.amdgcn.init.exec",
    "llvm.amdgcn.init.exec.from.input",
    "llvm.amdgcn.interp.mov",
    "llvm.amdgcn.interp.p1",
    "llvm.amdgcn.interp.p2",
    "llvm.amdgcn.kernarg.segment.ptr",
    "llvm.amdgcn.kill",
    "llvm.amdgcn.ldexp",
    "llvm.amdgcn.lerp",
    "llvm.amdgcn.log.clamp",
    "llvm.amdgcn.loop",
    "llvm.amdgcn.mbcnt.hi",
    "llvm.amdgcn.mbcnt.lo",
    "llvm.amdgcn.mov.dpp",
    "llvm.amdgcn.mqsad.pk.u16.u8",
    "llvm.amdgcn.mqsad.u32.u8",
    "llvm.amdgcn.msad.u8",
    "llvm.amdgcn.ps.live",
    "llvm.amdgcn.qsad.pk.u16.u8",
    "llvm.amdgcn.queue.ptr",
    "llvm.amdgcn.rcp",
    "llvm.amdgcn.rcp.legacy",
    "llvm.amdgcn.readfirstlane",
    "llvm.amdgcn.readlane",
    "llvm.amdgcn.rsq",
    "llvm.amdgcn.rsq.clamp",
    "llvm.amdgcn.rsq.legacy",
    "llvm.amdgcn.s.barrier",
    "llvm.amdgcn.s.dcache.inv",
    "llvm.amdgcn.s.dcache.inv.vol",
    "llvm.amdgcn.s.dcache.wb",
    "llvm.amdgcn.s.dcache.wb.vol",
    "llvm.amdgcn.s.decperflevel",
    "llvm.amdgcn.s.getpc",
    "llvm.amdgcn.s.getreg",
    "llvm.amdgcn.s.incperflevel",
    "llvm.amdgcn.s.memrealtime",
    "llvm.amdgcn.s.memtime",
    "llvm.amdgcn.s.sendmsg",
    "llvm.amdgcn.s.sendmsghalt",
    "llvm.amdgcn.s.sleep",
    "llvm.amdgcn.s.waitcnt",
    "llvm.amdgcn.sad.hi.u8",
    "llvm.amdgcn.sad.u16",
    "llvm.amdgcn.sad.u8",
    "llvm.amdgcn.sbfe",
    "llvm.amdgcn.set.inactive",
    "llvm.amdgcn.sffbh",
    "llvm.amdgcn.sin",
    "llvm.amdgcn.tbuffer.load",
    "llvm.amdgcn.tbuffer.store",
    "llvm.amdgcn.trig.preop",
    "llvm.amdgcn.ubfe",
    "llvm.amdgcn.unreachable",
    "llvm.amdgcn.update.dpp",
    "llvm.amdgcn.wave.barrier",
    "llvm.amdgcn.workgroup.id.x",
    "llvm.amdgcn.workgroup.id.y",
    "llvm.amdgcn.workgroup.id.z",
    "llvm.amdgcn.workitem.id.x",
    "llvm.amdgcn.workitem.id.y",
    "llvm.amdgcn.workitem.id.z",
    "llvm.amdgcn.wqm",
    "llvm.amdgcn.wqm.vote",
    "llvm.amdgcn.wwm",
    "llvm.arm.cdp",
    "llvm.arm.cdp2",
    "llvm.arm.clrex",
    "llvm.arm.crc32b",
    "llvm.arm.crc32cb",
    "llvm.arm.crc32ch",
    "llvm.arm.crc32cw",
    "llvm.arm.crc32h",
    "llvm.arm.crc32w",
    "llvm.arm.dbg",
    "llvm.arm.dmb",
    "llvm.arm.dsb",
    "llvm.arm.get.fpscr",
    "llvm.arm.hint",
    "llvm.arm.isb",
    "llvm.arm.ldaex",
    "llvm.arm.ldaexd",
    "llvm.arm.ldc",
    "llvm.arm.ldc2",
    "llvm.arm.ldc2l",
    "llvm.arm.ldcl",
    "llvm.arm.ldrex",
    "llvm.arm.ldrexd",
    "llvm.arm.mcr",
    "llvm.arm.mcr2",
    "llvm.arm.mcrr",
    "llvm.arm.mcrr2",
    "llvm.arm.mrc",
    "llvm.arm.mrc2",
    "llvm.arm.mrrc",
    "llvm.arm.mrrc2",
    "llvm.arm.neon.aesd",
    "llvm.arm.neon.aese",
    "llvm.arm.neon.aesimc",
    "llvm.arm.neon.aesmc",
    "llvm.arm.neon.sha1c",
    "llvm.arm.neon.sha1h",
    "llvm.arm.neon.sha1m",
    "llvm.arm.neon.sha1p",
    "llvm.arm.neon.sha1su0",
    "llvm.arm.neon.sha1su1",
    "llvm.arm.neon.sha256h",
    "llvm.arm.neon.sha256h2",
    "llvm.arm.neon.sha256su0",
    "llvm.arm.neon.sha256su1",
    "llvm.arm.neon.vabds",
    "llvm.arm.neon.vabdu",
    "llvm.arm.neon.vabs",
    "llvm.arm.neon.vacge",
    "llvm.arm.neon.vacgt",
    "llvm.arm.neon.vbsl",
    "llvm.arm.neon.vcls",
    "llvm.arm.neon.vcvtas",
    "llvm.arm.neon.vcvtau",
    "llvm.arm.neon.vcvtfp2fxs",
    "llvm.arm.neon.vcvtfp2fxu",
    "llvm.arm.neon.vcvtfp2hf",
    "llvm.arm.neon.vcvtfxs2fp",
    "llvm.arm.neon.vcvtfxu2fp",
    "llvm.arm.neon.vcvthf2fp",
    "llvm.arm.neon.vcvtms",
    "llvm.arm.neon.vcvtmu",
    "llvm.arm.neon.vcvtns",
    "llvm.arm.neon.vcvtnu",
    "llvm.arm.neon.vcvtps",
    "llvm.arm.neon.vcvtpu",
    "llvm.arm.neon.vhadds",
    "llvm.arm.neon.vhaddu",
    "llvm.arm.neon.vhsubs",
    "llvm.arm.neon.vhsubu",
    "llvm.arm.neon.vld1",
    "llvm.arm.neon.vld2",
    "llvm.arm.neon.vld2lane",
    "llvm.arm.neon.vld3",
    "llvm.arm.neon.vld3lane",
    "llvm.arm.neon.vld4",
    "llvm.arm.neon.vld4lane",
    "llvm.arm.neon.vmaxnm",
    "llvm.arm.neon.vmaxs",
    "llvm.arm.neon.vmaxu",
    "llvm.arm.neon.vminnm",
    "llvm.arm.neon.vmins",
    "llvm.arm.neon.vminu",
    "llvm.arm.neon.vmullp",
    "llvm.arm.neon.vmulls",
    "llvm.arm.neon.vmullu",
    "llvm.arm.neon.vmulp",
    "llvm.arm.neon.vpadals",
    "llvm.arm.neon.vpadalu",
    "llvm.arm.neon.vpadd",
    "llvm.arm.neon.vpaddls",
    "llvm.arm.neon.vpaddlu",
    "llvm.arm.neon.vpmaxs",
    "llvm.arm.neon.vpmaxu",
    "llvm.arm.neon.vpmins",
    "llvm.arm.neon.vpminu",
    "llvm.arm.neon.vqabs",
    "llvm.arm.neon.vqadds",
    "llvm.arm.neon.vqaddu",
    "llvm.arm.neon.vqdmulh",
    "llvm.arm.neon.vqdmull",
    "llvm.arm.neon.vqmovns",
    "llvm.arm.neon.vqmovnsu",
    "llvm.arm.neon.vqmovnu",
    "llvm.arm.neon.vqneg",
    "llvm.arm.neon.vqrdmulh",
    "llvm.arm.neon.vqrshiftns",
    "llvm.arm.neon.vqrshiftnsu",
    "llvm.arm.neon.vqrshiftnu",
    "llvm.arm.neon.vqrshifts",
    "llvm.arm.neon.vqrshiftu",
    "llvm.arm.neon.vqshiftns",
    "llvm.arm.neon.vqshiftnsu",
    "llvm.arm.neon.vqshiftnu",
    "llvm.arm.neon.vqshifts",
    "llvm.arm.neon.vqshiftsu",
    "llvm.arm.neon.vqshiftu",
    "llvm.arm.neon.vqsubs",
    "llvm.arm.neon.vqsubu",
    "llvm.arm.neon.vraddhn",
    "llvm.arm.neon.vrecpe",
    "llvm.arm.neon.vrecps",
    "llvm.arm.neon.vrhadds",
    "llvm.arm.neon.vrhaddu",
    "llvm.arm.neon.vrinta",
    "llvm.arm.neon.vrintm",
    "llvm.arm.neon.vrintn",
    "llvm.arm.neon.vrintp",
    "llvm.arm.neon.vrintx",
    "llvm.arm.neon.vrintz",
    "llvm.arm.neon.vrshiftn",
    "llvm.arm.neon.vrshifts",
    "llvm.arm.neon.vrshiftu",
    "llvm.arm.neon.vrsqrte",
    "llvm.arm.neon.vrsqrts",
    "llvm.arm.neon.vrsubhn",
    "llvm.arm.neon.vshiftins",
    "llvm.arm.neon.vshifts",
    "llvm.arm.neon.vshiftu",
    "llvm.arm.neon.vst1",
    "llvm.arm.neon.vst2",
    "llvm.arm.neon.vst2lane",
    "llvm.arm.neon.vst3",
    "llvm.arm.neon.vst3lane",
    "llvm.arm.neon.vst4",
    "llvm.arm.neon.vst4lane",
    "llvm.arm.neon.vtbl1",
    "llvm.arm.neon.vtbl2",
    "llvm.arm.neon.vtbl3",
    "llvm.arm.neon.vtbl4",
    "llvm.arm.neon.vtbx1",
    "llvm.arm.neon.vtbx2",
    "llvm.arm.neon.vtbx3",
    "llvm.arm.neon.vtbx4",
    "llvm.arm.qadd",
    "llvm.arm.qadd16",
    "llvm.arm.qadd8",
    "llvm.arm.qasx",
    "llvm.arm.qsax",
    "llvm.arm.qsub",
    "llvm.arm.qsub16",
    "llvm.arm.qsub8",
    "llvm.arm.sadd16",
    "llvm.arm.sadd8",
    "llvm.arm.sasx",
    "llvm.arm.sel",
    "llvm.arm.set.fpscr",
    "llvm.arm.shadd16",
    "llvm.arm.shadd8",
    "llvm.arm.shasx",
    "llvm.arm.shsax",
    "llvm.arm.shsub16",
    "llvm.arm.shsub8",
    "llvm.arm.smlabb",
    "llvm.arm.smlabt",
    "llvm.arm.smlad",
    "llvm.arm.smladx",
    "llvm.arm.smlald",
    "llvm.arm.smlaldx",
    "llvm.arm.smlatb",
    "llvm.arm.smlatt",
    "llvm.arm.smlawb",
    "llvm.arm.smlawt",
    "llvm.arm.smlsd",
    "llvm.arm.smlsdx",
    "llvm.arm.smlsld",
    "llvm.arm.smlsldx",
    "llvm.arm.smuad",
    "llvm.arm.smuadx",
    "llvm.arm.smulbb",
    "llvm.arm.smulbt",
    "llvm.arm.smultb",
    "llvm.arm.smultt",
    "llvm.arm.smulwb",
    "llvm.arm.smulwt",
    "llvm.arm.smusd",
    "llvm.arm.smusdx",
    "llvm.arm.space",
    "llvm.arm.ssat",
    "llvm.arm.ssat16",
    "llvm.arm.ssax",
    "llvm.arm.ssub16",
    "llvm.arm.ssub8",
    "llvm.arm.stc",
    "llvm.arm.stc2",
    "llvm.arm.stc2l",
    "llvm.arm.stcl",
    "llvm.arm.stlex",
    "llvm.arm.stlexd",
    "llvm.arm.strex",
    "llvm.arm.strexd",
    "llvm.arm.sxtab16",
    "llvm.arm.sxtb16",
    "llvm.arm.uadd16",
    "llvm.arm.uadd8",
    "llvm.arm.uasx",
    "llvm.arm.uhadd16",
    "llvm.arm.uhadd8",
    "llvm.arm.uhasx",
    "llvm.arm.uhsax",
    "llvm.arm.uhsub16",
    "llvm.arm.uhsub8",
    "llvm.arm.undefined",
    "llvm.arm.uqadd16",
    "llvm.arm.uqadd8",
    "llvm.arm.uqasx",
    "llvm.arm.uqsax",
    "llvm.arm.uqsub16",
    "llvm.arm.uqsub8",
    "llvm.arm.usad8",
    "llvm.arm.usada8",
    "llvm.arm.usat",
    "llvm.arm.usat16",
    "llvm.arm.usax",
    "llvm.arm.usub16",
    "llvm.arm.usub8",
    "llvm.arm.uxtab16",
    "llvm.arm.uxtb16",
    "llvm.arm.vcvtr",
    "llvm.arm.vcvtru",
    "llvm.bpf.load.byte",
    "llvm.bpf.load.half",
    "llvm.bpf.load.word",
    "llvm.bpf.pseudo",
    "llvm.hexagon.A2.abs",
    "llvm.hexagon.A2.absp",
    "llvm.hexagon.A2.abssat",
    "llvm.hexagon.A2.add",
    "llvm.hexagon.A2.addh.h16.hh",
    "llvm.hexagon.A2.addh.h16.hl",
    "llvm.hexagon.A2.addh.h16.lh",
    "llvm.hexagon.A2.addh.h16.ll",
    "llvm.hexagon.A2.addh.h16.sat.hh",
    "llvm.hexagon.A2.addh.h16.sat.hl",
    "llvm.hexagon.A2.addh.h16.sat.lh",
    "llvm.hexagon.A2.addh.h16.sat.ll",
    "llvm.hexagon.A2.addh.l16.hl",
    "llvm.hexagon.A2.addh.l16.ll",
    "llvm.hexagon.A2.addh.l16.sat.hl",
    "llvm.hexagon.A2.addh.l16.sat.ll",
    "llvm.hexagon.A2.addi",
    "llvm.hexagon.A2.addp",
    "llvm.hexagon.A2.addpsat",
    "llvm.hexagon.A2.addsat",
    "llvm.hexagon.A2.addsp",
    "llvm.hexagon.A2.and",
    "llvm.hexagon.A2.andir",
    "llvm.hexagon.A2.andp",
    "llvm.hexagon.A2.aslh",
    "llvm.hexagon.A2.asrh",
    "llvm.hexagon.A2.combine.hh",
    "llvm.hexagon.A2.combine.hl",
    "llvm.hexagon.A2.combine.lh",
    "llvm.hexagon.A2.combine.ll",
    "llvm.hexagon.A2.combineii",
    "llvm.hexagon.A2.combinew",
    "llvm.hexagon.A2.max",
    "llvm.hexagon.A2.maxp",
    "llvm.hexagon.A2.maxu",
    "llvm.hexagon.A2.maxup",
    "llvm.hexagon.A2.min",
    "llvm.hexagon.A2.minp",
    "llvm.hexagon.A2.minu",
    "llvm.hexagon.A2.minup",
    "llvm.hexagon.A2.neg",
    "llvm.hexagon.A2.negp",
    "llvm.hexagon.A2.negsat",
    "llvm.hexagon.A2.not",
    "llvm.hexagon.A2.notp",
    "llvm.hexagon.A2.or",
    "llvm.hexagon.A2.orir",
    "llvm.hexagon.A2.orp",
    "llvm.hexagon.A2.roundsat",
    "llvm.hexagon.A2.sat",
    "llvm.hexagon.A2.satb",
    "llvm.hexagon.A2.sath",
    "llvm.hexagon.A2.satub",
    "llvm.hexagon.A2.satuh",
    "llvm.hexagon.A2.sub",
    "llvm.hexagon.A2.subh.h16.hh",
    "llvm.hexagon.A2.subh.h16.hl",
    "llvm.hexagon.A2.subh.h16.lh",
    "llvm.hexagon.A2.subh.h16.ll",
    "llvm.hexagon.A2.subh.h16.sat.hh",
    "llvm.hexagon.A2.subh.h16.sat.hl",
    "llvm.hexagon.A2.subh.h16.sat.lh",
    "llvm.hexagon.A2.subh.h16.sat.ll",
    "llvm.hexagon.A2.subh.l16.hl",
    "llvm.hexagon.A2.subh.l16.ll",
    "llvm.hexagon.A2.subh.l16.sat.hl",
    "llvm.hexagon.A2.subh.l16.sat.ll",
    "llvm.hexagon.A2.subp",
    "llvm.hexagon.A2.subri",
    "llvm.hexagon.A2.subsat",
    "llvm.hexagon.A2.svaddh",
    "llvm.hexagon.A2.svaddhs",
    "llvm.hexagon.A2.svadduhs",
    "llvm.hexagon.A2.svavgh",
    "llvm.hexagon.A2.svavghs",
    "llvm.hexagon.A2.svnavgh",
    "llvm.hexagon.A2.svsubh",
    "llvm.hexagon.A2.svsubhs",
    "llvm.hexagon.A2.svsubuhs",
    "llvm.hexagon.A2.swiz",
    "llvm.hexagon.A2.sxtb",
    "llvm.hexagon.A2.sxth",
    "llvm.hexagon.A2.sxtw",
    "llvm.hexagon.A2.tfr",
    "llvm.hexagon.A2.tfrih",
    "llvm.hexagon.A2.tfril",
    "llvm.hexagon.A2.tfrp",
    "llvm.hexagon.A2.tfrpi",
    "llvm.hexagon.A2.tfrsi",
    "llvm.hexagon.A2.vabsh",
    "llvm.hexagon.A2.vabshsat",
    "llvm.hexagon.A2.vabsw",
    "llvm.hexagon.A2.vabswsat",
    "llvm.hexagon.A2.vaddb.map",
    "llvm.hexagon.A2.vaddh",
    "llvm.hexagon.A2.vaddhs",
    "llvm.hexagon.A2.vaddub",
    "llvm.hexagon.A2.vaddubs",
    "llvm.hexagon.A2.vadduhs",
    "llvm.hexagon.A2.vaddw",
    "llvm.hexagon.A2.vaddws",
    "llvm.hexagon.A2.vavgh",
    "llvm.hexagon.A2.vavghcr",
    "llvm.hexagon.A2.vavghr",
    "llvm.hexagon.A2.vavgub",
    "llvm.hexagon.A2.vavgubr",
    "llvm.hexagon.A2.vavguh",
    "llvm.hexagon.A2.vavguhr",
    "llvm.hexagon.A2.vavguw",
    "llvm.hexagon.A2.vavguwr",
    "llvm.hexagon.A2.vavgw",
    "llvm.hexagon.A2.vavgwcr",
    "llvm.hexagon.A2.vavgwr",
    "llvm.hexagon.A2.vcmpbeq",
    "llvm.hexagon.A2.vcmpbgtu",
    "llvm.hexagon.A2.vcmpheq",
    "llvm.hexagon.A2.vcmphgt",
    "llvm.hexagon.A2.vcmphgtu",
    "llvm.hexagon.A2.vcmpweq",
    "llvm.hexagon.A2.vcmpwgt",
    "llvm.hexagon.A2.vcmpwgtu",
    "llvm.hexagon.A2.vconj",
    "llvm.hexagon.A2.vmaxb",
    "llvm.hexagon.A2.vmaxh",
    "llvm.hexagon.A2.vmaxub",
    "llvm.hexagon.A2.vmaxuh",
    "llvm.hexagon.A2.vmaxuw",
    "llvm.hexagon.A2.vmaxw",
    "llvm.hexagon.A2.vminb",
    "llvm.hexagon.A2.vminh",
    "llvm.hexagon.A2.vminub",
    "llvm.hexagon.A2.vminuh",
    "llvm.hexagon.A2.vminuw",
    "llvm.hexagon.A2.vminw",
    "llvm.hexagon.A2.vnavgh",
    "llvm.hexagon.A2.vnavghcr",
    "llvm.hexagon.A2.vnavghr",
    "llvm.hexagon.A2.vnavgw",
    "llvm.hexagon.A2.vnavgwcr",
    "llvm.hexagon.A2.vnavgwr",
    "llvm.hexagon.A2.vraddub",
    "llvm.hexagon.A2.vraddub.acc",
    "llvm.hexagon.A2.vrsadub",
    "llvm.hexagon.A2.vrsadub.acc",
    "llvm.hexagon.A2.vsubb.map",
    "llvm.hexagon.A2.vsubh",
    "llvm.hexagon.A2.vsubhs",
    "llvm.hexagon.A2.vsubub",
    "llvm.hexagon.A2.vsububs",
    "llvm.hexagon.A2.vsubuhs",
    "llvm.hexagon.A2.vsubw",
    "llvm.hexagon.A2.vsubws",
    "llvm.hexagon.A2.xor",
    "llvm.hexagon.A2.xorp",
    "llvm.hexagon.A2.zxtb",
    "llvm.hexagon.A2.zxth",
    "llvm.hexagon.A4.andn",
    "llvm.hexagon.A4.andnp",
    "llvm.hexagon.A4.bitsplit",
    "llvm.hexagon.A4.bitspliti",
    "llvm.hexagon.A4.boundscheck",
    "llvm.hexagon.A4.cmpbeq",
    "llvm.hexagon.A4.cmpbeqi",
    "llvm.hexagon.A4.cmpbgt",
    "llvm.hexagon.A4.cmpbgti",
    "llvm.hexagon.A4.cmpbgtu",
    "llvm.hexagon.A4.cmpbgtui",
    "llvm.hexagon.A4.cmpheq",
    "llvm.hexagon.A4.cmpheqi",
    "llvm.hexagon.A4.cmphgt",
    "llvm.hexagon.A4.cmphgti",
    "llvm.hexagon.A4.cmphgtu",
    "llvm.hexagon.A4.cmphgtui",
    "llvm.hexagon.A4.combineir",
    "llvm.hexagon.A4.combineri",
    "llvm.hexagon.A4.cround.ri",
    "llvm.hexagon.A4.cround.rr",
    "llvm.hexagon.A4.modwrapu",
    "llvm.hexagon.A4.orn",
    "llvm.hexagon.A4.ornp",
    "llvm.hexagon.A4.rcmpeq",
    "llvm.hexagon.A4.rcmpeqi",
    "llvm.hexagon.A4.rcmpneq",
    "llvm.hexagon.A4.rcmpneqi",
    "llvm.hexagon.A4.round.ri",
    "llvm.hexagon.A4.round.ri.sat",
    "llvm.hexagon.A4.round.rr",
    "llvm.hexagon.A4.round.rr.sat",
    "llvm.hexagon.A4.tlbmatch",
    "llvm.hexagon.A4.vcmpbeq.any",
    "llvm.hexagon.A4.vcmpbeqi",
    "llvm.hexagon.A4.vcmpbgt",
    "llvm.hexagon.A4.vcmpbgti",
    "llvm.hexagon.A4.vcmpbgtui",
    "llvm.hexagon.A4.vcmpheqi",
    "llvm.hexagon.A4.vcmphgti",
    "llvm.hexagon.A4.vcmphgtui",
    "llvm.hexagon.A4.vcmpweqi",
    "llvm.hexagon.A4.vcmpwgti",
    "llvm.hexagon.A4.vcmpwgtui",
    "llvm.hexagon.A4.vrmaxh",
    "llvm.hexagon.A4.vrmaxuh",
    "llvm.hexagon.A4.vrmaxuw",
    "llvm.hexagon.A4.vrmaxw",
    "llvm.hexagon.A4.vrminh",
    "llvm.hexagon.A4.vrminuh",
    "llvm.hexagon.A4.vrminuw",
    "llvm.hexagon.A4.vrminw",
    "llvm.hexagon.A5.vaddhubs",
    "llvm.hexagon.A6.vcmpbeq.notany",
    "llvm.hexagon.A6.vcmpbeq.notany.128B",
    "llvm.hexagon.C2.all8",
    "llvm.hexagon.C2.and",
    "llvm.hexagon.C2.andn",
    "llvm.hexagon.C2.any8",
    "llvm.hexagon.C2.bitsclr",
    "llvm.hexagon.C2.bitsclri",
    "llvm.hexagon.C2.bitsset",
    "llvm.hexagon.C2.cmpeq",
    "llvm.hexagon.C2.cmpeqi",
    "llvm.hexagon.C2.cmpeqp",
    "llvm.hexagon.C2.cmpgei",
    "llvm.hexagon.C2.cmpgeui",
    "llvm.hexagon.C2.cmpgt",
    "llvm.hexagon.C2.cmpgti",
    "llvm.hexagon.C2.cmpgtp",
    "llvm.hexagon.C2.cmpgtu",
    "llvm.hexagon.C2.cmpgtui",
    "llvm.hexagon.C2.cmpgtup",
    "llvm.hexagon.C2.cmplt",
    "llvm.hexagon.C2.cmpltu",
    "llvm.hexagon.C2.mask",
    "llvm.hexagon.C2.mux",
    "llvm.hexagon.C2.muxii",
    "llvm.hexagon.C2.muxir",
    "llvm.hexagon.C2.muxri",
    "llvm.hexagon.C2.not",
    "llvm.hexagon.C2.or",
    "llvm.hexagon.C2.orn",
    "llvm.hexagon.C2.pxfer.map",
    "llvm.hexagon.C2.tfrpr",
    "llvm.hexagon.C2.tfrrp",
    "llvm.hexagon.C2.vitpack",
    "llvm.hexagon.C2.vmux",
    "llvm.hexagon.C2.xor",
    "llvm.hexagon.C4.and.and",
    "llvm.hexagon.C4.and.andn",
    "llvm.hexagon.C4.and.or",
    "llvm.hexagon.C4.and.orn",
    "llvm.hexagon.C4.cmplte",
    "llvm.hexagon.C4.cmpltei",
    "llvm.hexagon.C4.cmplteu",
    "llvm.hexagon.C4.cmplteui",
    "llvm.hexagon.C4.cmpneq",
    "llvm.hexagon.C4.cmpneqi",
    "llvm.hexagon.C4.fastcorner9",
    "llvm.hexagon.C4.fastcorner9.not",
    "llvm.hexagon.C4.nbitsclr",
    "llvm.hexagon.C4.nbitsclri",
    "llvm.hexagon.C4.nbitsset",
    "llvm.hexagon.C4.or.and",
    "llvm.hexagon.C4.or.andn",
    "llvm.hexagon.C4.or.or",
    "llvm.hexagon.C4.or.orn",
    "llvm.hexagon.F2.conv.d2df",
    "llvm.hexagon.F2.conv.d2sf",
    "llvm.hexagon.F2.conv.df2d",
    "llvm.hexagon.F2.conv.df2d.chop",
    "llvm.hexagon.F2.conv.df2sf",
    "llvm.hexagon.F2.conv.df2ud",
    "llvm.hexagon.F2.conv.df2ud.chop",
    "llvm.hexagon.F2.conv.df2uw",
    "llvm.hexagon.F2.conv.df2uw.chop",
    "llvm.hexagon.F2.conv.df2w",
    "llvm.hexagon.F2.conv.df2w.chop",
    "llvm.hexagon.F2.conv.sf2d",
    "llvm.hexagon.F2.conv.sf2d.chop",
    "llvm.hexagon.F2.conv.sf2df",
    "llvm.hexagon.F2.conv.sf2ud",
    "llvm.hexagon.F2.conv.sf2ud.chop",
    "llvm.hexagon.F2.conv.sf2uw",
    "llvm.hexagon.F2.conv.sf2uw.chop",
    "llvm.hexagon.F2.conv.sf2w",
    "llvm.hexagon.F2.conv.sf2w.chop",
    "llvm.hexagon.F2.conv.ud2df",
    "llvm.hexagon.F2.conv.ud2sf",
    "llvm.hexagon.F2.conv.uw2df",
    "llvm.hexagon.F2.conv.uw2sf",
    "llvm.hexagon.F2.conv.w2df",
    "llvm.hexagon.F2.conv.w2sf",
    "llvm.hexagon.F2.dfclass",
    "llvm.hexagon.F2.dfcmpeq",
    "llvm.hexagon.F2.dfcmpge",
    "llvm.hexagon.F2.dfcmpgt",
    "llvm.hexagon.F2.dfcmpuo",
    "llvm.hexagon.F2.dfimm.n",
    "llvm.hexagon.F2.dfimm.p",
    "llvm.hexagon.F2.sfadd",
    "llvm.hexagon.F2.sfclass",
    "llvm.hexagon.F2.sfcmpeq",
    "llvm.hexagon.F2.sfcmpge",
    "llvm.hexagon.F2.sfcmpgt",
    "llvm.hexagon.F2.sfcmpuo",
    "llvm.hexagon.F2.sffixupd",
    "llvm.hexagon.F2.sffixupn",
    "llvm.hexagon.F2.sffixupr",
    "llvm.hexagon.F2.sffma",
    "llvm.hexagon.F2.sffma.lib",
    "llvm.hexagon.F2.sffma.sc",
    "llvm.hexagon.F2.sffms",
    "llvm.hexagon.F2.sffms.lib",
    "llvm.hexagon.F2.sfimm.n",
    "llvm.hexagon.F2.sfimm.p",
    "llvm.hexagon.F2.sfmax",
    "llvm.hexagon.F2.sfmin",
    "llvm.hexagon.F2.sfmpy",
    "llvm.hexagon.F2.sfsub",
    "llvm.hexagon.L2.loadw.locked",
    "llvm.hexagon.L4.loadd.locked",
    "llvm.hexagon.M2.acci",
    "llvm.hexagon.M2.accii",
    "llvm.hexagon.M2.cmaci.s0",
    "llvm.hexagon.M2.cmacr.s0",
    "llvm.hexagon.M2.cmacs.s0",
    "llvm.hexagon.M2.cmacs.s1",
    "llvm.hexagon.M2.cmacsc.s0",
    "llvm.hexagon.M2.cmacsc.s1",
    "llvm.hexagon.M2.cmpyi.s0",
    "llvm.hexagon.M2.cmpyr.s0",
    "llvm.hexagon.M2.cmpyrs.s0",
    "llvm.hexagon.M2.cmpyrs.s1",
    "llvm.hexagon.M2.cmpyrsc.s0",
    "llvm.hexagon.M2.cmpyrsc.s1",
    "llvm.hexagon.M2.cmpys.s0",
    "llvm.hexagon.M2.cmpys.s1",
    "llvm.hexagon.M2.cmpysc.s0",
    "llvm.hexagon.M2.cmpysc.s1",
    "llvm.hexagon.M2.cnacs.s0",
    "llvm.hexagon.M2.cnacs.s1",
    "llvm.hexagon.M2.cnacsc.s0",
    "llvm.hexagon.M2.cnacsc.s1",
    "llvm.hexagon.M2.dpmpyss.acc.s0",
    "llvm.hexagon.M2.dpmpyss.nac.s0",
    "llvm.hexagon.M2.dpmpyss.rnd.s0",
    "llvm.hexagon.M2.dpmpyss.s0",
    "llvm.hexagon.M2.dpmpyuu.acc.s0",
    "llvm.hexagon.M2.dpmpyuu.nac.s0",
    "llvm.hexagon.M2.dpmpyuu.s0",
    "llvm.hexagon.M2.hmmpyh.rs1",
    "llvm.hexagon.M2.hmmpyh.s1",
    "llvm.hexagon.M2.hmmpyl.rs1",
    "llvm.hexagon.M2.hmmpyl.s1",
    "llvm.hexagon.M2.maci",
    "llvm.hexagon.M2.macsin",
    "llvm.hexagon.M2.macsip",
    "llvm.hexagon.M2.mmachs.rs0",
    "llvm.hexagon.M2.mmachs.rs1",
    "llvm.hexagon.M2.mmachs.s0",
    "llvm.hexagon.M2.mmachs.s1",
    "llvm.hexagon.M2.mmacls.rs0",
    "llvm.hexagon.M2.mmacls.rs1",
    "llvm.hexagon.M2.mmacls.s0",
    "llvm.hexagon.M2.mmacls.s1",
    "llvm.hexagon.M2.mmacuhs.rs0",
    "llvm.hexagon.M2.mmacuhs.rs1",
    "llvm.hexagon.M2.mmacuhs.s0",
    "llvm.hexagon.M2.mmacuhs.s1",
    "llvm.hexagon.M2.mmaculs.rs0",
    "llvm.hexagon.M2.mmaculs.rs1",
    "llvm.hexagon.M2.mmaculs.s0",
    "llvm.hexagon.M2.mmaculs.s1",
    "llvm.hexagon.M2.mmpyh.rs0",
    "llvm.hexagon.M2.mmpyh.rs1",
    "llvm.hexagon.M2.mmpyh.s0",
    "llvm.hexagon.M2.mmpyh.s1",
    "llvm.hexagon.M2.mmpyl.rs0",
    "llvm.hexagon.M2.mmpyl.rs1",
    "llvm.hexagon.M2.mmpyl.s0",
    "llvm.hexagon.M2.mmpyl.s1",
    "llvm.hexagon.M2.mmpyuh.rs0",
    "llvm.hexagon.M2.mmpyuh.rs1",
    "llvm.hexagon.M2.mmpyuh.s0",
    "llvm.hexagon.M2.mmpyuh.s1",
    "llvm.hexagon.M2.mmpyul.rs0",
    "llvm.hexagon.M2.mmpyul.rs1",
    "llvm.hexagon.M2.mmpyul.s0",
    "llvm.hexagon.M2.mmpyul.s1",
    "llvm.hexagon.M2.mpy.acc.hh.s0",
    "llvm.hexagon.M2.mpy.acc.hh.s1",
    "llvm.hexagon.M2.mpy.acc.hl.s0",
    "llvm.hexagon.M2.mpy.acc.hl.s1",
    "llvm.hexagon.M2.mpy.acc.lh.s0",
    "llvm.hexagon.M2.mpy.acc.lh.s1",
    "llvm.hexagon.M2.mpy.acc.ll.s0",
    "llvm.hexagon.M2.mpy.acc.ll.s1",
    "llvm.hexagon.M2.mpy.acc.sat.hh.s0",
    "llvm.hexagon.M2.mpy.acc.sat.hh.s1",
    "llvm.hexagon.M2.mpy.acc.sat.hl.s0",
    "llvm.hexagon.M2.mpy.acc.sat.hl.s1",
    "llvm.hexagon.M2.mpy.acc.sat.lh.s0",
    "llvm.hexagon.M2.mpy.acc.sat.lh.s1",
    "llvm.hexagon.M2.mpy.acc.sat.ll.s0",
    "llvm.hexagon.M2.mpy.acc.sat.ll.s1",
    "llvm.hexagon.M2.mpy.hh.s0",
    "llvm.hexagon.M2.mpy.hh.s1",
    "llvm.hexagon.M2.mpy.hl.s0",
    "llvm.hexagon.M2.mpy.hl.s1",
    "llvm.hexagon.M2.mpy.lh.s0",
    "llvm.hexagon.M2.mpy.lh.s1",
    "llvm.hexagon.M2.mpy.ll.s0",
    "llvm.hexagon.M2.mpy.ll.s1",
    "llvm.hexagon.M2.mpy.nac.hh.s0",
    "llvm.hexagon.M2.mpy.nac.hh.s1",
    "llvm.hexagon.M2.mpy.nac.hl.s0",
    "llvm.hexagon.M2.mpy.nac.hl.s1",
    "llvm.hexagon.M2.mpy.nac.lh.s0",
    "llvm.hexagon.M2.mpy.nac.lh.s1",
    "llvm.hexagon.M2.mpy.nac.ll.s0",
    "llvm.hexagon.M2.mpy.nac.ll.s1",
    "llvm.hexagon.M2.mpy.nac.sat.hh.s0",
    "llvm.hexagon.M2.mpy.nac.sat.hh.s1",
    "llvm.hexagon.M2.mpy.nac.sat.hl.s0",
    "llvm.hexagon.M2.mpy.nac.sat.hl.s1",
    "llvm.hexagon.M2.mpy.nac.sat.lh.s0",
    "llvm.hexagon.M2.mpy.nac.sat.lh.s1",
    "llvm.hexagon.M2.mpy.nac.sat.ll.s0",
    "llvm.hexagon.M2.mpy.nac.sat.ll.s1",
    "llvm.hexagon.M2.mpy.rnd.hh.s0",
    "llvm.hexagon.M2.mpy.rnd.hh.s1",
    "llvm.hexagon.M2.mpy.rnd.hl.s0",
    "llvm.hexagon.M2.mpy.rnd.hl.s1",
    "llvm.hexagon.M2.mpy.rnd.lh.s0",
    "llvm.hexagon.M2.mpy.rnd.lh.s1",
    "llvm.hexagon.M2.mpy.rnd.ll.s0",
    "llvm.hexagon.M2.mpy.rnd.ll.s1",
    "llvm.hexagon.M2.mpy.sat.hh.s0",
    "llvm.hexagon.M2.mpy.sat.hh.s1",
    "llvm.hexagon.M2.mpy.sat.hl.s0",
    "llvm.hexagon.M2.mpy.sat.hl.s1",
    "llvm.hexagon.M2.mpy.sat.lh.s0",
    "llvm.hexagon.M2.mpy.sat.lh.s1",
    "llvm.hexagon.M2.mpy.sat.ll.s0",
    "llvm.hexagon.M2.mpy.sat.ll.s1",
    "llvm.hexagon.M2.mpy.sat.rnd.hh.s0",
    "llvm.hexagon.M2.mpy.sat.rnd.hh.s1",
    "llvm.hexagon.M2.mpy.sat.rnd.hl.s0",
    "llvm.hexagon.M2.mpy.sat.rnd.hl.s1",
    "llvm.hexagon.M2.mpy.sat.rnd.lh.s0",
    "llvm.hexagon.M2.mpy.sat.rnd.lh.s1",
    "llvm.hexagon.M2.mpy.sat.rnd.ll.s0",
    "llvm.hexagon.M2.mpy.sat.rnd.ll.s1",
    "llvm.hexagon.M2.mpy.up",
    "llvm.hexagon.M2.mpy.up.s1",
    "llvm.hexagon.M2.mpy.up.s1.sat",
    "llvm.hexagon.M2.mpyd.acc.hh.s0",
    "llvm.hexagon.M2.mpyd.acc.hh.s1",
    "llvm.hexagon.M2.mpyd.acc.hl.s0",
    "llvm.hexagon.M2.mpyd.acc.hl.s1",
    "llvm.hexagon.M2.mpyd.acc.lh.s0",
    "llvm.hexagon.M2.mpyd.acc.lh.s1",
    "llvm.hexagon.M2.mpyd.acc.ll.s0",
    "llvm.hexagon.M2.mpyd.acc.ll.s1",
    "llvm.hexagon.M2.mpyd.hh.s0",
    "llvm.hexagon.M2.mpyd.hh.s1",
    "llvm.hexagon.M2.mpyd.hl.s0",
    "llvm.hexagon.M2.mpyd.hl.s1",
    "llvm.hexagon.M2.mpyd.lh.s0",
    "llvm.hexagon.M2.mpyd.lh.s1",
    "llvm.hexagon.M2.mpyd.ll.s0",
    "llvm.hexagon.M2.mpyd.ll.s1",
    "llvm.hexagon.M2.mpyd.nac.hh.s0",
    "llvm.hexagon.M2.mpyd.nac.hh.s1",
    "llvm.hexagon.M2.mpyd.nac.hl.s0",
    "llvm.hexagon.M2.mpyd.nac.hl.s1",
    "llvm.hexagon.M2.mpyd.nac.lh.s0",
    "llvm.hexagon.M2.mpyd.nac.lh.s1",
    "llvm.hexagon.M2.mpyd.nac.ll.s0",
    "llvm.hexagon.M2.mpyd.nac.ll.s1",
    "llvm.hexagon.M2.mpyd.rnd.hh.s0",
    "llvm.hexagon.M2.mpyd.rnd.hh.s1",
    "llvm.hexagon.M2.mpyd.rnd.hl.s0",
    "llvm.hexagon.M2.mpyd.rnd.hl.s1",
    "llvm.hexagon.M2.mpyd.rnd.lh.s0",
    "llvm.hexagon.M2.mpyd.rnd.lh.s1",
    "llvm.hexagon.M2.mpyd.rnd.ll.s0",
    "llvm.hexagon.M2.mpyd.rnd.ll.s1",
    "llvm.hexagon.M2.mpyi",
    "llvm.hexagon.M2.mpysmi",
    "llvm.hexagon.M2.mpysu.up",
    "llvm.hexagon.M2.mpyu.acc.hh.s0",
    "llvm.hexagon.M2.mpyu.acc.hh.s1",
    "llvm.hexagon.M2.mpyu.acc.hl.s0",
    "llvm.hexagon.M2.mpyu.acc.hl.s1",
    "llvm.hexagon.M2.mpyu.acc.lh.s0",
    "llvm.hexagon.M2.mpyu.acc.lh.s1",
    "llvm.hexagon.M2.mpyu.acc.ll.s0",
    "llvm.hexagon.M2.mpyu.acc.ll.s1",
    "llvm.hexagon.M2.mpyu.hh.s0",
    "llvm.hexagon.M2.mpyu.hh.s1",
    "llvm.hexagon.M2.mpyu.hl.s0",
    "llvm.hexagon.M2.mpyu.hl.s1",
    "llvm.hexagon.M2.mpyu.lh.s0",
    "llvm.hexagon.M2.mpyu.lh.s1",
    "llvm.hexagon.M2.mpyu.ll.s0",
    "llvm.hexagon.M2.mpyu.ll.s1",
    "llvm.hexagon.M2.mpyu.nac.hh.s0",
    "llvm.hexagon.M2.mpyu.nac.hh.s1",
    "llvm.hexagon.M2.mpyu.nac.hl.s0",
    "llvm.hexagon.M2.mpyu.nac.hl.s1",
    "llvm.hexagon.M2.mpyu.nac.lh.s0",
    "llvm.hexagon.M2.mpyu.nac.lh.s1",
    "llvm.hexagon.M2.mpyu.nac.ll.s0",
    "llvm.hexagon.M2.mpyu.nac.ll.s1",
    "llvm.hexagon.M2.mpyu.up",
    "llvm.hexagon.M2.mpyud.acc.hh.s0",
    "llvm.hexagon.M2.mpyud.acc.hh.s1",
    "llvm.hexagon.M2.mpyud.acc.hl.s0",
    "llvm.hexagon.M2.mpyud.acc.hl.s1",
    "llvm.hexagon.M2.mpyud.acc.lh.s0",
    "llvm.hexagon.M2.mpyud.acc.lh.s1",
    "llvm.hexagon.M2.mpyud.acc.ll.s0",
    "llvm.hexagon.M2.mpyud.acc.ll.s1",
    "llvm.hexagon.M2.mpyud.hh.s0",
    "llvm.hexagon.M2.mpyud.hh.s1",
    "llvm.hexagon.M2.mpyud.hl.s0",
    "llvm.hexagon.M2.mpyud.hl.s1",
    "llvm.hexagon.M2.mpyud.lh.s0",
    "llvm.hexagon.M2.mpyud.lh.s1",
    "llvm.hexagon.M2.mpyud.ll.s0",
    "llvm.hexagon.M2.mpyud.ll.s1",
    "llvm.hexagon.M2.mpyud.nac.hh.s0",
    "llvm.hexagon.M2.mpyud.nac.hh.s1",
    "llvm.hexagon.M2.mpyud.nac.hl.s0",
    "llvm.hexagon.M2.mpyud.nac.hl.s1",
    "llvm.hexagon.M2.mpyud.nac.lh.s0",
    "llvm.hexagon.M2.mpyud.nac.lh.s1",
    "llvm.hexagon.M2.mpyud.nac.ll.s0",
    "llvm.hexagon.M2.mpyud.nac.ll.s1",
    "llvm.hexagon.M2.mpyui",
    "llvm.hexagon.M2.nacci",
    "llvm.hexagon.M2.naccii",
    "llvm.hexagon.M2.subacc",
    "llvm.hexagon.M2.vabsdiffh",
    "llvm.hexagon.M2.vabsdiffw",
    "llvm.hexagon.M2.vcmac.s0.sat.i",
    "llvm.hexagon.M2.vcmac.s0.sat.r",
    "llvm.hexagon.M2.vcmpy.s0.sat.i",
    "llvm.hexagon.M2.vcmpy.s0.sat.r",
    "llvm.hexagon.M2.vcmpy.s1.sat.i",
    "llvm.hexagon.M2.vcmpy.s1.sat.r",
    "llvm.hexagon.M2.vdmacs.s0",
    "llvm.hexagon.M2.vdmacs.s1",
    "llvm.hexagon.M2.vdmpyrs.s0",
    "llvm.hexagon.M2.vdmpyrs.s1",
    "llvm.hexagon.M2.vdmpys.s0",
    "llvm.hexagon.M2.vdmpys.s1",
    "llvm.hexagon.M2.vmac2",
    "llvm.hexagon.M2.vmac2es",
    "llvm.hexagon.M2.vmac2es.s0",
    "llvm.hexagon.M2.vmac2es.s1",
    "llvm.hexagon.M2.vmac2s.s0",
    "llvm.hexagon.M2.vmac2s.s1",
    "llvm.hexagon.M2.vmac2su.s0",
    "llvm.hexagon.M2.vmac2su.s1",
    "llvm.hexagon.M2.vmpy2es.s0",
    "llvm.hexagon.M2.vmpy2es.s1",
    "llvm.hexagon.M2.vmpy2s.s0",
    "llvm.hexagon.M2.vmpy2s.s0pack",
    "llvm.hexagon.M2.vmpy2s.s1",
    "llvm.hexagon.M2.vmpy2s.s1pack",
    "llvm.hexagon.M2.vmpy2su.s0",
    "llvm.hexagon.M2.vmpy2su.s1",
    "llvm.hexagon.M2.vraddh",
    "llvm.hexagon.M2.vradduh",
    "llvm.hexagon.M2.vrcmaci.s0",
    "llvm.hexagon.M2.vrcmaci.s0c",
    "llvm.hexagon.M2.vrcmacr.s0",
    "llvm.hexagon.M2.vrcmacr.s0c",
    "llvm.hexagon.M2.vrcmpyi.s0",
    "llvm.hexagon.M2.vrcmpyi.s0c",
    "llvm.hexagon.M2.vrcmpyr.s0",
    "llvm.hexagon.M2.vrcmpyr.s0c",
    "llvm.hexagon.M2.vrcmpys.acc.s1",
    "llvm.hexagon.M2.vrcmpys.s1",
    "llvm.hexagon.M2.vrcmpys.s1rp",
    "llvm.hexagon.M2.vrmac.s0",
    "llvm.hexagon.M2.vrmpy.s0",
    "llvm.hexagon.M2.xor.xacc",
    "llvm.hexagon.M4.and.and",
    "llvm.hexagon.M4.and.andn",
    "llvm.hexagon.M4.and.or",
    "llvm.hexagon.M4.and.xor",
    "llvm.hexagon.M4.cmpyi.wh",
    "llvm.hexagon.M4.cmpyi.whc",
    "llvm.hexagon.M4.cmpyr.wh",
    "llvm.hexagon.M4.cmpyr.whc",
    "llvm.hexagon.M4.mac.up.s1.sat",
    "llvm.hexagon.M4.mpyri.addi",
    "llvm.hexagon.M4.mpyri.addr",
    "llvm.hexagon.M4.mpyri.addr.u2",
    "llvm.hexagon.M4.mpyrr.addi",
    "llvm.hexagon.M4.mpyrr.addr",
    "llvm.hexagon.M4.nac.up.s1.sat",
    "llvm.hexagon.M4.or.and",
    "llvm.hexagon.M4.or.andn",
    "llvm.hexagon.M4.or.or",
    "llvm.hexagon.M4.or.xor",
    "llvm.hexagon.M4.pmpyw",
    "llvm.hexagon.M4.pmpyw.acc",
    "llvm.hexagon.M4.vpmpyh",
    "llvm.hexagon.M4.vpmpyh.acc",
    "llvm.hexagon.M4.vrmpyeh.acc.s0",
    "llvm.hexagon.M4.vrmpyeh.acc.s1",
    "llvm.hexagon.M4.vrmpyeh.s0",
    "llvm.hexagon.M4.vrmpyeh.s1",
    "llvm.hexagon.M4.vrmpyoh.acc.s0",
    "llvm.hexagon.M4.vrmpyoh.acc.s1",
    "llvm.hexagon.M4.vrmpyoh.s0",
    "llvm.hexagon.M4.vrmpyoh.s1",
    "llvm.hexagon.M4.xor.and",
    "llvm.hexagon.M4.xor.andn",
    "llvm.hexagon.M4.xor.or",
    "llvm.hexagon.M4.xor.xacc",
    "llvm.hexagon.M5.vdmacbsu",
    "llvm.hexagon.M5.vdmpybsu",
    "llvm.hexagon.M5.vmacbsu",
    "llvm.hexagon.M5.vmacbuu",
    "llvm.hexagon.M5.vmpybsu",
    "llvm.hexagon.M5.vmpybuu",
    "llvm.hexagon.M5.vrmacbsu",
    "llvm.hexagon.M5.vrmacbuu",
    "llvm.hexagon.M5.vrmpybsu",
    "llvm.hexagon.M5.vrmpybuu",
    "llvm.hexagon.M6.vabsdiffb",
    "llvm.hexagon.M6.vabsdiffub",
    "llvm.hexagon.S2.addasl.rrri",
    "llvm.hexagon.S2.asl.i.p",
    "llvm.hexagon.S2.asl.i.p.acc",
    "llvm.hexagon.S2.asl.i.p.and",
    "llvm.hexagon.S2.asl.i.p.nac",
    "llvm.hexagon.S2.asl.i.p.or",
    "llvm.hexagon.S2.asl.i.p.xacc",
    "llvm.hexagon.S2.asl.i.r",
    "llvm.hexagon.S2.asl.i.r.acc",
    "llvm.hexagon.S2.asl.i.r.and",
    "llvm.hexagon.S2.asl.i.r.nac",
    "llvm.hexagon.S2.asl.i.r.or",
    "llvm.hexagon.S2.asl.i.r.sat",
    "llvm.hexagon.S2.asl.i.r.xacc",
    "llvm.hexagon.S2.asl.i.vh",
    "llvm.hexagon.S2.asl.i.vw",
    "llvm.hexagon.S2.asl.r.p",
    "llvm.hexagon.S2.asl.r.p.acc",
    "llvm.hexagon.S2.asl.r.p.and",
    "llvm.hexagon.S2.asl.r.p.nac",
    "llvm.hexagon.S2.asl.r.p.or",
    "llvm.hexagon.S2.asl.r.p.xor",
    "llvm.hexagon.S2.asl.r.r",
    "llvm.hexagon.S2.asl.r.r.acc",
    "llvm.hexagon.S2.asl.r.r.and",
    "llvm.hexagon.S2.asl.r.r.nac",
    "llvm.hexagon.S2.asl.r.r.or",
    "llvm.hexagon.S2.asl.r.r.sat",
    "llvm.hexagon.S2.asl.r.vh",
    "llvm.hexagon.S2.asl.r.vw",
    "llvm.hexagon.S2.asr.i.p",
    "llvm.hexagon.S2.asr.i.p.acc",
    "llvm.hexagon.S2.asr.i.p.and",
    "llvm.hexagon.S2.asr.i.p.nac",
    "llvm.hexagon.S2.asr.i.p.or",
    "llvm.hexagon.S2.asr.i.p.rnd",
    "llvm.hexagon.S2.asr.i.p.rnd.goodsyntax",
    "llvm.hexagon.S2.asr.i.r",
    "llvm.hexagon.S2.asr.i.r.acc",
    "llvm.hexagon.S2.asr.i.r.and",
    "llvm.hexagon.S2.asr.i.r.nac",
    "llvm.hexagon.S2.asr.i.r.or",
    "llvm.hexagon.S2.asr.i.r.rnd",
    "llvm.hexagon.S2.asr.i.r.rnd.goodsyntax",
    "llvm.hexagon.S2.asr.i.svw.trun",
    "llvm.hexagon.S2.asr.i.vh",
    "llvm.hexagon.S2.asr.i.vw",
    "llvm.hexagon.S2.asr.r.p",
    "llvm.hexagon.S2.asr.r.p.acc",
    "llvm.hexagon.S2.asr.r.p.and",
    "llvm.hexagon.S2.asr.r.p.nac",
    "llvm.hexagon.S2.asr.r.p.or",
    "llvm.hexagon.S2.asr.r.p.xor",
    "llvm.hexagon.S2.asr.r.r",
    "llvm.hexagon.S2.asr.r.r.acc",
    "llvm.hexagon.S2.asr.r.r.and",
    "llvm.hexagon.S2.asr.r.r.nac",
    "llvm.hexagon.S2.asr.r.r.or",
    "llvm.hexagon.S2.asr.r.r.sat",
    "llvm.hexagon.S2.asr.r.svw.trun",
    "llvm.hexagon.S2.asr.r.vh",
    "llvm.hexagon.S2.asr.r.vw",
    "llvm.hexagon.S2.brev",
    "llvm.hexagon.S2.brevp",
    "llvm.hexagon.S2.cabacencbin",
    "llvm.hexagon.S2.cl0",
    "llvm.hexagon.S2.cl0p",
    "llvm.hexagon.S2.cl1",
    "llvm.hexagon.S2.cl1p",
    "llvm.hexagon.S2.clb",
    "llvm.hexagon.S2.clbnorm",
    "llvm.hexagon.S2.clbp",
    "llvm.hexagon.S2.clrbit.i",
    "llvm.hexagon.S2.clrbit.r",
    "llvm.hexagon.S2.ct0",
    "llvm.hexagon.S2.ct0p",
    "llvm.hexagon.S2.ct1",
    "llvm.hexagon.S2.ct1p",
    "llvm.hexagon.S2.deinterleave",
    "llvm.hexagon.S2.extractu",
    "llvm.hexagon.S2.extractu.rp",
    "llvm.hexagon.S2.extractup",
    "llvm.hexagon.S2.extractup.rp",
    "llvm.hexagon.S2.insert",
    "llvm.hexagon.S2.insert.rp",
    "llvm.hexagon.S2.insertp",
    "llvm.hexagon.S2.insertp.rp",
    "llvm.hexagon.S2.interleave",
    "llvm.hexagon.S2.lfsp",
    "llvm.hexagon.S2.lsl.r.p",
    "llvm.hexagon.S2.lsl.r.p.acc",
    "llvm.hexagon.S2.lsl.r.p.and",
    "llvm.hexagon.S2.lsl.r.p.nac",
    "llvm.hexagon.S2.lsl.r.p.or",
    "llvm.hexagon.S2.lsl.r.p.xor",
    "llvm.hexagon.S2.lsl.r.r",
    "llvm.hexagon.S2.lsl.r.r.acc",
    "llvm.hexagon.S2.lsl.r.r.and",
    "llvm.hexagon.S2.lsl.r.r.nac",
    "llvm.hexagon.S2.lsl.r.r.or",
    "llvm.hexagon.S2.lsl.r.vh",
    "llvm.hexagon.S2.lsl.r.vw",
    "llvm.hexagon.S2.lsr.i.p",
    "llvm.hexagon.S2.lsr.i.p.acc",
    "llvm.hexagon.S2.lsr.i.p.and",
    "llvm.hexagon.S2.lsr.i.p.nac",
    "llvm.hexagon.S2.lsr.i.p.or",
    "llvm.hexagon.S2.lsr.i.p.xacc",
    "llvm.hexagon.S2.lsr.i.r",
    "llvm.hexagon.S2.lsr.i.r.acc",
    "llvm.hexagon.S2.lsr.i.r.and",
    "llvm.hexagon.S2.lsr.i.r.nac",
    "llvm.hexagon.S2.lsr.i.r.or",
    "llvm.hexagon.S2.lsr.i.r.xacc",
    "llvm.hexagon.S2.lsr.i.vh",
    "llvm.hexagon.S2.lsr.i.vw",
    "llvm.hexagon.S2.lsr.r.p",
    "llvm.hexagon.S2.lsr.r.p.acc",
    "llvm.hexagon.S2.lsr.r.p.and",
    "llvm.hexagon.S2.lsr.r.p.nac",
    "llvm.hexagon.S2.lsr.r.p.or",
    "llvm.hexagon.S2.lsr.r.p.xor",
    "llvm.hexagon.S2.lsr.r.r",
    "llvm.hexagon.S2.lsr.r.r.acc",
    "llvm.hexagon.S2.lsr.r.r.and",
    "llvm.hexagon.S2.lsr.r.r.nac",
    "llvm.hexagon.S2.lsr.r.r.or",
    "llvm.hexagon.S2.lsr.r.vh",
    "llvm.hexagon.S2.lsr.r.vw",
    "llvm.hexagon.S2.packhl",
    "llvm.hexagon.S2.parityp",
    "llvm.hexagon.S2.setbit.i",
    "llvm.hexagon.S2.setbit.r",
    "llvm.hexagon.S2.shuffeb",
    "llvm.hexagon.S2.shuffeh",
    "llvm.hexagon.S2.shuffob",
    "llvm.hexagon.S2.shuffoh",
    "llvm.hexagon.S2.storew.locked",
    "llvm.hexagon.S2.svsathb",
    "llvm.hexagon.S2.svsathub",
    "llvm.hexagon.S2.tableidxb.goodsyntax",
    "llvm.hexagon.S2.tableidxd.goodsyntax",
    "llvm.hexagon.S2.tableidxh.goodsyntax",
    "llvm.hexagon.S2.tableidxw.goodsyntax",
    "llvm.hexagon.S2.togglebit.i",
    "llvm.hexagon.S2.togglebit.r",
    "llvm.hexagon.S2.tstbit.i",
    "llvm.hexagon.S2.tstbit.r",
    "llvm.hexagon.S2.valignib",
    "llvm.hexagon.S2.valignrb",
    "llvm.hexagon.S2.vcnegh",
    "llvm.hexagon.S2.vcrotate",
    "llvm.hexagon.S2.vrcnegh",
    "llvm.hexagon.S2.vrndpackwh",
    "llvm.hexagon.S2.vrndpackwhs",
    "llvm.hexagon.S2.vsathb",
    "llvm.hexagon.S2.vsathb.nopack",
    "llvm.hexagon.S2.vsathub",
    "llvm.hexagon.S2.vsathub.nopack",
    "llvm.hexagon.S2.vsatwh",
    "llvm.hexagon.S2.vsatwh.nopack",
    "llvm.hexagon.S2.vsatwuh",
    "llvm.hexagon.S2.vsatwuh.nopack",
    "llvm.hexagon.S2.vsplatrb",
    "llvm.hexagon.S2.vsplatrh",
    "llvm.hexagon.S2.vspliceib",
    "llvm.hexagon.S2.vsplicerb",
    "llvm.hexagon.S2.vsxtbh",
    "llvm.hexagon.S2.vsxthw",
    "llvm.hexagon.S2.vtrunehb",
    "llvm.hexagon.S2.vtrunewh",
    "llvm.hexagon.S2.vtrunohb",
    "llvm.hexagon.S2.vtrunowh",
    "llvm.hexagon.S2.vzxtbh",
    "llvm.hexagon.S2.vzxthw",
    "llvm.hexagon.S4.addaddi",
    "llvm.hexagon.S4.addi.asl.ri",
    "llvm.hexagon.S4.addi.lsr.ri",
    "llvm.hexagon.S4.andi.asl.ri",
    "llvm.hexagon.S4.andi.lsr.ri",
    "llvm.hexagon.S4.clbaddi",
    "llvm.hexagon.S4.clbpaddi",
    "llvm.hexagon.S4.clbpnorm",
    "llvm.hexagon.S4.extract",
    "llvm.hexagon.S4.extract.rp",
    "llvm.hexagon.S4.extractp",
    "llvm.hexagon.S4.extractp.rp",
    "llvm.hexagon.S4.lsli",
    "llvm.hexagon.S4.ntstbit.i",
    "llvm.hexagon.S4.ntstbit.r",
    "llvm.hexagon.S4.or.andi",
    "llvm.hexagon.S4.or.andix",
    "llvm.hexagon.S4.or.ori",
    "llvm.hexagon.S4.ori.asl.ri",
    "llvm.hexagon.S4.ori.lsr.ri",
    "llvm.hexagon.S4.parity",
    "llvm.hexagon.S4.stored.locked",
    "llvm.hexagon.S4.subaddi",
    "llvm.hexagon.S4.subi.asl.ri",
    "llvm.hexagon.S4.subi.lsr.ri",
    "llvm.hexagon.S4.vrcrotate",
    "llvm.hexagon.S4.vrcrotate.acc",
    "llvm.hexagon.S4.vxaddsubh",
    "llvm.hexagon.S4.vxaddsubhr",
    "llvm.hexagon.S4.vxaddsubw",
    "llvm.hexagon.S4.vxsubaddh",
    "llvm.hexagon.S4.vxsubaddhr",
    "llvm.hexagon.S4.vxsubaddw",
    "llvm.hexagon.S5.asrhub.rnd.sat.goodsyntax",
    "llvm.hexagon.S5.asrhub.sat",
    "llvm.hexagon.S5.popcountp",
    "llvm.hexagon.S5.vasrhrnd.goodsyntax",
    "llvm.hexagon.S6.rol.i.p",
    "llvm.hexagon.S6.rol.i.p.acc",
    "llvm.hexagon.S6.rol.i.p.and",
    "llvm.hexagon.S6.rol.i.p.nac",
    "llvm.hexagon.S6.rol.i.p.or",
    "llvm.hexagon.S6.rol.i.p.xacc",
    "llvm.hexagon.S6.rol.i.r",
    "llvm.hexagon.S6.rol.i.r.acc",
    "llvm.hexagon.S6.rol.i.r.and",
    "llvm.hexagon.S6.rol.i.r.nac",
    "llvm.hexagon.S6.rol.i.r.or",
    "llvm.hexagon.S6.rol.i.r.xacc",
    "llvm.hexagon.S6.vsplatrbp",
    "llvm.hexagon.S6.vtrunehb.ppp",
    "llvm.hexagon.S6.vtrunohb.ppp",
    "llvm.hexagon.SI.to.SXTHI.asrh",
    "llvm.hexagon.V6.extractw",
    "llvm.hexagon.V6.extractw.128B",
    "llvm.hexagon.V6.hi",
    "llvm.hexagon.V6.hi.128B",
    "llvm.hexagon.V6.lo",
    "llvm.hexagon.V6.lo.128B",
    "llvm.hexagon.V6.lvsplatb",
    "llvm.hexagon.V6.lvsplatb.128B",
    "llvm.hexagon.V6.lvsplath",
    "llvm.hexagon.V6.lvsplath.128B",
    "llvm.hexagon.V6.lvsplatw",
    "llvm.hexagon.V6.lvsplatw.128B",
    "llvm.hexagon.V6.pred.and",
    "llvm.hexagon.V6.pred.and.128B",
    "llvm.hexagon.V6.pred.and.n",
    "llvm.hexagon.V6.pred.and.n.128B",
    "llvm.hexagon.V6.pred.not",
    "llvm.hexagon.V6.pred.not.128B",
    "llvm.hexagon.V6.pred.or",
    "llvm.hexagon.V6.pred.or.128B",
    "llvm.hexagon.V6.pred.or.n",
    "llvm.hexagon.V6.pred.or.n.128B",
    "llvm.hexagon.V6.pred.scalar2",
    "llvm.hexagon.V6.pred.scalar2.128B",
    "llvm.hexagon.V6.pred.scalar2v2",
    "llvm.hexagon.V6.pred.scalar2v2.128B",
    "llvm.hexagon.V6.pred.xor",
    "llvm.hexagon.V6.pred.xor.128B",
    "llvm.hexagon.V6.shuffeqh",
    "llvm.hexagon.V6.shuffeqh.128B",
    "llvm.hexagon.V6.shuffeqw",
    "llvm.hexagon.V6.shuffeqw.128B",
    "llvm.hexagon.V6.vS32b.nqpred.ai",
    "llvm.hexagon.V6.vS32b.nqpred.ai.128B",
    "llvm.hexagon.V6.vS32b.nt.nqpred.ai",
    "llvm.hexagon.V6.vS32b.nt.nqpred.ai.128B",
    "llvm.hexagon.V6.vS32b.nt.qpred.ai",
    "llvm.hexagon.V6.vS32b.nt.qpred.ai.128B",
    "llvm.hexagon.V6.vS32b.qpred.ai",
    "llvm.hexagon.V6.vS32b.qpred.ai.128B",
    "llvm.hexagon.V6.vabsb",
    "llvm.hexagon.V6.vabsb.128B",
    "llvm.hexagon.V6.vabsb.sat",
    "llvm.hexagon.V6.vabsb.sat.128B",
    "llvm.hexagon.V6.vabsdiffh",
    "llvm.hexagon.V6.vabsdiffh.128B",
    "llvm.hexagon.V6.vabsdiffub",
    "llvm.hexagon.V6.vabsdiffub.128B",
    "llvm.hexagon.V6.vabsdiffuh",
    "llvm.hexagon.V6.vabsdiffuh.128B",
    "llvm.hexagon.V6.vabsdiffw",
    "llvm.hexagon.V6.vabsdiffw.128B",
    "llvm.hexagon.V6.vabsh",
    "llvm.hexagon.V6.vabsh.128B",
    "llvm.hexagon.V6.vabsh.sat",
    "llvm.hexagon.V6.vabsh.sat.128B",
    "llvm.hexagon.V6.vabsw",
    "llvm.hexagon.V6.vabsw.128B",
    "llvm.hexagon.V6.vabsw.sat",
    "llvm.hexagon.V6.vabsw.sat.128B",
    "llvm.hexagon.V6.vaddb",
    "llvm.hexagon.V6.vaddb.128B",
    "llvm.hexagon.V6.vaddb.dv",
    "llvm.hexagon.V6.vaddb.dv.128B",
    "llvm.hexagon.V6.vaddbnq",
    "llvm.hexagon.V6.vaddbnq.128B",
    "llvm.hexagon.V6.vaddbq",
    "llvm.hexagon.V6.vaddbq.128B",
    "llvm.hexagon.V6.vaddbsat",
    "llvm.hexagon.V6.vaddbsat.128B",
    "llvm.hexagon.V6.vaddbsat.dv",
    "llvm.hexagon.V6.vaddbsat.dv.128B",
    "llvm.hexagon.V6.vaddcarry",
    "llvm.hexagon.V6.vaddcarry.128B",
    "llvm.hexagon.V6.vaddclbh",
    "llvm.hexagon.V6.vaddclbh.128B",
    "llvm.hexagon.V6.vaddclbw",
    "llvm.hexagon.V6.vaddclbw.128B",
    "llvm.hexagon.V6.vaddh",
    "llvm.hexagon.V6.vaddh.128B",
    "llvm.hexagon.V6.vaddh.dv",
    "llvm.hexagon.V6.vaddh.dv.128B",
    "llvm.hexagon.V6.vaddhnq",
    "llvm.hexagon.V6.vaddhnq.128B",
    "llvm.hexagon.V6.vaddhq",
    "llvm.hexagon.V6.vaddhq.128B",
    "llvm.hexagon.V6.vaddhsat",
    "llvm.hexagon.V6.vaddhsat.128B",
    "llvm.hexagon.V6.vaddhsat.dv",
    "llvm.hexagon.V6.vaddhsat.dv.128B",
    "llvm.hexagon.V6.vaddhw",
    "llvm.hexagon.V6.vaddhw.128B",
    "llvm.hexagon.V6.vaddhw.acc",
    "llvm.hexagon.V6.vaddhw.acc.128B",
    "llvm.hexagon.V6.vaddubh",
    "llvm.hexagon.V6.vaddubh.128B",
    "llvm.hexagon.V6.vaddubh.acc",
    "llvm.hexagon.V6.vaddubh.acc.128B",
    "llvm.hexagon.V6.vaddubsat",
    "llvm.hexagon.V6.vaddubsat.128B",
    "llvm.hexagon.V6.vaddubsat.dv",
    "llvm.hexagon.V6.vaddubsat.dv.128B",
    "llvm.hexagon.V6.vaddububb.sat",
    "llvm.hexagon.V6.vaddububb.sat.128B",
    "llvm.hexagon.V6.vadduhsat",
    "llvm.hexagon.V6.vadduhsat.128B",
    "llvm.hexagon.V6.vadduhsat.dv",
    "llvm.hexagon.V6.vadduhsat.dv.128B",
    "llvm.hexagon.V6.vadduhw",
    "llvm.hexagon.V6.vadduhw.128B",
    "llvm.hexagon.V6.vadduhw.acc",
    "llvm.hexagon.V6.vadduhw.acc.128B",
    "llvm.hexagon.V6.vadduwsat",
    "llvm.hexagon.V6.vadduwsat.128B",
    "llvm.hexagon.V6.vadduwsat.dv",
    "llvm.hexagon.V6.vadduwsat.dv.128B",
    "llvm.hexagon.V6.vaddw",
    "llvm.hexagon.V6.vaddw.128B",
    "llvm.hexagon.V6.vaddw.dv",
    "llvm.hexagon.V6.vaddw.dv.128B",
    "llvm.hexagon.V6.vaddwnq",
    "llvm.hexagon.V6.vaddwnq.128B",
    "llvm.hexagon.V6.vaddwq",
    "llvm.hexagon.V6.vaddwq.128B",
    "llvm.hexagon.V6.vaddwsat",
    "llvm.hexagon.V6.vaddwsat.128B",
    "llvm.hexagon.V6.vaddwsat.dv",
    "llvm.hexagon.V6.vaddwsat.dv.128B",
    "llvm.hexagon.V6.valignb",
    "llvm.hexagon.V6.valignb.128B",
    "llvm.hexagon.V6.valignbi",
    "llvm.hexagon.V6.valignbi.128B",
    "llvm.hexagon.V6.vand",
    "llvm.hexagon.V6.vand.128B",
    "llvm.hexagon.V6.vandnqrt",
    "llvm.hexagon.V6.vandnqrt.128B",
    "llvm.hexagon.V6.vandnqrt.acc",
    "llvm.hexagon.V6.vandnqrt.acc.128B",
    "llvm.hexagon.V6.vandqrt",
    "llvm.hexagon.V6.vandqrt.128B",
    "llvm.hexagon.V6.vandqrt.acc",
    "llvm.hexagon.V6.vandqrt.acc.128B",
    "llvm.hexagon.V6.vandvnqv",
    "llvm.hexagon.V6.vandvnqv.128B",
    "llvm.hexagon.V6.vandvqv",
    "llvm.hexagon.V6.vandvqv.128B",
    "llvm.hexagon.V6.vandvrt",
    "llvm.hexagon.V6.vandvrt.128B",
    "llvm.hexagon.V6.vandvrt.acc",
    "llvm.hexagon.V6.vandvrt.acc.128B",
    "llvm.hexagon.V6.vaslh",
    "llvm.hexagon.V6.vaslh.128B",
    "llvm.hexagon.V6.vaslh.acc",
    "llvm.hexagon.V6.vaslh.acc.128B",
    "llvm.hexagon.V6.vaslhv",
    "llvm.hexagon.V6.vaslhv.128B",
    "llvm.hexagon.V6.vaslw",
    "llvm.hexagon.V6.vaslw.128B",
    "llvm.hexagon.V6.vaslw.acc",
    "llvm.hexagon.V6.vaslw.acc.128B",
    "llvm.hexagon.V6.vaslwv",
    "llvm.hexagon.V6.vaslwv.128B",
    "llvm.hexagon.V6.vasrh",
    "llvm.hexagon.V6.vasrh.128B",
    "llvm.hexagon.V6.vasrh.acc",
    "llvm.hexagon.V6.vasrh.acc.128B",
    "llvm.hexagon.V6.vasrhbrndsat",
    "llvm.hexagon.V6.vasrhbrndsat.128B",
    "llvm.hexagon.V6.vasrhbsat",
    "llvm.hexagon.V6.vasrhbsat.128B",
    "llvm.hexagon.V6.vasrhubrndsat",
    "llvm.hexagon.V6.vasrhubrndsat.128B",
    "llvm.hexagon.V6.vasrhubsat",
    "llvm.hexagon.V6.vasrhubsat.128B",
    "llvm.hexagon.V6.vasrhv",
    "llvm.hexagon.V6.vasrhv.128B",
    "llvm.hexagon.V6.vasruhubrndsat",
    "llvm.hexagon.V6.vasruhubrndsat.128B",
    "llvm.hexagon.V6.vasruhubsat",
    "llvm.hexagon.V6.vasruhubsat.128B",
    "llvm.hexagon.V6.vasruwuhrndsat",
    "llvm.hexagon.V6.vasruwuhrndsat.128B",
    "llvm.hexagon.V6.vasruwuhsat",
    "llvm.hexagon.V6.vasruwuhsat.128B",
    "llvm.hexagon.V6.vasrw",
    "llvm.hexagon.V6.vasrw.128B",
    "llvm.hexagon.V6.vasrw.acc",
    "llvm.hexagon.V6.vasrw.acc.128B",
    "llvm.hexagon.V6.vasrwh",
    "llvm.hexagon.V6.vasrwh.128B",
    "llvm.hexagon.V6.vasrwhrndsat",
    "llvm.hexagon.V6.vasrwhrndsat.128B",
    "llvm.hexagon.V6.vasrwhsat",
    "llvm.hexagon.V6.vasrwhsat.128B",
    "llvm.hexagon.V6.vasrwuhrndsat",
    "llvm.hexagon.V6.vasrwuhrndsat.128B",
    "llvm.hexagon.V6.vasrwuhsat",
    "llvm.hexagon.V6.vasrwuhsat.128B",
    "llvm.hexagon.V6.vasrwv",
    "llvm.hexagon.V6.vasrwv.128B",
    "llvm.hexagon.V6.vassign",
    "llvm.hexagon.V6.vassign.128B",
    "llvm.hexagon.V6.vassignp",
    "llvm.hexagon.V6.vassignp.128B",
    "llvm.hexagon.V6.vavgb",
    "llvm.hexagon.V6.vavgb.128B",
    "llvm.hexagon.V6.vavgbrnd",
    "llvm.hexagon.V6.vavgbrnd.128B",
    "llvm.hexagon.V6.vavgh",
    "llvm.hexagon.V6.vavgh.128B",
    "llvm.hexagon.V6.vavghrnd",
    "llvm.hexagon.V6.vavghrnd.128B",
    "llvm.hexagon.V6.vavgub",
    "llvm.hexagon.V6.vavgub.128B",
    "llvm.hexagon.V6.vavgubrnd",
    "llvm.hexagon.V6.vavgubrnd.128B",
    "llvm.hexagon.V6.vavguh",
    "llvm.hexagon.V6.vavguh.128B",
    "llvm.hexagon.V6.vavguhrnd",
    "llvm.hexagon.V6.vavguhrnd.128B",
    "llvm.hexagon.V6.vavguw",
    "llvm.hexagon.V6.vavguw.128B",
    "llvm.hexagon.V6.vavguwrnd",
    "llvm.hexagon.V6.vavguwrnd.128B",
    "llvm.hexagon.V6.vavgw",
    "llvm.hexagon.V6.vavgw.128B",
    "llvm.hexagon.V6.vavgwrnd",
    "llvm.hexagon.V6.vavgwrnd.128B",
    "llvm.hexagon.V6.vcl0h",
    "llvm.hexagon.V6.vcl0h.128B",
    "llvm.hexagon.V6.vcl0w",
    "llvm.hexagon.V6.vcl0w.128B",
    "llvm.hexagon.V6.vcombine",
    "llvm.hexagon.V6.vcombine.128B",
    "llvm.hexagon.V6.vd0",
    "llvm.hexagon.V6.vd0.128B",
    "llvm.hexagon.V6.vdd0",
    "llvm.hexagon.V6.vdd0.128B",
    "llvm.hexagon.V6.vdealb",
    "llvm.hexagon.V6.vdealb.128B",
    "llvm.hexagon.V6.vdealb4w",
    "llvm.hexagon.V6.vdealb4w.128B",
    "llvm.hexagon.V6.vdealh",
    "llvm.hexagon.V6.vdealh.128B",
    "llvm.hexagon.V6.vdealvdd",
    "llvm.hexagon.V6.vdealvdd.128B",
    "llvm.hexagon.V6.vdelta",
    "llvm.hexagon.V6.vdelta.128B",
    "llvm.hexagon.V6.vdmpybus",
    "llvm.hexagon.V6.vdmpybus.128B",
    "llvm.hexagon.V6.vdmpybus.acc",
    "llvm.hexagon.V6.vdmpybus.acc.128B",
    "llvm.hexagon.V6.vdmpybus.dv",
    "llvm.hexagon.V6.vdmpybus.dv.128B",
    "llvm.hexagon.V6.vdmpybus.dv.acc",
    "llvm.hexagon.V6.vdmpybus.dv.acc.128B",
    "llvm.hexagon.V6.vdmpyhb",
    "llvm.hexagon.V6.vdmpyhb.128B",
    "llvm.hexagon.V6.vdmpyhb.acc",
    "llvm.hexagon.V6.vdmpyhb.acc.128B",
    "llvm.hexagon.V6.vdmpyhb.dv",
    "llvm.hexagon.V6.vdmpyhb.dv.128B",
    "llvm.hexagon.V6.vdmpyhb.dv.acc",
    "llvm.hexagon.V6.vdmpyhb.dv.acc.128B",
    "llvm.hexagon.V6.vdmpyhisat",
    "llvm.hexagon.V6.vdmpyhisat.128B",
    "llvm.hexagon.V6.vdmpyhisat.acc",
    "llvm.hexagon.V6.vdmpyhisat.acc.128B",
    "llvm.hexagon.V6.vdmpyhsat",
    "llvm.hexagon.V6.vdmpyhsat.128B",
    "llvm.hexagon.V6.vdmpyhsat.acc",
    "llvm.hexagon.V6.vdmpyhsat.acc.128B",
    "llvm.hexagon.V6.vdmpyhsuisat",
    "llvm.hexagon.V6.vdmpyhsuisat.128B",
    "llvm.hexagon.V6.vdmpyhsuisat.acc",
    "llvm.hexagon.V6.vdmpyhsuisat.acc.128B",
    "llvm.hexagon.V6.vdmpyhsusat",
    "llvm.hexagon.V6.vdmpyhsusat.128B",
    "llvm.hexagon.V6.vdmpyhsusat.acc",
    "llvm.hexagon.V6.vdmpyhsusat.acc.128B",
    "llvm.hexagon.V6.vdmpyhvsat",
    "llvm.hexagon.V6.vdmpyhvsat.128B",
    "llvm.hexagon.V6.vdmpyhvsat.acc",
    "llvm.hexagon.V6.vdmpyhvsat.acc.128B",
    "llvm.hexagon.V6.vdsaduh",
    "llvm.hexagon.V6.vdsaduh.128B",
    "llvm.hexagon.V6.vdsaduh.acc",
    "llvm.hexagon.V6.vdsaduh.acc.128B",
    "llvm.hexagon.V6.veqb",
    "llvm.hexagon.V6.veqb.128B",
    "llvm.hexagon.V6.veqb.and",
    "llvm.hexagon.V6.veqb.and.128B",
    "llvm.hexagon.V6.veqb.or",
    "llvm.hexagon.V6.veqb.or.128B",
    "llvm.hexagon.V6.veqb.xor",
    "llvm.hexagon.V6.veqb.xor.128B",
    "llvm.hexagon.V6.veqh",
    "llvm.hexagon.V6.veqh.128B",
    "llvm.hexagon.V6.veqh.and",
    "llvm.hexagon.V6.veqh.and.128B",
    "llvm.hexagon.V6.veqh.or",
    "llvm.hexagon.V6.veqh.or.128B",
    "llvm.hexagon.V6.veqh.xor",
    "llvm.hexagon.V6.veqh.xor.128B",
    "llvm.hexagon.V6.veqw",
    "llvm.hexagon.V6.veqw.128B",
    "llvm.hexagon.V6.veqw.and",
    "llvm.hexagon.V6.veqw.and.128B",
    "llvm.hexagon.V6.veqw.or",
    "llvm.hexagon.V6.veqw.or.128B",
    "llvm.hexagon.V6.veqw.xor",
    "llvm.hexagon.V6.veqw.xor.128B",
    "llvm.hexagon.V6.vgathermh",
    "llvm.hexagon.V6.vgathermh.128B",
    "llvm.hexagon.V6.vgathermhq",
    "llvm.hexagon.V6.vgathermhq.128B",
    "llvm.hexagon.V6.vgathermhw",
    "llvm.hexagon.V6.vgathermhw.128B",
    "llvm.hexagon.V6.vgathermhwq",
    "llvm.hexagon.V6.vgathermhwq.128B",
    "llvm.hexagon.V6.vgathermw",
    "llvm.hexagon.V6.vgathermw.128B",
    "llvm.hexagon.V6.vgathermwq",
    "llvm.hexagon.V6.vgathermwq.128B",
    "llvm.hexagon.V6.vgtb",
    "llvm.hexagon.V6.vgtb.128B",
    "llvm.hexagon.V6.vgtb.and",
    "llvm.hexagon.V6.vgtb.and.128B",
    "llvm.hexagon.V6.vgtb.or",
    "llvm.hexagon.V6.vgtb.or.128B",
    "llvm.hexagon.V6.vgtb.xor",
    "llvm.hexagon.V6.vgtb.xor.128B",
    "llvm.hexagon.V6.vgth",
    "llvm.hexagon.V6.vgth.128B",
    "llvm.hexagon.V6.vgth.and",
    "llvm.hexagon.V6.vgth.and.128B",
    "llvm.hexagon.V6.vgth.or",
    "llvm.hexagon.V6.vgth.or.128B",
    "llvm.hexagon.V6.vgth.xor",
    "llvm.hexagon.V6.vgth.xor.128B",
    "llvm.hexagon.V6.vgtub",
    "llvm.hexagon.V6.vgtub.128B",
    "llvm.hexagon.V6.vgtub.and",
    "llvm.hexagon.V6.vgtub.and.128B",
    "llvm.hexagon.V6.vgtub.or",
    "llvm.hexagon.V6.vgtub.or.128B",
    "llvm.hexagon.V6.vgtub.xor",
    "llvm.hexagon.V6.vgtub.xor.128B",
    "llvm.hexagon.V6.vgtuh",
    "llvm.hexagon.V6.vgtuh.128B",
    "llvm.hexagon.V6.vgtuh.and",
    "llvm.hexagon.V6.vgtuh.and.128B",
    "llvm.hexagon.V6.vgtuh.or",
    "llvm.hexagon.V6.vgtuh.or.128B",
    "llvm.hexagon.V6.vgtuh.xor",
    "llvm.hexagon.V6.vgtuh.xor.128B",
    "llvm.hexagon.V6.vgtuw",
    "llvm.hexagon.V6.vgtuw.128B",
    "llvm.hexagon.V6.vgtuw.and",
    "llvm.hexagon.V6.vgtuw.and.128B",
    "llvm.hexagon.V6.vgtuw.or",
    "llvm.hexagon.V6.vgtuw.or.128B",
    "llvm.hexagon.V6.vgtuw.xor",
    "llvm.hexagon.V6.vgtuw.xor.128B",
    "llvm.hexagon.V6.vgtw",
    "llvm.hexagon.V6.vgtw.128B",
    "llvm.hexagon.V6.vgtw.and",
    "llvm.hexagon.V6.vgtw.and.128B",
    "llvm.hexagon.V6.vgtw.or",
    "llvm.hexagon.V6.vgtw.or.128B",
    "llvm.hexagon.V6.vgtw.xor",
    "llvm.hexagon.V6.vgtw.xor.128B",
    "llvm.hexagon.V6.vinsertwr",
    "llvm.hexagon.V6.vinsertwr.128B",
    "llvm.hexagon.V6.vlalignb",
    "llvm.hexagon.V6.vlalignb.128B",
    "llvm.hexagon.V6.vlalignbi",
    "llvm.hexagon.V6.vlalignbi.128B",
    "llvm.hexagon.V6.vlsrb",
    "llvm.hexagon.V6.vlsrb.128B",
    "llvm.hexagon.V6.vlsrh",
    "llvm.hexagon.V6.vlsrh.128B",
    "llvm.hexagon.V6.vlsrhv",
    "llvm.hexagon.V6.vlsrhv.128B",
    "llvm.hexagon.V6.vlsrw",
    "llvm.hexagon.V6.vlsrw.128B",
    "llvm.hexagon.V6.vlsrwv",
    "llvm.hexagon.V6.vlsrwv.128B",
    "llvm.hexagon.V6.vlut4",
    "llvm.hexagon.V6.vlut4.128B",
    "llvm.hexagon.V6.vlutvvb",
    "llvm.hexagon.V6.vlutvvb.128B",
    "llvm.hexagon.V6.vlutvvb.nm",
    "llvm.hexagon.V6.vlutvvb.nm.128B",
    "llvm.hexagon.V6.vlutvvb.oracc",
    "llvm.hexagon.V6.vlutvvb.oracc.128B",
    "llvm.hexagon.V6.vlutvvb.oracci",
    "llvm.hexagon.V6.vlutvvb.oracci.128B",
    "llvm.hexagon.V6.vlutvvbi",
    "llvm.hexagon.V6.vlutvvbi.128B",
    "llvm.hexagon.V6.vlutvwh",
    "llvm.hexagon.V6.vlutvwh.128B",
    "llvm.hexagon.V6.vlutvwh.nm",
    "llvm.hexagon.V6.vlutvwh.nm.128B",
    "llvm.hexagon.V6.vlutvwh.oracc",
    "llvm.hexagon.V6.vlutvwh.oracc.128B",
    "llvm.hexagon.V6.vlutvwh.oracci",
    "llvm.hexagon.V6.vlutvwh.oracci.128B",
    "llvm.hexagon.V6.vlutvwhi",
    "llvm.hexagon.V6.vlutvwhi.128B",
    "llvm.hexagon.V6.vmaskedstorenq",
    "llvm.hexagon.V6.vmaskedstorenq.128B",
    "llvm.hexagon.V6.vmaskedstorentnq",
    "llvm.hexagon.V6.vmaskedstorentnq.128B",
    "llvm.hexagon.V6.vmaskedstorentq",
    "llvm.hexagon.V6.vmaskedstorentq.128B",
    "llvm.hexagon.V6.vmaskedstoreq",
    "llvm.hexagon.V6.vmaskedstoreq.128B",
    "llvm.hexagon.V6.vmaxb",
    "llvm.hexagon.V6.vmaxb.128B",
    "llvm.hexagon.V6.vmaxh",
    "llvm.hexagon.V6.vmaxh.128B",
    "llvm.hexagon.V6.vmaxub",
    "llvm.hexagon.V6.vmaxub.128B",
    "llvm.hexagon.V6.vmaxuh",
    "llvm.hexagon.V6.vmaxuh.128B",
    "llvm.hexagon.V6.vmaxw",
    "llvm.hexagon.V6.vmaxw.128B",
    "llvm.hexagon.V6.vminb",
    "llvm.hexagon.V6.vminb.128B",
    "llvm.hexagon.V6.vminh",
    "llvm.hexagon.V6.vminh.128B",
    "llvm.hexagon.V6.vminub",
    "llvm.hexagon.V6.vminub.128B",
    "llvm.hexagon.V6.vminuh",
    "llvm.hexagon.V6.vminuh.128B",
    "llvm.hexagon.V6.vminw",
    "llvm.hexagon.V6.vminw.128B",
    "llvm.hexagon.V6.vmpabus",
    "llvm.hexagon.V6.vmpabus.128B",
    "llvm.hexagon.V6.vmpabus.acc",
    "llvm.hexagon.V6.vmpabus.acc.128B",
    "llvm.hexagon.V6.vmpabusv",
    "llvm.hexagon.V6.vmpabusv.128B",
    "llvm.hexagon.V6.vmpabuu",
    "llvm.hexagon.V6.vmpabuu.128B",
    "llvm.hexagon.V6.vmpabuu.acc",
    "llvm.hexagon.V6.vmpabuu.acc.128B",
    "llvm.hexagon.V6.vmpabuuv",
    "llvm.hexagon.V6.vmpabuuv.128B",
    "llvm.hexagon.V6.vmpahb",
    "llvm.hexagon.V6.vmpahb.128B",
    "llvm.hexagon.V6.vmpahb.acc",
    "llvm.hexagon.V6.vmpahb.acc.128B",
    "llvm.hexagon.V6.vmpahhsat",
    "llvm.hexagon.V6.vmpahhsat.128B",
    "llvm.hexagon.V6.vmpauhb",
    "llvm.hexagon.V6.vmpauhb.128B",
    "llvm.hexagon.V6.vmpauhb.acc",
    "llvm.hexagon.V6.vmpauhb.acc.128B",
    "llvm.hexagon.V6.vmpauhuhsat",
    "llvm.hexagon.V6.vmpauhuhsat.128B",
    "llvm.hexagon.V6.vmpsuhuhsat",
    "llvm.hexagon.V6.vmpsuhuhsat.128B",
    "llvm.hexagon.V6.vmpybus",
    "llvm.hexagon.V6.vmpybus.128B",
    "llvm.hexagon.V6.vmpybus.acc",
    "llvm.hexagon.V6.vmpybus.acc.128B",
    "llvm.hexagon.V6.vmpybusv",
    "llvm.hexagon.V6.vmpybusv.128B",
    "llvm.hexagon.V6.vmpybusv.acc",
    "llvm.hexagon.V6.vmpybusv.acc.128B",
    "llvm.hexagon.V6.vmpybv",
    "llvm.hexagon.V6.vmpybv.128B",
    "llvm.hexagon.V6.vmpybv.acc",
    "llvm.hexagon.V6.vmpybv.acc.128B",
    "llvm.hexagon.V6.vmpyewuh",
    "llvm.hexagon.V6.vmpyewuh.128B",
    "llvm.hexagon.V6.vmpyewuh.64",
    "llvm.hexagon.V6.vmpyewuh.64.128B",
    "llvm.hexagon.V6.vmpyh",
    "llvm.hexagon.V6.vmpyh.128B",
    "llvm.hexagon.V6.vmpyh.acc",
    "llvm.hexagon.V6.vmpyh.acc.128B",
    "llvm.hexagon.V6.vmpyhsat.acc",
    "llvm.hexagon.V6.vmpyhsat.acc.128B",
    "llvm.hexagon.V6.vmpyhsrs",
    "llvm.hexagon.V6.vmpyhsrs.128B",
    "llvm.hexagon.V6.vmpyhss",
    "llvm.hexagon.V6.vmpyhss.128B",
    "llvm.hexagon.V6.vmpyhus",
    "llvm.hexagon.V6.vmpyhus.128B",
    "llvm.hexagon.V6.vmpyhus.acc",
    "llvm.hexagon.V6.vmpyhus.acc.128B",
    "llvm.hexagon.V6.vmpyhv",
    "llvm.hexagon.V6.vmpyhv.128B",
    "llvm.hexagon.V6.vmpyhv.acc",
    "llvm.hexagon.V6.vmpyhv.acc.128B",
    "llvm.hexagon.V6.vmpyhvsrs",
    "llvm.hexagon.V6.vmpyhvsrs.128B",
    "llvm.hexagon.V6.vmpyieoh",
    "llvm.hexagon.V6.vmpyieoh.128B",
    "llvm.hexagon.V6.vmpyiewh.acc",
    "llvm.hexagon.V6.vmpyiewh.acc.128B",
    "llvm.hexagon.V6.vmpyiewuh",
    "llvm.hexagon.V6.vmpyiewuh.128B",
    "llvm.hexagon.V6.vmpyiewuh.acc",
    "llvm.hexagon.V6.vmpyiewuh.acc.128B",
    "llvm.hexagon.V6.vmpyih",
    "llvm.hexagon.V6.vmpyih.128B",
    "llvm.hexagon.V6.vmpyih.acc",
    "llvm.hexagon.V6.vmpyih.acc.128B",
    "llvm.hexagon.V6.vmpyihb",
    "llvm.hexagon.V6.vmpyihb.128B",
    "llvm.hexagon.V6.vmpyihb.acc",
    "llvm.hexagon.V6.vmpyihb.acc.128B",
    "llvm.hexagon.V6.vmpyiowh",
    "llvm.hexagon.V6.vmpyiowh.128B",
    "llvm.hexagon.V6.vmpyiwb",
    "llvm.hexagon.V6.vmpyiwb.128B",
    "llvm.hexagon.V6.vmpyiwb.acc",
    "llvm.hexagon.V6.vmpyiwb.acc.128B",
    "llvm.hexagon.V6.vmpyiwh",
    "llvm.hexagon.V6.vmpyiwh.128B",
    "llvm.hexagon.V6.vmpyiwh.acc",
    "llvm.hexagon.V6.vmpyiwh.acc.128B",
    "llvm.hexagon.V6.vmpyiwub",
    "llvm.hexagon.V6.vmpyiwub.128B",
    "llvm.hexagon.V6.vmpyiwub.acc",
    "llvm.hexagon.V6.vmpyiwub.acc.128B",
    "llvm.hexagon.V6.vmpyowh",
    "llvm.hexagon.V6.vmpyowh.128B",
    "llvm.hexagon.V6.vmpyowh.64.acc",
    "llvm.hexagon.V6.vmpyowh.64.acc.128B",
    "llvm.hexagon.V6.vmpyowh.rnd",
    "llvm.hexagon.V6.vmpyowh.rnd.128B",
    "llvm.hexagon.V6.vmpyowh.rnd.sacc",
    "llvm.hexagon.V6.vmpyowh.rnd.sacc.128B",
    "llvm.hexagon.V6.vmpyowh.sacc",
    "llvm.hexagon.V6.vmpyowh.sacc.128B",
    "llvm.hexagon.V6.vmpyub",
    "llvm.hexagon.V6.vmpyub.128B",
    "llvm.hexagon.V6.vmpyub.acc",
    "llvm.hexagon.V6.vmpyub.acc.128B",
    "llvm.hexagon.V6.vmpyubv",
    "llvm.hexagon.V6.vmpyubv.128B",
    "llvm.hexagon.V6.vmpyubv.acc",
    "llvm.hexagon.V6.vmpyubv.acc.128B",
    "llvm.hexagon.V6.vmpyuh",
    "llvm.hexagon.V6.vmpyuh.128B",
    "llvm.hexagon.V6.vmpyuh.acc",
    "llvm.hexagon.V6.vmpyuh.acc.128B",
    "llvm.hexagon.V6.vmpyuhe",
    "llvm.hexagon.V6.vmpyuhe.128B",
    "llvm.hexagon.V6.vmpyuhe.acc",
    "llvm.hexagon.V6.vmpyuhe.acc.128B",
    "llvm.hexagon.V6.vmpyuhv",
    "llvm.hexagon.V6.vmpyuhv.128B",
    "llvm.hexagon.V6.vmpyuhv.acc",
    "llvm.hexagon.V6.vmpyuhv.acc.128B",
    "llvm.hexagon.V6.vmux",
    "llvm.hexagon.V6.vmux.128B",
    "llvm.hexagon.V6.vnavgb",
    "llvm.hexagon.V6.vnavgb.128B",
    "llvm.hexagon.V6.vnavgh",
    "llvm.hexagon.V6.vnavgh.128B",
    "llvm.hexagon.V6.vnavgub",
    "llvm.hexagon.V6.vnavgub.128B",
    "llvm.hexagon.V6.vnavgw",
    "llvm.hexagon.V6.vnavgw.128B",
    "llvm.hexagon.V6.vnormamth",
    "llvm.hexagon.V6.vnormamth.128B",
    "llvm.hexagon.V6.vnormamtw",
    "llvm.hexagon.V6.vnormamtw.128B",
    "llvm.hexagon.V6.vnot",
    "llvm.hexagon.V6.vnot.128B",
    "llvm.hexagon.V6.vor",
    "llvm.hexagon.V6.vor.128B",
    "llvm.hexagon.V6.vpackeb",
    "llvm.hexagon.V6.vpackeb.128B",
    "llvm.hexagon.V6.vpackeh",
    "llvm.hexagon.V6.vpackeh.128B",
    "llvm.hexagon.V6.vpackhb.sat",
    "llvm.hexagon.V6.vpackhb.sat.128B",
    "llvm.hexagon.V6.vpackhub.sat",
    "llvm.hexagon.V6.vpackhub.sat.128B",
    "llvm.hexagon.V6.vpackob",
    "llvm.hexagon.V6.vpackob.128B",
    "llvm.hexagon.V6.vpackoh",
    "llvm.hexagon.V6.vpackoh.128B",
    "llvm.hexagon.V6.vpackwh.sat",
    "llvm.hexagon.V6.vpackwh.sat.128B",
    "llvm.hexagon.V6.vpackwuh.sat",
    "llvm.hexagon.V6.vpackwuh.sat.128B",
    "llvm.hexagon.V6.vpopcounth",
    "llvm.hexagon.V6.vpopcounth.128B",
    "llvm.hexagon.V6.vprefixqb",
    "llvm.hexagon.V6.vprefixqb.128B",
    "llvm.hexagon.V6.vprefixqh",
    "llvm.hexagon.V6.vprefixqh.128B",
    "llvm.hexagon.V6.vprefixqw",
    "llvm.hexagon.V6.vprefixqw.128B",
    "llvm.hexagon.V6.vrdelta",
    "llvm.hexagon.V6.vrdelta.128B",
    "llvm.hexagon.V6.vrmpybub.rtt",
    "llvm.hexagon.V6.vrmpybub.rtt.128B",
    "llvm.hexagon.V6.vrmpybub.rtt.acc",
    "llvm.hexagon.V6.vrmpybub.rtt.acc.128B",
    "llvm.hexagon.V6.vrmpybus",
    "llvm.hexagon.V6.vrmpybus.128B",
    "llvm.hexagon.V6.vrmpybus.acc",
    "llvm.hexagon.V6.vrmpybus.acc.128B",
    "llvm.hexagon.V6.vrmpybusi",
    "llvm.hexagon.V6.vrmpybusi.128B",
    "llvm.hexagon.V6.vrmpybusi.acc",
    "llvm.hexagon.V6.vrmpybusi.acc.128B",
    "llvm.hexagon.V6.vrmpybusv",
    "llvm.hexagon.V6.vrmpybusv.128B",
    "llvm.hexagon.V6.vrmpybusv.acc",
    "llvm.hexagon.V6.vrmpybusv.acc.128B",
    "llvm.hexagon.V6.vrmpybv",
    "llvm.hexagon.V6.vrmpybv.128B",
    "llvm.hexagon.V6.vrmpybv.acc",
    "llvm.hexagon.V6.vrmpybv.acc.128B",
    "llvm.hexagon.V6.vrmpyub",
    "llvm.hexagon.V6.vrmpyub.128B",
    "llvm.hexagon.V6.vrmpyub.acc",
    "llvm.hexagon.V6.vrmpyub.acc.128B",
    "llvm.hexagon.V6.vrmpyub.rtt",
    "llvm.hexagon.V6.vrmpyub.rtt.128B",
    "llvm.hexagon.V6.vrmpyub.rtt.acc",
    "llvm.hexagon.V6.vrmpyub.rtt.acc.128B",
    "llvm.hexagon.V6.vrmpyubi",
    "llvm.hexagon.V6.vrmpyubi.128B",
    "llvm.hexagon.V6.vrmpyubi.acc",
    "llvm.hexagon.V6.vrmpyubi.acc.128B",
    "llvm.hexagon.V6.vrmpyubv",
    "llvm.hexagon.V6.vrmpyubv.128B",
    "llvm.hexagon.V6.vrmpyubv.acc",
    "llvm.hexagon.V6.vrmpyubv.acc.128B",
    "llvm.hexagon.V6.vror",
    "llvm.hexagon.V6.vror.128B",
    "llvm.hexagon.V6.vroundhb",
    "llvm.hexagon.V6.vroundhb.128B",
    "llvm.hexagon.V6.vroundhub",
    "llvm.hexagon.V6.vroundhub.128B",
    "llvm.hexagon.V6.vrounduhub",
    "llvm.hexagon.V6.vrounduhub.128B",
    "llvm.hexagon.V6.vrounduwuh",
    "llvm.hexagon.V6.vrounduwuh.128B",
    "llvm.hexagon.V6.vroundwh",
    "llvm.hexagon.V6.vroundwh.128B",
    "llvm.hexagon.V6.vroundwuh",
    "llvm.hexagon.V6.vroundwuh.128B",
    "llvm.hexagon.V6.vrsadubi",
    "llvm.hexagon.V6.vrsadubi.128B",
    "llvm.hexagon.V6.vrsadubi.acc",
    "llvm.hexagon.V6.vrsadubi.acc.128B",
    "llvm.hexagon.V6.vsathub",
    "llvm.hexagon.V6.vsathub.128B",
    "llvm.hexagon.V6.vsatuwuh",
    "llvm.hexagon.V6.vsatuwuh.128B",
    "llvm.hexagon.V6.vsatwh",
    "llvm.hexagon.V6.vsatwh.128B",
    "llvm.hexagon.V6.vsb",
    "llvm.hexagon.V6.vsb.128B",
    "llvm.hexagon.V6.vscattermh",
    "llvm.hexagon.V6.vscattermh.128B",
    "llvm.hexagon.V6.vscattermh.add",
    "llvm.hexagon.V6.vscattermh.add.128B",
    "llvm.hexagon.V6.vscattermhq",
    "llvm.hexagon.V6.vscattermhq.128B",
    "llvm.hexagon.V6.vscattermhw",
    "llvm.hexagon.V6.vscattermhw.128B",
    "llvm.hexagon.V6.vscattermhw.add",
    "llvm.hexagon.V6.vscattermhw.add.128B",
    "llvm.hexagon.V6.vscattermhwq",
    "llvm.hexagon.V6.vscattermhwq.128B",
    "llvm.hexagon.V6.vscattermw",
    "llvm.hexagon.V6.vscattermw.128B",
    "llvm.hexagon.V6.vscattermw.add",
    "llvm.hexagon.V6.vscattermw.add.128B",
    "llvm.hexagon.V6.vscattermwq",
    "llvm.hexagon.V6.vscattermwq.128B",
    "llvm.hexagon.V6.vsh",
    "llvm.hexagon.V6.vsh.128B",
    "llvm.hexagon.V6.vshufeh",
    "llvm.hexagon.V6.vshufeh.128B",
    "llvm.hexagon.V6.vshuffb",
    "llvm.hexagon.V6.vshuffb.128B",
    "llvm.hexagon.V6.vshuffeb",
    "llvm.hexagon.V6.vshuffeb.128B",
    "llvm.hexagon.V6.vshuffh",
    "llvm.hexagon.V6.vshuffh.128B",
    "llvm.hexagon.V6.vshuffob",
    "llvm.hexagon.V6.vshuffob.128B",
    "llvm.hexagon.V6.vshuffvdd",
    "llvm.hexagon.V6.vshuffvdd.128B",
    "llvm.hexagon.V6.vshufoeb",
    "llvm.hexagon.V6.vshufoeb.128B",
    "llvm.hexagon.V6.vshufoeh",
    "llvm.hexagon.V6.vshufoeh.128B",
    "llvm.hexagon.V6.vshufoh",
    "llvm.hexagon.V6.vshufoh.128B",
    "llvm.hexagon.V6.vsubb",
    "llvm.hexagon.V6.vsubb.128B",
    "llvm.hexagon.V6.vsubb.dv",
    "llvm.hexagon.V6.vsubb.dv.128B",
    "llvm.hexagon.V6.vsubbnq",
    "llvm.hexagon.V6.vsubbnq.128B",
    "llvm.hexagon.V6.vsubbq",
    "llvm.hexagon.V6.vsubbq.128B",
    "llvm.hexagon.V6.vsubbsat",
    "llvm.hexagon.V6.vsubbsat.128B",
    "llvm.hexagon.V6.vsubbsat.dv",
    "llvm.hexagon.V6.vsubbsat.dv.128B",
    "llvm.hexagon.V6.vsubcarry",
    "llvm.hexagon.V6.vsubcarry.128B",
    "llvm.hexagon.V6.vsubh",
    "llvm.hexagon.V6.vsubh.128B",
    "llvm.hexagon.V6.vsubh.dv",
    "llvm.hexagon.V6.vsubh.dv.128B",
    "llvm.hexagon.V6.vsubhnq",
    "llvm.hexagon.V6.vsubhnq.128B",
    "llvm.hexagon.V6.vsubhq",
    "llvm.hexagon.V6.vsubhq.128B",
    "llvm.hexagon.V6.vsubhsat",
    "llvm.hexagon.V6.vsubhsat.128B",
    "llvm.hexagon.V6.vsubhsat.dv",
    "llvm.hexagon.V6.vsubhsat.dv.128B",
    "llvm.hexagon.V6.vsubhw",
    "llvm.hexagon.V6.vsubhw.128B",
    "llvm.hexagon.V6.vsububh",
    "llvm.hexagon.V6.vsububh.128B",
    "llvm.hexagon.V6.vsububsat",
    "llvm.hexagon.V6.vsububsat.128B",
    "llvm.hexagon.V6.vsububsat.dv",
    "llvm.hexagon.V6.vsububsat.dv.128B",
    "llvm.hexagon.V6.vsubububb.sat",
    "llvm.hexagon.V6.vsubububb.sat.128B",
    "llvm.hexagon.V6.vsubuhsat",
    "llvm.hexagon.V6.vsubuhsat.128B",
    "llvm.hexagon.V6.vsubuhsat.dv",
    "llvm.hexagon.V6.vsubuhsat.dv.128B",
    "llvm.hexagon.V6.vsubuhw",
    "llvm.hexagon.V6.vsubuhw.128B",
    "llvm.hexagon.V6.vsubuwsat",
    "llvm.hexagon.V6.vsubuwsat.128B",
    "llvm.hexagon.V6.vsubuwsat.dv",
    "llvm.hexagon.V6.vsubuwsat.dv.128B",
    "llvm.hexagon.V6.vsubw",
    "llvm.hexagon.V6.vsubw.128B",
    "llvm.hexagon.V6.vsubw.dv",
    "llvm.hexagon.V6.vsubw.dv.128B",
    "llvm.hexagon.V6.vsubwnq",
    "llvm.hexagon.V6.vsubwnq.128B",
    "llvm.hexagon.V6.vsubwq",
    "llvm.hexagon.V6.vsubwq.128B",
    "llvm.hexagon.V6.vsubwsat",
    "llvm.hexagon.V6.vsubwsat.128B",
    "llvm.hexagon.V6.vsubwsat.dv",
    "llvm.hexagon.V6.vsubwsat.dv.128B",
    "llvm.hexagon.V6.vswap",
    "llvm.hexagon.V6.vswap.128B",
    "llvm.hexagon.V6.vtmpyb",
    "llvm.hexagon.V6.vtmpyb.128B",
    "llvm.hexagon.V6.vtmpyb.acc",
    "llvm.hexagon.V6.vtmpyb.acc.128B",
    "llvm.hexagon.V6.vtmpybus",
    "llvm.hexagon.V6.vtmpybus.128B",
    "llvm.hexagon.V6.vtmpybus.acc",
    "llvm.hexagon.V6.vtmpybus.acc.128B",
    "llvm.hexagon.V6.vtmpyhb",
    "llvm.hexagon.V6.vtmpyhb.128B",
    "llvm.hexagon.V6.vtmpyhb.acc",
    "llvm.hexagon.V6.vtmpyhb.acc.128B",
    "llvm.hexagon.V6.vunpackb",
    "llvm.hexagon.V6.vunpackb.128B",
    "llvm.hexagon.V6.vunpackh",
    "llvm.hexagon.V6.vunpackh.128B",
    "llvm.hexagon.V6.vunpackob",
    "llvm.hexagon.V6.vunpackob.128B",
    "llvm.hexagon.V6.vunpackoh",
    "llvm.hexagon.V6.vunpackoh.128B",
    "llvm.hexagon.V6.vunpackub",
    "llvm.hexagon.V6.vunpackub.128B",
    "llvm.hexagon.V6.vunpackuh",
    "llvm.hexagon.V6.vunpackuh.128B",
    "llvm.hexagon.V6.vxor",
    "llvm.hexagon.V6.vxor.128B",
    "llvm.hexagon.V6.vzb",
    "llvm.hexagon.V6.vzb.128B",
    "llvm.hexagon.V6.vzh",
    "llvm.hexagon.V6.vzh.128B",
    "llvm.hexagon.Y2.dccleana",
    "llvm.hexagon.Y2.dccleaninva",
    "llvm.hexagon.Y2.dcinva",
    "llvm.hexagon.Y2.dczeroa",
    "llvm.hexagon.Y4.l2fetch",
    "llvm.hexagon.Y5.l2fetch",
    "llvm.hexagon.brev.ldb",
    "llvm.hexagon.brev.ldd",
    "llvm.hexagon.brev.ldh",
    "llvm.hexagon.brev.ldub",
    "llvm.hexagon.brev.lduh",
    "llvm.hexagon.brev.ldw",
    "llvm.hexagon.brev.stb",
    "llvm.hexagon.brev.std",
    "llvm.hexagon.brev.sth",
    "llvm.hexagon.brev.sthhi",
    "llvm.hexagon.brev.stw",
    "llvm.hexagon.circ.ldb",
    "llvm.hexagon.circ.ldd",
    "llvm.hexagon.circ.ldh",
    "llvm.hexagon.circ.ldub",
    "llvm.hexagon.circ.lduh",
    "llvm.hexagon.circ.ldw",
    "llvm.hexagon.circ.stb",
    "llvm.hexagon.circ.std",
    "llvm.hexagon.circ.sth",
    "llvm.hexagon.circ.sthhi",
    "llvm.hexagon.circ.stw",
    "llvm.hexagon.mm256i.vaddw",
    "llvm.hexagon.prefetch",
    "llvm.mips.absq.s.ph",
    "llvm.mips.absq.s.qb",
    "llvm.mips.absq.s.w",
    "llvm.mips.add.a.b",
    "llvm.mips.add.a.d",
    "llvm.mips.add.a.h",
    "llvm.mips.add.a.w",
    "llvm.mips.addq.ph",
    "llvm.mips.addq.s.ph",
    "llvm.mips.addq.s.w",
    "llvm.mips.addqh.ph",
    "llvm.mips.addqh.r.ph",
    "llvm.mips.addqh.r.w",
    "llvm.mips.addqh.w",
    "llvm.mips.adds.a.b",
    "llvm.mips.adds.a.d",
    "llvm.mips.adds.a.h",
    "llvm.mips.adds.a.w",
    "llvm.mips.adds.s.b",
    "llvm.mips.adds.s.d",
    "llvm.mips.adds.s.h",
    "llvm.mips.adds.s.w",
    "llvm.mips.adds.u.b",
    "llvm.mips.adds.u.d",
    "llvm.mips.adds.u.h",
    "llvm.mips.adds.u.w",
    "llvm.mips.addsc",
    "llvm.mips.addu.ph",
    "llvm.mips.addu.qb",
    "llvm.mips.addu.s.ph",
    "llvm.mips.addu.s.qb",
    "llvm.mips.adduh.qb",
    "llvm.mips.adduh.r.qb",
    "llvm.mips.addv.b",
    "llvm.mips.addv.d",
    "llvm.mips.addv.h",
    "llvm.mips.addv.w",
    "llvm.mips.addvi.b",
    "llvm.mips.addvi.d",
    "llvm.mips.addvi.h",
    "llvm.mips.addvi.w",
    "llvm.mips.addwc",
    "llvm.mips.and.v",
    "llvm.mips.andi.b",
    "llvm.mips.append",
    "llvm.mips.asub.s.b",
    "llvm.mips.asub.s.d",
    "llvm.mips.asub.s.h",
    "llvm.mips.asub.s.w",
    "llvm.mips.asub.u.b",
    "llvm.mips.asub.u.d",
    "llvm.mips.asub.u.h",
    "llvm.mips.asub.u.w",
    "llvm.mips.ave.s.b",
    "llvm.mips.ave.s.d",
    "llvm.mips.ave.s.h",
    "llvm.mips.ave.s.w",
    "llvm.mips.ave.u.b",
    "llvm.mips.ave.u.d",
    "llvm.mips.ave.u.h",
    "llvm.mips.ave.u.w",
    "llvm.mips.aver.s.b",
    "llvm.mips.aver.s.d",
    "llvm.mips.aver.s.h",
    "llvm.mips.aver.s.w",
    "llvm.mips.aver.u.b",
    "llvm.mips.aver.u.d",
    "llvm.mips.aver.u.h",
    "llvm.mips.aver.u.w",
    "llvm.mips.balign",
    "llvm.mips.bclr.b",
    "llvm.mips.bclr.d",
    "llvm.mips.bclr.h",
    "llvm.mips.bclr.w",
    "llvm.mips.bclri.b",
    "llvm.mips.bclri.d",
    "llvm.mips.bclri.h",
    "llvm.mips.bclri.w",
    "llvm.mips.binsl.b",
    "llvm.mips.binsl.d",
    "llvm.mips.binsl.h",
    "llvm.mips.binsl.w",
    "llvm.mips.binsli.b",
    "llvm.mips.binsli.d",
    "llvm.mips.binsli.h",
    "llvm.mips.binsli.w",
    "llvm.mips.binsr.b",
    "llvm.mips.binsr.d",
    "llvm.mips.binsr.h",
    "llvm.mips.binsr.w",
    "llvm.mips.binsri.b",
    "llvm.mips.binsri.d",
    "llvm.mips.binsri.h",
    "llvm.mips.binsri.w",
    "llvm.mips.bitrev",
    "llvm.mips.bmnz.v",
    "llvm.mips.bmnzi.b",
    "llvm.mips.bmz.v",
    "llvm.mips.bmzi.b",
    "llvm.mips.bneg.b",
    "llvm.mips.bneg.d",
    "llvm.mips.bneg.h",
    "llvm.mips.bneg.w",
    "llvm.mips.bnegi.b",
    "llvm.mips.bnegi.d",
    "llvm.mips.bnegi.h",
    "llvm.mips.bnegi.w",
    "llvm.mips.bnz.b",
    "llvm.mips.bnz.d",
    "llvm.mips.bnz.h",
    "llvm.mips.bnz.v",
    "llvm.mips.bnz.w",
    "llvm.mips.bposge32",
    "llvm.mips.bsel.v",
    "llvm.mips.bseli.b",
    "llvm.mips.bset.b",
    "llvm.mips.bset.d",
    "llvm.mips.bset.h",
    "llvm.mips.bset.w",
    "llvm.mips.bseti.b",
    "llvm.mips.bseti.d",
    "llvm.mips.bseti.h",
    "llvm.mips.bseti.w",
    "llvm.mips.bz.b",
    "llvm.mips.bz.d",
    "llvm.mips.bz.h",
    "llvm.mips.bz.v",
    "llvm.mips.bz.w",
    "llvm.mips.ceq.b",
    "llvm.mips.ceq.d",
    "llvm.mips.ceq.h",
    "llvm.mips.ceq.w",
    "llvm.mips.ceqi.b",
    "llvm.mips.ceqi.d",
    "llvm.mips.ceqi.h",
    "llvm.mips.ceqi.w",
    "llvm.mips.cfcmsa",
    "llvm.mips.cle.s.b",
    "llvm.mips.cle.s.d",
    "llvm.mips.cle.s.h",
    "llvm.mips.cle.s.w",
    "llvm.mips.cle.u.b",
    "llvm.mips.cle.u.d",
    "llvm.mips.cle.u.h",
    "llvm.mips.cle.u.w",
    "llvm.mips.clei.s.b",
    "llvm.mips.clei.s.d",
    "llvm.mips.clei.s.h",
    "llvm.mips.clei.s.w",
    "llvm.mips.clei.u.b",
    "llvm.mips.clei.u.d",
    "llvm.mips.clei.u.h",
    "llvm.mips.clei.u.w",
    "llvm.mips.clt.s.b",
    "llvm.mips.clt.s.d",
    "llvm.mips.clt.s.h",
    "llvm.mips.clt.s.w",
    "llvm.mips.clt.u.b",
    "llvm.mips.clt.u.d",
    "llvm.mips.clt.u.h",
    "llvm.mips.clt.u.w",
    "llvm.mips.clti.s.b",
    "llvm.mips.clti.s.d",
    "llvm.mips.clti.s.h",
    "llvm.mips.clti.s.w",
    "llvm.mips.clti.u.b",
    "llvm.mips.clti.u.d",
    "llvm.mips.clti.u.h",
    "llvm.mips.clti.u.w",
    "llvm.mips.cmp.eq.ph",
    "llvm.mips.cmp.le.ph",
    "llvm.mips.cmp.lt.ph",
    "llvm.mips.cmpgdu.eq.qb",
    "llvm.mips.cmpgdu.le.qb",
    "llvm.mips.cmpgdu.lt.qb",
    "llvm.mips.cmpgu.eq.qb",
    "llvm.mips.cmpgu.le.qb",
    "llvm.mips.cmpgu.lt.qb",
    "llvm.mips.cmpu.eq.qb",
    "llvm.mips.cmpu.le.qb",
    "llvm.mips.cmpu.lt.qb",
    "llvm.mips.copy.s.b",
    "llvm.mips.copy.s.d",
    "llvm.mips.copy.s.h",
    "llvm.mips.copy.s.w",
    "llvm.mips.copy.u.b",
    "llvm.mips.copy.u.d",
    "llvm.mips.copy.u.h",
    "llvm.mips.copy.u.w",
    "llvm.mips.ctcmsa",
    "llvm.mips.div.s.b",
    "llvm.mips.div.s.d",
    "llvm.mips.div.s.h",
    "llvm.mips.div.s.w",
    "llvm.mips.div.u.b",
    "llvm.mips.div.u.d",
    "llvm.mips.div.u.h",
    "llvm.mips.div.u.w",
    "llvm.mips.dlsa",
    "llvm.mips.dotp.s.d",
    "llvm.mips.dotp.s.h",
    "llvm.mips.dotp.s.w",
    "llvm.mips.dotp.u.d",
    "llvm.mips.dotp.u.h",
    "llvm.mips.dotp.u.w",
    "llvm.mips.dpa.w.ph",
    "llvm.mips.dpadd.s.d",
    "llvm.mips.dpadd.s.h",
    "llvm.mips.dpadd.s.w",
    "llvm.mips.dpadd.u.d",
    "llvm.mips.dpadd.u.h",
    "llvm.mips.dpadd.u.w",
    "llvm.mips.dpaq.s.w.ph",
    "llvm.mips.dpaq.sa.l.w",
    "llvm.mips.dpaqx.s.w.ph",
    "llvm.mips.dpaqx.sa.w.ph",
    "llvm.mips.dpau.h.qbl",
    "llvm.mips.dpau.h.qbr",
    "llvm.mips.dpax.w.ph",
    "llvm.mips.dps.w.ph",
    "llvm.mips.dpsq.s.w.ph",
    "llvm.mips.dpsq.sa.l.w",
    "llvm.mips.dpsqx.s.w.ph",
    "llvm.mips.dpsqx.sa.w.ph",
    "llvm.mips.dpsu.h.qbl",
    "llvm.mips.dpsu.h.qbr",
    "llvm.mips.dpsub.s.d",
    "llvm.mips.dpsub.s.h",
    "llvm.mips.dpsub.s.w",
    "llvm.mips.dpsub.u.d",
    "llvm.mips.dpsub.u.h",
    "llvm.mips.dpsub.u.w",
    "llvm.mips.dpsx.w.ph",
    "llvm.mips.extp",
    "llvm.mips.extpdp",
    "llvm.mips.extr.r.w",
    "llvm.mips.extr.rs.w",
    "llvm.mips.extr.s.h",
    "llvm.mips.extr.w",
    "llvm.mips.fadd.d",
    "llvm.mips.fadd.w",
    "llvm.mips.fcaf.d",
    "llvm.mips.fcaf.w",
    "llvm.mips.fceq.d",
    "llvm.mips.fceq.w",
    "llvm.mips.fclass.d",
    "llvm.mips.fclass.w",
    "llvm.mips.fcle.d",
    "llvm.mips.fcle.w",
    "llvm.mips.fclt.d",
    "llvm.mips.fclt.w",
    "llvm.mips.fcne.d",
    "llvm.mips.fcne.w",
    "llvm.mips.fcor.d",
    "llvm.mips.fcor.w",
    "llvm.mips.fcueq.d",
    "llvm.mips.fcueq.w",
    "llvm.mips.fcule.d",
    "llvm.mips.fcule.w",
    "llvm.mips.fcult.d",
    "llvm.mips.fcult.w",
    "llvm.mips.fcun.d",
    "llvm.mips.fcun.w",
    "llvm.mips.fcune.d",
    "llvm.mips.fcune.w",
    "llvm.mips.fdiv.d",
    "llvm.mips.fdiv.w",
    "llvm.mips.fexdo.h",
    "llvm.mips.fexdo.w",
    "llvm.mips.fexp2.d",
    "llvm.mips.fexp2.w",
    "llvm.mips.fexupl.d",
    "llvm.mips.fexupl.w",
    "llvm.mips.fexupr.d",
    "llvm.mips.fexupr.w",
    "llvm.mips.ffint.s.d",
    "llvm.mips.ffint.s.w",
    "llvm.mips.ffint.u.d",
    "llvm.mips.ffint.u.w",
    "llvm.mips.ffql.d",
    "llvm.mips.ffql.w",
    "llvm.mips.ffqr.d",
    "llvm.mips.ffqr.w",
    "llvm.mips.fill.b",
    "llvm.mips.fill.d",
    "llvm.mips.fill.h",
    "llvm.mips.fill.w",
    "llvm.mips.flog2.d",
    "llvm.mips.flog2.w",
    "llvm.mips.fmadd.d",
    "llvm.mips.fmadd.w",
    "llvm.mips.fmax.a.d",
    "llvm.mips.fmax.a.w",
    "llvm.mips.fmax.d",
    "llvm.mips.fmax.w",
    "llvm.mips.fmin.a.d",
    "llvm.mips.fmin.a.w",
    "llvm.mips.fmin.d",
    "llvm.mips.fmin.w",
    "llvm.mips.fmsub.d",
    "llvm.mips.fmsub.w",
    "llvm.mips.fmul.d",
    "llvm.mips.fmul.w",
    "llvm.mips.frcp.d",
    "llvm.mips.frcp.w",
    "llvm.mips.frint.d",
    "llvm.mips.frint.w",
    "llvm.mips.frsqrt.d",
    "llvm.mips.frsqrt.w",
    "llvm.mips.fsaf.d",
    "llvm.mips.fsaf.w",
    "llvm.mips.fseq.d",
    "llvm.mips.fseq.w",
    "llvm.mips.fsle.d",
    "llvm.mips.fsle.w",
    "llvm.mips.fslt.d",
    "llvm.mips.fslt.w",
    "llvm.mips.fsne.d",
    "llvm.mips.fsne.w",
    "llvm.mips.fsor.d",
    "llvm.mips.fsor.w",
    "llvm.mips.fsqrt.d",
    "llvm.mips.fsqrt.w",
    "llvm.mips.fsub.d",
    "llvm.mips.fsub.w",
    "llvm.mips.fsueq.d",
    "llvm.mips.fsueq.w",
    "llvm.mips.fsule.d",
    "llvm.mips.fsule.w",
    "llvm.mips.fsult.d",
    "llvm.mips.fsult.w",
    "llvm.mips.fsun.d",
    "llvm.mips.fsun.w",
    "llvm.mips.fsune.d",
    "llvm.mips.fsune.w",
    "llvm.mips.ftint.s.d",
    "llvm.mips.ftint.s.w",
    "llvm.mips.ftint.u.d",
    "llvm.mips.ftint.u.w",
    "llvm.mips.ftq.h",
    "llvm.mips.ftq.w",
    "llvm.mips.ftrunc.s.d",
    "llvm.mips.ftrunc.s.w",
    "llvm.mips.ftrunc.u.d",
    "llvm.mips.ftrunc.u.w",
    "llvm.mips.hadd.s.d",
    "llvm.mips.hadd.s.h",
    "llvm.mips.hadd.s.w",
    "llvm.mips.hadd.u.d",
    "llvm.mips.hadd.u.h",
    "llvm.mips.hadd.u.w",
    "llvm.mips.hsub.s.d",
    "llvm.mips.hsub.s.h",
    "llvm.mips.hsub.s.w",
    "llvm.mips.hsub.u.d",
    "llvm.mips.hsub.u.h",
    "llvm.mips.hsub.u.w",
    "llvm.mips.ilvev.b",
    "llvm.mips.ilvev.d",
    "llvm.mips.ilvev.h",
    "llvm.mips.ilvev.w",
    "llvm.mips.ilvl.b",
    "llvm.mips.ilvl.d",
    "llvm.mips.ilvl.h",
    "llvm.mips.ilvl.w",
    "llvm.mips.ilvod.b",
    "llvm.mips.ilvod.d",
    "llvm.mips.ilvod.h",
    "llvm.mips.ilvod.w",
    "llvm.mips.ilvr.b",
    "llvm.mips.ilvr.d",
    "llvm.mips.ilvr.h",
    "llvm.mips.ilvr.w",
    "llvm.mips.insert.b",
    "llvm.mips.insert.d",
    "llvm.mips.insert.h",
    "llvm.mips.insert.w",
    "llvm.mips.insv",
    "llvm.mips.insve.b",
    "llvm.mips.insve.d",
    "llvm.mips.insve.h",
    "llvm.mips.insve.w",
    "llvm.mips.lbux",
    "llvm.mips.ld.b",
    "llvm.mips.ld.d",
    "llvm.mips.ld.h",
    "llvm.mips.ld.w",
    "llvm.mips.ldi.b",
    "llvm.mips.ldi.d",
    "llvm.mips.ldi.h",
    "llvm.mips.ldi.w",
    "llvm.mips.lhx",
    "llvm.mips.lsa",
    "llvm.mips.lwx",
    "llvm.mips.madd",
    "llvm.mips.madd.q.h",
    "llvm.mips.madd.q.w",
    "llvm.mips.maddr.q.h",
    "llvm.mips.maddr.q.w",
    "llvm.mips.maddu",
    "llvm.mips.maddv.b",
    "llvm.mips.maddv.d",
    "llvm.mips.maddv.h",
    "llvm.mips.maddv.w",
    "llvm.mips.maq.s.w.phl",
    "llvm.mips.maq.s.w.phr",
    "llvm.mips.maq.sa.w.phl",
    "llvm.mips.maq.sa.w.phr",
    "llvm.mips.max.a.b",
    "llvm.mips.max.a.d",
    "llvm.mips.max.a.h",
    "llvm.mips.max.a.w",
    "llvm.mips.max.s.b",
    "llvm.mips.max.s.d",
    "llvm.mips.max.s.h",
    "llvm.mips.max.s.w",
    "llvm.mips.max.u.b",
    "llvm.mips.max.u.d",
    "llvm.mips.max.u.h",
    "llvm.mips.max.u.w",
    "llvm.mips.maxi.s.b",
    "llvm.mips.maxi.s.d",
    "llvm.mips.maxi.s.h",
    "llvm.mips.maxi.s.w",
    "llvm.mips.maxi.u.b",
    "llvm.mips.maxi.u.d",
    "llvm.mips.maxi.u.h",
    "llvm.mips.maxi.u.w",
    "llvm.mips.min.a.b",
    "llvm.mips.min.a.d",
    "llvm.mips.min.a.h",
    "llvm.mips.min.a.w",
    "llvm.mips.min.s.b",
    "llvm.mips.min.s.d",
    "llvm.mips.min.s.h",
    "llvm.mips.min.s.w",
    "llvm.mips.min.u.b",
    "llvm.mips.min.u.d",
    "llvm.mips.min.u.h",
    "llvm.mips.min.u.w",
    "llvm.mips.mini.s.b",
    "llvm.mips.mini.s.d",
    "llvm.mips.mini.s.h",
    "llvm.mips.mini.s.w",
    "llvm.mips.mini.u.b",
    "llvm.mips.mini.u.d",
    "llvm.mips.mini.u.h",
    "llvm.mips.mini.u.w",
    "llvm.mips.mod.s.b",
    "llvm.mips.mod.s.d",
    "llvm.mips.mod.s.h",
    "llvm.mips.mod.s.w",
    "llvm.mips.mod.u.b",
    "llvm.mips.mod.u.d",
    "llvm.mips.mod.u.h",
    "llvm.mips.mod.u.w",
    "llvm.mips.modsub",
    "llvm.mips.move.v",
    "llvm.mips.msub",
    "llvm.mips.msub.q.h",
    "llvm.mips.msub.q.w",
    "llvm.mips.msubr.q.h",
    "llvm.mips.msubr.q.w",
    "llvm.mips.msubu",
    "llvm.mips.msubv.b",
    "llvm.mips.msubv.d",
    "llvm.mips.msubv.h",
    "llvm.mips.msubv.w",
    "llvm.mips.mthlip",
    "llvm.mips.mul.ph",
    "llvm.mips.mul.q.h",
    "llvm.mips.mul.q.w",
    "llvm.mips.mul.s.ph",
    "llvm.mips.muleq.s.w.phl",
    "llvm.mips.muleq.s.w.phr",
    "llvm.mips.muleu.s.ph.qbl",
    "llvm.mips.muleu.s.ph.qbr",
    "llvm.mips.mulq.rs.ph",
    "llvm.mips.mulq.rs.w",
    "llvm.mips.mulq.s.ph",
    "llvm.mips.mulq.s.w",
    "llvm.mips.mulr.q.h",
    "llvm.mips.mulr.q.w",
    "llvm.mips.mulsa.w.ph",
    "llvm.mips.mulsaq.s.w.ph",
    "llvm.mips.mult",
    "llvm.mips.multu",
    "llvm.mips.mulv.b",
    "llvm.mips.mulv.d",
    "llvm.mips.mulv.h",
    "llvm.mips.mulv.w",
    "llvm.mips.nloc.b",
    "llvm.mips.nloc.d",
    "llvm.mips.nloc.h",
    "llvm.mips.nloc.w",
    "llvm.mips.nlzc.b",
    "llvm.mips.nlzc.d",
    "llvm.mips.nlzc.h",
    "llvm.mips.nlzc.w",
    "llvm.mips.nor.v",
    "llvm.mips.nori.b",
    "llvm.mips.or.v",
    "llvm.mips.ori.b",
    "llvm.mips.packrl.ph",
    "llvm.mips.pckev.b",
    "llvm.mips.pckev.d",
    "llvm.mips.pckev.h",
    "llvm.mips.pckev.w",
    "llvm.mips.pckod.b",
    "llvm.mips.pckod.d",
    "llvm.mips.pckod.h",
    "llvm.mips.pckod.w",
    "llvm.mips.pcnt.b",
    "llvm.mips.pcnt.d",
    "llvm.mips.pcnt.h",
    "llvm.mips.pcnt.w",
    "llvm.mips.pick.ph",
    "llvm.mips.pick.qb",
    "llvm.mips.preceq.w.phl",
    "llvm.mips.preceq.w.phr",
    "llvm.mips.precequ.ph.qbl",
    "llvm.mips.precequ.ph.qbla",
    "llvm.mips.precequ.ph.qbr",
    "llvm.mips.precequ.ph.qbra",
    "llvm.mips.preceu.ph.qbl",
    "llvm.mips.preceu.ph.qbla",
    "llvm.mips.preceu.ph.qbr",
    "llvm.mips.preceu.ph.qbra",
    "llvm.mips.precr.qb.ph",
    "llvm.mips.precr.sra.ph.w",
    "llvm.mips.precr.sra.r.ph.w",
    "llvm.mips.precrq.ph.w",
    "llvm.mips.precrq.qb.ph",
    "llvm.mips.precrq.rs.ph.w",
    "llvm.mips.precrqu.s.qb.ph",
    "llvm.mips.prepend",
    "llvm.mips.raddu.w.qb",
    "llvm.mips.rddsp",
    "llvm.mips.repl.ph",
    "llvm.mips.repl.qb",
    "llvm.mips.sat.s.b",
    "llvm.mips.sat.s.d",
    "llvm.mips.sat.s.h",
    "llvm.mips.sat.s.w",
    "llvm.mips.sat.u.b",
    "llvm.mips.sat.u.d",
    "llvm.mips.sat.u.h",
    "llvm.mips.sat.u.w",
    "llvm.mips.shf.b",
    "llvm.mips.shf.h",
    "llvm.mips.shf.w",
    "llvm.mips.shilo",
    "llvm.mips.shll.ph",
    "llvm.mips.shll.qb",
    "llvm.mips.shll.s.ph",
    "llvm.mips.shll.s.w",
    "llvm.mips.shra.ph",
    "llvm.mips.shra.qb",
    "llvm.mips.shra.r.ph",
    "llvm.mips.shra.r.qb",
    "llvm.mips.shra.r.w",
    "llvm.mips.shrl.ph",
    "llvm.mips.shrl.qb",
    "llvm.mips.sld.b",
    "llvm.mips.sld.d",
    "llvm.mips.sld.h",
    "llvm.mips.sld.w",
    "llvm.mips.sldi.b",
    "llvm.mips.sldi.d",
    "llvm.mips.sldi.h",
    "llvm.mips.sldi.w",
    "llvm.mips.sll.b",
    "llvm.mips.sll.d",
    "llvm.mips.sll.h",
    "llvm.mips.sll.w",
    "llvm.mips.slli.b",
    "llvm.mips.slli.d",
    "llvm.mips.slli.h",
    "llvm.mips.slli.w",
    "llvm.mips.splat.b",
    "llvm.mips.splat.d",
    "llvm.mips.splat.h",
    "llvm.mips.splat.w",
    "llvm.mips.splati.b",
    "llvm.mips.splati.d",
    "llvm.mips.splati.h",
    "llvm.mips.splati.w",
    "llvm.mips.sra.b",
    "llvm.mips.sra.d",
    "llvm.mips.sra.h",
    "llvm.mips.sra.w",
    "llvm.mips.srai.b",
    "llvm.mips.srai.d",
    "llvm.mips.srai.h",
    "llvm.mips.srai.w",
    "llvm.mips.srar.b",
    "llvm.mips.srar.d",
    "llvm.mips.srar.h",
    "llvm.mips.srar.w",
    "llvm.mips.srari.b",
    "llvm.mips.srari.d",
    "llvm.mips.srari.h",
    "llvm.mips.srari.w",
    "llvm.mips.srl.b",
    "llvm.mips.srl.d",
    "llvm.mips.srl.h",
    "llvm.mips.srl.w",
    "llvm.mips.srli.b",
    "llvm.mips.srli.d",
    "llvm.mips.srli.h",
    "llvm.mips.srli.w",
    "llvm.mips.srlr.b",
    "llvm.mips.srlr.d",
    "llvm.mips.srlr.h",
    "llvm.mips.srlr.w",
    "llvm.mips.srlri.b",
    "llvm.mips.srlri.d",
    "llvm.mips.srlri.h",
    "llvm.mips.srlri.w",
    "llvm.mips.st.b",
    "llvm.mips.st.d",
    "llvm.mips.st.h",
    "llvm.mips.st.w",
    "llvm.mips.subq.ph",
    "llvm.mips.subq.s.ph",
    "llvm.mips.subq.s.w",
    "llvm.mips.subqh.ph",
    "llvm.mips.subqh.r.ph",
    "llvm.mips.subqh.r.w",
    "llvm.mips.subqh.w",
    "llvm.mips.subs.s.b",
    "llvm.mips.subs.s.d",
    "llvm.mips.subs.s.h",
    "llvm.mips.subs.s.w",
    "llvm.mips.subs.u.b",
    "llvm.mips.subs.u.d",
    "llvm.mips.subs.u.h",
    "llvm.mips.subs.u.w",
    "llvm.mips.subsus.u.b",
    "llvm.mips.subsus.u.d",
    "llvm.mips.subsus.u.h",
    "llvm.mips.subsus.u.w",
    "llvm.mips.subsuu.s.b",
    "llvm.mips.subsuu.s.d",
    "llvm.mips.subsuu.s.h",
    "llvm.mips.subsuu.s.w",
    "llvm.mips.subu.ph",
    "llvm.mips.subu.qb",
    "llvm.mips.subu.s.ph",
    "llvm.mips.subu.s.qb",
    "llvm.mips.subuh.qb",
    "llvm.mips.subuh.r.qb",
    "llvm.mips.subv.b",
    "llvm.mips.subv.d",
    "llvm.mips.subv.h",
    "llvm.mips.subv.w",
    "llvm.mips.subvi.b",
    "llvm.mips.subvi.d",
    "llvm.mips.subvi.h",
    "llvm.mips.subvi.w",
    "llvm.mips.vshf.b",
    "llvm.mips.vshf.d",
    "llvm.mips.vshf.h",
    "llvm.mips.vshf.w",
    "llvm.mips.wrdsp",
    "llvm.mips.xor.v",
    "llvm.mips.xori.b",
    "llvm.nvvm.add.rm.d",
    "llvm.nvvm.add.rm.f",
    "llvm.nvvm.add.rm.ftz.f",
    "llvm.nvvm.add.rn.d",
    "llvm.nvvm.add.rn.f",
    "llvm.nvvm.add.rn.ftz.f",
    "llvm.nvvm.add.rp.d",
    "llvm.nvvm.add.rp.f",
    "llvm.nvvm.add.rp.ftz.f",
    "llvm.nvvm.add.rz.d",
    "llvm.nvvm.add.rz.f",
    "llvm.nvvm.add.rz.ftz.f",
    "llvm.nvvm.atomic.add.gen.f.cta",
    "llvm.nvvm.atomic.add.gen.f.sys",
    "llvm.nvvm.atomic.add.gen.i.cta",
    "llvm.nvvm.atomic.add.gen.i.sys",
    "llvm.nvvm.atomic.and.gen.i.cta",
    "llvm.nvvm.atomic.and.gen.i.sys",
    "llvm.nvvm.atomic.cas.gen.i.cta",
    "llvm.nvvm.atomic.cas.gen.i.sys",
    "llvm.nvvm.atomic.dec.gen.i.cta",
    "llvm.nvvm.atomic.dec.gen.i.sys",
    "llvm.nvvm.atomic.exch.gen.i.cta",
    "llvm.nvvm.atomic.exch.gen.i.sys",
    "llvm.nvvm.atomic.inc.gen.i.cta",
    "llvm.nvvm.atomic.inc.gen.i.sys",
    "llvm.nvvm.atomic.load.add.f32",
    "llvm.nvvm.atomic.load.add.f64",
    "llvm.nvvm.atomic.load.dec.32",
    "llvm.nvvm.atomic.load.inc.32",
    "llvm.nvvm.atomic.max.gen.i.cta",
    "llvm.nvvm.atomic.max.gen.i.sys",
    "llvm.nvvm.atomic.min.gen.i.cta",
    "llvm.nvvm.atomic.min.gen.i.sys",
    "llvm.nvvm.atomic.or.gen.i.cta",
    "llvm.nvvm.atomic.or.gen.i.sys",
    "llvm.nvvm.atomic.xor.gen.i.cta",
    "llvm.nvvm.atomic.xor.gen.i.sys",
    "llvm.nvvm.bar.sync",
    "llvm.nvvm.bar.warp.sync",
    "llvm.nvvm.barrier",
    "llvm.nvvm.barrier.n",
    "llvm.nvvm.barrier.sync",
    "llvm.nvvm.barrier.sync.cnt",
    "llvm.nvvm.barrier0",
    "llvm.nvvm.barrier0.and",
    "llvm.nvvm.barrier0.or",
    "llvm.nvvm.barrier0.popc",
    "llvm.nvvm.bitcast.d2ll",
    "llvm.nvvm.bitcast.f2i",
    "llvm.nvvm.bitcast.i2f",
    "llvm.nvvm.bitcast.ll2d",
    "llvm.nvvm.ceil.d",
    "llvm.nvvm.ceil.f",
    "llvm.nvvm.ceil.ftz.f",
    "llvm.nvvm.compiler.error",
    "llvm.nvvm.compiler.warn",
    "llvm.nvvm.cos.approx.f",
    "llvm.nvvm.cos.approx.ftz.f",
    "llvm.nvvm.d2f.rm",
    "llvm.nvvm.d2f.rm.ftz",
    "llvm.nvvm.d2f.rn",
    "llvm.nvvm.d2f.rn.ftz",
    "llvm.nvvm.d2f.rp",
    "llvm.nvvm.d2f.rp.ftz",
    "llvm.nvvm.d2f.rz",
    "llvm.nvvm.d2f.rz.ftz",
    "llvm.nvvm.d2i.hi",
    "llvm.nvvm.d2i.lo",
    "llvm.nvvm.d2i.rm",
    "llvm.nvvm.d2i.rn",
    "llvm.nvvm.d2i.rp",
    "llvm.nvvm.d2i.rz",
    "llvm.nvvm.d2ll.rm",
    "llvm.nvvm.d2ll.rn",
    "llvm.nvvm.d2ll.rp",
    "llvm.nvvm.d2ll.rz",
    "llvm.nvvm.d2ui.rm",
    "llvm.nvvm.d2ui.rn",
    "llvm.nvvm.d2ui.rp",
    "llvm.nvvm.d2ui.rz",
    "llvm.nvvm.d2ull.rm",
    "llvm.nvvm.d2ull.rn",
    "llvm.nvvm.d2ull.rp",
    "llvm.nvvm.d2ull.rz",
    "llvm.nvvm.div.approx.f",
    "llvm.nvvm.div.approx.ftz.f",
    "llvm.nvvm.div.rm.d",
    "llvm.nvvm.div.rm.f",
    "llvm.nvvm.div.rm.ftz.f",
    "llvm.nvvm.div.rn.d",
    "llvm.nvvm.div.rn.f",
    "llvm.nvvm.div.rn.ftz.f",
    "llvm.nvvm.div.rp.d",
    "llvm.nvvm.div.rp.f",
    "llvm.nvvm.div.rp.ftz.f",
    "llvm.nvvm.div.rz.d",
    "llvm.nvvm.div.rz.f",
    "llvm.nvvm.div.rz.ftz.f",
    "llvm.nvvm.ex2.approx.d",
    "llvm.nvvm.ex2.approx.f",
    "llvm.nvvm.ex2.approx.ftz.f",
    "llvm.nvvm.f2h.rn",
    "llvm.nvvm.f2h.rn.ftz",
    "llvm.nvvm.f2i.rm",
    "llvm.nvvm.f2i.rm.ftz",
    "llvm.nvvm.f2i.rn",
    "llvm.nvvm.f2i.rn.ftz",
    "llvm.nvvm.f2i.rp",
    "llvm.nvvm.f2i.rp.ftz",
    "llvm.nvvm.f2i.rz",
    "llvm.nvvm.f2i.rz.ftz",
    "llvm.nvvm.f2ll.rm",
    "llvm.nvvm.f2ll.rm.ftz",
    "llvm.nvvm.f2ll.rn",
    "llvm.nvvm.f2ll.rn.ftz",
    "llvm.nvvm.f2ll.rp",
    "llvm.nvvm.f2ll.rp.ftz",
    "llvm.nvvm.f2ll.rz",
    "llvm.nvvm.f2ll.rz.ftz",
    "llvm.nvvm.f2ui.rm",
    "llvm.nvvm.f2ui.rm.ftz",
    "llvm.nvvm.f2ui.rn",
    "llvm.nvvm.f2ui.rn.ftz",
    "llvm.nvvm.f2ui.rp",
    "llvm.nvvm.f2ui.rp.ftz",
    "llvm.nvvm.f2ui.rz",
    "llvm.nvvm.f2ui.rz.ftz",
    "llvm.nvvm.f2ull.rm",
    "llvm.nvvm.f2ull.rm.ftz",
    "llvm.nvvm.f2ull.rn",
    "llvm.nvvm.f2ull.rn.ftz",
    "llvm.nvvm.f2ull.rp",
    "llvm.nvvm.f2ull.rp.ftz",
    "llvm.nvvm.f2ull.rz",
    "llvm.nvvm.f2ull.rz.ftz",
    "llvm.nvvm.fabs.d",
    "llvm.nvvm.fabs.f",
    "llvm.nvvm.fabs.ftz.f",
    "llvm.nvvm.floor.d",
    "llvm.nvvm.floor.f",
    "llvm.nvvm.floor.ftz.f",
    "llvm.nvvm.fma.rm.d",
    "llvm.nvvm.fma.rm.f",
    "llvm.nvvm.fma.rm.ftz.f",
    "llvm.nvvm.fma.rn.d",
    "llvm.nvvm.fma.rn.f",
    "llvm.nvvm.fma.rn.ftz.f",
    "llvm.nvvm.fma.rp.d",
    "llvm.nvvm.fma.rp.f",
    "llvm.nvvm.fma.rp.ftz.f",
    "llvm.nvvm.fma.rz.d",
    "llvm.nvvm.fma.rz.f",
    "llvm.nvvm.fma.rz.ftz.f",
    "llvm.nvvm.fmax.d",
    "llvm.nvvm.fmax.f",
    "llvm.nvvm.fmax.ftz.f",
    "llvm.nvvm.fmin.d",
    "llvm.nvvm.fmin.f",
    "llvm.nvvm.fmin.ftz.f",
    "llvm.nvvm.fns",
    "llvm.nvvm.i2d.rm",
    "llvm.nvvm.i2d.rn",
    "llvm.nvvm.i2d.rp",
    "llvm.nvvm.i2d.rz",
    "llvm.nvvm.i2f.rm",
    "llvm.nvvm.i2f.rn",
    "llvm.nvvm.i2f.rp",
    "llvm.nvvm.i2f.rz",
    "llvm.nvvm.isspacep.const",
    "llvm.nvvm.isspacep.global",
    "llvm.nvvm.isspacep.local",
    "llvm.nvvm.isspacep.shared",
    "llvm.nvvm.istypep.sampler",
    "llvm.nvvm.istypep.surface",
    "llvm.nvvm.istypep.texture",
    "llvm.nvvm.ldg.global.f",
    "llvm.nvvm.ldg.global.i",
    "llvm.nvvm.ldg.global.p",
    "llvm.nvvm.ldu.global.f",
    "llvm.nvvm.ldu.global.i",
    "llvm.nvvm.ldu.global.p",
    "llvm.nvvm.lg2.approx.d",
    "llvm.nvvm.lg2.approx.f",
    "llvm.nvvm.lg2.approx.ftz.f",
    "llvm.nvvm.ll2d.rm",
    "llvm.nvvm.ll2d.rn",
    "llvm.nvvm.ll2d.rp",
    "llvm.nvvm.ll2d.rz",
    "llvm.nvvm.ll2f.rm",
    "llvm.nvvm.ll2f.rn",
    "llvm.nvvm.ll2f.rp",
    "llvm.nvvm.ll2f.rz",
    "llvm.nvvm.lohi.i2d",
    "llvm.nvvm.match.all.sync.i32p",
    "llvm.nvvm.match.all.sync.i64p",
    "llvm.nvvm.match.any.sync.i32",
    "llvm.nvvm.match.any.sync.i64",
    "llvm.nvvm.membar.cta",
    "llvm.nvvm.membar.gl",
    "llvm.nvvm.membar.sys",
    "llvm.nvvm.move.double",
    "llvm.nvvm.move.float",
    "llvm.nvvm.move.i16",
    "llvm.nvvm.move.i32",
    "llvm.nvvm.move.i64",
    "llvm.nvvm.move.ptr",
    "llvm.nvvm.mul.rm.d",
    "llvm.nvvm.mul.rm.f",
    "llvm.nvvm.mul.rm.ftz.f",
    "llvm.nvvm.mul.rn.d",
    "llvm.nvvm.mul.rn.f",
    "llvm.nvvm.mul.rn.ftz.f",
    "llvm.nvvm.mul.rp.d",
    "llvm.nvvm.mul.rp.f",
    "llvm.nvvm.mul.rp.ftz.f",
    "llvm.nvvm.mul.rz.d",
    "llvm.nvvm.mul.rz.f",
    "llvm.nvvm.mul.rz.ftz.f",
    "llvm.nvvm.mul24.i",
    "llvm.nvvm.mul24.ui",
    "llvm.nvvm.mulhi.i",
    "llvm.nvvm.mulhi.ll",
    "llvm.nvvm.mulhi.ui",
    "llvm.nvvm.mulhi.ull",
    "llvm.nvvm.prmt",
    "llvm.nvvm.ptr.constant.to.gen",
    "llvm.nvvm.ptr.gen.to.constant",
    "llvm.nvvm.ptr.gen.to.global",
    "llvm.nvvm.ptr.gen.to.local",
    "llvm.nvvm.ptr.gen.to.param",
    "llvm.nvvm.ptr.gen.to.shared",
    "llvm.nvvm.ptr.global.to.gen",
    "llvm.nvvm.ptr.local.to.gen",
    "llvm.nvvm.ptr.shared.to.gen",
    "llvm.nvvm.rcp.approx.ftz.d",
    "llvm.nvvm.rcp.rm.d",
    "llvm.nvvm.rcp.rm.f",
    "llvm.nvvm.rcp.rm.ftz.f",
    "llvm.nvvm.rcp.rn.d",
    "llvm.nvvm.rcp.rn.f",
    "llvm.nvvm.rcp.rn.ftz.f",
    "llvm.nvvm.rcp.rp.d",
    "llvm.nvvm.rcp.rp.f",
    "llvm.nvvm.rcp.rp.ftz.f",
    "llvm.nvvm.rcp.rz.d",
    "llvm.nvvm.rcp.rz.f",
    "llvm.nvvm.rcp.rz.ftz.f",
    "llvm.nvvm.read.ptx.sreg.clock",
    "llvm.nvvm.read.ptx.sreg.clock64",
    "llvm.nvvm.read.ptx.sreg.ctaid.w",
    "llvm.nvvm.read.ptx.sreg.ctaid.x",
    "llvm.nvvm.read.ptx.sreg.ctaid.y",
    "llvm.nvvm.read.ptx.sreg.ctaid.z",
    "llvm.nvvm.read.ptx.sreg.envreg0",
    "llvm.nvvm.read.ptx.sreg.envreg1",
    "llvm.nvvm.read.ptx.sreg.envreg10",
    "llvm.nvvm.read.ptx.sreg.envreg11",
    "llvm.nvvm.read.ptx.sreg.envreg12",
    "llvm.nvvm.read.ptx.sreg.envreg13",
    "llvm.nvvm.read.ptx.sreg.envreg14",
    "llvm.nvvm.read.ptx.sreg.envreg15",
    "llvm.nvvm.read.ptx.sreg.envreg16",
    "llvm.nvvm.read.ptx.sreg.envreg17",
    "llvm.nvvm.read.ptx.sreg.envreg18",
    "llvm.nvvm.read.ptx.sreg.envreg19",
    "llvm.nvvm.read.ptx.sreg.envreg2",
    "llvm.nvvm.read.ptx.sreg.envreg20",
    "llvm.nvvm.read.ptx.sreg.envreg21",
    "llvm.nvvm.read.ptx.sreg.envreg22",
    "llvm.nvvm.read.ptx.sreg.envreg23",
    "llvm.nvvm.read.ptx.sreg.envreg24",
    "llvm.nvvm.read.ptx.sreg.envreg25",
    "llvm.nvvm.read.ptx.sreg.envreg26",
    "llvm.nvvm.read.ptx.sreg.envreg27",
    "llvm.nvvm.read.ptx.sreg.envreg28",
    "llvm.nvvm.read.ptx.sreg.envreg29",
    "llvm.nvvm.read.ptx.sreg.envreg3",
    "llvm.nvvm.read.ptx.sreg.envreg30",
    "llvm.nvvm.read.ptx.sreg.envreg31",
    "llvm.nvvm.read.ptx.sreg.envreg4",
    "llvm.nvvm.read.ptx.sreg.envreg5",
    "llvm.nvvm.read.ptx.sreg.envreg6",
    "llvm.nvvm.read.ptx.sreg.envreg7",
    "llvm.nvvm.read.ptx.sreg.envreg8",
    "llvm.nvvm.read.ptx.sreg.envreg9",
    "llvm.nvvm.read.ptx.sreg.gridid",
    "llvm.nvvm.read.ptx.sreg.laneid",
    "llvm.nvvm.read.ptx.sreg.lanemask.eq",
    "llvm.nvvm.read.ptx.sreg.lanemask.ge",
    "llvm.nvvm.read.ptx.sreg.lanemask.gt",
    "llvm.nvvm.read.ptx.sreg.lanemask.le",
    "llvm.nvvm.read.ptx.sreg.lanemask.lt",
    "llvm.nvvm.read.ptx.sreg.nctaid.w",
    "llvm.nvvm.read.ptx.sreg.nctaid.x",
    "llvm.nvvm.read.ptx.sreg.nctaid.y",
    "llvm.nvvm.read.ptx.sreg.nctaid.z",
    "llvm.nvvm.read.ptx.sreg.nsmid",
    "llvm.nvvm.read.ptx.sreg.ntid.w",
    "llvm.nvvm.read.ptx.sreg.ntid.x",
    "llvm.nvvm.read.ptx.sreg.ntid.y",
    "llvm.nvvm.read.ptx.sreg.ntid.z",
    "llvm.nvvm.read.ptx.sreg.nwarpid",
    "llvm.nvvm.read.ptx.sreg.pm0",
    "llvm.nvvm.read.ptx.sreg.pm1",
    "llvm.nvvm.read.ptx.sreg.pm2",
    "llvm.nvvm.read.ptx.sreg.pm3",
    "llvm.nvvm.read.ptx.sreg.smid",
    "llvm.nvvm.read.ptx.sreg.tid.w",
    "llvm.nvvm.read.ptx.sreg.tid.x",
    "llvm.nvvm.read.ptx.sreg.tid.y",
    "llvm.nvvm.read.ptx.sreg.tid.z",
    "llvm.nvvm.read.ptx.sreg.warpid",
    "llvm.nvvm.read.ptx.sreg.warpsize",
    "llvm.nvvm.reflect",
    "llvm.nvvm.rotate.b32",
    "llvm.nvvm.rotate.b64",
    "llvm.nvvm.rotate.right.b64",
    "llvm.nvvm.round.d",
    "llvm.nvvm.round.f",
    "llvm.nvvm.round.ftz.f",
    "llvm.nvvm.rsqrt.approx.d",
    "llvm.nvvm.rsqrt.approx.f",
    "llvm.nvvm.rsqrt.approx.ftz.f",
    "llvm.nvvm.sad.i",
    "llvm.nvvm.sad.ui",
    "llvm.nvvm.saturate.d",
    "llvm.nvvm.saturate.f",
    "llvm.nvvm.saturate.ftz.f",
    "llvm.nvvm.shfl.bfly.f32",
    "llvm.nvvm.shfl.bfly.i32",
    "llvm.nvvm.shfl.down.f32",
    "llvm.nvvm.shfl.down.i32",
    "llvm.nvvm.shfl.idx.f32",
    "llvm.nvvm.shfl.idx.i32",
    "llvm.nvvm.shfl.sync.bfly.f32",
    "llvm.nvvm.shfl.sync.bfly.i32",
    "llvm.nvvm.shfl.sync.down.f32",
    "llvm.nvvm.shfl.sync.down.i32",
    "llvm.nvvm.shfl.sync.idx.f32",
    "llvm.nvvm.shfl.sync.idx.i32",
    "llvm.nvvm.shfl.sync.up.f32",
    "llvm.nvvm.shfl.sync.up.i32",
    "llvm.nvvm.shfl.up.f32",
    "llvm.nvvm.shfl.up.i32",
    "llvm.nvvm.sin.approx.f",
    "llvm.nvvm.sin.approx.ftz.f",
    "llvm.nvvm.sqrt.approx.f",
    "llvm.nvvm.sqrt.approx.ftz.f",
    "llvm.nvvm.sqrt.f",
    "llvm.nvvm.sqrt.rm.d",
    "llvm.nvvm.sqrt.rm.f",
    "llvm.nvvm.sqrt.rm.ftz.f",
    "llvm.nvvm.sqrt.rn.d",
    "llvm.nvvm.sqrt.rn.f",
    "llvm.nvvm.sqrt.rn.ftz.f",
    "llvm.nvvm.sqrt.rp.d",
    "llvm.nvvm.sqrt.rp.f",
    "llvm.nvvm.sqrt.rp.ftz.f",
    "llvm.nvvm.sqrt.rz.d",
    "llvm.nvvm.sqrt.rz.f",
    "llvm.nvvm.sqrt.rz.ftz.f",
    "llvm.nvvm.suld.1d.array.i16.clamp",
    "llvm.nvvm.suld.1d.array.i16.trap",
    "llvm.nvvm.suld.1d.array.i16.zero",
    "llvm.nvvm.suld.1d.array.i32.clamp",
    "llvm.nvvm.suld.1d.array.i32.trap",
    "llvm.nvvm.suld.1d.array.i32.zero",
    "llvm.nvvm.suld.1d.array.i64.clamp",
    "llvm.nvvm.suld.1d.array.i64.trap",
    "llvm.nvvm.suld.1d.array.i64.zero",
    "llvm.nvvm.suld.1d.array.i8.clamp",
    "llvm.nvvm.suld.1d.array.i8.trap",
    "llvm.nvvm.suld.1d.array.i8.zero",
    "llvm.nvvm.suld.1d.array.v2i16.clamp",
    "llvm.nvvm.suld.1d.array.v2i16.trap",
    "llvm.nvvm.suld.1d.array.v2i16.zero",
    "llvm.nvvm.suld.1d.array.v2i32.clamp",
    "llvm.nvvm.suld.1d.array.v2i32.trap",
    "llvm.nvvm.suld.1d.array.v2i32.zero",
    "llvm.nvvm.suld.1d.array.v2i64.clamp",
    "llvm.nvvm.suld.1d.array.v2i64.trap",
    "llvm.nvvm.suld.1d.array.v2i64.zero",
    "llvm.nvvm.suld.1d.array.v2i8.clamp",
    "llvm.nvvm.suld.1d.array.v2i8.trap",
    "llvm.nvvm.suld.1d.array.v2i8.zero",
    "llvm.nvvm.suld.1d.array.v4i16.clamp",
    "llvm.nvvm.suld.1d.array.v4i16.trap",
    "llvm.nvvm.suld.1d.array.v4i16.zero",
    "llvm.nvvm.suld.1d.array.v4i32.clamp",
    "llvm.nvvm.suld.1d.array.v4i32.trap",
    "llvm.nvvm.suld.1d.array.v4i32.zero",
    "llvm.nvvm.suld.1d.array.v4i8.clamp",
    "llvm.nvvm.suld.1d.array.v4i8.trap",
    "llvm.nvvm.suld.1d.array.v4i8.zero",
    "llvm.nvvm.suld.1d.i16.clamp",
    "llvm.nvvm.suld.1d.i16.trap",
    "llvm.nvvm.suld.1d.i16.zero",
    "llvm.nvvm.suld.1d.i32.clamp",
    "llvm.nvvm.suld.1d.i32.trap",
    "llvm.nvvm.suld.1d.i32.zero",
    "llvm.nvvm.suld.1d.i64.clamp",
    "llvm.nvvm.suld.1d.i64.trap",
    "llvm.nvvm.suld.1d.i64.zero",
    "llvm.nvvm.suld.1d.i8.clamp",
    "llvm.nvvm.suld.1d.i8.trap",
    "llvm.nvvm.suld.1d.i8.zero",
    "llvm.nvvm.suld.1d.v2i16.clamp",
    "llvm.nvvm.suld.1d.v2i16.trap",
    "llvm.nvvm.suld.1d.v2i16.zero",
    "llvm.nvvm.suld.1d.v2i32.clamp",
    "llvm.nvvm.suld.1d.v2i32.trap",
    "llvm.nvvm.suld.1d.v2i32.zero",
    "llvm.nvvm.suld.1d.v2i64.clamp",
    "llvm.nvvm.suld.1d.v2i64.trap",
    "llvm.nvvm.suld.1d.v2i64.zero",
    "llvm.nvvm.suld.1d.v2i8.clamp",
    "llvm.nvvm.suld.1d.v2i8.trap",
    "llvm.nvvm.suld.1d.v2i8.zero",
    "llvm.nvvm.suld.1d.v4i16.clamp",
    "llvm.nvvm.suld.1d.v4i16.trap",
    "llvm.nvvm.suld.1d.v4i16.zero",
    "llvm.nvvm.suld.1d.v4i32.clamp",
    "llvm.nvvm.suld.1d.v4i32.trap",
    "llvm.nvvm.suld.1d.v4i32.zero",
    "llvm.nvvm.suld.1d.v4i8.clamp",
    "llvm.nvvm.suld.1d.v4i8.trap",
    "llvm.nvvm.suld.1d.v4i8.zero",
    "llvm.nvvm.suld.2d.array.i16.clamp",
    "llvm.nvvm.suld.2d.array.i16.trap",
    "llvm.nvvm.suld.2d.array.i16.zero",
    "llvm.nvvm.suld.2d.array.i32.clamp",
    "llvm.nvvm.suld.2d.array.i32.trap",
    "llvm.nvvm.suld.2d.array.i32.zero",
    "llvm.nvvm.suld.2d.array.i64.clamp",
    "llvm.nvvm.suld.2d.array.i64.trap",
    "llvm.nvvm.suld.2d.array.i64.zero",
    "llvm.nvvm.suld.2d.array.i8.clamp",
    "llvm.nvvm.suld.2d.array.i8.trap",
    "llvm.nvvm.suld.2d.array.i8.zero",
    "llvm.nvvm.suld.2d.array.v2i16.clamp",
    "llvm.nvvm.suld.2d.array.v2i16.trap",
    "llvm.nvvm.suld.2d.array.v2i16.zero",
    "llvm.nvvm.suld.2d.array.v2i32.clamp",
    "llvm.nvvm.suld.2d.array.v2i32.trap",
    "llvm.nvvm.suld.2d.array.v2i32.zero",
    "llvm.nvvm.suld.2d.array.v2i64.clamp",
    "llvm.nvvm.suld.2d.array.v2i64.trap",
    "llvm.nvvm.suld.2d.array.v2i64.zero",
    "llvm.nvvm.suld.2d.array.v2i8.clamp",
    "llvm.nvvm.suld.2d.array.v2i8.trap",
    "llvm.nvvm.suld.2d.array.v2i8.zero",
    "llvm.nvvm.suld.2d.array.v4i16.clamp",
    "llvm.nvvm.suld.2d.array.v4i16.trap",
    "llvm.nvvm.suld.2d.array.v4i16.zero",
    "llvm.nvvm.suld.2d.array.v4i32.clamp",
    "llvm.nvvm.suld.2d.array.v4i32.trap",
    "llvm.nvvm.suld.2d.array.v4i32.zero",
    "llvm.nvvm.suld.2d.array.v4i8.clamp",
    "llvm.nvvm.suld.2d.array.v4i8.trap",
    "llvm.nvvm.suld.2d.array.v4i8.zero",
    "llvm.nvvm.suld.2d.i16.clamp",
    "llvm.nvvm.suld.2d.i16.trap",
    "llvm.nvvm.suld.2d.i16.zero",
    "llvm.nvvm.suld.2d.i32.clamp",
    "llvm.nvvm.suld.2d.i32.trap",
    "llvm.nvvm.suld.2d.i32.zero",
    "llvm.nvvm.suld.2d.i64.clamp",
    "llvm.nvvm.suld.2d.i64.trap",
    "llvm.nvvm.suld.2d.i64.zero",
    "llvm.nvvm.suld.2d.i8.clamp",
    "llvm.nvvm.suld.2d.i8.trap",
    "llvm.nvvm.suld.2d.i8.zero",
    "llvm.nvvm.suld.2d.v2i16.clamp",
    "llvm.nvvm.suld.2d.v2i16.trap",
    "llvm.nvvm.suld.2d.v2i16.zero",
    "llvm.nvvm.suld.2d.v2i32.clamp",
    "llvm.nvvm.suld.2d.v2i32.trap",
    "llvm.nvvm.suld.2d.v2i32.zero",
    "llvm.nvvm.suld.2d.v2i64.clamp",
    "llvm.nvvm.suld.2d.v2i64.trap",
    "llvm.nvvm.suld.2d.v2i64.zero",
    "llvm.nvvm.suld.2d.v2i8.clamp",
    "llvm.nvvm.suld.2d.v2i8.trap",
    "llvm.nvvm.suld.2d.v2i8.zero",
    "llvm.nvvm.suld.2d.v4i16.clamp",
    "llvm.nvvm.suld.2d.v4i16.trap",
    "llvm.nvvm.suld.2d.v4i16.zero",
    "llvm.nvvm.suld.2d.v4i32.clamp",
    "llvm.nvvm.suld.2d.v4i32.trap",
    "llvm.nvvm.suld.2d.v4i32.zero",
    "llvm.nvvm.suld.2d.v4i8.clamp",
    "llvm.nvvm.suld.2d.v4i8.trap",
    "llvm.nvvm.suld.2d.v4i8.zero",
    "llvm.nvvm.suld.3d.i16.clamp",
    "llvm.nvvm.suld.3d.i16.trap",
    "llvm.nvvm.suld.3d.i16.zero",
    "llvm.nvvm.suld.3d.i32.clamp",
    "llvm.nvvm.suld.3d.i32.trap",
    "llvm.nvvm.suld.3d.i32.zero",
    "llvm.nvvm.suld.3d.i64.clamp",
    "llvm.nvvm.suld.3d.i64.trap",
    "llvm.nvvm.suld.3d.i64.zero",
    "llvm.nvvm.suld.3d.i8.clamp",
    "llvm.nvvm.suld.3d.i8.trap",
    "llvm.nvvm.suld.3d.i8.zero",
    "llvm.nvvm.suld.3d.v2i16.clamp",
    "llvm.nvvm.suld.3d.v2i16.trap",
    "llvm.nvvm.suld.3d.v2i16.zero",
    "llvm.nvvm.suld.3d.v2i32.clamp",
    "llvm.nvvm.suld.3d.v2i32.trap",
    "llvm.nvvm.suld.3d.v2i32.zero",
    "llvm.nvvm.suld.3d.v2i64.clamp",
    "llvm.nvvm.suld.3d.v2i64.trap",
    "llvm.nvvm.suld.3d.v2i64.zero",
    "llvm.nvvm.suld.3d.v2i8.clamp",
    "llvm.nvvm.suld.3d.v2i8.trap",
    "llvm.nvvm.suld.3d.v2i8.zero",
    "llvm.nvvm.suld.3d.v4i16.clamp",
    "llvm.nvvm.suld.3d.v4i16.trap",
    "llvm.nvvm.suld.3d.v4i16.zero",
    "llvm.nvvm.suld.3d.v4i32.clamp",
    "llvm.nvvm.suld.3d.v4i32.trap",
    "llvm.nvvm.suld.3d.v4i32.zero",
    "llvm.nvvm.suld.3d.v4i8.clamp",
    "llvm.nvvm.suld.3d.v4i8.trap",
    "llvm.nvvm.suld.3d.v4i8.zero",
    "llvm.nvvm.suq.array.size",
    "llvm.nvvm.suq.channel.data.type",
    "llvm.nvvm.suq.channel.order",
    "llvm.nvvm.suq.depth",
    "llvm.nvvm.suq.height",
    "llvm.nvvm.suq.width",
    "llvm.nvvm.sust.b.1d.array.i16.clamp",
    "llvm.nvvm.sust.b.1d.array.i16.trap",
    "llvm.nvvm.sust.b.1d.array.i16.zero",
    "llvm.nvvm.sust.b.1d.array.i32.clamp",
    "llvm.nvvm.sust.b.1d.array.i32.trap",
    "llvm.nvvm.sust.b.1d.array.i32.zero",
    "llvm.nvvm.sust.b.1d.array.i64.clamp",
    "llvm.nvvm.sust.b.1d.array.i64.trap",
    "llvm.nvvm.sust.b.1d.array.i64.zero",
    "llvm.nvvm.sust.b.1d.array.i8.clamp",
    "llvm.nvvm.sust.b.1d.array.i8.trap",
    "llvm.nvvm.sust.b.1d.array.i8.zero",
    "llvm.nvvm.sust.b.1d.array.v2i16.clamp",
    "llvm.nvvm.sust.b.1d.array.v2i16.trap",
    "llvm.nvvm.sust.b.1d.array.v2i16.zero",
    "llvm.nvvm.sust.b.1d.array.v2i32.clamp",
    "llvm.nvvm.sust.b.1d.array.v2i32.trap",
    "llvm.nvvm.sust.b.1d.array.v2i32.zero",
    "llvm.nvvm.sust.b.1d.array.v2i64.clamp",
    "llvm.nvvm.sust.b.1d.array.v2i64.trap",
    "llvm.nvvm.sust.b.1d.array.v2i64.zero",
    "llvm.nvvm.sust.b.1d.array.v2i8.clamp",
    "llvm.nvvm.sust.b.1d.array.v2i8.trap",
    "llvm.nvvm.sust.b.1d.array.v2i8.zero",
    "llvm.nvvm.sust.b.1d.array.v4i16.clamp",
    "llvm.nvvm.sust.b.1d.array.v4i16.trap",
    "llvm.nvvm.sust.b.1d.array.v4i16.zero",
    "llvm.nvvm.sust.b.1d.array.v4i32.clamp",
    "llvm.nvvm.sust.b.1d.array.v4i32.trap",
    "llvm.nvvm.sust.b.1d.array.v4i32.zero",
    "llvm.nvvm.sust.b.1d.array.v4i8.clamp",
    "llvm.nvvm.sust.b.1d.array.v4i8.trap",
    "llvm.nvvm.sust.b.1d.array.v4i8.zero",
    "llvm.nvvm.sust.b.1d.i16.clamp",
    "llvm.nvvm.sust.b.1d.i16.trap",
    "llvm.nvvm.sust.b.1d.i16.zero",
    "llvm.nvvm.sust.b.1d.i32.clamp",
    "llvm.nvvm.sust.b.1d.i32.trap",
    "llvm.nvvm.sust.b.1d.i32.zero",
    "llvm.nvvm.sust.b.1d.i64.clamp",
    "llvm.nvvm.sust.b.1d.i64.trap",
    "llvm.nvvm.sust.b.1d.i64.zero",
    "llvm.nvvm.sust.b.1d.i8.clamp",
    "llvm.nvvm.sust.b.1d.i8.trap",
    "llvm.nvvm.sust.b.1d.i8.zero",
    "llvm.nvvm.sust.b.1d.v2i16.clamp",
    "llvm.nvvm.sust.b.1d.v2i16.trap",
    "llvm.nvvm.sust.b.1d.v2i16.zero",
    "llvm.nvvm.sust.b.1d.v2i32.clamp",
    "llvm.nvvm.sust.b.1d.v2i32.trap",
    "llvm.nvvm.sust.b.1d.v2i32.zero",
    "llvm.nvvm.sust.b.1d.v2i64.clamp",
    "llvm.nvvm.sust.b.1d.v2i64.trap",
    "llvm.nvvm.sust.b.1d.v2i64.zero",
    "llvm.nvvm.sust.b.1d.v2i8.clamp",
    "llvm.nvvm.sust.b.1d.v2i8.trap",
    "llvm.nvvm.sust.b.1d.v2i8.zero",
    "llvm.nvvm.sust.b.1d.v4i16.clamp",
    "llvm.nvvm.sust.b.1d.v4i16.trap",
    "llvm.nvvm.sust.b.1d.v4i16.zero",
    "llvm.nvvm.sust.b.1d.v4i32.clamp",
    "llvm.nvvm.sust.b.1d.v4i32.trap",
    "llvm.nvvm.sust.b.1d.v4i32.zero",
    "llvm.nvvm.sust.b.1d.v4i8.clamp",
    "llvm.nvvm.sust.b.1d.v4i8.trap",
    "llvm.nvvm.sust.b.1d.v4i8.zero",
    "llvm.nvvm.sust.b.2d.array.i16.clamp",
    "llvm.nvvm.sust.b.2d.array.i16.trap",
    "llvm.nvvm.sust.b.2d.array.i16.zero",
    "llvm.nvvm.sust.b.2d.array.i32.clamp",
    "llvm.nvvm.sust.b.2d.array.i32.trap",
    "llvm.nvvm.sust.b.2d.array.i32.zero",
    "llvm.nvvm.sust.b.2d.array.i64.clamp",
    "llvm.nvvm.sust.b.2d.array.i64.trap",
    "llvm.nvvm.sust.b.2d.array.i64.zero",
    "llvm.nvvm.sust.b.2d.array.i8.clamp",
    "llvm.nvvm.sust.b.2d.array.i8.trap",
    "llvm.nvvm.sust.b.2d.array.i8.zero",
    "llvm.nvvm.sust.b.2d.array.v2i16.clamp",
    "llvm.nvvm.sust.b.2d.array.v2i16.trap",
    "llvm.nvvm.sust.b.2d.array.v2i16.zero",
    "llvm.nvvm.sust.b.2d.array.v2i32.clamp",
    "llvm.nvvm.sust.b.2d.array.v2i32.trap",
    "llvm.nvvm.sust.b.2d.array.v2i32.zero",
    "llvm.nvvm.sust.b.2d.array.v2i64.clamp",
    "llvm.nvvm.sust.b.2d.array.v2i64.trap",
    "llvm.nvvm.sust.b.2d.array.v2i64.zero",
    "llvm.nvvm.sust.b.2d.array.v2i8.clamp",
    "llvm.nvvm.sust.b.2d.array.v2i8.trap",
    "llvm.nvvm.sust.b.2d.array.v2i8.zero",
    "llvm.nvvm.sust.b.2d.array.v4i16.clamp",
    "llvm.nvvm.sust.b.2d.array.v4i16.trap",
    "llvm.nvvm.sust.b.2d.array.v4i16.zero",
    "llvm.nvvm.sust.b.2d.array.v4i32.clamp",
    "llvm.nvvm.sust.b.2d.array.v4i32.trap",
    "llvm.nvvm.sust.b.2d.array.v4i32.zero",
    "llvm.nvvm.sust.b.2d.array.v4i8.clamp",
    "llvm.nvvm.sust.b.2d.array.v4i8.trap",
    "llvm.nvvm.sust.b.2d.array.v4i8.zero",
    "llvm.nvvm.sust.b.2d.i16.clamp",
    "llvm.nvvm.sust.b.2d.i16.trap",
    "llvm.nvvm.sust.b.2d.i16.zero",
    "llvm.nvvm.sust.b.2d.i32.clamp",
    "llvm.nvvm.sust.b.2d.i32.trap",
    "llvm.nvvm.sust.b.2d.i32.zero",
    "llvm.nvvm.sust.b.2d.i64.clamp",
    "llvm.nvvm.sust.b.2d.i64.trap",
    "llvm.nvvm.sust.b.2d.i64.zero",
    "llvm.nvvm.sust.b.2d.i8.clamp",
    "llvm.nvvm.sust.b.2d.i8.trap",
    "llvm.nvvm.sust.b.2d.i8.zero",
    "llvm.nvvm.sust.b.2d.v2i16.clamp",
    "llvm.nvvm.sust.b.2d.v2i16.trap",
    "llvm.nvvm.sust.b.2d.v2i16.zero",
    "llvm.nvvm.sust.b.2d.v2i32.clamp",
    "llvm.nvvm.sust.b.2d.v2i32.trap",
    "llvm.nvvm.sust.b.2d.v2i32.zero",
    "llvm.nvvm.sust.b.2d.v2i64.clamp",
    "llvm.nvvm.sust.b.2d.v2i64.trap",
    "llvm.nvvm.sust.b.2d.v2i64.zero",
    "llvm.nvvm.sust.b.2d.v2i8.clamp",
    "llvm.nvvm.sust.b.2d.v2i8.trap",
    "llvm.nvvm.sust.b.2d.v2i8.zero",
    "llvm.nvvm.sust.b.2d.v4i16.clamp",
    "llvm.nvvm.sust.b.2d.v4i16.trap",
    "llvm.nvvm.sust.b.2d.v4i16.zero",
    "llvm.nvvm.sust.b.2d.v4i32.clamp",
    "llvm.nvvm.sust.b.2d.v4i32.trap",
    "llvm.nvvm.sust.b.2d.v4i32.zero",
    "llvm.nvvm.sust.b.2d.v4i8.clamp",
    "llvm.nvvm.sust.b.2d.v4i8.trap",
    "llvm.nvvm.sust.b.2d.v4i8.zero",
    "llvm.nvvm.sust.b.3d.i16.clamp",
    "llvm.nvvm.sust.b.3d.i16.trap",
    "llvm.nvvm.sust.b.3d.i16.zero",
    "llvm.nvvm.sust.b.3d.i32.clamp",
    "llvm.nvvm.sust.b.3d.i32.trap",
    "llvm.nvvm.sust.b.3d.i32.zero",
    "llvm.nvvm.sust.b.3d.i64.clamp",
    "llvm.nvvm.sust.b.3d.i64.trap",
    "llvm.nvvm.sust.b.3d.i64.zero",
    "llvm.nvvm.sust.b.3d.i8.clamp",
    "llvm.nvvm.sust.b.3d.i8.trap",
    "llvm.nvvm.sust.b.3d.i8.zero",
    "llvm.nvvm.sust.b.3d.v2i16.clamp",
    "llvm.nvvm.sust.b.3d.v2i16.trap",
    "llvm.nvvm.sust.b.3d.v2i16.zero",
    "llvm.nvvm.sust.b.3d.v2i32.clamp",
    "llvm.nvvm.sust.b.3d.v2i32.trap",
    "llvm.nvvm.sust.b.3d.v2i32.zero",
    "llvm.nvvm.sust.b.3d.v2i64.clamp",
    "llvm.nvvm.sust.b.3d.v2i64.trap",
    "llvm.nvvm.sust.b.3d.v2i64.zero",
    "llvm.nvvm.sust.b.3d.v2i8.clamp",
    "llvm.nvvm.sust.b.3d.v2i8.trap",
    "llvm.nvvm.sust.b.3d.v2i8.zero",
    "llvm.nvvm.sust.b.3d.v4i16.clamp",
    "llvm.nvvm.sust.b.3d.v4i16.trap",
    "llvm.nvvm.sust.b.3d.v4i16.zero",
    "llvm.nvvm.sust.b.3d.v4i32.clamp",
    "llvm.nvvm.sust.b.3d.v4i32.trap",
    "llvm.nvvm.sust.b.3d.v4i32.zero",
    "llvm.nvvm.sust.b.3d.v4i8.clamp",
    "llvm.nvvm.sust.b.3d.v4i8.trap",
    "llvm.nvvm.sust.b.3d.v4i8.zero",
    "llvm.nvvm.sust.p.1d.array.i16.trap",
    "llvm.nvvm.sust.p.1d.array.i32.trap",
    "llvm.nvvm.sust.p.1d.array.i8.trap",
    "llvm.nvvm.sust.p.1d.array.v2i16.trap",
    "llvm.nvvm.sust.p.1d.array.v2i32.trap",
    "llvm.nvvm.sust.p.1d.array.v2i8.trap",
    "llvm.nvvm.sust.p.1d.array.v4i16.trap",
    "llvm.nvvm.sust.p.1d.array.v4i32.trap",
    "llvm.nvvm.sust.p.1d.array.v4i8.trap",
    "llvm.nvvm.sust.p.1d.i16.trap",
    "llvm.nvvm.sust.p.1d.i32.trap",
    "llvm.nvvm.sust.p.1d.i8.trap",
    "llvm.nvvm.sust.p.1d.v2i16.trap",
    "llvm.nvvm.sust.p.1d.v2i32.trap",
    "llvm.nvvm.sust.p.1d.v2i8.trap",
    "llvm.nvvm.sust.p.1d.v4i16.trap",
    "llvm.nvvm.sust.p.1d.v4i32.trap",
    "llvm.nvvm.sust.p.1d.v4i8.trap",
    "llvm.nvvm.sust.p.2d.array.i16.trap",
    "llvm.nvvm.sust.p.2d.array.i32.trap",
    "llvm.nvvm.sust.p.2d.array.i8.trap",
    "llvm.nvvm.sust.p.2d.array.v2i16.trap",
    "llvm.nvvm.sust.p.2d.array.v2i32.trap",
    "llvm.nvvm.sust.p.2d.array.v2i8.trap",
    "llvm.nvvm.sust.p.2d.array.v4i16.trap",
    "llvm.nvvm.sust.p.2d.array.v4i32.trap",
    "llvm.nvvm.sust.p.2d.array.v4i8.trap",
    "llvm.nvvm.sust.p.2d.i16.trap",
    "llvm.nvvm.sust.p.2d.i32.trap",
    "llvm.nvvm.sust.p.2d.i8.trap",
    "llvm.nvvm.sust.p.2d.v2i16.trap",
    "llvm.nvvm.sust.p.2d.v2i32.trap",
    "llvm.nvvm.sust.p.2d.v2i8.trap",
    "llvm.nvvm.sust.p.2d.v4i16.trap",
    "llvm.nvvm.sust.p.2d.v4i32.trap",
    "llvm.nvvm.sust.p.2d.v4i8.trap",
    "llvm.nvvm.sust.p.3d.i16.trap",
    "llvm.nvvm.sust.p.3d.i32.trap",
    "llvm.nvvm.sust.p.3d.i8.trap",
    "llvm.nvvm.sust.p.3d.v2i16.trap",
    "llvm.nvvm.sust.p.3d.v2i32.trap",
    "llvm.nvvm.sust.p.3d.v2i8.trap",
    "llvm.nvvm.sust.p.3d.v4i16.trap",
    "llvm.nvvm.sust.p.3d.v4i32.trap",
    "llvm.nvvm.sust.p.3d.v4i8.trap",
    "llvm.nvvm.swap.lo.hi.b64",
    "llvm.nvvm.tex.1d.array.grad.v4f32.f32",
    "llvm.nvvm.tex.1d.array.grad.v4s32.f32",
    "llvm.nvvm.tex.1d.array.grad.v4u32.f32",
    "llvm.nvvm.tex.1d.array.level.v4f32.f32",
    "llvm.nvvm.tex.1d.array.level.v4s32.f32",
    "llvm.nvvm.tex.1d.array.level.v4u32.f32",
    "llvm.nvvm.tex.1d.array.v4f32.f32",
    "llvm.nvvm.tex.1d.array.v4f32.s32",
    "llvm.nvvm.tex.1d.array.v4s32.f32",
    "llvm.nvvm.tex.1d.array.v4s32.s32",
    "llvm.nvvm.tex.1d.array.v4u32.f32",
    "llvm.nvvm.tex.1d.array.v4u32.s32",
    "llvm.nvvm.tex.1d.grad.v4f32.f32",
    "llvm.nvvm.tex.1d.grad.v4s32.f32",
    "llvm.nvvm.tex.1d.grad.v4u32.f32",
    "llvm.nvvm.tex.1d.level.v4f32.f32",
    "llvm.nvvm.tex.1d.level.v4s32.f32",
    "llvm.nvvm.tex.1d.level.v4u32.f32",
    "llvm.nvvm.tex.1d.v4f32.f32",
    "llvm.nvvm.tex.1d.v4f32.s32",
    "llvm.nvvm.tex.1d.v4s32.f32",
    "llvm.nvvm.tex.1d.v4s32.s32",
    "llvm.nvvm.tex.1d.v4u32.f32",
    "llvm.nvvm.tex.1d.v4u32.s32",
    "llvm.nvvm.tex.2d.array.grad.v4f32.f32",
    "llvm.nvvm.tex.2d.array.grad.v4s32.f32",
    "llvm.nvvm.tex.2d.array.grad.v4u32.f32",
    "llvm.nvvm.tex.2d.array.level.v4f32.f32",
    "llvm.nvvm.tex.2d.array.level.v4s32.f32",
    "llvm.nvvm.tex.2d.array.level.v4u32.f32",
    "llvm.nvvm.tex.2d.array.v4f32.f32",
    "llvm.nvvm.tex.2d.array.v4f32.s32",
    "llvm.nvvm.tex.2d.array.v4s32.f32",
    "llvm.nvvm.tex.2d.array.v4s32.s32",
    "llvm.nvvm.tex.2d.array.v4u32.f32",
    "llvm.nvvm.tex.2d.array.v4u32.s32",
    "llvm.nvvm.tex.2d.grad.v4f32.f32",
    "llvm.nvvm.tex.2d.grad.v4s32.f32",
    "llvm.nvvm.tex.2d.grad.v4u32.f32",
    "llvm.nvvm.tex.2d.level.v4f32.f32",
    "llvm.nvvm.tex.2d.level.v4s32.f32",
    "llvm.nvvm.tex.2d.level.v4u32.f32",
    "llvm.nvvm.tex.2d.v4f32.f32",
    "llvm.nvvm.tex.2d.v4f32.s32",
    "llvm.nvvm.tex.2d.v4s32.f32",
    "llvm.nvvm.tex.2d.v4s32.s32",
    "llvm.nvvm.tex.2d.v4u32.f32",
    "llvm.nvvm.tex.2d.v4u32.s32",
    "llvm.nvvm.tex.3d.grad.v4f32.f32",
    "llvm.nvvm.tex.3d.grad.v4s32.f32",
    "llvm.nvvm.tex.3d.grad.v4u32.f32",
    "llvm.nvvm.tex.3d.level.v4f32.f32",
    "llvm.nvvm.tex.3d.level.v4s32.f32",
    "llvm.nvvm.tex.3d.level.v4u32.f32",
    "llvm.nvvm.tex.3d.v4f32.f32",
    "llvm.nvvm.tex.3d.v4f32.s32",
    "llvm.nvvm.tex.3d.v4s32.f32",
    "llvm.nvvm.tex.3d.v4s32.s32",
    "llvm.nvvm.tex.3d.v4u32.f32",
    "llvm.nvvm.tex.3d.v4u32.s32",
    "llvm.nvvm.tex.cube.array.level.v4f32.f32",
    "llvm.nvvm.tex.cube.array.level.v4s32.f32",
    "llvm.nvvm.tex.cube.array.level.v4u32.f32",
    "llvm.nvvm.tex.cube.array.v4f32.f32",
    "llvm.nvvm.tex.cube.array.v4s32.f32",
    "llvm.nvvm.tex.cube.array.v4u32.f32",
    "llvm.nvvm.tex.cube.level.v4f32.f32",
    "llvm.nvvm.tex.cube.level.v4s32.f32",
    "llvm.nvvm.tex.cube.level.v4u32.f32",
    "llvm.nvvm.tex.cube.v4f32.f32",
    "llvm.nvvm.tex.cube.v4s32.f32",
    "llvm.nvvm.tex.cube.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.array.grad.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.array.grad.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.array.grad.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.array.level.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.array.level.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.array.level.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.array.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.array.v4f32.s32",
    "llvm.nvvm.tex.unified.1d.array.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.array.v4s32.s32",
    "llvm.nvvm.tex.unified.1d.array.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.array.v4u32.s32",
    "llvm.nvvm.tex.unified.1d.grad.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.grad.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.grad.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.level.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.level.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.level.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.v4f32.f32",
    "llvm.nvvm.tex.unified.1d.v4f32.s32",
    "llvm.nvvm.tex.unified.1d.v4s32.f32",
    "llvm.nvvm.tex.unified.1d.v4s32.s32",
    "llvm.nvvm.tex.unified.1d.v4u32.f32",
    "llvm.nvvm.tex.unified.1d.v4u32.s32",
    "llvm.nvvm.tex.unified.2d.array.grad.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.array.grad.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.array.grad.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.array.level.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.array.level.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.array.level.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.array.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.array.v4f32.s32",
    "llvm.nvvm.tex.unified.2d.array.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.array.v4s32.s32",
    "llvm.nvvm.tex.unified.2d.array.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.array.v4u32.s32",
    "llvm.nvvm.tex.unified.2d.grad.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.grad.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.grad.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.level.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.level.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.level.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.v4f32.f32",
    "llvm.nvvm.tex.unified.2d.v4f32.s32",
    "llvm.nvvm.tex.unified.2d.v4s32.f32",
    "llvm.nvvm.tex.unified.2d.v4s32.s32",
    "llvm.nvvm.tex.unified.2d.v4u32.f32",
    "llvm.nvvm.tex.unified.2d.v4u32.s32",
    "llvm.nvvm.tex.unified.3d.grad.v4f32.f32",
    "llvm.nvvm.tex.unified.3d.grad.v4s32.f32",
    "llvm.nvvm.tex.unified.3d.grad.v4u32.f32",
    "llvm.nvvm.tex.unified.3d.level.v4f32.f32",
    "llvm.nvvm.tex.unified.3d.level.v4s32.f32",
    "llvm.nvvm.tex.unified.3d.level.v4u32.f32",
    "llvm.nvvm.tex.unified.3d.v4f32.f32",
    "llvm.nvvm.tex.unified.3d.v4f32.s32",
    "llvm.nvvm.tex.unified.3d.v4s32.f32",
    "llvm.nvvm.tex.unified.3d.v4s32.s32",
    "llvm.nvvm.tex.unified.3d.v4u32.f32",
    "llvm.nvvm.tex.unified.3d.v4u32.s32",
    "llvm.nvvm.tex.unified.cube.array.level.v4f32.f32",
    "llvm.nvvm.tex.unified.cube.array.level.v4s32.f32",
    "llvm.nvvm.tex.unified.cube.array.level.v4u32.f32",
    "llvm.nvvm.tex.unified.cube.array.v4f32.f32",
    "llvm.nvvm.tex.unified.cube.array.v4s32.f32",
    "llvm.nvvm.tex.unified.cube.array.v4u32.f32",
    "llvm.nvvm.tex.unified.cube.level.v4f32.f32",
    "llvm.nvvm.tex.unified.cube.level.v4s32.f32",
    "llvm.nvvm.tex.unified.cube.level.v4u32.f32",
    "llvm.nvvm.tex.unified.cube.v4f32.f32",
    "llvm.nvvm.tex.unified.cube.v4s32.f32",
    "llvm.nvvm.tex.unified.cube.v4u32.f32",
    "llvm.nvvm.texsurf.handle",
    "llvm.nvvm.texsurf.handle.internal",
    "llvm.nvvm.tld4.a.2d.v4f32.f32",
    "llvm.nvvm.tld4.a.2d.v4s32.f32",
    "llvm.nvvm.tld4.a.2d.v4u32.f32",
    "llvm.nvvm.tld4.b.2d.v4f32.f32",
    "llvm.nvvm.tld4.b.2d.v4s32.f32",
    "llvm.nvvm.tld4.b.2d.v4u32.f32",
    "llvm.nvvm.tld4.g.2d.v4f32.f32",
    "llvm.nvvm.tld4.g.2d.v4s32.f32",
    "llvm.nvvm.tld4.g.2d.v4u32.f32",
    "llvm.nvvm.tld4.r.2d.v4f32.f32",
    "llvm.nvvm.tld4.r.2d.v4s32.f32",
    "llvm.nvvm.tld4.r.2d.v4u32.f32",
    "llvm.nvvm.tld4.unified.a.2d.v4f32.f32",
    "llvm.nvvm.tld4.unified.a.2d.v4s32.f32",
    "llvm.nvvm.tld4.unified.a.2d.v4u32.f32",
    "llvm.nvvm.tld4.unified.b.2d.v4f32.f32",
    "llvm.nvvm.tld4.unified.b.2d.v4s32.f32",
    "llvm.nvvm.tld4.unified.b.2d.v4u32.f32",
    "llvm.nvvm.tld4.unified.g.2d.v4f32.f32",
    "llvm.nvvm.tld4.unified.g.2d.v4s32.f32",
    "llvm.nvvm.tld4.unified.g.2d.v4u32.f32",
    "llvm.nvvm.tld4.unified.r.2d.v4f32.f32",
    "llvm.nvvm.tld4.unified.r.2d.v4s32.f32",
    "llvm.nvvm.tld4.unified.r.2d.v4u32.f32",
    "llvm.nvvm.trunc.d",
    "llvm.nvvm.trunc.f",
    "llvm.nvvm.trunc.ftz.f",
    "llvm.nvvm.txq.array.size",
    "llvm.nvvm.txq.channel.data.type",
    "llvm.nvvm.txq.channel.order",
    "llvm.nvvm.txq.depth",
    "llvm.nvvm.txq.height",
    "llvm.nvvm.txq.num.mipmap.levels",
    "llvm.nvvm.txq.num.samples",
    "llvm.nvvm.txq.width",
    "llvm.nvvm.ui2d.rm",
    "llvm.nvvm.ui2d.rn",
    "llvm.nvvm.ui2d.rp",
    "llvm.nvvm.ui2d.rz",
    "llvm.nvvm.ui2f.rm",
    "llvm.nvvm.ui2f.rn",
    "llvm.nvvm.ui2f.rp",
    "llvm.nvvm.ui2f.rz",
    "llvm.nvvm.ull2d.rm",
    "llvm.nvvm.ull2d.rn",
    "llvm.nvvm.ull2d.rp",
    "llvm.nvvm.ull2d.rz",
    "llvm.nvvm.ull2f.rm",
    "llvm.nvvm.ull2f.rn",
    "llvm.nvvm.ull2f.rp",
    "llvm.nvvm.ull2f.rz",
    "llvm.nvvm.vote.all",
    "llvm.nvvm.vote.all.sync",
    "llvm.nvvm.vote.any",
    "llvm.nvvm.vote.any.sync",
    "llvm.nvvm.vote.ballot",
    "llvm.nvvm.vote.ballot.sync",
    "llvm.nvvm.vote.uni",
    "llvm.nvvm.vote.uni.sync",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.f16",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.a.sync.col.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.a.sync.row.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.col.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.b.sync.row.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.f32",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.f32",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.global.stride.f32",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.f32",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.shared.stride.f32",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.col.m16n16k16.stride.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.global.stride.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.shared.stride.f32",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.load.c.sync.row.m16n16k16.stride.f32",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f16",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f32",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f16.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f16",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f32",
    "llvm.nvvm.wmma.mma.sync.col.col.m16n16k16.f32.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f16",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f32",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f16.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f16",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f32",
    "llvm.nvvm.wmma.mma.sync.col.row.m16n16k16.f32.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f16",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f32",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f16.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f16",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f32",
    "llvm.nvvm.wmma.mma.sync.row.col.m16n16k16.f32.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f16",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f32",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f16.f32.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f16",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f16.satfinite",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f32",
    "llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f32.satfinite",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.f32",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.f32",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.global.stride.f32",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.f32",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.shared.stride.f32",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.col.m16n16k16.stride.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.global.stride.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.shared.stride.f32",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.stride.f16",
    "llvm.nvvm.wmma.store.d.sync.row.m16n16k16.stride.f32",
    "llvm.ppc.altivec.crypto.vcipher",
    "llvm.ppc.altivec.crypto.vcipherlast",
    "llvm.ppc.altivec.crypto.vncipher",
    "llvm.ppc.altivec.crypto.vncipherlast",
    "llvm.ppc.altivec.crypto.vpermxor",
    "llvm.ppc.altivec.crypto.vpmsumb",
    "llvm.ppc.altivec.crypto.vpmsumd",
    "llvm.ppc.altivec.crypto.vpmsumh",
    "llvm.ppc.altivec.crypto.vpmsumw",
    "llvm.ppc.altivec.crypto.vsbox",
    "llvm.ppc.altivec.crypto.vshasigmad",
    "llvm.ppc.altivec.crypto.vshasigmaw",
    "llvm.ppc.altivec.dss",
    "llvm.ppc.altivec.dssall",
    "llvm.ppc.altivec.dst",
    "llvm.ppc.altivec.dstst",
    "llvm.ppc.altivec.dststt",
    "llvm.ppc.altivec.dstt",
    "llvm.ppc.altivec.lvebx",
    "llvm.ppc.altivec.lvehx",
    "llvm.ppc.altivec.lvewx",
    "llvm.ppc.altivec.lvsl",
    "llvm.ppc.altivec.lvsr",
    "llvm.ppc.altivec.lvx",
    "llvm.ppc.altivec.lvxl",
    "llvm.ppc.altivec.mfvscr",
    "llvm.ppc.altivec.mtvscr",
    "llvm.ppc.altivec.stvebx",
    "llvm.ppc.altivec.stvehx",
    "llvm.ppc.altivec.stvewx",
    "llvm.ppc.altivec.stvx",
    "llvm.ppc.altivec.stvxl",
    "llvm.ppc.altivec.vabsdub",
    "llvm.ppc.altivec.vabsduh",
    "llvm.ppc.altivec.vabsduw",
    "llvm.ppc.altivec.vaddcuq",
    "llvm.ppc.altivec.vaddcuw",
    "llvm.ppc.altivec.vaddecuq",
    "llvm.ppc.altivec.vaddeuqm",
    "llvm.ppc.altivec.vaddsbs",
    "llvm.ppc.altivec.vaddshs",
    "llvm.ppc.altivec.vaddsws",
    "llvm.ppc.altivec.vaddubs",
    "llvm.ppc.altivec.vadduhs",
    "llvm.ppc.altivec.vadduws",
    "llvm.ppc.altivec.vavgsb",
    "llvm.ppc.altivec.vavgsh",
    "llvm.ppc.altivec.vavgsw",
    "llvm.ppc.altivec.vavgub",
    "llvm.ppc.altivec.vavguh",
    "llvm.ppc.altivec.vavguw",
    "llvm.ppc.altivec.vbpermq",
    "llvm.ppc.altivec.vcfsx",
    "llvm.ppc.altivec.vcfux",
    "llvm.ppc.altivec.vclzlsbb",
    "llvm.ppc.altivec.vcmpbfp",
    "llvm.ppc.altivec.vcmpbfp.p",
    "llvm.ppc.altivec.vcmpeqfp",
    "llvm.ppc.altivec.vcmpeqfp.p",
    "llvm.ppc.altivec.vcmpequb",
    "llvm.ppc.altivec.vcmpequb.p",
    "llvm.ppc.altivec.vcmpequd",
    "llvm.ppc.altivec.vcmpequd.p",
    "llvm.ppc.altivec.vcmpequh",
    "llvm.ppc.altivec.vcmpequh.p",
    "llvm.ppc.altivec.vcmpequw",
    "llvm.ppc.altivec.vcmpequw.p",
    "llvm.ppc.altivec.vcmpgefp",
    "llvm.ppc.altivec.vcmpgefp.p",
    "llvm.ppc.altivec.vcmpgtfp",
    "llvm.ppc.altivec.vcmpgtfp.p",
    "llvm.ppc.altivec.vcmpgtsb",
    "llvm.ppc.altivec.vcmpgtsb.p",
    "llvm.ppc.altivec.vcmpgtsd",
    "llvm.ppc.altivec.vcmpgtsd.p",
    "llvm.ppc.altivec.vcmpgtsh",
    "llvm.ppc.altivec.vcmpgtsh.p",
    "llvm.ppc.altivec.vcmpgtsw",
    "llvm.ppc.altivec.vcmpgtsw.p",
    "llvm.ppc.altivec.vcmpgtub",
    "llvm.ppc.altivec.vcmpgtub.p",
    "llvm.ppc.altivec.vcmpgtud",
    "llvm.ppc.altivec.vcmpgtud.p",
    "llvm.ppc.altivec.vcmpgtuh",
    "llvm.ppc.altivec.vcmpgtuh.p",
    "llvm.ppc.altivec.vcmpgtuw",
    "llvm.ppc.altivec.vcmpgtuw.p",
    "llvm.ppc.altivec.vcmpneb",
    "llvm.ppc.altivec.vcmpneb.p",
    "llvm.ppc.altivec.vcmpneh",
    "llvm.ppc.altivec.vcmpneh.p",
    "llvm.ppc.altivec.vcmpnew",
    "llvm.ppc.altivec.vcmpnew.p",
    "llvm.ppc.altivec.vcmpnezb",
    "llvm.ppc.altivec.vcmpnezb.p",
    "llvm.ppc.altivec.vcmpnezh",
    "llvm.ppc.altivec.vcmpnezh.p",
    "llvm.ppc.altivec.vcmpnezw",
    "llvm.ppc.altivec.vcmpnezw.p",
    "llvm.ppc.altivec.vctsxs",
    "llvm.ppc.altivec.vctuxs",
    "llvm.ppc.altivec.vctzlsbb",
    "llvm.ppc.altivec.vexptefp",
    "llvm.ppc.altivec.vgbbd",
    "llvm.ppc.altivec.vlogefp",
    "llvm.ppc.altivec.vmaddfp",
    "llvm.ppc.altivec.vmaxfp",
    "llvm.ppc.altivec.vmaxsb",
    "llvm.ppc.altivec.vmaxsd",
    "llvm.ppc.altivec.vmaxsh",
    "llvm.ppc.altivec.vmaxsw",
    "llvm.ppc.altivec.vmaxub",
    "llvm.ppc.altivec.vmaxud",
    "llvm.ppc.altivec.vmaxuh",
    "llvm.ppc.altivec.vmaxuw",
    "llvm.ppc.altivec.vmhaddshs",
    "llvm.ppc.altivec.vmhraddshs",
    "llvm.ppc.altivec.vminfp",
    "llvm.ppc.altivec.vminsb",
    "llvm.ppc.altivec.vminsd",
    "llvm.ppc.altivec.vminsh",
    "llvm.ppc.altivec.vminsw",
    "llvm.ppc.altivec.vminub",
    "llvm.ppc.altivec.vminud",
    "llvm.ppc.altivec.vminuh",
    "llvm.ppc.altivec.vminuw",
    "llvm.ppc.altivec.vmladduhm",
    "llvm.ppc.altivec.vmsummbm",
    "llvm.ppc.altivec.vmsumshm",
    "llvm.ppc.altivec.vmsumshs",
    "llvm.ppc.altivec.vmsumubm",
    "llvm.ppc.altivec.vmsumuhm",
    "llvm.ppc.altivec.vmsumuhs",
    "llvm.ppc.altivec.vmulesb",
    "llvm.ppc.altivec.vmulesh",
    "llvm.ppc.altivec.vmulesw",
    "llvm.ppc.altivec.vmuleub",
    "llvm.ppc.altivec.vmuleuh",
    "llvm.ppc.altivec.vmuleuw",
    "llvm.ppc.altivec.vmulosb",
    "llvm.ppc.altivec.vmulosh",
    "llvm.ppc.altivec.vmulosw",
    "llvm.ppc.altivec.vmuloub",
    "llvm.ppc.altivec.vmulouh",
    "llvm.ppc.altivec.vmulouw",
    "llvm.ppc.altivec.vnmsubfp",
    "llvm.ppc.altivec.vperm",
    "llvm.ppc.altivec.vpkpx",
    "llvm.ppc.altivec.vpksdss",
    "llvm.ppc.altivec.vpksdus",
    "llvm.ppc.altivec.vpkshss",
    "llvm.ppc.altivec.vpkshus",
    "llvm.ppc.altivec.vpkswss",
    "llvm.ppc.altivec.vpkswus",
    "llvm.ppc.altivec.vpkudus",
    "llvm.ppc.altivec.vpkuhus",
    "llvm.ppc.altivec.vpkuwus",
    "llvm.ppc.altivec.vprtybd",
    "llvm.ppc.altivec.vprtybq",
    "llvm.ppc.altivec.vprtybw",
    "llvm.ppc.altivec.vrefp",
    "llvm.ppc.altivec.vrfim",
    "llvm.ppc.altivec.vrfin",
    "llvm.ppc.altivec.vrfip",
    "llvm.ppc.altivec.vrfiz",
    "llvm.ppc.altivec.vrlb",
    "llvm.ppc.altivec.vrld",
    "llvm.ppc.altivec.vrldmi",
    "llvm.ppc.altivec.vrldnm",
    "llvm.ppc.altivec.vrlh",
    "llvm.ppc.altivec.vrlw",
    "llvm.ppc.altivec.vrlwmi",
    "llvm.ppc.altivec.vrlwnm",
    "llvm.ppc.altivec.vrsqrtefp",
    "llvm.ppc.altivec.vsel",
    "llvm.ppc.altivec.vsl",
    "llvm.ppc.altivec.vslb",
    "llvm.ppc.altivec.vslh",
    "llvm.ppc.altivec.vslo",
    "llvm.ppc.altivec.vslv",
    "llvm.ppc.altivec.vslw",
    "llvm.ppc.altivec.vsr",
    "llvm.ppc.altivec.vsrab",
    "llvm.ppc.altivec.vsrah",
    "llvm.ppc.altivec.vsraw",
    "llvm.ppc.altivec.vsrb",
    "llvm.ppc.altivec.vsrh",
    "llvm.ppc.altivec.vsro",
    "llvm.ppc.altivec.vsrv",
    "llvm.ppc.altivec.vsrw",
    "llvm.ppc.altivec.vsubcuq",
    "llvm.ppc.altivec.vsubcuw",
    "llvm.ppc.altivec.vsubecuq",
    "llvm.ppc.altivec.vsubeuqm",
    "llvm.ppc.altivec.vsubsbs",
    "llvm.ppc.altivec.vsubshs",
    "llvm.ppc.altivec.vsubsws",
    "llvm.ppc.altivec.vsububs",
    "llvm.ppc.altivec.vsubuhs",
    "llvm.ppc.altivec.vsubuws",
    "llvm.ppc.altivec.vsum2sws",
    "llvm.ppc.altivec.vsum4sbs",
    "llvm.ppc.altivec.vsum4shs",
    "llvm.ppc.altivec.vsum4ubs",
    "llvm.ppc.altivec.vsumsws",
    "llvm.ppc.altivec.vupkhpx",
    "llvm.ppc.altivec.vupkhsb",
    "llvm.ppc.altivec.vupkhsh",
    "llvm.ppc.altivec.vupkhsw",
    "llvm.ppc.altivec.vupklpx",
    "llvm.ppc.altivec.vupklsb",
    "llvm.ppc.altivec.vupklsh",
    "llvm.ppc.altivec.vupklsw",
    "llvm.ppc.bpermd",
    "llvm.ppc.cfence",
    "llvm.ppc.dcba",
    "llvm.ppc.dcbf",
    "llvm.ppc.dcbi",
    "llvm.ppc.dcbst",
    "llvm.ppc.dcbt",
    "llvm.ppc.dcbtst",
    "llvm.ppc.dcbz",
    "llvm.ppc.dcbzl",
    "llvm.ppc.divde",
    "llvm.ppc.divdeu",
    "llvm.ppc.divwe",
    "llvm.ppc.divweu",
    "llvm.ppc.get.texasr",
    "llvm.ppc.get.texasru",
    "llvm.ppc.get.tfhar",
    "llvm.ppc.get.tfiar",
    "llvm.ppc.is.decremented.ctr.nonzero",
    "llvm.ppc.lwsync",
    "llvm.ppc.mtctr",
    "llvm.ppc.qpx.qvfabs",
    "llvm.ppc.qpx.qvfadd",
    "llvm.ppc.qpx.qvfadds",
    "llvm.ppc.qpx.qvfcfid",
    "llvm.ppc.qpx.qvfcfids",
    "llvm.ppc.qpx.qvfcfidu",
    "llvm.ppc.qpx.qvfcfidus",
    "llvm.ppc.qpx.qvfcmpeq",
    "llvm.ppc.qpx.qvfcmpgt",
    "llvm.ppc.qpx.qvfcmplt",
    "llvm.ppc.qpx.qvfcpsgn",
    "llvm.ppc.qpx.qvfctid",
    "llvm.ppc.qpx.qvfctidu",
    "llvm.ppc.qpx.qvfctiduz",
    "llvm.ppc.qpx.qvfctidz",
    "llvm.ppc.qpx.qvfctiw",
    "llvm.ppc.qpx.qvfctiwu",
    "llvm.ppc.qpx.qvfctiwuz",
    "llvm.ppc.qpx.qvfctiwz",
    "llvm.ppc.qpx.qvflogical",
    "llvm.ppc.qpx.qvfmadd",
    "llvm.ppc.qpx.qvfmadds",
    "llvm.ppc.qpx.qvfmsub",
    "llvm.ppc.qpx.qvfmsubs",
    "llvm.ppc.qpx.qvfmul",
    "llvm.ppc.qpx.qvfmuls",
    "llvm.ppc.qpx.qvfnabs",
    "llvm.ppc.qpx.qvfneg",
    "llvm.ppc.qpx.qvfnmadd",
    "llvm.ppc.qpx.qvfnmadds",
    "llvm.ppc.qpx.qvfnmsub",
    "llvm.ppc.qpx.qvfnmsubs",
    "llvm.ppc.qpx.qvfperm",
    "llvm.ppc.qpx.qvfre",
    "llvm.ppc.qpx.qvfres",
    "llvm.ppc.qpx.qvfrim",
    "llvm.ppc.qpx.qvfrin",
    "llvm.ppc.qpx.qvfrip",
    "llvm.ppc.qpx.qvfriz",
    "llvm.ppc.qpx.qvfrsp",
    "llvm.ppc.qpx.qvfrsqrte",
    "llvm.ppc.qpx.qvfrsqrtes",
    "llvm.ppc.qpx.qvfsel",
    "llvm.ppc.qpx.qvfsub",
    "llvm.ppc.qpx.qvfsubs",
    "llvm.ppc.qpx.qvftstnan",
    "llvm.ppc.qpx.qvfxmadd",
    "llvm.ppc.qpx.qvfxmadds",
    "llvm.ppc.qpx.qvfxmul",
    "llvm.ppc.qpx.qvfxmuls",
    "llvm.ppc.qpx.qvfxxcpnmadd",
    "llvm.ppc.qpx.qvfxxcpnmadds",
    "llvm.ppc.qpx.qvfxxmadd",
    "llvm.ppc.qpx.qvfxxmadds",
    "llvm.ppc.qpx.qvfxxnpmadd",
    "llvm.ppc.qpx.qvfxxnpmadds",
    "llvm.ppc.qpx.qvgpci",
    "llvm.ppc.qpx.qvlfcd",
    "llvm.ppc.qpx.qvlfcda",
    "llvm.ppc.qpx.qvlfcs",
    "llvm.ppc.qpx.qvlfcsa",
    "llvm.ppc.qpx.qvlfd",
    "llvm.ppc.qpx.qvlfda",
    "llvm.ppc.qpx.qvlfiwa",
    "llvm.ppc.qpx.qvlfiwaa",
    "llvm.ppc.qpx.qvlfiwz",
    "llvm.ppc.qpx.qvlfiwza",
    "llvm.ppc.qpx.qvlfs",
    "llvm.ppc.qpx.qvlfsa",
    "llvm.ppc.qpx.qvlpcld",
    "llvm.ppc.qpx.qvlpcls",
    "llvm.ppc.qpx.qvlpcrd",
    "llvm.ppc.qpx.qvlpcrs",
    "llvm.ppc.qpx.qvstfcd",
    "llvm.ppc.qpx.qvstfcda",
    "llvm.ppc.qpx.qvstfcs",
    "llvm.ppc.qpx.qvstfcsa",
    "llvm.ppc.qpx.qvstfd",
    "llvm.ppc.qpx.qvstfda",
    "llvm.ppc.qpx.qvstfiw",
    "llvm.ppc.qpx.qvstfiwa",
    "llvm.ppc.qpx.qvstfs",
    "llvm.ppc.qpx.qvstfsa",
    "llvm.ppc.set.texasr",
    "llvm.ppc.set.texasru",
    "llvm.ppc.set.tfhar",
    "llvm.ppc.set.tfiar",
    "llvm.ppc.sync",
    "llvm.ppc.tabort",
    "llvm.ppc.tabortdc",
    "llvm.ppc.tabortdci",
    "llvm.ppc.tabortwc",
    "llvm.ppc.tabortwci",
    "llvm.ppc.tbegin",
    "llvm.ppc.tcheck",
    "llvm.ppc.tend",
    "llvm.ppc.tendall",
    "llvm.ppc.trechkpt",
    "llvm.ppc.treclaim",
    "llvm.ppc.tresume",
    "llvm.ppc.tsr",
    "llvm.ppc.tsuspend",
    "llvm.ppc.ttest",
    "llvm.ppc.vsx.lxvd2x",
    "llvm.ppc.vsx.lxvd2x.be",
    "llvm.ppc.vsx.lxvl",
    "llvm.ppc.vsx.lxvll",
    "llvm.ppc.vsx.lxvw4x",
    "llvm.ppc.vsx.lxvw4x.be",
    "llvm.ppc.vsx.stxvd2x",
    "llvm.ppc.vsx.stxvd2x.be",
    "llvm.ppc.vsx.stxvl",
    "llvm.ppc.vsx.stxvll",
    "llvm.ppc.vsx.stxvw4x",
    "llvm.ppc.vsx.stxvw4x.be",
    "llvm.ppc.vsx.xsmaxdp",
    "llvm.ppc.vsx.xsmindp",
    "llvm.ppc.vsx.xvcmpeqdp",
    "llvm.ppc.vsx.xvcmpeqdp.p",
    "llvm.ppc.vsx.xvcmpeqsp",
    "llvm.ppc.vsx.xvcmpeqsp.p",
    "llvm.ppc.vsx.xvcmpgedp",
    "llvm.ppc.vsx.xvcmpgedp.p",
    "llvm.ppc.vsx.xvcmpgesp",
    "llvm.ppc.vsx.xvcmpgesp.p",
    "llvm.ppc.vsx.xvcmpgtdp",
    "llvm.ppc.vsx.xvcmpgtdp.p",
    "llvm.ppc.vsx.xvcmpgtsp",
    "llvm.ppc.vsx.xvcmpgtsp.p",
    "llvm.ppc.vsx.xvcvdpsp",
    "llvm.ppc.vsx.xvcvdpsxws",
    "llvm.ppc.vsx.xvcvdpuxws",
    "llvm.ppc.vsx.xvcvhpsp",
    "llvm.ppc.vsx.xvcvspdp",
    "llvm.ppc.vsx.xvcvsphp",
    "llvm.ppc.vsx.xvcvsxdsp",
    "llvm.ppc.vsx.xvcvsxwdp",
    "llvm.ppc.vsx.xvcvuxdsp",
    "llvm.ppc.vsx.xvcvuxwdp",
    "llvm.ppc.vsx.xvdivdp",
    "llvm.ppc.vsx.xvdivsp",
    "llvm.ppc.vsx.xviexpdp",
    "llvm.ppc.vsx.xviexpsp",
    "llvm.ppc.vsx.xvmaxdp",
    "llvm.ppc.vsx.xvmaxsp",
    "llvm.ppc.vsx.xvmindp",
    "llvm.ppc.vsx.xvminsp",
    "llvm.ppc.vsx.xvrdpip",
    "llvm.ppc.vsx.xvredp",
    "llvm.ppc.vsx.xvresp",
    "llvm.ppc.vsx.xvrspip",
    "llvm.ppc.vsx.xvrsqrtedp",
    "llvm.ppc.vsx.xvrsqrtesp",
    "llvm.ppc.vsx.xvtstdcdp",
    "llvm.ppc.vsx.xvtstdcsp",
    "llvm.ppc.vsx.xvxexpdp",
    "llvm.ppc.vsx.xvxexpsp",
    "llvm.ppc.vsx.xvxsigdp",
    "llvm.ppc.vsx.xvxsigsp",
    "llvm.ppc.vsx.xxextractuw",
    "llvm.ppc.vsx.xxinsertw",
    "llvm.ppc.vsx.xxleqv",
    "llvm.r600.cube",
    "llvm.r600.group.barrier",
    "llvm.r600.implicitarg.ptr",
    "llvm.r600.rat.store.typed",
    "llvm.r600.read.global.size.x",
    "llvm.r600.read.global.size.y",
    "llvm.r600.read.global.size.z",
    "llvm.r600.read.local.size.x",
    "llvm.r600.read.local.size.y",
    "llvm.r600.read.local.size.z",
    "llvm.r600.read.ngroups.x",
    "llvm.r600.read.ngroups.y",
    "llvm.r600.read.ngroups.z",
    "llvm.r600.read.tgid.x",
    "llvm.r600.read.tgid.y",
    "llvm.r600.read.tgid.z",
    "llvm.r600.read.tidig.x",
    "llvm.r600.read.tidig.y",
    "llvm.r600.read.tidig.z",
    "llvm.r600.recipsqrt.clamped",
    "llvm.r600.recipsqrt.ieee",
    "llvm.s390.efpc",
    "llvm.s390.etnd",
    "llvm.s390.lcbb",
    "llvm.s390.ntstg",
    "llvm.s390.ppa.txassist",
    "llvm.s390.sfpc",
    "llvm.s390.tabort",
    "llvm.s390.tbegin",
    "llvm.s390.tbegin.nofloat",
    "llvm.s390.tbeginc",
    "llvm.s390.tdc",
    "llvm.s390.tend",
    "llvm.s390.vaccb",
    "llvm.s390.vacccq",
    "llvm.s390.vaccf",
    "llvm.s390.vaccg",
    "llvm.s390.vacch",
    "llvm.s390.vaccq",
    "llvm.s390.vacq",
    "llvm.s390.vaq",
    "llvm.s390.vavgb",
    "llvm.s390.vavgf",
    "llvm.s390.vavgg",
    "llvm.s390.vavgh",
    "llvm.s390.vavglb",
    "llvm.s390.vavglf",
    "llvm.s390.vavglg",
    "llvm.s390.vavglh",
    "llvm.s390.vbperm",
    "llvm.s390.vceqbs",
    "llvm.s390.vceqfs",
    "llvm.s390.vceqgs",
    "llvm.s390.vceqhs",
    "llvm.s390.vchbs",
    "llvm.s390.vchfs",
    "llvm.s390.vchgs",
    "llvm.s390.vchhs",
    "llvm.s390.vchlbs",
    "llvm.s390.vchlfs",
    "llvm.s390.vchlgs",
    "llvm.s390.vchlhs",
    "llvm.s390.vcksm",
    "llvm.s390.verimb",
    "llvm.s390.verimf",
    "llvm.s390.verimg",
    "llvm.s390.verimh",
    "llvm.s390.verllb",
    "llvm.s390.verllf",
    "llvm.s390.verllg",
    "llvm.s390.verllh",
    "llvm.s390.verllvb",
    "llvm.s390.verllvf",
    "llvm.s390.verllvg",
    "llvm.s390.verllvh",
    "llvm.s390.vfaeb",
    "llvm.s390.vfaebs",
    "llvm.s390.vfaef",
    "llvm.s390.vfaefs",
    "llvm.s390.vfaeh",
    "llvm.s390.vfaehs",
    "llvm.s390.vfaezb",
    "llvm.s390.vfaezbs",
    "llvm.s390.vfaezf",
    "llvm.s390.vfaezfs",
    "llvm.s390.vfaezh",
    "llvm.s390.vfaezhs",
    "llvm.s390.vfcedbs",
    "llvm.s390.vfcesbs",
    "llvm.s390.vfchdbs",
    "llvm.s390.vfchedbs",
    "llvm.s390.vfchesbs",
    "llvm.s390.vfchsbs",
    "llvm.s390.vfeeb",
    "llvm.s390.vfeebs",
    "llvm.s390.vfeef",
    "llvm.s390.vfeefs",
    "llvm.s390.vfeeh",
    "llvm.s390.vfeehs",
    "llvm.s390.vfeezb",
    "llvm.s390.vfeezbs",
    "llvm.s390.vfeezf",
    "llvm.s390.vfeezfs",
    "llvm.s390.vfeezh",
    "llvm.s390.vfeezhs",
    "llvm.s390.vfeneb",
    "llvm.s390.vfenebs",
    "llvm.s390.vfenef",
    "llvm.s390.vfenefs",
    "llvm.s390.vfeneh",
    "llvm.s390.vfenehs",
    "llvm.s390.vfenezb",
    "llvm.s390.vfenezbs",
    "llvm.s390.vfenezf",
    "llvm.s390.vfenezfs",
    "llvm.s390.vfenezh",
    "llvm.s390.vfenezhs",
    "llvm.s390.vfidb",
    "llvm.s390.vfisb",
    "llvm.s390.vfmaxdb",
    "llvm.s390.vfmaxsb",
    "llvm.s390.vfmindb",
    "llvm.s390.vfminsb",
    "llvm.s390.vftcidb",
    "llvm.s390.vftcisb",
    "llvm.s390.vgfmab",
    "llvm.s390.vgfmaf",
    "llvm.s390.vgfmag",
    "llvm.s390.vgfmah",
    "llvm.s390.vgfmb",
    "llvm.s390.vgfmf",
    "llvm.s390.vgfmg",
    "llvm.s390.vgfmh",
    "llvm.s390.vistrb",
    "llvm.s390.vistrbs",
    "llvm.s390.vistrf",
    "llvm.s390.vistrfs",
    "llvm.s390.vistrh",
    "llvm.s390.vistrhs",
    "llvm.s390.vlbb",
    "llvm.s390.vll",
    "llvm.s390.vlrl",
    "llvm.s390.vmaeb",
    "llvm.s390.vmaef",
    "llvm.s390.vmaeh",
    "llvm.s390.vmahb",
    "llvm.s390.vmahf",
    "llvm.s390.vmahh",
    "llvm.s390.vmaleb",
    "llvm.s390.vmalef",
    "llvm.s390.vmaleh",
    "llvm.s390.vmalhb",
    "llvm.s390.vmalhf",
    "llvm.s390.vmalhh",
    "llvm.s390.vmalob",
    "llvm.s390.vmalof",
    "llvm.s390.vmaloh",
    "llvm.s390.vmaob",
    "llvm.s390.vmaof",
    "llvm.s390.vmaoh",
    "llvm.s390.vmeb",
    "llvm.s390.vmef",
    "llvm.s390.vmeh",
    "llvm.s390.vmhb",
    "llvm.s390.vmhf",
    "llvm.s390.vmhh",
    "llvm.s390.vmleb",
    "llvm.s390.vmlef",
    "llvm.s390.vmleh",
    "llvm.s390.vmlhb",
    "llvm.s390.vmlhf",
    "llvm.s390.vmlhh",
    "llvm.s390.vmlob",
    "llvm.s390.vmlof",
    "llvm.s390.vmloh",
    "llvm.s390.vmob",
    "llvm.s390.vmof",
    "llvm.s390.vmoh",
    "llvm.s390.vmslg",
    "llvm.s390.vpdi",
    "llvm.s390.vperm",
    "llvm.s390.vpklsf",
    "llvm.s390.vpklsfs",
    "llvm.s390.vpklsg",
    "llvm.s390.vpklsgs",
    "llvm.s390.vpklsh",
    "llvm.s390.vpklshs",
    "llvm.s390.vpksf",
    "llvm.s390.vpksfs",
    "llvm.s390.vpksg",
    "llvm.s390.vpksgs",
    "llvm.s390.vpksh",
    "llvm.s390.vpkshs",
    "llvm.s390.vsbcbiq",
    "llvm.s390.vsbiq",
    "llvm.s390.vscbib",
    "llvm.s390.vscbif",
    "llvm.s390.vscbig",
    "llvm.s390.vscbih",
    "llvm.s390.vscbiq",
    "llvm.s390.vsl",
    "llvm.s390.vslb",
    "llvm.s390.vsldb",
    "llvm.s390.vsq",
    "llvm.s390.vsra",
    "llvm.s390.vsrab",
    "llvm.s390.vsrl",
    "llvm.s390.vsrlb",
    "llvm.s390.vstl",
    "llvm.s390.vstrcb",
    "llvm.s390.vstrcbs",
    "llvm.s390.vstrcf",
    "llvm.s390.vstrcfs",
    "llvm.s390.vstrch",
    "llvm.s390.vstrchs",
    "llvm.s390.vstrczb",
    "llvm.s390.vstrczbs",
    "llvm.s390.vstrczf",
    "llvm.s390.vstrczfs",
    "llvm.s390.vstrczh",
    "llvm.s390.vstrczhs",
    "llvm.s390.vstrl",
    "llvm.s390.vsumb",
    "llvm.s390.vsumgf",
    "llvm.s390.vsumgh",
    "llvm.s390.vsumh",
    "llvm.s390.vsumqf",
    "llvm.s390.vsumqg",
    "llvm.s390.vtm",
    "llvm.s390.vuphb",
    "llvm.s390.vuphf",
    "llvm.s390.vuphh",
    "llvm.s390.vuplb",
    "llvm.s390.vuplf",
    "llvm.s390.vuplhb",
    "llvm.s390.vuplhf",
    "llvm.s390.vuplhh",
    "llvm.s390.vuplhw",
    "llvm.s390.vupllb",
    "llvm.s390.vupllf",
    "llvm.s390.vupllh",
    "llvm.wasm.current.memory",
    "llvm.wasm.grow.memory",
    "llvm.wasm.rethrow",
    "llvm.wasm.throw",
    "llvm.x86.3dnow.pavgusb",
    "llvm.x86.3dnow.pf2id",
    "llvm.x86.3dnow.pfacc",
    "llvm.x86.3dnow.pfadd",
    "llvm.x86.3dnow.pfcmpeq",
    "llvm.x86.3dnow.pfcmpge",
    "llvm.x86.3dnow.pfcmpgt",
    "llvm.x86.3dnow.pfmax",
    "llvm.x86.3dnow.pfmin",
    "llvm.x86.3dnow.pfmul",
    "llvm.x86.3dnow.pfrcp",
    "llvm.x86.3dnow.pfrcpit1",
    "llvm.x86.3dnow.pfrcpit2",
    "llvm.x86.3dnow.pfrsqit1",
    "llvm.x86.3dnow.pfrsqrt",
    "llvm.x86.3dnow.pfsub",
    "llvm.x86.3dnow.pfsubr",
    "llvm.x86.3dnow.pi2fd",
    "llvm.x86.3dnow.pmulhrw",
    "llvm.x86.3dnowa.pf2iw",
    "llvm.x86.3dnowa.pfnacc",
    "llvm.x86.3dnowa.pfpnacc",
    "llvm.x86.3dnowa.pi2fw",
    "llvm.x86.3dnowa.pswapd",
    "llvm.x86.addcarry.u32",
    "llvm.x86.addcarry.u64",
    "llvm.x86.addcarryx.u32",
    "llvm.x86.addcarryx.u64",
    "llvm.x86.aesni.aesdec",
    "llvm.x86.aesni.aesdec.256",
    "llvm.x86.aesni.aesdec.512",
    "llvm.x86.aesni.aesdeclast",
    "llvm.x86.aesni.aesdeclast.256",
    "llvm.x86.aesni.aesdeclast.512",
    "llvm.x86.aesni.aesenc",
    "llvm.x86.aesni.aesenc.256",
    "llvm.x86.aesni.aesenc.512",
    "llvm.x86.aesni.aesenclast",
    "llvm.x86.aesni.aesenclast.256",
    "llvm.x86.aesni.aesenclast.512",
    "llvm.x86.aesni.aesimc",
    "llvm.x86.aesni.aeskeygenassist",
    "llvm.x86.avx.addsub.pd.256",
    "llvm.x86.avx.addsub.ps.256",
    "llvm.x86.avx.blendv.pd.256",
    "llvm.x86.avx.blendv.ps.256",
    "llvm.x86.avx.cmp.pd.256",
    "llvm.x86.avx.cmp.ps.256",
    "llvm.x86.avx.cvt.pd2.ps.256",
    "llvm.x86.avx.cvt.pd2dq.256",
    "llvm.x86.avx.cvt.ps2dq.256",
    "llvm.x86.avx.cvtdq2.ps.256",
    "llvm.x86.avx.cvtt.pd2dq.256",
    "llvm.x86.avx.cvtt.ps2dq.256",
    "llvm.x86.avx.dp.ps.256",
    "llvm.x86.avx.hadd.pd.256",
    "llvm.x86.avx.hadd.ps.256",
    "llvm.x86.avx.hsub.pd.256",
    "llvm.x86.avx.hsub.ps.256",
    "llvm.x86.avx.ldu.dq.256",
    "llvm.x86.avx.maskload.pd",
    "llvm.x86.avx.maskload.pd.256",
    "llvm.x86.avx.maskload.ps",
    "llvm.x86.avx.maskload.ps.256",
    "llvm.x86.avx.maskstore.pd",
    "llvm.x86.avx.maskstore.pd.256",
    "llvm.x86.avx.maskstore.ps",
    "llvm.x86.avx.maskstore.ps.256",
    "llvm.x86.avx.max.pd.256",
    "llvm.x86.avx.max.ps.256",
    "llvm.x86.avx.min.pd.256",
    "llvm.x86.avx.min.ps.256",
    "llvm.x86.avx.movmsk.pd.256",
    "llvm.x86.avx.movmsk.ps.256",
    "llvm.x86.avx.ptestc.256",
    "llvm.x86.avx.ptestnzc.256",
    "llvm.x86.avx.ptestz.256",
    "llvm.x86.avx.rcp.ps.256",
    "llvm.x86.avx.round.pd.256",
    "llvm.x86.avx.round.ps.256",
    "llvm.x86.avx.rsqrt.ps.256",
    "llvm.x86.avx.sqrt.pd.256",
    "llvm.x86.avx.sqrt.ps.256",
    "llvm.x86.avx.vpermilvar.pd",
    "llvm.x86.avx.vpermilvar.pd.256",
    "llvm.x86.avx.vpermilvar.ps",
    "llvm.x86.avx.vpermilvar.ps.256",
    "llvm.x86.avx.vtestc.pd",
    "llvm.x86.avx.vtestc.pd.256",
    "llvm.x86.avx.vtestc.ps",
    "llvm.x86.avx.vtestc.ps.256",
    "llvm.x86.avx.vtestnzc.pd",
    "llvm.x86.avx.vtestnzc.pd.256",
    "llvm.x86.avx.vtestnzc.ps",
    "llvm.x86.avx.vtestnzc.ps.256",
    "llvm.x86.avx.vtestz.pd",
    "llvm.x86.avx.vtestz.pd.256",
    "llvm.x86.avx.vtestz.ps",
    "llvm.x86.avx.vtestz.ps.256",
    "llvm.x86.avx.vzeroall",
    "llvm.x86.avx.vzeroupper",
    "llvm.x86.avx2.gather.d.d",
    "llvm.x86.avx2.gather.d.d.256",
    "llvm.x86.avx2.gather.d.pd",
    "llvm.x86.avx2.gather.d.pd.256",
    "llvm.x86.avx2.gather.d.ps",
    "llvm.x86.avx2.gather.d.ps.256",
    "llvm.x86.avx2.gather.d.q",
    "llvm.x86.avx2.gather.d.q.256",
    "llvm.x86.avx2.gather.q.d",
    "llvm.x86.avx2.gather.q.d.256",
    "llvm.x86.avx2.gather.q.pd",
    "llvm.x86.avx2.gather.q.pd.256",
    "llvm.x86.avx2.gather.q.ps",
    "llvm.x86.avx2.gather.q.ps.256",
    "llvm.x86.avx2.gather.q.q",
    "llvm.x86.avx2.gather.q.q.256",
    "llvm.x86.avx2.maskload.d",
    "llvm.x86.avx2.maskload.d.256",
    "llvm.x86.avx2.maskload.q",
    "llvm.x86.avx2.maskload.q.256",
    "llvm.x86.avx2.maskstore.d",
    "llvm.x86.avx2.maskstore.d.256",
    "llvm.x86.avx2.maskstore.q",
    "llvm.x86.avx2.maskstore.q.256",
    "llvm.x86.avx2.mpsadbw",
    "llvm.x86.avx2.packssdw",
    "llvm.x86.avx2.packsswb",
    "llvm.x86.avx2.packusdw",
    "llvm.x86.avx2.packuswb",
    "llvm.x86.avx2.padds.b",
    "llvm.x86.avx2.padds.w",
    "llvm.x86.avx2.paddus.b",
    "llvm.x86.avx2.paddus.w",
    "llvm.x86.avx2.pblendvb",
    "llvm.x86.avx2.permd",
    "llvm.x86.avx2.permps",
    "llvm.x86.avx2.phadd.d",
    "llvm.x86.avx2.phadd.sw",
    "llvm.x86.avx2.phadd.w",
    "llvm.x86.avx2.phsub.d",
    "llvm.x86.avx2.phsub.sw",
    "llvm.x86.avx2.phsub.w",
    "llvm.x86.avx2.pmadd.ub.sw",
    "llvm.x86.avx2.pmadd.wd",
    "llvm.x86.avx2.pmovmskb",
    "llvm.x86.avx2.pmul.dq",
    "llvm.x86.avx2.pmul.hr.sw",
    "llvm.x86.avx2.pmulh.w",
    "llvm.x86.avx2.pmulhu.w",
    "llvm.x86.avx2.pmulu.dq",
    "llvm.x86.avx2.psad.bw",
    "llvm.x86.avx2.pshuf.b",
    "llvm.x86.avx2.psign.b",
    "llvm.x86.avx2.psign.d",
    "llvm.x86.avx2.psign.w",
    "llvm.x86.avx2.psll.d",
    "llvm.x86.avx2.psll.q",
    "llvm.x86.avx2.psll.w",
    "llvm.x86.avx2.pslli.d",
    "llvm.x86.avx2.pslli.q",
    "llvm.x86.avx2.pslli.w",
    "llvm.x86.avx2.psllv.d",
    "llvm.x86.avx2.psllv.d.256",
    "llvm.x86.avx2.psllv.q",
    "llvm.x86.avx2.psllv.q.256",
    "llvm.x86.avx2.psra.d",
    "llvm.x86.avx2.psra.w",
    "llvm.x86.avx2.psrai.d",
    "llvm.x86.avx2.psrai.w",
    "llvm.x86.avx2.psrav.d",
    "llvm.x86.avx2.psrav.d.256",
    "llvm.x86.avx2.psrl.d",
    "llvm.x86.avx2.psrl.q",
    "llvm.x86.avx2.psrl.w",
    "llvm.x86.avx2.psrli.d",
    "llvm.x86.avx2.psrli.q",
    "llvm.x86.avx2.psrli.w",
    "llvm.x86.avx2.psrlv.d",
    "llvm.x86.avx2.psrlv.d.256",
    "llvm.x86.avx2.psrlv.q",
    "llvm.x86.avx2.psrlv.q.256",
    "llvm.x86.avx2.psubs.b",
    "llvm.x86.avx2.psubs.w",
    "llvm.x86.avx2.psubus.b",
    "llvm.x86.avx2.psubus.w",
    "llvm.x86.avx512.broadcastmb.128",
    "llvm.x86.avx512.broadcastmb.256",
    "llvm.x86.avx512.broadcastmb.512",
    "llvm.x86.avx512.broadcastmw.128",
    "llvm.x86.avx512.broadcastmw.256",
    "llvm.x86.avx512.broadcastmw.512",
    "llvm.x86.avx512.cvtb2mask.128",
    "llvm.x86.avx512.cvtb2mask.256",
    "llvm.x86.avx512.cvtb2mask.512",
    "llvm.x86.avx512.cvtd2mask.128",
    "llvm.x86.avx512.cvtd2mask.256",
    "llvm.x86.avx512.cvtd2mask.512",
    "llvm.x86.avx512.cvtq2mask.128",
    "llvm.x86.avx512.cvtq2mask.256",
    "llvm.x86.avx512.cvtq2mask.512",
    "llvm.x86.avx512.cvtsi2sd64",
    "llvm.x86.avx512.cvtsi2ss32",
    "llvm.x86.avx512.cvtsi2ss64",
    "llvm.x86.avx512.cvttsd2si",
    "llvm.x86.avx512.cvttsd2si64",
    "llvm.x86.avx512.cvttsd2usi",
    "llvm.x86.avx512.cvttsd2usi64",
    "llvm.x86.avx512.cvttss2si",
    "llvm.x86.avx512.cvttss2si64",
    "llvm.x86.avx512.cvttss2usi",
    "llvm.x86.avx512.cvttss2usi64",
    "llvm.x86.avx512.cvtusi2sd",
    "llvm.x86.avx512.cvtusi2ss",
    "llvm.x86.avx512.cvtusi642sd",
    "llvm.x86.avx512.cvtusi642ss",
    "llvm.x86.avx512.cvtw2mask.128",
    "llvm.x86.avx512.cvtw2mask.256",
    "llvm.x86.avx512.cvtw2mask.512",
    "llvm.x86.avx512.exp2.pd",
    "llvm.x86.avx512.exp2.ps",
    "llvm.x86.avx512.gather.dpd.512",
    "llvm.x86.avx512.gather.dpi.512",
    "llvm.x86.avx512.gather.dpq.512",
    "llvm.x86.avx512.gather.dps.512",
    "llvm.x86.avx512.gather.qpd.512",
    "llvm.x86.avx512.gather.qpi.512",
    "llvm.x86.avx512.gather.qpq.512",
    "llvm.x86.avx512.gather.qps.512",
    "llvm.x86.avx512.gather3div2.df",
    "llvm.x86.avx512.gather3div2.di",
    "llvm.x86.avx512.gather3div4.df",
    "llvm.x86.avx512.gather3div4.di",
    "llvm.x86.avx512.gather3div4.sf",
    "llvm.x86.avx512.gather3div4.si",
    "llvm.x86.avx512.gather3div8.sf",
    "llvm.x86.avx512.gather3div8.si",
    "llvm.x86.avx512.gather3siv2.df",
    "llvm.x86.avx512.gather3siv2.di",
    "llvm.x86.avx512.gather3siv4.df",
    "llvm.x86.avx512.gather3siv4.di",
    "llvm.x86.avx512.gather3siv4.sf",
    "llvm.x86.avx512.gather3siv4.si",
    "llvm.x86.avx512.gather3siv8.sf",
    "llvm.x86.avx512.gather3siv8.si",
    "llvm.x86.avx512.gatherpf.dpd.512",
    "llvm.x86.avx512.gatherpf.dps.512",
    "llvm.x86.avx512.gatherpf.qpd.512",
    "llvm.x86.avx512.gatherpf.qps.512",
    "llvm.x86.avx512.kand.w",
    "llvm.x86.avx512.kandn.w",
    "llvm.x86.avx512.knot.w",
    "llvm.x86.avx512.kor.w",
    "llvm.x86.avx512.kortestc.w",
    "llvm.x86.avx512.kortestz.w",
    "llvm.x86.avx512.kunpck.bw",
    "llvm.x86.avx512.kunpck.dq",
    "llvm.x86.avx512.kunpck.wd",
    "llvm.x86.avx512.kxnor.w",
    "llvm.x86.avx512.kxor.w",
    "llvm.x86.avx512.mask.add.pd.512",
    "llvm.x86.avx512.mask.add.ps.512",
    "llvm.x86.avx512.mask.add.sd.round",
    "llvm.x86.avx512.mask.add.ss.round",
    "llvm.x86.avx512.mask.cmp.pd.128",
    "llvm.x86.avx512.mask.cmp.pd.256",
    "llvm.x86.avx512.mask.cmp.pd.512",
    "llvm.x86.avx512.mask.cmp.ps.128",
    "llvm.x86.avx512.mask.cmp.ps.256",
    "llvm.x86.avx512.mask.cmp.ps.512",
    "llvm.x86.avx512.mask.cmp.sd",
    "llvm.x86.avx512.mask.cmp.ss",
    "llvm.x86.avx512.mask.compress.b.128",
    "llvm.x86.avx512.mask.compress.b.256",
    "llvm.x86.avx512.mask.compress.b.512",
    "llvm.x86.avx512.mask.compress.d.128",
    "llvm.x86.avx512.mask.compress.d.256",
    "llvm.x86.avx512.mask.compress.d.512",
    "llvm.x86.avx512.mask.compress.pd.128",
    "llvm.x86.avx512.mask.compress.pd.256",
    "llvm.x86.avx512.mask.compress.pd.512",
    "llvm.x86.avx512.mask.compress.ps.128",
    "llvm.x86.avx512.mask.compress.ps.256",
    "llvm.x86.avx512.mask.compress.ps.512",
    "llvm.x86.avx512.mask.compress.q.128",
    "llvm.x86.avx512.mask.compress.q.256",
    "llvm.x86.avx512.mask.compress.q.512",
    "llvm.x86.avx512.mask.compress.store.b.128",
    "llvm.x86.avx512.mask.compress.store.b.256",
    "llvm.x86.avx512.mask.compress.store.b.512",
    "llvm.x86.avx512.mask.compress.store.d.128",
    "llvm.x86.avx512.mask.compress.store.d.256",
    "llvm.x86.avx512.mask.compress.store.d.512",
    "llvm.x86.avx512.mask.compress.store.pd.128",
    "llvm.x86.avx512.mask.compress.store.pd.256",
    "llvm.x86.avx512.mask.compress.store.pd.512",
    "llvm.x86.avx512.mask.compress.store.ps.128",
    "llvm.x86.avx512.mask.compress.store.ps.256",
    "llvm.x86.avx512.mask.compress.store.ps.512",
    "llvm.x86.avx512.mask.compress.store.q.128",
    "llvm.x86.avx512.mask.compress.store.q.256",
    "llvm.x86.avx512.mask.compress.store.q.512",
    "llvm.x86.avx512.mask.compress.store.w.128",
    "llvm.x86.avx512.mask.compress.store.w.256",
    "llvm.x86.avx512.mask.compress.store.w.512",
    "llvm.x86.avx512.mask.compress.w.128",
    "llvm.x86.avx512.mask.compress.w.256",
    "llvm.x86.avx512.mask.compress.w.512",
    "llvm.x86.avx512.mask.conflict.d.128",
    "llvm.x86.avx512.mask.conflict.d.256",
    "llvm.x86.avx512.mask.conflict.d.512",
    "llvm.x86.avx512.mask.conflict.q.128",
    "llvm.x86.avx512.mask.conflict.q.256",
    "llvm.x86.avx512.mask.conflict.q.512",
    "llvm.x86.avx512.mask.cvtdq2ps.128",
    "llvm.x86.avx512.mask.cvtdq2ps.256",
    "llvm.x86.avx512.mask.cvtdq2ps.512",
    "llvm.x86.avx512.mask.cvtpd2dq.128",
    "llvm.x86.avx512.mask.cvtpd2dq.256",
    "llvm.x86.avx512.mask.cvtpd2dq.512",
    "llvm.x86.avx512.mask.cvtpd2ps",
    "llvm.x86.avx512.mask.cvtpd2ps.256",
    "llvm.x86.avx512.mask.cvtpd2ps.512",
    "llvm.x86.avx512.mask.cvtpd2qq.128",
    "llvm.x86.avx512.mask.cvtpd2qq.256",
    "llvm.x86.avx512.mask.cvtpd2qq.512",
    "llvm.x86.avx512.mask.cvtpd2udq.128",
    "llvm.x86.avx512.mask.cvtpd2udq.256",
    "llvm.x86.avx512.mask.cvtpd2udq.512",
    "llvm.x86.avx512.mask.cvtpd2uqq.128",
    "llvm.x86.avx512.mask.cvtpd2uqq.256",
    "llvm.x86.avx512.mask.cvtpd2uqq.512",
    "llvm.x86.avx512.mask.cvtps2dq.128",
    "llvm.x86.avx512.mask.cvtps2dq.256",
    "llvm.x86.avx512.mask.cvtps2dq.512",
    "llvm.x86.avx512.mask.cvtps2pd.128",
    "llvm.x86.avx512.mask.cvtps2pd.256",
    "llvm.x86.avx512.mask.cvtps2pd.512",
    "llvm.x86.avx512.mask.cvtps2qq.128",
    "llvm.x86.avx512.mask.cvtps2qq.256",
    "llvm.x86.avx512.mask.cvtps2qq.512",
    "llvm.x86.avx512.mask.cvtps2udq.128",
    "llvm.x86.avx512.mask.cvtps2udq.256",
    "llvm.x86.avx512.mask.cvtps2udq.512",
    "llvm.x86.avx512.mask.cvtps2uqq.128",
    "llvm.x86.avx512.mask.cvtps2uqq.256",
    "llvm.x86.avx512.mask.cvtps2uqq.512",
    "llvm.x86.avx512.mask.cvtqq2pd.128",
    "llvm.x86.avx512.mask.cvtqq2pd.256",
    "llvm.x86.avx512.mask.cvtqq2pd.512",
    "llvm.x86.avx512.mask.cvtqq2ps.128",
    "llvm.x86.avx512.mask.cvtqq2ps.256",
    "llvm.x86.avx512.mask.cvtqq2ps.512",
    "llvm.x86.avx512.mask.cvtsd2ss.round",
    "llvm.x86.avx512.mask.cvtss2sd.round",
    "llvm.x86.avx512.mask.cvttpd2dq.128",
    "llvm.x86.avx512.mask.cvttpd2dq.256",
    "llvm.x86.avx512.mask.cvttpd2dq.512",
    "llvm.x86.avx512.mask.cvttpd2qq.128",
    "llvm.x86.avx512.mask.cvttpd2qq.256",
    "llvm.x86.avx512.mask.cvttpd2qq.512",
    "llvm.x86.avx512.mask.cvttpd2udq.128",
    "llvm.x86.avx512.mask.cvttpd2udq.256",
    "llvm.x86.avx512.mask.cvttpd2udq.512",
    "llvm.x86.avx512.mask.cvttpd2uqq.128",
    "llvm.x86.avx512.mask.cvttpd2uqq.256",
    "llvm.x86.avx512.mask.cvttpd2uqq.512",
    "llvm.x86.avx512.mask.cvttps2dq.128",
    "llvm.x86.avx512.mask.cvttps2dq.256",
    "llvm.x86.avx512.mask.cvttps2dq.512",
    "llvm.x86.avx512.mask.cvttps2qq.128",
    "llvm.x86.avx512.mask.cvttps2qq.256",
    "llvm.x86.avx512.mask.cvttps2qq.512",
    "llvm.x86.avx512.mask.cvttps2udq.128",
    "llvm.x86.avx512.mask.cvttps2udq.256",
    "llvm.x86.avx512.mask.cvttps2udq.512",
    "llvm.x86.avx512.mask.cvttps2uqq.128",
    "llvm.x86.avx512.mask.cvttps2uqq.256",
    "llvm.x86.avx512.mask.cvttps2uqq.512",
    "llvm.x86.avx512.mask.cvtudq2ps.128",
    "llvm.x86.avx512.mask.cvtudq2ps.256",
    "llvm.x86.avx512.mask.cvtudq2ps.512",
    "llvm.x86.avx512.mask.cvtuqq2pd.128",
    "llvm.x86.avx512.mask.cvtuqq2pd.256",
    "llvm.x86.avx512.mask.cvtuqq2pd.512",
    "llvm.x86.avx512.mask.cvtuqq2ps.128",
    "llvm.x86.avx512.mask.cvtuqq2ps.256",
    "llvm.x86.avx512.mask.cvtuqq2ps.512",
    "llvm.x86.avx512.mask.dbpsadbw.128",
    "llvm.x86.avx512.mask.dbpsadbw.256",
    "llvm.x86.avx512.mask.dbpsadbw.512",
    "llvm.x86.avx512.mask.div.pd.512",
    "llvm.x86.avx512.mask.div.ps.512",
    "llvm.x86.avx512.mask.div.sd.round",
    "llvm.x86.avx512.mask.div.ss.round",
    "llvm.x86.avx512.mask.expand.b.128",
    "llvm.x86.avx512.mask.expand.b.256",
    "llvm.x86.avx512.mask.expand.b.512",
    "llvm.x86.avx512.mask.expand.d.128",
    "llvm.x86.avx512.mask.expand.d.256",
    "llvm.x86.avx512.mask.expand.d.512",
    "llvm.x86.avx512.mask.expand.load.b.128",
    "llvm.x86.avx512.mask.expand.load.b.256",
    "llvm.x86.avx512.mask.expand.load.b.512",
    "llvm.x86.avx512.mask.expand.load.d.128",
    "llvm.x86.avx512.mask.expand.load.d.256",
    "llvm.x86.avx512.mask.expand.load.d.512",
    "llvm.x86.avx512.mask.expand.load.pd.128",
    "llvm.x86.avx512.mask.expand.load.pd.256",
    "llvm.x86.avx512.mask.expand.load.pd.512",
    "llvm.x86.avx512.mask.expand.load.ps.128",
    "llvm.x86.avx512.mask.expand.load.ps.256",
    "llvm.x86.avx512.mask.expand.load.ps.512",
    "llvm.x86.avx512.mask.expand.load.q.128",
    "llvm.x86.avx512.mask.expand.load.q.256",
    "llvm.x86.avx512.mask.expand.load.q.512",
    "llvm.x86.avx512.mask.expand.load.w.128",
    "llvm.x86.avx512.mask.expand.load.w.256",
    "llvm.x86.avx512.mask.expand.load.w.512",
    "llvm.x86.avx512.mask.expand.pd.128",
    "llvm.x86.avx512.mask.expand.pd.256",
    "llvm.x86.avx512.mask.expand.pd.512",
    "llvm.x86.avx512.mask.expand.ps.128",
    "llvm.x86.avx512.mask.expand.ps.256",
    "llvm.x86.avx512.mask.expand.ps.512",
    "llvm.x86.avx512.mask.expand.q.128",
    "llvm.x86.avx512.mask.expand.q.256",
    "llvm.x86.avx512.mask.expand.q.512",
    "llvm.x86.avx512.mask.expand.w.128",
    "llvm.x86.avx512.mask.expand.w.256",
    "llvm.x86.avx512.mask.expand.w.512",
    "llvm.x86.avx512.mask.fixupimm.pd.128",
    "llvm.x86.avx512.mask.fixupimm.pd.256",
    "llvm.x86.avx512.mask.fixupimm.pd.512",
    "llvm.x86.avx512.mask.fixupimm.ps.128",
    "llvm.x86.avx512.mask.fixupimm.ps.256",
    "llvm.x86.avx512.mask.fixupimm.ps.512",
    "llvm.x86.avx512.mask.fixupimm.sd",
    "llvm.x86.avx512.mask.fixupimm.ss",
    "llvm.x86.avx512.mask.fpclass.pd.128",
    "llvm.x86.avx512.mask.fpclass.pd.256",
    "llvm.x86.avx512.mask.fpclass.pd.512",
    "llvm.x86.avx512.mask.fpclass.ps.128",
    "llvm.x86.avx512.mask.fpclass.ps.256",
    "llvm.x86.avx512.mask.fpclass.ps.512",
    "llvm.x86.avx512.mask.fpclass.sd",
    "llvm.x86.avx512.mask.fpclass.ss",
    "llvm.x86.avx512.mask.getexp.pd.128",
    "llvm.x86.avx512.mask.getexp.pd.256",
    "llvm.x86.avx512.mask.getexp.pd.512",
    "llvm.x86.avx512.mask.getexp.ps.128",
    "llvm.x86.avx512.mask.getexp.ps.256",
    "llvm.x86.avx512.mask.getexp.ps.512",
    "llvm.x86.avx512.mask.getexp.sd",
    "llvm.x86.avx512.mask.getexp.ss",
    "llvm.x86.avx512.mask.getmant.pd.128",
    "llvm.x86.avx512.mask.getmant.pd.256",
    "llvm.x86.avx512.mask.getmant.pd.512",
    "llvm.x86.avx512.mask.getmant.ps.128",
    "llvm.x86.avx512.mask.getmant.ps.256",
    "llvm.x86.avx512.mask.getmant.ps.512",
    "llvm.x86.avx512.mask.getmant.sd",
    "llvm.x86.avx512.mask.getmant.ss",
    "llvm.x86.avx512.mask.max.pd.512",
    "llvm.x86.avx512.mask.max.ps.512",
    "llvm.x86.avx512.mask.max.sd.round",
    "llvm.x86.avx512.mask.max.ss.round",
    "llvm.x86.avx512.mask.min.pd.512",
    "llvm.x86.avx512.mask.min.ps.512",
    "llvm.x86.avx512.mask.min.sd.round",
    "llvm.x86.avx512.mask.min.ss.round",
    "llvm.x86.avx512.mask.mul.pd.512",
    "llvm.x86.avx512.mask.mul.ps.512",
    "llvm.x86.avx512.mask.mul.sd.round",
    "llvm.x86.avx512.mask.mul.ss.round",
    "llvm.x86.avx512.mask.padds.b.128",
    "llvm.x86.avx512.mask.padds.b.256",
    "llvm.x86.avx512.mask.padds.b.512",
    "llvm.x86.avx512.mask.padds.w.128",
    "llvm.x86.avx512.mask.padds.w.256",
    "llvm.x86.avx512.mask.padds.w.512",
    "llvm.x86.avx512.mask.paddus.b.128",
    "llvm.x86.avx512.mask.paddus.b.256",
    "llvm.x86.avx512.mask.paddus.b.512",
    "llvm.x86.avx512.mask.paddus.w.128",
    "llvm.x86.avx512.mask.paddus.w.256",
    "llvm.x86.avx512.mask.paddus.w.512",
    "llvm.x86.avx512.mask.permvar.df.256",
    "llvm.x86.avx512.mask.permvar.df.512",
    "llvm.x86.avx512.mask.permvar.di.256",
    "llvm.x86.avx512.mask.permvar.di.512",
    "llvm.x86.avx512.mask.permvar.hi.128",
    "llvm.x86.avx512.mask.permvar.hi.256",
    "llvm.x86.avx512.mask.permvar.hi.512",
    "llvm.x86.avx512.mask.permvar.qi.128",
    "llvm.x86.avx512.mask.permvar.qi.256",
    "llvm.x86.avx512.mask.permvar.qi.512",
    "llvm.x86.avx512.mask.permvar.sf.256",
    "llvm.x86.avx512.mask.permvar.sf.512",
    "llvm.x86.avx512.mask.permvar.si.256",
    "llvm.x86.avx512.mask.permvar.si.512",
    "llvm.x86.avx512.mask.pmaddubs.w.128",
    "llvm.x86.avx512.mask.pmaddubs.w.256",
    "llvm.x86.avx512.mask.pmaddubs.w.512",
    "llvm.x86.avx512.mask.pmaddw.d.128",
    "llvm.x86.avx512.mask.pmaddw.d.256",
    "llvm.x86.avx512.mask.pmaddw.d.512",
    "llvm.x86.avx512.mask.pmov.db.128",
    "llvm.x86.avx512.mask.pmov.db.256",
    "llvm.x86.avx512.mask.pmov.db.512",
    "llvm.x86.avx512.mask.pmov.db.mem.128",
    "llvm.x86.avx512.mask.pmov.db.mem.256",
    "llvm.x86.avx512.mask.pmov.db.mem.512",
    "llvm.x86.avx512.mask.pmov.dw.128",
    "llvm.x86.avx512.mask.pmov.dw.256",
    "llvm.x86.avx512.mask.pmov.dw.512",
    "llvm.x86.avx512.mask.pmov.dw.mem.128",
    "llvm.x86.avx512.mask.pmov.dw.mem.256",
    "llvm.x86.avx512.mask.pmov.dw.mem.512",
    "llvm.x86.avx512.mask.pmov.qb.128",
    "llvm.x86.avx512.mask.pmov.qb.256",
    "llvm.x86.avx512.mask.pmov.qb.512",
    "llvm.x86.avx512.mask.pmov.qb.mem.128",
    "llvm.x86.avx512.mask.pmov.qb.mem.256",
    "llvm.x86.avx512.mask.pmov.qb.mem.512",
    "llvm.x86.avx512.mask.pmov.qd.128",
    "llvm.x86.avx512.mask.pmov.qd.256",
    "llvm.x86.avx512.mask.pmov.qd.512",
    "llvm.x86.avx512.mask.pmov.qd.mem.128",
    "llvm.x86.avx512.mask.pmov.qd.mem.256",
    "llvm.x86.avx512.mask.pmov.qd.mem.512",
    "llvm.x86.avx512.mask.pmov.qw.128",
    "llvm.x86.avx512.mask.pmov.qw.256",
    "llvm.x86.avx512.mask.pmov.qw.512",
    "llvm.x86.avx512.mask.pmov.qw.mem.128",
    "llvm.x86.avx512.mask.pmov.qw.mem.256",
    "llvm.x86.avx512.mask.pmov.qw.mem.512",
    "llvm.x86.avx512.mask.pmov.wb.128",
    "llvm.x86.avx512.mask.pmov.wb.256",
    "llvm.x86.avx512.mask.pmov.wb.512",
    "llvm.x86.avx512.mask.pmov.wb.mem.128",
    "llvm.x86.avx512.mask.pmov.wb.mem.256",
    "llvm.x86.avx512.mask.pmov.wb.mem.512",
    "llvm.x86.avx512.mask.pmovs.db.128",
    "llvm.x86.avx512.mask.pmovs.db.256",
    "llvm.x86.avx512.mask.pmovs.db.512",
    "llvm.x86.avx512.mask.pmovs.db.mem.128",
    "llvm.x86.avx512.mask.pmovs.db.mem.256",
    "llvm.x86.avx512.mask.pmovs.db.mem.512",
    "llvm.x86.avx512.mask.pmovs.dw.128",
    "llvm.x86.avx512.mask.pmovs.dw.256",
    "llvm.x86.avx512.mask.pmovs.dw.512",
    "llvm.x86.avx512.mask.pmovs.dw.mem.128",
    "llvm.x86.avx512.mask.pmovs.dw.mem.256",
    "llvm.x86.avx512.mask.pmovs.dw.mem.512",
    "llvm.x86.avx512.mask.pmovs.qb.128",
    "llvm.x86.avx512.mask.pmovs.qb.256",
    "llvm.x86.avx512.mask.pmovs.qb.512",
    "llvm.x86.avx512.mask.pmovs.qb.mem.128",
    "llvm.x86.avx512.mask.pmovs.qb.mem.256",
    "llvm.x86.avx512.mask.pmovs.qb.mem.512",
    "llvm.x86.avx512.mask.pmovs.qd.128",
    "llvm.x86.avx512.mask.pmovs.qd.256",
    "llvm.x86.avx512.mask.pmovs.qd.512",
    "llvm.x86.avx512.mask.pmovs.qd.mem.128",
    "llvm.x86.avx512.mask.pmovs.qd.mem.256",
    "llvm.x86.avx512.mask.pmovs.qd.mem.512",
    "llvm.x86.avx512.mask.pmovs.qw.128",
    "llvm.x86.avx512.mask.pmovs.qw.256",
    "llvm.x86.avx512.mask.pmovs.qw.512",
    "llvm.x86.avx512.mask.pmovs.qw.mem.128",
    "llvm.x86.avx512.mask.pmovs.qw.mem.256",
    "llvm.x86.avx512.mask.pmovs.qw.mem.512",
    "llvm.x86.avx512.mask.pmovs.wb.128",
    "llvm.x86.avx512.mask.pmovs.wb.256",
    "llvm.x86.avx512.mask.pmovs.wb.512",
    "llvm.x86.avx512.mask.pmovs.wb.mem.128",
    "llvm.x86.avx512.mask.pmovs.wb.mem.256",
    "llvm.x86.avx512.mask.pmovs.wb.mem.512",
    "llvm.x86.avx512.mask.pmovus.db.128",
    "llvm.x86.avx512.mask.pmovus.db.256",
    "llvm.x86.avx512.mask.pmovus.db.512",
    "llvm.x86.avx512.mask.pmovus.db.mem.128",
    "llvm.x86.avx512.mask.pmovus.db.mem.256",
    "llvm.x86.avx512.mask.pmovus.db.mem.512",
    "llvm.x86.avx512.mask.pmovus.dw.128",
    "llvm.x86.avx512.mask.pmovus.dw.256",
    "llvm.x86.avx512.mask.pmovus.dw.512",
    "llvm.x86.avx512.mask.pmovus.dw.mem.128",
    "llvm.x86.avx512.mask.pmovus.dw.mem.256",
    "llvm.x86.avx512.mask.pmovus.dw.mem.512",
    "llvm.x86.avx512.mask.pmovus.qb.128",
    "llvm.x86.avx512.mask.pmovus.qb.256",
    "llvm.x86.avx512.mask.pmovus.qb.512",
    "llvm.x86.avx512.mask.pmovus.qb.mem.128",
    "llvm.x86.avx512.mask.pmovus.qb.mem.256",
    "llvm.x86.avx512.mask.pmovus.qb.mem.512",
    "llvm.x86.avx512.mask.pmovus.qd.128",
    "llvm.x86.avx512.mask.pmovus.qd.256",
    "llvm.x86.avx512.mask.pmovus.qd.512",
    "llvm.x86.avx512.mask.pmovus.qd.mem.128",
    "llvm.x86.avx512.mask.pmovus.qd.mem.256",
    "llvm.x86.avx512.mask.pmovus.qd.mem.512",
    "llvm.x86.avx512.mask.pmovus.qw.128",
    "llvm.x86.avx512.mask.pmovus.qw.256",
    "llvm.x86.avx512.mask.pmovus.qw.512",
    "llvm.x86.avx512.mask.pmovus.qw.mem.128",
    "llvm.x86.avx512.mask.pmovus.qw.mem.256",
    "llvm.x86.avx512.mask.pmovus.qw.mem.512",
    "llvm.x86.avx512.mask.pmovus.wb.128",
    "llvm.x86.avx512.mask.pmovus.wb.256",
    "llvm.x86.avx512.mask.pmovus.wb.512",
    "llvm.x86.avx512.mask.pmovus.wb.mem.128",
    "llvm.x86.avx512.mask.pmovus.wb.mem.256",
    "llvm.x86.avx512.mask.pmovus.wb.mem.512",
    "llvm.x86.avx512.mask.pmul.hr.sw.128",
    "llvm.x86.avx512.mask.pmul.hr.sw.256",
    "llvm.x86.avx512.mask.pmul.hr.sw.512",
    "llvm.x86.avx512.mask.pmulh.w.128",
    "llvm.x86.avx512.mask.pmulh.w.256",
    "llvm.x86.avx512.mask.pmulh.w.512",
    "llvm.x86.avx512.mask.pmulhu.w.128",
    "llvm.x86.avx512.mask.pmulhu.w.256",
    "llvm.x86.avx512.mask.pmulhu.w.512",
    "llvm.x86.avx512.mask.pmultishift.qb.128",
    "llvm.x86.avx512.mask.pmultishift.qb.256",
    "llvm.x86.avx512.mask.pmultishift.qb.512",
    "llvm.x86.avx512.mask.prol.d.128",
    "llvm.x86.avx512.mask.prol.d.256",
    "llvm.x86.avx512.mask.prol.d.512",
    "llvm.x86.avx512.mask.prol.q.128",
    "llvm.x86.avx512.mask.prol.q.256",
    "llvm.x86.avx512.mask.prol.q.512",
    "llvm.x86.avx512.mask.prolv.d.128",
    "llvm.x86.avx512.mask.prolv.d.256",
    "llvm.x86.avx512.mask.prolv.d.512",
    "llvm.x86.avx512.mask.prolv.q.128",
    "llvm.x86.avx512.mask.prolv.q.256",
    "llvm.x86.avx512.mask.prolv.q.512",
    "llvm.x86.avx512.mask.pror.d.128",
    "llvm.x86.avx512.mask.pror.d.256",
    "llvm.x86.avx512.mask.pror.d.512",
    "llvm.x86.avx512.mask.pror.q.128",
    "llvm.x86.avx512.mask.pror.q.256",
    "llvm.x86.avx512.mask.pror.q.512",
    "llvm.x86.avx512.mask.prorv.d.128",
    "llvm.x86.avx512.mask.prorv.d.256",
    "llvm.x86.avx512.mask.prorv.d.512",
    "llvm.x86.avx512.mask.prorv.q.128",
    "llvm.x86.avx512.mask.prorv.q.256",
    "llvm.x86.avx512.mask.prorv.q.512",
    "llvm.x86.avx512.mask.psubs.b.128",
    "llvm.x86.avx512.mask.psubs.b.256",
    "llvm.x86.avx512.mask.psubs.b.512",
    "llvm.x86.avx512.mask.psubs.w.128",
    "llvm.x86.avx512.mask.psubs.w.256",
    "llvm.x86.avx512.mask.psubs.w.512",
    "llvm.x86.avx512.mask.psubus.b.128",
    "llvm.x86.avx512.mask.psubus.b.256",
    "llvm.x86.avx512.mask.psubus.b.512",
    "llvm.x86.avx512.mask.psubus.w.128",
    "llvm.x86.avx512.mask.psubus.w.256",
    "llvm.x86.avx512.mask.psubus.w.512",
    "llvm.x86.avx512.mask.pternlog.d.128",
    "llvm.x86.avx512.mask.pternlog.d.256",
    "llvm.x86.avx512.mask.pternlog.d.512",
    "llvm.x86.avx512.mask.pternlog.q.128",
    "llvm.x86.avx512.mask.pternlog.q.256",
    "llvm.x86.avx512.mask.pternlog.q.512",
    "llvm.x86.avx512.mask.range.pd.128",
    "llvm.x86.avx512.mask.range.pd.256",
    "llvm.x86.avx512.mask.range.pd.512",
    "llvm.x86.avx512.mask.range.ps.128",
    "llvm.x86.avx512.mask.range.ps.256",
    "llvm.x86.avx512.mask.range.ps.512",
    "llvm.x86.avx512.mask.range.sd",
    "llvm.x86.avx512.mask.range.ss",
    "llvm.x86.avx512.mask.reduce.pd.128",
    "llvm.x86.avx512.mask.reduce.pd.256",
    "llvm.x86.avx512.mask.reduce.pd.512",
    "llvm.x86.avx512.mask.reduce.ps.128",
    "llvm.x86.avx512.mask.reduce.ps.256",
    "llvm.x86.avx512.mask.reduce.ps.512",
    "llvm.x86.avx512.mask.reduce.sd",
    "llvm.x86.avx512.mask.reduce.ss",
    "llvm.x86.avx512.mask.rndscale.pd.128",
    "llvm.x86.avx512.mask.rndscale.pd.256",
    "llvm.x86.avx512.mask.rndscale.pd.512",
    "llvm.x86.avx512.mask.rndscale.ps.128",
    "llvm.x86.avx512.mask.rndscale.ps.256",
    "llvm.x86.avx512.mask.rndscale.ps.512",
    "llvm.x86.avx512.mask.rndscale.sd",
    "llvm.x86.avx512.mask.rndscale.ss",
    "llvm.x86.avx512.mask.scalef.pd.128",
    "llvm.x86.avx512.mask.scalef.pd.256",
    "llvm.x86.avx512.mask.scalef.pd.512",
    "llvm.x86.avx512.mask.scalef.ps.128",
    "llvm.x86.avx512.mask.scalef.ps.256",
    "llvm.x86.avx512.mask.scalef.ps.512",
    "llvm.x86.avx512.mask.scalef.sd",
    "llvm.x86.avx512.mask.scalef.ss",
    "llvm.x86.avx512.mask.sqrt.pd.128",
    "llvm.x86.avx512.mask.sqrt.pd.256",
    "llvm.x86.avx512.mask.sqrt.pd.512",
    "llvm.x86.avx512.mask.sqrt.ps.128",
    "llvm.x86.avx512.mask.sqrt.ps.256",
    "llvm.x86.avx512.mask.sqrt.ps.512",
    "llvm.x86.avx512.mask.sqrt.sd",
    "llvm.x86.avx512.mask.sqrt.ss",
    "llvm.x86.avx512.mask.store.ss",
    "llvm.x86.avx512.mask.sub.pd.512",
    "llvm.x86.avx512.mask.sub.ps.512",
    "llvm.x86.avx512.mask.sub.sd.round",
    "llvm.x86.avx512.mask.sub.ss.round",
    "llvm.x86.avx512.mask.vcvtph2ps.128",
    "llvm.x86.avx512.mask.vcvtph2ps.256",
    "llvm.x86.avx512.mask.vcvtph2ps.512",
    "llvm.x86.avx512.mask.vcvtps2ph.128",
    "llvm.x86.avx512.mask.vcvtps2ph.256",
    "llvm.x86.avx512.mask.vcvtps2ph.512",
    "llvm.x86.avx512.mask.vfmadd.pd.128",
    "llvm.x86.avx512.mask.vfmadd.pd.256",
    "llvm.x86.avx512.mask.vfmadd.pd.512",
    "llvm.x86.avx512.mask.vfmadd.ps.128",
    "llvm.x86.avx512.mask.vfmadd.ps.256",
    "llvm.x86.avx512.mask.vfmadd.ps.512",
    "llvm.x86.avx512.mask.vfmadd.sd",
    "llvm.x86.avx512.mask.vfmadd.ss",
    "llvm.x86.avx512.mask.vfmaddsub.pd.128",
    "llvm.x86.avx512.mask.vfmaddsub.pd.256",
    "llvm.x86.avx512.mask.vfmaddsub.pd.512",
    "llvm.x86.avx512.mask.vfmaddsub.ps.128",
    "llvm.x86.avx512.mask.vfmaddsub.ps.256",
    "llvm.x86.avx512.mask.vfmaddsub.ps.512",
    "llvm.x86.avx512.mask.vfnmadd.pd.128",
    "llvm.x86.avx512.mask.vfnmadd.pd.256",
    "llvm.x86.avx512.mask.vfnmadd.pd.512",
    "llvm.x86.avx512.mask.vfnmadd.ps.128",
    "llvm.x86.avx512.mask.vfnmadd.ps.256",
    "llvm.x86.avx512.mask.vfnmadd.ps.512",
    "llvm.x86.avx512.mask.vfnmsub.pd.128",
    "llvm.x86.avx512.mask.vfnmsub.pd.256",
    "llvm.x86.avx512.mask.vfnmsub.pd.512",
    "llvm.x86.avx512.mask.vfnmsub.ps.128",
    "llvm.x86.avx512.mask.vfnmsub.ps.256",
    "llvm.x86.avx512.mask.vfnmsub.ps.512",
    "llvm.x86.avx512.mask.vpdpbusd.128",
    "llvm.x86.avx512.mask.vpdpbusd.256",
    "llvm.x86.avx512.mask.vpdpbusd.512",
    "llvm.x86.avx512.mask.vpdpbusds.128",
    "llvm.x86.avx512.mask.vpdpbusds.256",
    "llvm.x86.avx512.mask.vpdpbusds.512",
    "llvm.x86.avx512.mask.vpdpwssd.128",
    "llvm.x86.avx512.mask.vpdpwssd.256",
    "llvm.x86.avx512.mask.vpdpwssd.512",
    "llvm.x86.avx512.mask.vpdpwssds.128",
    "llvm.x86.avx512.mask.vpdpwssds.256",
    "llvm.x86.avx512.mask.vpdpwssds.512",
    "llvm.x86.avx512.mask.vpermi2var.d.128",
    "llvm.x86.avx512.mask.vpermi2var.d.256",
    "llvm.x86.avx512.mask.vpermi2var.d.512",
    "llvm.x86.avx512.mask.vpermi2var.hi.128",
    "llvm.x86.avx512.mask.vpermi2var.hi.256",
    "llvm.x86.avx512.mask.vpermi2var.hi.512",
    "llvm.x86.avx512.mask.vpermi2var.pd.128",
    "llvm.x86.avx512.mask.vpermi2var.pd.256",
    "llvm.x86.avx512.mask.vpermi2var.pd.512",
    "llvm.x86.avx512.mask.vpermi2var.ps.128",
    "llvm.x86.avx512.mask.vpermi2var.ps.256",
    "llvm.x86.avx512.mask.vpermi2var.ps.512",
    "llvm.x86.avx512.mask.vpermi2var.q.128",
    "llvm.x86.avx512.mask.vpermi2var.q.256",
    "llvm.x86.avx512.mask.vpermi2var.q.512",
    "llvm.x86.avx512.mask.vpermi2var.qi.128",
    "llvm.x86.avx512.mask.vpermi2var.qi.256",
    "llvm.x86.avx512.mask.vpermi2var.qi.512",
    "llvm.x86.avx512.mask.vpermt2var.d.128",
    "llvm.x86.avx512.mask.vpermt2var.d.256",
    "llvm.x86.avx512.mask.vpermt2var.d.512",
    "llvm.x86.avx512.mask.vpermt2var.hi.128",
    "llvm.x86.avx512.mask.vpermt2var.hi.256",
    "llvm.x86.avx512.mask.vpermt2var.hi.512",
    "llvm.x86.avx512.mask.vpermt2var.pd.128",
    "llvm.x86.avx512.mask.vpermt2var.pd.256",
    "llvm.x86.avx512.mask.vpermt2var.pd.512",
    "llvm.x86.avx512.mask.vpermt2var.ps.128",
    "llvm.x86.avx512.mask.vpermt2var.ps.256",
    "llvm.x86.avx512.mask.vpermt2var.ps.512",
    "llvm.x86.avx512.mask.vpermt2var.q.128",
    "llvm.x86.avx512.mask.vpermt2var.q.256",
    "llvm.x86.avx512.mask.vpermt2var.q.512",
    "llvm.x86.avx512.mask.vpermt2var.qi.128",
    "llvm.x86.avx512.mask.vpermt2var.qi.256",
    "llvm.x86.avx512.mask.vpermt2var.qi.512",
    "llvm.x86.avx512.mask.vpmadd52h.uq.128",
    "llvm.x86.avx512.mask.vpmadd52h.uq.256",
    "llvm.x86.avx512.mask.vpmadd52h.uq.512",
    "llvm.x86.avx512.mask.vpmadd52l.uq.128",
    "llvm.x86.avx512.mask.vpmadd52l.uq.256",
    "llvm.x86.avx512.mask.vpmadd52l.uq.512",
    "llvm.x86.avx512.mask.vpshld.d.128",
    "llvm.x86.avx512.mask.vpshld.d.256",
    "llvm.x86.avx512.mask.vpshld.d.512",
    "llvm.x86.avx512.mask.vpshld.q.128",
    "llvm.x86.avx512.mask.vpshld.q.256",
    "llvm.x86.avx512.mask.vpshld.q.512",
    "llvm.x86.avx512.mask.vpshld.w.128",
    "llvm.x86.avx512.mask.vpshld.w.256",
    "llvm.x86.avx512.mask.vpshld.w.512",
    "llvm.x86.avx512.mask.vpshldv.d.128",
    "llvm.x86.avx512.mask.vpshldv.d.256",
    "llvm.x86.avx512.mask.vpshldv.d.512",
    "llvm.x86.avx512.mask.vpshldv.q.128",
    "llvm.x86.avx512.mask.vpshldv.q.256",
    "llvm.x86.avx512.mask.vpshldv.q.512",
    "llvm.x86.avx512.mask.vpshldv.w.128",
    "llvm.x86.avx512.mask.vpshldv.w.256",
    "llvm.x86.avx512.mask.vpshldv.w.512",
    "llvm.x86.avx512.mask.vpshrd.d.128",
    "llvm.x86.avx512.mask.vpshrd.d.256",
    "llvm.x86.avx512.mask.vpshrd.d.512",
    "llvm.x86.avx512.mask.vpshrd.q.128",
    "llvm.x86.avx512.mask.vpshrd.q.256",
    "llvm.x86.avx512.mask.vpshrd.q.512",
    "llvm.x86.avx512.mask.vpshrd.w.128",
    "llvm.x86.avx512.mask.vpshrd.w.256",
    "llvm.x86.avx512.mask.vpshrd.w.512",
    "llvm.x86.avx512.mask.vpshrdv.d.128",
    "llvm.x86.avx512.mask.vpshrdv.d.256",
    "llvm.x86.avx512.mask.vpshrdv.d.512",
    "llvm.x86.avx512.mask.vpshrdv.q.128",
    "llvm.x86.avx512.mask.vpshrdv.q.256",
    "llvm.x86.avx512.mask.vpshrdv.q.512",
    "llvm.x86.avx512.mask.vpshrdv.w.128",
    "llvm.x86.avx512.mask.vpshrdv.w.256",
    "llvm.x86.avx512.mask.vpshrdv.w.512",
    "llvm.x86.avx512.mask.vpshufbitqmb.128",
    "llvm.x86.avx512.mask.vpshufbitqmb.256",
    "llvm.x86.avx512.mask.vpshufbitqmb.512",
    "llvm.x86.avx512.mask3.vfmadd.pd.128",
    "llvm.x86.avx512.mask3.vfmadd.pd.256",
    "llvm.x86.avx512.mask3.vfmadd.pd.512",
    "llvm.x86.avx512.mask3.vfmadd.ps.128",
    "llvm.x86.avx512.mask3.vfmadd.ps.256",
    "llvm.x86.avx512.mask3.vfmadd.ps.512",
    "llvm.x86.avx512.mask3.vfmadd.sd",
    "llvm.x86.avx512.mask3.vfmadd.ss",
    "llvm.x86.avx512.mask3.vfmaddsub.pd.128",
    "llvm.x86.avx512.mask3.vfmaddsub.pd.256",
    "llvm.x86.avx512.mask3.vfmaddsub.pd.512",
    "llvm.x86.avx512.mask3.vfmaddsub.ps.128",
    "llvm.x86.avx512.mask3.vfmaddsub.ps.256",
    "llvm.x86.avx512.mask3.vfmaddsub.ps.512",
    "llvm.x86.avx512.mask3.vfmsub.pd.128",
    "llvm.x86.avx512.mask3.vfmsub.pd.256",
    "llvm.x86.avx512.mask3.vfmsub.pd.512",
    "llvm.x86.avx512.mask3.vfmsub.ps.128",
    "llvm.x86.avx512.mask3.vfmsub.ps.256",
    "llvm.x86.avx512.mask3.vfmsub.ps.512",
    "llvm.x86.avx512.mask3.vfmsub.sd",
    "llvm.x86.avx512.mask3.vfmsub.ss",
    "llvm.x86.avx512.mask3.vfmsubadd.pd.128",
    "llvm.x86.avx512.mask3.vfmsubadd.pd.256",
    "llvm.x86.avx512.mask3.vfmsubadd.pd.512",
    "llvm.x86.avx512.mask3.vfmsubadd.ps.128",
    "llvm.x86.avx512.mask3.vfmsubadd.ps.256",
    "llvm.x86.avx512.mask3.vfmsubadd.ps.512",
    "llvm.x86.avx512.mask3.vfnmsub.pd.128",
    "llvm.x86.avx512.mask3.vfnmsub.pd.256",
    "llvm.x86.avx512.mask3.vfnmsub.pd.512",
    "llvm.x86.avx512.mask3.vfnmsub.ps.128",
    "llvm.x86.avx512.mask3.vfnmsub.ps.256",
    "llvm.x86.avx512.mask3.vfnmsub.ps.512",
    "llvm.x86.avx512.mask3.vfnmsub.sd",
    "llvm.x86.avx512.mask3.vfnmsub.ss",
    "llvm.x86.avx512.maskz.fixupimm.pd.128",
    "llvm.x86.avx512.maskz.fixupimm.pd.256",
    "llvm.x86.avx512.maskz.fixupimm.pd.512",
    "llvm.x86.avx512.maskz.fixupimm.ps.128",
    "llvm.x86.avx512.maskz.fixupimm.ps.256",
    "llvm.x86.avx512.maskz.fixupimm.ps.512",
    "llvm.x86.avx512.maskz.fixupimm.sd",
    "llvm.x86.avx512.maskz.fixupimm.ss",
    "llvm.x86.avx512.maskz.pternlog.d.128",
    "llvm.x86.avx512.maskz.pternlog.d.256",
    "llvm.x86.avx512.maskz.pternlog.d.512",
    "llvm.x86.avx512.maskz.pternlog.q.128",
    "llvm.x86.avx512.maskz.pternlog.q.256",
    "llvm.x86.avx512.maskz.pternlog.q.512",
    "llvm.x86.avx512.maskz.vfmadd.pd.128",
    "llvm.x86.avx512.maskz.vfmadd.pd.256",
    "llvm.x86.avx512.maskz.vfmadd.pd.512",
    "llvm.x86.avx512.maskz.vfmadd.ps.128",
    "llvm.x86.avx512.maskz.vfmadd.ps.256",
    "llvm.x86.avx512.maskz.vfmadd.ps.512",
    "llvm.x86.avx512.maskz.vfmadd.sd",
    "llvm.x86.avx512.maskz.vfmadd.ss",
    "llvm.x86.avx512.maskz.vfmaddsub.pd.128",
    "llvm.x86.avx512.maskz.vfmaddsub.pd.256",
    "llvm.x86.avx512.maskz.vfmaddsub.pd.512",
    "llvm.x86.avx512.maskz.vfmaddsub.ps.128",
    "llvm.x86.avx512.maskz.vfmaddsub.ps.256",
    "llvm.x86.avx512.maskz.vfmaddsub.ps.512",
    "llvm.x86.avx512.maskz.vpdpbusd.128",
    "llvm.x86.avx512.maskz.vpdpbusd.256",
    "llvm.x86.avx512.maskz.vpdpbusd.512",
    "llvm.x86.avx512.maskz.vpdpbusds.128",
    "llvm.x86.avx512.maskz.vpdpbusds.256",
    "llvm.x86.avx512.maskz.vpdpbusds.512",
    "llvm.x86.avx512.maskz.vpdpwssd.128",
    "llvm.x86.avx512.maskz.vpdpwssd.256",
    "llvm.x86.avx512.maskz.vpdpwssd.512",
    "llvm.x86.avx512.maskz.vpdpwssds.128",
    "llvm.x86.avx512.maskz.vpdpwssds.256",
    "llvm.x86.avx512.maskz.vpdpwssds.512",
    "llvm.x86.avx512.maskz.vpermt2var.d.128",
    "llvm.x86.avx512.maskz.vpermt2var.d.256",
    "llvm.x86.avx512.maskz.vpermt2var.d.512",
    "llvm.x86.avx512.maskz.vpermt2var.hi.128",
    "llvm.x86.avx512.maskz.vpermt2var.hi.256",
    "llvm.x86.avx512.maskz.vpermt2var.hi.512",
    "llvm.x86.avx512.maskz.vpermt2var.pd.128",
    "llvm.x86.avx512.maskz.vpermt2var.pd.256",
    "llvm.x86.avx512.maskz.vpermt2var.pd.512",
    "llvm.x86.avx512.maskz.vpermt2var.ps.128",
    "llvm.x86.avx512.maskz.vpermt2var.ps.256",
    "llvm.x86.avx512.maskz.vpermt2var.ps.512",
    "llvm.x86.avx512.maskz.vpermt2var.q.128",
    "llvm.x86.avx512.maskz.vpermt2var.q.256",
    "llvm.x86.avx512.maskz.vpermt2var.q.512",
    "llvm.x86.avx512.maskz.vpermt2var.qi.128",
    "llvm.x86.avx512.maskz.vpermt2var.qi.256",
    "llvm.x86.avx512.maskz.vpermt2var.qi.512",
    "llvm.x86.avx512.maskz.vpmadd52h.uq.128",
    "llvm.x86.avx512.maskz.vpmadd52h.uq.256",
    "llvm.x86.avx512.maskz.vpmadd52h.uq.512",
    "llvm.x86.avx512.maskz.vpmadd52l.uq.128",
    "llvm.x86.avx512.maskz.vpmadd52l.uq.256",
    "llvm.x86.avx512.maskz.vpmadd52l.uq.512",
    "llvm.x86.avx512.maskz.vpshldv.d.128",
    "llvm.x86.avx512.maskz.vpshldv.d.256",
    "llvm.x86.avx512.maskz.vpshldv.d.512",
    "llvm.x86.avx512.maskz.vpshldv.q.128",
    "llvm.x86.avx512.maskz.vpshldv.q.256",
    "llvm.x86.avx512.maskz.vpshldv.q.512",
    "llvm.x86.avx512.maskz.vpshldv.w.128",
    "llvm.x86.avx512.maskz.vpshldv.w.256",
    "llvm.x86.avx512.maskz.vpshldv.w.512",
    "llvm.x86.avx512.maskz.vpshrdv.d.128",
    "llvm.x86.avx512.maskz.vpshrdv.d.256",
    "llvm.x86.avx512.maskz.vpshrdv.d.512",
    "llvm.x86.avx512.maskz.vpshrdv.q.128",
    "llvm.x86.avx512.maskz.vpshrdv.q.256",
    "llvm.x86.avx512.maskz.vpshrdv.q.512",
    "llvm.x86.avx512.maskz.vpshrdv.w.128",
    "llvm.x86.avx512.maskz.vpshrdv.w.256",
    "llvm.x86.avx512.maskz.vpshrdv.w.512",
    "llvm.x86.avx512.packssdw.512",
    "llvm.x86.avx512.packsswb.512",
    "llvm.x86.avx512.packusdw.512",
    "llvm.x86.avx512.packuswb.512",
    "llvm.x86.avx512.pmul.dq.512",
    "llvm.x86.avx512.pmulu.dq.512",
    "llvm.x86.avx512.psad.bw.512",
    "llvm.x86.avx512.pshuf.b.512",
    "llvm.x86.avx512.psll.d.512",
    "llvm.x86.avx512.psll.q.512",
    "llvm.x86.avx512.psll.w.512",
    "llvm.x86.avx512.pslli.d.512",
    "llvm.x86.avx512.pslli.q.512",
    "llvm.x86.avx512.pslli.w.512",
    "llvm.x86.avx512.psllv.d.512",
    "llvm.x86.avx512.psllv.q.512",
    "llvm.x86.avx512.psllv.w.128",
    "llvm.x86.avx512.psllv.w.256",
    "llvm.x86.avx512.psllv.w.512",
    "llvm.x86.avx512.psra.d.512",
    "llvm.x86.avx512.psra.q.128",
    "llvm.x86.avx512.psra.q.256",
    "llvm.x86.avx512.psra.q.512",
    "llvm.x86.avx512.psra.w.512",
    "llvm.x86.avx512.psrai.d.512",
    "llvm.x86.avx512.psrai.q.128",
    "llvm.x86.avx512.psrai.q.256",
    "llvm.x86.avx512.psrai.q.512",
    "llvm.x86.avx512.psrai.w.512",
    "llvm.x86.avx512.psrav.d.512",
    "llvm.x86.avx512.psrav.q.128",
    "llvm.x86.avx512.psrav.q.256",
    "llvm.x86.avx512.psrav.q.512",
    "llvm.x86.avx512.psrav.w.128",
    "llvm.x86.avx512.psrav.w.256",
    "llvm.x86.avx512.psrav.w.512",
    "llvm.x86.avx512.psrl.d.512",
    "llvm.x86.avx512.psrl.q.512",
    "llvm.x86.avx512.psrl.w.512",
    "llvm.x86.avx512.psrli.d.512",
    "llvm.x86.avx512.psrli.q.512",
    "llvm.x86.avx512.psrli.w.512",
    "llvm.x86.avx512.psrlv.d.512",
    "llvm.x86.avx512.psrlv.q.512",
    "llvm.x86.avx512.psrlv.w.128",
    "llvm.x86.avx512.psrlv.w.256",
    "llvm.x86.avx512.psrlv.w.512",
    "llvm.x86.avx512.rcp14.pd.128",
    "llvm.x86.avx512.rcp14.pd.256",
    "llvm.x86.avx512.rcp14.pd.512",
    "llvm.x86.avx512.rcp14.ps.128",
    "llvm.x86.avx512.rcp14.ps.256",
    "llvm.x86.avx512.rcp14.ps.512",
    "llvm.x86.avx512.rcp14.sd",
    "llvm.x86.avx512.rcp14.ss",
    "llvm.x86.avx512.rcp28.pd",
    "llvm.x86.avx512.rcp28.ps",
    "llvm.x86.avx512.rcp28.sd",
    "llvm.x86.avx512.rcp28.ss",
    "llvm.x86.avx512.rsqrt14.pd.128",
    "llvm.x86.avx512.rsqrt14.pd.256",
    "llvm.x86.avx512.rsqrt14.pd.512",
    "llvm.x86.avx512.rsqrt14.ps.128",
    "llvm.x86.avx512.rsqrt14.ps.256",
    "llvm.x86.avx512.rsqrt14.ps.512",
    "llvm.x86.avx512.rsqrt14.sd",
    "llvm.x86.avx512.rsqrt14.ss",
    "llvm.x86.avx512.rsqrt28.pd",
    "llvm.x86.avx512.rsqrt28.ps",
    "llvm.x86.avx512.rsqrt28.sd",
    "llvm.x86.avx512.rsqrt28.ss",
    "llvm.x86.avx512.scatter.dpd.512",
    "llvm.x86.avx512.scatter.dpi.512",
    "llvm.x86.avx512.scatter.dpq.512",
    "llvm.x86.avx512.scatter.dps.512",
    "llvm.x86.avx512.scatter.qpd.512",
    "llvm.x86.avx512.scatter.qpi.512",
    "llvm.x86.avx512.scatter.qpq.512",
    "llvm.x86.avx512.scatter.qps.512",
    "llvm.x86.avx512.scatterdiv2.df",
    "llvm.x86.avx512.scatterdiv2.di",
    "llvm.x86.avx512.scatterdiv4.df",
    "llvm.x86.avx512.scatterdiv4.di",
    "llvm.x86.avx512.scatterdiv4.sf",
    "llvm.x86.avx512.scatterdiv4.si",
    "llvm.x86.avx512.scatterdiv8.sf",
    "llvm.x86.avx512.scatterdiv8.si",
    "llvm.x86.avx512.scatterpf.dpd.512",
    "llvm.x86.avx512.scatterpf.dps.512",
    "llvm.x86.avx512.scatterpf.qpd.512",
    "llvm.x86.avx512.scatterpf.qps.512",
    "llvm.x86.avx512.scattersiv2.df",
    "llvm.x86.avx512.scattersiv2.di",
    "llvm.x86.avx512.scattersiv4.df",
    "llvm.x86.avx512.scattersiv4.di",
    "llvm.x86.avx512.scattersiv4.sf",
    "llvm.x86.avx512.scattersiv4.si",
    "llvm.x86.avx512.scattersiv8.sf",
    "llvm.x86.avx512.scattersiv8.si",
    "llvm.x86.avx512.vbroadcast.sd.512",
    "llvm.x86.avx512.vbroadcast.ss.512",
    "llvm.x86.avx512.vcomi.sd",
    "llvm.x86.avx512.vcomi.ss",
    "llvm.x86.avx512.vcvtsd2si32",
    "llvm.x86.avx512.vcvtsd2si64",
    "llvm.x86.avx512.vcvtsd2usi32",
    "llvm.x86.avx512.vcvtsd2usi64",
    "llvm.x86.avx512.vcvtss2si32",
    "llvm.x86.avx512.vcvtss2si64",
    "llvm.x86.avx512.vcvtss2usi32",
    "llvm.x86.avx512.vcvtss2usi64",
    "llvm.x86.avx512.vpermilvar.pd.512",
    "llvm.x86.avx512.vpermilvar.ps.512",
    "llvm.x86.bmi.bextr.32",
    "llvm.x86.bmi.bextr.64",
    "llvm.x86.bmi.bzhi.32",
    "llvm.x86.bmi.bzhi.64",
    "llvm.x86.bmi.pdep.32",
    "llvm.x86.bmi.pdep.64",
    "llvm.x86.bmi.pext.32",
    "llvm.x86.bmi.pext.64",
    "llvm.x86.clflushopt",
    "llvm.x86.clrssbsy",
    "llvm.x86.clwb",
    "llvm.x86.clzero",
    "llvm.x86.flags.read.u32",
    "llvm.x86.flags.read.u64",
    "llvm.x86.flags.write.u32",
    "llvm.x86.flags.write.u64",
    "llvm.x86.fma.vfmadd.pd",
    "llvm.x86.fma.vfmadd.pd.256",
    "llvm.x86.fma.vfmadd.ps",
    "llvm.x86.fma.vfmadd.ps.256",
    "llvm.x86.fma.vfmadd.sd",
    "llvm.x86.fma.vfmadd.ss",
    "llvm.x86.fma.vfmaddsub.pd",
    "llvm.x86.fma.vfmaddsub.pd.256",
    "llvm.x86.fma.vfmaddsub.ps",
    "llvm.x86.fma.vfmaddsub.ps.256",
    "llvm.x86.fma.vfmsub.pd",
    "llvm.x86.fma.vfmsub.pd.256",
    "llvm.x86.fma.vfmsub.ps",
    "llvm.x86.fma.vfmsub.ps.256",
    "llvm.x86.fma.vfmsub.sd",
    "llvm.x86.fma.vfmsub.ss",
    "llvm.x86.fma.vfmsubadd.pd",
    "llvm.x86.fma.vfmsubadd.pd.256",
    "llvm.x86.fma.vfmsubadd.ps",
    "llvm.x86.fma.vfmsubadd.ps.256",
    "llvm.x86.fma.vfnmadd.pd",
    "llvm.x86.fma.vfnmadd.pd.256",
    "llvm.x86.fma.vfnmadd.ps",
    "llvm.x86.fma.vfnmadd.ps.256",
    "llvm.x86.fma.vfnmadd.sd",
    "llvm.x86.fma.vfnmadd.ss",
    "llvm.x86.fma.vfnmsub.pd",
    "llvm.x86.fma.vfnmsub.pd.256",
    "llvm.x86.fma.vfnmsub.ps",
    "llvm.x86.fma.vfnmsub.ps.256",
    "llvm.x86.fma.vfnmsub.sd",
    "llvm.x86.fma.vfnmsub.ss",
    "llvm.x86.fma4.vfmadd.sd",
    "llvm.x86.fma4.vfmadd.ss",
    "llvm.x86.fxrstor",
    "llvm.x86.fxrstor64",
    "llvm.x86.fxsave",
    "llvm.x86.fxsave64",
    "llvm.x86.incsspd",
    "llvm.x86.incsspq",
    "llvm.x86.int",
    "llvm.x86.llwpcb",
    "llvm.x86.lwpins32",
    "llvm.x86.lwpins64",
    "llvm.x86.lwpval32",
    "llvm.x86.lwpval64",
    "llvm.x86.mmx.emms",
    "llvm.x86.mmx.femms",
    "llvm.x86.mmx.maskmovq",
    "llvm.x86.mmx.movnt.dq",
    "llvm.x86.mmx.packssdw",
    "llvm.x86.mmx.packsswb",
    "llvm.x86.mmx.packuswb",
    "llvm.x86.mmx.padd.b",
    "llvm.x86.mmx.padd.d",
    "llvm.x86.mmx.padd.q",
    "llvm.x86.mmx.padd.w",
    "llvm.x86.mmx.padds.b",
    "llvm.x86.mmx.padds.w",
    "llvm.x86.mmx.paddus.b",
    "llvm.x86.mmx.paddus.w",
    "llvm.x86.mmx.palignr.b",
    "llvm.x86.mmx.pand",
    "llvm.x86.mmx.pandn",
    "llvm.x86.mmx.pavg.b",
    "llvm.x86.mmx.pavg.w",
    "llvm.x86.mmx.pcmpeq.b",
    "llvm.x86.mmx.pcmpeq.d",
    "llvm.x86.mmx.pcmpeq.w",
    "llvm.x86.mmx.pcmpgt.b",
    "llvm.x86.mmx.pcmpgt.d",
    "llvm.x86.mmx.pcmpgt.w",
    "llvm.x86.mmx.pextr.w",
    "llvm.x86.mmx.pinsr.w",
    "llvm.x86.mmx.pmadd.wd",
    "llvm.x86.mmx.pmaxs.w",
    "llvm.x86.mmx.pmaxu.b",
    "llvm.x86.mmx.pmins.w",
    "llvm.x86.mmx.pminu.b",
    "llvm.x86.mmx.pmovmskb",
    "llvm.x86.mmx.pmulh.w",
    "llvm.x86.mmx.pmulhu.w",
    "llvm.x86.mmx.pmull.w",
    "llvm.x86.mmx.pmulu.dq",
    "llvm.x86.mmx.por",
    "llvm.x86.mmx.psad.bw",
    "llvm.x86.mmx.psll.d",
    "llvm.x86.mmx.psll.q",
    "llvm.x86.mmx.psll.w",
    "llvm.x86.mmx.pslli.d",
    "llvm.x86.mmx.pslli.q",
    "llvm.x86.mmx.pslli.w",
    "llvm.x86.mmx.psra.d",
    "llvm.x86.mmx.psra.w",
    "llvm.x86.mmx.psrai.d",
    "llvm.x86.mmx.psrai.w",
    "llvm.x86.mmx.psrl.d",
    "llvm.x86.mmx.psrl.q",
    "llvm.x86.mmx.psrl.w",
    "llvm.x86.mmx.psrli.d",
    "llvm.x86.mmx.psrli.q",
    "llvm.x86.mmx.psrli.w",
    "llvm.x86.mmx.psub.b",
    "llvm.x86.mmx.psub.d",
    "llvm.x86.mmx.psub.q",
    "llvm.x86.mmx.psub.w",
    "llvm.x86.mmx.psubs.b",
    "llvm.x86.mmx.psubs.w",
    "llvm.x86.mmx.psubus.b",
    "llvm.x86.mmx.psubus.w",
    "llvm.x86.mmx.punpckhbw",
    "llvm.x86.mmx.punpckhdq",
    "llvm.x86.mmx.punpckhwd",
    "llvm.x86.mmx.punpcklbw",
    "llvm.x86.mmx.punpckldq",
    "llvm.x86.mmx.punpcklwd",
    "llvm.x86.mmx.pxor",
    "llvm.x86.monitorx",
    "llvm.x86.mwaitx",
    "llvm.x86.pclmulqdq",
    "llvm.x86.pclmulqdq.256",
    "llvm.x86.pclmulqdq.512",
    "llvm.x86.rdfsbase.32",
    "llvm.x86.rdfsbase.64",
    "llvm.x86.rdgsbase.32",
    "llvm.x86.rdgsbase.64",
    "llvm.x86.rdpkru",
    "llvm.x86.rdpmc",
    "llvm.x86.rdrand.16",
    "llvm.x86.rdrand.32",
    "llvm.x86.rdrand.64",
    "llvm.x86.rdseed.16",
    "llvm.x86.rdseed.32",
    "llvm.x86.rdseed.64",
    "llvm.x86.rdsspd",
    "llvm.x86.rdsspq",
    "llvm.x86.rdtsc",
    "llvm.x86.rdtscp",
    "llvm.x86.rstorssp",
    "llvm.x86.saveprevssp",
    "llvm.x86.seh.ehguard",
    "llvm.x86.seh.ehregnode",
    "llvm.x86.seh.lsda",
    "llvm.x86.seh.recoverfp",
    "llvm.x86.setssbsy",
    "llvm.x86.sha1msg1",
    "llvm.x86.sha1msg2",
    "llvm.x86.sha1nexte",
    "llvm.x86.sha1rnds4",
    "llvm.x86.sha256msg1",
    "llvm.x86.sha256msg2",
    "llvm.x86.sha256rnds2",
    "llvm.x86.slwpcb",
    "llvm.x86.sse.cmp.ps",
    "llvm.x86.sse.cmp.ss",
    "llvm.x86.sse.comieq.ss",
    "llvm.x86.sse.comige.ss",
    "llvm.x86.sse.comigt.ss",
    "llvm.x86.sse.comile.ss",
    "llvm.x86.sse.comilt.ss",
    "llvm.x86.sse.comineq.ss",
    "llvm.x86.sse.cvtpd2pi",
    "llvm.x86.sse.cvtpi2pd",
    "llvm.x86.sse.cvtpi2ps",
    "llvm.x86.sse.cvtps2pi",
    "llvm.x86.sse.cvtsi2ss",
    "llvm.x86.sse.cvtsi642ss",
    "llvm.x86.sse.cvtss2si",
    "llvm.x86.sse.cvtss2si64",
    "llvm.x86.sse.cvttpd2pi",
    "llvm.x86.sse.cvttps2pi",
    "llvm.x86.sse.cvttss2si",
    "llvm.x86.sse.cvttss2si64",
    "llvm.x86.sse.ldmxcsr",
    "llvm.x86.sse.max.ps",
    "llvm.x86.sse.max.ss",
    "llvm.x86.sse.min.ps",
    "llvm.x86.sse.min.ss",
    "llvm.x86.sse.movmsk.ps",
    "llvm.x86.sse.pshuf.w",
    "llvm.x86.sse.rcp.ps",
    "llvm.x86.sse.rcp.ss",
    "llvm.x86.sse.rsqrt.ps",
    "llvm.x86.sse.rsqrt.ss",
    "llvm.x86.sse.sfence",
    "llvm.x86.sse.sqrt.ps",
    "llvm.x86.sse.sqrt.ss",
    "llvm.x86.sse.stmxcsr",
    "llvm.x86.sse.ucomieq.ss",
    "llvm.x86.sse.ucomige.ss",
    "llvm.x86.sse.ucomigt.ss",
    "llvm.x86.sse.ucomile.ss",
    "llvm.x86.sse.ucomilt.ss",
    "llvm.x86.sse.ucomineq.ss",
    "llvm.x86.sse2.clflush",
    "llvm.x86.sse2.cmp.pd",
    "llvm.x86.sse2.cmp.sd",
    "llvm.x86.sse2.comieq.sd",
    "llvm.x86.sse2.comige.sd",
    "llvm.x86.sse2.comigt.sd",
    "llvm.x86.sse2.comile.sd",
    "llvm.x86.sse2.comilt.sd",
    "llvm.x86.sse2.comineq.sd",
    "llvm.x86.sse2.cvtdq2ps",
    "llvm.x86.sse2.cvtpd2dq",
    "llvm.x86.sse2.cvtpd2ps",
    "llvm.x86.sse2.cvtps2dq",
    "llvm.x86.sse2.cvtsd2si",
    "llvm.x86.sse2.cvtsd2si64",
    "llvm.x86.sse2.cvtsd2ss",
    "llvm.x86.sse2.cvtsi2sd",
    "llvm.x86.sse2.cvtsi642sd",
    "llvm.x86.sse2.cvtss2sd",
    "llvm.x86.sse2.cvttpd2dq",
    "llvm.x86.sse2.cvttps2dq",
    "llvm.x86.sse2.cvttsd2si",
    "llvm.x86.sse2.cvttsd2si64",
    "llvm.x86.sse2.lfence",
    "llvm.x86.sse2.maskmov.dqu",
    "llvm.x86.sse2.max.pd",
    "llvm.x86.sse2.max.sd",
    "llvm.x86.sse2.mfence",
    "llvm.x86.sse2.min.pd",
    "llvm.x86.sse2.min.sd",
    "llvm.x86.sse2.movmsk.pd",
    "llvm.x86.sse2.packssdw.128",
    "llvm.x86.sse2.packsswb.128",
    "llvm.x86.sse2.packuswb.128",
    "llvm.x86.sse2.padds.b",
    "llvm.x86.sse2.padds.w",
    "llvm.x86.sse2.paddus.b",
    "llvm.x86.sse2.paddus.w",
    "llvm.x86.sse2.pause",
    "llvm.x86.sse2.pmadd.wd",
    "llvm.x86.sse2.pmovmskb.128",
    "llvm.x86.sse2.pmulh.w",
    "llvm.x86.sse2.pmulhu.w",
    "llvm.x86.sse2.pmulu.dq",
    "llvm.x86.sse2.psad.bw",
    "llvm.x86.sse2.psll.d",
    "llvm.x86.sse2.psll.q",
    "llvm.x86.sse2.psll.w",
    "llvm.x86.sse2.pslli.d",
    "llvm.x86.sse2.pslli.q",
    "llvm.x86.sse2.pslli.w",
    "llvm.x86.sse2.psra.d",
    "llvm.x86.sse2.psra.w",
    "llvm.x86.sse2.psrai.d",
    "llvm.x86.sse2.psrai.w",
    "llvm.x86.sse2.psrl.d",
    "llvm.x86.sse2.psrl.q",
    "llvm.x86.sse2.psrl.w",
    "llvm.x86.sse2.psrli.d",
    "llvm.x86.sse2.psrli.q",
    "llvm.x86.sse2.psrli.w",
    "llvm.x86.sse2.psubs.b",
    "llvm.x86.sse2.psubs.w",
    "llvm.x86.sse2.psubus.b",
    "llvm.x86.sse2.psubus.w",
    "llvm.x86.sse2.sqrt.pd",
    "llvm.x86.sse2.sqrt.sd",
    "llvm.x86.sse2.ucomieq.sd",
    "llvm.x86.sse2.ucomige.sd",
    "llvm.x86.sse2.ucomigt.sd",
    "llvm.x86.sse2.ucomile.sd",
    "llvm.x86.sse2.ucomilt.sd",
    "llvm.x86.sse2.ucomineq.sd",
    "llvm.x86.sse3.addsub.pd",
    "llvm.x86.sse3.addsub.ps",
    "llvm.x86.sse3.hadd.pd",
    "llvm.x86.sse3.hadd.ps",
    "llvm.x86.sse3.hsub.pd",
    "llvm.x86.sse3.hsub.ps",
    "llvm.x86.sse3.ldu.dq",
    "llvm.x86.sse3.monitor",
    "llvm.x86.sse3.mwait",
    "llvm.x86.sse41.blendvpd",
    "llvm.x86.sse41.blendvps",
    "llvm.x86.sse41.dppd",
    "llvm.x86.sse41.dpps",
    "llvm.x86.sse41.insertps",
    "llvm.x86.sse41.mpsadbw",
    "llvm.x86.sse41.packusdw",
    "llvm.x86.sse41.pblendvb",
    "llvm.x86.sse41.phminposuw",
    "llvm.x86.sse41.pmuldq",
    "llvm.x86.sse41.ptestc",
    "llvm.x86.sse41.ptestnzc",
    "llvm.x86.sse41.ptestz",
    "llvm.x86.sse41.round.pd",
    "llvm.x86.sse41.round.ps",
    "llvm.x86.sse41.round.sd",
    "llvm.x86.sse41.round.ss",
    "llvm.x86.sse42.crc32.32.16",
    "llvm.x86.sse42.crc32.32.32",
    "llvm.x86.sse42.crc32.32.8",
    "llvm.x86.sse42.crc32.64.64",
    "llvm.x86.sse42.pcmpestri128",
    "llvm.x86.sse42.pcmpestria128",
    "llvm.x86.sse42.pcmpestric128",
    "llvm.x86.sse42.pcmpestrio128",
    "llvm.x86.sse42.pcmpestris128",
    "llvm.x86.sse42.pcmpestriz128",
    "llvm.x86.sse42.pcmpestrm128",
    "llvm.x86.sse42.pcmpistri128",
    "llvm.x86.sse42.pcmpistria128",
    "llvm.x86.sse42.pcmpistric128",
    "llvm.x86.sse42.pcmpistrio128",
    "llvm.x86.sse42.pcmpistris128",
    "llvm.x86.sse42.pcmpistriz128",
    "llvm.x86.sse42.pcmpistrm128",
    "llvm.x86.sse4a.extrq",
    "llvm.x86.sse4a.extrqi",
    "llvm.x86.sse4a.insertq",
    "llvm.x86.sse4a.insertqi",
    "llvm.x86.ssse3.pabs.b",
    "llvm.x86.ssse3.pabs.d",
    "llvm.x86.ssse3.pabs.w",
    "llvm.x86.ssse3.phadd.d",
    "llvm.x86.ssse3.phadd.d.128",
    "llvm.x86.ssse3.phadd.sw",
    "llvm.x86.ssse3.phadd.sw.128",
    "llvm.x86.ssse3.phadd.w",
    "llvm.x86.ssse3.phadd.w.128",
    "llvm.x86.ssse3.phsub.d",
    "llvm.x86.ssse3.phsub.d.128",
    "llvm.x86.ssse3.phsub.sw",
    "llvm.x86.ssse3.phsub.sw.128",
    "llvm.x86.ssse3.phsub.w",
    "llvm.x86.ssse3.phsub.w.128",
    "llvm.x86.ssse3.pmadd.ub.sw",
    "llvm.x86.ssse3.pmadd.ub.sw.128",
    "llvm.x86.ssse3.pmul.hr.sw",
    "llvm.x86.ssse3.pmul.hr.sw.128",
    "llvm.x86.ssse3.pshuf.b",
    "llvm.x86.ssse3.pshuf.b.128",
    "llvm.x86.ssse3.psign.b",
    "llvm.x86.ssse3.psign.b.128",
    "llvm.x86.ssse3.psign.d",
    "llvm.x86.ssse3.psign.d.128",
    "llvm.x86.ssse3.psign.w",
    "llvm.x86.ssse3.psign.w.128",
    "llvm.x86.subborrow.u32",
    "llvm.x86.subborrow.u64",
    "llvm.x86.tbm.bextri.u32",
    "llvm.x86.tbm.bextri.u64",
    "llvm.x86.vcvtph2ps.128",
    "llvm.x86.vcvtph2ps.256",
    "llvm.x86.vcvtps2ph.128",
    "llvm.x86.vcvtps2ph.256",
    "llvm.x86.vgf2p8affineinvqb.128",
    "llvm.x86.vgf2p8affineinvqb.256",
    "llvm.x86.vgf2p8affineinvqb.512",
    "llvm.x86.vgf2p8affineqb.128",
    "llvm.x86.vgf2p8affineqb.256",
    "llvm.x86.vgf2p8affineqb.512",
    "llvm.x86.vgf2p8mulb.128",
    "llvm.x86.vgf2p8mulb.256",
    "llvm.x86.vgf2p8mulb.512",
    "llvm.x86.wrfsbase.32",
    "llvm.x86.wrfsbase.64",
    "llvm.x86.wrgsbase.32",
    "llvm.x86.wrgsbase.64",
    "llvm.x86.wrpkru",
    "llvm.x86.wrssd",
    "llvm.x86.wrssq",
    "llvm.x86.wrussd",
    "llvm.x86.wrussq",
    "llvm.x86.xabort",
    "llvm.x86.xbegin",
    "llvm.x86.xend",
    "llvm.x86.xgetbv",
    "llvm.x86.xop.vfrcz.pd",
    "llvm.x86.xop.vfrcz.pd.256",
    "llvm.x86.xop.vfrcz.ps",
    "llvm.x86.xop.vfrcz.ps.256",
    "llvm.x86.xop.vfrcz.sd",
    "llvm.x86.xop.vfrcz.ss",
    "llvm.x86.xop.vpcomb",
    "llvm.x86.xop.vpcomd",
    "llvm.x86.xop.vpcomq",
    "llvm.x86.xop.vpcomub",
    "llvm.x86.xop.vpcomud",
    "llvm.x86.xop.vpcomuq",
    "llvm.x86.xop.vpcomuw",
    "llvm.x86.xop.vpcomw",
    "llvm.x86.xop.vpermil2pd",
    "llvm.x86.xop.vpermil2pd.256",
    "llvm.x86.xop.vpermil2ps",
    "llvm.x86.xop.vpermil2ps.256",
    "llvm.x86.xop.vphaddbd",
    "llvm.x86.xop.vphaddbq",
    "llvm.x86.xop.vphaddbw",
    "llvm.x86.xop.vphadddq",
    "llvm.x86.xop.vphaddubd",
    "llvm.x86.xop.vphaddubq",
    "llvm.x86.xop.vphaddubw",
    "llvm.x86.xop.vphaddudq",
    "llvm.x86.xop.vphadduwd",
    "llvm.x86.xop.vphadduwq",
    "llvm.x86.xop.vphaddwd",
    "llvm.x86.xop.vphaddwq",
    "llvm.x86.xop.vphsubbw",
    "llvm.x86.xop.vphsubdq",
    "llvm.x86.xop.vphsubwd",
    "llvm.x86.xop.vpmacsdd",
    "llvm.x86.xop.vpmacsdqh",
    "llvm.x86.xop.vpmacsdql",
    "llvm.x86.xop.vpmacssdd",
    "llvm.x86.xop.vpmacssdqh",
    "llvm.x86.xop.vpmacssdql",
    "llvm.x86.xop.vpmacsswd",
    "llvm.x86.xop.vpmacssww",
    "llvm.x86.xop.vpmacswd",
    "llvm.x86.xop.vpmacsww",
    "llvm.x86.xop.vpmadcsswd",
    "llvm.x86.xop.vpmadcswd",
    "llvm.x86.xop.vpperm",
    "llvm.x86.xop.vprotb",
    "llvm.x86.xop.vprotbi",
    "llvm.x86.xop.vprotd",
    "llvm.x86.xop.vprotdi",
    "llvm.x86.xop.vprotq",
    "llvm.x86.xop.vprotqi",
    "llvm.x86.xop.vprotw",
    "llvm.x86.xop.vprotwi",
    "llvm.x86.xop.vpshab",
    "llvm.x86.xop.vpshad",
    "llvm.x86.xop.vpshaq",
    "llvm.x86.xop.vpshaw",
    "llvm.x86.xop.vpshlb",
    "llvm.x86.xop.vpshld",
    "llvm.x86.xop.vpshlq",
    "llvm.x86.xop.vpshlw",
    "llvm.x86.xrstor",
    "llvm.x86.xrstor64",
    "llvm.x86.xrstors",
    "llvm.x86.xrstors64",
    "llvm.x86.xsave",
    "llvm.x86.xsave64",
    "llvm.x86.xsavec",
    "llvm.x86.xsavec64",
    "llvm.x86.xsaveopt",
    "llvm.x86.xsaveopt64",
    "llvm.x86.xsaves",
    "llvm.x86.xsaves64",
    "llvm.x86.xsetbv",
    "llvm.x86.xtest",
    "llvm.xcore.bitrev",
    "llvm.xcore.checkevent",
    "llvm.xcore.chkct",
    "llvm.xcore.clre",
    "llvm.xcore.clrpt",
    "llvm.xcore.clrsr",
    "llvm.xcore.crc32",
    "llvm.xcore.crc8",
    "llvm.xcore.edu",
    "llvm.xcore.eeu",
    "llvm.xcore.endin",
    "llvm.xcore.freer",
    "llvm.xcore.geted",
    "llvm.xcore.getet",
    "llvm.xcore.getid",
    "llvm.xcore.getps",
    "llvm.xcore.getr",
    "llvm.xcore.getst",
    "llvm.xcore.getts",
    "llvm.xcore.in",
    "llvm.xcore.inct",
    "llvm.xcore.initcp",
    "llvm.xcore.initdp",
    "llvm.xcore.initlr",
    "llvm.xcore.initpc",
    "llvm.xcore.initsp",
    "llvm.xcore.inshr",
    "llvm.xcore.int",
    "llvm.xcore.mjoin",
    "llvm.xcore.msync",
    "llvm.xcore.out",
    "llvm.xcore.outct",
    "llvm.xcore.outshr",
    "llvm.xcore.outt",
    "llvm.xcore.peek",
    "llvm.xcore.setc",
    "llvm.xcore.setclk",
    "llvm.xcore.setd",
    "llvm.xcore.setev",
    "llvm.xcore.setps",
    "llvm.xcore.setpsc",
    "llvm.xcore.setpt",
    "llvm.xcore.setrdy",
    "llvm.xcore.setsr",
    "llvm.xcore.settw",
    "llvm.xcore.setv",
    "llvm.xcore.sext",
    "llvm.xcore.ssync",
    "llvm.xcore.syncr",
    "llvm.xcore.testct",
    "llvm.xcore.testwct",
    "llvm.xcore.waitevent",
    "llvm.xcore.zext",
];

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const IIT_TABLE: &[u32] = &[
  0x2e, 0x2e2e, (1<<31) | 1768, 0x10, 0x1f1f, 0x1f1f, 0x2f2f,
  0x2f2f, 0x2e2e0, (1<<31) | 4088, 0x32f, 0x2f3, 0x2f2f2f, (1<<31) | 4081, (1<<31) | 1044,
  0x2e0, 0x2e1, 0x12e1, 0x2e, (1<<31) | 1044, (1<<31) | 973, 0x2e2e1, 0x142e2e,
  0x2e0, (1<<31) | 1046, 0x1f, 0x22e2e, (1<<31) | 187, 0x2f2f, 0x11f1f, 0x1f1f,
  0x11f1f, (1<<31) | 4135, (1<<31) | 4135, (1<<31) | 4135, 0x0, 0x0, 0x42e, (1<<31) | 4085,
  (1<<31) | 4084, 0x2e40, 0x2e50, 0x40, 0x2e0, 0x2e0, 0x2e, 0x2e4,
  0x0, 0x2e4, 0x0, 0x2f2f, 0x2f2f, 0x1f1f1f, (1<<31) | 4120, (1<<31) | 4120,
  (1<<31) | 4120, (1<<31) | 4118, (1<<31) | 4118, (1<<31) | 4116, (1<<31) | 4118, (1<<31) | 4118, (1<<31) | 4118, (1<<31) | 4120,
  (1<<31) | 4120, (1<<31) | 4120, (1<<31) | 4120, (1<<31) | 4118, (1<<31) | 4127, (1<<31) | 4120, (1<<31) | 4120, (1<<31) | 4120,
  (1<<31) | 4140, (1<<31) | 2443, (1<<31) | 4077, (1<<31) | 4164, (1<<31) | 4144, (1<<31) | 4156, (1<<31) | 4148, (1<<31) | 4173,
  0xbf1f, 0xbf1f, (1<<31) | 4109, 0xbf2f, 0xbf2f, (1<<31) | 4109, 0xbf1f, 0xbf1f,
  0xbf1f, 0xbf1f, 0xbf1f, 0xbf1f, 0xbf1f, 0x2f2f, 0x2f2f, 0x4,
  0x2f2f2f2f, 0x2f2f2f2f, 0x42e, 0x2ee2e2e, 0x2e2ee0, 0x2ee2e2e0, 0x1f, 0x2e2e2e0,
  0x4452e0, 0x54452e0, 0x44552e0, (1<<31) | 3009, 0x4f4f, (1<<31) | 3010, 0x4f50, 0x4f50,
  0x1f2e2e, 0x2e, (1<<31) | 4141, 0x42e2e2e, 0x2f2f, 0x2f2f, 0x2f2f, 0x42e0,
  (1<<31) | 107, (1<<31) | 1229, (1<<31) | 1239, (1<<31) | 1251, (1<<31) | 116, (1<<31) | 127, 0x2f2f2f, (1<<31) | 158,
  (1<<31) | 3055, (1<<31) | 158, (1<<31) | 3055, 0x149f24f0, 0x49f24f0, 0x2f2f2f, 0x2f2f, 0x11cf1f,
  0x40, 0x2f2f2f, 0x42f2f, 0x4442e0, (1<<31) | 1778, (1<<31) | 4091, 0x5, 0x42e,
  0x2f2f, 0x2f2f, (1<<31) | 173, 0x2e4, 0x0, 0x42e0, 0x42e4, 0x2f2f,
  (1<<31) | 173, 0x2f2f, 0xf0f, (1<<31) | 173, 0x2e, 0x2ee2e0, 0x2e0, 0x2e,
  0x2e, 0x0, 0x2f2f, (1<<31) | 4100, (1<<31) | 4095, (1<<31) | 173, (1<<31) | 173, (1<<31) | 173,
  0x2e2e0, 0x2e0, 0x2e0, 0x42e2e2e0, (1<<31) | 182, 0x42e0, 0x0, 0x444,
  0x444, 0x444, 0x444, 0x544, 0x444, 0x444, 0x544, 0x2c2c2c,
  0x2c2c2c, 0x2c2c, 0x2c2c, 0x4a44a4a, 0x44, 0x4a44a4a, 0x4a44a4a, 0x4a4a4a4a,
  0x4a4a4a, 0x4a4a4a4a, 0x4a4a4a4a, 0x4a4a4a, 0x4a4a4a4a, 0x40, 0x40, 0x40,
  0x40, (1<<31) | 988, 0x4f5, (1<<31) | 988, 0x4f5, 0x1f1f, (1<<31) | 1306, 0x3f3f3f,
  0x3f3f, 0x3f3f3f, 0xafaf1f, 0xafaf1f, 0xbf2f, 0xaf1f, 0xaf1f, 0xaf1f,
  0xaf1f, 0xaf1f, 0xaf1f, 0xaf1f, 0xaf1f, 0xbf3f, 0xaf1f, 0xaf1f,
  0x3f3f3f, 0x2f2f2f, 0x3f3f3f, 0xbf2f, 0x3f3f3f, 0xbf2f, 0x3f3f3f, 0x2f2f2f,
  0x3f3f3f, 0xbf2f, 0x3f3f3f, 0xbf2f, 0x2f2f2f, 0x2f2f, 0x2f2f2f, 0x2f2f,
  0x2f2f, 0x2f2f, 0x2f2f2f, (1<<31) | 3963, (1<<31) | 3953, (1<<31) | 3941, (1<<31) | 3963, (1<<31) | 4042,
  (1<<31) | 3963, (1<<31) | 3953, (1<<31) | 4025, (1<<31) | 3953, (1<<31) | 3941, (1<<31) | 4004, (1<<31) | 3941, 0x3f3f3f,
  (1<<31) | 1318, 0x552c, (1<<31) | 1306, 0x3f3f, (1<<31) | 1325, (1<<31) | 1306, 0x3f3f3f, 0xbf3f,
  0xbf1f, 0xbf1f, 0x9f1f, 0x9f1f, 0x9f1f, 0x3f3f3f, (1<<31) | 1313, 0x3f3f3f,
  0x3f3f3f, 0x3f3f3f, 0xbf1f, 0x3f3f3f, 0x3f3f3f, 0xbf1f, (1<<31) | 1318, 0x1f1f,
  0x1f1f1f, 0x1f1f1f, (1<<31) | 1318, 0x445, 0x1f1f, 0x1f1f1f, 0x1f1f1f, (1<<31) | 1325,
  (1<<31) | 1325, 0x1f1f1f, 0x1f1f1f, (1<<31) | 1325, (1<<31) | 1325, 0x1f1f1f, (1<<31) | 191, (1<<31) | 191,
  0x3f3f3f, 0x1f1f1f, 0x1f1f1f, (1<<31) | 1970, 0xcf3f3f0, (1<<31) | 3919, (1<<31) | 3929, 0xcf3f3f0,
  (1<<31) | 3971, (1<<31) | 3919, (1<<31) | 3980, (1<<31) | 3929, (1<<31) | 3991, (1<<31) | 1306, 0x1f1f1f, 0x3f2c3f,
  0x3f2c2c3f, (1<<31) | 1279, (1<<31) | 1264, 0x3f2c3f3f, (1<<31) | 1290, (1<<31) | 1277, (1<<31) | 1262, 0x3f3f3f,
  0xbf3f, 0xbf1f, 0xbf1f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0xbf1f,
  0x3f3f3f, 0x3f3f3f, 0xbf1f, (1<<31) | 1318, 0x1f1f1f, 0x1f1f1f, (1<<31) | 1325, 0x1f1f1f,
  (1<<31) | 1325, 0x1f1f1f, (1<<31) | 191, 0x3f3f, 0x3f3f3f, 0x1f1f1f, 0x3f3f, 0x1f1f1f,
  (1<<31) | 1970, 0x1f1f1f, 0x53f5bf3f, 0x4af1f, 0x4af1f, 0x7a3a, 0x49f2f, 0x49f2f,
  0x3a7a, 0x43f3f3f, 0x43f3f3f, 0x1f1f1f, 0x2f2f2f, 0x87, 0x2e554, 0x4f54,
  0x2e554, 0x4f54, 0x1f1f1f, 0x4444, 0x4444, (1<<31) | 137, (1<<31) | 137, 0x55,
  0x1444a44, 0x1444a44, 0x1444a444, 0x1444a44, 0x1444a44, 0x1444a44, 0x1444a44, 0x1444a44,
  0x1444a44, 0x1444a44, 0x1444a44, 0x11444a2f, 0x11444a2f, (1<<31) | 77, (1<<31) | 77, 0x0,
  0x0, 0x0, 0x42f1, 0x2f2f, 0x7777, 0x7777, 0x7777, 0x7777,
  0x4439, 0x4439, 0x4474, 0x7739, 0x7739, 0x7769, 0x5, (1<<31) | 242,
  0x2f2f2f2f, (1<<31) | 97, (1<<31) | 87, 0x444, 0x444, 0x444, (1<<31) | 3114, 0x555,
  0x50, (1<<31) | 0, (1<<31) | 53, 0x42f2f5, 0x777, 0x2f2f2f2f, 0x777, 0x2f2f,
  0xaf1f, 0x2f2f, 0x4, 0x41f1f5, (1<<31) | 168, 0x515, (1<<31) | 43, (1<<31) | 43,
  (1<<31) | 42, (1<<31) | 43, (1<<31) | 43, (1<<31) | 43, (1<<31) | 43, (1<<31) | 43, (1<<31) | 43, (1<<31) | 43,
  (1<<31) | 43, (1<<31) | 43, (1<<31) | 43, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 30, (1<<31) | 30, (1<<31) | 30, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14,
  (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 14, (1<<31) | 29,
  (1<<31) | 29, (1<<31) | 242, (1<<31) | 242, 0x50, 0x440, 0x44447, 0x44477, 0x444777,
  (1<<31) | 242, 0x10, 0x42f2f, 0x4444, 0x2f2f, 0x51, 0x444, 0x444,
  0x14441f1f, 0x5455, 0x4a454a, 0x4444, 0x1, 0x5455, (1<<31) | 242, 0x2f2f,
  0x77, 0x44, 0x444, 0x2f2f, 0x2f2f, 0x77, 0x0, 0x0,
  0x0, 0x0, 0x0, 0x40, 0x5, 0x44, 0x40, 0x5,
  0x5, 0x440, 0x440, 0x40, 0x40, 0x4444, 0x4444, 0x4444,
  0x441f1f, 0x1f1f1f, 0x1f1f, 0x2f2f, (1<<31) | 64, (1<<31) | 63, 0x42f2f, 0x441f1f,
  0x0, (1<<31) | 147, 0x0, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0xf0f, 0x11, 0xf0f, 0x4444440, 0x4444440, 0x0, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x40, 0x40, 0x40,
  0x4, 0x40, 0x40, 0x4f4, (1<<31) | 982, 0x2e440, 0x2e440, 0x2e440,
  0x2e440, 0x4f4, (1<<31) | 982, 0x4444440, 0x4444440, 0x444440, 0x444440, 0x444444,
  0x444444, (1<<31) | 2070, (1<<31) | 2070, 0x2c2c2c, 0x2c2c2c, 0x2c2c, 0x2c2c, 0x4a44a4a,
  0x44, 0x4a44a4a, 0x4a44a4a, 0x4a4a4a4a, 0x4a4a4a, 0x4a4a4a4a, 0x4a4a4a4a, 0x4a4a4a,
  0x4a4a4a4a, 0x3f3f3f, 0x3f3f3f, 0x3f3f, 0xbfbf3f, 0xbfbf3f, 0x3f3f3f3f, 0x3f3f,
  0xbf3f, 0xbf3f, 0x4af1f, 0x4af1f, 0x7a3a, 0x49f2f, 0x49f2f, 0x3a7a,
  0xbf3f, 0xbf3f, 0xbf3f, 0xbf3f, 0xbf3f, 0xbf3f, 0x3f3f3f, 0x3f3f3f,
  0x3f3f3f, 0x3f3f3f, 0x4cf3f, (1<<31) | 2894, (1<<31) | 2046, (1<<31) | 2883, (1<<31) | 2028, (1<<31) | 2870,
  (1<<31) | 2006, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, (1<<31) | 1318,
  (1<<31) | 1318, (1<<31) | 1318, 0x3f3f3f, 0xbf3f3f, 0xbf3f3f, 0x3f3f3f, 0xbf3f, 0xbf3f,
  0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f,
  (1<<31) | 1318, (1<<31) | 1301, (1<<31) | 1301, (1<<31) | 1301, 0x3f3f, 0x3f3f3f, (1<<31) | 1306, (1<<31) | 1306,
  (1<<31) | 1306, 0x3f3f3f, 0x3f3f3f, (1<<31) | 1306, (1<<31) | 1306, (1<<31) | 1306, 0x3f3f3f, 0x3f3f3f,
  0x3f3f3f, 0x3f3f3f, 0x3f3f3f, (1<<31) | 1306, 0x3f3f, 0x3f3f3f, 0x3f3f3f, 0x3f3f3f,
  0x3f3f, 0x3f3f, 0x3f3f, 0x3f3f, 0x3f3f, 0x3f3f, (1<<31) | 1306, 0x3f3f3f,
  0x3f3f3f, 0x3f3f, 0x3f3f3f, (1<<31) | 1306, 0x3f3f3f3f, 0x3f3f3f, 0x3f3f3f, 0x4bf4f0,
  0x4bfbf4f0, (1<<31) | 2312, (1<<31) | 2774, (1<<31) | 2322, (1<<31) | 2785, (1<<31) | 2334, 0x2b2b2b, 0x2b2b2b2b,
  (1<<31) | 946, (1<<31) | 944, 0x2b2b2b2b, (1<<31) | 946, (1<<31) | 944, (1<<31) | 942, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x40, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x5445, 0x5445, 0x4444,
  0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x5445, 0x5445, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x2e440,
  0x2e440, 0x2e440, 0x2e440, 0x4f44, 0x2e444, 0x4f44, 0x2e444, 0x444,
  0x44, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x40, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x4444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x44, 0x2f7, 0x2f7, 0x52e5, 0x52e5, 0x52e5, 0x555,
  0x44, 0x55, 0x44, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x555, 0x555, 0x444, 0x545, 0x444, 0x444, 0x555,
  0x44, 0x44, 0x444, 0x444, 0x444, 0x444, 0x445, 0x445,
  0x444, 0x555, 0x444, 0x555, 0x444, 0x555, 0x444, 0x555,
  0x44, 0x55, 0x44, 0x44, 0x55, 0x444, 0x444, 0x555,
  0x54, 0x54, 0x44, 0x44, 0x44, 0x44, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x555, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x44,
  0x44, 0x44, 0x45, 0x44, 0x444, 0x444, 0x55, 0x45,
  0x44, 0x55, 0x55, 0x55, 0x55, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x554, 0x554, 0x554, 0x554, 0x554, 0x554, 0x554,
  0x554, 0x55, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x5555, 0x555, 0x5555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x444, 0x555, 0x44, 0x44, 0x444, 0x555, 0x445, 0x445,
  0x544, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x445, 0x445, 0x444,
  0x444, 0x444, 0x444, 0x555, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x454, 0x554, 0x454, 0x554,
  0x454, 0x454, 0x454, 0x454, 0x454, 0x454, 0x454, 0x454,
  0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x4555,
  0x554, 0x554, 0x554, 0x44, 0x444, 0x444, 0x44, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x554, 0x444, 0x444, 0x444,
  0x444, 0x554, 0x444, 0x444, 0x554, 0x444, 0x444, 0x45,
  0x4444, 0x4444, 0x4444, 0x4444, 0x44, 0x444, 0x444, 0x44,
  0x44, 0x44, 0x444, 0x5545, 0x444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x58, 0x57, 0x85, 0x85, 0x87, 0x85, 0x85, 0x84,
  0x84, 0x84, 0x84, 0x75, 0x75, 0x78, 0x75, 0x75,
  0x74, 0x74, 0x74, 0x74, 0x58, 0x57, 0x48, 0x47,
  0x48, 0x47, 0x484, 0x884, 0x884, 0x884, 0x884, 0x48,
  0x48, 0x777, 0x474, 0x774, 0x774, 0x774, 0x774, 0x777,
  0x777, 0x77, 0x7777, 0x7777, 0x47777, 0x7777, 0x7777, 0x47,
  0x47, 0x777, 0x777, 0x777, 0x777, 0x4e4, 0x5e5, 0x4444,
  0x4444, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x445,
  0x445, 0x444, 0x444, 0x444, 0x444, 0x445, 0x445, 0x445,
  0x445, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x444,
  0x445, 0x4455, 0x4455, 0x445, 0x444, 0x444, 0x444, 0x444,
  0x4444, 0x4444, 0x4444, 0x5555, 0x5555, 0x5555, 0x5555, 0x5555,
  0x5555, 0x5555, 0x5555, 0x5555, 0x5555, 0x5555, 0x5555, 0x5555,
  0x5555, 0x5555, 0x5555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x4455, 0x4455,
  0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x445, 0x445,
  0x445, 0x445, 0x445, 0x445, 0x445, 0x445, 0x4455, 0x4455,
  0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x445, 0x445,
  0x445, 0x445, 0x445, 0x445, 0x445, 0x445, 0x444, 0x444,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444, 0x444,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x444, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455,
  0x4455, 0x4455, 0x445, 0x445, 0x445, 0x445, 0x445, 0x445,
  0x445, 0x445, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455, 0x4455,
  0x4455, 0x4455, 0x444, 0x4444, 0x4444, 0x4444, 0x555, 0x555,
  0x5555, 0x5555, 0x555, 0x555, 0x555, 0x555, 0x5555, 0x5555,
  0x554, 0x554, 0x555, 0x555, 0x4455, 0x5555, 0x5555, 0x5555,
  0x4455, 0x4455, 0x4455, 0x4455, 0x555, 0x555, 0x445, 0x444,
  0x445, 0x444, 0x445, 0x445, 0x554, 0x554, 0x5555, 0x5555,
  0x5555, 0x5555, 0x555, 0x555, 0x555, 0x555, 0x4555, 0x455,
  0x454, 0x5555, 0x555, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444,
  0x454, 0x454, 0x454, 0x454, 0x4444, 0x4444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x445,
  0x4455, 0x445, 0x4455, 0x5555, 0x5555, 0x555, 0x555, 0x5555,
  0x5555, 0x555, 0x555, 0x4444, 0x4444, 0x4444, 0x5555, 0x5555,
  0x555, 0x4455, 0x4455, 0x445, 0x445, 0x5555, 0x5555, 0x555,
  0x555, 0x555, 0x555, 0x4444, 0x455, 0x4555, 0x4555, 0x4555,
  0x4555, 0x4555, 0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444,
  0x4444, 0x455, 0x455, 0x455, 0x4555, 0x4555, 0x4555, 0x4555,
  0x4555, 0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444, 0x455,
  0x455, 0x455, 0x4555, 0x4555, 0x4555, 0x4555, 0x455, 0x455,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444, 0x444, 0x454,
  0x455, 0x455, 0x455, 0x4555, 0x4555, 0x4555, 0x4555, 0x4555,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444, 0x454, 0x455,
  0x455, 0x44, 0x55, 0x4555, 0x44, 0x54, 0x44, 0x54,
  0x44, 0x44, 0x54, 0x444, 0x444, 0x44, 0x54, 0x44,
  0x54, 0x55, 0x4444, 0x544, 0x4455, 0x555, 0x44444, 0x5444,
  0x44555, 0x5555, 0x55, 0x555, 0x455, 0x4555, 0x4555, 0x4555,
  0x4555, 0x4555, 0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x455,
  0x455, 0x455, 0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x444,
  0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x455, 0x455, 0x455,
  0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x455, 0x455, 0x445, 0x554, 0x444, 0x444,
  0x555, 0x555, 0x555, 0x555, 0x44e4, 0x44, 0x44, 0x44444,
  0x44444, 0x44444, 0x44444, 0x444, 0x444, 0x444, 0x444, 0x4555,
  0x4555, 0x455, 0x455, 0x4555, 0x54, 0x54, 0x54, 0x55,
  0x54, 0x55, 0x54, 0x55, 0x54, 0x55, 0x44, 0x45,
  0x4555, 0x4555, 0x45, 0x45, 0x54, 0x555, 0x54, 0x555,
  0x45, 0x45, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444,
  0x454, 0x54, 0x4444, 0x544, 0x4455, 0x555, 0x444, 0x444,
  0x444, 0x4444, 0x4444, 0x4444, 0x4444, 0x4444, 0x444, 0x55e4,
  0x4444, 0x4444, 0x4444, 0x4455, 0x44555, 0x555, 0x555, 0x555,
  0x555, 0x555, 0x555, 0x454, 0x454, 0x54, 0x455, 0x455,
  0x4555, 0x4555, 0x4555, 0x4555, 0x4555, 0x444, 0x4444, 0x4444,
  0x4444, 0x4444, 0x4444, 0x45, 0x555, 0x555, 0x44, 0x44c4,
  0x44d4, 0x4d4c, (1<<31) | 3039, 0x4d4c, (1<<31) | 3039, 0x44c, 0x44d, 0x44c,
  0x44d, 0x44c, 0x44d, (1<<31) | 196, (1<<31) | 215, (1<<31) | 196, (1<<31) | 215, (1<<31) | 198,
  (1<<31) | 217, (1<<31) | 196, (1<<31) | 215, (1<<31) | 196, (1<<31) | 215, (1<<31) | 1335, (1<<31) | 1343, (1<<31) | 1335,
  (1<<31) | 1343, (1<<31) | 196, (1<<31) | 215, (1<<31) | 196, (1<<31) | 215, (1<<31) | 196, (1<<31) | 215, (1<<31) | 2805,
  (1<<31) | 2910, (1<<31) | 2805, (1<<31) | 2910, (1<<31) | 2805, (1<<31) | 2910, (1<<31) | 2805, (1<<31) | 2910, 0x4c4c,
  0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c, 0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c,
  0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, (1<<31) | 2832,
  (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, (1<<31) | 203,
  (1<<31) | 222, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, (1<<31) | 2832, (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d,
  (1<<31) | 2975, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4c,
  0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, (1<<31) | 2832,
  (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x44c4c4c,
  0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c, 0x4d4d4d, (1<<31) | 1333, (1<<31) | 1341, (1<<31) | 1331,
  (1<<31) | 1339, (1<<31) | 1333, (1<<31) | 1341, (1<<31) | 1331, (1<<31) | 1339, (1<<31) | 2798, (1<<31) | 2903, (1<<31) | 2798,
  (1<<31) | 2903, (1<<31) | 2350, (1<<31) | 2388, (1<<31) | 2348, (1<<31) | 2386, 0x44c4c, 0x44d4d, 0x44c4c4c,
  0x44d4d4d, 0x4c4c4c, 0x4d4d4d, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c,
  0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c, 0x4d4d4d, 0x44c4c4c,
  0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c,
  0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c,
  0x4d4d, 0x4d4d, (1<<31) | 3046, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c, 0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c4d,
  (1<<31) | 2977, 0x4c, 0x4d, 0x4d, (1<<31) | 3028, 0x4c4c, 0x4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c, 0x4d4d, 0x44c4c4d, (1<<31) | 2406, 0x4c4c4c, 0x4d4d4d, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x44d4c,
  (1<<31) | 2424, 0x44d4c4c, (1<<31) | 2422, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44d4c,
  (1<<31) | 2424, 0x44d4c4c, (1<<31) | 2422, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, (1<<31) | 2825,
  (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825,
  (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825,
  (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, 0x4c442e0,
  0x4d442e0, (1<<31) | 2813, (1<<31) | 2928, 0x4d442e0, (1<<31) | 3031, (1<<31) | 2918, (1<<31) | 3021, 0x4c442e0,
  0x4d442e0, (1<<31) | 2813, (1<<31) | 2928, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2825, (1<<31) | 2940, (1<<31) | 2823, (1<<31) | 2938, (1<<31) | 2823,
  (1<<31) | 2938, (1<<31) | 2823, (1<<31) | 2938, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c,
  0x44d4d4d, 0x44c4c, 0x44d4d, 0x44c4c, 0x44d4d, 0x4c4c4c, 0x4d4d4d, 0x44c4c,
  0x44d4d, 0x4c4c4c, 0x4d4d4d, 0x54c4c, 0x54d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c4c,
  0x44d4d4d, (1<<31) | 2366, (1<<31) | 2394, (1<<31) | 2366, (1<<31) | 2394, 0x44c4c4c, 0x44d4d4d, 0x44c4c4d,
  (1<<31) | 2406, 0x44c4c4d, (1<<31) | 2406, (1<<31) | 2376, (1<<31) | 2404, (1<<31) | 2376, (1<<31) | 2404, 0x44c4c4d,
  (1<<31) | 2406, (1<<31) | 2805, (1<<31) | 2910, (1<<31) | 2805, (1<<31) | 2910, (1<<31) | 2805, (1<<31) | 2910, (1<<31) | 2805,
  (1<<31) | 2910, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x44d4d, (1<<31) | 2432, 0x44d4d4d,
  (1<<31) | 2430, 0x4d4d4d, (1<<31) | 3044, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x4d4d4d,
  (1<<31) | 3044, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x54c4c4c, 0x54d4d4d, 0x44d4d,
  (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x54c4c4c, 0x54d4d4d, 0x54c4c4c, 0x54d4d4d, 0x44c4d,
  (1<<31) | 2416, 0x44c4d4d, (1<<31) | 2414, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4d,
  (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4c, 0x4d4d4d, 0x4c4c4d, (1<<31) | 2977, 0x44c4d,
  (1<<31) | 2416, 0x44c4d4d, (1<<31) | 2414, 0x44c4d4d, (1<<31) | 2414, 0x44c4c, 0x44d4d, 0x44c4c,
  0x44d4d, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d,
  (1<<31) | 2975, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4c, 0x4d4d4d, 0x44c4c, 0x44d4d, 0x44c4c4c,
  0x44d4d4d, 0x44c4c, 0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x44c4c, 0x44d4d, 0x44c4c4c,
  0x44d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4d4d, (1<<31) | 2975, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c4c,
  0x4d4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x44c4d, (1<<31) | 2416, 0x44c4d4d, (1<<31) | 2414, 0x4c4c4d,
  (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, 0x44c4d, (1<<31) | 2416, 0x44c4d4d, (1<<31) | 2414, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d4d, (1<<31) | 2975, (1<<31) | 2832,
  (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c, 0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c, 0x4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c, 0x4d4d, (1<<31) | 210, (1<<31) | 229, (1<<31) | 210, (1<<31) | 229, (1<<31) | 210,
  (1<<31) | 229, 0x4c4c4c, 0x4d4d4d, 0x54c4d, (1<<31) | 3108, 0x54c4d4d, (1<<31) | 3106, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x444d4d, (1<<31) | 2155, 0x444d4d4d, (1<<31) | 2153, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x44c4c,
  0x44d4d, 0x44c4c4c, 0x44d4d4d, 0x54c4d, (1<<31) | 3108, 0x54c4d4d, (1<<31) | 3106, 0x444d4d,
  (1<<31) | 2155, 0x444d4d4d, (1<<31) | 2153, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c4c, 0x4d4d4d4d, 0x44c4c,
  0x44d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x444d4d, (1<<31) | 2155, 0x444d4d4d,
  (1<<31) | 2153, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4d,
  (1<<31) | 2987, 0x4c4c440, 0x4d4d440, 0x4c4c440, 0x4d4d440, (1<<31) | 2850, (1<<31) | 2965, 0x4c4d440,
  (1<<31) | 2984, 0x4c4d440, (1<<31) | 2984, (1<<31) | 2860, (1<<31) | 2992, 0x4c4c440, 0x4d4d440, 0x4c4c440,
  0x4d4d440, (1<<31) | 2850, (1<<31) | 2965, 0x4c4d, (1<<31) | 2987, 0x4c4c4c, 0x4d4d4d, 0x4c4c,
  0x4d4d, 0x4c4c4c, 0x4d4d4d, 0x4c4c, 0x4d4d, 0x4c4c4c, 0x4d4d4d, 0x44c4c4d,
  (1<<31) | 2406, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c,
  0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, (1<<31) | 2832, (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c,
  0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, (1<<31) | 203, (1<<31) | 222, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, (1<<31) | 2832, (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4d, (1<<31) | 2977, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, 0x4c4c4c, 0x4d4d4d, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x4c4c4d,
  (1<<31) | 2977, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d, (1<<31) | 3044, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, (1<<31) | 2832, (1<<31) | 2947, (1<<31) | 2832, (1<<31) | 2947, 0x4c4c4c, 0x4d4d4d, 0x4d4d4d,
  (1<<31) | 3044, (1<<31) | 2841, (1<<31) | 2956, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x44d4d,
  (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x44d4d, (1<<31) | 2432, 0x44d4d4d, (1<<31) | 2430, 0x4c4d,
  (1<<31) | 2987, 0x4c4d, (1<<31) | 2987, 0x4c4d4d, (1<<31) | 3002, 0x4c4d4d, (1<<31) | 3002, 0x4c4d,
  (1<<31) | 2987, 0x4c4d, (1<<31) | 2987, 0x4c4c4c, 0x4d4d4d, 0x4c4d, (1<<31) | 2987, 0x4c4d,
  (1<<31) | 2987, 0x2e0, 0x2e0, 0x2e0, 0x2e0, 0x42e0, 0x52e0, 0x42e2e2e,
  0x42e2e2e, 0x42e2e2e, 0x42e2e2e, 0x42e2e2e, 0x42e2e2e, 0x442e2e, 0x452e2e, 0x442e2e,
  0x442e2e, 0x442e2e, 0x442e2e2e, 0x442e2e2e, 0x442e2e2e, 0x442e2e2e, 0x442e2e2e, 0x442e2e2e,
  0x4442e2e, 0x4452e2e, 0x4442e2e, 0x4442e2e, 0x4442e2e, 0x4b4b4b, 0x2e0, 0x3939,
  0x2a2a, 0x44, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x393939, 0x393939,
  0x444, 0x393939, 0x393939, 0x444, 0x444, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x444, 0x393939, 0x2a2a2a, 0x393939, 0x2a2a2a, 0x2a2a2a, 0x2a2a2a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a,
  0x444, 0x2c2c2c, 0x42c2c, 0x4444, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x4444, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x2c2c2c2c, 0x59595959, 0x3b3b3b3b,
  0x4a4a4a4a, 0x42c2c2c, 0x4595959, 0x43b3b3b, 0x44a4a4a, 0x2c2c2c2c, 0x59595959, 0x3b3b3b3b,
  0x4a4a4a4a, 0x42c2c2c, 0x4595959, 0x43b3b3b, 0x44a4a4a, 0x44, 0x2c2c2c2c, 0x42c2c2c,
  0x2c2c2c2c, 0x42c2c2c, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c4, 0x594, 0x3b4, 0x2c4, 0x4a4, 0x4,
  0x2c2c2c2c, 0x42c2c2c, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c4, 0x594, 0x3b4, 0x2c4, 0x4a4, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x44,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a,
  0x39390, 0x39390, 0x39390, 0x2a2a4, 0x2a2a4, 0x2a2a4, 0x2a2a4, 0x2a2a4,
  0x2a2a4, 0x2a2a0, 0x2a2a0, 0x2a2a0, 0x42c4, 0x4595, 0x43b4, 0x44a4,
  0x42c4, 0x4595, 0x43b4, 0x44a4, 0x440, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x4555, 0x4a4a59, 0x2c2c3b,
  0x3b3b4a, 0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x393955, 0x4a4a5959, 0x2c2c3b3b, 0x3b3b4a4a,
  0x4a4a5959, 0x2c2c3b3b, 0x3b3b4a4a, 0x393955, 0x4455, 0x393955, 0x393955, 0x2a2a55,
  0x2a2a55, 0x393955, 0x393955, 0x393955, 0x4455, 0x393955, 0x393955, 0x2a2a55,
  0x2a2a55, 0x4a4a5959, 0x2c2c3b3b, 0x3b3b4a4a, 0x4a4a5959, 0x2c2c3b3b, 0x3b3b4a4a, 0x393955,
  0x454, 0x454, 0x454, 0x454, 0x454, 0x454, 0x898989, 0x7a7a7a,
  0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x8959, 0x7a4a, 0x898959, 0x7a7a4a,
  0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a,
  0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a,
  0x898989, 0x7a7a7a, 0x7a7a6b, 0x89897a, 0x598989, 0x4a7a7a, 0x7a89, 0x6b7a,
  0x7a89, 0x6b7a, 0x5989, 0x4a7a, 0x5989, 0x4a7a, 0x4a89, 0x3b7a,
  0x4a89, 0x3b7a, 0x42c, 0x559, 0x43b, 0x44a, 0x8989, 0x7a7a,
  (1<<31) | 3897, 0x7a7a7a7a, 0x898989, 0x7a7a7a, 0x898989, 0x7a7a7a, 0x898989, 0x7a7a7a,
  0x898989, 0x7a7a7a, (1<<31) | 3897, 0x7a7a7a7a, 0x898989, 0x7a7a7a, 0x8989, 0x7a7a,
  0x8989, 0x7a7a, 0x8989, 0x7a7a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a,
  0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a,
  0x8989, 0x7a7a, 0x898989, 0x7a7a7a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a,
  0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x898959, 0x7a7a4a, 0x8959, 0x7a4a,
  0x8959, 0x7a4a, 0x7a7a3b, 0x89894a, 0x8959, 0x7a4a, 0x8959, 0x7a4a,
  0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x4a4a59, 0x2c2c3b,
  0x3b3b4a, 0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x442c2c, 0x545959, 0x443b3b, 0x444a4a,
  0x444, 0x2c42c2c, 0x5945959, 0x3b43b3b, 0x4a44a4a, 0x42e4, 0x42e2c, 0x42e59,
  0x42e3b, 0x42e4a, 0x42c, 0x459, 0x43b, 0x44a, 0x42e4, 0x4444,
  0x42e4, 0x4455, 0x3b3b3b3b, 0x4a4a4a4a, 0x3b3b3b3b, 0x4a4a4a4a, 0x4455, 0x2c2c2c2c,
  0x59595959, 0x3b3b3b3b, 0x4a4a4a4a, 0x393955, 0x393955, 0x393955, 0x393955, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x42c2c,
  0x45959, 0x43b3b, 0x44a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c,
  0x45959, 0x43b3b, 0x44a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x444,
  0x2c2c, 0x4455, 0x3b3b3b3b, 0x4a4a4a4a, 0x3b3b3b3b, 0x4a4a4a4a, 0x4455, 0x2c2c2c2c,
  0x59595959, 0x3b3b3b3b, 0x4a4a4a4a, 0x455, 0x393939, 0x3b3b3b, 0x4a4a4a, 0x393939,
  0x39394, 0x39394, 0x392a39, 0x392a39, 0x393939, 0x444, 0x393939, 0x444,
  0x3b3b3b, 0x4a4a4a, 0x393955, 0x393955, 0x445, 0x445, 0x2c2c2c, 0x595959,
  0x3b3b3b, 0x4a4a4a, 0x2c2c, 0x5959, 0x3b3b, 0x4a4a, 0x2c2c, 0x5959,
  0x3b3b, 0x4a4a, 0x2c2c2c, 0x42c2c, 0x2c2c2c, 0x42c2c, 0x393939, 0x2c2c2c,
  0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c,
  0x5959, 0x3b3b, 0x4a4a, 0x393939, 0x2a2a2a, 0x394, 0x394, 0x2a39,
  0x2a39, 0x2a39, 0x2a39, 0x2a39, 0x2a39, 0x2a39, 0x2a39, 0x39392a,
  0x44439, 0x44439, 0x4439, 0x39392a, 0x4439, 0x39392a, 0x4444, 0x2a4,
  0x44, 0x439, 0x42a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x42c2c,
  0x45959, 0x43b3b, 0x44a4a, 0x42c2c, 0x43b3b, 0x44a4a, 0x455, 0x43939,
  0x42a2a, 0x43939, 0x444, 0x43939, 0x42a2a, 0x43939, 0x42a2a, 0x444,
  0x43939, 0x42a2a, 0x42c2c2c, 0x4595959, 0x43b3b3b, 0x44a4a4a, 0x42c2c2c, 0x4595959,
  0x43b3b3b, 0x44a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x42c2c, 0x45959, 0x43b3b, 0x44a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c, 0x45959,
  0x43b3b, 0x44a4a, 0x42e2c0, 0x42e590, 0x42e3b0, 0x42e4a0, 0x393939, 0x393939,
  0x444, 0x393939, 0x393939, 0x444, 0x444, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x393939, 0x2a2a2a, 0x393939,
  0x2a2a2a, 0x2a2a2a, 0x2a2a2a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x42c2c,
  0x45959, 0x43b3b, 0x44a4a, 0x2c2c2c2c, 0x59595959, 0x3b3b3b3b, 0x4a4a4a4a, 0x440,
  0x2c2c2c, 0x42c2c, 0x888, 0x777, 0x777, 0x888, 0x777, 0x777,
  0x888, 0x777, 0x777, 0x888, 0x777, 0x777, 0x2fcf2f, 0x2fcf2f,
  0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1f1fcf1f, 0x1f1fcf1f, 0x1fcf1f, 0x1fcf1f,
  0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x74f7, 0x84f8, 0x44f4, 0x44f4,
  0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f, 0x1fcf1f,
  0x40, 0x40, 0x440, 0x40, 0x40, 0x440, 0x0, 0x44,
  0x44, 0x44, 0x85, 0x74, 0x47, 0x58, 0x88, 0x77,
  0x77, 0x4f0, 0x4f0, 0x77, 0x77, 0x87, 0x87, 0x87,
  0x87, 0x87, 0x87, 0x87, 0x87, 0x84, 0x84, 0x84,
  0x84, 0x84, 0x84, 0x85, 0x85, 0x85, 0x85, 0x84,
  0x84, 0x84, 0x84, 0x85, 0x85, 0x85, 0x85, 0x777,
  0x777, 0x888, 0x777, 0x777, 0x888, 0x777, 0x777, 0x888,
  0x777, 0x777, 0x888, 0x777, 0x777, 0x88, 0x77, 0x77,
  0x73, 0x73, 0x74, 0x74, 0x74, 0x74, 0x74, 0x74,
  0x74, 0x74, 0x75, 0x75, 0x75, 0x75, 0x75, 0x75,
  0x75, 0x75, 0x74, 0x74, 0x74, 0x74, 0x74, 0x74,
  0x74, 0x74, 0x75, 0x75, 0x75, 0x75, 0x75, 0x75,
  0x75, 0x75, 0x88, 0x77, 0x77, 0x88, 0x77, 0x77,
  0x8888, 0x7777, 0x7777, 0x8888, 0x7777, 0x7777, 0x8888, 0x7777,
  0x7777, 0x8888, 0x7777, 0x7777, 0x888, 0x777, 0x777, 0x888,
  0x777, 0x777, 0x4444, 0x48, 0x48, 0x48, 0x48, 0x47,
  0x47, 0x47, 0x47, 0x2e1, 0x2e1, 0x2e1, 0x2e1, 0x51,
  0x51, 0x51, 0x4cf2f, 0x4cf1f, 0x4cf4f, 0x4cf2f, 0x4cf1f, 0x4cf4f,
  0x88, 0x77, 0x77, 0x58, 0x58, 0x58, 0x58, 0x57,
  0x57, 0x57, 0x57, 0x448, (1<<31) | 1976, (1<<31) | 3100, 0x444, 0x545,
  0x0, 0x0, 0x0, 0x88, 0x77, 0x33, 0x44, 0x55,
  0xcf4f, 0x888, 0x777, 0x777, 0x888, 0x777, 0x777, 0x888,
  0x777, 0x777, 0x888, 0x777, 0x777, 0x444, 0x444, 0x444,
  0x555, 0x444, 0x555, 0x4444, 0xcf4f, 0xcf4f, 0xcf4f, 0xcf4f,
  0xcf4f, 0xcf4f, 0xcf4f, 0xcf4f, 0xcf4f, 0x88, 0x88, 0x77,
  0x77, 0x88, 0x77, 0x77, 0x88, 0x77, 0x77, 0x88,
  0x77, 0x77, 0x4, 0x5, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4f4, 0x444, 0x455, 0x455,
  0x88, 0x77, 0x77, 0x88, 0x77, 0x77, 0x4444, 0x4444,
  0x88, 0x77, 0x77, 0x4477, 0x4444, 0x4477, 0x4444, 0x4477,
  0x4444, 0x44747, 0x44444, 0x44747, 0x44444, 0x44747, 0x44444, 0x44747,
  0x44444, 0x4477, 0x4444, 0x77, 0x77, 0x77, 0x77, 0x77,
  0x88, 0x77, 0x77, 0x88, 0x77, 0x77, 0x88, 0x77,
  0x77, 0x88, 0x77, 0x77, 0x4453, 0x4453, 0x4453, 0x4454,
  0x4454, 0x4454, 0x4455, 0x4455, 0x4455, 0x4453, 0x4453, 0x4453,
  (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2187, (1<<31) | 2187, (1<<31) | 2187, (1<<31) | 2204, (1<<31) | 2204,
  (1<<31) | 2204, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2162, (1<<31) | 2162, (1<<31) | 2162, (1<<31) | 2178,
  (1<<31) | 2178, (1<<31) | 2178, (1<<31) | 2162, (1<<31) | 2162, (1<<31) | 2162, 0x453, 0x453, 0x453,
  0x454, 0x454, 0x454, 0x455, 0x455, 0x455, 0x453, 0x453,
  0x453, (1<<31) | 2461, (1<<31) | 2461, (1<<31) | 2461, (1<<31) | 2475, (1<<31) | 2475, (1<<31) | 2475, (1<<31) | 2490,
  (1<<31) | 2490, (1<<31) | 2490, (1<<31) | 2461, (1<<31) | 2461, (1<<31) | 2461, (1<<31) | 2453, (1<<31) | 2453, (1<<31) | 2453,
  (1<<31) | 2467, (1<<31) | 2467, (1<<31) | 2467, (1<<31) | 2453, (1<<31) | 2453, (1<<31) | 2453, 0x44453, 0x44453,
  0x44453, 0x44454, 0x44454, 0x44454, 0x44455, 0x44455, 0x44455, 0x44453,
  0x44453, 0x44453, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2105, (1<<31) | 2105, (1<<31) | 2105,
  (1<<31) | 2124, (1<<31) | 2124, (1<<31) | 2124, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2077, (1<<31) | 2077,
  (1<<31) | 2077, (1<<31) | 2095, (1<<31) | 2095, (1<<31) | 2095, (1<<31) | 2077, (1<<31) | 2077, (1<<31) | 2077, 0x4453,
  0x4453, 0x4453, 0x4454, 0x4454, 0x4454, 0x4455, 0x4455, 0x4455,
  0x4453, 0x4453, 0x4453, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2187, (1<<31) | 2187,
  (1<<31) | 2187, (1<<31) | 2204, (1<<31) | 2204, (1<<31) | 2204, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2171, (1<<31) | 2162,
  (1<<31) | 2162, (1<<31) | 2162, (1<<31) | 2178, (1<<31) | 2178, (1<<31) | 2178, (1<<31) | 2162, (1<<31) | 2162, (1<<31) | 2162,
  0x44453, 0x44453, 0x44453, 0x44454, 0x44454, 0x44454, 0x44455, 0x44455,
  0x44455, 0x44453, 0x44453, 0x44453, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2105,
  (1<<31) | 2105, (1<<31) | 2105, (1<<31) | 2124, (1<<31) | 2124, (1<<31) | 2124, (1<<31) | 2087, (1<<31) | 2087, (1<<31) | 2087,
  (1<<31) | 2077, (1<<31) | 2077, (1<<31) | 2077, (1<<31) | 2095, (1<<31) | 2095, (1<<31) | 2095, (1<<31) | 2077, (1<<31) | 2077,
  (1<<31) | 2077, 0x54, 0x54, 0x54, 0x54, 0x54, 0x54, 0x34450,
  0x34450, 0x34450, 0x44450, 0x44450, 0x44450, 0x54450, 0x54450, 0x54450,
  0x34450, 0x34450, 0x34450, 0x334450, 0x334450, 0x334450, 0x444450, 0x444450,
  0x444450, 0x554450, 0x554450, 0x554450, 0x334450, 0x334450, 0x334450, 0x33334450,
  0x33334450, 0x33334450, 0x44444450, 0x44444450, 0x44444450, 0x33334450, 0x33334450, 0x33334450,
  0x3450, 0x3450, 0x3450, 0x4450, 0x4450, 0x4450, 0x5450, 0x5450,
  0x5450, 0x3450, 0x3450, 0x3450, 0x33450, 0x33450, 0x33450, 0x44450,
  0x44450, 0x44450, 0x55450, 0x55450, 0x55450, 0x33450, 0x33450, 0x33450,
  0x3333450, 0x3333450, 0x3333450, 0x4444450, 0x4444450, 0x4444450, 0x3333450, 0x3333450,
  0x3333450, 0x344450, 0x344450, 0x344450, 0x444450, 0x444450, 0x444450, 0x544450,
  0x544450, 0x544450, 0x344450, 0x344450, 0x344450, 0x3344450, 0x3344450, 0x3344450,
  0x4444450, 0x4444450, 0x4444450, 0x5544450, 0x5544450, 0x5544450, 0x3344450, 0x3344450,
  0x3344450, (1<<31) | 1083, (1<<31) | 1083, (1<<31) | 1083, (1<<31) | 2060, (1<<31) | 2060, (1<<31) | 2060, (1<<31) | 1083,
  (1<<31) | 1083, (1<<31) | 1083, 0x34450, 0x34450, 0x34450, 0x44450, 0x44450, 0x44450,
  0x54450, 0x54450, 0x54450, 0x34450, 0x34450, 0x34450, 0x334450, 0x334450,
  0x334450, 0x444450, 0x444450, 0x444450, 0x554450, 0x554450, 0x554450, 0x334450,
  0x334450, 0x334450, 0x33334450, 0x33334450, 0x33334450, 0x44444450, 0x44444450, 0x44444450,
  0x33334450, 0x33334450, 0x33334450, 0x344450, 0x344450, 0x344450, 0x444450, 0x444450,
  0x444450, 0x544450, 0x544450, 0x544450, 0x344450, 0x344450, 0x344450, 0x3344450,
  0x3344450, 0x3344450, 0x4444450, 0x4444450, 0x4444450, 0x5544450, 0x5544450, 0x5544450,
  0x3344450, 0x3344450, 0x3344450, (1<<31) | 1083, (1<<31) | 1083, (1<<31) | 1083, (1<<31) | 2060, (1<<31) | 2060,
  (1<<31) | 2060, (1<<31) | 1083, (1<<31) | 1083, (1<<31) | 1083, 0x34450, 0x44450, 0x34450, 0x334450,
  0x444450, 0x334450, 0x33334450, 0x44444450, 0x33334450, 0x3450, 0x4450, 0x3450,
  0x33450, 0x44450, 0x33450, 0x3333450, 0x4444450, 0x3333450, 0x344450, 0x444450,
  0x344450, 0x3344450, 0x4444450, 0x3344450, (1<<31) | 1083, (1<<31) | 2060, (1<<31) | 1083, 0x34450,
  0x44450, 0x34450, 0x334450, 0x444450, 0x334450, 0x33334450, 0x44444450, 0x33334450,
  0x344450, 0x444450, 0x344450, 0x3344450, 0x4444450, 0x3344450, (1<<31) | 1083, (1<<31) | 2060,
  (1<<31) | 1083, 0x55, (1<<31) | 3424, (1<<31) | 3412, (1<<31) | 3412, (1<<31) | 3342, (1<<31) | 3331, (1<<31) | 3331,
  (1<<31) | 3268, (1<<31) | 2211, (1<<31) | 3258, (1<<31) | 2194, (1<<31) | 3258, (1<<31) | 2194, (1<<31) | 3468, (1<<31) | 3457,
  (1<<31) | 3457, (1<<31) | 3382, (1<<31) | 3372, (1<<31) | 3372, (1<<31) | 3304, (1<<31) | 2496, (1<<31) | 3295, (1<<31) | 2481,
  (1<<31) | 3295, (1<<31) | 2481, (1<<31) | 3614, (1<<31) | 3599, (1<<31) | 3599, (1<<31) | 3424, (1<<31) | 3412, (1<<31) | 3412,
  (1<<31) | 3342, (1<<31) | 2132, (1<<31) | 3331, (1<<31) | 2113, (1<<31) | 3331, (1<<31) | 2113, (1<<31) | 3670, (1<<31) | 3656,
  (1<<31) | 3656, (1<<31) | 3468, (1<<31) | 3457, (1<<31) | 3457, (1<<31) | 3382, (1<<31) | 2211, (1<<31) | 3372, (1<<31) | 2194,
  (1<<31) | 3372, (1<<31) | 2194, (1<<31) | 3842, (1<<31) | 3825, (1<<31) | 3825, (1<<31) | 3562, (1<<31) | 3550, (1<<31) | 3550,
  (1<<31) | 3468, (1<<31) | 2132, (1<<31) | 3457, (1<<31) | 2113, (1<<31) | 3457, (1<<31) | 2113, (1<<31) | 3514, (1<<31) | 3501,
  (1<<31) | 3501, (1<<31) | 3424, (1<<31) | 3412, (1<<31) | 3412, (1<<31) | 3562, (1<<31) | 3550, (1<<31) | 3550, (1<<31) | 3468,
  (1<<31) | 3457, (1<<31) | 3457, (1<<31) | 3436, (1<<31) | 3401, (1<<31) | 3401, (1<<31) | 3353, (1<<31) | 3321, (1<<31) | 3321,
  (1<<31) | 3278, (1<<31) | 2221, (1<<31) | 3249, (1<<31) | 2178, (1<<31) | 3249, (1<<31) | 2178, (1<<31) | 3479, (1<<31) | 3447,
  (1<<31) | 3447, (1<<31) | 3392, (1<<31) | 3363, (1<<31) | 3363, (1<<31) | 3313, (1<<31) | 2505, (1<<31) | 3287, (1<<31) | 2467,
  (1<<31) | 3287, (1<<31) | 2467, (1<<31) | 3629, (1<<31) | 3585, (1<<31) | 3585, (1<<31) | 3436, (1<<31) | 3401, (1<<31) | 3401,
  (1<<31) | 3353, (1<<31) | 2143, (1<<31) | 3321, (1<<31) | 2095, (1<<31) | 3321, (1<<31) | 2095, (1<<31) | 3684, (1<<31) | 3643,
  (1<<31) | 3643, (1<<31) | 3479, (1<<31) | 3447, (1<<31) | 3447, (1<<31) | 3392, (1<<31) | 2221, (1<<31) | 3363, (1<<31) | 2178,
  (1<<31) | 3363, (1<<31) | 2178, (1<<31) | 3859, (1<<31) | 3809, (1<<31) | 3809, (1<<31) | 3574, (1<<31) | 3539, (1<<31) | 3539,
  (1<<31) | 3479, (1<<31) | 2143, (1<<31) | 3447, (1<<31) | 2095, (1<<31) | 3447, (1<<31) | 2095, (1<<31) | 3527, (1<<31) | 3489,
  (1<<31) | 3489, (1<<31) | 3436, (1<<31) | 3401, (1<<31) | 3401, (1<<31) | 3574, (1<<31) | 3539, (1<<31) | 3539, (1<<31) | 3479,
  (1<<31) | 3447, (1<<31) | 3447, (1<<31) | 3016, 0x4f5, (1<<31) | 3382, (1<<31) | 3372, (1<<31) | 3372, (1<<31) | 3382,
  (1<<31) | 3372, (1<<31) | 3372, (1<<31) | 3382, (1<<31) | 3372, (1<<31) | 3372, (1<<31) | 3382, (1<<31) | 3372, (1<<31) | 3372,
  (1<<31) | 3392, (1<<31) | 3363, (1<<31) | 3363, (1<<31) | 3392, (1<<31) | 3363, (1<<31) | 3363, (1<<31) | 3392, (1<<31) | 3363,
  (1<<31) | 3363, (1<<31) | 3392, (1<<31) | 3363, (1<<31) | 3363, 0x88, 0x77, 0x77, 0x54,
  0x54, 0x54, 0x54, 0x54, 0x54, 0x54, 0x54, 0x48,
  0x48, 0x48, 0x48, 0x47, 0x47, 0x47, 0x47, 0x58,
  0x58, 0x58, 0x58, 0x57, 0x57, 0x57, 0x57, 0x11,
  0x141, 0x11, 0x141, 0x14, 0x144, 0x11, 0x141, (1<<31) | 994,
  (1<<31) | 994, (1<<31) | 1788, (1<<31) | 994, (1<<31) | 1788, (1<<31) | 1788, (1<<31) | 994, (1<<31) | 994, (1<<31) | 1788,
  (1<<31) | 994, (1<<31) | 1788, (1<<31) | 1788, (1<<31) | 994, (1<<31) | 994, (1<<31) | 1788, (1<<31) | 994, (1<<31) | 1788,
  (1<<31) | 1788, (1<<31) | 994, (1<<31) | 994, (1<<31) | 1788, (1<<31) | 994, (1<<31) | 1788, (1<<31) | 1788, (1<<31) | 1014,
  (1<<31) | 1026, (1<<31) | 1014, (1<<31) | 1026, (1<<31) | 1809, (1<<31) | 1822, (1<<31) | 1014, (1<<31) | 1026, (1<<31) | 1809,
  (1<<31) | 1822, (1<<31) | 1809, (1<<31) | 1822, (1<<31) | 1014, (1<<31) | 1026, (1<<31) | 1014, (1<<31) | 1026, (1<<31) | 1809,
  (1<<31) | 1822, (1<<31) | 1014, (1<<31) | 1026, (1<<31) | 1809, (1<<31) | 1822, (1<<31) | 1809, (1<<31) | 1822, (1<<31) | 3149,
  (1<<31) | 3149, (1<<31) | 3709, (1<<31) | 3709, (1<<31) | 3199, (1<<31) | 3199, (1<<31) | 3759, (1<<31) | 3759, (1<<31) | 3149,
  (1<<31) | 3149, (1<<31) | 3709, (1<<31) | 3709, (1<<31) | 3199, (1<<31) | 3199, (1<<31) | 3759, (1<<31) | 3759, (1<<31) | 3149,
  (1<<31) | 3149, (1<<31) | 3709, (1<<31) | 3709, (1<<31) | 3199, (1<<31) | 3199, (1<<31) | 3759, (1<<31) | 3759, (1<<31) | 3149,
  (1<<31) | 3149, (1<<31) | 3709, (1<<31) | 3709, (1<<31) | 3199, (1<<31) | 3199, (1<<31) | 3759, (1<<31) | 3759, (1<<31) | 3137,
  (1<<31) | 3697, (1<<31) | 3137, (1<<31) | 3697, (1<<31) | 2587, (1<<31) | 2600, (1<<31) | 3137, (1<<31) | 3697, (1<<31) | 2587,
  (1<<31) | 2600, (1<<31) | 2587, (1<<31) | 2600, (1<<31) | 3137, (1<<31) | 3697, (1<<31) | 3137, (1<<31) | 3697, (1<<31) | 2587,
  (1<<31) | 2600, (1<<31) | 3137, (1<<31) | 3697, (1<<31) | 2587, (1<<31) | 2600, (1<<31) | 2587, (1<<31) | 2600, 0x595959,
  0x595959, 0x595959, 0x595959, 0x2c2c2c2c, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a,
  0x5959, 0x445959, 0x444a4a, 0x40, 0x0, 0x442e0, 0x442e0, 0x442e0,
  0x442e0, 0x2e2c, 0x2e3b, 0x2e4a, 0x2e2c, 0x2e2c, 0x2e4a, 0x2e4a,
  0x3b, 0x4a0, 0x2e2c0, 0x2e3b0, 0x2e4a0, 0x2e4a0, 0x2e4a0, 0x2c2c2c,
  0x3b3b3b, 0x4a4a4a, (1<<31) | 4180, 0x4a4a4a, (1<<31) | 4178, (1<<31) | 4178, 0x2c2c2c, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c,
  0x3b3b3b, 0x4a4a4a, 0x2c2c59, 0x44a7a, 0x44a7a, 0x2c4, 0x7a7a4a, 0x7a7a44,
  0x7a7a4a, 0x7a7a44, 0x2c2c2c, 0x2c2c44, 0x595959, 0x595944, 0x3b3b3b, 0x3b3b44,
  0x4a4a4a, 0x4a4a44, 0x7a7a4a, 0x7a7a44, 0x7a7a4a, 0x7a7a44, 0x2c2c2c, 0x2c2c44,
  0x595959, 0x595944, 0x3b3b3b, 0x3b3b44, 0x4a4a4a, 0x4a4a44, 0x2c2c2c, 0x2c2c44,
  0x595959, 0x595944, 0x3b3b3b, 0x3b3b44, 0x4a4a4a, 0x4a4a44, 0x2c2c2c, 0x2c2c44,
  0x3b3b3b, 0x3b3b44, 0x4a4a4a, 0x4a4a44, 0x2c2c2c, 0x2c2c44, 0x3b3b3b, 0x3b3b44,
  0x4a4a4a, 0x4a4a44, 0x47a4a, 0x47a4a, 0x2c4, 0x7a7a, 0x2c2c, 0x7a7a,
  0x7a7a7a7a, 0x7a7a7a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x595959,
  0x3b3b3b, 0x4a4a4a, 0x3b3b3b3b, 0x3b3b3b3b, 0x7a7a7a, 0x2c2c2c, 0x595959, 0x3b3b3b,
  0x4a4a4a, 0x2c2c2c, 0x595959, 0x3b3b3b, 0x4a4a4a, 0x3b3b3b3b, 0x4a2c2c4a, 0x4a3b3b4a,
  0x4a3b3b4a, 0x4a2c2c4a, 0x4a3b3b4a, 0x4a3b3b4a, 0x2c2c3b, 0x3b3b4a, 0x4a4a59, 0x2c2c3b,
  0x3b3b4a, 0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x4a4a59, 0x2c2c3b, 0x3b3b4a, 0x4a4a59,
  0x7a7a7a7a, 0x2c4a4a4a, 0x4a4a3b, 0x59594a, 0x59594a, 0x3b3b2c, 0x3b3b2c, 0x4a4a3b,
  0x4a4a3b, 0x59594a, 0x3b3b2c, 0x4a4a3b, 0x5959, (1<<31) | 4182, 0x4a4a, 0x7a7a,
  0x7a7a, 0x7a7a, 0x7a7a, 0x7a7a, 0x2c2c2c, 0x595959, 0x59595959, 0x595959,
  0x3b3b3b, 0x4a4a4a, 0x4a4a4a4a, 0x4a4a4a, 0x7a7a, 0x4a4a4a4a, 0x4a4a4a, 0x2c2c2c,
  0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x4a4a4a, 0x4a4a4a, 0x2c2c2c, 0x3b3b3b, 0x4a4a4a,
  0x2c2c2c, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x4a4a4a, (1<<31) | 4180, 0x4a4a4a, (1<<31) | 4178,
  (1<<31) | 4178, 0x2c2c2c, 0x3b3b3b, 0x4a4a4a, 0x2c2c2c, 0x3b3b3b, 0x4a4a4a, 0x4a4a4a,
  0x4a2c4a, 0x4a3b4a, 0x4a2c4a, 0x4a4a4a, 0x3b4a, 0x2c3b, 0x3b4a, 0x4a59,
  0x3b4a, 0x2c3b, 0x3b4a, 0x4a59, 0x555, 0x1f0, 0x2e0, 0x2e0,
  0x2e0, 0x2e0, 0x2e0, 0x2e0, 0x2e0, 0x2e0, 0x555, 0x555,
  0x444, 0x444, 0x5, 0x5, 0x5, 0x5, 0x1, 0x0,
  0x1f0, 0x8a8a, 0x8a8a8a, 0x8a8a8a, 0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a,
  0x8a8a8a, 0x8a8a8a, 0x8a8a8a, 0x8a8a8a, 0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a,
  0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a, 0x48a8a8a, (1<<31) | 3910, (1<<31) | 3910, (1<<31) | 3910,
  (1<<31) | 3910, 0x8a8a8a, 0x8a8a8a, 0x8a8a, 0x8a8a, (1<<31) | 3910, (1<<31) | 3910, (1<<31) | 3910,
  (1<<31) | 3910, (1<<31) | 3910, 0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a, 0x8a8a,
  0x8a8a, 0x8a8a, 0x8a8a, (1<<31) | 3910, 0x8a8a8a, 0x8a8a8a, 0x8a8a8a, (1<<31) | 3910,
  (1<<31) | 3910, 0x8a8a8a, 0x8a8a8a, (1<<31) | 3910, (1<<31) | 3910, (1<<31) | 3910, (1<<31) | 3910, (1<<31) | 3910,
  (1<<31) | 3910, 0x48a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a,
  0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a, 0x2e8a,
  0x2e8a, 0x2e8a, 0x2e8a0, 0x2e8a0, 0x2e8a0, 0x2e8a0, 0x2e8a0, 0x2e8a0,
  0x2e8a0, 0x2e8a0, 0x2e8a0, 0x2e8a0, 0x50, 0x50, 0x50, 0x50,
  0x0, 0x44, 0x4444, 0x4444, 0x4444, 0x4444, 0x44, 0x4,
  0x44, 0x4, 0x4, 0x44, 0x4, 0x44, 0x4, 0x5,
  0x2e89, 0x2e89, 0x52e4a, 0x52e4a, 0x2e4a, 0x2e4a, 0x2e890, 0x2e890,
  0x52e4a0, 0x52e4a0, 0x2e4a0, 0x2e4a0, 0x888, 0x888, 0x898959, 0x898944,
  0x7a7a4a, 0x7a7a44, 0x898959, 0x898944, 0x7a7a4a, 0x7a7a44, 0x898959, 0x898944,
  0x7a7a4a, 0x7a7a44, 0x897a, 0x894a, 0x894a, 0x3b7a, 0x7a89, 0x7a7a,
  0x597a, 0x4a89, 0x597a, 0x4a89, 0x898989, 0x7a7a7a, 0x595989, 0x4a4a7a,
  0x898989, 0x7a7a7a, 0x898989, 0x7a7a7a, 0x8989, 0x8989, 0x7a7a, 0x7a7a,
  0x8989, 0x7a7a, 0x48959, 0x47a4a, 0x8959, 0x7a4a, 0x8959, 0x7a4a,
  0x45959, 0x4594a4a, 0x4a4a4a, 0x7a7a, 0x0, (1<<31) | 788, 0x44a4a0, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
  0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x2f2f, 0x2f2f,
  0x4, 0x4, 0x42e4, 0x5e50, 0x40, 0x40, 0x50, 0x42e4,
  0x42e4, 0x42e0, 0x52f4, 0x4, 0x2c2c2c, 0x2c2c2c2c, 0x4a4a4a, 0x595959,
  0x3b3b3b, 0x2c2c2c, 0x2c2c2c2c, 0x2c2c2c, 0x2c2c2c, 0x4a4a4a, 0x595959, 0x3b3b3b,
  0x2c2c2c, 0x4a4a4a, 0x595959, 0x3b3b3b, 0x2c2c59, (1<<31) | 957, (1<<31) | 2765, (1<<31) | 3128,
  (1<<31) | 1206, (1<<31) | 957, (1<<31) | 2765, (1<<31) | 3128, (1<<31) | 1206, (1<<31) | 957, (1<<31) | 2765, (1<<31) | 3128,
  (1<<31) | 1206, 0x4a4a4a, (1<<31) | 1716, (1<<31) | 2262, (1<<31) | 2533, (1<<31) | 1892, 0x42c2c, 0x44a4a,
  0x45959, 0x43b3b, 0x2c2c2c, 0x4a4a4a, 0x595959, 0x3b3b3b, 0x42c2c2c, (1<<31) | 1738,
  0x44a4a4a, (1<<31) | 2240, 0x43b3b3b, (1<<31) | 1914, 0x42c2c2c, (1<<31) | 1738, 0x44a4a4a, (1<<31) | 2240,
  0x43b3b3b, (1<<31) | 1914, (1<<31) | 3888, (1<<31) | 3875, (1<<31) | 3888, (1<<31) | 3888, (1<<31) | 3875, (1<<31) | 3875,
  0x2c2c2c, (1<<31) | 957, 0x4a4a4a, (1<<31) | 2765, 0x3b3b3b, (1<<31) | 1206, 0x2c2c2c, (1<<31) | 957,
  0x4a4a4a, (1<<31) | 2765, 0x3b3b3b, (1<<31) | 1206, 0x2c2c2c, (1<<31) | 957, 0x4a4a4a, (1<<31) | 2765,
  0x3b3b3b, (1<<31) | 1206, 0x2c2c2c, (1<<31) | 957, 0x4a4a4a, (1<<31) | 2765, 0x3b3b3b, (1<<31) | 1206,
  0x448989, 0x447a7a, 0x4898989, 0x47a7a7a, 0x4898989, 0x47a7a7a, (1<<31) | 2681, (1<<31) | 2613,
  0x3b2c2c3b, 0x594a4a59, 0x2c59592c, 0x4a3b3b4a, 0x2c2c3b, 0x4a4a59, 0x59592c, 0x3b3b4a,
  0x2c2c, (1<<31) | 966, 0x4a4a, (1<<31) | 2749, 0x3b3b, (1<<31) | 1215, 0x42e2c, 0x2e42c,
  0x2e42c, 0x3b2c2c3b, 0x594a4a59, 0x4a3b3b4a, 0x2c2c2c2c, 0x4a4a4a4a, 0x3b3b3b3b, 0x3b2c2c3b,
  0x594a4a59, 0x4a3b3b4a, 0x2c2c2c2c, 0x4a4a4a4a, 0x3b3b3b3b, 0x3b2c2c3b, 0x594a4a59, 0x4a3b3b4a,
  0x3b2c2c3b, 0x594a4a59, 0x4a3b3b4a, 0x2c2c3b, 0x4a4a59, 0x3b3b4a, 0x2c2c2c, 0x4a4a4a,
  0x3b3b3b, 0x2c2c3b, 0x4a4a59, 0x3b3b4a, 0x2c2c2c, 0x4a4a4a, 0x3b3b3b, 0x2c2c3b,
  0x4a4a59, 0x3b3b4a, 0x2c2c3b, 0x4a4a59, 0x3b3b4a, (1<<31) | 1748, 0x4595959, 0x2c2c2c2c,
  0x4a4a3b, (1<<31) | 2756, 0x59594a, (1<<31) | 3119, 0x3b3b2c, (1<<31) | 1197, 0x4a4a3b, (1<<31) | 2756,
  0x59594a, (1<<31) | 3119, 0x3b3b2c, (1<<31) | 1197, 0x2c2c2c2c, 0x2c2c2c2c, 0x2c2c2c, 0x4a4a4a,
  0x595959, 0x3b3b3b, 0x2c2c2c, 0x2c2c2c, 0x2c2c2c, 0x42c2c2c, 0x2c2c2c, 0x2c2c2c,
  0x2c2c2c, 0x2c2c2c, 0x2c2c2c, 0x2e42c0, (1<<31) | 1716, (1<<31) | 1726, (1<<31) | 2262, (1<<31) | 2250,
  (1<<31) | 1892, (1<<31) | 1902, (1<<31) | 1716, (1<<31) | 1726, (1<<31) | 2262, (1<<31) | 2250, (1<<31) | 1892, (1<<31) | 1902,
  0x2e42c0, 0x2c2c4a, 0x4a4a59, 0x3b3b59, 0x3b3b4a, 0x4a4a2c, 0x59592c, 0x2c2c4,
  0x2c3b, 0x4a59, 0x3b4a, 0x2c3b, 0x4a59, 0x2c3b, 0x4a59, 0x3b4a,
  0x3b4a, 0x2c3b, 0x4a59, 0x3b4a, 0x1f, 0x1f1f, 0x0, 0x2e40,
  (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4070, (1<<31) | 4070,
  0x2e4422, 0x2e5522, 0x2e4422, 0x2e5522, 0x595959, 0x5a5a5a, 0x5b5b5b, 0x595959,
  0x5a5a5a, 0x5b5b5b, 0x595959, 0x5a5a5a, 0x5b5b5b, 0x595959, 0x5a5a5a, 0x5b5b5b,
  0x5959, 0x25959, 0x8a8a8a, 0x7b7b7b, (1<<31) | 3910, 0x7b7b7b7b, 0x28a8a8a, 0x27b7b7b,
  0x8a7a, 0x8a4a, 0x7b4b, 0x4b7b, 0x8a4a, 0x7b4b, 0x27b7b7b, 0x8a8a8a,
  0x7b7b7b, 0x8a8a8a, 0x7b7b7b, 0x2e2d, 0x592e89, 0x5a2e8a, 0x4a2e7a, 0x4b2e7b,
  0x89592e0, 0x8a5a2e0, 0x7a4a2e0, 0x7b4b2e0, 0x8a8a8a, 0x7b7b7b, 0x8a8a8a, 0x7b7b7b,
  0x8a4, 0x7b4, 0x5a5a4, 0x5a5a4, 0x5a5a4, 0x7b7b, 0x48a8a, 0x47b7b,
  0x7b7b, 0x8a8a, 0x7b7b, 0x598989, 0x5a8a8a, 0x4a7a7a, 0x4b7b7b, 0x89894,
  0x8a8a4, 0x7a7a4, 0x7b7b4, 0x89894, 0x8a8a4, 0x7a7a4, 0x7b7b4, 0x89894,
  0x8a8a4, 0x7a7a4, 0x7b7b4, 0x0, 0x0, (1<<31) | 418, (1<<31) | 495, (1<<31) | 803,
  (1<<31) | 868, (1<<31) | 669, (1<<31) | 746, (1<<31) | 538, (1<<31) | 593, (1<<31) | 440, (1<<31) | 452, (1<<31) | 815,
  (1<<31) | 880, (1<<31) | 691, (1<<31) | 703, (1<<31) | 550, (1<<31) | 605, 0x4a2e4a, 0x4b2e4b, 0x592e59,
  0x5a2e5a, 0x4a4a2e0, 0x4b4b2e0, 0x59592e0, 0x5a5a2e0, 0x22d2d3c, 0x4b4b3c, 0x3c3c2d,
  0x4b4b3c, 0x3c3c2d, 0x2d2d2d, 0x3c3c3c, 0x2d2d2d, 0x3c3c3c, 0x2d2d2d2d, 0x4b4b4b,
  0x4b7b7b, 0x4b4b4b, 0x3c3c3c, 0x3c3c3c, 0x4b4b4b, 0x3c3c3c, 0x3c3c3c, 0x2d2d3c,
  0x3c3c4b, 0x2d4, 0x4b4b5a, 0x3c3c3c, 0x3c3c3c, 0x3c3c3c, 0x4b4b5a, 0x2d2d5a,
  0x2d2d2d, 0x2d2d2d, 0x4b4b4b, 0x3c3c3c, 0x4a4b4b, 0x595a5a, 0x3b3c3c, 0x44b4b,
  0x45a5a, 0x43c3c, 0x4a4a4a, 0x4b4b4b, 0x595959, 0x5a5a5a, 0x4a4b4b, 0x3b3c3c,
  0x44b4b, 0x43c3c, 0x4a4a4a, 0x4b4b4b, 0x4a4b4b, 0x595a5a, 0x3b3c3c, 0x44b4b,
  0x45a5a, 0x43c3c, 0x4a4a4a, 0x4b4b4b, 0x595959, 0x5a5a5a, 0x2d2d2d, 0x3c3c3c,
  0x2d2d2d, 0x3c3c3c, 0x259, 0x25a, 0x25b, 0x34a, 0x34b, 0x34c,
  0x2c3, 0x2d4, (1<<31) | 1064, 0x4a2, 0x4b2, 0x4c3, 0x592, 0x5a2,
  0x5b2, 0x458989, 0x447a7a, 0x457a7a, 0x4894, 0x4895, 0x4894, 0x4895,
  0x47a4, 0x47a5, 0x47a4, 0x47a5, 0x48989, 0x447a7a, 0x458989, 0x457a7a,
  0x3b2, 0x3c3, 0x3d4, 0x428b8b8b, 0x437c7c7c, (1<<31) | 1482, (1<<31) | 1847, (1<<31) | 1460,
  (1<<31) | 1858, (1<<31) | 1614, (1<<31) | 1581, (1<<31) | 1592, (1<<31) | 1603, (1<<31) | 1526, (1<<31) | 1504, (1<<31) | 1570,
  (1<<31) | 1548, (1<<31) | 1515, (1<<31) | 1493, (1<<31) | 1559, (1<<31) | 1537, (1<<31) | 1427, (1<<31) | 1394, (1<<31) | 1438,
  (1<<31) | 1405, (1<<31) | 1416, (1<<31) | 1383, (1<<31) | 1471, (1<<31) | 1449, 0x442e4b20, 0x442e4c30, 0x442e5b20,
  0x442e5b20, 0x333, 0x333, 0x33, 0x333, 0x334, 0x334, 0x333,
  0x555, 0x444, 0x333, 0x333, (1<<31) | 1705, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637,
  0x2489892, 0x248a8a2, 0x4248b8b2, 0x247a7a2, 0x247b7b2, 0x4347c7c3, 0x42489892, 0x4247a7a2,
  0x32c2c2c, 0x42d2d2d, (1<<31) | 3085, 0x24a4a4a, 0x24b4b4b, 0x34c4c4c, 0x2898989, 0x28a8a8a,
  0x28b8b8b, 0x27a7a7a, 0x27b7b7b, 0x37c7c7c, 0x2595959, 0x25a5a5a, 0x25b5b5b, 0x32c2e0,
  0x42d2e0, (1<<31) | 3068, 0x24a2e0, 0x24b2e0, 0x34c2e0, 0x2892e0, 0x28a2e0, 0x28b2e0,
  0x27a2e0, 0x27b2e0, 0x37c2e0, 0x2592e0, 0x25a2e0, 0x25b2e0, 0x23b2e0, 0x33c2e0,
  0x43d2e0, 0x23b3b3b, 0x33c3c3c, 0x43d3d3d, 0x24a4a4a, 0x24b4b4b, 0x34c4c4c, 0x2595959,
  0x25a5a5a, 0x25b5b5b, 0x27a4a7a, 0x27b4b7b, 0x437c4c7c, 0x24a894a, 0x24a8a4a, 0x424b8b4b,
  0x27a897a, 0x27a8a7a, 0x427b8b7b, 0x2598959, 0x25a8a5a, 0x425b8b5b, 0x24a894a, 0x24a8a4a,
  0x424b8b4b, 0x2598959, 0x25a8a5a, 0x425b8b5b, 0x24a7a4a, 0x24b7b4b, 0x434c7c4c, 0x2897a89,
  0x28a7a8a, 0x428b7b8b, 0x2597a59, 0x25a7a5a, 0x425b7b5b, 0x24a7a4a, 0x24b7b4b, 0x434c7c4c,
  0x2597a59, 0x25a7a5a, 0x425b7b5b, 0x2895989, 0x28a5a8a, 0x428b5b8b, 0x27a597a, 0x27a5a7a,
  0x427b5b7b, (1<<31) | 1648, (1<<31) | 1671, 0x24a894a, 0x24a8a4a, 0x424b8b4b, 0x2598959, 0x25a8a5a,
  0x425b8b5b, 0x24a894a, 0x24a8a4a, 0x424b8b4b, 0x2598959, 0x25a8a5a, 0x425b8b5b, 0x24a7a4a,
  0x24b7b4b, 0x434c7c4c, 0x2597a59, 0x25a7a5a, 0x425b7b5b, 0x24a7a4a, 0x24b7b4b, 0x434c7c4c,
  0x2597a59, 0x25a7a5a, 0x425b7b5b, 0x27a4a7a, 0x27b4b7b, 0x437c4c7c, 0x2895989, 0x28a5a8a,
  0x428b5b8b, 0x27a597a, 0x27a5a7a, 0x427b5b7b, (1<<31) | 266, (1<<31) | 1113, (1<<31) | 1944, (1<<31) | 1705,
  (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, 0x32c2c2c, 0x42d2d2d, (1<<31) | 3085, 0x24a4a4a, 0x24b4b4b,
  0x34c4c4c, 0x32c2e2c, 0x42d2e2d, (1<<31) | 3075, 0x24a2e4a, 0x24b2e4b, 0x34c2e4c, 0x2892e89,
  0x28a2e8a, 0x28b2e8b, 0x27a2e7a, 0x27b2e7b, 0x37c2e7c, 0x2592e59, 0x25a2e5a, 0x25b2e5b,
  0x23b2e3b, 0x33c2e3c, 0x43d2e3d, 0x2898989, 0x28a8a8a, 0x28b8b8b, 0x27a7a7a, 0x27b7b7b,
  0x37c7c7c, 0x2595959, 0x25a5a5a, 0x25b5b5b, 0x23b3b3b, 0x33c3c3c, 0x43d3d3d, (1<<31) | 353,
  (1<<31) | 375, (1<<31) | 1371, (1<<31) | 309, (1<<31) | 331, (1<<31) | 1835, (1<<31) | 1359, (1<<31) | 1347, 0x24892,
  0x248a2, 0x248b2, 0x247a2, 0x247b2, 0x347c3, 0x24892, 0x247a2, 0x2898989,
  0x28a8a8a, 0x428b8b8b, 0x27a7a7a, 0x27b7b7b, 0x437c7c7c, (1<<31) | 1682, (1<<31) | 1637, 0x28948989,
  0x28a48a8a, (1<<31) | 1695, 0x27a47a7a, 0x27b47b7b, (1<<31) | 1871, (1<<31) | 1659, (1<<31) | 1625, (1<<31) | 1705,
  (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 1705, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 1705,
  (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 256, (1<<31) | 1103,
  (1<<31) | 1934, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 892,
  (1<<31) | 922, (1<<31) | 617, (1<<31) | 648, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 1073, (1<<31) | 1758,
  (1<<31) | 3083, (1<<31) | 758, (1<<31) | 1177, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 246, (1<<31) | 1093, (1<<31) | 1924,
  (1<<31) | 397, (1<<31) | 474, (1<<31) | 1146, 0x22c4a2c, 0x22c4b2c, 0x32c4c2c, 0x24a2e0, 0x24b2e0,
  0x34c2e0, 0x23b4a3b, 0x23b4b3b, 0x33c4c3c, 0x24a2e0, 0x24b2e0, 0x34c2e0, 0x22c592c,
  0x22c5a2c, 0x22c5b2c, 0x2592e0, 0x25a2e0, 0x25b2e0, 0x24a594a, 0x24a5a4a, 0x24b5b4b,
  0x2592e0, 0x25a2e0, 0x25b2e0, 0x23b593b, 0x23b5a3b, 0x23b5b3b, 0x2592e0, 0x25a2e0,
  0x25b2e0, 0x22c3b2c, 0x32c3c2c, 0x42d3d2d, 0x23b2e0, 0x33c2e0, 0x43d2e0, 0x22c4a2c,
  0x22c4b2c, 0x32c4c2c, 0x24a2e0, 0x24b2e0, 0x34c2e0, 0x23b4a3b, 0x23b4b3b, 0x33c4c3c,
  0x24a2e0, 0x24b2e0, 0x34c2e0, 0x22c592c, 0x22c5a2c, 0x22c5b2c, 0x2592e0, 0x25a2e0,
  0x25b2e0, 0x24a594a, 0x24a5a4a, 0x24b5b4b, 0x2592e0, 0x25a2e0, 0x25b2e0, 0x23b593b,
  0x23b5a3b, 0x23b5b3b, 0x2592e0, 0x25a2e0, 0x25b2e0, 0x22c3b2c, 0x32c3c2c, 0x42d3d2d,
  0x23b2e0, 0x33c2e0, 0x43d2e0, 0x22c4a2c, 0x22c4b2c, 0x32c4c2c, 0x24a2e0, 0x24b2e0,
  0x34c2e0, 0x23b4a3b, 0x23b4b3b, 0x33c4c3c, 0x24a2e0, 0x24b2e0, 0x34c2e0, 0x22c592c,
  0x22c5a2c, 0x22c5b2c, 0x2592e0, 0x25a2e0, 0x25b2e0, 0x24a594a, 0x24a5a4a, 0x24b5b4b,
  0x2592e0, 0x25a2e0, 0x25b2e0, 0x23b593b, 0x23b5a3b, 0x23b5b3b, 0x2592e0, 0x25a2e0,
  0x25b2e0, 0x22c3b2c, 0x32c3c2c, 0x42d3d2d, 0x23b2e0, 0x33c2e0, 0x43d2e0, (1<<31) | 256,
  (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934,
  (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, 0x24a44a4a, 0x24b44b4b, 0x34c44c4c, 0x25945959, 0x25a45a5a,
  0x25b45b5b, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, 0x24a44a4a,
  0x24b44b4b, 0x34c44c4c, 0x25945959, 0x25a45a5a, 0x25b45b5b, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167,
  (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 256, (1<<31) | 1103,
  (1<<31) | 1934, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 298,
  (1<<31) | 320, (1<<31) | 1135, (1<<31) | 342, (1<<31) | 364, (1<<31) | 386, (1<<31) | 792, (1<<31) | 857, (1<<31) | 1693,
  (1<<31) | 658, (1<<31) | 735, (1<<31) | 1869, (1<<31) | 1994, (1<<31) | 1982, 0x28948989, 0x28a48a8a, (1<<31) | 1695,
  0x27a47a7a, 0x27b47b7b, (1<<31) | 1871, (1<<31) | 1994, (1<<31) | 1982, 0x28948989, 0x28a48a8a, (1<<31) | 1695,
  0x27a47a7a, 0x27b47b7b, (1<<31) | 1871, (1<<31) | 1994, (1<<31) | 1982, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705,
  (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, 0x2898989, 0x28a8a8a, 0x428b8b8b,
  0x27a7a7a, 0x27b7b7b, 0x437c7c7c, (1<<31) | 1682, (1<<31) | 1637, 0x27a2e0, (1<<31) | 1705, (1<<31) | 1881,
  (1<<31) | 1682, (1<<31) | 1637, 0x27a3b7a, 0x27b3b7b, 0x437c3c7c, 0x23b47a3b, 0x23b47b3b, 0x33c47c3c,
  (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637,
  (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 847, (1<<31) | 912,
  (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725,
  (1<<31) | 778, (1<<31) | 1881, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167,
  (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507,
  (1<<31) | 1167, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 827, (1<<31) | 892, (1<<31) | 922, (1<<31) | 681,
  (1<<31) | 758, (1<<31) | 1177, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083,
  (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 837, (1<<31) | 902,
  (1<<31) | 932, (1<<31) | 715, (1<<31) | 768, (1<<31) | 1187, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 1073,
  (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648,
  (1<<31) | 407, (1<<31) | 484, (1<<31) | 1156, (1<<31) | 527, (1<<31) | 582, (1<<31) | 637, (1<<31) | 277, (1<<31) | 1124,
  (1<<31) | 1955, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 256,
  (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 407, (1<<31) | 484, (1<<31) | 1156, (1<<31) | 527, (1<<31) | 582, (1<<31) | 637,
  (1<<31) | 277, (1<<31) | 1124, (1<<31) | 1955, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 562, (1<<31) | 617,
  (1<<31) | 648, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, 0x32c2c3, 0x42d2d4, (1<<31) | 3093, (1<<31) | 847,
  (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 847,
  (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705,
  (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705,
  (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778,
  (1<<31) | 1881, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 353, (1<<31) | 375, (1<<31) | 1371, (1<<31) | 309, (1<<31) | 331,
  (1<<31) | 1835, (1<<31) | 1359, (1<<31) | 1347, (1<<31) | 298, (1<<31) | 320, (1<<31) | 1135, (1<<31) | 342, (1<<31) | 364,
  (1<<31) | 386, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 1682,
  (1<<31) | 1637, (1<<31) | 847, (1<<31) | 912, (1<<31) | 1705, (1<<31) | 725, (1<<31) | 778, (1<<31) | 1881, (1<<31) | 430,
  (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167,
  (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 256, (1<<31) | 1103,
  (1<<31) | 1934, (1<<31) | 837, (1<<31) | 902, (1<<31) | 932, (1<<31) | 715, (1<<31) | 768, (1<<31) | 1187, (1<<31) | 562,
  (1<<31) | 617, (1<<31) | 648, (1<<31) | 1073, (1<<31) | 1758, (1<<31) | 3083, (1<<31) | 562, (1<<31) | 617, (1<<31) | 648,
  (1<<31) | 562, (1<<31) | 617, (1<<31) | 648, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 562, (1<<31) | 617,
  (1<<31) | 648, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, (1<<31) | 430, (1<<31) | 507, (1<<31) | 1167, (1<<31) | 562,
  (1<<31) | 617, (1<<31) | 648, (1<<31) | 256, (1<<31) | 1103, (1<<31) | 1934, 0x4c4c3d, (1<<31) | 1222, 0x4c4c3d,
  (1<<31) | 1222, 0x4c4c5b, 0x4c4c5b, (1<<31) | 1057, (1<<31) | 1050, 0x4a4c4c, 0x595b5b, 0x3b3d3d,
  0x44c4c, 0x45b5b, 0x43d3d, 0x4c4c4c, 0x5b5b5b, 0x3b3b3b, 0x3c3c3c, 0x3d3d3d,
  0x4a4c4c, 0x595959, 0x595a5a, 0x595b5b, 0x3b3d3d, 0x44c4c, 0x45959, 0x45a5a,
  0x45b5b, 0x43d3d, 0x4c4c4c, 0x595959, 0x5a5a5a, 0x5b5b5b, 0x3b3b3b, 0x3c3c3c,
  0x3d3d3d, 0x4a4c4c, 0x595b5b, 0x3b3d3d, 0x44c4c, 0x45b5b, 0x43d3d, 0x4c4c4c,
  0x5b5b5b, 0x3b3b3b, 0x3c3c3c, 0x3d3d3d, 0x2898989, 0x28a8a8a, 0x28b8b8b, 0x27a7a7a,
  0x27b7b7b, 0x37c7c7c, (1<<31) | 847, (1<<31) | 725, 0x428b8b8b, 0x437c7c7c, (1<<31) | 1682, (1<<31) | 1637,
  0x2898989, 0x28a8a8a, 0x28b8b8b, 0x27a7a7a, 0x27b7b7b, 0x37c7c7c, (1<<31) | 847, (1<<31) | 725,
  0x428b8b8b, 0x437c7c7c, (1<<31) | 1682, (1<<31) | 1637, (1<<31) | 2729, (1<<31) | 2356, (1<<31) | 2563, (1<<31) | 2671,
  (1<<31) | 2739, (1<<31) | 2302, (1<<31) | 2573, (1<<31) | 2661, (1<<31) | 2699, (1<<31) | 2523, (1<<31) | 2719, (1<<31) | 2553,
  (1<<31) | 2631, (1<<31) | 2272, (1<<31) | 2641, (1<<31) | 2282, 0x442e4b20, 0x442e4c30, 0x442e5b20, 0x442e5b20,
  (1<<31) | 2689, (1<<31) | 2513, (1<<31) | 2709, (1<<31) | 2543, (1<<31) | 2621, (1<<31) | 2230, (1<<31) | 2651, (1<<31) | 2292,
  0x2e8b, 0x2e7c, 0x4489894, 0x447a7a4, 0x4894, 0x4895, 0x4894, 0x4895,
  0x47a4, 0x47a5, 0x47a4, 0x47a5, 0x5b8b8b, 0x4c7c7c, 0x444, 0x555,
  0x444, 0x555, 0x444, 0x555, 0x444, 0x555, 0x2e0, 0x2e0,
  0x2e0, 0x2e0, 0x4, 0x5, 0x40, 0x50, (1<<31) | 3897, (1<<31) | 3910,
  0x7a7a7a7a, 0x7b7b7b7b, (1<<31) | 3897, 0x7a7a7a7a, (1<<31) | 3897, (1<<31) | 3910, 0x7a7a7a7a, 0x7b7b7b7b,
  (1<<31) | 3897, (1<<31) | 3910, 0x7a7a7a7a, 0x7b7b7b7b, (1<<31) | 3897, 0x7a7a7a7a, (1<<31) | 3897, (1<<31) | 3910,
  0x7a7a7a7a, 0x7b7b7b7b, (1<<31) | 3897, (1<<31) | 3910, 0x7a7a7a7a, 0x7b7b7b7b, (1<<31) | 3897, 0x7a7a7a7a,
  (1<<31) | 3897, (1<<31) | 3910, 0x7a7a7a7a, 0x7b7b7b7b, (1<<31) | 3897, 0x7a7a7a7a, (1<<31) | 3897, 0x7a7a7a7a,
  0x2e0, 0x2e0, 0x2e0, 0x2e0, 0x40, 0x50, 0x20, 0x2e0,
  0x4442, 0x4452, 0x4440, 0x4450, 0x0, 0x0, (1<<31) | 1038, (1<<31) | 4068,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 1068, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 3051, (1<<31) | 2438,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4055, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 3064,
  (1<<31) | 3064, (1<<31) | 3064, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 3064, (1<<31) | 3064, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 3064, (1<<31) | 3064, (1<<31) | 3064, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073,
  (1<<31) | 4073, (1<<31) | 4073, (1<<31) | 4073, 0x442e0, 0x4440, 0x2595959, 0x25a5a5a, 0x25b5b5b,
  0x4, 0x5, 0x4, 0x5, 0x4, 0x45, (1<<31) | 1966, (1<<31) | 2449,
  (1<<31) | 2583, (1<<31) | 1966, (1<<31) | 2449, (1<<31) | 2583, 0x44, 0x55, 0x5, 0x2e5,
  0x2e0, 0x0, 0x2e0, 0x2e0, 0x2e2e, 0x2e2e2e, 0x0, 0x4a4a4a,
  0x4a4a4a, 0x4a4a4a, 0x24a4a4a, 0x4a4a4a, 0x4a4a4a, 0x4a4a4a4a, 0x2e, 0x27a7a7a,
  0x27a7a7a, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4, (1<<31) | 3906,
  (1<<31) | 4064, (1<<31) | 4058, (1<<31) | 3884, 0x47a7a, 0x57a7a, 0x7a4, 0x7a5, (1<<31) | 3906,
  (1<<31) | 3884, 0x7a4, 0x7a5, 0x2e0, 0x7a7a7a, 0x7a7a7a, 0x7a7a7a, 0x7a7a7a,
  0x7a4, (1<<31) | 1069, 0x7a7a, 0x7a7a, 0x7a7a, 0x7a7a, 0x0, 0x7a7a,
  0x7a7a, 0x2e0, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4, 0x7a7a4,
  0x2e0, 0x2898989, 0x2898989, 0x89894, 0x89894, 0x89894, 0x89894, 0x89894,
  0x89894, 0x4a7a, 0x894a, 0x897a, 0x7a4a, 0x894, 0x895, 0x897a7a,
  0x48989, 0x58989, 0x7a8989, 0x894a, 0x7a4a, 0x894, 0x895, 0x0,
  0x2e2c2c0, 0x898989, 0x898989, 0x0, 0x898989, 0x898989, 0x894, 0x4a4a3b,
  0x3b3b2c, 0x3b3b2c, 0x2c2c2c, 0x3b3b3b, 0x2c2c2c, 0x3b3b3b, 0x0, 0x3b3b4a,
  0x2c4, 0x3b3b3b, 0x3b3b3b, 0x4a4a59, 0x2c2c59, 0x4a4a4a, 0x595959, 0x3b3b3b,
  0x44a4a, 0x45959, 0x43b3b, 0x4a4a4a, 0x3b3b3b, 0x44a4a, 0x43b3b, 0x4a4a4a,
  0x595959, 0x3b3b3b, 0x44a4a, 0x45959, 0x43b3b, 0x2c2c2c, 0x3b3b3b, 0x2c2c2c,
  0x3b3b3b, 0x8989, 0x8989, 0x89894, 0x89894, 0x89894, 0x89894, 0x89894,
  0x89894, 0x898989, 0x7a7a7a, 0x898989, 0x7a7a7a, 0x898989, 0x7a7a7a, 0x2e2c,
  0x442e0, 0x440, (1<<31) | 3897, 0x7a7a7a7a, 0x2898989, 0x27a7a7a, 0x27a7a7a, 0x22c2c3b,
  0x4a4a3b, 0x2c2c2c2c, 0x3b3b, 0x4a4a59, 0x59594, 0x59594, 0x59594, 0x48989,
  0x47a7a, 0x4898989, 0x47a7a7a, 0x344, 0x444, 0x244, 0x555, 0x242c42c4,
  0x242c42c4, 0x242c42c4, 0x242c42c4, 0x242c42c4, 0x242c42c4, (1<<31) | 288, 0x22c2c4, 0x22c2c4,
  0x22c2c4, 0x22c2c4, 0x22c2c4, 0x22c2c4, 0x22c2c2c, 0x2c5959, 0x225959, 0x595959,
  0x22595959, (1<<31) | 4070, (1<<31) | 4070, (1<<31) | 4070, (1<<31) | 4073, 0x4a4a4a, (1<<31) | 4073, 0x3b3b3b,
  (1<<31) | 4073, 0x3b3b3b, (1<<31) | 4073, 0x4a4a4a, (1<<31) | 4073, 0x3b3b3b, (1<<31) | 4073, 0x3b3b3b,
  (1<<31) | 4073, 0x2c2c3b, (1<<31) | 4073, 0x3b3b3b, (1<<31) | 4073, 0x2c2c2c, (1<<31) | 4073, 0x2c2c2c,
  (1<<31) | 4073, 0x4a4a4a, (1<<31) | 4073, 0x3b3b3b, 0x2e4422, 0x2e5522, 0x444, 0x555,
  0x3b7a, 0x3b7b, 0x47a3b, 0x47b3b, 0x22c2c2c, 0x22d2d2d, (1<<31) | 234, 0x22c2c2c,
  0x22d2d2d, (1<<31) | 234, 0x2c2c2c, 0x2d2d2d, (1<<31) | 1050, 0x40, 0x50, 0x40,
  0x50, 0x40, 0x2e40, 0x2e50, 0x2e40, 0x2e50, 0x20, 0x4,
  0x0, 0x45, 0x8989, 0x8a8a, 0x7a7a, 0x7b7b, 0x8989, 0x7a7a,
  0x22c2c2c, 0x24a4a4a, 0x2595959, 0x22c2c2c, 0x24a4a4a, 0x2595959, 0x23b3b3b, 0x23b3b3b,
  (1<<31) | 572, (1<<31) | 627, (1<<31) | 464, (1<<31) | 517, 0x2c4a, 0x2c59, 0x2c3b, 0x4a59,
  0x2c4a, 0x2c59, 0x2c3b, 0x4a59, 0x3b4a, 0x3b59, 0x3b4a, 0x3b59,
  0x2c3b, 0x4a59, 0x3b4a, 0x4a4a4a4a, 0x594a4a59, 0x594a4a59, 0x4a4a4a4a, 0x594a4a59,
  0x594a4a59, 0x4a3b3b4a, 0x3b3b3b3b, 0x4a3b3b4a, 0x3b3b3b3b, 0x4a3b3b4a, 0x4a3b3b4a, 0x2c2c2c2c,
  0x2c2c2c, 0x22c2c, 0x4a4a4a, 0x24a4a, 0x595959, 0x25959, 0x3b3b3b, 0x23b3b,
  0x2c2c2c, 0x4a4a4a, 0x595959, 0x3b3b3b, 0x2c2c2c, 0x4a4a4a, 0x595959, 0x3b3b3b,
  0x442e0, 0x442e0, 0x442e0, 0x442e0, 0x442e0, 0x442e0, 0x442e0, 0x442e0,
  0x442e0, 0x442e0, 0x442e0, 0x442e0, 0x4440, 0x4, 0x44, 0x2e2e,
  0x44f0, 0x0, 0x4f0, 0x40, 0x4444, (1<<31) | 2070, 0x4f0, 0x4f0,
  0x4f4, 0x4f0, 0x4, 0x4, 0x4, 0x44, 0x44f, 0xcf4f,
  0x4f4, 0x4f4, 0x4f4, 0x2e4f0, 0x2e4f0, 0x2e4f0, 0x2e4f0, 0x2e4f0,
  0x44f4, 0x4f4, 0x4f0, 0x4f0, 0x44f0, 0x44f0, 0x44f4, 0x44f0,
  0x4f4, 0x44f0, 0xcf4f0, 0x44f0, 0x2e4f0, 0x440, 0x44f0, 0x44f0,
  0xcf4f0, 0x40, 0x44f0, 0x2e4f0, 0x444, 0x0, 0x4f0, 0x4f4,
  0x4f4, 0x2e, 0x444, 0
];

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const IIT_LONG_ENCODING_TABLE: &[u8]= &[
  /* 0 */ 0, 4, 4, 15, 0, 15, 0, 15, 0, 15, 0, 1, 1, 0,
  /* 14 */ 15, 2, 15, 10, 15, 17, 10, 4, 4, 1, 1, 1, 1, 1, 0,
  /* 29 */ 0, 15, 2, 15, 9, 15, 17, 4, 1, 1, 1, 1, 0,
  /* 42 */ 4, 4, 4, 15, 1, 11, 4, 1, 1, 1, 0,
  /* 53 */ 0, 4, 4, 15, 3, 15, 3, 1, 1, 0,
  /* 63 */ 0, 15, 0, 10, 4, 4, 4, 4, 4, 4, 4, 1, 1, 0,
  /* 77 */ 0, 15, 2, 10, 4, 4, 4, 1, 1, 0,
  /* 87 */ 21, 15, 2, 1, 15, 2, 15, 2, 1, 0,
  /* 97 */ 15, 2, 15, 2, 15, 2, 15, 2, 1, 0,
  /* 107 */ 0, 15, 3, 33, 3, 31, 3, 1, 0,
  /* 116 */ 0, 15, 3, 34, 1, 0, 4, 31, 3, 1, 0,
  /* 127 */ 0, 15, 3, 15, 12, 4, 31, 3, 1, 0,
  /* 137 */ 15, 1, 15, 12, 15, 1, 4, 4, 1, 0,
  /* 147 */ 15, 1, 15, 1, 15, 1, 4, 4, 4, 1, 0,
  /* 158 */ 0, 15, 4, 15, 12, 15, 17, 4, 1, 0,
  /* 168 */ 21, 1, 5, 1, 0,
  /* 173 */ 21, 15, 1, 1, 15, 1, 15, 1, 0,
  /* 182 */ 0, 19, 15, 1, 0,
  /* 187 */ 2, 18, 1, 0,
  /* 191 */ 15, 1, 25, 1, 0,
  /* 196 */ 36, 1, 36, 1, 36, 1, 0,
  /* 203 */ 21, 12, 4, 36, 1, 12, 4, 12, 4, 36, 1, 0,
  /* 215 */ 37, 1, 37, 1, 37, 1, 0,
  /* 222 */ 21, 13, 4, 37, 1, 13, 4, 13, 4, 37, 1, 0,
  /* 234 */ 16, 2, 16, 2, 16, 2, 2, 0,
  /* 242 */ 27, 2, 2, 0,
  /* 246 */ 11, 3, 12, 2, 12, 2, 11, 3, 2, 0,
  /* 256 */ 11, 3, 11, 3, 11, 3, 11, 3, 2, 0,
  /* 266 */ 11, 3, 12, 2, 12, 2, 4, 11, 3, 2, 0,
  /* 277 */ 11, 3, 11, 3, 11, 3, 4, 11, 3, 2, 0,
  /* 288 */ 12, 2, 12, 2, 4, 12, 2, 4, 2, 0,
  /* 298 */ 10, 4, 10, 4, 10, 4, 10, 4, 4, 2, 0,
  /* 309 */ 10, 7, 10, 7, 10, 7, 10, 4, 4, 2, 0,
  /* 320 */ 11, 4, 11, 4, 11, 4, 11, 4, 4, 2, 0,
  /* 331 */ 11, 7, 11, 7, 11, 7, 11, 4, 4, 2, 0,
  /* 342 */ 9, 5, 9, 5, 9, 5, 9, 5, 4, 2, 0,
  /* 353 */ 9, 8, 9, 8, 9, 8, 9, 5, 4, 2, 0,
  /* 364 */ 10, 5, 10, 5, 10, 5, 10, 5, 4, 2, 0,
  /* 375 */ 10, 8, 10, 8, 10, 8, 10, 5, 4, 2, 0,
  /* 386 */ 11, 5, 11, 5, 11, 5, 11, 5, 4, 2, 0,
  /* 397 */ 10, 4, 11, 3, 11, 3, 10, 4, 2, 0,
  /* 407 */ 10, 4, 10, 4, 10, 4, 4, 10, 4, 2, 0,
  /* 418 */ 10, 4, 10, 4, 14, 2, 10, 4, 10, 4, 2, 0,
  /* 430 */ 10, 4, 10, 4, 10, 4, 10, 4, 2, 0,
  /* 440 */ 10, 4, 10, 4, 14, 2, 9, 5, 10, 4, 2, 0,
  /* 452 */ 10, 4, 10, 4, 14, 2, 10, 5, 10, 4, 2, 0,
  /* 464 */ 10, 7, 10, 7, 10, 7, 10, 4, 2, 0,
  /* 474 */ 11, 4, 12, 3, 12, 3, 11, 4, 2, 0,
  /* 484 */ 11, 4, 11, 4, 11, 4, 4, 11, 4, 2, 0,
  /* 495 */ 11, 4, 11, 4, 14, 2, 11, 4, 11, 4, 2, 0,
  /* 507 */ 11, 4, 11, 4, 11, 4, 11, 4, 2, 0,
  /* 517 */ 11, 7, 11, 7, 11, 7, 11, 4, 2, 0,
  /* 527 */ 9, 5, 9, 5, 9, 5, 4, 9, 5, 2, 0,
  /* 538 */ 9, 5, 9, 5, 14, 2, 10, 4, 9, 5, 2, 0,
  /* 550 */ 9, 5, 9, 5, 14, 2, 9, 5, 9, 5, 2, 0,
  /* 562 */ 9, 5, 9, 5, 9, 5, 9, 5, 2, 0,
  /* 572 */ 9, 8, 9, 8, 9, 8, 9, 5, 2, 0,
  /* 582 */ 10, 5, 10, 5, 10, 5, 4, 10, 5, 2, 0,
  /* 593 */ 10, 5, 10, 5, 14, 2, 10, 4, 10, 5, 2, 0,
  /* 605 */ 10, 5, 10, 5, 14, 2, 10, 5, 10, 5, 2, 0,
  /* 617 */ 10, 5, 10, 5, 10, 5, 10, 5, 2, 0,
  /* 627 */ 10, 8, 10, 8, 10, 8, 10, 5, 2, 0,
  /* 637 */ 11, 5, 11, 5, 11, 5, 4, 11, 5, 2, 0,
  /* 648 */ 11, 5, 11, 5, 11, 5, 11, 5, 2, 0,
  /* 658 */ 10, 7, 10, 7, 10, 7, 4, 10, 7, 2, 0,
  /* 669 */ 10, 7, 10, 7, 14, 2, 10, 4, 10, 7, 2, 0,
  /* 681 */ 10, 7, 10, 7, 10, 4, 10, 7, 2, 0,
  /* 691 */ 10, 7, 10, 7, 14, 2, 9, 5, 10, 7, 2, 0,
  /* 703 */ 10, 7, 10, 7, 14, 2, 10, 5, 10, 7, 2, 0,
  /* 715 */ 10, 7, 10, 4, 10, 7, 10, 7, 2, 0,
  /* 725 */ 10, 7, 10, 7, 10, 7, 10, 7, 2, 0,
  /* 735 */ 11, 7, 11, 7, 11, 7, 4, 11, 7, 2, 0,
  /* 746 */ 11, 7, 11, 7, 14, 2, 11, 4, 11, 7, 2, 0,
  /* 758 */ 11, 7, 11, 7, 11, 4, 11, 7, 2, 0,
  /* 768 */ 11, 7, 11, 4, 11, 7, 11, 7, 2, 0,
  /* 778 */ 11, 7, 11, 7, 11, 7, 11, 7, 2, 0,
  /* 788 */ 27, 7, 2, 0,
  /* 792 */ 9, 8, 9, 8, 9, 8, 4, 9, 8, 2, 0,
  /* 803 */ 9, 8, 9, 8, 14, 2, 10, 4, 9, 8, 2, 0,
  /* 815 */ 9, 8, 9, 8, 14, 2, 9, 5, 9, 8, 2, 0,
  /* 827 */ 9, 8, 9, 8, 9, 5, 9, 8, 2, 0,
  /* 837 */ 9, 8, 9, 5, 9, 8, 9, 8, 2, 0,
  /* 847 */ 9, 8, 9, 8, 9, 8, 9, 8, 2, 0,
  /* 857 */ 10, 8, 10, 8, 10, 8, 4, 10, 8, 2, 0,
  /* 868 */ 10, 8, 10, 8, 14, 2, 10, 4, 10, 8, 2, 0,
  /* 880 */ 10, 8, 10, 8, 14, 2, 10, 5, 10, 8, 2, 0,
  /* 892 */ 10, 8, 10, 8, 10, 5, 10, 8, 2, 0,
  /* 902 */ 10, 8, 10, 5, 10, 8, 10, 8, 2, 0,
  /* 912 */ 10, 8, 10, 8, 10, 8, 10, 8, 2, 0,
  /* 922 */ 11, 8, 11, 8, 11, 5, 11, 8, 2, 0,
  /* 932 */ 11, 8, 11, 5, 11, 8, 11, 8, 2, 0,
  /* 942 */ 11, 2, 11, 2, 11, 2, 11, 2, 11, 2, 11, 2, 11, 2, 0,
  /* 957 */ 21, 12, 2, 4, 12, 2, 12, 2, 0,
  /* 966 */ 21, 12, 2, 4, 12, 2, 0,
  /* 973 */ 18, 4, 14, 2, 14, 2, 14, 2, 0,
  /* 982 */ 21, 4, 4, 14, 2, 0,
  /* 988 */ 21, 5, 5, 14, 2, 0,
  /* 994 */ 40, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 14, 2, 0,
  /* 1014 */ 23, 9, 6, 9, 6, 9, 6, 9, 6, 14, 2, 0,
  /* 1026 */ 40, 7, 7, 7, 7, 7, 7, 7, 7, 14, 2, 0,
  /* 1038 */ 0, 17, 17, 14, 2, 0,
  /* 1044 */ 14, 2, 18, 14, 2, 0,
  /* 1050 */ 16, 2, 16, 2, 16, 2, 0,
  /* 1057 */ 11, 5, 16, 2, 16, 2, 0,
  /* 1064 */ 5, 16, 2, 0,
  /* 1068 */ 17, 17, 17, 2, 0,
  /* 1073 */ 12, 2, 12, 2, 12, 2, 12, 2, 3, 0,
  /* 1083 */ 0, 5, 4, 4, 4, 3, 3, 3, 3, 0,
  /* 1093 */ 12, 3, 13, 2, 13, 2, 12, 3, 3, 0,
  /* 1103 */ 12, 3, 12, 3, 12, 3, 12, 3, 3, 0,
  /* 1113 */ 12, 3, 13, 2, 13, 2, 4, 12, 3, 3, 0,
  /* 1124 */ 12, 3, 12, 3, 12, 3, 4, 12, 3, 3, 0,
  /* 1135 */ 12, 4, 12, 4, 12, 4, 12, 4, 4, 3, 0,
  /* 1146 */ 12, 4, 13, 3, 13, 3, 12, 4, 3, 0,
  /* 1156 */ 12, 4, 12, 4, 12, 4, 4, 12, 4, 3, 0,
  /* 1167 */ 12, 4, 12, 4, 12, 4, 12, 4, 3, 0,
  /* 1177 */ 12, 7, 12, 7, 12, 4, 12, 7, 3, 0,
  /* 1187 */ 12, 7, 12, 4, 12, 7, 12, 7, 3, 0,
  /* 1197 */ 21, 12, 2, 4, 11, 3, 11, 3, 0,
  /* 1206 */ 21, 11, 3, 4, 11, 3, 11, 3, 0,
  /* 1215 */ 21, 11, 3, 4, 11, 3, 0,
  /* 1222 */ 16, 2, 13, 3, 13, 3, 0,
  /* 1229 */ 15, 3, 33, 3, 31, 3, 1, 15, 3, 0,
  /* 1239 */ 15, 3, 34, 1, 0, 4, 31, 3, 1, 15, 3, 0,
  /* 1251 */ 15, 3, 15, 12, 4, 31, 3, 1, 15, 3, 0,
  /* 1262 */ 15, 3, 15, 3, 12, 2, 12, 2, 12, 2, 12, 2, 15, 3, 0,
  /* 1277 */ 15, 3, 15, 3, 12, 2, 12, 2, 12, 2, 15, 3, 0,
  /* 1290 */ 15, 3, 15, 3, 12, 2, 12, 2, 15, 3, 0,
  /* 1301 */ 15, 3, 25, 3, 0,
  /* 1306 */ 15, 3, 25, 3, 25, 3, 0,
  /* 1313 */ 15, 3, 26, 3, 0,
  /* 1318 */ 15, 3, 26, 3, 26, 3, 0,
  /* 1325 */ 15, 1, 25, 1, 4, 0,
  /* 1331 */ 12, 4, 12, 4, 36, 1, 4, 0,
  /* 1339 */ 13, 4, 13, 4, 37, 1, 4, 0,
  /* 1347 */ 10, 7, 10, 7, 10, 7, 10, 4, 4, 2, 4, 0,
  /* 1359 */ 9, 8, 9, 8, 9, 8, 9, 5, 4, 2, 4, 0,
  /* 1371 */ 11, 8, 11, 8, 11, 8, 11, 5, 4, 2, 4, 0,
  /* 1383 */ 10, 4, 10, 4, 14, 2, 10, 4, 2, 4, 0,
  /* 1394 */ 9, 5, 9, 5, 14, 2, 10, 4, 2, 4, 0,
  /* 1405 */ 10, 5, 10, 5, 14, 2, 10, 4, 2, 4, 0,
  /* 1416 */ 10, 7, 10, 7, 14, 2, 10, 4, 2, 4, 0,
  /* 1427 */ 9, 8, 9, 8, 14, 2, 10, 4, 2, 4, 0,
  /* 1438 */ 10, 8, 10, 8, 14, 2, 10, 4, 2, 4, 0,
  /* 1449 */ 11, 4, 11, 4, 14, 2, 11, 4, 2, 4, 0,
  /* 1460 */ 11, 5, 11, 5, 14, 2, 11, 4, 2, 4, 0,
  /* 1471 */ 11, 7, 11, 7, 14, 2, 11, 4, 2, 4, 0,
  /* 1482 */ 11, 8, 11, 8, 14, 2, 11, 4, 2, 4, 0,
  /* 1493 */ 10, 4, 10, 4, 14, 2, 9, 5, 2, 4, 0,
  /* 1504 */ 9, 5, 9, 5, 14, 2, 9, 5, 2, 4, 0,
  /* 1515 */ 10, 7, 10, 7, 14, 2, 9, 5, 2, 4, 0,
  /* 1526 */ 9, 8, 9, 8, 14, 2, 9, 5, 2, 4, 0,
  /* 1537 */ 10, 4, 10, 4, 14, 2, 10, 5, 2, 4, 0,
  /* 1548 */ 10, 5, 10, 5, 14, 2, 10, 5, 2, 4, 0,
  /* 1559 */ 10, 7, 10, 7, 14, 2, 10, 5, 2, 4, 0,
  /* 1570 */ 10, 8, 10, 8, 14, 2, 10, 5, 2, 4, 0,
  /* 1581 */ 11, 4, 11, 4, 14, 2, 11, 5, 2, 4, 0,
  /* 1592 */ 11, 5, 11, 5, 14, 2, 11, 5, 2, 4, 0,
  /* 1603 */ 11, 7, 11, 7, 14, 2, 11, 5, 2, 4, 0,
  /* 1614 */ 11, 8, 11, 8, 14, 2, 11, 5, 2, 4, 0,
  /* 1625 */ 10, 7, 10, 7, 10, 7, 4, 10, 7, 2, 4, 0,
  /* 1637 */ 10, 7, 10, 7, 10, 7, 10, 7, 2, 4, 0,
  /* 1648 */ 10, 7, 10, 7, 9, 8, 10, 7, 2, 4, 0,
  /* 1659 */ 9, 8, 9, 8, 9, 8, 4, 9, 8, 2, 4, 0,
  /* 1671 */ 9, 8, 9, 8, 10, 7, 9, 8, 2, 4, 0,
  /* 1682 */ 9, 8, 9, 8, 9, 8, 9, 8, 2, 4, 0,
  /* 1693 */ 11, 8, 11, 8, 11, 8, 4, 11, 8, 2, 4, 0,
  /* 1705 */ 11, 8, 11, 8, 11, 8, 11, 8, 2, 4, 0,
  /* 1716 */ 12, 2, 12, 2, 12, 2, 12, 2, 4, 0,
  /* 1726 */ 21, 12, 2, 4, 12, 2, 12, 2, 12, 2, 4, 0,
  /* 1738 */ 21, 12, 2, 4, 12, 2, 12, 2, 4, 0,
  /* 1748 */ 12, 2, 9, 5, 9, 5, 12, 2, 4, 0,
  /* 1758 */ 13, 2, 13, 2, 13, 2, 13, 2, 4, 0,
  /* 1768 */ 15, 1, 15, 1, 14, 2, 14, 2, 4, 0,
  /* 1778 */ 15, 4, 15, 4, 14, 2, 14, 2, 4, 0,
  /* 1788 */ 40, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 14, 2, 4, 0,
  /* 1809 */ 23, 9, 6, 9, 6, 9, 6, 9, 6, 14, 2, 4, 0,
  /* 1822 */ 40, 7, 7, 7, 7, 7, 7, 7, 7, 14, 2, 4, 0,
  /* 1835 */ 12, 7, 12, 7, 12, 7, 12, 4, 4, 3, 4, 0,
  /* 1847 */ 12, 4, 12, 4, 14, 2, 12, 4, 3, 4, 0,
  /* 1858 */ 12, 7, 12, 7, 14, 2, 12, 4, 3, 4, 0,
  /* 1869 */ 12, 7, 12, 7, 12, 7, 4, 12, 7, 3, 4, 0,
  /* 1881 */ 12, 7, 12, 7, 12, 7, 12, 7, 3, 4, 0,
  /* 1892 */ 11, 3, 11, 3, 11, 3, 11, 3, 4, 0,
  /* 1902 */ 21, 11, 3, 4, 11, 3, 11, 3, 11, 3, 4, 0,
  /* 1914 */ 21, 11, 3, 4, 11, 3, 11, 3, 4, 0,
  /* 1924 */ 13, 3, 16, 2, 16, 2, 13, 3, 4, 0,
  /* 1934 */ 13, 3, 13, 3, 13, 3, 13, 3, 4, 0,
  /* 1944 */ 13, 3, 16, 2, 16, 2, 4, 13, 3, 4, 0,
  /* 1955 */ 13, 3, 13, 3, 13, 3, 4, 13, 3, 4, 0,
  /* 1966 */ 21, 3, 4, 0,
  /* 1970 */ 15, 3, 26, 3, 4, 0,
  /* 1976 */ 21, 4, 1, 4, 4, 0,
  /* 1982 */ 10, 7, 10, 7, 10, 7, 10, 7, 2, 4, 4, 0,
  /* 1994 */ 9, 8, 9, 8, 9, 8, 9, 8, 2, 4, 4, 0,
  /* 2006 */ 23, 15, 3, 15, 3, 15, 3, 15, 3, 15, 12, 15, 3, 15, 3, 15, 3, 15, 3, 4, 4, 0,
  /* 2028 */ 22, 15, 3, 15, 3, 15, 3, 15, 12, 15, 3, 15, 3, 15, 3, 4, 4, 0,
  /* 2046 */ 21, 15, 3, 15, 3, 15, 12, 15, 3, 15, 3, 4, 4, 0,
  /* 2060 */ 0, 5, 4, 4, 4, 4, 4, 4, 4, 0,
  /* 2070 */ 21, 4, 4, 4, 4, 4, 0,
  /* 2077 */ 23, 3, 3, 3, 3, 5, 4, 4, 4, 0,
  /* 2087 */ 21, 3, 3, 5, 4, 4, 4, 0,
  /* 2095 */ 23, 4, 4, 4, 4, 5, 4, 4, 4, 0,
  /* 2105 */ 21, 4, 4, 5, 4, 4, 4, 0,
  /* 2113 */ 23, 4, 4, 4, 4, 5, 5, 4, 4, 4, 0,
  /* 2124 */ 21, 5, 5, 5, 4, 4, 4, 0,
  /* 2132 */ 23, 7, 7, 7, 7, 5, 5, 4, 4, 4, 0,
  /* 2143 */ 23, 7, 7, 7, 7, 5, 4, 4, 4, 0,
  /* 2153 */ 16, 4, 16, 4, 16, 4, 4, 4, 0,
  /* 2162 */ 23, 3, 3, 3, 3, 5, 4, 4, 0,
  /* 2171 */ 21, 3, 3, 5, 4, 4, 0,
  /* 2178 */ 23, 4, 4, 4, 4, 5, 4, 4, 0,
  /* 2187 */ 21, 4, 4, 5, 4, 4, 0,
  /* 2194 */ 23, 4, 4, 4, 4, 5, 5, 4, 4, 0,
  /* 2204 */ 21, 5, 5, 5, 4, 4, 0,
  /* 2211 */ 23, 7, 7, 7, 7, 5, 5, 4, 4, 0,
  /* 2221 */ 23, 7, 7, 7, 7, 5, 4, 4, 0,
  /* 2230 */ 0, 14, 2, 2, 10, 4, 10, 4, 4, 0,
  /* 2240 */ 21, 10, 4, 4, 10, 4, 10, 4, 4, 0,
  /* 2250 */ 21, 10, 4, 4, 10, 4, 10, 4, 10, 4, 4, 0,
  /* 2262 */ 10, 4, 10, 4, 10, 4, 10, 4, 4, 0,
  /* 2272 */ 0, 14, 2, 2, 9, 5, 10, 4, 4, 0,
  /* 2282 */ 0, 14, 2, 2, 10, 5, 10, 4, 4, 0,
  /* 2292 */ 0, 14, 2, 2, 11, 4, 11, 4, 4, 0,
  /* 2302 */ 0, 14, 2, 2, 11, 5, 11, 4, 4, 0,
  /* 2312 */ 0, 15, 4, 15, 11, 15, 11, 4, 4, 0,
  /* 2322 */ 0, 15, 4, 15, 11, 15, 11, 15, 11, 4, 4, 0,
  /* 2334 */ 0, 15, 4, 15, 11, 15, 11, 15, 11, 15, 11, 4, 4, 0,
  /* 2348 */ 36, 1, 36, 1, 12, 4, 4, 0,
  /* 2356 */ 0, 14, 2, 3, 12, 4, 12, 4, 4, 0,
  /* 2366 */ 12, 4, 12, 4, 12, 4, 12, 4, 4, 0,
  /* 2376 */ 13, 4, 13, 4, 12, 4, 12, 4, 4, 0,
  /* 2386 */ 37, 1, 37, 1, 13, 4, 4, 0,
  /* 2394 */ 13, 4, 13, 4, 13, 4, 13, 4, 4, 0,
  /* 2404 */ 16, 4, 16, 4, 13, 4, 13, 4, 4, 0,
  /* 2414 */ 16, 4, 16, 4, 13, 4, 4, 0,
  /* 2422 */ 13, 4, 13, 4, 16, 4, 4, 0,
  /* 2430 */ 16, 4, 16, 4, 16, 4, 4, 0,
  /* 2438 */ 17, 17, 4, 4, 0,
  /* 2443 */ 15, 0, 18, 4, 4, 0,
  /* 2449 */ 21, 4, 4, 0,
  /* 2453 */ 23, 3, 3, 3, 3, 5, 4, 0,
  /* 2461 */ 21, 3, 3, 5, 4, 0,
  /* 2467 */ 23, 4, 4, 4, 4, 5, 4, 0,
  /* 2475 */ 21, 4, 4, 5, 4, 0,
  /* 2481 */ 23, 4, 4, 4, 4, 5, 5, 4, 0,
  /* 2490 */ 21, 5, 5, 5, 4, 0,
  /* 2496 */ 23, 7, 7, 7, 7, 5, 5, 4, 0,
  /* 2505 */ 23, 7, 7, 7, 7, 5, 4, 0,
  /* 2513 */ 0, 14, 2, 2, 10, 4, 9, 5, 4, 0,
  /* 2523 */ 0, 14, 2, 2, 9, 5, 9, 5, 4, 0,
  /* 2533 */ 9, 5, 9, 5, 9, 5, 9, 5, 4, 0,
  /* 2543 */ 0, 14, 2, 2, 10, 4, 10, 5, 4, 0,
  /* 2553 */ 0, 14, 2, 2, 10, 5, 10, 5, 4, 0,
  /* 2563 */ 0, 14, 2, 2, 11, 4, 11, 5, 4, 0,
  /* 2573 */ 0, 14, 2, 2, 11, 5, 11, 5, 4, 0,
  /* 2583 */ 21, 5, 4, 0,
  /* 2587 */ 0, 14, 2, 9, 6, 9, 6, 9, 6, 9, 6, 4, 0,
  /* 2600 */ 0, 14, 2, 7, 7, 7, 7, 7, 7, 7, 7, 4, 0,
  /* 2613 */ 21, 10, 4, 4, 10, 7, 4, 0,
  /* 2621 */ 0, 14, 2, 2, 10, 4, 10, 7, 4, 0,
  /* 2631 */ 0, 14, 2, 2, 9, 5, 10, 7, 4, 0,
  /* 2641 */ 0, 14, 2, 2, 10, 5, 10, 7, 4, 0,
  /* 2651 */ 0, 14, 2, 2, 11, 4, 11, 7, 4, 0,
  /* 2661 */ 0, 14, 2, 2, 11, 5, 11, 7, 4, 0,
  /* 2671 */ 0, 14, 2, 3, 12, 4, 12, 7, 4, 0,
  /* 2681 */ 21, 9, 5, 4, 9, 8, 4, 0,
  /* 2689 */ 0, 14, 2, 2, 10, 4, 9, 8, 4, 0,
  /* 2699 */ 0, 14, 2, 2, 9, 5, 9, 8, 4, 0,
  /* 2709 */ 0, 14, 2, 2, 10, 4, 10, 8, 4, 0,
  /* 2719 */ 0, 14, 2, 2, 10, 5, 10, 8, 4, 0,
  /* 2729 */ 0, 14, 2, 2, 11, 4, 11, 8, 4, 0,
  /* 2739 */ 0, 14, 2, 2, 11, 5, 11, 8, 4, 0,
  /* 2749 */ 21, 10, 4, 4, 10, 4, 0,
  /* 2756 */ 21, 11, 3, 4, 10, 4, 10, 4, 0,
  /* 2765 */ 21, 10, 4, 4, 10, 4, 10, 4, 0,
  /* 2774 */ 0, 15, 4, 15, 11, 15, 11, 15, 11, 4, 0,
  /* 2785 */ 0, 15, 4, 15, 11, 15, 11, 15, 11, 15, 11, 4, 0,
  /* 2798 */ 12, 4, 36, 1, 12, 4, 0,
  /* 2805 */ 0, 36, 1, 14, 2, 12, 4, 0,
  /* 2813 */ 0, 14, 2, 36, 1, 4, 4, 12, 4, 0,
  /* 2823 */ 36, 1, 36, 1, 12, 4, 12, 4, 0,
  /* 2832 */ 12, 4, 36, 1, 12, 4, 12, 4, 0,
  /* 2841 */ 13, 4, 36, 1, 12, 4, 12, 4, 0,
  /* 2850 */ 0, 36, 1, 4, 4, 12, 4, 12, 4, 0,
  /* 2860 */ 0, 36, 1, 4, 4, 13, 4, 12, 4, 0,
  /* 2870 */ 23, 15, 3, 15, 3, 15, 3, 15, 3, 15, 12, 4, 0,
  /* 2883 */ 22, 15, 3, 15, 3, 15, 3, 15, 12, 4, 0,
  /* 2894 */ 21, 15, 3, 15, 3, 15, 12, 4, 0,
  /* 2903 */ 13, 4, 37, 1, 13, 4, 0,
  /* 2910 */ 0, 37, 1, 14, 2, 13, 4, 0,
  /* 2918 */ 0, 14, 2, 36, 1, 4, 4, 13, 4, 0,
  /* 2928 */ 0, 14, 2, 37, 1, 4, 4, 13, 4, 0,
  /* 2938 */ 37, 1, 37, 1, 13, 4, 13, 4, 0,
  /* 2947 */ 13, 4, 37, 1, 13, 4, 13, 4, 0,
  /* 2956 */ 16, 4, 37, 1, 13, 4, 13, 4, 0,
  /* 2965 */ 0, 37, 1, 4, 4, 13, 4, 13, 4, 0,
  /* 2975 */ 16, 4, 16, 4, 13, 4, 13, 4, 0,
  /* 2984 */ 0, 4, 4, 16, 4, 13, 4, 0,
  /* 2992 */ 0, 37, 1, 4, 4, 16, 4, 13, 4, 0,
  /* 3002 */ 16, 4, 16, 4, 13, 4, 0,
  /* 3009 */ 0, 14, 20, 5, 15, 4, 0,
  /* 3016 */ 5, 19, 15, 4, 0,
  /* 3021 */ 0, 14, 2, 37, 1, 4, 4, 16, 4, 0,
  /* 3031 */ 0, 14, 2, 4, 4, 16, 4, 0,
  /* 3039 */ 13, 4, 16, 4, 0,
  /* 3044 */ 16, 4, 16, 4, 16, 4, 0,
  /* 3051 */ 4, 17, 4, 0,
  /* 3055 */ 0, 15, 4, 15, 12, 15, 17, 4, 0,
  /* 3064 */ 17, 17, 4, 0,
  /* 3068 */ 0, 14, 2, 16, 2, 5, 0,
  /* 3075 */ 16, 2, 14, 2, 16, 2, 5, 0,
  /* 3083 */ 16, 2, 16, 2, 16, 2, 16, 2, 5, 0,
  /* 3093 */ 5, 16, 2, 16, 2, 5, 0,
  /* 3100 */ 21, 5, 1, 4, 5, 0,
  /* 3106 */ 16, 4, 16, 4, 13, 4, 5, 0,
  /* 3114 */ 21, 1, 5, 5, 0,
  /* 3119 */ 21, 10, 4, 4, 9, 5, 9, 5, 0,
  /* 3128 */ 21, 9, 5, 4, 9, 5, 9, 5, 0,
  /* 3137 */ 0, 14, 2, 9, 6, 9, 6, 9, 6, 9, 6, 0,
  /* 3149 */ 23, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 0,
  /* 3199 */ 40, 7, 7, 7, 7, 7, 7, 7, 7, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 0,
  /* 3249 */ 23, 4, 4, 4, 4, 5, 4, 7, 0,
  /* 3258 */ 23, 4, 4, 4, 4, 5, 5, 4, 7, 0,
  /* 3268 */ 23, 7, 7, 7, 7, 5, 5, 4, 7, 0,
  /* 3278 */ 23, 7, 7, 7, 7, 5, 4, 7, 0,
  /* 3287 */ 23, 4, 4, 4, 4, 5, 7, 0,
  /* 3295 */ 23, 4, 4, 4, 4, 5, 5, 7, 0,
  /* 3304 */ 23, 7, 7, 7, 7, 5, 5, 7, 0,
  /* 3313 */ 23, 7, 7, 7, 7, 5, 7, 0,
  /* 3321 */ 23, 4, 4, 4, 4, 5, 4, 7, 7, 0,
  /* 3331 */ 23, 4, 4, 4, 4, 5, 5, 4, 7, 7, 0,
  /* 3342 */ 23, 7, 7, 7, 7, 5, 5, 4, 7, 7, 0,
  /* 3353 */ 23, 7, 7, 7, 7, 5, 4, 7, 7, 0,
  /* 3363 */ 23, 4, 4, 4, 4, 5, 7, 7, 0,
  /* 3372 */ 23, 4, 4, 4, 4, 5, 5, 7, 7, 0,
  /* 3382 */ 23, 7, 7, 7, 7, 5, 5, 7, 7, 0,
  /* 3392 */ 23, 7, 7, 7, 7, 5, 7, 7, 0,
  /* 3401 */ 23, 4, 4, 4, 4, 5, 4, 7, 7, 7, 0,
  /* 3412 */ 23, 4, 4, 4, 4, 5, 5, 4, 7, 7, 7, 0,
  /* 3424 */ 23, 7, 7, 7, 7, 5, 5, 4, 7, 7, 7, 0,
  /* 3436 */ 23, 7, 7, 7, 7, 5, 4, 7, 7, 7, 0,
  /* 3447 */ 23, 4, 4, 4, 4, 5, 7, 7, 7, 0,
  /* 3457 */ 23, 4, 4, 4, 4, 5, 5, 7, 7, 7, 0,
  /* 3468 */ 23, 7, 7, 7, 7, 5, 5, 7, 7, 7, 0,
  /* 3479 */ 23, 7, 7, 7, 7, 5, 7, 7, 7, 0,
  /* 3489 */ 23, 4, 4, 4, 4, 5, 4, 7, 7, 7, 7, 0,
  /* 3501 */ 23, 4, 4, 4, 4, 5, 5, 4, 7, 7, 7, 7, 0,
  /* 3514 */ 23, 7, 7, 7, 7, 5, 5, 4, 7, 7, 7, 7, 0,
  /* 3527 */ 23, 7, 7, 7, 7, 5, 4, 7, 7, 7, 7, 0,
  /* 3539 */ 23, 4, 4, 4, 4, 5, 7, 7, 7, 7, 0,
  /* 3550 */ 23, 4, 4, 4, 4, 5, 5, 7, 7, 7, 7, 0,
  /* 3562 */ 23, 7, 7, 7, 7, 5, 5, 7, 7, 7, 7, 0,
  /* 3574 */ 23, 7, 7, 7, 7, 5, 7, 7, 7, 7, 0,
  /* 3585 */ 23, 4, 4, 4, 4, 5, 4, 7, 7, 7, 7, 7, 7, 0,
  /* 3599 */ 23, 4, 4, 4, 4, 5, 5, 4, 7, 7, 7, 7, 7, 7, 0,
  /* 3614 */ 23, 7, 7, 7, 7, 5, 5, 4, 7, 7, 7, 7, 7, 7, 0,
  /* 3629 */ 23, 7, 7, 7, 7, 5, 4, 7, 7, 7, 7, 7, 7, 0,
  /* 3643 */ 23, 4, 4, 4, 4, 5, 7, 7, 7, 7, 7, 7, 0,
  /* 3656 */ 23, 4, 4, 4, 4, 5, 5, 7, 7, 7, 7, 7, 7, 0,
  /* 3670 */ 23, 7, 7, 7, 7, 5, 5, 7, 7, 7, 7, 7, 7, 0,
  /* 3684 */ 23, 7, 7, 7, 7, 5, 7, 7, 7, 7, 7, 7, 0,
  /* 3697 */ 0, 14, 2, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3709 */ 23, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3759 */ 40, 7, 7, 7, 7, 7, 7, 7, 7, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 9, 6, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3809 */ 23, 4, 4, 4, 4, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3825 */ 23, 4, 4, 4, 4, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3842 */ 23, 7, 7, 7, 7, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3859 */ 23, 7, 7, 7, 7, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0,
  /* 3875 */ 21, 10, 4, 4, 10, 7, 10, 7, 0,
  /* 3884 */ 17, 10, 7, 0,
  /* 3888 */ 21, 9, 5, 4, 9, 8, 9, 8, 0,
  /* 3897 */ 9, 8, 9, 8, 9, 8, 9, 8, 0,
  /* 3906 */ 17, 9, 8, 0,
  /* 3910 */ 10, 8, 10, 8, 10, 8, 10, 8, 0,
  /* 3919 */ 0, 15, 3, 15, 3, 15, 3, 15, 12, 0,
  /* 3929 */ 0, 15, 3, 15, 3, 15, 3, 15, 3, 15, 12, 0,
  /* 3941 */ 23, 15, 3, 15, 3, 15, 3, 15, 3, 15, 12, 0,
  /* 3953 */ 22, 15, 3, 15, 3, 15, 3, 15, 12, 0,
  /* 3963 */ 21, 15, 3, 15, 3, 15, 12, 0,
  /* 3971 */ 0, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 3980 */ 0, 15, 3, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 3991 */ 0, 15, 3, 15, 3, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 4004 */ 23, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 4025 */ 22, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 4042 */ 21, 15, 3, 15, 3, 15, 3, 15, 3, 5, 15, 12, 0,
  /* 4055 */ 4, 17, 0,
  /* 4058 */ 10, 7, 10, 7, 17, 0,
  /* 4064 */ 9, 8, 17, 0,
  /* 4068 */ 0, 14, 17, 17, 0,
  /* 4073 */ 17, 17, 17, 0,
  /* 4077 */ 15, 0, 18, 0,
  /* 4081 */ 1, 18, 0,
  /* 4084 */ 15, 4, 18, 0,
  /* 4088 */ 0, 19, 0,
  /* 4091 */ 15, 1, 19, 0,
  /* 4095 */ 1, 14, 2, 19, 0,
  /* 4100 */ 21, 14, 2, 1, 14, 2, 4, 19, 0,
  /* 4109 */ 15, 2, 15, 10, 15, 19, 0,
  /* 4116 */ 15, 2, 15, 2, 15, 2, 15, 2, 19, 19, 0,
  /* 4127 */ 15, 2, 15, 2, 4, 19, 19, 0,
  /* 4135 */ 0, 19, 19, 19, 0,
  /* 4140 */ 15, 0, 29, 0,
  /* 4144 */ 0, 1, 29, 0,
  /* 4148 */ 0, 5, 4, 14, 2, 4, 29, 0,
  /* 4156 */ 5, 5, 4, 14, 2, 4, 29, 0,
  /* 4164 */ 18, 5, 4, 15, 4, 4, 4, 29, 0,
  /* 4173 */ 0, 5, 4, 29, 0,
  /* 4178 */ 28, 35, 28, 35, 28, 35, 28, 35, 0,
  255
];
