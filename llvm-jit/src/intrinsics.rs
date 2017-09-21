use llvm::core::*;

use boolinator::Boolinator;

use function::Function;
use module::Module;
use utils::AsRaw;

pub type Intrinsic = Function;
pub type IntrinsicId = u32;

pub const NOT_INTRINSIC: IntrinsicId = 0; // Must be zero

impl Function {
    pub fn intrinsic_id(&self) -> Option<IntrinsicId> {
        let id = unsafe { LLVMGetIntrinsicID(self.as_raw()) };

        (id != NOT_INTRINSIC).as_some(id)
    }
}

pub trait Intrinsics {
    //===--------------- Variable Argument Handling Intrinsics ----------------===//

    /// The `llvm.va_start` intrinsic initializes *<arglist> for subsequent use by va_arg.
    ///
    /// ```
    /// declare void @llvm.va_start(i8* <arglist>)
    /// ```
    fn va_start(&self) -> Intrinsic;

    /// The `llvm.va_copy` intrinsic copies the current argument position
    /// from the source argument list to the destination argument list.
    ///
    /// ```
    /// declare void @llvm.va_end(i8* <arglist>)
    /// ```
    fn va_copy(&self) -> Intrinsic;

    /// The `llvm.va_end` intrinsic destroys *<arglist>,
    /// which has been initialized previously with llvm.va_start or llvm.va_copy.
    ///
    /// ```
    /// declare void @llvm.va_copy(i8* <destarglist>, i8* <srcarglist>)
    /// ```
    fn va_end(&self) -> Intrinsic;

    //===------------------- Garbage Collection Intrinsics --------------------===//

    /// The `llvm.gcroot` intrinsic declares the existence of a GC root to the code generator,
    /// and allows some metadata to be associated with it.
    ///
    /// ```
    /// declare void @llvm.gcroot(i8** %ptrloc, i8* %metadata)
    /// ```
    fn gc_root(&self) -> Intrinsic;

    /// The `llvm.gcread` intrinsic identifies reads of references from heap locations,
    /// allowing garbage collector implementations that require read barriers.
    ///
    /// ```
    /// declare i8* @llvm.gcread(i8* %ObjPtr, i8** %Ptr)
    /// ```
    fn gc_read(&self) -> Intrinsic;

    /// The `llvm.gcwrite` intrinsic identifies writes of references to heap locations,
    /// allowing garbage collector implementations that require write barriers
    /// (such as generational or reference counting collectors).
    ///
    /// ```
    /// declare void @llvm.gcwrite(i8* %P1, i8* %Obj, i8** %P2)
    /// ```
    fn gc_write(&self) -> Intrinsic;

    //===--------------------- Code Generator Intrinsics ----------------------===//

    /// The `llvm.returnaddress` intrinsic attempts to compute a target-specific value
    /// indicating the return address of the current function or one of its callers.
    ///
    /// ```
    /// declare i8  *@llvm.returnaddress(i32 <level>)
    /// ```
    fn return_address(&self) -> Intrinsic;

    /// The `llvm.addressofreturnaddress` intrinsic returns a target-specific pointer
    /// to the place in the stack frame where the return address of the current function is stored.
    ///
    /// ```
    /// declare i8  *@llvm.addressofreturnaddress()
    /// ```
    fn address_of_return_address(&self) -> Intrinsic;

    /// The `llvm.frameaddress` intrinsic attempts to return the target-specific frame pointer value
    /// for the specified stack frame.
    ///
    /// ```
    /// declare i8* @llvm.frameaddress(i32 <level>)
    /// ```
    fn frame_address(&self) -> Intrinsic;

    /// The `llvm.localaddress gets the address of the local variable area.
    /// This is typically a copy of the stack, frame, or base pointer depending on the type of prologue.
    ///
    /// ```
    /// declare i8* @llvm.localaddress()
    /// ```
    fn local_address(&self) -> Intrinsic;

    /// The `llvm.localescape` intrinsic escapes offsets of a collection of static allocas.
    ///
    /// ```
    /// declare void @llvm.localescape(...)
    /// ```
    fn local_escape(&self) -> Intrinsic;

    /// The `llvm.localrecover` intrinsic applies those offsets to a live frame pointer
    /// to recover the address of the allocation.
    /// The offset is computed during frame layout of the caller of llvm.localescape.
    ///
    /// ```
    /// declare i8* @llvm.localrecover(i8* %func, i8* %fp, i32 %idx)
    /// ```
    fn local_recover(&self) -> Intrinsic;

    /// The `llvm.read_register` intrinsics provides access to the named register.
    /// The register must be valid on the architecture being compiled to.
    /// The type needs to be compatible with the register being read.
    ///
    /// ```
    /// declare i32 @llvm.read_register.i32(metadata)
    /// ```
    fn read_register_i32(&self) -> Intrinsic;

    /// The `llvm.read_register` intrinsics provides access to the named register.
    /// The register must be valid on the architecture being compiled to.
    /// The type needs to be compatible with the register being read.
    ///
    /// ```
    /// declare i64 @llvm.read_register.i64(metadata)
    /// ```
    fn read_register_i64(&self) -> Intrinsic;

    /// `llvm.write_register` intrinsics provides access to the named register.
    /// The register must be valid on the architecture being compiled to.
    /// The type needs to be compatible with the register being read.
    ///
    /// ```
    /// declare void @llvm.write_register.i32(metadata, i32 @value)
    /// ```
    fn write_register_i32(&self) -> Intrinsic;

    /// `llvm.write_register` intrinsics provides access to the named register.
    /// The register must be valid on the architecture being compiled to.
    /// The type needs to be compatible with the register being read.
    ///
    /// ```
    /// declare void @llvm.write_register.i64(metadata, i64 @value)
    /// ```
    fn write_register_i64(&self) -> Intrinsic;

    /// The `llvm.stacksave` intrinsic is used to remember the current state of the function stack,
    /// for use with `llvm.stackrestore`.
    ///
    /// This is useful for implementing language features like scoped automatic variable sized arrays in C99.
    ///
    /// ```
    /// declare i8* @llvm.stacksave()
    /// ```
    fn stack_save(&self) -> Intrinsic;

    /// The `llvm.stackrestore` intrinsic is used to restore the state of the function stack
    /// to the state it was in when the corresponding `llvm.stacksave` intrinsic executed.
    ///
    /// This is useful for implementing language features like scoped automatic variable sized arrays in C99.
    ///
    /// ```
    /// declare void @llvm.stackrestore(i8* %ptr)
    /// ```
    fn stack_restore(&self) -> Intrinsic;

    /// The `llvm.get.dynamic.area.offset.*` intrinsic family is used to get the offset from native stack pointer
    /// to the address of the most recent dynamic alloca on the caller’s stack.
    ///
    /// These intrinsics are intendend for use in combination with `llvm.stacksave` to get a pointer
    /// to the most recent dynamic alloca.
    ///
    /// This is useful, for example, for AddressSanitizer’s stack unpoisoning routines.
    ///
    /// ```
    /// declare i32 @llvm.get.dynamic.area.offset.i32()
    /// ```
    fn get_dynamic_area_offset_i32(&self) -> Intrinsic;

    /// The `llvm.get.dynamic.area.offset.*` intrinsic family is used to get the offset from native stack pointer
    /// to the address of the most recent dynamic alloca on the caller’s stack.
    ///
    /// These intrinsics are intendend for use in combination with `llvm.stacksave` to get a pointer
    /// to the most recent dynamic alloca.
    ///
    /// This is useful, for example, for AddressSanitizer’s stack unpoisoning routines.
    ///
    /// ```
    /// declare i64 @llvm.get.dynamic.area.offset.i64()
    /// ```
    fn get_dynamic_area_offset_i64(&self) -> Intrinsic;

    /// The `llvm.thread.pointer` intrinsic returns the value of the thread pointer.
    ///
    /// ```
    /// declare i8* @llvm.thread.pointer()
    /// ```
    fn thread_pointer(&self) -> Intrinsic;

    /// The `llvm.prefetch` intrinsic is a hint to the code generator to insert a prefetch instruction if supported;
    /// otherwise, it is a noop.
    ///
    /// Prefetches have no effect on the behavior of the program but can change its performance characteristics.
    ///
    /// ```
    /// declare void @llvm.prefetch(i8* <address>, i32 <rw>, i32 <locality>, i32 <cache type>)
    /// ```
    fn prefetch(&self) -> Intrinsic;

    /// The `llvm.pcmarker` intrinsic is a method to export a Program Counter (PC) in a region of code
    /// to simulators and other tools.
    ///
    /// The method is target specific, but it is expected that the marker will use exported symbols
    /// to transmit the PC of the marker. The marker makes no guarantees that it will remain
    /// with any specific instruction after optimizations.
    /// It is possible that the presence of a marker will inhibit optimizations.
    /// The intended use is to be inserted after optimizations to allow correlations of simulation runs.
    ///
    /// ```
    /// declare void @llvm.pcmarker(i32 <id>)
    /// ```
    fn pc_marker(&self) -> Intrinsic;

    /// The `llvm.readcyclecounter` intrinsic provides access to the cycle counter register (or similar low latency,
    /// high accuracy clocks) on those targets that support it.
    ///
    /// On X86, it should map to RDTSC. On Alpha, it should map to RPCC.
    /// As the backing counters overflow quickly (on the order of 9 seconds on alpha),
    /// this should only be used for small timings.
    ///
    /// ```
    /// declare i64 @llvm.readcyclecounter()
    /// ```
    fn read_cycle_counter(&self) -> Intrinsic;

    /// The llvm.stackprotector intrinsic takes the guard and stores it onto the stack at slot.
    /// The stack slot is adjusted to ensure that it is placed on the stack before local variables.
    ///
    /// ```
    /// declare void @llvm.stackprotector(i8* <guard>, i8** <slot>)
    /// ```
    fn stack_protector(&self) -> Intrinsic;

    /// The llvm.stackguard intrinsic returns the system stack guard value.
    ///
    /// It should not be generated by frontends, since it is only for internal usage.
    /// The reason why we create this intrinsic is that we still support IR form Stack Protector in FastISel.
    ///
    /// ```
    /// declare i8* @llvm.stackguard()
    /// ```
    fn stack_guard(&self) -> Intrinsic;

    /// The `llvm.instrprof_increment` intrinsic can be emitted by a frontend for use
    /// with instrumentation based profiling.
    ///
    /// These will be lowered by the -instrprof pass to generate execution counts of a program at runtime.
    ///
    /// ```
    /// declare void @llvm.instrprof_increment(i8* <name>, i64 <hash>, i32 <num-counters>, i32 <index>)
    /// ```
    fn instrprof_increment(&self) -> Intrinsic;

    /// The `llvm.instrprof_increment_step` intrinsic is an extension to the `llvm.instrprof_increment` intrinsic
    /// with an additional fifth argument to specify the step of the increment.
    ///
    /// ```
    /// declare void @llvm.instrprof_increment_step(i8* <name>, i64 <hash>, i32 <num-counters>, i32 <index>, i64 <step>)
    /// ```
    fn instrprof_step(&self) -> Intrinsic;

    /// The `llvm.instrprof_value_profile` intrinsic can be emitted by a frontend for use
    /// with instrumentation based profiling.
    ///
    /// This will be lowered by the -instrprof pass to find out the target values,
    /// instrumented expressions take in a program at runtime.
    ///
    /// ```
    /// declare void @llvm.instrprof_value_profile(i8* <name>, i64 <hash>, i64 <value>, i32 <value_kind>, i32 <index>)
    /// ```
    fn instrprof_value_profile(&self) -> Intrinsic;

    /// This intrinsic returns the type info index in the exception table of the current function.
    ///
    /// This value can be used to compare against the result of landingpad instruction.
    /// The single argument is a reference to a type info.
    ///
    /// ```
    /// i32 @llvm.eh.typeid.for(i8* %type_info)
    /// ```
    fn eh_typeid_for(&self) -> Intrinsic;

    fn eh_return_i32(&self) -> Intrinsic;

    fn eh_return_i64(&self) -> Intrinsic;
}

impl Intrinsics for Module {
    fn va_start(&self) -> Intrinsic {
        self.get_function("llvm.va_start").unwrap()
    }

    fn va_copy(&self) -> Intrinsic {
        self.get_function("llvm.va_copy").unwrap()
    }

    fn va_end(&self) -> Intrinsic {
        self.get_function("llvm.va_end").unwrap()
    }

    fn gc_root(&self) -> Intrinsic {
        self.get_function("llvm.gcroot").unwrap()
    }

    fn gc_read(&self) -> Intrinsic {
        self.get_function("llvm.gcread").unwrap()
    }

    fn gc_write(&self) -> Intrinsic {
        self.get_function("llvm.gcwrite").unwrap()
    }

    fn return_address(&self) -> Intrinsic {
        self.get_function("llvm.returnaddress").unwrap()
    }

    fn address_of_return_address(&self) -> Intrinsic {
        self.get_function("llvm.addressofreturnaddress").unwrap()
    }

    fn frame_address(&self) -> Intrinsic {
        self.get_function("llvm.frameaddress").unwrap()
    }

    fn local_address(&self) -> Intrinsic {
        self.get_function("llvm.localaddress").unwrap()
    }

    fn local_escape(&self) -> Intrinsic {
        self.get_function("llvm.localescape").unwrap()
    }

    fn local_recover(&self) -> Intrinsic {
        self.get_function("llvm.localrecover").unwrap()
    }

    fn read_register_i32(&self) -> Intrinsic {
        self.get_function("llvm.read_register.i32").unwrap()
    }

    fn read_register_i64(&self) -> Intrinsic {
        self.get_function("llvm.read_register.i64").unwrap()
    }

    fn write_register_i32(&self) -> Intrinsic {
        self.get_function("llvm.write_register.i32").unwrap()
    }

    fn write_register_i64(&self) -> Intrinsic {
        self.get_function("llvm.write_register.i64").unwrap()
    }

    fn stack_save(&self) -> Intrinsic {
        self.get_function("llvm.localrecover").unwrap()
    }

    fn stack_restore(&self) -> Intrinsic {
        self.get_function("llvm.stackrestore").unwrap()
    }

    fn get_dynamic_area_offset_i32(&self) -> Intrinsic {
        self.get_function("llvm.get.dynamic.area.offset.i32")
            .unwrap()
    }

    fn get_dynamic_area_offset_i64(&self) -> Intrinsic {
        self.get_function("llvm.get.dynamic.area.offset.i64")
            .unwrap()
    }

    fn thread_pointer(&self) -> Intrinsic {
        self.get_function("llvm.thread.pointer").unwrap()
    }

    fn prefetch(&self) -> Intrinsic {
        self.get_function("llvm.prefetch").unwrap()
    }

    fn pc_marker(&self) -> Intrinsic {
        self.get_function("llvm.pcmarker").unwrap()
    }

    fn read_cycle_counter(&self) -> Intrinsic {
        self.get_function("llvm.readcyclecounter").unwrap()
    }

    fn stack_protector(&self) -> Intrinsic {
        self.get_function("llvm.stackprotector").unwrap()
    }

    fn stack_guard(&self) -> Intrinsic {
        self.get_function("llvm.stackguard").unwrap()
    }

    fn instrprof_increment(&self) -> Intrinsic {
        self.get_function("llvm.instrprof_increment").unwrap()
    }

    fn instrprof_step(&self) -> Intrinsic {
        self.get_function("llvm.instrprof_increment_step").unwrap()
    }

    fn instrprof_value_profile(&self) -> Intrinsic {
        self.get_function("llvm.instrprof_value_profile").unwrap()
    }

    fn eh_typeid_for(&self) -> Intrinsic {
        self.get_function("llvm.eh.typeid.fo").unwrap()
    }

    fn eh_return_i32(&self) -> Intrinsic {
        self.get_function("llvm.eh.return.i32").unwrap()
    }

    fn eh_return_i64(&self) -> Intrinsic {
        self.get_function("llvm.eh.return.i64").unwrap()
    }
}
