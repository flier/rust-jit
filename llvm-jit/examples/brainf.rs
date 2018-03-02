//===-- BrainF.rs - BrainF compiler example -------------------------------===//
//
// This class compiles the BrainF language into LLVM assembly.
//
// The BrainF language has 8 commands:
// Command   Equivalent C    Action
// -------   ------------    ------
// ,         *h=getchar();   Read a character from stdin, 255 on EOF
// .         putchar(*h);    Write a character to stdout
// -         --*h;           Decrement tape
// +         ++*h;           Increment tape
// <         --h;            Move head left
// >         ++h;            Move head right
// [         while(*h) {     Start loop
// ]         }               End loop
//
//===----------------------------------------------------------------------===//
//
// This implementation has a tape of 65536 bytes,
// with the head starting in the middle.
// Range checking is off by default, so be careful.
// It can be enabled with -abc.
//
// Use:
// ./BrainF -jit      prog.bf          #Run program now
// ./BrainF -jit -abc prog.bf          #Run program now safely
// ./BrainF           prog.bf          #Write as BitCode
//
// lli prog.bf.bc                      #Run generated BitCode
//
//===----------------------------------------------------------------------===//
#[macro_use]
extern crate bitflags;
extern crate boolinator;
#[macro_use]
extern crate error_chain;
extern crate getopts;
extern crate hexplay;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::ops::{Deref, DerefMut};
use std::path::Path;

use boolinator::Boolinator;
use getopts::{Matches, Options};
use hexplay::HexViewBuilder;

use jit::insts::*;
use jit::prelude::*;
use jit::target::*;

error_chain! {
    foreign_links {
        Io(::std::io::Error);

        Opts(::getopts::Fail);
    }

    links {
        Jit(::jit::errors::Error, ::jit::errors::ErrorKind);
    }
}

bitflags! {
    struct CompileFlags : u32 {
        const FLAG_ARRAY_BOUNDS = 1;
    }
}

const TAPER_REG: &str = "tape";
const HEAD_REG: &str = "head";
const LABEL: &str = "brainf";
const TEST_REG: &str = "test";

const DEFAULT_TAPE_SIZE: usize = 65536;

/// The different symbols in the BrainF language
#[derive(Clone, Copy, Debug, PartialEq)]
enum Symbol {
    None,
    Read,
    Write,
    Move,
    Change,
    Loop,
    Endloop,
    Eof,
}

struct BrainF<'a> {
    compile_flags: CompileFlags,
    context: &'a Context,
    module: Module,
    state: State,
}

struct State {
    ptr_arr: Instruction,
    ptr_arrmax: Option<GetElementPtrInst>,
    getchar_func: Function,
    putchar_func: Function,
    brainf_func: Function,
    brainf_bb: BasicBlock,
    end_bb: BasicBlock,
    aberror_bb: Option<BasicBlock>,
    cur_head: Instruction,
}

impl<'a> Deref for BrainF<'a> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'a> DerefMut for BrainF<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<'a> BrainF<'a> {
    fn new(mem_total: usize, compile_flags: CompileFlags, context: &'a Context) -> BrainF<'a> {
        let (module, state) = Self::gen_module(mem_total, compile_flags, context);

        debug!("generated module header:\n{}", module);

        BrainF {
            compile_flags,
            context,
            module,
            state,
        }
    }

    fn parse(&mut self, code: &str) -> Result<()> {
        debug!(
            "parsing code:\n{}",
            HexViewBuilder::new(code.as_bytes()).row_width(16).finish()
        );

        let mut iter = code.trim().bytes();

        self.read_loop(&mut iter, None, None, None)?;

        debug!("generated module:\n{}", self.module);

        self.module.verify()?;

        Ok(())
    }

    fn gen_module(mem_total: usize, compile_flags: CompileFlags, context: &'a Context) -> (Module, State) {
        let module = context.create_module("BrainF");

        let void_t = context.void_t();
        let i1_t = context.int1_t();
        let i8_t = context.int8_t();
        let i32_t = context.int32_t();

        //Function prototypes

        //declare void @llvm.memset.p0i8.i32(i8 *, i8, i32, i32, i1)
        let memset_func = module.get_or_insert_function(
            "llvm.memset.p0i8.i32",
            void_t,
            types![i8_t.ptr_t(), i8_t, i32_t, i32_t, i1_t],
        );

        //declare i32 @getchar()
        let getchar_func = module.get_or_insert_function("getchar", i32_t, types![]);

        //declare i32 @putchar(i32)
        let putchar_func = module.get_or_insert_function("putchar", i32_t, types![i32_t]);

        //Function header

        //define void @brainf()
        let brainf_func = module.get_or_insert_function("brainf", void_t, types![]);

        let brainf_bb = brainf_func.append_basic_block_in_context(LABEL, &context);

        let mut builder = context.create_builder();

        builder.position_at_end(brainf_bb);

        //%arr = malloc i8, i32 %d
        let ptr_arr = malloc!(i8_t, i32_t.int(mem_total as i64); "arr").emit_to(&builder);

        //call void @llvm.memset.p0i8.i32(i8 *%arr, i8 0, i32 %d, i32 1, i1 0)
        builder <<= call!(
            memset_func,
            ptr_arr,
            i8_t.int(0),
            i32_t.int(mem_total as i64),
            i32_t.int(1),
            i1_t.int(0)
        ).set_tail_call(false);

        //%arrmax = getelementptr i8 *%arr, i32 %d
        let ptr_arrmax = compile_flags
            .contains(CompileFlags::FLAG_ARRAY_BOUNDS)
            .as_option()
            .map(|_| gep!(ptr_arr, i32_t.int(mem_total as i64); "arrmax").emit_to(&builder));

        //%head.%d = getelementptr i8 *%arr, i32 %d
        let cur_head = gep!(ptr_arr, i32_t.int(mem_total as i64 / 2); HEAD_REG).emit_to(&builder);

        //Function footer

        //brainf.end:
        let end_bb = brainf_func.append_basic_block_in_context(format!("{}.end", LABEL), &context);

        //call free(i8 *%arr)
        builder.position_at_end(end_bb);

        builder <<= free!(ptr_arr);

        //ret void
        builder <<= ret!();

        //Error block for array out of bounds
        let aberror_bb = compile_flags
            .contains(CompileFlags::FLAG_ARRAY_BOUNDS)
            .as_some({
                //@aberrormsg = internal constant [%d x i8] c"\00"
                let msg = context.str("Error: The head has left the tape.");

                let aberror_msg = module.add_global_var("aberrormsg", msg.type_of());

                aberror_msg.set_linkage(llvm::LLVMLinkage::LLVMInternalLinkage);
                aberror_msg.set_initializer(msg);

                //declare i32 @puts(i8 *)
                let puts_func = module.get_or_insert_function("puts", i32_t, types![i8_t.ptr_t()]);

                //brainf.aberror:
                let aberror_bb = brainf_func.append_basic_block_in_context(format!("{}.aberror", LABEL), &context);

                builder.position_at_end(aberror_bb);

                //call i32 @puts(i8 *getelementptr([%d x i8] *@aberrormsg, i32 0, i32 0))
                let msg_ptr = gep!(aberror_msg, i32_t.null(), i32_t.null()).emit_to(&builder);

                builder <<= call!(puts_func, msg_ptr).set_tail_call(false);

                builder <<= br!(end_bb);

                aberror_bb
            });

        (
            module,
            State {
                ptr_arr,
                ptr_arrmax,
                getchar_func,
                putchar_func,
                brainf_func,
                brainf_bb,
                end_bb,
                aberror_bb,
                cur_head: cur_head.into(),
            },
        )
    }

    fn read_loop<I: Iterator<Item = u8>>(
        &mut self,
        iter: &mut I,
        phi: Option<PhiNode>,
        old_bb: Option<BasicBlock>,
        test_bb: Option<BasicBlock>,
    ) -> Result<()> {
        let i8_t = self.context.int8_t();
        let i32_t = self.context.int32_t();

        let mut cur_sym = Symbol::None;
        let mut next_sym = Symbol::None;
        let mut cur_value = 0;
        let mut next_value = 0;
        let mut line = 0;
        let mut column = 0;

        let mut builder = self.context.create_builder();

        builder.position_at_end(self.brainf_bb);

        while ![Symbol::Eof, Symbol::Endloop].contains(&cur_sym) {
            trace!("generate code for {:?}", cur_sym);

            // Write out commands
            match cur_sym {
                Symbol::None => {
                    // Do nothing
                }
                Symbol::Read => {
                    //%tape.%d = call i32 @getchar()
                    let tape_0 = call!(self.getchar_func; TAPER_REG).set_tail_call(false);

                    //%tape.%d = trunc i32 %tape.%d to i8
                    let tape_1 = trunc!(tape_0, i8_t; TAPER_REG);

                    //store i8 %tape.%d, i8 *%head.%d
                    builder <<= store!(tape_1, self.cur_head);
                }
                Symbol::Write => {
                    //%tape.%d = load i8 *%head.%d
                    let tape_0 = load!(self.cur_head; TAPER_REG);

                    //%tape.%d = sext i8 %tape.%d to i32
                    let tape_1 = sext!(tape_0, i32_t; TAPER_REG).emit_to(&builder);

                    //call i32 @putchar(i32 %tape.%d)
                    builder <<= call!(self.putchar_func, tape_1).set_tail_call(false);
                }
                Symbol::Move => {
                    //%head.%d = getelementptr i8 *%head.%d, i32 %d
                    self.cur_head = gep!(self.cur_head, i32_t.int(cur_value); HEAD_REG)
                        .emit_to(&builder)
                        .into();

                    if self.compile_flags.contains(CompileFlags::FLAG_ARRAY_BOUNDS) {
                        trace!("checking array bounds");

                        //%test.%d = icmp uge i8 *%head.%d, %arrmax
                        let test_0 = icmp!(UGE self.cur_head, self.ptr_arrmax.unwrap(); TEST_REG);

                        //%test.%d = icmp ult i8 *%head.%d, %arr
                        let test_1 = icmp!(ULT self.cur_head, self.ptr_arr; TEST_REG);

                        //%test.%d = or i1 %test.%d, %test.%d
                        let test_2 = or!(test_0, test_1; TEST_REG);

                        //br i1 %test.%d, label %main.%d, label %main.%d
                        let next_bb = self.brainf_func
                            .append_basic_block_in_context(LABEL, &self.context);

                        builder <<= br!(test_2 => self.aberror_bb.unwrap(), _ => next_bb);

                        //main.%d:
                        builder.position_at_end(next_bb);
                    }
                }
                Symbol::Change => {
                    //%tape.%d = load i8 *%head.%d
                    let tape_0 = load!(self.cur_head; TAPER_REG);

                    //%tape.%d = add i8 %tape.%d, %d
                    let tape_1 = add!(tape_0, i8_t.int(cur_value); TAPER_REG);

                    //store i8 %tape.%d, i8 *%head.%d\n"
                    builder <<= store!(tape_1, self.cur_head);
                }
                Symbol::Loop => {
                    //br label %main.%d
                    let test_bb = self.brainf_func
                        .append_basic_block_in_context(LABEL, &self.context);

                    builder <<= br!(test_bb);

                    //main.%d:
                    let bb_0 = builder.insert_block().unwrap();
                    let bb_1 = self.brainf_func
                        .append_basic_block_in_context(LABEL, &self.context);
                    builder.position_at_end(bb_1);

                    // Make part of PHI instruction now, wait until end of loop to finish
                    let phi_0 = phi!(i8_t.ptr_t(), self.cur_head => bb_0; HEAD_REG).emit_to(&builder);

                    self.cur_head = phi_0.into();

                    self.read_loop(iter, Some(phi_0), Some(bb_1), Some(test_bb))?;
                }
                _ => {
                    bail!("Error: Unknown symbol: {:?}", cur_sym);
                }
            }

            cur_sym = next_sym;
            cur_value = next_value;
            next_sym = Symbol::None;

            let mut terminated = ![Symbol::None, Symbol::Move, Symbol::Change].contains(&cur_sym);

            while !terminated {
                if let Some(c) = iter.next() {
                    column += 1;

                    match c {
                        b'+' | b'-' => {
                            let direction = if c == b'+' { 1 } else { -1 };

                            match cur_sym {
                                Symbol::Change => {
                                    cur_value += direction;
                                }
                                Symbol::None => {
                                    cur_sym = Symbol::Change;
                                    cur_value = direction;
                                }
                                _ => {
                                    next_sym = Symbol::Change;
                                    next_value = direction;
                                    terminated = true;
                                }
                            }
                        }
                        b'<' | b'>' => {
                            let direction = if c == b'>' { 1 } else { -1 };

                            match cur_sym {
                                Symbol::Move => {
                                    cur_value += direction;
                                }
                                Symbol::None => {
                                    cur_sym = Symbol::Move;
                                    cur_value = direction;
                                }
                                _ => {
                                    next_sym = Symbol::Move;
                                    next_value = direction;
                                    terminated = true;
                                }
                            }
                        }
                        b',' => {
                            if cur_sym == Symbol::None {
                                cur_sym = Symbol::Read
                            } else {
                                next_sym = Symbol::Read
                            }
                            terminated = true;
                        }
                        b'.' => {
                            if cur_sym == Symbol::None {
                                cur_sym = Symbol::Write
                            } else {
                                next_sym = Symbol::Write
                            }
                            terminated = true;
                        }
                        b'[' => {
                            if cur_sym == Symbol::None {
                                cur_sym = Symbol::Loop
                            } else {
                                next_sym = Symbol::Loop
                            }
                            terminated = true;
                        }
                        b']' => {
                            if cur_sym == Symbol::None {
                                cur_sym = Symbol::Endloop
                            } else {
                                next_sym = Symbol::Endloop
                            }
                            terminated = true;
                        }
                        b'\n' => {
                            line += 1;
                            column = 0;
                        }
                        _ => {
                            // trace!("Ignore other characters: `{}`", c)
                        }
                    }
                } else {
                    if cur_sym == Symbol::None {
                        cur_sym = Symbol::Eof
                    } else {
                        next_sym = Symbol::Eof
                    }
                    terminated = true;
                }
            }
        }

        if cur_sym == Symbol::Endloop {
            if phi.is_none() {
                bail!("Error: Extra ']'");
            }

            trace!("generate code for {:?}", cur_sym);

            // Write loop test

            //br label %main.%d
            builder <<= br!(test_bb.unwrap());

            //main.%d:

            //%head.%d = phi i8 *[%head.%d, %main.%d], [%head.%d, %main.%d]
            //Finish phi made at beginning of loop
            phi.unwrap()
                .add_incoming(self.cur_head, builder.insert_block().unwrap());

            let head_0 = phi;

            //%tape.%d = load i8 *%head.%d
            let tape_0 = load!(head_0.unwrap(); TAPER_REG);

            //%test.%d = icmp eq i8 %tape.%d, 0
            let test_0 = icmp!(EQ tape_0, i8_t.int(0); TEST_REG);

            //br i1 %test.%d, label %main.%d, label %main.%d
            let bb_0 = self.brainf_func
                .append_basic_block_in_context(LABEL, &self.context);

            builder.position_at_end(test_bb.unwrap());

            builder <<= br!(test_0 => bb_0, _ => old_bb.unwrap());

            //main.%d:
            builder.position_at_end(bb_0);

            //%head.%d = phi i8 *[%head.%d, %main.%d]
            let phi_1 = phi!(i8_t.ptr_t(), head_0.unwrap() => test_bb.unwrap()).emit_to(&builder);

            self.cur_head = phi_1.into();
        } else {
            if phi.is_some() {
                bail!("Error: Missing ']'")
            }

            //End of the program, so go to return block
            builder <<= br!(self.end_bb);
        }

        Ok(())
    }
}

fn parse_cmdline(program: &str, args: &[String]) -> Result<Option<Matches>> {
    let mut opts = Options::new();

    opts.optopt("o", "output", "output filename", "FILE");
    opts.optflag("", "abc", "enable array bounds checking");
    opts.optflag("", "jit", "run program Just-In-Time");
    opts.optopt(
        "",
        "tape-size",
        format!("tape size (default: {})", DEFAULT_TAPE_SIZE).as_str(),
        "SIZE",
    );
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(args)?;

    if matches.opt_present("h") {
        let brief = format!("Usage: {} FILE [options]", program);

        print!("{}", opts.usage(&brief));

        Ok(None)
    } else if matches.free.is_empty() {
        bail!("Error: You must specify the filename of the program to be compiled.  Use --help to see the options.");
    } else {
        Ok(Some(matches))
    }
}

fn main() {
    pretty_env_logger::init();

    let args: Vec<String> = env::args().collect();
    let (program, args) = args.split_first().unwrap();

    if let Some(opts) = parse_cmdline(program, args).unwrap() {
        let (input_filename, _input_files) = opts.free.split_first().unwrap();

        let mut code = String::new();

        if input_filename == "-" {
            debug!("read input from STDIN");

            let stdin = io::stdin();
            let mut handle = stdin.lock();

            handle.read_to_string(&mut code).unwrap();
        } else {
            debug!("read input from file: {}", input_filename);

            File::open(input_filename)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();
        }

        let tape_size = opts.opt_str("tape-size")
            .map_or(DEFAULT_TAPE_SIZE, |s| s.parse().unwrap());

        //Gather the compile flags
        let compile_flags = if opts.opt_present("abc") {
            debug!("array bounds checking enabled");

            CompileFlags::FLAG_ARRAY_BOUNDS
        } else {
            CompileFlags::empty()
        };

        //Read the BrainF program
        let context = Context::new();
        let mut brainf = BrainF::new(tape_size, compile_flags, &context);

        brainf.parse(&code).unwrap();

        let module = brainf.module;

        //Write it out
        if opts.opt_present("jit") {
            NativeTarget::init().unwrap();
            NativeAsmPrinter::init().unwrap();

            println!("------- Running JIT -------");

            let brainf_func = module.get_function("brainf").unwrap();

            let ee = ExecutionEngine::for_module(module).unwrap();

            let _gv = ee.run_function(&brainf_func, vec![]);
        } else {
            //Get the output stream
            let output_filename = opts.opt_str("o").map_or_else(
                || {
                    // Use default filename.
                    Path::new(if input_filename == "-" {
                        "a"
                    } else {
                        input_filename.as_str()
                    }).with_extension("bc")
                },
                |filename| Path::new(filename.as_str()).to_owned(),
            );

            debug!("write output to file: {:?}", output_filename);

            module.write_bitcode(output_filename).unwrap();
        }

        jit::shutdown();
    }
}
