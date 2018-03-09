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

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token {
    Move(i64),
    Change(i64),
    Write,
    Read,
    Loop,
    EndLoop,
    Skip(u8),
}

struct Lexer<I>(I);

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = u8>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|b| match b {
            b'>' => Token::Move(1),
            b'<' => Token::Move(-1),
            b'+' => Token::Change(1),
            b'-' => Token::Change(-1),
            b'.' => Token::Write,
            b',' => Token::Read,
            b'[' => Token::Loop,
            b']' => Token::EndLoop,
            _ => Token::Skip(b),
        })
    }
}

struct BrainF<'a> {
    compile_flags: CompileFlags,
    context: &'a Context,
    module: Module,
    state: State,
    builder: IRBuilder,
}

struct State {
    ptr_arr: Instruction,
    ptr_arrmax: Option<GetElementPtrInst>,
    getchar_func: Function,
    putchar_func: Function,
    brainf_func: Function,
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
        let (module, builder, state) = Self::header(mem_total, compile_flags, context);

        debug!("generated module header:\n{}", module);

        BrainF {
            compile_flags,
            context,
            module,
            state,
            builder,
        }
    }

    fn parse(&mut self, code: &str) -> Result<()> {
        trace!("parsing code:\n{}", code);

        let mut lexer = Lexer(code.trim().bytes());

        self.read_loop(&mut lexer, None, None, None)?;

        debug!("generated module:\n{}", self.module);

        self.module.verify()?;

        Ok(())
    }

    fn header(mem_total: usize, compile_flags: CompileFlags, context: &'a Context) -> (Module, IRBuilder, State) {
        let module = context.create_module("BrainF");

        let void_t = context.void_t();
        let bool_t = context.int1_t();
        let i8_t = context.int8_t();
        let i32_t = context.int32_t();

        //Function prototypes

        //declare void @llvm.memset.p0i8.i32(i8 *, i8, i32, i32, i1)
        let memset_func = module.intrinsic_declaration(IntrinsicId::memset, types![i8_t.ptr_t(), i32_t]);

        //declare i32 @getchar()
        let getchar_func = module.get_or_insert_function("getchar", i32_t, types![]);

        //declare i32 @putchar(i32)
        let putchar_func = module.get_or_insert_function("putchar", i32_t, types![i32_t]);

        //Function header

        //define void @brainf()
        let brainf_func = module.get_or_insert_function("brainf", void_t, types![]);

        let entry_bb = brainf_func.append_basic_block_in_context(format!("{}.entry", LABEL), &context);

        let mut builder = context.create_builder();

        builder.position_at_end(entry_bb);

        //%arr = malloc i8, i32 %d
        let ptr_arr = malloc!(i8_t, i32_t.int(mem_total as i64); "arr").emit_to(&builder);

        //call void @llvm.memset.p0i8.i32(i8 *%arr, i8 0, i32 %d, i32 1, i1 0)
        builder <<= call!(
            memset_func,
            ptr_arr,
            i8_t.int(0),
            i32_t.int(mem_total as i64),
            i32_t.int(1),
            bool_t.int(0)
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
        let aberror_bb = if compile_flags.contains(CompileFlags::FLAG_ARRAY_BOUNDS) {
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
            let msg_ptr = gep!(aberror_msg, i32_t.null(), i32_t.null());

            builder <<= call!(puts_func, msg_ptr).set_tail_call(false);
            builder <<= br!(end_bb);

            Some(aberror_bb)
        } else {
            None
        };

        builder.position_at_end(entry_bb);

        (
            module,
            builder,
            State {
                ptr_arr,
                ptr_arrmax,
                getchar_func,
                putchar_func,
                brainf_func,
                end_bb,
                aberror_bb,
                cur_head: cur_head.into(),
            },
        )
    }

    fn read_loop<I: Iterator<Item = Token>>(
        &mut self,
        tokens: &mut I,
        phi: Option<Phi<'a>>,
        old_bb: Option<BasicBlock>,
        test_bb: Option<BasicBlock>,
    ) -> Result<()> {
        let mut line = 0;
        let mut column = 0;
        let mut cur_loc = (line, column);
        let mut cur_tok = None;
        let mut next_tok = None;

        while cur_tok != Some(Token::EndLoop) {
            if let Some(token) = cur_tok {
                trace!("generate code for {:?} @ {:?}", token, cur_loc);

                // Write out commands
                match token {
                    Token::Read => self.emit_read(),
                    Token::Write => self.emit_write(),
                    Token::Move(steps) => self.cur_head = self.emit_move(steps),
                    Token::Change(delta) => self.emit_change(delta),
                    Token::Loop => {
                        let (phi, old_bb, test_bb) = self.emit_loop();

                        self.read_loop(tokens, Some(phi), Some(old_bb), Some(test_bb))?;
                    }
                    _ => {
                        bail!("Error: Unknown token: {:?}", token);
                    }
                }
            }

            cur_tok = next_tok.take();

            let need_more = |token| match token {
                None | Some(Token::Move(_)) | Some(Token::Change(_)) => true,
                _ => false,
            };

            while need_more(cur_tok) {
                column += 1;
                cur_loc = (line, column);

                next_tok = tokens.next();

                match (cur_tok, next_tok) {
                    (Some(Token::Move(cur_steps)), Some(Token::Move(next_steps))) => {
                        trace!("merge {:?} with {:?} @ {:?}", cur_tok, next_tok, cur_loc);

                        cur_tok = Some(Token::Move(cur_steps + next_steps))
                    }
                    (Some(Token::Change(cur_value)), Some(Token::Change(next_value))) => {
                        trace!("merge {:?} with {:?} @ {:?}", cur_tok, next_tok, cur_loc);

                        cur_tok = Some(Token::Change(cur_value + next_value))
                    }
                    (_, Some(Token::Skip(b))) => {
                        if b == b'\n' {
                            line += 1;
                            column = 0;
                        }
                    }
                    (_, token) => {
                        trace!("current: {:?}, next: {:?} @ {:?}", cur_tok, token, cur_loc);

                        break;
                    }
                }

                if next_tok.is_none() {
                    break;
                }
            }

            if cur_tok.is_none() && next_tok.is_none() {
                break;
            }
        }

        if cur_tok == Some(Token::EndLoop) {
            if phi.is_none() {
                bail!("Error: Extra ']'");
            }

            trace!("generate code for {:?} @ {:?}", cur_tok, cur_loc);

            self.cur_head = self.emit_end_loop(phi.unwrap(), old_bb.unwrap(), test_bb.unwrap())
        } else {
            if phi.is_some() {
                bail!("Error: Missing ']'")
            }

            //End of the program, so go to return block
            self.builder <<= br!(self.end_bb);
        }

        Ok(())
    }

    fn emit_read(&self) {
        let i8_t = self.context.int8_t();

        //%tape.%d = call i32 @getchar()
        let tape_0 = call!(self.getchar_func; TAPER_REG).set_tail_call(false);

        //%tape.%d = trunc i32 %tape.%d to i8
        let tape_1 = trunc!(tape_0, i8_t; TAPER_REG);

        //store i8 %tape.%d, i8 *%head.%d
        store!(tape_1, self.cur_head).emit_to(&self.builder);
    }

    fn emit_write(&self) {
        let i32_t = self.context.int32_t();

        //%tape.%d = load i8 *%head.%d
        let tape_0 = load!(self.cur_head; TAPER_REG);

        //%tape.%d = sext i8 %tape.%d to i32
        let tape_1 = sext!(tape_0, i32_t; TAPER_REG);

        //call i32 @putchar(i32 %tape.%d)
        call!(self.putchar_func, tape_1)
            .set_tail_call(false)
            .emit_to(&self.builder);
    }

    fn emit_move(&self, steps: i64) -> Instruction {
        let i32_t = self.context.int32_t();

        //%head.%d = getelementptr i8 *%head.%d, i32 %d
        let cur_head = gep!(self.cur_head, i32_t.int(steps); HEAD_REG).emit_to(&self.builder);

        if self.compile_flags.contains(CompileFlags::FLAG_ARRAY_BOUNDS) {
            trace!("checking array bounds");

            //%test.%d = icmp uge i8 *%head.%d, %arrmax
            let test_0 = icmp!(UGE cur_head, self.ptr_arrmax.unwrap(); TEST_REG);

            //%test.%d = icmp ult i8 *%head.%d, %arr
            let test_1 = icmp!(ULT cur_head, self.ptr_arr; TEST_REG);

            //%test.%d = or i1 %test.%d, %test.%d
            let test_2 = or!(test_0, test_1; TEST_REG);

            //br i1 %test.%d, label %main.%d, label %main.%d
            let next_bb = self.brainf_func
                .append_basic_block_in_context(LABEL, &self.context);

            br!(test_2 => self.aberror_bb.unwrap(), _ => next_bb).emit_to(&self.builder);

            //main.%d:
            self.builder.position_at_end(next_bb);
        }

        cur_head.into()
    }

    fn emit_change(&self, delta: i64) {
        let i8_t = self.context.int8_t();

        //%tape.%d = load i8 *%head.%d
        let tape_0 = load!(self.cur_head; TAPER_REG);

        //%tape.%d = add i8 %tape.%d, %d
        let tape_1 = add!(tape_0, i8_t.int(delta); TAPER_REG);

        //store i8 %tape.%d, i8 *%head.%d\n"
        store!(tape_1, self.cur_head).emit_to(&self.builder);
    }

    fn emit_loop(&self) -> (Phi<'a>, BasicBlock, BasicBlock) {
        let i8_t = self.context.int8_t();

        //br label %main.%d
        let test_bb = self.brainf_func
            .append_basic_block_in_context(format!("{}.loop.test", LABEL), &self.context);

        br!(test_bb).emit_to(&self.builder);

        //main.%d:
        let old_bb = self.builder.insert_block().unwrap();

        let loop_bb = self.brainf_func
            .append_basic_block_in_context(format!("{}.loop.entry", LABEL), &self.context);
        self.builder.position_at_end(loop_bb);

        // Make part of PHI instruction now, wait until end of loop to finish
        let phi = phi!(i8_t.ptr_t(), self.cur_head => old_bb; HEAD_REG);

        (phi, loop_bb, test_bb)
    }

    fn emit_end_loop(&self, phi: Phi, old_bb: BasicBlock, test_bb: BasicBlock) -> Instruction {
        let i8_t = self.context.int8_t();

        // Write loop test

        //br label %main.%d
        br!(test_bb).emit_to(&self.builder);

        //main.%d:

        self.builder.position_at_end(test_bb);

        //%head.%d = phi i8 *[%head.%d, %main.%d], [%head.%d, %main.%d]
        //Finish phi made at beginning of loop
        let head_0 = phi.add_incoming(self.cur_head, old_bb)
            .emit_to(&self.builder);

        //%tape.%d = load i8 *%head.%d
        let tape_0 = load!(head_0; TAPER_REG);

        //%test.%d = icmp eq i8 %tape.%d, 0
        let test_0 = icmp!(EQ tape_0, i8_t.int(0); TEST_REG);

        //br i1 %test.%d, label %main.%d, label %main.%d
        let loop_end_bb = self.brainf_func
            .append_basic_block_in_context(format!("{}.loop.end", LABEL), &self.context);

        br!(test_0 => loop_end_bb, _ => old_bb).emit_to(&self.builder);

        //main.%d:
        self.builder.position_at_end(loop_end_bb);

        //%head.%d = phi i8 *[%head.%d, %main.%d]
        phi!(i8_t.ptr_t(), head_0 => test_bb)
            .emit_to(&self.builder)
            .into()
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

    match parse_cmdline(program, args) {
        Ok(Some(opts)) => {
            let (code, input) = match opts.free.split_first() {
                None => {
                    debug!("read input from STDIN");

                    let stdin = io::stdin();
                    let mut handle = stdin.lock();
                    let mut code = String::new();

                    handle.read_to_string(&mut code).unwrap();

                    (code, Path::new("stdin"))
                }
                Some((filename, _)) if Path::new(filename).exists() => {
                    debug!("read input from file: {}", filename);

                    let mut code = String::new();

                    File::open(filename)
                        .unwrap()
                        .read_to_string(&mut code)
                        .unwrap();

                    (code, Path::new(filename))
                }
                Some((code, _)) => (code.to_owned(), Path::new("expr")),
            };

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

            if let Err(err) = brainf.parse(&code) {
                eprintln!("Fail to generate module, {}", err);
            } else {
                let module = brainf.module;

                //Write it out
                if opts.opt_present("jit") {
                    NativeTarget::init().unwrap();
                    NativeAsmPrinter::init().unwrap();

                    println!("------- Running JIT -------");

                    let brainf_func = module.get_function("brainf").unwrap();

                    ExecutionEngine::for_module(module)
                        .unwrap()
                        .run_function(&brainf_func, vec![]);

                    jit::shutdown();
                } else {
                    //Get the output stream
                    let output_filename = opts.opt_str("o").map_or_else(
                        || input.with_extension("bc"),
                        |filename| Path::new(filename.as_str()).to_owned(),
                    );

                    debug!("write output to file: {:?}", output_filename);

                    module.write_bitcode(output_filename).unwrap();
                }
            }
        }
        Ok(None) => {}
        Err(err) => eprintln!("Fail to parse command line, {}", err),
    }
}
