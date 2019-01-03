#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate nom;
extern crate llvm_bpf as bpf;

use std::borrow::Cow;
use std::collections::HashSet;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;
use std::str::{self, FromStr};

use bpf::{
    raw::{bpf_insn, bpf_program},
    InstFmt, Program,
};
use failure::Error;
use nom::{digit, multispace, rest_s};
use pcap_file::{Packet, PcapReader};
use rustyline::{completion::Completer, error::ReadlineError, highlight::Highlighter, hint::Hinter, Editor, Helper};
use structopt::StructOpt;

type Result<T> = StdResult<T, Error>;

#[derive(Debug, StructOpt)]
#[structopt(name = "bpf_dbg", about = "Minimal BPF debugger")]
struct Opt {
    #[structopt(long = "history", default_value = "~/.bpf_dbg.history")]
    history_file: String,
}

#[derive(Debug, Default)]
struct Debugger {
    reader: Option<PcapReader<File>>,
    packets: Vec<Packet<'static>>,
    packet: Option<Packet<'static>>,
    state: State,
}

#[derive(Debug, Default)]
struct State {
    program: Program,
    breakpoints: HashSet<usize>,
    pc: u32,
    a: u32,
    x: u32,
    m: [u32; bpf::BPF_MEMWORDS],
}

const CMD_USAGE: &str = r#"
load bpf <code>         Load BPF program.
                        use `tcpdump -ien0 -ddd port 22 | tr '\n' ','` to load as filter
load pcap <file>        Load PCAP file as packets
select <where>          Select the <where> packet to start.
step	                Continue running your program until control reaches a different source line.
step <count>            Continue running as in step, but do so <count> times.
run                     Run the filter for all the packets.
run <count>             Run the filter for the next <count> packets.
breakpoint <where>      Set breakpoint at the <where> line.
breakpoint reset        Cleanup all the breakpoints.
breakpoint list         List all the breakpoints.
disassemble	            Disassemble the current line.
disassemble <where>     Disassemble the <where> line.
dump                    Dump the runtime status.
help, h, ?              Show the help screen.
quit, q                 Quit the program.
"#;

impl Debugger {
    pub fn run(&mut self, cmd: Cmd) -> Result<()> {
        match cmd {
            Cmd::Help => {
                println!("{}", CMD_USAGE);

                Ok(())
            }
            Cmd::Load(Load::Bpf(code)) => self.load_bpf(&code),
            Cmd::Load(Load::Pcap(filename)) => self.load_pcap(&filename),
            Cmd::Select(idx) => self.select_packet(idx),
            Cmd::Breakpoint(Breakpoint::List) => self.list_breakpoints(),
            Cmd::Breakpoint(Breakpoint::Reset) => self.clear_breakpoints(),
            Cmd::Breakpoint(Breakpoint::Line(pos)) => self.set_breakpoint(pos),
            Cmd::Disassemble(pos) => self.disassemble(pos),
            _ => unimplemented!(),
        }
    }

    fn program(&self) -> Result<&Program> {
        if self.state.program.is_empty() {
            bail!("no BPF program loaded")
        }

        Ok(&self.state.program)
    }

    fn load_bpf(&mut self, code: &[bpf_insn]) -> Result<()> {
        let program = bpf_program {
            bf_len: code.len() as u32,
            bf_insns: code.as_ptr() as *mut _,
        };

        let program = Result::<bpf::Program>::from(&program)?;

        debug!("load BPF:\n{}", program);

        self.state = State {
            program,
            ..State::default()
        };

        Ok(())
    }

    fn load_pcap(&mut self, filename: &Path) -> Result<()> {
        let f = File::open(filename)?;
        let r = PcapReader::new(f).map_err(|err| format_err!("fail to load PCAP file `{:?}`, {}", filename, err))?;
        self.reader = Some(r);
        self.packets.clear();
        self.packet = None;

        debug!("load PCAP file from {:?}", filename);

        Ok(())
    }

    fn select_packet(&mut self, idx: usize) -> Result<()> {
        if let Some(ref mut reader) = self.reader {
            if idx > self.packets.len() {
                while let Some(pkt) = reader.next() {
                    self.packets
                        .push(pkt.map_err(|err| format_err!("fail to read PCAP file, {}", err))?);

                    if idx < self.packets.len() {
                        break;
                    }
                }
            }

            if idx < self.packets.len() {
                self.packet = Some(self.packets[idx].clone());

                Ok(())
            } else {
                bail!(
                    "expect to select the #{} packet, but PCAP file only have {} packets",
                    idx,
                    self.packets.len()
                );
            }
        } else {
            bail!("no PCAP file loaded");
        }
    }

    fn list_breakpoints(&self) -> Result<()> {
        let program = self.program()?;

        for (idx, pos) in self.state.breakpoints.iter().cloned().enumerate() {
            println!("breakpoint #{} at {}", idx, InstFmt(pos, &program[pos]));
        }

        Ok(())
    }

    fn clear_breakpoints(&mut self) -> Result<()> {
        debug!("clear all the breakpoints");

        self.state.breakpoints.clear();

        Ok(())
    }

    fn set_breakpoint(&mut self, pos: usize) -> Result<()> {
        debug!("set breakpoint at #{}", pos);

        if pos >= self.program()?.len() {
            bail!("line {} out of range", pos);
        }

        if !self.state.breakpoints.insert(pos) {
            eprintln!("breakpoint already set");
        }

        Ok(())
    }

    fn disassemble(&self, pos: Option<usize>) -> Result<()> {
        let program = self.program()?;

        if let Some(pos) = pos {
            if pos >= program.len() {
                bail!("line {} out of range", pos);
            }

            println!("{}", InstFmt(pos, &program[pos]));
        } else {
            println!("{}", program)
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum Cmd {
    Load(Load),
    Select(usize),
    Step(Option<usize>),
    Run(Option<usize>),
    Breakpoint(Breakpoint),
    Disassemble(Option<usize>),
    Dump,
    Help,
    Quit,
}

#[derive(Debug, PartialEq)]
enum Load {
    Bpf(Vec<bpf_insn>),
    Pcap(PathBuf),
}

#[derive(Debug, PartialEq)]
enum Breakpoint {
    List,
    Reset,
    Line(usize),
}

const CMD_LOAD: &str = "load";
const CMD_SELECT: &str = "select";
const CMD_STEP: &str = "step";
const CMD_NEXT: &str = "next";
const CMD_RUN: &str = "run";
const CMD_BREAKPOINT: &str = "breakpoint";
const CMD_DISASSEMBLE: &str = "disassemble";
const CMD_HELP: &str = "help";
const CMD_QUIT: &str = "quit";

const COMMANDS: &[&str] = &[
    CMD_LOAD,
    CMD_SELECT,
    CMD_STEP,
    CMD_NEXT,
    CMD_RUN,
    CMD_BREAKPOINT,
    CMD_DISASSEMBLE,
    CMD_HELP,
    CMD_QUIT,
];

named!(parse_cmd<&str, Cmd>, alt_complete!(
    tuple!(cmd_load, multispace, parse_load) => {
        |(_, _, load)| Cmd::Load(load)
    } |
    tuple!(cmd_select, multispace, parse_num) => {
        |(_, _, line)| Cmd::Select(line)
    } |
    tuple!(cmd_step, multispace, parse_num) => {
        |(_, _, lines)| Cmd::Step(Some(lines))
    } |
    cmd_step => {
        |_| Cmd::Step(None)
    } |
    tuple!(cmd_run, multispace, parse_num) => {
        |(_, _, lines)| Cmd::Run(Some(lines))
    } |
    cmd_run => {
        |_| Cmd::Run(None)
    } |
    tuple!(cmd_breakpoint, multispace, parse_breakpoint) => {
        |(_, _, breakpoint)| Cmd::Breakpoint(breakpoint)
    } |
    cmd_breakpoint => {
        |_| Cmd::Breakpoint(Breakpoint::List)
    } |
    tuple!(cmd_disassemble, multispace, parse_num) => {
        |(_, _, lines)| Cmd::Disassemble(Some(lines))
    } |
    cmd_disassemble => {
        |_| Cmd::Disassemble(None)
    } |
    tag!("dump") => {
        |_| Cmd::Dump
    } |
    alt_complete!(tag!("h") | tag!("?") | tag!(CMD_HELP)) => {
        |_| Cmd::Help
    } |
    alt_complete!(tag!("q") | tag!(CMD_QUIT)) => {
        |_| Cmd::Quit
    }
));

named!(cmd_load<&str, &str>, alt_complete!(tag!(CMD_LOAD) | tag!("l")));
named!(cmd_select<&str, &str>, alt_complete!(tag!(CMD_SELECT) | tag!("sel")));
named!(cmd_step<&str, &str>, alt_complete!(tag!(CMD_STEP) | tag!(CMD_NEXT) | tag!("n")));
named!(cmd_run<&str, &str>, alt_complete!(tag!(CMD_RUN) | tag!("r")));
named!(cmd_breakpoint<&str, &str>, alt_complete!(tag!(CMD_BREAKPOINT) | tag!("br")));
named!(cmd_disassemble<&str, &str>, alt_complete!(tag!(CMD_DISASSEMBLE) | tag!("dis") | tag!("d")));

named!(parse_load<&str, Load>, alt_complete!(
    tuple!(tag!("bpf"), multispace, parse_code) => {
        |(_, _, code)| Load::Bpf(code)
    } |
    tuple!(tag!("pcap"), multispace, map!(map!(map_res!(rest_s, shellexpand::full), Cow::into_owned), PathBuf::from)) => {
        |(_, _, path)| Load::Pcap(path)
    }
));

named!(parse_code<&str, Vec<bpf_insn>>, do_parse!(
    _len: map_res!(digit, usize::from_str) >>
    opt!(multispace) >>
    tag!(",") >>
    opt!(multispace) >>
    insts: separated_nonempty_list_complete!(tag!(","), parse_inst) >>
    (
        insts
    )
));

/// `tcpdump -ien0 -ddd port 22 | tr '\n' ','` to load as filter
named!(parse_inst<&str, bpf_insn>, do_parse!(
    opt!(multispace) >>
    code: map_res!(digit, u16::from_str) >>
    multispace >>
    jt: map_res!(digit, u8::from_str) >>
    multispace >>
    jf: map_res!(digit, u8::from_str) >>
    multispace >>
    k: map_res!(digit, u32::from_str) >>
    opt!(multispace) >>
    (
        bpf_insn { code, jt, jf, k }
    )
));

named!(parse_breakpoint<&str, Breakpoint>, alt_complete!(
    tag!("list") => { |_| Breakpoint::List } |
    tag!("reset") => { |_| Breakpoint::Reset } |
    parse_num => { |line| Breakpoint::Line(line) } |
    eof!() => { |_| Breakpoint::List }
));

named!(parse_num<&str, usize>, map_res!(rest_s, usize::from_str));

struct CmdHelper;

impl Helper for CmdHelper {}

impl Completer for CmdHelper {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let mut candidates = COMMANDS
            .iter()
            .filter(|cmd| cmd.starts_with(line))
            .map(|cmd| (*cmd).to_owned())
            .collect();

        Ok((0, candidates))
    }
}

impl Hinter for CmdHelper {
    fn hint(&self, line: &str, pos: usize) -> Option<String> {
        if line.is_empty() {
            None
        } else {
            COMMANDS
                .iter()
                .find(|cmd| cmd.starts_with(line))
                .map(|cmd| (&cmd[pos..]).to_owned())
        }
    }
}

impl Highlighter for CmdHelper {}

fn run_shell_loop(opt: Opt) -> Result<()> {
    let mut rl = Editor::<CmdHelper>::new();

    rl.set_helper(Some(CmdHelper));

    let history_file = PathBuf::from(String::from(shellexpand::full(&opt.history_file)?));

    if rl.load_history(&history_file).is_err() {
        trace!("not found history @ {:?}", history_file)
    }

    let mut dbg = Debugger::default();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                trace!("parsing input line: {}", line);

                match parse_cmd(&line) {
                    Ok((_, cmd)) => {
                        debug!("parsed command: {:?}", cmd);

                        rl.add_history_entry((line).as_ref());

                        if cmd == Cmd::Quit {
                            break;
                        } else if let Err(err) = dbg.run(cmd) {
                            eprintln!("ERROR: {}", err)
                        }
                    }
                    Err(err) => eprintln!("fail to parse command `{}`, {}", line, err),
                }
            }
            Err(ReadlineError::Interrupted) => {
                trace!("received signal: CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                trace!("received signal: CTRL-D");
                break;
            }
            Err(err) => bail!(err),
        }
    }

    if rl.save_history(&history_file).is_ok() {
        trace!("write history @ {:?}", history_file)
    }

    Ok(())
}

fn main() -> Result<()> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    debug!("parsed cmdline: {:?}", opt);

    run_shell_loop(opt)
}
