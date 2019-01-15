use std::cell::RefCell;
use std::collections::HashMap;
use std::str::FromStr;

use nom::*;

use crate::ast::Program;
use crate::compile::OpCode;
use crate::raw::*;

/// Parse BPF assembly code to program
pub fn parse<S: AsRef<str>>(code: S) -> Result<Program, failure::Error> {
    code.as_ref().parse()
}

impl FromStr for Program {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let labels = RefCell::new(Vec::new());
        let (rest, prog) = prog(s, &labels).map_err(|err| format_err!("fail to parse BPF code, {}", err))?;

        if rest.chars().any(|c| !c.is_whitespace()) {
            bail!("unexpected code: {}", rest)
        }

        let labels = labels.into_inner();
        let mut insns = vec![];
        let mut offsets = HashMap::new();

        for (label, insn, _) in prog {
            if let Some(label) = label {
                if offsets.contains_key(label) {
                    bail!("duplicate label: {}", label);
                }

                offsets.insert(label, insns.len());
            }

            if let Some(insn) = insn {
                insns.push(insn)
            }
        }

        let reduce_label = |pos, id| {
            if id > 0 {
                let idx = id as usize - 1;

                if let Some(label) = labels.get(idx) {
                    if let Some(off) = offsets.get(label) {
                        Ok((off - pos - 1) as u8)
                    } else {
                        bail!("invalid label name: {}", label)
                    }
                } else {
                    bail!("invalid label id: {}", id)
                }
            } else {
                Ok(0)
            }
        };

        insns
            .into_iter()
            .enumerate()
            .map(|(pos, insn)| {
                let opcode = OpCode(insn.code as u8);

                if opcode.class() == BPF_JMP {
                    match opcode.op() {
                        BPF_JA => Ok(BPF_JUMP!(insn.code, reduce_label(pos, insn.k as u8)? as u32)),
                        BPF_JEQ | BPF_JGT | BPF_JGE | BPF_JSET => Ok(BPF_JUMP!(
                            insn.code,
                            reduce_label(pos, insn.jt)?,
                            reduce_label(pos, insn.jf)?
                        )),
                        _ => Ok(insn),
                    }
                } else {
                    Ok(insn)
                }
            })
            .collect::<Result<Vec<_>, Self::Err>>()
            .and_then(|insns| insns.iter().collect::<Result<Program, Self::Err>>())
    }
}

macro_rules! next {
    ($i:expr, $submac:ident) => {
        preceded!($i, multispace0, $submac)
    };
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        preceded!($i, multispace0, $submac !( $( $args )* ))
    };
}

named_args!(prog<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, Vec<(Option<&'a str>, Option<bpf_insn>, Option<&'a str>)>>,
    separated_list_complete!(eol, apply!(line, labels))
);

named_args!(line<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, (Option<&'a str>, Option<bpf_insn>, Option<&'a str>)>,
    alt!(
        full_line_comment => { |comment| (None, None, Some(comment)) } |
        tuple!(opt!(complete!(labelled)), opt!(next!(apply!(instr, labels))), opt!(complete!(next!(inline_comment))))
    )
);

named!(inline_comment<&str, &str>, alt!(block_comment | end_of_line_comment));
named!(block_comment<&str, &str>, preceded!(tag!("/*"), take_until_and_consume!("*/")));
named!(end_of_line_comment<&str, &str>, preceded!(tag!(";"), take_until_and_consume!("\n")));
named!(full_line_comment<&str, &str>, preceded!(tag!("#"), take_until_and_consume!("\n")));

named!(labelled<&str, &str>, terminated!(label, tag!(":")));

named_args!(instr<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, bpf_insn>, alt_complete!(
      load
    | st
    | apply!(jump, labels)
    | alu
    | ret
    | misc
));

named!(load<&str, bpf_insn>, alt_complete!(
      ldb
    | ldh
	| ldi
	| ldx
	| ldxi
    | ldxb
    | ld
));

/// Load byte into A
named!(ldb<&str, bpf_insn>, do_parse!(
    tag!("ldb") >> multispace1 >>
    insn: apply!(load_arg, BPF_B) >>
    (
        insn
    )
));

/// Load half-word into A
named!(ldh<&str, bpf_insn>, do_parse!(
    tag!("ldh") >> multispace1 >>
    insn: apply!(load_arg, BPF_H) >>
    (
        insn
    )
));

/// Load word into A
named!(ldi<&str, bpf_insn>, do_parse!(
    tag!("ldi") >> multispace1 >>
    opt!(tag!("#")) >> n: number >>
    (
        BPF_STMT!(BPF_LD | BPF_IMM, n)
    )
));

named_args!(load_arg(size: u32)<&str, bpf_insn>, do_parse!(
    value: alt_complete!(
        load_value |
        extension => {
            |ext| (BPF_ABS, SKF_AD_OFF + ext)
        }
    ) >>
    ({
        let (ty, k) = value;

        BPF_STMT!(BPF_LD | size | ty, k)
    })
));

named!(load_value<&str, (u32, u32)>, do_parse!(
    tag!("[") >>
    value: next!(alt!(
        do_parse!(
            idx >>
            next!(tag!("+")) >>
            k: next!(number) >>
            (
                k
            )
        ) => {
            |k| (BPF_IND, k)
        } |
        number => {
            |k| (BPF_ABS, k)
        }
    )) >>
    next!(tag!("]")) >>
    ( value )
));

named!(ld<&str, bpf_insn>, do_parse!(
    tag!("ld") >> multispace1 >>
    insn: alt!(
        imm => {
            |k| BPF_STMT!(BPF_LD | BPF_IMM, k)
        } |
        len => {
            |op| BPF_STMT!(BPF_LD | BPF_W | op)
        } |
        extension => {
            |ext| BPF_STMT!(BPF_LD | BPF_W | BPF_ABS, SKF_AD_OFF + ext)
        } |
        mem => {
            |k| BPF_STMT!(BPF_LD | BPF_MEM, k)
        } |
        load_value => {
            |(ty, k)| BPF_STMT!(BPF_LD | BPF_W | ty, k)
        }
    ) >>
    (
        insn
    )
));

/// Load word into X
named!(ldxi<&str, bpf_insn>, do_parse!(
    tag!("ldxi") >> multispace1 >>
    opt!(tag!("#")) >> n: number >>
    (
        BPF_STMT!(BPF_LDX | BPF_IMM, n)
    )
));

named!(
    ldx<&str, bpf_insn>, do_parse!(
        tag!("ldx") >> multispace1 >>
        insn: alt!(
            imm => {
                |n| BPF_STMT!(BPF_LDX | BPF_IMM, n)
            } |
            len => {
                |op| BPF_STMT!(BPF_LDX | BPF_W | op)
            } |
            mem => {
                |k| BPF_STMT!(BPF_LDX | BPF_MEM, k)
            } |
            msh
        ) >>
        (
            insn
        )
    )
);

named!(ldxb<&str, bpf_insn>, preceded!(tag!("ldxb"), preceded!(multispace1, msh)));

named!(
    msh<&str, bpf_insn>, do_parse!(
        verify!(number, |n| n == 4) >>
        next!(tag!("*")) >>
        next!(tag!("(")) >>
            k: delimited!(next!(tag!("[")), next!(number), next!(tag!("]"))) >>
            next!(tag!("&")) >>
            verify!(next!(number), |n| n == 0x0f) >>
        next!(tag!(")")) >>
        (
            BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, k)
        )
    )
);

// Copy A|X into M[]
named!(st<&str, bpf_insn>, do_parse!(
    code: alt_complete!(
        tag!("stx") => { |_| BPF_STX } |
        tag!("st")  => { |_| BPF_ST }
    ) >> multispace1 >>
    k: mem >>
    (
        BPF_STMT!(code, k)
    )
));

named_args!(jump<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, bpf_insn>, alt!(
      apply!(jmp, labels)
    | apply!(jcond, labels)
    | apply!(jcond_neg, labels)
));

fn get_or_insert_label_id<'a>(labels: &RefCell<Vec<&'a str>>, label: &'a str) -> usize {
    let pos = labels.borrow().iter().position(|l| *l == label);

    if let Some(pos) = pos {
        pos + 1
    } else {
        labels.borrow_mut().push(label);
        labels.borrow().len()
    }
}

named_args!(jmp<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, bpf_insn>, do_parse!(
    alt!(tag!("jmp") | tag!("ja")) >> multispace1 >>
    label: label >>
    ({
        let id = get_or_insert_label_id(labels, label) as u32;

        BPF_JUMP!(BPF_JMP | BPF_JA, id)
    })
));

named_args!(jcond<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, bpf_insn>, do_parse!(
    code: alt_complete!(
        tag!("jeq")  => { |_| BPF_JEQ } |
        tag!("jgt")  => { |_| BPF_JGT } |
        tag!("jge")  => { |_| BPF_JGE } |
        tag!("jset") => { |_| BPF_JSET }
    ) >> multispace1 >>
    insn: alt_complete!(
        apply!(then_else, code) => { |(insn, then, or_else)| (insn, then, Some(or_else)) } |
        apply!(then, code) => { |(insn, then)| (insn, then, None) }
    ) >>
    ({
        let (mut insn, then, or_else) = insn;

        insn.jt = get_or_insert_label_id(labels, then) as u8;

        if let Some(or_else) = or_else {
            insn.jf = get_or_insert_label_id(labels, or_else) as u8;
        }

        insn
    })
));

named_args!(jcond_neg<'a>(labels: &RefCell<Vec<&'a str>>)<&'a str, bpf_insn>, do_parse!(
    code: alt_complete!(
        tag!("jneq") => { |_| BPF_JEQ } |
        tag!("jne")  => { |_| BPF_JEQ } |
        tag!("jlt")  => { |_| BPF_JGT } |
        tag!("jle")  => { |_| BPF_JGE }
    ) >> multispace1 >>
    insn: apply!(then, code) >>
    ({
        let (mut insn, then) = insn;

        insn.jf = get_or_insert_label_id(labels, then) as u8;

        insn
    })
));

named_args!(then_else(code: u32)<&str, (bpf_insn, &str, &str)>, alt_complete!(
    do_parse!(
        k: imm >>
        then: next!(if_clause) >>
        or_else: next!(if_clause) >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_K, k), then, or_else
        )
    ) |
    do_parse!(
        idx >>
        then: next!(if_clause) >>
        or_else: next!(if_clause) >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_X), then, or_else
        )
    )
));

named_args!(then(code: u32)<&str, (bpf_insn, &str)>, alt_complete!(
    do_parse!(
        k: imm >>
        then: next!(if_clause) >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_K, k), then
        )
    ) |
    do_parse!(
        idx >>
        then: next!(if_clause) >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_X), then
        )
    )
));

named!(if_clause<&str, &str>, preceded!(tag!(","), next!(label)));

named!(alu<&str, bpf_insn>, alt!(bin_op | unary_op));

named!(bin_op<&str, bpf_insn>, do_parse!(
    op: alt_complete!(
        tag!("add") => { |_| BPF_ADD } |
        tag!("sub") => { |_| BPF_SUB } |
        tag!("mul") => { |_| BPF_MUL } |
        tag!("div") => { |_| BPF_DIV } |
        tag!("mod") => { |_| BPF_MOD } |
        tag!("and") => { |_| BPF_AND } |
        tag!("xor") => { |_| BPF_XOR } |
        tag!("or")  => { |_| BPF_OR } |
        tag!("lsh") => { |_| BPF_LSH } |
        tag!("rsh") => { |_| BPF_RSH }
    ) >> multispace1 >>
    insn: alt_complete!(
        imm => { |k| BPF_STMT!(BPF_ALU | op | BPF_K, k) } |
        idx => { |_| BPF_STMT!(BPF_ALU | op | BPF_X) }
    ) >>
    (
        insn
    )
));

named!(unary_op<&str, bpf_insn>, map!(tag!("neg"), |_| BPF_STMT!(BPF_ALU | BPF_NEG)));

named!(ret<&str, bpf_insn>, do_parse!(
    tag!("ret") >> multispace1 >>
    insn: alt!(
        acc => { |_| BPF_STMT!(BPF_RET | BPF_A) } |
        idx => { |_| BPF_STMT!(BPF_RET | BPF_X) } |
        imm => { |k| BPF_STMT!(BPF_RET | BPF_K, k) }
    ) >>
    (
        insn
    )
));

named!(misc<&str, bpf_insn>, map!(
    alt!(
        tag!("tax") => { |_| BPF_TAX } |
        tag!("txa") => { |_| BPF_TXA }
    ),
    |op| BPF_STMT!(BPF_MISC | op)
));

named!(
    imm<&str, u32>, preceded!(tag!("#"), number)
);

named!(
    acc<&str, &str>, preceded!(opt!(tag!("%")), tag!("a"))
);

named!(
    idx<&str, &str>, preceded!(opt!(tag!("%")), tag!("x"))
);

named!(
    len<&str, u32>, map!(preceded!(opt!(tag!("#")), alt_complete!(tag!("len") | tag!("pktlen"))), |_| BPF_LEN)
);

named!(mem<&str, u32>, delimited!(tag!("M["), next!(number), next!(tag!("]"))));

named!(
    extension<&str, u32>, preceded!(opt!(tag!("#")),
        alt_complete!(
            alt_complete!(
                tag!("proto") |
                tag!("pro")
            )                   => { |_| SKF_AD_PROTOCOL } |
            tag!("type")        => { |_| SKF_AD_PKTTYPE } |
            tag!("poff")        => { |_| SKF_AD_PAY_OFFSET } |
            alt_complete!(
                tag!("ifidx") |
                tag!("ifx")
            )                   => { |_| SKF_AD_IFINDEX } |
            tag!("nla")         => { |_| SKF_AD_NLATTR } |
            tag!("nlan")        => { |_| SKF_AD_NLATTR_NEST } |
            tag!("mark")        => { |_| SKF_AD_MARK } |
            tag!("queue")       => { |_| SKF_AD_QUEUE } |
            tag!("que")         => { |_| SKF_AD_QUEUE } |
            tag!("hatype")      => { |_| SKF_AD_HATYPE } |
            tag!("rxhash")      => { |_| SKF_AD_RXHASH } |
            tag!("rxh")         => { |_| SKF_AD_RXHASH } |
            alt_complete!(
                tag!("vlan_tci") |
                tag!("vlant")
            )                   => { |_| SKF_AD_VLAN_TAG } |
            alt_complete!(
                tag!("vlan_pr") |
                tag!("vlanp") |
                tag!("vlan_avail")
            )                   => { |_| SKF_AD_VLAN_TAG_PRESENT } |
            tag!("tpid")        => { |_| SKF_AD_VLAN_TPID } |
            tag!("rand")        => { |_| SKF_AD_RANDOM }
        )
    )
);

named!(
    label<&str, &str>,
    verify!(take_while1!(|c: char| c.is_alphanumeric() || c == '_'), |s: &str| {
        let c = s.chars().next().unwrap();

        c.is_alphabetic() || c == '_'
    })
);

named!(
    number<&str, u32>, alt_complete!(
        preceded!(tag!("0x"), apply!(take_str_radix, 16)) |
        preceded!(tag!("0b"), apply!(take_str_radix, 2)) |
        preceded!(alt!(tag!("0o") | tag!("0")), apply!(take_str_radix, 8)) |
        do_parse!(
            sign: opt!(alt!(tag!("+") | tag!("-"))) >>
            value: apply!(take_str_radix, 10) >>
            (
                match sign {
                    Some("-") => -(value as i32) as u32,
                    _ => value
                }
            )
        )
    )
);

named_args!(
    take_str_radix(radix: u32)<&str, u32>,
    map_res!(take_while_s!(|c: char| c.is_digit(radix)), |s| u32::from_str_radix(s, radix))
);

/* RATIONALE. Negative offsets are invalid in BPF.
  We use them to reference ancillary data.
  Unlike introduction new instructions, it does not break
  existing compilers/optimizers.
*/
const SKF_AD_OFF: u32 = (-0x1000 as i32) as u32;
/// Ethernet type field (skb->protocol)
const SKF_AD_PROTOCOL: u32 = 0;
/// Packet type (**) (skb->pkt_type)
const SKF_AD_PKTTYPE: u32 = 4;
/// Interface index (skb->dev->ifindex)
const SKF_AD_IFINDEX: u32 = 8;
/// Netlink attribute of type X with offset A
const SKF_AD_NLATTR: u32 = 12;
/// Nested Netlink attribute of type X with offset A
const SKF_AD_NLATTR_NEST: u32 = 16;
/// Packet mark (skb->mark)
const SKF_AD_MARK: u32 = 20;
/// NIC queue index (skb->queue_mapping)
const SKF_AD_QUEUE: u32 = 24;
/// NIC hardware type (**) (skb->dev->type)
const SKF_AD_HATYPE: u32 = 28;
/// Receive hash (skb->rxhash)
const SKF_AD_RXHASH: u32 = 32;
/// Current CPU (raw_smp_processor_id())
const SKF_AD_CPU: u32 = 36;
const SKF_AD_ALU_XOR_X: u32 = 40;
/// VLAN TCI value (vlan_tx_tag_get(skb))
const SKF_AD_VLAN_TAG: u32 = 44;
/// VLAN present (vlan_tx_tag_present(skb))
const SKF_AD_VLAN_TAG_PRESENT: u32 = 48;
/// Detected payload start offset
const SKF_AD_PAY_OFFSET: u32 = 52;
const SKF_AD_RANDOM: u32 = 56;
const SKF_AD_VLAN_TPID: u32 = 60;
const SKF_AD_MAX: u32 = 64;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Cond::*, Inst::*, Mode, RVal, Size::*, Src::*};

    #[test]
    pub fn parse_number() {
        assert_eq!(number("123\n"), Ok(("\n", 123)));
        assert_eq!(number("+123\n"), Ok(("\n", 123)));
        assert_eq!(number("-123\n"), Ok(("\n", 0xffffff85)));
        assert_eq!(number("0x1f\n"), Ok(("\n", 0x1f)));
        assert_eq!(number("0b01010101\n"), Ok(("\n", 0x55)));
        assert_eq!(number("0777\n"), Ok(("\n", 0o777)));
        assert_eq!(number("0o777\n"), Ok(("\n", 0o777)));
    }

    #[test]
    pub fn parse_imm() {
        assert_eq!(imm("#123\n"), Ok(("\n", 123)));
        assert_eq!(imm("#0x1f\n"), Ok(("\n", 0x1f)));
        assert_eq!(imm("#0b01010101\n"), Ok(("\n", 0x55)));
        assert_eq!(imm("#0777\n"), Ok(("\n", 0o777)));
        assert_eq!(imm("#0o777\n"), Ok(("\n", 0o777)));
    }

    #[test]
    pub fn parse_idx() {
        assert_eq!(idx("x"), Ok(("", "x")));
        assert_eq!(idx("%x"), Ok(("", "x")));
    }

    #[test]
    pub fn parse_len() {
        assert_eq!(len("len"), Ok(("", BPF_LEN)));
        assert_eq!(len("#len"), Ok(("", BPF_LEN)));
        assert_eq!(len("pktlen"), Ok(("", BPF_LEN)));
        assert_eq!(len("#pktlen"), Ok(("", BPF_LEN)));
    }

    #[test]
    pub fn parse_load() {
        assert_eq!(load("ldb [x+5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_IND, 5))));
        assert_eq!(load("ldb [%x + 5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_IND, 5))));
        assert_eq!(load("ldb [5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_ABS, 5))));
        assert_eq!(
            load("ldb #type"),
            Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_ABS, SKF_AD_OFF + SKF_AD_PKTTYPE)))
        );

        assert_eq!(load("ldh [x+5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 5))));
        assert_eq!(load("ldh [%x + 5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 5))));
        assert_eq!(load("ldh [5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, 5))));
        assert_eq!(
            load("ldh #proto"),
            Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, SKF_AD_OFF + SKF_AD_PROTOCOL)))
        );

        assert_eq!(load("ldi 5\n"), Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5))));
        assert_eq!(load("ldi #5\n"), Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5))));

        assert_eq!(load("ld #5\n"), Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5))));
        assert_eq!(load("ld len"), Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_LEN))));
        assert_eq!(load("ld #pktlen"), Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_LEN))));
        assert_eq!(load("ld M[5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_MEM, 5))));
        assert_eq!(load("ld [x+5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_IND, 5))));
        assert_eq!(load("ld [%x + 5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_IND, 5))));
        assert_eq!(load("ld [5]"), Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_ABS, 5))));

        assert_eq!(load("ldxi 5\n"), Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5))));
        assert_eq!(load("ldxi #5\n"), Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5))));

        assert_eq!(load("ldx #5\n"), Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5))));
        assert_eq!(load("ldx len"), Ok(("", BPF_STMT!(BPF_LDX | BPF_W | BPF_LEN))));
        assert_eq!(load("ldx #pktlen"), Ok(("", BPF_STMT!(BPF_LDX | BPF_W | BPF_LEN))));
        assert_eq!(load("ldx M[5]"), Ok(("", BPF_STMT!(BPF_LDX | BPF_MEM, 5))));
        assert_eq!(
            load("ldx 4*([5]&0xf)"),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, 5)))
        );
        assert_eq!(
            load("ldxb 4*([5]&0xf)"),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, 5)))
        );
    }

    #[test]
    pub fn parse_st() {
        assert_eq!(st("st M[5]"), Ok(("", BPF_STMT!(BPF_ST, 5))));
        assert_eq!(st("stx M[5]"), Ok(("", BPF_STMT!(BPF_STX, 5))));
    }

    #[test]
    pub fn parse_jump() {
        let labels = RefCell::new(vec![]);

        assert_eq!(jump("jmp drop\n", &labels), Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JA, 1))));
        assert_eq!(jump("ja L3\n", &labels), Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JA, 2))));
    }

    #[test]
    pub fn parse_jump_with_condition() {
        let labels = RefCell::new(vec![]);

        assert_eq!(
            jump("jeq #0x16, pass, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 1, 2)))
        );
        assert_eq!(
            jump("jgt #0x16, pass, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_K, 0x16, 1, 2)))
        );
        assert_eq!(
            jump("jge x, pass, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 1, 2)))
        );
        assert_eq!(
            jump("jset %x, pass, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JSET | BPF_X, 1, 2)))
        );

        assert_eq!(
            jump("jeq #0x16, pass\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 1, 0)))
        );
        assert_eq!(
            jump("jgt #0x16, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_K, 0x16, 2, 0)))
        );
        assert_eq!(
            jump("jge x, pass\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 1, 0)))
        );
        assert_eq!(
            jump("jset %x, pass\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JSET | BPF_X, 1, 0)))
        );

        assert_eq!(
            jump("jneq #0x16, pass\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 0, 1)))
        );
        assert_eq!(
            jump("jne #0x16, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 0, 2)))
        );
        assert_eq!(
            jump("jlt x, pass\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_X, 0, 1)))
        );
        assert_eq!(
            jump("jle %x, drop\n", &labels),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 0, 2)))
        );
    }

    #[test]
    pub fn parse_alu() {
        assert_eq!(alu("add #5\n"), Ok(("\n", BPF_STMT!(BPF_ALU | BPF_ADD | BPF_K, 5))));
        assert_eq!(alu("sub x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_SUB | BPF_X))));
        assert_eq!(alu("mul x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_MUL | BPF_X))));
        assert_eq!(alu("div #5\n"), Ok(("\n", BPF_STMT!(BPF_ALU | BPF_DIV | BPF_K, 5))));
        assert_eq!(alu("mod x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_MOD | BPF_X))));
        assert_eq!(alu("and x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_AND | BPF_X))));
        assert_eq!(alu("or #5\n"), Ok(("\n", BPF_STMT!(BPF_ALU | BPF_OR | BPF_K, 5))));
        assert_eq!(alu("xor x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_XOR | BPF_X))));
        assert_eq!(alu("lsh x"), Ok(("", BPF_STMT!(BPF_ALU | BPF_LSH | BPF_X))));
        assert_eq!(alu("rsh #5\n"), Ok(("\n", BPF_STMT!(BPF_ALU | BPF_RSH | BPF_K, 5))));
        assert_eq!(alu("neg"), Ok(("", BPF_STMT!(BPF_ALU | BPF_NEG))));
    }

    #[test]
    pub fn parse_ret() {
        assert_eq!(ret("ret #0\n"), Ok(("\n", BPF_STMT!(BPF_RET | BPF_K, 0))));
        assert_eq!(ret("ret #-1\n"), Ok(("\n", BPF_STMT!(BPF_RET | BPF_K, 0xFFFFFFFF))));
        assert_eq!(ret("ret %a"), Ok(("", BPF_STMT!(BPF_RET | BPF_A))));
        assert_eq!(ret("ret a"), Ok(("", BPF_STMT!(BPF_RET | BPF_A))));
        assert_eq!(ret("ret %x"), Ok(("", BPF_STMT!(BPF_RET | BPF_X))));
        assert_eq!(ret("ret x"), Ok(("", BPF_STMT!(BPF_RET | BPF_X))));
    }

    #[test]
    pub fn parse_misc() {
        assert_eq!(misc("tax"), Ok(("", BPF_STMT!(BPF_MISC | BPF_TAX))));
        assert_eq!(misc("txa"), Ok(("", BPF_STMT!(BPF_MISC | BPF_TXA))));
    }

    #[test]
    pub fn parse_labelled() {
        assert_eq!(labelled("drop:"), Ok(("", "drop")));
    }

    #[test]
    pub fn parse_comment() {
        assert_eq!(inline_comment("/* hello */"), Ok(("", " hello ")));
        assert_eq!(inline_comment("; world\n"), Ok(("", " world")));
    }

    #[test]
    pub fn parse_line() {
        let labels = RefCell::new(vec![]);

        assert_eq!(
            line("drop: add #5\n", &labels),
            Ok((
                "\n",
                (Some("drop"), Some(BPF_STMT!(BPF_ALU | BPF_ADD | BPF_K, 5)), None)
            ))
        );
        assert_eq!(
            line("tax", &labels),
            Ok(("", (None, Some(BPF_STMT!(BPF_MISC | BPF_TAX)), None)))
        );

        assert_eq!(
            line("drop: add #5 ; hello\n", &labels),
            Ok((
                "",
                (
                    Some("drop"),
                    Some(BPF_STMT!(BPF_ALU | BPF_ADD | BPF_K, 5)),
                    Some(" hello")
                )
            ))
        );
        assert_eq!(
            line("tax /* world */", &labels),
            Ok(("", (None, Some(BPF_STMT!(BPF_MISC | BPF_TAX)), Some(" world "))))
        );
        assert_eq!(
            line("# hello world\n", &labels),
            Ok(("", (None, None, Some(" hello world"))))
        );
    }

    #[test]
    pub fn parse_prog() {
        let labels = RefCell::new(vec![]);

        assert_eq!(
            prog(
                r#"
ldh [12]
jne #0x800, drop
ldb [23]
jneq #6, drop
ldh [20]
jset #0x1fff, drop
ldxb 4 * ([14] & 0xf)
ldh [x + 14]
jeq #0x16, pass
ldh [x + 16]
jne #0x16, drop
pass: ret #-1
drop: ret #0
"#,
                &labels
            ),
            Ok((
                "\n",
                vec![
                    (None, Some(BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, 12)), None),
                    (None, Some(BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x800, 0, 1)), None),
                    (None, Some(BPF_STMT!(BPF_LD | BPF_B | BPF_ABS, 23)), None),
                    (None, Some(BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 6, 0, 1)), None),
                    (None, Some(BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, 20)), None),
                    (None, Some(BPF_JUMP!(BPF_JMP | BPF_JSET | BPF_K, 0x1FFF, 1, 0)), None),
                    (None, Some(BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, 14)), None),
                    (None, Some(BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 14)), None),
                    (None, Some(BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 2, 0)), None),
                    (None, Some(BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 16)), None),
                    (None, Some(BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0x16, 0, 1)), None),
                    (Some("pass"), Some(BPF_STMT!(BPF_RET | BPF_K, 0xffffffff)), None),
                    (Some("drop"), Some(BPF_STMT!(BPF_RET | BPF_K, 0)), None),
                ]
            ))
        );
    }

    #[test]
    pub fn parse_program() {
        assert_eq!(
            r#"ldh [12]
jne #0x800, drop
ldb [23]
jneq #6, drop
ldh [20]
jset #0x1fff, drop
ldxb 4 * ([14] & 0xf)
ldh [x + 14]
jeq #0x16, pass
ldh [x + 16]
jne #0x16, drop
pass: ret #-1
drop: ret #0
"#
            .parse::<Program>()
            .unwrap(),
            Program(vec![
                Load(Mode::Abs(12, Half)),
                Jmp(Eq(K(0), 0, 10)),
                Load(Mode::Abs(23, Byte)),
                Jmp(Eq(K(0), 0, 8)),
                Load(Mode::Abs(20, Half)),
                Jmp(Set(K(0), 6, 0)),
                LoadX(Mode::Msh(14)),
                Load(Mode::Ind(14, Half)),
                Jmp(Eq(K(0), 2, 0)),
                Load(Mode::Ind(16, Half)),
                Jmp(Eq(K(0), 0, 1)),
                Ret(Some(RVal::K(0xffffffff))),
                Ret(Some(RVal::K(0)))
            ])
        );
    }
}
