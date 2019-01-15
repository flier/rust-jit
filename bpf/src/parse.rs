use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use nom::*;

use crate::raw::*;

named_args!(instr(labels: Rc<RefCell<BTreeMap<String, u8>>>)<&str, bpf_insn>, alt!(
      ldb
    | ldh
	| ldi
	| ldx
	| ldxi
    | ldxb
	| ld
    | st
    | jmp
    | apply!(jcond, labels.clone())
    | apply!(jcond_neg, labels.clone())
    | misc
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
    value: alt!(
        load_value => {
            |(ty, k)| (ty, k)
        } |
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
    tag!("[") >> multispace0 >>
    value: alt!(
        do_parse!(
            opt!(tag!("%")) >> tag!("x") >> multispace0 >>
            tag!("+") >> multispace0 >>
            k: number >> multispace0 >>
            (
                k
            )
        ) => {
            |k| (BPF_IND, k)
        } |
        number => {
            |k| (BPF_ABS, k)
        }
    ) >> multispace0 >>
    tag!("]") >>
    ( value )
));

named!(ld<&str, bpf_insn>, do_parse!(
    tag!("ld") >> multispace1 >>
    insn: alt!(
        imm => {
            |n| BPF_STMT!(BPF_LD | BPF_IMM, n)
        } |
        len => {
            |_| BPF_STMT!(BPF_LD | BPF_W | BPF_LEN)
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
                |_| BPF_STMT!(BPF_LDX | BPF_W | BPF_LEN)
            } |
            mem => {
                |k| BPF_STMT!(BPF_LDX | BPF_MEM, k)
            } |
            load_msh
        ) >>
        (
            insn
        )
    )
);

named!(ldxb<&str, bpf_insn>, preceded!(tag!("ldxb"), preceded!(multispace1, load_msh)));

named!(
    load_msh<&str, bpf_insn>, do_parse!(
        verify!(number, |n| n == 4) >> multispace0 >>
        tag!("*") >>  multispace0 >>
        tag!("(") >> multispace0 >>
        tag!("[") >> multispace0 >>
        k: number >> multispace0 >>
        tag!("]") >> multispace0 >>
        tag!("&") >>  multispace0 >>
        verify!(number, |n| n == 0x0f) >> multispace0 >>
        tag!(")") >>
        (
            BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, k)
        )
    )
);

named_args!(
    take_str_radix(radix: u32)<&str, u32>,
    map_res!(take_while_s!(|c: char| c.is_digit(radix)), |s| u32::from_str_radix(s, radix))
);

// Copy A|X into M[]
named!(st<&str, bpf_insn>, do_parse!(
    code: alt!(
        tag!("stx") => { |_| BPF_STX } |
        tag!("st")  => { |_| BPF_ST }
    ) >> multispace1 >>
    k: mem >>
    (
        BPF_STMT!(code, k)
    )
));

named!(jmp<&str, bpf_insn>, do_parse!(
    tag!("jmp") >> multispace1 >>
    label: label >>
    (
        BPF_STMT!(BPF_JMP | BPF_JA)
    )
));

named_args!(jcond(labels: Rc<RefCell<BTreeMap<String, u8>>>)<&str, bpf_insn>, do_parse!(
    code: alt_complete!(
        tag!("jeq") => { |_| BPF_JEQ } |
        tag!("jgt") => { |_| BPF_JGT } |
        tag!("jge") => { |_| BPF_JGE } |
        tag!("jset") => { |_| BPF_JSET }
    ) >> multispace1 >>
    insn: alt_complete!(
        apply!(then_else, code) => { |(insn, then, or_else)| (insn, then, Some(or_else)) } |
        apply!(then, code) => { |(insn, then)| (insn, then, None) }
    ) >>
    ({
        let (mut insn, then, or_else) = insn;

        let next_id = labels.borrow().len() as u8 + 1;
        insn.jt = *labels.borrow_mut().entry(then.to_owned()).or_insert(next_id);

        if let Some(or_else) = or_else {
            let next_id = labels.borrow().len() as u8 + 1;
            insn.jf = *labels.borrow_mut().entry(or_else.to_owned()).or_insert(next_id);
        }

        insn
    })
));

named_args!(jcond_neg(labels: Rc<RefCell<BTreeMap<String, u8>>>)<&str, bpf_insn>, do_parse!(
    code: alt_complete!(
        tag!("jneq") => { |_| BPF_JEQ } |
        tag!("jne") => { |_| BPF_JEQ } |
        tag!("jlt") => { |_| BPF_JGT } |
        tag!("jle") => { |_| BPF_JGE }
    ) >> multispace1 >>
    insn: apply!(then, code) >>
    ({
        let (mut insn, then) = insn;
        let next_id = labels.borrow().len() as u8 + 1;

        insn.jf = *labels.borrow_mut().entry(then.to_owned()).or_insert(next_id);

        insn
    })
));

named_args!(then_else(code: u32)<&str, (bpf_insn, &str, &str)>, alt_complete!(
    do_parse!(
        k: imm >>
        then: if_clause >>
        or_else: if_clause >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_K, k), then, or_else
        )
    ) |
    do_parse!(
        opt!(tag!("%")) >> tag!("x") >> multispace0 >>
        then: if_clause >>
        or_else: if_clause >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_X), then, or_else
        )
    )
));

named_args!(then(code: u32)<&str, (bpf_insn, &str)>, alt_complete!(
    do_parse!(
        k: imm >>
        then: if_clause >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_K, k), then
        )
    ) |
    do_parse!(
        opt!(tag!("%")) >> tag!("x") >> multispace0 >>
        then: if_clause >>
        (
            BPF_STMT!(BPF_JMP | code | BPF_X), then
        )
    )
));

named!(if_clause<&str, &str>, do_parse!(
    multispace0 >>
    tag!(",") >>
    multispace0 >>
    label: label >>
    (
        label
    )
));

named!(misc<&str, bpf_insn>, alt_complete!(
    tag!("tax") => { |_| BPF_STMT!(BPF_MISC | BPF_TAX) } |
    tag!("txa") => { |_| BPF_STMT!(BPF_MISC | BPF_TXA) }
));

named!(
    imm<&str, u32>, preceded!(tag!("#"), number)
);

named!(
    len<&str, &str>, preceded!(opt!(tag!("#")), alt_complete!(tag!("len") | tag!("pktlen")))
);

named!(
    mem<&str, u32>, do_parse!(
        tag!("M[") >> multispace0 >>
        k: number >> multispace0 >>
        tag!("]") >>
        (
            k
        )
    )
);

named!(
    extension<&str, u32>, do_parse!(
        opt!(tag!("#")) >>
        off: alt_complete!(
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
        ) >>
        (
            off
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

    #[test]
    pub fn parse_number() {
        assert_eq!(number("123\n"), Ok(("\n", 123)));
        assert_eq!(number("0x1f\n"), Ok(("\n", 0x1f)));
        assert_eq!(number("0b01010101\n"), Ok(("\n", 0x55)));
        assert_eq!(number("0777\n"), Ok(("\n", 0o777)));
        assert_eq!(number("0o777\n"), Ok(("\n", 0o777)));
    }

    #[test]
    pub fn parse_ldb() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ldb [x+5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ldb [%x + 5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ldb [5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_ABS, 5)))
        );
        assert_eq!(
            instr("ldb #type", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_B | BPF_ABS, SKF_AD_OFF + SKF_AD_PKTTYPE)))
        );
    }

    #[test]
    pub fn parse_ldh() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ldh [x+5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ldh [%x + 5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ldh [5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, 5)))
        );
        assert_eq!(
            instr("ldh #proto", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_H | BPF_ABS, SKF_AD_OFF + SKF_AD_PROTOCOL)))
        );
    }

    #[test]
    pub fn parse_ldi() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ldi 5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5)))
        );
        assert_eq!(
            instr("ldi #5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5)))
        );
    }

    #[test]
    pub fn parse_ld() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ld #5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LD | BPF_IMM, 5)))
        );

        assert_eq!(
            instr("ld len", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_LEN)))
        );
        assert_eq!(
            instr("ld #pktlen", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_LEN)))
        );

        assert_eq!(
            instr("ld M[5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_MEM, 5)))
        );

        assert_eq!(
            instr("ld [x+5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ld [%x + 5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_IND, 5)))
        );
        assert_eq!(
            instr("ld [5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LD | BPF_W | BPF_ABS, 5)))
        );
    }

    #[test]
    pub fn parse_ldxi() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ldxi 5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5)))
        );
        assert_eq!(
            instr("ldxi #5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5)))
        );
    }

    #[test]
    pub fn parse_ldx() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("ldx #5\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_LDX | BPF_IMM, 5)))
        );

        assert_eq!(
            instr("ldx len", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_W | BPF_LEN)))
        );
        assert_eq!(
            instr("ldx #pktlen", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_W | BPF_LEN)))
        );

        assert_eq!(
            instr("ldx M[5]", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_MEM, 5)))
        );

        assert_eq!(
            instr("ldx 4*([5]&0xf)", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, 5)))
        );
        assert_eq!(
            instr("ldxb 4*([5]&0xf)", labels.clone()),
            Ok(("", BPF_STMT!(BPF_LDX | BPF_MSH | BPF_B, 5)))
        );
    }

    #[test]
    pub fn parse_st() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(instr("st M[5]", labels.clone()), Ok(("", BPF_STMT!(BPF_ST, 5))));
        assert_eq!(instr("stx M[5]", labels.clone()), Ok(("", BPF_STMT!(BPF_STX, 5))));
    }

    #[test]
    pub fn parse_jmp() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("jmp drop\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_JMP | BPF_JA)))
        );
        assert_eq!(
            instr("jmp L3\n", labels.clone()),
            Ok(("\n", BPF_STMT!(BPF_JMP | BPF_JA)))
        );
    }

    #[test]
    pub fn parse_jcond() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(
            instr("jeq #0x16, pass, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 1, 2, 0x16)))
        );
        assert_eq!(
            instr("jgt #0x16, pass, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_K, 1, 2, 0x16)))
        );
        assert_eq!(
            instr("jge x, pass, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 1, 2)))
        );
        assert_eq!(
            instr("jset %x, pass, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JSET | BPF_X, 1, 2)))
        );

        assert_eq!(
            instr("jeq #0x16, pass\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 1, 0, 0x16)))
        );
        assert_eq!(
            instr("jgt #0x16, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_K, 2, 0, 0x16)))
        );
        assert_eq!(
            instr("jge x, pass\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 1, 0)))
        );
        assert_eq!(
            instr("jset %x, pass\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JSET | BPF_X, 1, 0)))
        );

        assert_eq!(
            instr("jneq #0x16, pass\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0, 1, 0x16)))
        );
        assert_eq!(
            instr("jne #0x16, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JEQ | BPF_K, 0, 2, 0x16)))
        );
        assert_eq!(
            instr("jlt x, pass\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGT | BPF_X, 0, 1)))
        );
        assert_eq!(
            instr("jle %x, drop\n", labels.clone()),
            Ok(("\n", BPF_JUMP!(BPF_JMP | BPF_JGE | BPF_X, 0, 2)))
        );
    }

    #[test]
    pub fn parse_misc() {
        let labels = Rc::new(RefCell::new(BTreeMap::new()));

        assert_eq!(instr("tax", labels.clone()), Ok(("", BPF_STMT!(BPF_MISC | BPF_TAX))));
        assert_eq!(instr("txa", labels.clone()), Ok(("", BPF_STMT!(BPF_MISC | BPF_TXA))));
    }
}
