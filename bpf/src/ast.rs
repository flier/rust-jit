use std::iter::IntoIterator;
use std::ops::Deref;
use std::slice;
use std::vec;

/// A filter program is an array of instructions,
/// with all branches forwardly directed, terminated by a return instruction.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Program(pub Vec<Inst>);

impl Deref for Program {
    type Target = [Inst];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IntoIterator for Program {
    type Item = Inst;
    type IntoIter = vec::IntoIter<Inst>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Program {
    type Item = &'a Inst;
    type IntoIter = slice::Iter<'a, Inst>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// A fixed offset
pub type Off = u32;

/// A constant
pub type K = u32;

/// The	data size
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Size {
    #[doc(hidden)]
    Reserved,
    /// byte (u8)
    Byte,
    /// unsigned halfword (u16)
    Half,
    /// word (u32)
    Word,
}

/// The addressing mode
#[derive(Clone, Debug, PartialEq)]
pub enum Mode {
    /// packet data at a fixed offset
    Abs(Off, Size),
    /// packet data at a variable offset
    Ind(Off, Size),
    /// the packet length
    Len,
    /// constant
    Imm(Off),
    /// a word in the scratch memory store
    Mem(Off),
    /// the IP header length
    Msh(Off),
}

/// The source mode
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Src {
    /// a constant K
    K(u32),
    /// the index register X
    X,
}

/// The ALU operations
#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Add(Src),
    Sub(Src),
    Mul(Src),
    Div(Src),
    Or(Src),
    And(Src),
    LShift(Src),
    RShift(Src),
    Neg,
    Mod(Src),
    Xor(Src),
}

/// Conditional jumps compare the accumulator against a constant (BPF_K) or the index register (BPF_X).
#[derive(Clone, Debug, PartialEq)]
pub enum Cond {
    /// Jump to a fixed offset
    Abs(Off),
    /// Jump when `A` greater than the source
    Gt(Src, u8, u8),
    /// Jump when `A` greater than or equals to the source
    Ge(Src, u8, u8),
    /// Jump when `A` equals to the source
    Eq(Src, u8, u8),
    /// Jump when the bits of `A` was set
    Set(Src, u8, u8),
}

/// The return value
#[derive(Clone, Debug, PartialEq)]
pub enum RVal {
    /// a constant K
    K(K),
    /// the accumulator A
    A,
}

/// The MISC operations
#[derive(Clone, Debug, PartialEq)]
pub enum MiscOp {
    /// X <- A
    Tax,
    /// A <- X
    Txa,
}

/// The BPF instruction
#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    /// These instructions copy a value into the accumulator.
    Load(Mode),
    /// These instructions load a value into the index register.
    LoadX(Mode),
    /// This instruction stores the accumulator into the scratch memory.
    Store(Off),
    /// This instruction stores the index register in the scratch memory store.
    StoreX(Off),
    /// The alu instructions perform operations between the accumulator and index register or constant,
    /// and store the result back in the accumulator.
    Alu(Op),
    /// The jump instructions alter flow of control.
    Jmp(Cond),
    /// The return instructions terminate the filter program
    /// and specify the amount of packet to accept (i.e., they return the truncation amount).
    Ret(Option<RVal>),
    /// The miscellaneous category was created for anything
    /// that does not fit into the	above classes,
    /// and for any new instructions that might need to be added.
    Misc(MiscOp),
}
