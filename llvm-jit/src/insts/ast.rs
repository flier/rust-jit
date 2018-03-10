use value::{BlockAddress, Instruction};
use constant::*;
use value::*;
use insts::*;
use global::GlobalVar;

#[derive(Clone, Debug, PartialEq)]
pub enum AstNode<'a> {
    ValueRef(ValueRef),
    Instruction(Instruction),
    Constant(Constant),
    // Terminators
    Ret(Ret<'a>),
    Br(Br),
    CondBr(CondBr<'a>),
    Switch(Switch<'a>),
    IndirectBr(IndirectBr<'a>),
    Invoke(Invoke<'a>),
    LandingPad(LandingPad<'a>),
    Resume(Resume<'a>),
    Unreachable(Unreachable),
    // Arithmetic
    Add(Add<'a>),
    NSWAdd(NSWAdd<'a>),
    NUWAdd(NUWAdd<'a>),
    FAdd(FAdd<'a>),
    Sub(Sub<'a>),
    NSWSub(NSWSub<'a>),
    NUWSub(NUWSub<'a>),
    FSub(FSub<'a>),
    Mul(Mul<'a>),
    NSWMul(NSWMul<'a>),
    NUWMul(NUWMul<'a>),
    FMul(FMul<'a>),
    UDiv(UDiv<'a>),
    ExactUDiv(ExactUDiv<'a>),
    SDiv(SDiv<'a>),
    ExactSDiv(ExactSDiv<'a>),
    FDiv(FDiv<'a>),
    URem(URem<'a>),
    SRem(SRem<'a>),
    FRem(FRem<'a>),
    Shl(Shl<'a>),
    LShr(LShr<'a>),
    AShr(AShr<'a>),
    And(And<'a>),
    Or(Or<'a>),
    Xor(Xor<'a>),
    Neg(Neg<'a>),
    NSWNeg(NSWNeg<'a>),
    NUWNeg(NUWNeg<'a>),
    FNeg(FNeg<'a>),
    Not(Not<'a>),
    IsNull(IsNull<'a>),
    IsNotNull(IsNotNull<'a>),
    // Memory
    Malloc(Malloc<'a>),
    Free(Free<'a>),
    Alloca(Alloca<'a>),
    Load(Load<'a>),
    Store(Store<'a>),
    GetElementPtr(GetElementPtr<'a>),
    GlobalString(GlobalString<'a>),
    GlobalStringPtr(GlobalStringPtr<'a>),
    // Casts
    Trunc(Trunc<'a>),
    ZExt(ZExt<'a>),
    SExt(SExt<'a>),
    FPToUI(FPToUI<'a>),
    FPToSI(FPToSI<'a>),
    UIToFP(UIToFP<'a>),
    SIToFP(SIToFP<'a>),
    FPTrunc(FPTrunc<'a>),
    FPExt(FPExt<'a>),
    PtrToInt(PtrToInt<'a>),
    IntToPtr(IntToPtr<'a>),
    BitCast(BitCast<'a>),
    ZExtOrBitCast(ZExtOrBitCast<'a>),
    SExtOrBitCast(SExtOrBitCast<'a>),
    TruncOrBitCast(TruncOrBitCast<'a>),
    PointerCast(PointerCast<'a>),
    IntCast(IntCast<'a>),
    FPCast(FPCast<'a>),
    // Comparisons
    ICmp(ICmp<'a>),
    FCmp(FCmp<'a>),
    // Miscellaneous instructions
    Phi(Phi<'a>),
    Call(Call<'a>),
    Select(Select<'a>),
    VaArg(VaArg<'a>),
    ExtractElement(ExtractElement<'a>),
    InsertElement(InsertElement<'a>),
    ShuffleVector(ShuffleVector<'a>),
    ExtractValue(ExtractValue<'a>),
    InsertValue(InsertValue<'a>),
    Fence(Fence),
    AtomicRMW(AtomicRMW<'a>),
    AtomicCmpXchg(AtomicCmpXchg<'a>),
}

impl<'a> InstructionBuilder for AstNode<'a> {
    type Target = ValueRef;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        match self {
            AstNode::ValueRef(node) => node,
            AstNode::Instruction(node) => node.into(),
            AstNode::Constant(node) => node.into(),
            AstNode::Ret(node) => node.emit_to(builder).into(),
            AstNode::Br(node) => node.emit_to(builder).into(),
            AstNode::CondBr(node) => node.emit_to(builder).into(),
            AstNode::Switch(node) => node.emit_to(builder).into(),
            AstNode::IndirectBr(node) => node.emit_to(builder).into(),
            AstNode::Invoke(node) => node.emit_to(builder).into(),
            AstNode::LandingPad(node) => node.emit_to(builder).into(),
            AstNode::Resume(node) => node.emit_to(builder).into(),
            AstNode::Unreachable(node) => node.emit_to(builder).into(),
            AstNode::Add(node) => node.emit_to(builder).into(),
            AstNode::NSWAdd(node) => node.emit_to(builder).into(),
            AstNode::NUWAdd(node) => node.emit_to(builder).into(),
            AstNode::FAdd(node) => node.emit_to(builder).into(),
            AstNode::Sub(node) => node.emit_to(builder).into(),
            AstNode::NSWSub(node) => node.emit_to(builder).into(),
            AstNode::NUWSub(node) => node.emit_to(builder).into(),
            AstNode::FSub(node) => node.emit_to(builder).into(),
            AstNode::Mul(node) => node.emit_to(builder).into(),
            AstNode::NSWMul(node) => node.emit_to(builder).into(),
            AstNode::NUWMul(node) => node.emit_to(builder).into(),
            AstNode::FMul(node) => node.emit_to(builder).into(),
            AstNode::UDiv(node) => node.emit_to(builder).into(),
            AstNode::ExactUDiv(node) => node.emit_to(builder).into(),
            AstNode::SDiv(node) => node.emit_to(builder).into(),
            AstNode::ExactSDiv(node) => node.emit_to(builder).into(),
            AstNode::FDiv(node) => node.emit_to(builder).into(),
            AstNode::URem(node) => node.emit_to(builder).into(),
            AstNode::SRem(node) => node.emit_to(builder).into(),
            AstNode::FRem(node) => node.emit_to(builder).into(),
            AstNode::Shl(node) => node.emit_to(builder).into(),
            AstNode::LShr(node) => node.emit_to(builder).into(),
            AstNode::AShr(node) => node.emit_to(builder).into(),
            AstNode::And(node) => node.emit_to(builder).into(),
            AstNode::Or(node) => node.emit_to(builder).into(),
            AstNode::Xor(node) => node.emit_to(builder).into(),
            AstNode::Neg(node) => node.emit_to(builder).into(),
            AstNode::NSWNeg(node) => node.emit_to(builder).into(),
            AstNode::NUWNeg(node) => node.emit_to(builder).into(),
            AstNode::FNeg(node) => node.emit_to(builder).into(),
            AstNode::Not(node) => node.emit_to(builder).into(),
            AstNode::IsNull(node) => node.emit_to(builder).into(),
            AstNode::IsNotNull(node) => node.emit_to(builder).into(),
            AstNode::Malloc(node) => node.emit_to(builder).into(),
            AstNode::Free(node) => node.emit_to(builder).into(),
            AstNode::Alloca(node) => node.emit_to(builder).into(),
            AstNode::Load(node) => node.emit_to(builder).into(),
            AstNode::Store(node) => node.emit_to(builder).into(),
            AstNode::GetElementPtr(node) => node.emit_to(builder).into(),
            AstNode::GlobalString(node) => node.emit_to(builder).into(),
            AstNode::GlobalStringPtr(node) => node.emit_to(builder).into(),
            AstNode::Trunc(node) => node.emit_to(builder).into(),
            AstNode::ZExt(node) => node.emit_to(builder).into(),
            AstNode::SExt(node) => node.emit_to(builder).into(),
            AstNode::FPToUI(node) => node.emit_to(builder).into(),
            AstNode::FPToSI(node) => node.emit_to(builder).into(),
            AstNode::UIToFP(node) => node.emit_to(builder).into(),
            AstNode::SIToFP(node) => node.emit_to(builder).into(),
            AstNode::FPTrunc(node) => node.emit_to(builder).into(),
            AstNode::FPExt(node) => node.emit_to(builder).into(),
            AstNode::PtrToInt(node) => node.emit_to(builder).into(),
            AstNode::IntToPtr(node) => node.emit_to(builder).into(),
            AstNode::BitCast(node) => node.emit_to(builder).into(),
            AstNode::ZExtOrBitCast(node) => node.emit_to(builder).into(),
            AstNode::SExtOrBitCast(node) => node.emit_to(builder).into(),
            AstNode::TruncOrBitCast(node) => node.emit_to(builder).into(),
            AstNode::PointerCast(node) => node.emit_to(builder).into(),
            AstNode::IntCast(node) => node.emit_to(builder).into(),
            AstNode::FPCast(node) => node.emit_to(builder).into(),
            AstNode::ICmp(node) => node.emit_to(builder).into(),
            AstNode::FCmp(node) => node.emit_to(builder).into(),
            AstNode::Phi(node) => node.emit_to(builder).into(),
            AstNode::Call(node) => node.emit_to(builder).into(),
            AstNode::Select(node) => node.emit_to(builder).into(),
            AstNode::VaArg(node) => node.emit_to(builder).into(),
            AstNode::ExtractElement(node) => node.emit_to(builder).into(),
            AstNode::InsertElement(node) => node.emit_to(builder).into(),
            AstNode::ShuffleVector(node) => node.emit_to(builder).into(),
            AstNode::ExtractValue(node) => node.emit_to(builder).into(),
            AstNode::InsertValue(node) => node.emit_to(builder).into(),
            AstNode::Fence(node) => node.emit_to(builder).into(),
            AstNode::AtomicRMW(node) => node.emit_to(builder).into(),
            AstNode::AtomicCmpXchg(node) => node.emit_to(builder).into(),
        }
    }
}

macro_rules! impl_ast_node {
    ($name:ident) => {
        impl<'a> From<$name> for AstNode<'a> {
            fn from(node: $name) -> Self {
                AstNode::$name(node)
            }
        }
    };
    ($name:ident <'a>) => {
        impl<'a> From<$name<'a>> for AstNode<'a> {
            fn from(node: $name<'a>) -> Self {
                AstNode::$name(node)
            }
        }
    };
    ($from:ident => $name:ident) => {
        impl<'a> From<$from> for AstNode<'a> {
            fn from(node: $from) -> Self {
                AstNode::$name(node.into())
            }
        }
    };
}

impl_ast_node!(ValueRef);
impl_ast_node!(BlockAddress => ValueRef);
impl_ast_node!(Constant);
impl_ast_node!(ConstantInt => Constant);
impl_ast_node!(ConstantFP => Constant);
impl_ast_node!(ConstantString => Constant);
impl_ast_node!(ConstantStruct => Constant);
impl_ast_node!(ConstantArray => Constant);
impl_ast_node!(ConstantVector => Constant);
impl_ast_node!(GlobalVar => Constant);
impl_ast_node!(InlineAsm => Constant);
impl_ast_node!(Instruction);
impl_ast_node!(AllocaInst => Instruction);
impl_ast_node!(BranchInst => Instruction);
impl_ast_node!(CallInst => Instruction);
impl_ast_node!(ICmpInst => Instruction);
impl_ast_node!(FCmpInst => Instruction);
impl_ast_node!(LoadInst => Instruction);
impl_ast_node!(StoreInst => Instruction);
impl_ast_node!(GetElementPtrInst => Instruction);
impl_ast_node!(InvokeInst => Instruction);
impl_ast_node!(PhiNode => Instruction);
impl_ast_node!(ReturnInst => Instruction);
impl_ast_node!(SwitchInst => Instruction);
impl_ast_node!(LandingPadInst => Instruction);
impl_ast_node!(Ret<'a>);
impl_ast_node!(Br);
impl_ast_node!(CondBr<'a>);
impl_ast_node!(Switch<'a>);
impl_ast_node!(IndirectBr<'a>);
impl_ast_node!(Invoke<'a>);
impl_ast_node!(LandingPad<'a>);
impl_ast_node!(Resume<'a>);
impl_ast_node!(Unreachable);
impl_ast_node!(Add<'a>);
impl_ast_node!(NSWAdd<'a>);
impl_ast_node!(NUWAdd<'a>);
impl_ast_node!(FAdd<'a>);
impl_ast_node!(Sub<'a>);
impl_ast_node!(NSWSub<'a>);
impl_ast_node!(NUWSub<'a>);
impl_ast_node!(FSub<'a>);
impl_ast_node!(Mul<'a>);
impl_ast_node!(NSWMul<'a>);
impl_ast_node!(NUWMul<'a>);
impl_ast_node!(FMul<'a>);
impl_ast_node!(UDiv<'a>);
impl_ast_node!(ExactUDiv<'a>);
impl_ast_node!(SDiv<'a>);
impl_ast_node!(ExactSDiv<'a>);
impl_ast_node!(FDiv<'a>);
impl_ast_node!(URem<'a>);
impl_ast_node!(SRem<'a>);
impl_ast_node!(FRem<'a>);
impl_ast_node!(Shl<'a>);
impl_ast_node!(LShr<'a>);
impl_ast_node!(AShr<'a>);
impl_ast_node!(And<'a>);
impl_ast_node!(Or<'a>);
impl_ast_node!(Xor<'a>);
impl_ast_node!(Neg<'a>);
impl_ast_node!(NSWNeg<'a>);
impl_ast_node!(NUWNeg<'a>);
impl_ast_node!(FNeg<'a>);
impl_ast_node!(Not<'a>);
impl_ast_node!(IsNull<'a>);
impl_ast_node!(IsNotNull<'a>);
impl_ast_node!(Malloc<'a>);
impl_ast_node!(Free<'a>);
impl_ast_node!(Alloca<'a>);
impl_ast_node!(Load<'a>);
impl_ast_node!(Store<'a>);
impl_ast_node!(GetElementPtr<'a>);
impl_ast_node!(GlobalString<'a>);
impl_ast_node!(GlobalStringPtr<'a>);
impl_ast_node!(Trunc<'a>);
impl_ast_node!(ZExt<'a>);
impl_ast_node!(SExt<'a>);
impl_ast_node!(FPToUI<'a>);
impl_ast_node!(FPToSI<'a>);
impl_ast_node!(UIToFP<'a>);
impl_ast_node!(SIToFP<'a>);
impl_ast_node!(FPTrunc<'a>);
impl_ast_node!(FPExt<'a>);
impl_ast_node!(PtrToInt<'a>);
impl_ast_node!(IntToPtr<'a>);
impl_ast_node!(BitCast<'a>);
impl_ast_node!(ZExtOrBitCast<'a>);
impl_ast_node!(SExtOrBitCast<'a>);
impl_ast_node!(TruncOrBitCast<'a>);
impl_ast_node!(PointerCast<'a>);
impl_ast_node!(IntCast<'a>);
impl_ast_node!(FPCast<'a>);
impl_ast_node!(ICmp<'a>);
impl_ast_node!(FCmp<'a>);
impl_ast_node!(Phi<'a>);
impl_ast_node!(Call<'a>);
impl_ast_node!(Select<'a>);
impl_ast_node!(VaArg<'a>);
impl_ast_node!(ExtractElement<'a>);
impl_ast_node!(InsertElement<'a>);
impl_ast_node!(ShuffleVector<'a>);
impl_ast_node!(ExtractValue<'a>);
impl_ast_node!(InsertValue<'a>);
impl_ast_node!(Fence);
impl_ast_node!(AtomicRMW<'a>);
impl_ast_node!(AtomicCmpXchg<'a>);
