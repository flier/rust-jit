use std::ops::{Deref, DerefMut};

use crate::insts::AstNode;

#[derive(Clone, Debug)]
struct AstNodes<'a>(Vec<AstNode<'a>>);

impl<'a> Default for AstNodes<'a> {
    fn default() -> Self {
        AstNodes(vec![])
    }
}

impl<'a> Deref for AstNodes<'a> {
    type Target = Vec<AstNode<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for AstNodes<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[macro_export]
macro_rules! ir {
    ($($tt:tt)*) => {
        {
            let mut _s = $crate::insts::ir::AstNodes::default();
            ir_each_token!(_s $($tt)*);
            _s
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! ir_each_token {
    ($nodes:ident) => {};

    ($nodes:ident ret void $($rest:tt)*) => {
        $nodes.push(AstNode::Ret(Ret::void()));

        ir_each_token!($nodes $($rest)*);
    };

    ($nodes:ident ret # $name:ident $($rest:tt)*) => {
        $nodes.push(AstNode::Ret(Ret::new( $name )));

        ir_each_token!($nodes $($rest)*);
    };

    ($nodes:ident ret # { $($inner:tt)* } $($rest:tt)*) => {
        $nodes.push(AstNode::Ret(Ret::new( $($inner)* )));

        ir_each_token!($nodes $($rest)*);
    };
}

#[cfg(test)]
mod tests {
    use crate::constant::ConstantInts;
    use crate::context::Context;
    use crate::insts::*;
    use crate::types::IntegerTypes;

    #[test]
    fn ir_code() {
        let ctxt = Context::new();

        let i8_t = ctxt.int8_t();
        let i123 = i8_t.int(123);

        assert_eq!(
            ir! {
                ret void
                ret #i123
                ret #{ i8_t.int(123) }
            }
            .as_slice(),
            &[
                AstNode::Ret(Ret::void()),
                AstNode::Ret(Ret::new(i123)),
                AstNode::Ret(Ret::new(i123))
            ]
        );
    }
}
