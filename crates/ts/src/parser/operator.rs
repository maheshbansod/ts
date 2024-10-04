use std::fmt::Display;

use macros::make_operators;

use crate::tokenizer::{Token, TokenKind};

make_operators!(
    //     BinaryAdd,
    (infix, BinaryAdd, Plus, "+"),
    //     /// The ?: operator
    //     Conditional,
    (infix, Conditional, QuestionMark, "?:"),
    //     Divide,
    (infix, Divide, Slash, "/"),
    //     /// == comparison operator
    //     Equals,
    (infix, Equals, Equals, "=="),
    //     FunctionCall,
    (pre, FunctionCall, Unknown, "CALL"), // todo
    //     Multiply,
    (infix, Multiply, Star, "*"),
    //     Negate,
    (pre, Negate, Minus, "-"),
    //     Not,
    (pre, Not, Exclamation, "!"),
    //     NotEquals,
    (infix, NotEquals, NotEquals, "!="),
    //     PostIncrement,
    (post, PostIncrement, Increment, "++"),
    //     PreIncrement,
    (pre, PreIncrement, Increment, "++"),
    //     Subscript,
    (infix, Subscript, SquareBracketOpen, "[]"),
    //     Subtract,
    (infix, Subtract, Minus, "-"),
);

#[derive(Debug, PartialEq)]
pub(super) struct POperator<'a> {
    pub kind: POperatorKind,
    pub token: Token<'a>,
}

impl<'a> POperator<'a> {
    pub(super) const fn new(kind: POperatorKind, token: Token<'a>) -> Self {
        Self { kind, token }
    }
    pub(super) const fn token_type(&self) -> &TokenKind {
        self.token.token_type()
    }

    pub(super) const fn kind(&self) -> &POperatorKind {
        &self.kind
    }
}

impl<'a> Display for POperator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

// match self.kind() {
//     POperatorKind::BinaryAdd => write!(f, "+"),
//     POperatorKind::Conditional => write!(f, "?:"),
//     POperatorKind::Divide => write!(f, "/"),
//     POperatorKind::Equals => write!(f, "=="),
//     POperatorKind::FunctionCall => write!(f, "CALL"),
//     POperatorKind::Multiply => write!(f, "*"),
//     POperatorKind::Negate | POperatorKind::Subtract => write!(f, "-"),
//     POperatorKind::Not => write!(f, "!"),
//     POperatorKind::NotEquals => write!(f, "!="),
//     POperatorKind::PostIncrement | POperatorKind::PreIncrement => write!(f, "++"),
//     POperatorKind::Subscript => write!(f, "[]"),
// }
