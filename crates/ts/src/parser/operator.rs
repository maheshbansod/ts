use std::fmt::Display;

use macros::make_operators;

use crate::tokenizer::{Token, TokenKind};

make_operators!(
    (infix, BinaryAdd, Plus, "+"),
    //     /// The ?: operator
    (infix, Conditional, QuestionMark, "?:"),
    (infix, Divide, Slash, "/"),
    //     /// == comparison operator
    (infix, Equals, Equals, "=="),
    (pre, FunctionCall, Unknown, "CALL"), // todo
    (infix, Multiply, Star, "*"),
    (pre, Negate, Minus, "-"),
    (pre, Not, Exclamation, "!"),
    (infix, NotEquals, NotEquals, "!="),
    (post, PostIncrement, Increment, "++"),
    (pre, PreIncrement, Increment, "++"),
    (infix, Subscript, SquareBracketOpen, "[]"),
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
