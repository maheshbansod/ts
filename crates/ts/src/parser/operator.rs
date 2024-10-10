use std::fmt::Display;

use macros::make_operators;

use crate::tokenizer::{Token, TokenKind};

use super::Parser;

make_operators!(
    (infix, Conditional, QuestionMark, "?:", "The ?: operator"),
    (infix, Equals, Equals, "==", "comparison operator"),
    (infix, BinaryAdd, Plus, "+"),
    (infix, Subtract, Minus, "-"),
    (infix, Divide, Slash, "/"),
    (pre, FunctionCall, None, "CALL"),
    (infix, Multiply, Star, "*"),
    (infix, NotEquals, NotEquals, "!="),
    (post, Subscript, SquareBracketOpen, "[]"),
    (pre, Not, Exclamation, "!"),
    (pre, Negate, Minus, "-"),
    (post, PostIncrement, Increment, "++"),
    (pre, PreIncrement, Increment, "++"),
    (infix, MemberAccess, Dot, "->")
);

#[derive(Debug, PartialEq)]
pub struct POperator<'a> {
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
