use std::collections::HashMap;

use crate::parser::{PAtom, PExpression, PLiteralPrimitive, PStatement, ParseTree};

pub struct Checker<'a> {
    tree: &'a ParseTree<'a>,
    pub errors: Vec<TsError<'a>>,
}

impl<'a> Checker<'a> {
    pub fn new(tree: &'a ParseTree<'a>) -> Self {
        Checker {
            errors: vec![],
            tree,
        }
    }
    pub fn check(mut self) -> (Vec<TsError<'a>>, HashMap<String, TsType<'a>>) {
        let root = &self.tree.root;
        let mut types = HashMap::new();
        for statement in &root.statements {
            match statement {
                PStatement::Expression { expression } => {
                    self.expression(&expression);
                }
                PStatement::Binding {
                    binding_type: _,
                    identifier,
                    value,
                } => {
                    let identifier = identifier.name();
                    if let Some(value) = value {
                        let t_holder = self.expression(&value);
                        types.insert(identifier.to_string(), t_holder.kind);
                    }
                }
                _ => todo!(),
            };
        }

        (self.errors, types)
    }

    pub fn expression<'b>(&mut self, expression: &'a PExpression<'a>) -> TsTypeHolder<'b, 'a> {
        match expression {
            PExpression::Atom(ref atom) => match atom {
                PAtom::Literal(literal) => TsTypeHolder {
                    kind: literal.into(),
                    holding_for: expression,
                },
                _ => todo!(),
            },
            PExpression::Cons(_operator, args) => {
                // Naive algo just checks if all args have same type

                let mut last_type = None;
                for arg in args {
                    let expr_type = self.expression(arg);

                    if let Some(previous_type) = last_type {
                        if let Some(common_type) =
                            Checker::merge_types(previous_type, expr_type.kind)
                        {
                            last_type = Some(common_type);
                        } else {
                            self.errors.push(TsError {
                                kind: TypeErrorKind::ExpectedType {
                                    got: expr_type,
                                    expected: previous_type,
                                },
                            })
                        }
                    } else {
                        last_type = Some(expr_type.kind);
                    }
                }

                let t = last_type.unwrap_or(TsType::Any);
                TsTypeHolder {
                    kind: t,
                    holding_for: &expression,
                }
            }
        }
    }

    fn merge_types(a: TsType<'a>, b: TsType<'a>) -> Option<TsType<'a>> {
        if matches!(a, TsType::Any) || matches!(b, TsType::Any) {
            return Some(TsType::Any);
        }

        match (a, b) {
            (TsType::Literal(literal_a), TsType::Literal(literal_b)) => {
                if literal_a.is_of_type(&literal_b) {
                    Some(literal_a.wider())
                } else {
                    None
                }
            }
            _ => {
                if a.contains(&b) {
                    Some(a)
                } else if b.contains(&a) {
                    Some(b)
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TsError<'a> {
    kind: TypeErrorKind<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorKind<'a> {
    ExpectedType {
        got: TsTypeHolder<'a, 'a>,
        expected: TsType<'a>,
    },
}

/// An entity that has a typescript type
#[derive(Debug, PartialEq)]
pub struct TsTypeHolder<'a, 'b> {
    kind: TsType<'a>,
    holding_for: &'a PExpression<'b>,
}

// todo: implement PartialEq manually maybe or i think better would be to remove it and implement different kinds
// of equality either as part of checker of separate functions
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TsType<'a> {
    Any,
    Literal(TsLiteral<'a>),
    Number,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TsLiteral<'a> {
    Number { value: f32 },
    String { value: &'a str },
}

impl<'a> From<&PLiteralPrimitive<'a>> for TsType<'a> {
    fn from(value: &PLiteralPrimitive<'a>) -> Self {
        let kind = match value {
            PLiteralPrimitive::Number { value, token: _ } => {
                TsType::Literal(TsLiteral::Number { value: *value })
            }
            PLiteralPrimitive::String { value, .. } => TsType::Literal(TsLiteral::String { value }),
        };
        kind
    }
}

impl<'a> TsType<'a> {
    fn contains(&self, b: &TsType<'a>) -> bool {
        match (self, b) {
            (TsType::Any, _) => true,
            (a, TsType::Literal(l)) => *a == l.wider(),
            (a, b) if *a == *b => true,
            _ => false,
        }
    }
}

impl<'a> TsLiteral<'a> {
    fn wider(&self) -> TsType<'a> {
        match self {
            TsLiteral::String { value: _ } => TsType::String,
            TsLiteral::Number { value: _ } => TsType::Number,
        }
    }

    fn is_of_type(&self, b: &TsLiteral<'a>) -> bool {
        match (self, b) {
            (TsLiteral::String { .. }, TsLiteral::String { .. }) => true,
            (TsLiteral::Number { .. }, TsLiteral::Number { .. }) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        checker::{Checker, TsError, TsLiteral, TsType, TsTypeHolder, TypeErrorKind},
        parser::{PExpression, PStatement, ParseTree, Parser},
        tokenizer::Tokenizer,
    };

    #[test]
    fn unit_number() {
        let code = "4";
        let wrapper = parse_expr(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Literal(TsLiteral::Number { value: 4.0 }))
    }

    #[test]
    fn unit_string() {
        let code = "'string'";
        let wrapper = parse_expr(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Literal(TsLiteral::String { value: "string" }))
    }

    #[test]
    fn expr_string() {
        let code = "'string' + 'string'";
        let wrapper = parse_expr(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::String)
    }

    #[test]
    fn expr() {
        let code = "4 + 4";
        let wrapper = parse_expr(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Number)
    }

    #[test]
    fn error_different_types() {
        // TODO: this is not an error - maybe instead of merge types, depend on operator more
        let code = "4 + '4'";
        let tok = Tokenizer::new(code);
        let parser = Parser::new(tok);
        let (tree, errors) = parser.parse().unwrap();
        assert!(errors.is_empty());
        let checker = Checker::new(&tree);
        let (errors, types) = checker.check();
        let expr = match &tree.root.statements[0] {
            PStatement::Expression { expression } => match expression {
                PExpression::Cons(_op, args) => {
                    let rhs = &args[1];
                    rhs
                }
                _ => panic!(),
            },
            _ => panic!(),
        };
        let expected_errors = vec![TsError {
            kind: TypeErrorKind::ExpectedType {
                got: TsTypeHolder {
                    kind: TsType::Literal(TsLiteral::String { value: "4".into() }),
                    holding_for: expr,
                },
                expected: TsType::Literal(TsLiteral::Number { value: 4.0 }),
            },
        }];
        let mut expected_errors = expected_errors.into_iter();
        for error in errors {
            let expected_error = expected_errors.next().unwrap();
            assert_eq!(error, expected_error);
        }
        assert!(types.is_empty());
    }

    fn parse_expr(code: &str) -> TreeWrapper {
        let tok = Tokenizer::new(code);
        let parser = Parser::new(tok);
        let (tree, errors) = parser.parse().unwrap();
        assert!(errors.is_empty());
        TreeWrapper { tree }
    }

    struct TreeWrapper<'a> {
        tree: ParseTree<'a>,
    }
    impl<'a> TreeWrapper<'a> {
        fn check_expr(&self) -> TsType {
            let mut checker = Checker::new(&self.tree);
            let s = match &self.tree.root.statements[0] {
                PStatement::Expression { expression } => expression,
                _ => panic!("nah"),
            };
            let exp = checker.expression(&s);
            exp.kind
        }
    }
}
