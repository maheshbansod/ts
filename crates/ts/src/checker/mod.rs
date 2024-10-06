use crate::parser::{PAtom, PExpression, PLiteralPrimitive, PStatement, ParseTree};

pub struct Checker {
    pub errors: Vec<TsError>,
}

impl Checker {
    pub fn new() -> Self {
        Self { errors: vec![] }
    }
    pub fn check(&mut self, tree: ParseTree) -> Vec<TsType> {
        let root = tree.root;
        let mut types = vec![];
        for statement in root.statements {
            let t = match statement {
                PStatement::Expression { expression } => self.expression(expression),
                _ => todo!(),
            };
            types.push(t);
        }
        types
    }

    pub fn expression(&mut self, expression: PExpression) -> TsType {
        match expression {
            PExpression::Atom(atom) => match atom {
                PAtom::Literal(literal) => match literal {
                    PLiteralPrimitive::Number { value, token: _ } => {
                        TsType::LiteralNumber { value }
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
            PExpression::Cons(_operator, args) => {
                // Naive algo just checks if all args have same type

                let mut last_type = None;
                for arg in args {
                    let expr_type = self.expression(arg);
                    if let Some(last_type) = last_type {
                        if !Checker::compare_type(last_type, expr_type) {
                            self.errors.push(TsError {
                                kind: TypeErrorKind::ExpectedType {
                                    got: expr_type,
                                    expected: last_type,
                                },
                            })
                        }
                    } else {
                        last_type = Some(expr_type);
                    }
                }

                last_type.unwrap_or(TsType::Any)
            }
        }
    }

    fn compare_type(a: TsType, b: TsType) -> bool {
        a == b
    }
}

#[derive(Debug)]
pub struct TsError {
    kind: TypeErrorKind,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    ExpectedType { got: TsType, expected: TsType },
}

/// An entity that has a typescript type
pub struct TsTypeHolder {
    kind: TsType,
}

// todo: implement PartialEq manually maybe or i think better would be to remove it and implement different kinds
// of equality either as part of checker of separate functions
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TsType {
    LiteralNumber { value: f32 },
    Any,
}
