mod scope;

use std::fmt::Display;

use scope::{TsScope, TsSymbol};

use crate::parser::{PAtom, PExpression, PLiteralPrimitive, PStatement, ParseTree};

pub struct Checker<'a> {
    tree: &'a ParseTree<'a>,
    pub errors: Vec<TsError<'a>>,
    scopes: Vec<TsScope<'a>>,
}

impl<'a> Checker<'a> {
    pub const fn new(tree: &'a ParseTree<'a>) -> Self {
        Checker {
            errors: vec![],
            tree,
            scopes: vec![],
        }
    }
    pub fn check(mut self) -> (Vec<TsError<'a>>, TsScope<'a>) {
        let root = &self.tree.root;
        self.add_scope();
        for statement in &root.statements {
            match statement {
                PStatement::Expression { expression } => {
                    self.expression(expression);
                }
                PStatement::Binding {
                    binding_type,
                    identifier,
                    value,
                } => {
                    let identifier_name = identifier.name();
                    if let Some(value) = value {
                        let ts_type = self.expression(value);
                        let sym = TsSymbol::new(binding_type, identifier, ts_type);
                        if let Err(e) = self.add_to_scope(&identifier_name.to_string(), sym) {
                            self.errors.push(e)
                        }
                    }
                }
                _ => todo!(),
            };
        }

        let scope = self.drop_scope().unwrap(); // todo: later only return exported ones i guess

        (self.errors, scope)
    }

    /// Resolves type of an expression
    pub fn expression<'b>(&mut self, expression: &'b PExpression<'a>) -> TsTypeHolder<'a, 'b> {
        match expression {
            PExpression::Atom(ref atom) => match atom {
                PAtom::Literal(literal) => TsTypeHolder {
                    kind: literal.into(),
                    holding_for: expression,
                },
                PAtom::Identifier(identifier) => {
                    // let's check the type of this identifier!
                    let default_type = TsTypeHolder {
                        kind: TsType::Any,
                        holding_for: expression,
                    };
                    return self
                        .current_scope_variable(&identifier.to_string())
                        .unwrap_or(default_type);
                }
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
                            });
                        }
                    } else {
                        last_type = Some(expr_type.kind);
                    }
                }

                let t = last_type.unwrap_or(TsType::Any);
                TsTypeHolder {
                    kind: t,
                    holding_for: expression,
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

    fn current_scope_variable(&self, id: &str) -> Option<TsTypeHolder<'a, 'a>> {
        if let Some(scope) = self.scopes.last() {
            let symbols = scope.symbols();
            if let Some(symbol) = symbols.get(id) {
                let t = symbol.ts_type();
                return Some(TsTypeHolder {
                    kind: t.kind,
                    holding_for: t.holding_for,
                });
            }
        }
        None
    }

    fn add_scope(&mut self) {
        self.scopes.push(TsScope::default())
    }

    fn drop_scope(&mut self) -> Option<TsScope<'a>> {
        self.scopes.pop()
    }

    fn add_to_scope(&mut self, id: &str, symbol: TsSymbol<'a>) -> Result<(), TsError<'a>> {
        let scope = self
            .scopes
            .last_mut()
            .expect("A scope must always be present");
        if scope.exists(id) {
            if symbol.is_redeclarable() {
                scope.add_symbol(id, symbol);
            } else {
                return Err(TsError {
                    kind: TypeErrorKind::RedeclareBlockScoped { symbol },
                });
            }
        } else {
            scope.add_symbol(id, symbol);
        }

        Ok(())
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
    RedeclareBlockScoped {
        symbol: TsSymbol<'a>,
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

impl<'a> Display for TsType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TsType::Any => write!(f, "any"),
            TsType::Number => write!(f, "number"),
            TsType::String => write!(f, "string"),
            TsType::Literal(literal) => write!(f, "{literal}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TsLiteral<'a> {
    Number { value: f32 },
    String { value: &'a str },
}

impl<'a> Display for TsLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TsLiteral::Number { value } => write!(f, "{value}"),
            TsLiteral::String { value } => write!(f, "`{value}`"), // TODO: we might wanna escape
                                                                   // backticks
        }
    }
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
    fn contains(&self, b: &Self) -> bool {
        match (self, b) {
            (TsType::Any, _) => true,
            (a, TsType::Literal(l)) => *a == l.wider(),
            (a, b) if *a == *b => true,
            _ => false,
        }
    }
}

impl<'a> TsLiteral<'a> {
    const fn wider(&self) -> TsType<'a> {
        match self {
            TsLiteral::String { value: _ } => TsType::String,
            TsLiteral::Number { value: _ } => TsType::Number,
        }
    }

    const fn is_of_type(&self, b: &Self) -> bool {
        match (self, b) {
            (TsLiteral::String { .. }, TsLiteral::String { .. }) => true,
            (TsLiteral::Number { .. }, TsLiteral::Number { .. }) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use crate::{
        checker::{Checker, TsError, TsLiteral, TsType, TsTypeHolder, TypeErrorKind},
        parser::{PExpression, PStatement, ParseTree, Parser},
        tokenizer::Tokenizer,
    };

    use super::scope::TsScope;

    #[test]
    fn unit_number() {
        let code = "4";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Literal(TsLiteral::Number { value: 4.0 }))
    }

    #[test]
    fn unit_string() {
        let code = "'string'";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Literal(TsLiteral::String { value: "string" }))
    }

    #[test]
    fn expr_string() {
        let code = "'string' + 'string'";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::String)
    }

    #[test]
    fn expr() {
        let code = "4 + 4";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert_eq!(t, TsType::Number)
    }

    #[test]
    fn error_different_types() {
        // TODO: this is not an error - maybe instead of merge types, depend on operator more
        let code = "4 + '4'";
        let tree_wrapper = make_parse_tree(code);
        let (errors, scope) = tree_wrapper.ts_check();
        let expr = match &tree_wrapper.tree.root.statements[0] {
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
        assert!(scope.symbols().is_empty());
    }

    #[test]
    fn binding_types() {
        let code = "
let a = 4 + 4;
let b = 'star'";
        let tree_wrapper = make_parse_tree(code);
        let (errors, scope) = tree_wrapper.ts_check();
        assert!(errors.is_empty());
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        expected_types.insert("b".to_string(), "let b: `star`");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 2);
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn ident_types() {
        let code = "
let a = 4 + 4;
let b = a";
        let tree_wrapper = make_parse_tree(code);
        let (errors, scope) = tree_wrapper.ts_check();
        assert!(errors.is_empty());
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        expected_types.insert("b".to_string(), "let b: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 2);
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    fn make_parse_tree(code: &str) -> TreeWrapper {
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

        fn ts_check<'b>(&'b self) -> (Vec<TsError<'b>>, TsScope<'b>) {
            let tree = &self.tree;
            let checker = Checker::new(tree);
            let (errors, scope) = checker.check();
            (errors, scope)
        }
    }

    #[test]
    fn error_redeclared_let() {
        let code = "
let a = 4 + 4;
let a = a";
        let wrapper = make_parse_tree(code);
        let (errors, scope) = wrapper.ts_check();
        assert_eq!(errors.len(), 1);
        match errors[0].kind {
            TypeErrorKind::RedeclareBlockScoped { symbol: _ } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 1);
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }
}
