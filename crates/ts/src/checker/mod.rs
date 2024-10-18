mod object;
mod scope;

use std::fmt::Display;

use object::TsObjectLiteral;
use scope::{TsScope, TsSymbol};

use crate::{
    parser::{
        BindingType, PAtom, PExpression, PIdentifier, PJsExpression, PLiteralPrimitive, POperator,
        POperatorKind, PStatement, ParseTree,
    },
    tokenizer::Token,
};

#[cfg(feature = "ts")]
use crate::parser::{PTsAtom, PTsExpression};

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

        let scope = self.block(&root.statements);

        (self.errors, scope)
    }

    fn block(&mut self, statements: &'a [PStatement<'a>]) -> TsScope<'a> {
        self.add_scope();
        for statement in statements {
            self.statement(statement);
        }
        self.drop_scope().unwrap()
    }

    fn statement(&mut self, statement: &'a PStatement<'a>) -> () {
        match statement {
            PStatement::Expression { expression } => {
                self.expression(expression);
            }
            PStatement::Binding {
                binding_type,
                identifier,
                value,
                #[cfg(feature = "ts")]
                ts_type,
            } => {
                let identifier_name = identifier.name();
                #[cfg(feature = "ts")]
                let ts_type = ts_type.as_ref().map(|t| self.expression(t));
                let final_type = if let Some(value) = value {
                    let mut rhs_type = self.expression(value);
                    let rhs_kind = rhs_type.kind.clone();
                    #[cfg(feature = "ts")]
                    if let Some(ts_type) = &ts_type {
                        if !ts_type.kind.contains(&rhs_kind) {
                            self.errors.push(TsError {
                                kind: TypeErrorKind::ExpectedType {
                                    got: rhs_type.clone(),
                                    expected: ts_type.kind.clone(),
                                },
                            })
                        }
                    }
                    if matches!(binding_type, BindingType::Let) {
                        if let TsType::Literal(literal) = rhs_kind {
                            rhs_type.kind = literal.wider();
                        }
                    }
                    Some(rhs_type)
                } else {
                    None
                };
                #[cfg(feature = "ts")]
                let final_type = if let Some(ts_type) = ts_type {
                    Some(ts_type)
                } else if let Some(final_type) = final_type {
                    Some(final_type)
                } else {
                    None
                };
                if let Some(ts_type) = final_type {
                    let sym = TsSymbol::new(binding_type, identifier, ts_type);
                    if let Err(e) = self.add_to_scope(identifier_name, sym) {
                        self.errors.push(e);
                    }
                }
            }
            PStatement::Block { statements } => {
                self.block(statements);
            }
            _ => todo!(),
        };
    }

    /// Resolves type of an expression
    pub fn expression<'b>(&mut self, expression: &'b PExpression<'a>) -> TsTypeHolder<'a, 'b> {
        match expression {
            PExpression::Js(exp) => match exp {
                PJsExpression::Atom(ref atom) => match atom {
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
                        if let Some(t) = self.current_scope_variable_type(&identifier.to_string()) {
                            t.clone()
                        } else {
                            self.errors.push(TsError {
                                kind: TypeErrorKind::UnknownIdentifier {
                                    identifier: identifier.token().clone(),
                                },
                            });
                            default_type
                        }
                    }
                    PAtom::ObjectLiteral(object) => {
                        let object = self.object(object);
                        TsTypeHolder {
                            kind: object,
                            holding_for: expression,
                        }
                    }
                    PAtom::Function(_) => todo!(),
                },
                PJsExpression::Cons(operator, args) => {
                    let t = self.resolve_operation(operator, args);

                    let t = t.unwrap_or(TsType::Any);
                    TsTypeHolder {
                        kind: t.clone(),
                        holding_for: expression,
                    }
                }
            },
            #[cfg(feature = "ts")]
            PExpression::Ts(ts_exp) => TsTypeHolder {
                kind: TsType::from_type_expression(&ts_exp),
                holding_for: &expression,
            },
        }
    }

    fn resolve_operation<'b>(
        &mut self,
        operator: &'b POperator<'a>,
        args: &'a Vec<PExpression<'a>>,
    ) -> Option<TsType<'a>> {
        let (mut errors, ts_type) = {
            let mut errors = vec![];
            let ts_type = if operator.kind == POperatorKind::Assign {
                // can't match to a wider type only narrower type
                let lhs = &args[0];
                let rhs = &args[1];
                let rhs_type = self.expression(rhs);

                let default_type = TsTypeHolder {
                    kind: TsType::Any,
                    holding_for: lhs,
                };

                if let Some((is_reassignable, lhs_kind)) = match lhs {
                    PExpression::Js(PJsExpression::Atom(PAtom::Identifier(identifier))) => {
                        Some(self.current_scope_variable(&identifier.to_string()).map_or(
                            (true, default_type.kind),
                            |symbol| {
                                let ts_info = symbol.ts_type();
                                let is_reassignable = symbol.is_reassignable();
                                if !is_reassignable {
                                    errors.push(TsError {
                                        kind: TypeErrorKind::ReassignConstant {
                                            symbol: symbol.clone(),
                                            assign_token: operator.token.clone(),
                                        },
                                    });
                                }
                                (is_reassignable, ts_info.kind.clone())
                            },
                        ))
                    }
                    PExpression::Js(PJsExpression::Cons(
                        POperator {
                            kind: POperatorKind::MemberAccess,
                            ..
                        },
                        _,
                    )) => {
                        let lhs_type = self.expression(lhs);
                        Some((true, lhs_type.kind))
                    }
                    _ => {
                        errors.push(TsError {
                            kind: TypeErrorKind::InvalidLvalue {
                                assign_token: operator.token.clone(),
                            },
                        });
                        None
                    }
                } {
                    if is_reassignable && !lhs_kind.contains(&rhs_type.kind) {
                        errors.push(TsError {
                            kind: TypeErrorKind::ExpectedType {
                                got: rhs_type.non_const(),
                                expected: lhs_kind.clone(),
                            },
                        });
                    }
                    Some(lhs_kind.clone())
                } else {
                    None
                }
            } else if operator.kind == POperatorKind::BinaryAdd {
                let lhs = &args[0];
                let rhs = &args[1];
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let lhs_is_number = TsType::Number.contains(&lhs.kind);
                let rhs_is_number = TsType::Number.contains(&rhs.kind);
                let lhs_is_string = TsType::String.contains(&lhs.kind);
                let rhs_is_string = TsType::String.contains(&rhs.kind);
                if lhs_is_number && rhs_is_number {
                    Some(TsType::Number)
                } else if (lhs_is_number || lhs_is_string) && (rhs_is_number || rhs_is_string) {
                    Some(TsType::String)
                } else {
                    errors.push(TsError {
                        kind: TypeErrorKind::InvalidOperands {
                            operator_token: operator.token.clone(),
                            operands: vec![lhs, rhs],
                        },
                    });
                    None
                }
            } else if operator.kind == POperatorKind::MemberAccess {
                let lhs = &args[0];
                let rhs = &args[1];

                let lhs_type = self.expression(lhs);
                match rhs {
                    PExpression::Js(PJsExpression::Atom(PAtom::Identifier(ident))) => {
                        lhs_type.kind.resolve_member_access_type(ident)
                    }
                    _ => panic!(
                        "this would never happen, should we represent expressions another way?"
                    ),
                }
            } else {
                // Naive algo just checks if all args have same type

                let mut last_type = None;
                for arg in args {
                    let expr_type = self.expression(arg);

                    if let Some(previous_type) = &last_type {
                        if let Some(common_type) =
                            Checker::merge_types(previous_type, &expr_type.kind)
                        {
                            last_type = Some(common_type);
                        } else {
                            self.errors.push(TsError {
                                kind: TypeErrorKind::ExpectedType {
                                    got: expr_type,
                                    expected: previous_type.clone(),
                                },
                            });
                        }
                    } else {
                        last_type = Some(expr_type.kind);
                    }
                }
                last_type
            };
            (errors, ts_type)
        };
        self.errors.append(&mut errors);
        ts_type
    }

    fn merge_types(a: &TsType<'a>, b: &TsType<'a>) -> Option<TsType<'a>> {
        if matches!(a, TsType::Any) || matches!(b, TsType::Any) {
            return Some(TsType::Any);
        }

        match (a, b) {
            (TsType::Literal(literal_a), TsType::Literal(literal_b)) => {
                if literal_a.is_of_type(literal_b) {
                    Some(literal_a.wider())
                } else {
                    None
                }
            }
            _ => {
                if a.contains(b) {
                    Some(a.clone())
                } else if b.contains(a) {
                    Some(b.clone())
                } else {
                    None
                }
            }
        }
    }

    fn current_scope_variable(&self, id: &str) -> Option<&TsSymbol<'a>> {
        let mut scopes = self.scopes.iter().rev();
        while let Some(scope) = scopes.next() {
            let symbols = scope.symbols();
            if let Some(symbol) = symbols.get(id) {
                return Some(symbol);
            }
        }
        None
    }

    fn current_scope_variable_type(&self, id: &str) -> Option<&TsTypeHolder<'a, 'a>> {
        if let Some(symbol) = self.current_scope_variable(id) {
            let ts_info = symbol.ts_type();
            return Some(ts_info);
        }
        None
    }

    fn add_scope(&mut self) {
        self.scopes.push(TsScope::default());
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

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsError<'a> {
    kind: TypeErrorKind<'a>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeErrorKind<'a> {
    ExpectedType {
        got: TsTypeHolder<'a, 'a>,
        expected: TsType<'a>,
    },
    InvalidLvalue {
        assign_token: Token<'a>,
    },
    InvalidOperands {
        operands: Vec<TsTypeHolder<'a, 'a>>,
        operator_token: Token<'a>,
    },
    ReassignConstant {
        symbol: TsSymbol<'a>,
        assign_token: Token<'a>,
    },
    RedeclareBlockScoped {
        symbol: TsSymbol<'a>,
    },
    UnknownIdentifier {
        identifier: Token<'a>,
    },
}

impl<'a> Display for TsError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a> Display for TypeErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeErrorKind::ExpectedType { got, expected } => {
                write!(
                    f,
                    "{}: Type '{}' is not assignable to type '{}'.",
                    got.holding_for.one_token().location(),
                    got.kind,
                    expected
                )
            }
            TypeErrorKind::InvalidLvalue { assign_token } => {
                write!(f, "{}: The left-hand side of an assignment expression must be a variable or a property access.", assign_token.location())
            }
            TypeErrorKind::InvalidOperands {
                operator_token,
                operands,
            } => {
                let first_operand = operands[0].clone();
                write!(
                    f,
                    "{}: Operator '{}' cannot be applied to types ",
                    first_operand.holding_for.one_token().location(),
                    operator_token.lexeme()
                )?;
                let (last, all_except_last) = operands.split_last().unwrap();
                let all_except_last = all_except_last
                    .iter()
                    .map(|t| format!("'{}'", t.kind.non_const()))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{all_except_last}")?;
                write!(f, " and '{}'.", last.kind.non_const())?;
                Ok(())
            }
            TypeErrorKind::ReassignConstant {
                symbol,
                assign_token,
            } => {
                write!(
                    f,
                    "{}: Cannot assign to '{}' because it is a constant.",
                    assign_token.location(),
                    symbol.name()
                )
            }
            TypeErrorKind::RedeclareBlockScoped { symbol } => {
                write!(
                    f,
                    "{}: Cannot redeclare block scoped variable '{}'.",
                    symbol.location(),
                    symbol.name()
                )
            }
            TypeErrorKind::UnknownIdentifier { identifier } => write!(
                f,
                "{}: Cannot find name '{}'.",
                identifier.location(),
                identifier.lexeme()
            ),
        }
    }
}

/// An entity that has a typescript type
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsTypeHolder<'a, 'b> {
    kind: TsType<'a>,
    holding_for: &'a PExpression<'b>,
}

impl<'a> TsType<'a> {
    /// Maybe move this method to TsObject
    fn resolve_member_access_type(&self, ident: &PIdentifier<'a>) -> Option<TsType<'a>> {
        match &self {
            TsType::Object(object) => object.get(&ident.to_string()).map(|t| t.kind),
            _ => todo!(),
        }
    }

    #[cfg(feature = "ts")]
    fn from_type_expression(expression: &PTsExpression<'a>) -> TsType<'a> {
        match expression {
            PTsExpression::Atom(atom) => match atom {
                PTsAtom::String(_) => TsType::String,
                PTsAtom::Number(_) => TsType::Number,
                PTsAtom::Any(_) => TsType::Any,
                PTsAtom::Identifier(_) => todo!(),
            },
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TsType<'a> {
    Any,
    Literal(TsLiteralPrimitive<'a>),
    Number,
    String,
    Object(TsObjectLiteral<'a>),
}

impl<'a> Display for TsType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TsType::Any => write!(f, "any"),
            TsType::Number => write!(f, "number"),
            TsType::String => write!(f, "string"),
            TsType::Literal(literal) => write!(f, "{literal}"),
            TsType::Object(obj) => write!(f, "{obj}"),
        }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TsLiteralPrimitive<'a> {
    Number { value: f32 },
    String { value: &'a str },
}

impl<'a> Display for TsLiteralPrimitive<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TsLiteralPrimitive::Number { value } => write!(f, "{value}"),
            TsLiteralPrimitive::String { value } => write!(f, "`{value}`"), // TODO: we might wanna escape
                                                                            // backticks
        }
    }
}

impl<'a> From<&PLiteralPrimitive<'a>> for TsType<'a> {
    fn from(value: &PLiteralPrimitive<'a>) -> Self {
        let kind = match value {
            PLiteralPrimitive::Number { value, token: _ } => {
                TsType::Literal(TsLiteralPrimitive::Number { value: *value })
            }
            PLiteralPrimitive::String { value, .. } => {
                TsType::Literal(TsLiteralPrimitive::String { value })
            }
        };
        kind
    }
}

impl TsTypeHolder<'_, '_> {
    fn non_const(&self) -> Self {
        let mut s = self.clone();
        s.kind = s.kind.non_const();
        s
    }
}

impl<'a> TsType<'a> {
    fn contains(&self, b: &Self) -> bool {
        match (self, b) {
            (TsType::Any, _)
            | (_, TsType::Any)
            | (TsType::Number, TsType::Number)
            | (TsType::String, TsType::String) => true,
            (TsType::Object(object1), TsType::Object(object2)) => object1.is_assignable_to(object2),
            (TsType::Literal(a), TsType::Literal(b)) => a.is_same_as(b),
            (a, TsType::Literal(b)) => b.matches_wider(a),
            _ => false,
        }
    }

    #[cfg(test)]
    fn matches(&self, b: &Self) -> bool {
        self.contains(b) && b.contains(self)
    }

    fn non_const(&self) -> Self {
        if let TsType::Literal(l) = self {
            l.wider()
        } else {
            self.clone()
        }
    }
}

impl<'a> TsLiteralPrimitive<'a> {
    const fn wider(&self) -> TsType<'a> {
        match self {
            TsLiteralPrimitive::String { value: _ } => TsType::String,
            TsLiteralPrimitive::Number { value: _ } => TsType::Number,
        }
    }

    /// Same type, can have diff value
    const fn is_of_type(&self, b: &Self) -> bool {
        matches!(
            (self, b),
            (
                TsLiteralPrimitive::String { .. },
                TsLiteralPrimitive::String { .. }
            ) | (
                TsLiteralPrimitive::Number { .. },
                TsLiteralPrimitive::Number { .. }
            )
        )
    }

    /// Same type and value
    fn is_same_as(&self, b: &Self) -> bool {
        match (self, b) {
            (TsLiteralPrimitive::String { value: a }, TsLiteralPrimitive::String { value: b }) => {
                a == b
            }
            (TsLiteralPrimitive::Number { value: a }, TsLiteralPrimitive::Number { value: b }) => {
                let error_margin = 0.0001;
                (a - b).abs() < error_margin
            }
            _ => false,
        }
    }

    /// Whether literal matches a wider type
    fn matches_wider(&self, t: &TsType<'a>) -> bool {
        let wider = self.wider();
        matches!(
            (wider, t),
            (TsType::Number, TsType::Number) | (TsType::String, TsType::String)
        )
    }
}

#[cfg(test)]
pub mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use crate::{
        checker::{Checker, TsError, TsLiteralPrimitive, TsType, TypeErrorKind},
        parser::{PStatement, ParseTree, Parser},
        tokenizer::Tokenizer,
    };

    use super::scope::TsScope;

    #[test]
    fn unit_number() {
        let code = "4";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert!(t.matches(&TsType::Literal(TsLiteralPrimitive::Number { value: 4.0 })))
    }

    #[test]
    fn unit_string() {
        let code = "'string'";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert!(t.matches(&TsType::Literal(TsLiteralPrimitive::String {
            value: "string"
        })))
    }

    #[test]
    fn expr_string() {
        let code = "'string' + 'string'";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert!(t.matches(&TsType::String))
    }

    #[test]
    fn expr() {
        let code = "4 + 4";
        let wrapper = make_parse_tree(code);
        let t = wrapper.check_expr();
        assert!(t.matches(&TsType::Number))
    }

    #[test]
    fn error_different_types() {
        let code = "4 + '4'";
        let tree_wrapper = make_parse_tree(code);
        let (errors, scope) = tree_wrapper.ts_check();
        assert_eq!(errors.len(), 0);
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
        expected_types.insert("b".to_string(), "let b: string");
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

    pub fn make_parse_tree(code: &str) -> TreeWrapper {
        let tok = Tokenizer::new(code);
        let parser = Parser::new(tok);
        let (tree, errors) = parser.parse().unwrap();
        assert!(errors.is_empty());
        TreeWrapper { tree }
    }

    pub struct TreeWrapper<'a> {
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
            for error in &checker.errors {
                println!("{error}");
            }
            assert_eq!(checker.errors.len(), 0);
            exp.kind
        }

        pub fn ts_check<'b>(&'b self) -> (Vec<TsError<'b>>, TsScope<'b>) {
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

    #[test]
    #[cfg(feature = "ts")]
    fn assignment_any() {
        let code = "
    let a = {a: 4};
    let h: any = 1;
    a = h; // filler for any type - would need to modify this test later
    ";
        let wrapper = make_parse_tree(code);
        let (errors, scope) = wrapper.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: {a: number, }");
        expected_types.insert("h".to_string(), "let h: any");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn invalid_assignment() {
        let code = "
    let a = 4 + 4;
    a = \"4\"
    ";
        let wrapper = make_parse_tree(code);
        let (errors, scope) = wrapper.ts_check();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                expected: TsType::Number,
                got,
            } if got.kind.matches(&TsType::String) => {}
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

    #[test]
    fn reassign_const() {
        let code = "
    const a = 4 + 4;
    a = 2
    ";
        let wrapper = make_parse_tree(code);
        let (errors, scope) = wrapper.ts_check();
        assert_eq!(errors.len(), 1);
        match errors[0].kind {
            TypeErrorKind::ReassignConstant {
                symbol: _,
                assign_token: _,
            } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "const a: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 1);
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn assign_lvalue_invalid() {
        let code = "
    4 = 2
    ";
        let wrapper = make_parse_tree(code);
        let (errors, scope) = wrapper.ts_check();
        assert_eq!(errors.len(), 1);
        match errors[0].kind {
            TypeErrorKind::InvalidLvalue { assign_token: _ } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 0);
    }

    #[test]
    fn member_access_valid() {
        let code = "let a = {b: 1};
a.b = 2;
";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: {b: number, }");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn member_access_invalid() {
        let code = "let a = {b: 1};
a.b = '2';
";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                got,
                expected: TsType::Number,
            } if got.kind == TsType::String => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: {b: number, }");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn ts_valid_type() {
        let code = "let a: number = 4;";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn ts_invalid_type() {
        let code = "let a: number = 'abc';";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                got,
                expected: TsType::Number,
            } if got.non_const().kind == TsType::String => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn cannot_find_name() {
        let code = "
let a = 1;
a = b;
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::UnknownIdentifier { identifier: _ } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn block_access_outer() {
        let code = "
    let a = 'abc';
    {
        let b = a;
        let c = a + b;
    }
    let b = 1;
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: string");
        expected_types.insert("b".to_string(), "let b: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn block_inner_invalid() {
        let code = "
    let a = 'abc';
    {
        let b = a;
        b = 1;
        let c = a + b;
    }
    let b = 1;
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                got,
                expected: TsType::String,
            } if got.non_const().kind == TsType::Number => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: string");
        expected_types.insert("b".to_string(), "let b: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn block_invalid() {
        let code = "
    let a = 'abc';
    let b = 1;
    {
        b = '43';
        let c = a + b;
    }
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                got,
                expected: TsType::Number,
            } if got.non_const().kind == TsType::String => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: string");
        expected_types.insert("b".to_string(), "let b: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn block_shadowing() {
        let code = "
    let a = 'abc';
    let b = 1;
    {
        let a = 5;
        let c = a + b;
    }
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: string");
        expected_types.insert("b".to_string(), "let b: number");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }
}
