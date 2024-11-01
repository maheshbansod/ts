use crate::parser::{BindingType, PAtom, PExpression, PFunction, PJsExpression};

use super::{scope::TsSymbol, Checker, TsType, TsTypeHolder};

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsFunction<'a> {
    pub params: Vec<TsType<'a>>,
    pub return_type: Box<TsType<'a>>,
}

impl<'a> TsFunction<'a> {
    pub fn new(params: Vec<TsType<'a>>, return_type: Box<TsType<'a>>) -> Self {
        Self {
            params,
            return_type,
        }
    }
}

impl<'a> Checker<'a> {
    pub(super) fn function(&mut self, function: &'a PFunction<'a>) -> TsFunction<'a> {
        self.add_scope();
        let args = function
            .arguments()
            .iter()
            .map(|arg| {
                if let PExpression::Js(PJsExpression::Atom(PAtom::Identifier(ident))) =
                    arg.expression()
                {
                    let kind = TsType::Any;
                    #[cfg(feature = "ts")]
                    let kind = if let Some(ts_type_expression) = arg.ts_type() {
                        let ts_type = self.expression(ts_type_expression);
                        ts_type.kind
                    } else {
                        kind
                    };
                    let symbol = TsSymbol::new(
                        &BindingType::Var,
                        ident,
                        TsTypeHolder {
                            kind: kind.clone(),
                            holding_for: arg.expression(),
                        },
                    );
                    if let Err(_e) = self.add_to_scope(ident.name(), symbol) {
                        todo!("argument already exists error");
                    }
                    kind
                } else {
                    todo!(); // maybe syntax error?
                }
            })
            .collect::<Vec<_>>();
        // time to go through the body
        self.block_content(function.body());
        self.drop_scope().expect("endo of scope");

        let return_type = TsType::Void;
        #[cfg(feature = "ts")]
        let return_type = function
            .return_type()
            .map(|r| self.expression(r).kind)
            .unwrap_or(return_type);

        TsFunction::new(args, Box::new(return_type))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use crate::checker::{tests::make_parse_tree, TsType, TypeErrorKind};

    #[test]
    fn function_any_args() {
        let code = "
let a = 2;
function foo(a, b) {
    const x = 4;
}
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        expected_types.insert("foo".to_string(), "var foo: (any, any) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn void_function() {
        let code = "
let a = 2;
function foo() {
    const x = 4;
}
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("a".to_string(), "let a: number");
        expected_types.insert("foo".to_string(), "var foo: () => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn function_redeclare_param() {
        let code = "
function foo(a, b) {
    const x = 4;
    let a = 2;
}
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::RedeclareBlockScoped { symbol } => {
                assert_eq!(symbol.type_info(), "let a: number")
            }
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (any, any) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn function_invalid_param() {
        let code = "
    function foo(a: number, b) {
        a = 'abc'
    }
        ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::ExpectedType {
                expected: TsType::Number,
                got,
            } if got.kind.matches(&TsType::String) => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (number, any) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn function_return_type() {
        let code = "
    function foo(a: number, b: string): string {
    }
        ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (number, string) => string");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn call() {
        let code = "
function foo(a) {
    a
}
foo(3);
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (any) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }
    #[test]
    fn call_args_1_0() {
        let code = "
function foo(a) {
    a
}
foo();
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::NumberOfArguments {
                token: _,
                expected: 1,
                got: 0,
            } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (any) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn call_with_type() {
        let code = "
function foo(a: number) {
    a
}
foo(3);
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        println!("{errors:?}");
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (number) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    fn call_non_function() {
        let code = "
(1+1)(4)
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::UncallableExpression {
                token: _,
                ts_type: TsType::Number,
            } => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), 0);
    }

    #[test]
    fn function_as_expression() {
        let code = "
const x = function foo() {}
let y = x()
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        assert_eq!(errors.len(), 0);
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("x".to_string(), "const x: () => void");
        expected_types.insert("y".to_string(), "let y: void");
        expected_types.insert("foo".to_string(), "var foo: () => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }

    #[test]
    #[cfg(feature = "ts")]
    fn call_with_invalid_type() {
        let code = "
function foo(a: string) {
    a
}
foo(3);
    ";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            TypeErrorKind::InvalidArgument {
                got,
                expected: TsType::String,
            } if got.kind.non_const().matches(&TsType::Number) => {}
            _ => panic!("Unexpected {:?}", errors[0]),
        }
        let mut expected_types = HashMap::<String, _>::new();
        expected_types.insert("foo".to_string(), "var foo: (string) => void");
        let symbols = scope.symbols();
        assert_eq!(symbols.len(), expected_types.len());
        for (id, symbol) in symbols {
            let id = id.clone();
            assert_eq!(&symbol.type_info(), expected_types.get(&id).unwrap())
        }
    }
}
