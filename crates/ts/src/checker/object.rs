use std::{collections::HashMap, fmt::Display};

use crate::parser::{PKeyValue, PObject, PObjectEntry, PObjectKey};

use super::{Checker, TsType, TsTypeHolder};

impl<'a> Checker<'a> {
    pub(super) fn object(&mut self, object: &'a PObject<'a>) -> TsType<'a> {
        let mut object_entries = HashMap::new();
        for entry in &object.entries {
            let (key, value) = match entry {
                PObjectEntry::KeyValue(PKeyValue { key, value }) => {
                    let key = match key {
                        PObjectKey::Identifier(identifier) => identifier.name(),
                        _ => todo!(),
                    };
                    let exp_type = self.expression(&value);

                    (key, exp_type.non_const())
                }
                _ => todo!(),
            };
            object_entries.insert(key, value);
        }
        TsType::Object(TsObjectLiteral {
            entries: object_entries,
        })
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsObjectLiteral<'a> {
    entries: HashMap<&'a str, TsTypeHolder<'a, 'a>>,
}

impl<'a> Display for TsObjectLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut keys = self.entries.keys().collect::<Vec<_>>();
        keys.sort();
        for key in keys {
            let entry = self.entries.get(key).unwrap();
            write!(f, "{key}: {}, ", entry.kind)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use crate::checker::tests::make_parse_tree;

    #[test]
    fn object() {
        let code = "
let x = {
    a: 1,
    b: 'four',
    c: {}
};
x
";
        let tree = make_parse_tree(code);
        let (errors, scope) = tree.ts_check();
        assert_eq!(errors.len(), 0);
        let symbols = scope.symbols();
        let mut expected_types = HashMap::new();
        expected_types.insert(
            "x".to_string(),
            "let x: {a: number, b: string, c: {}, }".to_string(),
        );
        assert_eq!(expected_types.len(), symbols.len());

        for (id, symbol) in symbols {
            let t = expected_types.get(id).unwrap();
            assert_eq!(t, &symbol.type_info());
        }
    }
}
