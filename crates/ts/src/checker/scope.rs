use std::collections::HashMap;

use crate::parser::{BindingType, PIdentifier};

use super::TsTypeHolder;

#[derive(Debug, Default)]
pub struct TsScope<'a> {
    symbols: HashMap<String, TsSymbol<'a>>,
}

impl<'a> TsScope<'a> {
    pub fn new(symbols: HashMap<String, TsSymbol<'a>>) -> Self {
        Self { symbols }
    }

    pub fn symbols(&self) -> &HashMap<String, TsSymbol<'a>> {
        &self.symbols
    }

    pub fn add_symbol(&mut self, id: &str, symbol: TsSymbol<'a>) {
        self.symbols.insert(id.into(), symbol);
    }

    pub fn exists(&self, id: &str) -> bool {
        self.symbols.contains_key(id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TsSymbol<'a> {
    binding_type: &'a BindingType,
    identifier: &'a PIdentifier<'a>,
    ts_type: TsTypeHolder<'a, 'a>,
}

impl<'a> TsSymbol<'a> {
    pub fn new(
        binding_type: &'a BindingType,
        identifier: &'a PIdentifier<'a>,
        ts_type: TsTypeHolder<'a, 'a>,
    ) -> Self {
        Self {
            binding_type,
            identifier,
            ts_type,
        }
    }

    pub fn ts_type<'b>(&self) -> &TsTypeHolder<'a, 'b>
    where
        'a: 'b,
    {
        &self.ts_type
    }

    pub fn is_redeclarable(&self) -> bool {
        // todo: fix it when i add var
        false
    }

    pub fn is_reassignable(&self) -> bool {
        !matches!(self.binding_type, BindingType::Const)
    }

    #[cfg(test)]
    pub fn type_info(&self) -> String {
        format!(
            "{} {}: {}",
            self.binding_type, self.identifier, self.ts_type.kind
        )
    }
}
