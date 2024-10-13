use std::collections::HashMap;

use crate::{
    parser::{BindingType, PIdentifier},
    tokenizer::TokenLocation,
};

use super::TsTypeHolder;

#[derive(Debug, Default)]
pub struct TsScope<'a> {
    symbols: HashMap<String, TsSymbol<'a>>,
}

impl<'a> TsScope<'a> {
    pub const fn symbols(&self) -> &HashMap<String, TsSymbol<'a>> {
        &self.symbols
    }

    pub fn add_symbol(&mut self, id: &str, symbol: TsSymbol<'a>) {
        self.symbols.insert(id.into(), symbol);
    }

    pub fn exists(&self, id: &str) -> bool {
        self.symbols.contains_key(id)
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsSymbol<'a> {
    binding_type: &'a BindingType,
    identifier: &'a PIdentifier<'a>,
    ts_type: TsTypeHolder<'a, 'a>,
}

impl<'a> TsSymbol<'a> {
    pub const fn new(
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

    pub fn name(&self) -> &str {
        self.identifier.name()
    }

    pub fn location(&self) -> &TokenLocation {
        self.identifier.location()
    }

    pub const fn ts_type<'b>(&self) -> &TsTypeHolder<'a, 'b>
    where
        'a: 'b,
    {
        &self.ts_type
    }

    pub const fn is_redeclarable(&self) -> bool {
        matches!(self.binding_type, BindingType::Var)
    }

    pub const fn is_reassignable(&self) -> bool {
        !matches!(self.binding_type, BindingType::Const)
    }

    pub fn type_info(&self) -> String {
        format!(
            "{} {}: {}",
            self.binding_type, self.identifier, self.ts_type.kind
        )
    }
}
