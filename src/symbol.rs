use std::clone;

#[derive(Debug)]
pub struct SymbolInfo {
    pub type_: Type,
    pub attrs: IdentifierAttrs,
}

#[derive(Clone, Debug)]
pub enum IdentifierAttrs {
    FuncAttr { defined: bool, global: bool },

    StaticAttr { init: InitialValue, global: bool },

    LocalAttr,
}

#[derive(Clone, Debug)]
pub enum InitialValue {
    Tentative,
    Initial(i32),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    FunType { param_count: i32 },
}
