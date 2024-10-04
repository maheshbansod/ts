use core::panic;

use proc_macro::TokenStream;
use syn::{parenthesized, parse::Parse, parse_macro_input, Expr, ExprPath, Ident, LitStr, Token};

use quote::quote;

#[proc_macro]
pub fn make_operators(input: TokenStream) -> TokenStream {
    let MakeOperatorsInput { operators } = parse_macro_input!(input as MakeOperatorsInput);

    let operator_idents = operators
        .iter()
        .map(|op| op.operator_variant.clone())
        .collect::<Vec<_>>();

    let operator_strs = operators
        .iter()
        .map(|op| op.display_str.clone())
        .collect::<Vec<_>>();

    // 1. enum construction
    let my_enum = quote! {
        #[derive(Debug, PartialEq)]
        pub(super) enum POperatorKind {
            #(  #operator_idents  ),*
        }
    };

    let my_enum_display_impl = quote! {
        impl std::fmt::Display for POperatorKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #( POperatorKind::#operator_idents => write!(f, #operator_strs) ),*
                }
            }
        }
    };

    let result = quote! {
        #my_enum

        #my_enum_display_impl
    };

    result.into()
}

impl Parse for MakeOperatorsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ops = input.parse_terminated(POperatorDataInput::parse, Token![,])?;
        let ops = ops.into_iter().map(|op| op).collect::<Vec<_>>();
        Ok(MakeOperatorsInput { operators: ops })
    }
}

impl Parse for POperatorDataInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let kind = content.parse::<POperatorKind>()?;
        content.parse::<Token![,]>()?;
        let operator_variant = content.parse::<Ident>()?;
        content.parse::<Token![,]>()?;
        let token_kind = content.parse::<Ident>()?;
        content.parse::<Token![,]>()?;
        let display_str = content.parse::<LitStr>()?;
        Ok(POperatorDataInput {
            kind,
            operator_variant,
            token_kind,
            display_str,
        })
    }
}

impl Parse for POperatorKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let kind = match ident.to_string().as_str() {
            "infix" => POperatorKind::In,
            "pre" => POperatorKind::Pre,
            "post" => POperatorKind::Post,
            _ => panic!("Unexpected token {:?}", ident),
        };
        Ok(kind)
    }
}

/// Parses something like this:
/// make_operators! (
///     (infix, BinaryAdd, TokenKind::Plus),
///     (infix, Subtract, TokenKind::Minus),
///     (pre, Negate, TokenKind::Minus),
/// )
struct MakeOperatorsInput {
    operators: Vec<POperatorDataInput>,
}

struct POperatorDataInput {
    kind: POperatorKind,
    /// The enum variant of the operator enum
    operator_variant: Ident,
    token_kind: Ident,
    /// The display impl for this
    display_str: LitStr,
}

struct OperatorPriority {
    lhs: Option<u8>,
    rhs: Option<u8>,
}

enum POperatorKind {
    Post,
    Pre,
    In,
}
