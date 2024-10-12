use core::panic;
use std::fmt::Display;

use proc_macro::TokenStream;
use syn::{parenthesized, parse::Parse, parse_macro_input, Ident, LitStr, Token};

use quote::{format_ident, quote};

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

    let doc_comments = operators.iter().map(|op| {
        let line_1 = format!(
            "({}) {} operator",
            op.kind.to_string(),
            op.display_str.value()
        );
        let line_3 = format!("\nassociated with TokenKind::{}", op.token_kind.to_string());
        let orig_comment = if let Some(c) = op.doc_comment.clone() {
            quote! {
                #[doc = #c]
            }
        } else {
            quote! {}
        };
        quote! {
            #[doc = #line_1]
            #orig_comment
            #[doc = #line_3]
        }
    });

    // enum construction
    let my_enum = quote! {
        #[derive(Debug, PartialEq, Eq)]
        pub enum POperatorKind {
            #(
                #doc_comments
                #operator_idents
            ),*
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

    let mut infix_operator_bps = vec![];
    let mut prefix_operator_bps = vec![];
    let mut postfix_operator_bps = vec![];

    let mut i = 1;
    for operator in operators.iter() {
        match &operator.kind {
            POperatorKind::In(associativity) => {
                if associativity == &OperatorAssociativity::Left {
                    infix_operator_bps.push((Some(i), Some(i + 1)));
                } else {
                    infix_operator_bps.push((Some(i + 1), Some(i)));
                }
            }
            POperatorKind::Pre => {
                prefix_operator_bps.push((None::<u8>, Some(i + 1)));
            }
            POperatorKind::Post => {
                postfix_operator_bps.push((Some(i), None::<u8>));
            }
        }
        i += 2;
    }

    let infix_operators = OperatorGroup::from_operator_inputs(
        POperatorKind::In(OperatorAssociativity::Left),
        &operators,
    );
    let infix_operator_parse_impl =
        generate_operator_parse_impl(infix_operators, infix_operator_bps);
    let prefix_operators = OperatorGroup::from_operator_inputs(POperatorKind::Pre, &operators);
    let prefix_operator_parse_impl =
        generate_operator_parse_impl(prefix_operators, prefix_operator_bps);
    let postfix_operators = OperatorGroup::from_operator_inputs(POperatorKind::Post, &operators);
    let postfix_operator_parse_impl =
        generate_operator_parse_impl(postfix_operators, postfix_operator_bps);

    let result = quote! {
        #my_enum

        #my_enum_display_impl

        impl<'a> Parser<'a> {
            #infix_operator_parse_impl

            #prefix_operator_parse_impl

            #postfix_operator_parse_impl
        }

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
        let doc_comment = if content.peek(Token![,]) {
            content.parse::<Token![,]>()?;
            let doc_comment = content.parse::<LitStr>()?;
            Some(doc_comment)
        } else {
            None
        };
        Ok(POperatorDataInput {
            kind,
            operator_variant,
            token_kind,
            display_str,
            doc_comment,
        })
    }
}

impl Parse for POperatorKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let kind = match ident.to_string().as_str() {
            "infix" => {
                if input.peek(Token![<]) {
                    input.parse::<Token![<]>()?;
                    let assoc = input.parse::<OperatorAssociativity>()?;
                    input.parse::<Token![>]>()?;
                    POperatorKind::In(assoc)
                } else {
                    POperatorKind::In(OperatorAssociativity::Left)
                }
            }
            "pre" => POperatorKind::Pre,
            "post" => POperatorKind::Post,
            _ => panic!("Unexpected token {:?}", ident),
        };
        Ok(kind)
    }
}

impl Parse for OperatorAssociativity {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let kind = match ident.to_string().as_str() {
            "left" => OperatorAssociativity::Left,
            "right" => OperatorAssociativity::Right,
            _ => panic!("Unexpected token {:?}", ident),
        };
        Ok(kind)
    }
}

fn generate_operator_parse_impl(
    operator_group: OperatorGroup,
    binding_powers: Vec<(Option<u8>, Option<u8>)>,
) -> proc_macro2::TokenStream {
    let OperatorGroup {
        token_kinds,
        operator_idents,
        kind,
    } = operator_group;
    let method_name = format_ident!("try_parse_{kind}_operator");
    let bps = binding_powers
        .iter()
        .map(|(l_bp, r_bp)| {
            let l_bp = if !matches!(kind, POperatorKind::Pre) {
                let l_bp = l_bp.expect(&format!("Expect left for {kind}"));
                quote! { #l_bp }
            } else {
                quote! { () }
            };
            let r_bp = if !matches!(kind, POperatorKind::Post) {
                let r_bp = r_bp.expect(&format!("Expect right for {kind}"));
                quote! { #r_bp }
            } else {
                quote! { () }
            };
            quote! { ( #l_bp , #r_bp ) }
        })
        .collect::<Vec<_>>();

    let return_type = {
        let r_bp = if !matches!(kind, POperatorKind::Post) {
            quote! { u8 }
        } else {
            quote! { () }
        };
        let l_bp = if !matches!(kind, POperatorKind::Pre) {
            quote! { u8 }
        } else {
            quote! { () }
        };
        quote! { ( #l_bp , #r_bp ) }
    };

    let bp_condition = if !matches!(kind, POperatorKind::Pre) {
        quote! {
            if l_bp < min_bp {
                None
            } else {
                self.tokenizer.next();
                Some((operator, (l_bp, r_bp)))
            }
        }
    } else {
        quote! {
            self.tokenizer.next();
            Some((operator, (l_bp, r_bp)))
        }
    };

    quote! {
        pub(super) fn #method_name(&mut self, min_bp: u8) -> Option<(POperator<'a>, #return_type )> {
            let token = self.tokenizer.peek()?;
            let token = token.clone();
            let (operator, (l_bp, r_bp)) = match token.token_type() {

                #( TokenKind::#token_kinds => Some((POperator::new(POperatorKind::#operator_idents, token), #bps)), )*

                _ => None,
            }?;
            #bp_condition
        }
    }
}

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
    doc_comment: Option<LitStr>,
}

struct OperatorGroup {
    kind: POperatorKind,
    operator_idents: Vec<Ident>,
    token_kinds: Vec<Ident>,
}

impl OperatorGroup {
    fn from_operator_inputs(kind: POperatorKind, operators: &Vec<POperatorDataInput>) -> Self {
        let (operator_idents, token_kinds) = operators
            .iter()
            .filter(|op| op.kind.is_similar_to(&kind))
            .map(|op| (op.operator_variant.clone(), op.token_kind.clone()))
            .collect();
        OperatorGroup {
            kind,
            operator_idents,
            token_kinds,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
enum OperatorAssociativity {
    Left,
    Right,
}

#[derive(Clone, PartialEq, Eq)]
enum POperatorKind {
    Post,
    Pre,
    In(OperatorAssociativity),
}

impl POperatorKind {
    fn is_similar_to(&self, kind: &POperatorKind) -> bool {
        match kind {
            POperatorKind::In(_assoc) => match self {
                POperatorKind::In(_assoc) => true,
                _ => false,
            },
            _ => self == kind,
        }
    }
}

impl Display for POperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::In(_assoc) => write!(f, "infix"),
            Self::Pre => write!(f, "prefix"),
            Self::Post => write!(f, "postfix"),
        }
    }
}

impl Display for OperatorAssociativity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorAssociativity::Left => write!(f, "left"),
            OperatorAssociativity::Right => write!(f, "right"),
        }
    }
}
