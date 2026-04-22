/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Codegen binary: reads the node schema and prints generated code to stdout.
//!
//! ## JS deserializer (default)
//!
//! Generates `FlowParserNodeDeserializers.js`:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen > \
//!     fbcode/flow/packages/flow-parser-oxidized/src/FlowParserNodeDeserializers.js
//! ```
//!
//! ## Rust serializer dispatch (`--rust`)
//!
//! Generates `serializer_dispatch.rs`:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- --rust > \
//!     fbcode/flow/rust_port/crates/flow_parser_wasm/src/serializer_dispatch.rs
//! ```

use std::str::FromStr;

use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

use flow_parser_wasm::node_kinds::DispatchType;
use flow_parser_wasm::node_kinds::NodeDef;
use flow_parser_wasm::node_kinds::PropType;
use flow_parser_wasm::node_kinds::SCHEMA;
use flow_parser_wasm::node_kinds::SERIALIZE_SCHEMA;
use flow_parser_wasm::node_kinds::SerializeBody;
use flow_parser_wasm::node_kinds::SerializeMapping;

fn main() {
    let rust_mode = std::env::args().any(|arg| arg == "--rust");
    if rust_mode {
        generate_rust();
    } else {
        generate_js();
    }
}

// ---------------------------------------------------------------------------
// JS codegen
// ---------------------------------------------------------------------------

fn generate_js() {
    print!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 * @generated
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten.
 * To regenerate: buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {{vars: \"local\"}}], no-redeclare: 'error' */

'use strict';

module.exports = [\n"
    );

    for (i, def) in SCHEMA.iter().enumerate() {
        if i > 0 {
            println!();
        }
        if def.custom_emit {
            print_custom_deserializer(def);
        } else {
            print_mechanical_deserializer(def);
        }
    }

    println!("];");
}

/// Emit the JS property name for a schema property.
/// Most names are used as-is; `await` is a JS keyword but is valid as an
/// object literal key in ES5+ strict mode, so it doesn't need quoting.
fn js_prop_name(name: &str) -> &str {
    name
}

/// Return the JS deserializer method call for a property type.
fn deserialize_call(ty: PropType) -> &'static str {
    match ty {
        PropType::Node => "this.deserializeNode()",
        PropType::NodeList => "this.deserializeNodeList()",
        PropType::String => "this.deserializeString()",
        PropType::Boolean => "this.deserializeBoolean()",
        PropType::Number => "this.deserializeNumber()",
    }
}

fn print_mechanical_deserializer(def: &NodeDef) {
    println!("  // {}: {}", def.kind_id, def.name);
    println!("  function () {{");
    println!("    return {{");
    // The JS `type` field is the ESTree node type. It usually matches the
    // NodeKind name, but can be overridden via `as "EsTreeName"` in the
    // schema for nodes that share an ESTree type (different binary shapes
    // routed to the same ESTree node, e.g. DeclareComponent).
    println!("      type: '{}',", def.estree_type);
    println!("      loc: this.addEmptyLoc(),");

    for (prop_name, prop_ty) in def.extra_pre_props {
        println!(
            "      {}: {},",
            js_prop_name(prop_name),
            deserialize_call(*prop_ty)
        );
    }

    if def.nested_object_props.is_empty() {
        for (prop_name, prop_ty) in def.properties {
            println!(
                "      {}: {},",
                js_prop_name(prop_name),
                deserialize_call(*prop_ty)
            );
        }
    } else {
        for (prop_name, prop_ty) in def.properties {
            if def.nested_object_props.contains(prop_name) {
                continue;
            }
            println!(
                "      {}: {},",
                js_prop_name(prop_name),
                deserialize_call(*prop_ty)
            );
        }
        println!("      value: {{");
        for (prop_name, prop_ty) in def.properties {
            if !def.nested_object_props.contains(prop_name) {
                continue;
            }
            println!(
                "        {}: {},",
                js_prop_name(prop_name),
                deserialize_call(*prop_ty)
            );
        }
        println!("      }},");
    }

    for (prop_name, prop_ty) in def.extra_post_props {
        println!(
            "      {}: {},",
            js_prop_name(prop_name),
            deserialize_call(*prop_ty)
        );
    }

    println!("    }};");
    println!("  }},");
}

/// Custom emission for nodes whose JS shape can't be expressed via the
/// mechanical schema (currently only `Literal`, kind 75).
fn print_custom_deserializer(def: &NodeDef) {
    match def.name {
        "Literal" => print_literal_deserializer(def),
        other => panic!("custom_emit set for {}, but no handler defined", other),
    }
}

fn print_literal_deserializer(def: &NodeDef) {
    println!("  // {}: {}", def.kind_id, def.name);
    println!("  // Custom encoding: valueKind discriminant + type-specific data");
    println!("  function () {{");
    println!("    const loc = this.addEmptyLoc();");
    println!("    // 0=null, 1=boolean, 2=number, 3=string");
    println!("    const valueKind = this.next();");
    println!("    let value = null;");
    println!("    if (valueKind === 1) value = this.deserializeBoolean();");
    println!("    else if (valueKind === 2) value = this.deserializeNumber();");
    println!("    else if (valueKind === 3) value = this.deserializeString();");
    println!("    const raw = this.deserializeString();");
    println!("    const bigint = this.deserializeString();");
    println!("    const regexPattern = this.deserializeString();");
    println!("    const regexFlags = this.deserializeString();");
    println!("    let regex = null;");
    println!("    if (regexPattern != null) {{");
    println!("      regex = {{pattern: regexPattern, flags: regexFlags ?? ''}};");
    println!("    }}");
    println!("    return {{type: 'Literal', loc, value, raw, bigint, regex}};");
    println!("  }},");
}

// ---------------------------------------------------------------------------
// Rust codegen
// ---------------------------------------------------------------------------

fn generate_rust() {
    let header = format!(
        "/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// @{tag}
//
// !!! GENERATED FILE !!!
//
// Any manual changes to this file will be overwritten.
// To regenerate:
//   buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- --rust > \\
//     fbcode/flow/rust_port/crates/flow_parser_wasm/src/serializer_dispatch.rs
//
// This file is included via `include!()` in serializer.rs at module level.
// It has access to all imports and types from serializer.rs.
",
        tag = "generated"
    );

    // Group mappings by dispatch type
    let statements: Vec<&SerializeMapping> = SERIALIZE_SCHEMA
        .iter()
        .filter(|m| m.dispatch == DispatchType::Statement)
        .collect();
    let expressions: Vec<&SerializeMapping> = SERIALIZE_SCHEMA
        .iter()
        .filter(|m| m.dispatch == DispatchType::Expression)
        .collect();
    let types: Vec<&SerializeMapping> = SERIALIZE_SCHEMA
        .iter()
        .filter(|m| m.dispatch == DispatchType::Type)
        .collect();

    let stmt_method = build_dispatch_method(
        "serialize_statement_dispatch",
        "stmt",
        quote!(&ast::statement::Statement<Loc, Loc>),
        quote!(ast::statement::StatementInner),
        format_ident!("StatementInner"),
        &statements,
    );
    let expr_method = build_dispatch_method(
        "serialize_expression_dispatch",
        "expr",
        quote!(&ast::expression::Expression<Loc, Loc>),
        quote!(ast::expression::ExpressionInner),
        format_ident!("ExpressionInner"),
        &expressions,
    );
    let type_method = build_dispatch_method(
        "serialize_type_dispatch",
        "ty",
        quote!(&ast::types::Type<Loc, Loc>),
        quote!(ast::types::TypeInner),
        format_ident!("TypeInner"),
        &types,
    );

    let body = quote! {
        impl<'a> Serializer<'a> {
            #stmt_method

            #expr_method

            #type_method
        }
    };

    let parsed = syn::parse_file(&body.to_string())
        .expect("generated dispatch tokens should parse as valid Rust");
    let formatted = prettyplease::unparse(&parsed);

    print!("{}{}", header, formatted);
}

fn build_dispatch_method(
    method_name: &str,
    param_name: &str,
    param_type: TokenStream,
    inner_type_path: TokenStream,
    inner_short: proc_macro2::Ident,
    mappings: &[&SerializeMapping],
) -> TokenStream {
    let method_ident = format_ident!("{}", method_name);
    let param_ident = format_ident!("{}", param_name);

    let arms = mappings.iter().map(|mapping| build_match_arm(mapping, &inner_short));

    quote! {
        fn #method_ident(
            &mut self,
            #param_ident: #param_type,
        ) {
            use #inner_type_path;
            match &**#param_ident {
                #(#arms)*
            }
        }
    }
}

fn build_match_arm(mapping: &SerializeMapping, inner_short: &proc_macro2::Ident) -> TokenStream {
    let variant = format_ident!("{}", mapping.variant);
    let bindings = parse_tokens(mapping.bindings, "bindings");
    let node_kind = format_ident!("{}", mapping.node_kind_name);

    match &mapping.body {
        SerializeBody::Leaf => {
            quote! {
                #inner_short::#variant { #bindings } => {
                    self.write_node_header(NodeKind::#node_kind, loc);
                }
            }
        }
        SerializeBody::Mechanical(exprs) => {
            let stmts = exprs.iter().map(|expr_str| {
                let expr = parse_tokens(expr_str, "mechanical expr");
                quote! { #expr; }
            });
            quote! {
                #inner_short::#variant { #bindings } => {
                    self.write_node_header(NodeKind::#node_kind, loc);
                    #(#stmts)*
                }
            }
        }
        SerializeBody::Delegate(delegate_str) => {
            let delegate = parse_tokens(delegate_str, "delegate expr");
            quote! {
                #inner_short::#variant { #bindings } => {
                    #delegate;
                }
            }
        }
    }
}

/// Parse a stringified Rust source fragment into a TokenStream.
///
/// The strings come from `stringify!()` in `define_nodes!`, which preserves
/// every token but loses the original spacing. Re-tokenizing with
/// `proc_macro2` produces a TokenStream with proper spans, so `quote!` and
/// `prettyplease` can lay it out correctly.
fn parse_tokens(s: &str, what: &str) -> TokenStream {
    TokenStream::from_str(s)
        .unwrap_or_else(|err| panic!("failed to parse {what} fragment {s:?}: {err}"))
}
