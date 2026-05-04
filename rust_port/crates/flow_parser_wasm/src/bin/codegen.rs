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
//!
//! ## ESTree visitor keys (`--estree-visitor-keys`)
//!
//! Generates `ESTreeVisitorKeys.js` for the flow-parser-oxidized package:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
//!     --estree-visitor-keys > \
//!     fbcode/flow/packages/flow-parser-oxidized/src/generated/ESTreeVisitorKeys.js
//! ```
//!
//! ## ESTree visitor keys Flow companion (`--estree-visitor-keys-flow`)
//!
//! Generates `ESTreeVisitorKeys.js.flow` — the Flow type companion for the
//! corresponding `.js` artifact. This is a fixed file (no schema content),
//! emitted via codegen so regen.sh covers both halves of the deliverable:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
//!     --estree-visitor-keys-flow > \
//!     fbcode/flow/packages/flow-parser-oxidized/src/generated/ESTreeVisitorKeys.js.flow
//! ```
//!
//! ## ESTree Flow types (`--estree-types`)
//!
//! Generates `types.js` for the flow-estree-oxidized package by mirroring
//! upstream `hermes-estree/src/types.js`. Run from the `fbsource` root so
//! the default upstream path resolves; alternatively set
//! `HERMES_ESTREE_TYPES_JS` to an absolute path:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
//!     --estree-types > \
//!     fbcode/flow/packages/flow-estree-oxidized/src/types.js
//! ```
//!
//! ## ESTree predicates (`--estree-predicates`)
//!
//! Generates `generated/predicates.js` for the flow-estree-oxidized package.
//! Mirrors upstream `hermes-estree/src/generated/predicates.js`:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
//!     --estree-predicates > \
//!     fbcode/flow/packages/flow-estree-oxidized/src/generated/predicates.js
//! ```
//!
//! ## ESTree selector types (`--estree-selectors`)
//!
//! Generates `generated/HermesESTreeSelectorTypes.js.flow` for the
//! flow-estree-oxidized package. Mirrors upstream's `genSelectorTypes.js`:
//!
//! ```sh
//! buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
//!     --estree-selectors > \
//!     fbcode/flow/packages/flow-estree-oxidized/src/generated/HermesESTreeSelectorTypes.js.flow
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
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--rust") {
        generate_rust();
    } else if args.iter().any(|a| a == "--estree-visitor-keys-flow") {
        generate_estree_visitor_keys_flow();
    } else if args.iter().any(|a| a == "--estree-visitor-keys") {
        generate_estree_visitor_keys();
    } else if args.iter().any(|a| a == "--estree-types") {
        generate_estree_types();
    } else if args.iter().any(|a| a == "--estree-predicates") {
        generate_estree_predicates();
    } else if args.iter().any(|a| a == "--estree-selectors") {
        generate_estree_selectors();
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
/// mechanical schema. Currently used for nodes that need host-language
/// value construction (BigInt / RegExp) that the wire can't carry.
fn print_custom_deserializer(def: &NodeDef) {
    match def.name {
        "Literal" => print_literal_deserializer(def),
        "BigIntLiteralTypeAnnotation" => print_bigint_literal_type_annotation_deserializer(def),
        other => panic!("custom_emit set for {}, but no handler defined", other),
    }
}

fn print_bigint_literal_type_annotation_deserializer(def: &NodeDef) {
    println!("  // {}: {}", def.kind_id, def.name);
    println!("  // Custom encoding: BigInt value constructed inline at deserialize time");
    println!("  function () {{");
    println!("    const loc = this.addEmptyLoc();");
    // The wire reserves a Node slot for `value` to keep the layout uniform
    // with the other LiteralTypeAnnotation kinds (which carry a literal
    // value); the slot is always a null Node placeholder for BigInt
    // because `BigInt(...)` cannot be JSON-serialized. Discard it and
    // construct the host BigInt value from `bigint` below.
    println!("    this.deserializeNode();");
    println!("    const raw = this.deserializeString();");
    println!("    const bigint = this.deserializeString();");
    // Mirror upstream Hermes' HermesToESTreeAdapter.mapBigIntLiteralTypeAnnotation
    // (lines 178-183): coerce the cleaned numeric string to a host BigInt.
    // BigInt() throws on syntactically-invalid strings (e.g. fixtures
    // that exercise `1.0n`, `.1n`, `0e0n` — the OCaml parser emits these
    // with the cleaned `bigint` field even though they're invalid). Mirror
    // the Literal handling above: swallow with null so the AST shape
    // stays consistent.
    println!("    let value = null;");
    println!("    if (bigint != null && typeof BigInt === 'function') {{");
    println!("      try {{");
    println!("        value = BigInt(bigint);");
    println!("      }} catch (e) {{");
    println!("        value = null;");
    println!("      }}");
    println!("    }}");
    println!("    return {{");
    println!("      type: '{}',", def.estree_type);
    println!("      loc,");
    println!("      value,");
    println!("      raw,");
    println!("      bigint,");
    println!("    }};");
    println!("  }},");
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
    println!("    const literalType = this.deserializeString();");
    println!("    const raw = this.deserializeString();");
    println!("    const bigint = this.deserializeString();");
    println!("    const regexPattern = this.deserializeString();");
    println!("    const regexFlags = this.deserializeString();");
    // Mirror upstream Hermes' HermesToESTreeAdapter.{mapBigIntLiteral,
    // mapRegExpLiteral}: construct the host BigInt / RegExp value here at
    // deserialization time. The wire can't carry these because BigInt and
    // RegExp aren't JSON-serializable; doing it inline eliminates any
    // post-deserialization walker. `new RegExp` may throw on invalid
    // flags — swallow with null per upstream Hermes
    // (HermesToESTreeAdapter.mapRegExpLiteral lines 161-166).
    println!("    const node = {{type: 'Literal', loc, value, raw, literalType}};");
    println!("    if (bigint != null) {{");
    println!("      node.bigint = bigint;");
    // BigInt() throws on syntactically-invalid strings (e.g. fixtures
    // that exercise `1.0n`, `.1n`, `0e0n` — the OCaml parser emits these
    // with the cleaned `bigint` field even though they're invalid). Match
    // the RegExp pattern below: swallow with null so the public AST
    // shape stays consistent.
    println!("      try {{");
    println!("        node.value = typeof BigInt === 'function' ? BigInt(bigint) : null;");
    println!("      }} catch (e) {{");
    println!("        node.value = null;");
    println!("      }}");
    println!("    }}");
    println!("    if (regexPattern != null) {{");
    println!("      const flags = regexFlags ?? '';");
    println!("      node.regex = {{pattern: regexPattern, flags}};");
    println!("      try {{");
    println!("        node.value = new RegExp(regexPattern, flags);");
    println!("      }} catch (e) {{");
    println!("        node.value = null;");
    println!("      }}");
    println!("    }}");
    println!("    return node;");
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

// ---------------------------------------------------------------------------
// ESTree visitor keys codegen (`--estree-visitor-keys`)
// ---------------------------------------------------------------------------

/// Property names to exclude per node when emitting visitor keys. Mirrors
/// the upstream `EXCLUDE_PROPERTIES_FROM_NODE` table in
/// `xplat/static_h/tools/hermes-parser/js/scripts/utils/scriptUtils.js`.
fn excluded_visitor_props(node: &str) -> &'static [&'static str] {
    match node {
        // upstream: this property is only needed for TS
        "PropertyDefinition" => &["tsModifiers"],
        _ => &[],
    }
}

/// Set of NodeKind names whose visitor keys are collapsed into the single
/// `Literal` entry by upstream's `LITERAL_TYPES` set. We omit them entirely
/// from the emitted map and emit `Literal: []` instead.
const LITERAL_COLLAPSED_KINDS: &[&str] = &[
    "BigIntLiteral",
    "BooleanLiteral",
    "NullLiteral",
    "NumericLiteral",
    "RegExpLiteral",
    "StringLiteral",
];

/// Hard-coded visitor-key overrides applied after the schema-driven map is
/// built. The first four mirror the overrides in upstream
/// `genESTreeVisitorKeys.js`. The remainder reconcile the schema's
/// wire-serialization order with the ESTree traversal order that upstream
/// emits — every entry below was diff-checked against
/// `xplat/static_h/tools/hermes-parser/js/hermes-parser/src/generated/ESTreeVisitorKeys.js`.
const VISITOR_KEY_OVERRIDES: &[(&str, &[&str])] = &[
    // Direct upstream overrides.
    ("IfStatement", &["test", "consequent", "alternate"]),
    ("ConditionalExpression", &["test", "consequent", "alternate"]),
    ("WhileStatement", &["test", "body"]),
    ("VariableDeclarator", &["id", "init"]),

    // Reconciliation overrides: schema wire-order differs from upstream
    // ESTree visitor order. Upstream's order reflects child traversal
    // semantics (e.g. body is visited last for class declarations).
    (
        "ArrowFunctionExpression",
        &["params", "body", "typeParameters", "returnType", "predicate"],
    ),
    (
        "ClassDeclaration",
        &[
            "id",
            "typeParameters",
            "superClass",
            "superTypeArguments",
            "implements",
            "decorators",
            "body",
        ],
    ),
    (
        "ClassExpression",
        &[
            "id",
            "typeParameters",
            "superClass",
            "superTypeArguments",
            "implements",
            "decorators",
            "body",
        ],
    ),
    (
        "ComponentDeclaration",
        &["id", "params", "body", "typeParameters", "rendersType"],
    ),
    (
        "ComponentTypeAnnotation",
        &["params", "rest", "typeParameters", "rendersType"],
    ),
    (
        "DeclareClass",
        &[
            "id",
            "typeParameters",
            "extends",
            "implements",
            "mixins",
            "body",
        ],
    ),
    (
        "DeclareInterface",
        &["id", "typeParameters", "extends", "body"],
    ),
    ("ExportAllDeclaration", &["exported", "source"]),
    ("ExportSpecifier", &["exported", "local"]),
    (
        "FunctionDeclaration",
        &["id", "params", "body", "typeParameters", "returnType", "predicate"],
    ),
    (
        "FunctionExpression",
        &["id", "params", "body", "typeParameters", "returnType", "predicate"],
    ),
    (
        "FunctionTypeAnnotation",
        &["params", "this", "returnType", "rest", "typeParameters"],
    ),
    (
        "HookDeclaration",
        &["id", "params", "body", "typeParameters", "returnType"],
    ),
    (
        "InterfaceDeclaration",
        &["id", "typeParameters", "extends", "body"],
    ),
    (
        "JSXElement",
        &["openingElement", "children", "closingElement"],
    ),
    (
        "PropertyDefinition",
        &["key", "value", "decorators", "variance", "typeAnnotation"],
    ),
];

/// Per-node property exclusions on top of `excluded_visitor_props`.
/// Used when the schema's Node-typed slot exists but is always emitted as
/// a null wire value (so it has no meaningful child to traverse), or when
/// upstream intentionally omits the field from visitor traversal.
///
/// Each entry is verified against upstream
/// `xplat/static_h/tools/hermes-parser/js/hermes-parser/src/generated/ESTreeVisitorKeys.js`.
fn extra_visitor_excludes(node: &str) -> &'static [&'static str] {
    match node {
        // Schema has `value: Node` but the serializer always writes null
        // (BigIntLiteralTypeAnnotation never carries a child node).
        "BigIntLiteralTypeAnnotation" => &["value"],
        // Upstream omits `typeAnnotation` from DeclareFunction/DeclareHook
        // visitor keys even though the field exists on the AST.
        "DeclareFunction" => &["typeAnnotation"],
        "DeclareHook" => &["typeAnnotation"],
        // Hermes-only fields: not present in upstream visitor traversal.
        "ObjectTypeMappedTypeProperty" => &["nameType"],
        "ObjectTypeProperty" => &["init"],
        "PrivateIdentifier" => &["typeAnnotation"],
        "PropertyDefinition" => &["tsModifiers"],
        "TaggedTemplateExpression" => &["typeArguments"],
        _ => &[],
    }
}

/// Per-node visitor-key INSERTIONS — fields present in upstream but not in
/// our schema that should still be emitted as visitor keys (slots that are
/// synthesized by the JS-side adapter fixups). Verified against upstream.
fn extra_visitor_inserts(node: &str) -> &'static [&'static str] {
    match node {
        // Upstream DeclareComponent visitor keys are
        // `['id', 'params', 'rest', 'typeParameters', 'rendersType']`.
        // Schema now includes `rest` natively; this entry pins the upstream
        // order so the generated visitor keys list matches byte-for-byte.
        "DeclareComponent" => &["id", "params", "rest", "typeParameters", "rendersType"],
        // (No DeclareVariable insert — see comment in extra_visitor_excludes.)
        _ => &[],
    }
}

fn generate_estree_visitor_keys() {
    // Header: copy verbatim from upstream's `formatAndWriteSrcArtifact` HEADER
    // template (with `flow=false`, which produces `@noflow`). The `@generated`
    // tag is split with concatenation via the `tag` interpolation so this
    // Rust source itself is not tagged as generated.
    let header = format!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 * @{tag}
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run \
 `buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
 --estree-visitor-keys`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {{vars: \"local\"}}], no-redeclare: 'error' */
/* global $NonMaybeType, Partial, $ReadOnly, $ReadOnlyArray, $FlowFixMe */

'use strict';

module.exports = {{
",
        tag = "generated"
    );
    print!("{header}");

    // Build the visitor-keys map keyed by ESTree type name (the
    // `def.estree_type`, which collapses NodeKinds that share an ESTree type
    // such as `DeclareComponentAmbient` -> "DeclareComponent"). Use an
    // ordered map so output order is deterministic.
    let mut keys: std::collections::BTreeMap<&'static str, Vec<&'static str>> =
        std::collections::BTreeMap::new();

    for def in SCHEMA.iter() {
        // Skip Literal subtypes that upstream collapses into `Literal: []`.
        if LITERAL_COLLAPSED_KINDS.contains(&def.name) {
            continue;
        }
        // Literal itself is custom-encoded and has no Node/NodeList children
        // in its ESTree shape; emit an empty entry.
        if def.custom_emit {
            keys.insert(def.estree_type, Vec::new());
            continue;
        }

        let upstream_excluded = excluded_visitor_props(def.estree_type);
        let extra_excluded = extra_visitor_excludes(def.estree_type);
        let mut entry: Vec<&'static str> = Vec::new();
        // Only include Node / NodeList properties (matches upstream
        // `ALLOWED_ARG_TYPES = {NodePtr, NodeList}`). `extra_pre_props` and
        // `extra_post_props` are always non-Node (Booleans for the existing
        // schema), but iterate them in upstream order all the same.
        let consider = |name: &'static str, ty: &PropType, entry: &mut Vec<&'static str>| {
            if matches!(ty, PropType::Node | PropType::NodeList)
                && !upstream_excluded.contains(&name)
                && !extra_excluded.contains(&name)
            {
                entry.push(name);
            }
        };
        for (name, ty) in def.extra_pre_props.iter() {
            consider(name, ty, &mut entry);
        }
        for (name, ty) in def.properties.iter() {
            consider(name, ty, &mut entry);
        }
        for (name, ty) in def.extra_post_props.iter() {
            consider(name, ty, &mut entry);
        }
        // If two NodeKinds share an ESTree type (e.g. ComponentDeclaration
        // ambient form aliasing to DeclareComponent), the first one wins —
        // matches upstream behavior where there's a single JSON entry per
        // ESTree node.
        keys.entry(def.estree_type).or_insert(entry);
    }

    // Per-node insertions for upstream-only fields not in the Rust schema
    // (synthesized by the JS adapter — see `extra_visitor_inserts`).
    for def in SCHEMA.iter() {
        if LITERAL_COLLAPSED_KINDS.contains(&def.name) {
            continue;
        }
        let inserts = extra_visitor_inserts(def.estree_type);
        if !inserts.is_empty() {
            keys.insert(def.estree_type, inserts.to_vec());
        }
    }

    // Synthesize visitor-keys entries for ESTree node types our parser
    // doesn't emit directly but the JS-side adapter creates as fixups.
    // ChainExpression wraps OptionalCall/OptionalMember per upstream
    // `HermesToESTreeAdapter.mapChainExpression`; the equivalent fixup
    // lives in `flow-parser-oxidized/src/index.js` (`rewriteChain`,
    // ~lines 141-215) and produces the same shape.
    keys.insert("ChainExpression", vec!["expression"]);

    // Apply hard-coded order overrides.
    for (name, order) in VISITOR_KEY_OVERRIDES {
        if keys.contains_key(*name) {
            keys.insert(*name, order.to_vec());
        }
    }

    // Upstream emits these as legacy / collapsed entries AFTER the
    // alphabetized map (insertion order in JS preserves the original
    // alphabetic sort, then `Optional*` aliases and `Literal` are appended).
    // Our Rust schema defines OptionalMemberExpression / OptionalCallExpression
    // (kinds 56 & 58) and Literal (kind 75); pull them out of the alpha map
    // here and re-emit at the tail to match upstream byte-for-byte order.
    keys.remove("OptionalMemberExpression");
    keys.remove("OptionalCallExpression");
    keys.remove("Literal");

    // Emit alphabetically (BTreeMap iteration order). Match upstream's
    // 2-space indented JSON.stringify output exactly.
    for (name, props) in keys.iter() {
        print_visitor_keys_entry(name, props);
    }

    // Legacy aliases for prettier compat — appended after the alphabetized
    // map (matches upstream's append order). Look up via the freshly
    // computed map so any overrides above also flow through.
    let member = keys.get("MemberExpression").cloned().unwrap_or_default();
    print_visitor_keys_entry("OptionalMemberExpression", &member);
    let call = keys.get("CallExpression").cloned().unwrap_or_default();
    print_visitor_keys_entry("OptionalCallExpression", &call);

    // Final `Literal: []` — upstream emits this last after the literal
    // collapse loop deletes the individual literal subtype entries.
    print_visitor_keys_entry("Literal", &[]);

    println!("}};");
}

/// Generate the Flow companion `ESTreeVisitorKeys.js.flow`. This is a fixed
/// file (no schema content) that exports `VisitorKeys` as a `$ReadOnly<{[string]:
/// $ReadOnlyArray<string>}>` and declares the `module.exports` shape. Mirrors
/// upstream `xplat/static_h/tools/hermes-parser/js/hermes-parser/src/generated/ESTreeVisitorKeys.js.flow`
/// byte-for-byte (with our regen header instead of the hand-written one).
///
/// Emitted via codegen so `regen.sh` (#9) covers both halves of the
/// `ESTreeVisitorKeys.js (+ .flow)` deliverable in a single regeneration
/// step. Without this, Flow consumers of `flow-parser-oxidized/src/generated/
/// ESTreeVisitorKeys` would receive untyped `any` for the visitor-keys export.
fn generate_estree_visitor_keys_flow() {
    let header = format!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 * @{tag}
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run \
`buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
--estree-visitor-keys-flow`.
 */

'use strict';

export type VisitorKeys = $ReadOnly<{{[string]: $ReadOnlyArray<string>}}>;

declare module.exports: VisitorKeys;
",
        tag = "generated"
    );
    print!("{header}");
}

/// Emit one entry of the visitor-keys object using a layout that is
/// equivalent to `JSON.stringify(obj, null, 2)` post-processed by prettier
/// with the project config (single quotes, trailing commas, 80-col wrap).
///
/// We emit multi-element arrays one item per line to match what the
/// upstream `genESTreeVisitorKeys.js` produces before prettier runs;
/// prettier will then collapse short arrays back to a single line and
/// keep long ones multi-line. The final-on-disk file should be fed
/// through prettier to normalize formatting.
fn print_visitor_keys_entry(name: &str, props: &[&str]) {
    if props.is_empty() {
        println!("  {name}: [],");
        return;
    }
    println!("  {name}: [");
    for p in props.iter() {
        println!("    '{p}',");
    }
    println!("  ],");
}

// ---------------------------------------------------------------------------
// ESTree Flow types codegen (`--estree-types`)
// ---------------------------------------------------------------------------

/// Default fbsource-relative path to upstream `hermes-estree/src/types.js`,
/// used when `HERMES_ESTREE_TYPES_JS` is not set. Resolved against the
/// process's current working directory — the standard regen workflow runs
/// `buck run` from the `fbsource` root so this path resolves cleanly.
const DEFAULT_HERMES_ESTREE_TYPES_PATH: &str =
    "xplat/static_h/tools/hermes-parser/js/hermes-estree/src/types.js";

/// Names of NodeKinds that the Rust schema models but upstream
/// `hermes-estree/src/types.js` does not surface as a single
/// `export interface` declaration. Each entry is verified by hand:
///
/// - **Refinement splits** — upstream models the node as multiple refined
///   interfaces (each with the same `+type:` literal) plus a union alias.
///   Our schema has the single concrete kind; the union is the consumer
///   type. e.g. `MemberExpression` exists upstream as
///   `MemberExpressionWith{Computed,NonComputed}Name` plus
///   `export type MemberExpression = …`.
/// - **TS-only nodes** — upstream Hermes parses TypeScript; these kinds
///   exist in our SCHEMA only because the Rust serializer kind list mirrors
///   the OCaml binary protocol's full type space. Flow-only consumers will
///   never see them via the flow-parser-oxidized adapter.
/// - **Custom-encoded** — `Literal` (the only `custom_emit` kind) is
///   modeled upstream as the `Literal` union over BigInt/Boolean/Null/
///   Numeric/RegExp/StringLiteral, not as a standalone interface.
const KNOWN_TYPES_WITHOUT_INTERFACE: &[&str] = &[
    // Refinement splits: upstream emits multiple interfaces sharing the
    // `+type` discriminant plus a union alias.
    "BinaryExpression",          // → BinaryExpressionWithoutIn | BinaryExpressionIn
    "MemberExpression",          // → MemberExpressionWith{,Non}ComputedName
    "MethodDefinition",          // → MethodDefinition{Constructor,WithComputedName,WithNonComputedName}
    "PropertyDefinition",        // → PropertyDefinitionWith{,Non}ComputedName
    "Property",                  // → ObjectProperty{Non,}Shorthand{,Static} + Destructuring*
    "ObjectTypeProperty",        // → ObjectType{MethodSignature,PropertySignature,AccessorSignature}
    "ExportNamedDeclaration",    // → ExportNamedDeclarationWith{Specifiers,Declaration}
    "DeclareExportDeclaration",  // → DeclareExportDeclaration{NamedWith{Declaration,Specifiers},Default}
    "TypeOperator",              // → Renders{,Star,Question}TypeOperator
    "RendersType",               // → Renders{,Star,Question}TypeOperator (same union)
    // Custom-encoded literal collapse.
    "Literal",                   // → Literal = BigIntLiteral | BooleanLiteral | …
    // Optional-chain refinements: upstream merges these into CallExpression /
    // MemberExpression with `+optional: boolean`.
    "OptionalCallExpression",
    "OptionalMemberExpression",
    // TS-only kinds in the Rust schema (mirror OCaml's full type space; not
    // surfaced via the Flow-only adapter).
    "ImportEqualsDeclaration",
    "ExternalModuleReference",
    "ExportAssignment",
    "ExportNamespaceSpecifier",
    "NamespaceExportDeclaration",
    "SatisfiesExpression",
    "NonNullExpression",
    "ParameterProperty",
    "DeclareMethodDefinition",
    "AbstractMethodDefinition",
    "AbstractPropertyDefinition",
    "DeclareClassExtendsCall",
    "ConstructorTypeAnnotation",
    "ObjectTypePrivateField",
    "TupleTypeElement",
    "ImportType",
    "TemplateLiteralTypeAnnotation",
    "RendersMaybeType",
    "RendersStarType",
    "EnumBody",
];

/// Pure cross-check: returns the list of NodeDef `estree_type` names from
/// `schema` that have no matching `export interface <name>` /
/// `export interface <name>\n` / `export type <name> =` declaration in
/// `upstream` AND are not in `KNOWN_TYPES_WITHOUT_INTERFACE`. Caller decides
/// whether to hard-fail. Extracted so a unit test can verify the hard-fail
/// path without invoking `std::process::exit`.
fn estree_types_missing_kinds(schema: &[NodeDef], upstream: &str) -> Vec<&'static str> {
    let mut missing: Vec<&'static str> = Vec::new();
    for def in schema.iter() {
        if KNOWN_TYPES_WITHOUT_INTERFACE.contains(&def.estree_type)
            || missing.contains(&def.estree_type)
        {
            continue;
        }
        let iface_needle = format!("export interface {} ", def.estree_type);
        let iface_extends = format!("export interface {}\n", def.estree_type);
        let union_needle = format!("export type {} =", def.estree_type);
        if !upstream.contains(&iface_needle)
            && !upstream.contains(&iface_extends)
            && !upstream.contains(&union_needle)
        {
            missing.push(def.estree_type);
        }
    }
    missing
}

/// Generate the `flow-estree-oxidized/src/types.js` content.
///
/// The upstream `hermes-estree/src/types.js` is overwhelmingly hand-written
/// Flow — per-node `interface` declarations, hand-curated unions
/// (`ESNode`, `Statement`, `Expression`, `BindingName`, `AFunction`,
/// `Property`, `ChainElement`, `ModuleDeclaration`, `MemberExpression`,
/// `BinaryExpression`, …), discriminator-refined splits
/// (`MethodDefinitionConstructor` / `WithComputedName` /
/// `WithNonComputedName`, `ObjectPropertyWith{Non}Shorthand{Computed}Name`,
/// `ExportNamedDeclarationWith{Specifiers,Declaration}`, …), and pseudo-token
/// types (`Token`, `Comment`, `Position`, `SourceLocation`, `BaseNode`, …)
/// that the Rust schema does not encode.
///
/// Rather than reimplement that file with partial schema-driven generation
/// (which would lose the hand-curated refinements the downstream consumers
/// depend on), this codegen mode emits the upstream file *verbatim* with our
/// own header (Copyright + @noflow + @generated + regen instructions). This
/// is **not** schema-derived generation — it is a verbatim mirror of the
/// upstream Flow types with a SCHEMA cross-check.
///
/// The cross-check enforces a single invariant: every concrete NodeDef in
/// SCHEMA must have a matching `export interface <name>` or
/// `export type <name> =` in upstream's types.js (or appear in
/// `KNOWN_TYPES_WITHOUT_INTERFACE` with attribution). On any drift, codegen
/// **hard-fails with a non-zero exit** so the build cannot silently produce
/// a types.js that omits a Flow-only NodeKind. There is no synthesis or
/// soft-warning path — protecting against future SCHEMA/upstream drift IS
/// the hard-fail.
///
/// The Rust schema is the source of truth for *binary serialization* between
/// the Rust parser and the JS deserializer (kinds + property order); the
/// Flow type surface is the source of truth for *consumer typings*. Forking
/// the upstream Flow types in lockstep with upstream is the right
/// abstraction boundary here.
fn generate_estree_types() {
    let path = std::env::var("HERMES_ESTREE_TYPES_JS")
        .unwrap_or_else(|_| DEFAULT_HERMES_ESTREE_TYPES_PATH.to_string());
    let upstream = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!(
            "failed to read upstream hermes-estree types.js at {path:?}: {err}\n\
             hint: run from the fbsource root, or set HERMES_ESTREE_TYPES_JS to \
             an absolute path"
        )
    });

    // Cross-check: every concrete NodeDef in SCHEMA must appear as an
    // `export interface <name> ...` or `export type <name> =` in the
    // upstream file (the latter covers refinement splits). Any kind with
    // no matching upstream declaration AND no entry in
    // `KNOWN_TYPES_WITHOUT_INTERFACE` is a build-breaking drift error.
    //
    // Single failure mode: codegen hard-fails with a non-zero exit. There
    // is no synthesis path — upstream's types.js currently contains all
    // Flow-only nodes (e.g. `MatchStatement`, `ComponentDeclaration`,
    // `HookDeclaration`, `EnumDeclaration`, …), so the cross-check exists
    // solely to prevent future drift between the Rust SCHEMA and the
    // upstream Flow types.
    let missing = estree_types_missing_kinds(SCHEMA, &upstream);
    if !missing.is_empty() {
        eprintln!("error: SCHEMA NodeKinds missing from upstream {path}:");
        for name in &missing {
            eprintln!("  - {name}");
        }
        eprintln!(
            "Either add upstream interfaces, or extend \
             KNOWN_TYPES_WITHOUT_INTERFACE in codegen.rs with attribution."
        );
        std::process::exit(1);
    }

    // Replace the upstream file header (lines 1-9: `Copyright … @flow strict @format`)
    // with ours: same Copyright but `@noflow @generated`, plus regen instructions
    // and the design rationale required by the team-lead pushback.
    // The `@generated` tag is split via interpolation so this Rust file itself
    // is not tagged as generated.
    let header = format!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 * @{tag}
 */

/*
 * !!! GENERATED FILE !!!
 *
 * DESIGN NOTE — please read before editing.
 *
 * This file is a VERBATIM MIRROR of upstream
 *   xplat/static_h/tools/hermes-parser/js/hermes-estree/src/types.js
 * with our header swapped in. It is **not** schema-derived generation —
 * the body is byte-for-byte from upstream after the upstream header is
 * stripped. The `@{tag}` tag means \"do not hand-edit\" — to update,
 * edit upstream's types.js (or upstream the change first) and re-run the
 * regeneration command below.
 *
 * The codegen step ALSO performs a SCHEMA cross-check: every concrete
 * NodeDef in `node_kinds.rs` must have a matching `export interface` or
 * `export type` in upstream's types.js (or appear in
 * `KNOWN_TYPES_WITHOUT_INTERFACE` in codegen.rs with attribution). On any
 * drift, codegen FAILS the build with a non-zero exit and lists the
 * missing kinds. There is no synthesis or soft-warning path — upstream
 * already contains all Flow-only nodes today, so the cross-check exists
 * solely to prevent future drift between the Rust SCHEMA and upstream.
 *
 * Schema-derivable per-node interfaces are NOT independently generated
 * because upstream's interfaces carry hand-curated child unions (e.g.
 * `Expression`, `Statement`, `BindingName`, `MemberExpression` refinement
 * splits, `MethodDefinition` discriminator splits, per-property
 * nullability) that the Rust schema does not encode and should not encode
 * — the Rust schema is the source of truth for *binary serialization*
 * between the Rust parser and the JS deserializer; the upstream Flow
 * types are the source of truth for *consumer typings*. See
 * `generate_estree_types` in `flow_parser_wasm/src/bin/codegen.rs` for
 * the implementation.
 *
 * To regenerate (run from the fbsource root):
 *
 *   buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \\
 *     --estree-types > \\
 *     fbcode/flow/packages/flow-estree-oxidized/src/types.js
 *
 * To regenerate against a different upstream copy, set
 * `HERMES_ESTREE_TYPES_JS` to an absolute path before invoking codegen.
 */
",
        tag = "generated"
    );

    // Strip the upstream header (everything up to and including the closing
    // `*/` on line 9, followed by the blank line). The upstream file always
    // begins with `/**` and ends its header with ` */\n\n`.
    let body = if upstream.starts_with("/**") {
        match upstream.find("*/\n") {
            Some(end) => upstream[end + 3..].trim_start_matches('\n'),
            None => upstream.as_str(),
        }
    } else {
        upstream.as_str()
    };

    // Emit header, then the upstream body verbatim. Trailing newline
    // behavior of `body` depends on upstream; `print!` handles either.
    print!("{header}\n{body}");
}

// ---------------------------------------------------------------------------
// ESTree predicates codegen (`--estree-predicates`)
// ---------------------------------------------------------------------------

/// NodeKinds that get hand-written special-case predicates with an
/// `(X | MostTokens)` implies clause. Mirrors upstream's
/// `NODES_WITH_SPECIAL_HANDLING` in `genPredicateFunctions.js`.
const PREDICATE_SPECIAL_NODES: &[&str] = &["Identifier", "JSXIdentifier", "JSXText"];

/// ESTree type names that the Rust SCHEMA models but upstream's
/// `genPredicateFunctions.js` does NOT emit predicates for. Distinct from
/// `KNOWN_TYPES_WITHOUT_INTERFACE` (which is the cross-check exclusion set
/// for `--estree-types`): some kinds in that list (e.g. `MemberExpression`,
/// `MethodDefinition`, `Property`, `BinaryExpression`) DO get upstream
/// predicates because their union name is exported from `types.js`.
///
/// Each entry is verified against upstream
/// `xplat/static_h/tools/hermes-parser/js/hermes-estree/src/generated/predicates.js`:
///
/// - **Optional-chain refinements** — upstream merges `OptionalCallExpression`
///   / `OptionalMemberExpression` into `CallExpression` / `MemberExpression`
///   with `+optional: boolean`, so they're absent from upstream JSON.
/// - **Renders refinements** — upstream models `RendersType` as a union of
///   `Renders{,Star,Question}TypeOperator`; the union has no predicate.
/// - **`EnumBody`** — upstream models this as a union of the per-type bodies
///   (`Enum{BigInt,Boolean,Number,String,Symbol}Body`); the union has no
///   predicate.
/// - **TS-only kinds** — upstream Hermes parses TypeScript; these kinds exist
///   in our SCHEMA only because the Rust serializer kind list mirrors the
///   OCaml binary protocol's full type space. Flow-only consumers will never
///   see them via the flow-parser-oxidized adapter.
const PREDICATE_EXCLUDED_TYPES: &[&str] = &[
    // Optional-chain refinements collapsed into Call/Member by upstream.
    "OptionalCallExpression",
    "OptionalMemberExpression",
    // Renders refinement union — upstream emits per-variant interfaces only.
    "RendersType",
    // Enum body union — upstream emits per-type body predicates only.
    "EnumBody",
    // TS-only kinds (mirror OCaml's full type space; not surfaced via the
    // Flow-only adapter, so upstream JSON has no entry).
    "ImportEqualsDeclaration",
    "ExternalModuleReference",
    "ExportAssignment",
    "ExportNamespaceSpecifier",
    "NamespaceExportDeclaration",
    "SatisfiesExpression",
    "NonNullExpression",
    "ParameterProperty",
    "DeclareMethodDefinition",
    "AbstractMethodDefinition",
    "AbstractPropertyDefinition",
    "DeclareClassExtendsCall",
    "ConstructorTypeAnnotation",
    "ObjectTypePrivateField",
    "TupleTypeElement",
    "ImportType",
    "TemplateLiteralTypeAnnotation",
    "RendersMaybeType",
    "RendersStarType",
    // Flow-only kinds that exist in our SCHEMA but not in upstream Hermes
    // ESTree JSON (so upstream emits no predicate for them).
    "InterpreterDirective",
];

/// ESTree node types that the JS adapter synthesizes (or upstream emits a
/// predicate for) but our SCHEMA does not directly model. Each entry is
/// emitted as a bare-node predicate alongside the schema-derived ones, so
/// the resulting `predicates.js` matches upstream byte-for-byte.
///
/// - `ChainExpression` is created by the `HermesToESTreeAdapter` chain-
///   expression fixup wrapping `OptionalCall` / `OptionalMember`. The
///   equivalent fixup lives in `flow-parser-oxidized/src/index.js`
///   (`rewriteChain`). Upstream JSON has the entry; our schema does not.
const PREDICATE_EXTRA_NODES: &[&str] = &["ChainExpression"];

/// Token predicates emitted at the tail of `predicates.js`. Mirrors upstream's
/// hand-curated `TOKENS` array in
/// `xplat/static_h/tools/hermes-parser/js/scripts/genPredicateFunctions.js`.
/// Each entry is `(name, value, type)` where `type` is one of `"Punctuator"`,
/// `"Keyword"`, or `"Identifier"`. The order matches upstream verbatim so the
/// emitted predicates land in the same order downstream consumers expect.
const PREDICATE_TOKENS: &[(&str, &str, &str)] = &[
    // Both BinaryOperator and UnaryOperator
    ("Minus", "-", "Punctuator"),
    ("Plus", "+", "Punctuator"),
    // UnaryOperator
    ("LogicalNot", "!", "Punctuator"),
    ("UnaryNegation", "~", "Punctuator"),
    ("TypeOf", "typeof", "Keyword"),
    ("Void", "void", "Keyword"),
    ("Delete", "delete", "Keyword"),
    // BinaryOperator
    ("LooseEqual", "==", "Punctuator"),
    ("LooseNotEqual", "!=", "Punctuator"),
    ("StrictEqual", "===", "Punctuator"),
    ("StrictNotEqual", "!==", "Punctuator"),
    ("LessThan", "<", "Punctuator"),
    ("LessThanOrEqualTo", "<=", "Punctuator"),
    ("GreaterThan", ">", "Punctuator"),
    ("GreaterThanOrEqualTo", ">=", "Punctuator"),
    ("BitwiseLeftShift", "<<", "Punctuator"),
    ("BitwiseRightShift", ">>", "Punctuator"),
    ("BitwiseUnsignedRightShift", ">>>", "Punctuator"),
    ("Asterix", "*", "Punctuator"),
    ("ForwardSlash", "/", "Punctuator"),
    ("Percent", "%", "Punctuator"),
    ("Exponentiation", "**", "Punctuator"),
    ("BitwiseOR", "|", "Punctuator"),
    ("BitwiseXOR", "^", "Punctuator"),
    ("BitwiseAND", "&", "Punctuator"),
    ("In", "in", "Keyword"),
    ("InstanceOf", "instanceof", "Keyword"),
    // LogicalOperator
    ("LogicalOR", "||", "Punctuator"),
    ("LogicalAND", "&&", "Punctuator"),
    ("NullishCoalesce", "??", "Punctuator"),
    // AssignmentOperator
    ("Equal", "=", "Punctuator"),
    ("PlusEqual", "+=", "Punctuator"),
    ("MinusEqual", "-=", "Punctuator"),
    ("MultiplyEqual", "*=", "Punctuator"),
    ("DivideEqual", "/=", "Punctuator"),
    ("RemainderEqual", "%=", "Punctuator"),
    ("ExponentateEqual", "**=", "Punctuator"),
    ("BitwiseLeftShiftEqual", "<<=", "Punctuator"),
    ("BitwiseRightShiftEqual", ">>=", "Punctuator"),
    ("BitwiseUnsignedRightShiftEqual", ">>>=", "Punctuator"),
    ("BitwiseOREqual", "|=", "Punctuator"),
    ("BitwiseXOREqual", "^=", "Punctuator"),
    ("BitwiseANDEqual", "&=", "Punctuator"),
    ("LogicalOREqual", "||=", "Punctuator"),
    ("LogicalANDEqual", "&&=", "Punctuator"),
    ("NullishCoalesceEqual", "??=", "Punctuator"),
    // UpdateOperator
    ("Increment", "++", "Punctuator"),
    ("Decrement", "--", "Punctuator"),
    // Type Operators
    // These are duplicates of BinaryOperators, but here for convenience and clarity
    ("UnionType", "|", "Punctuator"),
    ("IntersectionType", "&", "Punctuator"),
    // Misc other keywords
    ("Break", "break", "Keyword"),
    ("Case", "case", "Keyword"),
    ("Catch", "catch", "Keyword"),
    ("Class", "class", "Keyword"),
    ("Const", "const", "Keyword"),
    ("Continue", "continue", "Keyword"),
    ("Debugger", "debugger", "Keyword"),
    ("Default", "default", "Keyword"),
    ("Do", "do", "Keyword"),
    ("Else", "else", "Keyword"),
    ("Enum", "enum", "Keyword"),
    ("Export", "export", "Keyword"),
    ("Extends", "extends", "Keyword"),
    ("Finally", "finally", "Keyword"),
    ("For", "for", "Keyword"),
    ("Function", "function", "Keyword"),
    ("If", "if", "Keyword"),
    ("Implements", "implements", "Keyword"),
    ("Import", "import", "Keyword"),
    ("Interface", "interface", "Keyword"),
    ("New", "new", "Keyword"),
    ("Return", "return", "Keyword"),
    ("Static", "static", "Keyword"),
    ("Super", "super", "Keyword"),
    ("Switch", "switch", "Keyword"),
    ("This", "this", "Keyword"),
    ("Throw", "throw", "Keyword"),
    ("Try", "try", "Keyword"),
    ("Var", "var", "Keyword"),
    ("While", "while", "Keyword"),
    ("With", "with", "Keyword"),
    ("Yield", "yield", "Keyword"),
    // these keywords aren't reported as Keyword tokens by the parser either
    // due to syntax ambiguity, or as they are flow-specific keywords
    ("As", "as", "Identifier"),
    ("Async", "async", "Identifier"),
    ("Await", "await", "Identifier"),
    ("Declare", "declare", "Identifier"),
    ("From", "from", "Identifier"),
    ("Get", "get", "Identifier"),
    ("Let", "let", "Identifier"),
    ("Module", "module", "Identifier"),
    ("Of", "of", "Identifier"),
    ("Set", "set", "Identifier"),
    ("Type", "type", "Identifier"),
    // Misc other punctuators
    ("Comma", ",", "Punctuator"),
    ("Colon", ":", "Punctuator"),
    ("Semicolon", ";", "Punctuator"),
    ("Dot", ".", "Punctuator"),
    ("DotDotDot", "...", "Punctuator"),
    ("OptionalChain", "?.", "Punctuator"),
    ("QuestionMark", "?", "Punctuator"),
    // could call these "round brackets" for consistency, but I think more people call them parentheses
    ("OpeningParenthesis", "(", "Punctuator"),
    ("ClosingParenthesis", ")", "Punctuator"),
    // could call these "Braces", but I think more people call them curlies
    ("OpeningCurlyBracket", "{", "Punctuator"),
    ("ClosingCurlyBracket", "}", "Punctuator"),
    // could call these "Chevrons", but I think more people call them angle brackets
    ("OpeningAngleBracket", "<", "Punctuator"),
    ("ClosingAngleBracket", ">", "Punctuator"),
];

/// Generate `flow-estree-oxidized/src/generated/predicates.js`.
///
/// The output mirrors upstream `hermes-estree/src/generated/predicates.js`:
/// a `/*:: import type { … } from '../types'; */` block followed by per-node
/// type-guard predicates, then literal/comment predicates, then 90+ token
/// predicates. The body is generated mechanically from `SCHEMA` (with the
/// upstream skip lists baked in) plus the hand-curated `PREDICATE_TOKENS`
/// table copied verbatim from upstream's `genPredicateFunctions.js`.
fn generate_estree_predicates() {
    // Build the bare-node estree-type list from SCHEMA + extras, then sort
    // case-insensitively (matches upstream JSON's ordering, which puts
    // `DeclaredPredicate` before `DeclareEnum`, `NullableTypeAnnotation`
    // before `NullLiteralTypeAnnotation`, etc.). Excludes literal subtypes
    // (collapsed to `Literal` upstream), special-case nodes
    // (Identifier/JSXIdentifier/JSXText), the custom-encoded `Literal`
    // (handled separately below), and the predicate exclusion list (TS-only
    // kinds, optional-chain refinements, Flow-only kinds, union aliases
    // without an upstream predicate).
    let mut bare_nodes: Vec<&'static str> = Vec::new();
    for def in SCHEMA.iter() {
        if LITERAL_COLLAPSED_KINDS.contains(&def.name) {
            continue;
        }
        if PREDICATE_SPECIAL_NODES.contains(&def.estree_type) {
            continue;
        }
        if def.custom_emit {
            // Literal — emitted explicitly after the bare-node block.
            continue;
        }
        if PREDICATE_EXCLUDED_TYPES.contains(&def.estree_type) {
            continue;
        }
        if !bare_nodes.contains(&def.estree_type) {
            bare_nodes.push(def.estree_type);
        }
    }
    for extra in PREDICATE_EXTRA_NODES {
        if !bare_nodes.contains(extra) {
            bare_nodes.push(extra);
        }
    }
    // Case-insensitive ASCII sort. Matches upstream JSON's `DeclaredPredicate`
    // < `DeclareEnum` (since uppercase 'D' / 'E' compare equal to lowercase
    // 'd' / 'e' here, the seventh char tiebreaks: 'D' vs 'E').
    bare_nodes.sort_by(|a, b| {
        a.bytes()
            .map(|c| c.to_ascii_lowercase())
            .cmp(b.bytes().map(|c| c.to_ascii_lowercase()))
    });

    // Header — matches upstream's `formatAndWriteSrcArtifact({flow:
    // 'strict-local'})` template byte-for-byte (Copyright + @flow strict-local
    // + @generated + lint pragmas + 'use strict';). Tag is split via
    // interpolation so this Rust source is not itself flagged as generated.
    let header = format!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @{tag}
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run \
`buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
--estree-predicates`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {{vars: \"local\"}}], no-redeclare: 'error' */
/* global $NonMaybeType, Partial, $ReadOnly, $ReadOnlyArray, $FlowFixMe */

'use strict';

",
        tag = "generated"
    );
    print!("{header}");

    // Import block. Order matches upstream's `nodeTypesToImport` push order:
    //   1. hard-coded prefix (ESNode, Token, AFunction, ClassMember, six
    //      literal subtypes)
    //   2. special-case nodes (Identifier, JSXIdentifier, JSXText)
    //   3. alphabetized bare-node estree types
    //   4. `Literal`
    //   5. `LineComment`, `BlockComment`
    //   6. `MostTokens`
    println!("/*::");
    println!("import type {{");
    println!("  ESNode,");
    println!("  Token,");
    println!("  AFunction,");
    println!("  ClassMember,");
    println!("  BigIntLiteral,");
    println!("  BooleanLiteral,");
    println!("  NullLiteral,");
    println!("  NumericLiteral,");
    println!("  RegExpLiteral,");
    println!("  StringLiteral,");
    for special in PREDICATE_SPECIAL_NODES {
        println!("  {special},");
    }
    for name in bare_nodes.iter() {
        println!("  {name},");
    }
    println!("  Literal,");
    println!("  LineComment,");
    println!("  BlockComment,");
    println!("  MostTokens,");
    println!("}} from '../types';");
    println!("*/");
    println!();

    // Special-case predicates (Identifier / JSXIdentifier / JSXText) —
    // emitted with `(X | MostTokens)` implies clause. Whitespace mirrors
    // upstream's template-string output (note the trailing two-space line
    // after each function before the joining blank line).
    for special in PREDICATE_SPECIAL_NODES {
        println!();
        println!(
            "export function is{special}(node /*: ESNode | Token */) /*: implies node is ({special} | MostTokens) */ {{"
        );
        println!("  return node.type === '{special}';");
        println!("}}");
        print!("  ");
        println!();
    }

    // Bare-node predicates (alphabetical). Trailing four-space line per
    // upstream template-string output.
    for name in bare_nodes.iter() {
        println!();
        println!(
            "export function is{name}(node /*: ESNode | Token */) /*: implies node is {name} */ {{"
        );
        println!("  return node.type === '{name}';");
        println!("}}");
        print!("    ");
        println!();
    }

    // `Literal` predicate — appended after the bare-node block (mirrors
    // upstream's `.concat('Literal')` on the iteration list).
    println!();
    println!(
        "export function isLiteral(node /*: ESNode | Token */) /*: implies node is Literal */ {{"
    );
    println!("  return node.type === 'Literal';");
    println!("}}");
    print!("    ");
    println!();

    // Comment predicates (Line, Block) — wire `node.type === 'Line'` /
    // `'Block'` to `LineComment` / `BlockComment` interface refinement.
    for (token_type, comment_name) in [("Line", "LineComment"), ("Block", "BlockComment")] {
        println!();
        println!(
            "export function is{comment_name}(node /*: ESNode | Token */) /*: implies node is (MostTokens | {comment_name}) */ {{"
        );
        println!("  return node.type === '{token_type}';");
        println!("}}");
        print!("    ");
        println!();
    }

    // Token predicates — Identifier-typed entries get the
    // `(Identifier && name === X) || (Keyword && value === X)` form (those
    // are flow-specific or syntactically ambiguous keywords); everything
    // else gets the simple `node.type === T && node.value === X` form.
    for (name, token, token_type) in PREDICATE_TOKENS {
        println!();
        if *token_type == "Identifier" {
            println!(
                "export function is{name}Keyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {{"
            );
            println!("  return (");
            println!("    (node.type === 'Identifier' && node.name === '{token}') ||");
            println!("    (node.type === 'Keyword' && node.value === '{token}')");
            println!("  );");
            println!("}}");
        } else {
            println!(
                "export function is{name}Token(node /*: ESNode | Token */) /*: implies node is MostTokens */ {{"
            );
            println!("  return node.type === '{token_type}' && node.value === '{token}';");
            println!("}}");
        }
        print!("      ");
        println!();
    }
}

// ---------------------------------------------------------------------------
// ESTree selector types codegen (`--estree-selectors`)
// ---------------------------------------------------------------------------

/// ESTree types that the Rust SCHEMA models but upstream's
/// `genSelectorTypes.js` should not emit selectors for. Same skip rationale
/// as `PREDICATE_EXCLUDED_TYPES` (TS-only kinds, optional-chain refinements,
/// EnumBody / RendersType union aliases, Flow-only InterpreterDirective).
/// Kept as a separate const so the two generators evolve independently.
const SELECTOR_EXCLUDED_TYPES: &[&str] = &[
    // Optional-chain refinements collapsed into Call/Member by upstream.
    "OptionalCallExpression",
    "OptionalMemberExpression",
    // Renders refinement union — upstream emits per-variant interfaces only.
    "RendersType",
    // Enum body union — upstream emits per-type body predicates only.
    "EnumBody",
    // TS-only kinds (mirror OCaml's full type space; not surfaced via the
    // Flow-only adapter, so upstream JSON has no entry).
    "ImportEqualsDeclaration",
    "ExternalModuleReference",
    "ExportAssignment",
    "ExportNamespaceSpecifier",
    "NamespaceExportDeclaration",
    "SatisfiesExpression",
    "NonNullExpression",
    "ParameterProperty",
    "DeclareMethodDefinition",
    "AbstractMethodDefinition",
    "AbstractPropertyDefinition",
    "DeclareClassExtendsCall",
    "ConstructorTypeAnnotation",
    "ObjectTypePrivateField",
    "TupleTypeElement",
    "ImportType",
    "TemplateLiteralTypeAnnotation",
    "RendersMaybeType",
    "RendersStarType",
    // Flow-only kinds that exist in our SCHEMA but not in upstream Hermes
    // ESTree JSON (so upstream emits no selector for them).
    "InterpreterDirective",
];

/// ESTree types the JS adapter synthesizes (or upstream emits a selector for)
/// but our SCHEMA does not directly model. Mirrors `PREDICATE_EXTRA_NODES`.
///
/// - `ChainExpression` is created by the deserializer chain-expression fixup.
const SELECTOR_EXTRA_NODES: &[&str] = &["ChainExpression"];

/// Properties to expose for the synthesized SELECTOR_EXTRA_NODES entries.
/// `ChainExpression` has a single `expression` child. Mirrors what upstream's
/// `genSelectorTypes.js` would emit if it iterated these node JSON entries.
fn extra_node_arguments(name: &str) -> &'static [&'static str] {
    match name {
        "ChainExpression" => &["expression"],
        _ => &[],
    }
}

/// Generate `flow-estree-oxidized/src/generated/HermesESTreeSelectorTypes.js.flow`.
///
/// Mirrors upstream's `xplat/static_h/tools/hermes-parser/js/scripts/genSelectorTypes.js`:
///
/// 1. Iterate every NodeKind in SCHEMA (skipping LITERAL_COLLAPSED_KINDS and
///    SELECTOR_EXCLUDED_TYPES), plus the synthesized `Literal` entry, plus
///    SELECTOR_EXTRA_NODES.
/// 2. For each kept node, emit one bare selector
///    `+'<Node>'?: (node: <Node>) => void` (and the matching `:exit` form).
/// 3. For each property on the node, emit a refinement selector
///    `+'<Node>[<prop>]'?: (node: <Node>_With_<prop>) => void` (and `:exit`)
///    plus an interface alias
///    `interface <Node>_With_<prop> extends <Node> { +<prop>: $NonMaybeType<<Node>['<prop>']> }`.
/// 4. Emit the 6 special selectors (`*`, `:statement`, `:declaration`,
///    `:pattern`, `:expression`, `:function`) with hand-curated type unions.
/// 5. Wrap in the standard `formatAndWriteSrcArtifact({flow: 'strict'})`
///    header (Copyright + @flow strict + @generated + lint pragmas).
fn generate_estree_selectors() {
    // Build the list of node names (estree_type) to emit selectors for.
    // Iteration order: SCHEMA order (matches upstream's HermesESTreeJSON
    // iteration), with `Literal` appended, then SELECTOR_EXTRA_NODES.
    let mut nodes: Vec<&'static str> = Vec::new();
    for def in SCHEMA.iter() {
        if LITERAL_COLLAPSED_KINDS.contains(&def.name) {
            continue;
        }
        if SELECTOR_EXCLUDED_TYPES.contains(&def.estree_type) {
            continue;
        }
        if def.custom_emit {
            // `Literal` — emitted explicitly after the SCHEMA iteration to
            // mirror upstream's `.concat({name: 'Literal', arguments: []})`.
            continue;
        }
        if !nodes.contains(&def.estree_type) {
            nodes.push(def.estree_type);
        }
    }
    nodes.push("Literal");
    for extra in SELECTOR_EXTRA_NODES {
        if !nodes.contains(extra) {
            nodes.push(extra);
        }
    }

    // Look up the property list for an estree_type. For SELECTOR_EXTRA_NODES,
    // delegate to `extra_node_arguments`; for schema-modeled nodes, return the
    // first SCHEMA NodeDef matching this estree_type (order: extra_pre_props,
    // properties, extra_post_props — matches upstream JSON arg order).
    fn props_for(name: &str) -> Vec<&'static str> {
        if SELECTOR_EXTRA_NODES.contains(&name) {
            return extra_node_arguments(name).to_vec();
        }
        if name == "Literal" {
            // Custom-encoded; upstream emits it with `arguments: []`.
            return Vec::new();
        }
        for def in SCHEMA.iter() {
            if def.estree_type != name {
                continue;
            }
            let mut out: Vec<&'static str> = Vec::new();
            for (n, _) in def.extra_pre_props.iter() {
                out.push(n);
            }
            for (n, _) in def.properties.iter() {
                out.push(n);
            }
            for (n, _) in def.extra_post_props.iter() {
                out.push(n);
            }
            return out;
        }
        Vec::new()
    }

    // Special selectors: alias name + selector string + member type list.
    // Mirrors upstream's `specialSelectors` array verbatim.
    let star_type: Vec<&'static str> = vec!["ESNode"];
    let statement_type: Vec<&'static str> = nodes
        .iter()
        .copied()
        .filter(|n| n.ends_with("Statement"))
        .collect();
    let declaration_type: Vec<&'static str> = nodes
        .iter()
        .copied()
        .filter(|n| n.ends_with("Declaration"))
        .collect();
    let pattern_type: Vec<&'static str> = nodes
        .iter()
        .copied()
        .filter(|n| n.ends_with("Pattern"))
        .collect();
    // `:expression` = ['Identifier', 'MetaProperty'] ++ *Expression ++ *Literal
    let mut expression_type: Vec<&'static str> = vec!["Identifier", "MetaProperty"];
    for n in nodes.iter() {
        if n.ends_with("Expression") {
            expression_type.push(n);
        }
    }
    for n in nodes.iter() {
        if n.ends_with("Literal") {
            expression_type.push(n);
        }
    }
    let function_type: Vec<&'static str> = vec![
        "FunctionDeclaration",
        "FunctionExpression",
        "ArrowFunctionExpression",
    ];

    let special_selectors: &[(&str, &str, &[&'static str])] = &[
        ("*", "Star", &star_type),
        (":statement", "Statement", &statement_type),
        (":declaration", "Declaration", &declaration_type),
        (":pattern", "Pattern", &pattern_type),
        (":expression", "Expression", &expression_type),
        (":function", "Function", &function_type),
    ];

    // Build imports list. Upstream pushes `'ESNode'` first, then each node
    // name in iteration order.
    let mut imports: Vec<&'static str> = vec!["ESNode"];
    for name in nodes.iter() {
        imports.push(name);
    }

    // Header — matches upstream's `formatAndWriteSrcArtifact({flow: 'strict'})`
    // template. Tag is split via interpolation so this Rust source itself is
    // not flagged as generated.
    let header = format!(
        "\
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @{tag}
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run \
`buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- \
--estree-selectors`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {{vars: \"local\"}}], no-redeclare: 'error' */
/* global $NonMaybeType, Partial, $ReadOnly, $ReadOnlyArray, $FlowFixMe */

'use strict';

",
        tag = "generated"
    );
    print!("{header}");

    // Emit the import block.
    println!("import type {{");
    for name in imports.iter() {
        println!("{name},");
    }
    println!("}} from '../types';");
    println!();

    // Emit the per-node `_With_<prop>` interface aliases (one per Node-prop
    // pair) followed by the per-special-selector type aliases.
    for name in nodes.iter() {
        for arg in props_for(name) {
            println!(
                "interface {name}_With_{arg} extends {name} {{ +{arg}: $NonMaybeType<{name}['{arg}']> }}"
            );
        }
    }
    for (_selector, alias_name, type_list) in special_selectors {
        println!(
            "type {alias_name}SpecialSelector = {types}",
            types = type_list.join(" | ")
        );
    }
    println!();

    // Emit the selector dictionary. Upstream concatenates enter selectors
    // first, then exit selectors. Each block is comma-separated; the final
    // entry of each block also has a trailing comma (mirrors the JS string
    // template's `enterSelectors.join(',\n')` followed by `,\n` separator).
    println!("export type ESQueryNodeSelectorsWithoutFallback = {{");

    // Enter selectors: bare-node + per-arg + special.
    let mut enter_lines: Vec<String> = Vec::new();
    for name in nodes.iter() {
        enter_lines.push(format!("+'{name}'?: (node: {name}) => void"));
        for arg in props_for(name) {
            enter_lines.push(format!(
                "+'{name}[{arg}]'?: (node: {name}_With_{arg}) => void"
            ));
        }
    }
    for (selector, alias_name, _ty) in special_selectors {
        enter_lines.push(format!(
            "+'{selector}'?: (node: {alias_name}SpecialSelector) => void"
        ));
    }

    // Exit selectors: same shape, with `:exit` appended to each selector key.
    let mut exit_lines: Vec<String> = Vec::new();
    for name in nodes.iter() {
        exit_lines.push(format!("+'{name}:exit'?: (node: {name}) => void"));
        for arg in props_for(name) {
            exit_lines.push(format!(
                "+'{name}[{arg}]:exit'?: (node: {name}_With_{arg}) => void"
            ));
        }
    }
    for (selector, alias_name, _ty) in special_selectors {
        exit_lines.push(format!(
            "+'{selector}:exit'?: (node: {alias_name}SpecialSelector) => void"
        ));
    }

    println!("{},", enter_lines.join(",\n"));
    println!("{},", exit_lines.join(",\n"));
    println!("}};");
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Verifies the `--estree-types` cross-check fires when a SCHEMA NodeKind
    /// has no matching upstream declaration. This is the protection against
    /// future SCHEMA/upstream drift the team-lead and reviewer required.
    #[test]
    fn estree_types_cross_check_flags_missing_kind() {
        // Synthetic SCHEMA with one kind that upstream does not declare.
        let synthetic_schema: &[NodeDef] = &[NodeDef {
            kind_id: 9999,
            name: "FlowOnlyTotallyMadeUpKind",
            estree_type: "FlowOnlyTotallyMadeUpKind",
            properties: &[],
            extra_pre_props: &[],
            extra_post_props: &[],
            nested_object_props: &[],
            custom_emit: false,
        }];
        // Upstream that declares only some other interface.
        let upstream = "export interface SomeOtherKind extends BaseNode {}\n";

        let missing = estree_types_missing_kinds(synthetic_schema, upstream);
        assert_eq!(
            missing,
            vec!["FlowOnlyTotallyMadeUpKind"],
            "cross-check should flag a SCHEMA kind absent from upstream",
        );
    }

    /// Negative case: upstream contains the kind, so cross-check passes.
    #[test]
    fn estree_types_cross_check_passes_when_present() {
        let synthetic_schema: &[NodeDef] = &[NodeDef {
            kind_id: 9999,
            name: "PresentKind",
            estree_type: "PresentKind",
            properties: &[],
            extra_pre_props: &[],
            extra_post_props: &[],
            nested_object_props: &[],
            custom_emit: false,
        }];
        let upstream = "export interface PresentKind extends BaseNode {}\n";

        let missing = estree_types_missing_kinds(synthetic_schema, upstream);
        assert!(
            missing.is_empty(),
            "cross-check should pass when upstream declares the kind, got {missing:?}",
        );
    }

    /// `KNOWN_TYPES_WITHOUT_INTERFACE` entries are exempt from the cross-check.
    #[test]
    fn estree_types_cross_check_respects_known_exclusion_list() {
        // Pick the first entry from KNOWN_TYPES_WITHOUT_INTERFACE — guaranteed
        // to be a real exclusion-list name.
        let excluded = KNOWN_TYPES_WITHOUT_INTERFACE[0];
        let synthetic_schema: &[NodeDef] = &[NodeDef {
            kind_id: 9999,
            name: excluded,
            estree_type: excluded,
            properties: &[],
            extra_pre_props: &[],
            extra_post_props: &[],
            nested_object_props: &[],
            custom_emit: false,
        }];
        let upstream = ""; // empty — would otherwise flag

        let missing = estree_types_missing_kinds(synthetic_schema, upstream);
        assert!(
            missing.is_empty(),
            "kinds in KNOWN_TYPES_WITHOUT_INTERFACE should be exempt, got {missing:?}",
        );
    }

    /// Today's invariant: against the real upstream types.js, every SCHEMA
    /// NodeKind is either declared or in the exclusion list. If this test
    /// breaks, either upstream regressed or a new Flow-only NodeKind was
    /// added without a corresponding upstream entry.
    #[test]
    fn estree_types_cross_check_passes_for_real_schema_today() {
        let path = std::env::var("HERMES_ESTREE_TYPES_JS")
            .unwrap_or_else(|_| DEFAULT_HERMES_ESTREE_TYPES_PATH.to_string());
        let Ok(upstream) = std::fs::read_to_string(&path) else {
            // Skip if upstream file isn't readable from the test sandbox.
            // The codegen binary panics in that case (with a clearer error),
            // which is what we want for actual regen — the unit test is just
            // a smoke check.
            return;
        };
        let missing = estree_types_missing_kinds(SCHEMA, &upstream);
        assert!(
            missing.is_empty(),
            "SCHEMA NodeKinds missing from upstream {path}: {missing:?}\n\
             Either add an upstream interface or extend \
             KNOWN_TYPES_WITHOUT_INTERFACE in codegen.rs with attribution.",
        );
    }
}
