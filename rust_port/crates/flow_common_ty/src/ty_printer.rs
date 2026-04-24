/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Write;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser_utils_output::layout;
use flow_parser_utils_output::layout::LayoutNode;
use flow_parser_utils_output::pretty_printer;

use crate::ty::*;
use crate::ty_symbol::*;
use crate::ty_utils;

/***********
 * Utils   *
 ***********/

const CROP_SYMBOL: &str = "...";
const MAX_SIZE: usize = 1000;

pub fn better_quote(prefer_single_quotes: bool, s: &str) -> String {
    let single_quotes = s.chars().filter(|&c| c == '\'').count();
    let double_quotes = s.chars().filter(|&c| c == '"').count();

    let quote_char = if prefer_single_quotes {
        if single_quotes <= double_quotes {
            '\''
        } else {
            '"'
        }
    } else if double_quotes <= single_quotes {
        '"'
    } else {
        '\''
    };
    quote_char.to_string()
}

pub fn property_key_quotes_needed(x: &str) -> bool {
    if x.is_empty() {
        return true;
    }

    let mut chars = x.chars();
    let first = chars.next().unwrap();

    if !first.is_ascii_alphabetic() && first != '$' && first != '_' {
        return true;
    }

    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '$' && c != '_' {
            return true;
        }
    }

    false
}

pub fn utf8_escape(quote: &str, s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let cp = c as u32;
        let next_cp = chars.peek().map(|next| *next as u32);
        match cp {
            0x0 => {
                let zero = match next_cp {
                    Some(n) if (0x30..=0x39).contains(&n) => "\\x00",
                    _ => "\\0",
                };
                result.push_str(zero);
            }
            0x8 => result.push_str("\\b"),
            0x9 => result.push_str("\\t"),
            0xA => result.push_str("\\n"),
            0xB => result.push_str("\\v"),
            0xC => result.push_str("\\f"),
            0xD => result.push_str("\\r"),
            0x22 if quote == "\"" => result.push_str("\\\""),
            0x27 if quote == "'" => result.push_str("\\'"),
            0x5C => result.push_str("\\\\"),
            n if 0x1F < n && n < 0x7F => result.push(c),
            n if n < 0x100 => {
                write!(result, "\\x{:02x}", n).unwrap();
            }
            n if n < 0x10000 => {
                write!(result, "\\u{:04x}", n).unwrap();
            }
            n => {
                let n2 = n - 0x10000;
                let hi = 0xD800 | (n2 >> 10);
                let lo = 0xDC00 | (n2 & 0x3FF);
                write!(result, "\\u{:4x}", hi).unwrap();
                write!(result, "\\u{:4x}", lo).unwrap();
            }
        }
    }

    result
}

pub fn variance_string(p: Polarity) -> &'static str {
    match p {
        Polarity::Positive => "+",
        Polarity::Negative => "-",
        Polarity::Neutral => "",
    }
}

fn crop_atom() -> LayoutNode {
    LayoutNode::atom(CROP_SYMBOL.to_string())
}

fn in_quotes(prefer_single_quotes: bool, s: &str) -> Vec<LayoutNode> {
    let quote = better_quote(prefer_single_quotes, s);
    let escaped = utf8_escape(&quote, s);
    vec![
        LayoutNode::atom(quote.clone()),
        LayoutNode::atom(escaped),
        LayoutNode::atom(quote),
    ]
}

fn identifier(name: &Name) -> LayoutNode {
    LayoutNode::atom(name.to_string())
}

fn variance(p: Polarity) -> LayoutNode {
    match p {
        Polarity::Positive => LayoutNode::atom("+".to_string()),
        Polarity::Negative => LayoutNode::atom("-".to_string()),
        Polarity::Neutral => LayoutNode::empty(),
    }
}

fn wrap_in_parens(node: LayoutNode) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom("(".to_string()),
        node,
        LayoutNode::atom(")".to_string()),
    ])
}

fn local_name_of_symbol<L>(symbol: &Symbol<L>) -> Name {
    // If the type is imported use the local name
    match &symbol.sym_provenance {
        Provenance::Remote(RemoteInfo {
            imported_as: Some(ImportedIdent(_, name, _)),
        }) => Name::new(name.as_str()),
        _ => symbol.sym_name.dupe(),
    }
}

fn builtin_value<L: Dupe>(pv: &BuiltinOrSymbol<L>) -> LayoutNode {
    match pv {
        BuiltinOrSymbol::FunProto => LayoutNode::atom("Function.prototype".to_string()),
        BuiltinOrSymbol::ObjProto => LayoutNode::atom("Object.prototype".to_string()),
        BuiltinOrSymbol::FunProtoBind => LayoutNode::atom("Function.prototype.bind".to_string()),
        BuiltinOrSymbol::TSymbol(symbol) => {
            LayoutNode::atom(local_name_of_symbol(symbol).to_string())
        }
    }
}

fn option<T, F>(f: F, opt: &Option<T>) -> LayoutNode
where
    F: FnOnce(&T) -> LayoutNode,
{
    match opt {
        Some(val) => f(val),
        None => LayoutNode::empty(),
    }
}

/*************************)
(* Main Transformation   *)
(*************************/

fn layout_of_elt<L: Dupe>(opts: &PrinterOptions, elt: &Elt<L>) -> LayoutNode {
    match elt {
        Elt::Type(t) => type_impl(opts, 0, t, opts.size),
        Elt::Decl(d) => decl(opts, 0, d),
    }
}

// The depth parameter is useful for formatting unions: Top-level does not
// get parentheses.
fn type_impl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    t: &Ty<L>,
    mut size: usize,
) -> LayoutNode {
    let depth = depth + 1;

    match t {
        Ty::Bound(data) => {
            let (_, name) = data.as_ref();
            LayoutNode::atom(name.to_string())
        }
        Ty::Any(kind) => any(opts, depth, kind),
        Ty::Top => LayoutNode::atom("mixed".to_string()),
        Ty::Bot(_) => LayoutNode::atom("empty".to_string()),
        Ty::Void => LayoutNode::atom("void".to_string()),
        Ty::Null => LayoutNode::atom("null".to_string()),
        Ty::Num => LayoutNode::atom("number".to_string()),
        Ty::Str => LayoutNode::atom("string".to_string()),
        Ty::Bool => LayoutNode::atom("boolean".to_string()),
        Ty::BigInt => LayoutNode::atom("bigint".to_string()),
        Ty::Symbol => LayoutNode::atom("symbol".to_string()),

        Ty::StrLit(raw) => layout::fuse(in_quotes(opts.prefer_single_quotes, &raw.to_string())),
        Ty::NumLit(raw) => LayoutNode::atom(raw.to_string()),
        Ty::BoolLit(value) => LayoutNode::atom(if *value { "true" } else { "false" }.to_string()),
        Ty::BigIntLit(raw) => LayoutNode::atom(raw.to_string()),

        Ty::Fun(func) => {
            let sep = layout::fuse(vec![
                layout::pretty_space(),
                LayoutNode::atom("=>".to_string()),
            ]);
            type_function(opts, depth, &sep, func, size)
        }
        Ty::Obj(obj) => type_object(opts, depth, obj, size),
        Ty::Arr(arr) => type_array(opts, depth, arr, size),
        Ty::Generic(g) => type_generic(opts, depth, g, size),
        Ty::Union(_, t1, t2, ts) => {
            let mut all = vec![t1.as_ref()];
            all.push(t2.as_ref());
            for t in ts.iter() {
                all.push(t.as_ref());
            }
            type_union(opts, depth, &all, size)
        }
        Ty::Inter(t1, t2, ts) => {
            let mut all = vec![t1.as_ref()];
            all.push(t2.as_ref());
            for t in ts.iter() {
                all.push(t.as_ref());
            }
            type_intersection(opts, depth, &all, size)
        }

        Ty::Utility(u) => utility(opts, depth, u, size),

        Ty::IndexedAccess {
            _object,
            index,
            optional,
        } => {
            let left_delim = if *optional { "?.[" } else { "[" };
            layout::fuse(vec![
                type_impl(opts, depth, _object.as_ref(), size),
                LayoutNode::atom(left_delim.to_string()),
                type_impl(opts, depth, index.as_ref(), size),
                LayoutNode::atom("]".to_string()),
            ])
        }

        Ty::Tup { elements, inexact } => {
            let mut elts_rev = Vec::new();
            for (idx, elem) in elements.iter().enumerate() {
                if size == 0 {
                    elts_rev.push(crop_atom());
                    break;
                }
                size -= 1;
                elts_rev.push(tuple_element(opts, depth, idx, elem, size));
            }
            if *inexact {
                elts_rev.push(LayoutNode::atom("...".to_string()));
            }
            layout::list(
                None,
                Some((
                    LayoutNode::atom("[".to_string()),
                    LayoutNode::atom("]".to_string()),
                )),
                Some(LayoutNode::atom(",".to_string())),
                false,
                None,
                None,
                elts_rev,
            )
        }

        Ty::InlineInterface(iface) => type_interface(
            opts,
            depth,
            &iface.if_extends,
            &iface.if_props,
            &iface.if_dict,
            size,
        ),

        Ty::TypeOf(data) => {
            let (pv, targs) = data.as_ref();
            layout::fuse(vec![
                LayoutNode::atom("typeof".to_string()),
                layout::space(),
                builtin_value(pv),
                option(|args| type_args(opts, depth, args, size), targs),
            ])
        }

        Ty::Conditional {
            check_type,
            extends_type,
            true_type,
            false_type,
        } => layout::group(vec![
            layout::fuse(vec![
                type_with_parens(opts, depth, check_type.as_ref(), size),
                layout::space(),
                LayoutNode::atom("extends".to_string()),
                layout::space(),
                type_with_parens(opts, depth, extends_type.as_ref(), size),
            ]),
            LayoutNode::indent(layout::fuse(vec![
                layout::pretty_line(),
                LayoutNode::atom("?".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, true_type.as_ref(), size),
                layout::pretty_line(),
                LayoutNode::atom(":".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, false_type.as_ref(), size),
            ])),
        ]),

        Ty::Infer(data) => {
            let (symbol, bound) = data.as_ref();
            layout::fuse(vec![
                LayoutNode::atom("infer".to_string()),
                layout::space(),
                layout::fuse(vec![
                    identifier(&symbol.sym_name),
                    option(
                        |t| {
                            layout::fuse(vec![
                                layout::space(),
                                LayoutNode::atom("extends".to_string()),
                                layout::space(),
                                type_impl(opts, depth, t.as_ref(), size),
                            ])
                        },
                        bound,
                    ),
                ]),
            ])
        }

        Ty::Component {
            regular_props,
            renders,
        } => layout::fuse(vec![
            LayoutNode::atom("component".to_string()),
            type_component_sig(opts, depth, regular_props, renders.as_ref(), size),
        ]),

        Ty::Renders(t, variant) => {
            let renders_str = match variant {
                RendersKind::RendersNormal => "renders",
                RendersKind::RendersMaybe => "renders?",
                RendersKind::RendersStar => "renders*",
            };
            layout::fuse(vec![
                LayoutNode::atom(renders_str.to_string()),
                layout::space(),
                type_with_parens(opts, depth, t.as_ref(), size),
            ])
        }
    }
}

fn any<L: Dupe>(opts: &PrinterOptions, depth: usize, kind: &AnyKind<L>) -> LayoutNode {
    let kind_str = match kind {
        AnyKind::Annotated(_) => "explicit",
        AnyKind::Recursive => "recursive",
        AnyKind::Placeholder => "placeholder",
        AnyKind::AnyError(_) | AnyKind::Unsound(_) | AnyKind::Untyped => "implicit",
    };

    layout::fuse(vec![
        LayoutNode::atom("any".to_string()),
        if depth == 1 && opts.with_comments {
            layout::fuse(vec![
                layout::pretty_space(),
                wrap_in_parens(LayoutNode::atom(kind_str.to_string())),
            ])
        } else {
            LayoutNode::empty()
        },
    ])
}

fn type_function<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    sep: &LayoutNode,
    func: &FunT<L>,
    mut size: usize,
) -> LayoutNode {
    let mut params = Vec::new();
    for param in func.fun_params.iter() {
        if size == 0 {
            params.push(crop_atom());
            break;
        }
        size -= 1;
        params.push(type_function_param(opts, depth, param, size));
    }

    if let Some((name, t)) = &func.fun_rest_param {
        if size > 0 {
            size -= 1;
            let rest_param = type_function_param(
                opts,
                depth,
                &(
                    name.dupe(),
                    t.dupe(),
                    FunParam {
                        prm_optional: false,
                    },
                ),
                size,
            );
            params.push(layout::fuse(vec![
                LayoutNode::atom("...".to_string()),
                rest_param,
            ]));
        }
    }

    let hook_prefix = if matches!(func.fun_effect, FunEffect::Hook) {
        vec![LayoutNode::atom("hook".to_string()), layout::pretty_space()]
    } else {
        vec![]
    };

    let type_params = match &func.fun_type_params {
        Some(tparams) if size > 0 => {
            size -= 1;
            type_parameter(opts, depth, tparams, size)
        }
        _ => LayoutNode::empty(),
    };

    let params_list = layout::list(
        None, // break_mode
        Some((
            LayoutNode::atom("(".to_string()),
            LayoutNode::atom(")".to_string()),
        )), // wrap
        Some(LayoutNode::atom(",".to_string())), // sep
        false, // trailing
        None, // inline
        None, // indent
        params,
    );

    let return_type = if size > 0 {
        size -= 1;
        return_t(opts, depth, &func.fun_return, size)
    } else {
        crop_atom()
    };

    let mut elements = hook_prefix;
    elements.push(type_params);
    elements.push(params_list);
    elements.push(sep.clone());
    elements.push(layout::pretty_space());
    elements.push(return_type);

    layout::fuse(elements)
}

fn type_function_param<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    param: &(Option<FlowSmolStr>, std::sync::Arc<Ty<L>>, FunParam),
    size: usize,
) -> LayoutNode {
    let (name, annot, fun_param) = param;

    let name_part = match name {
        Some(id) => layout::fuse(vec![
            LayoutNode::atom(id.to_string()),
            if fun_param.prm_optional {
                LayoutNode::atom("?".to_string())
            } else {
                LayoutNode::empty()
            },
            LayoutNode::atom(":".to_string()),
            layout::pretty_space(),
        ]),
        None => LayoutNode::empty(),
    };

    layout::fuse(vec![
        name_part,
        type_impl(opts, depth, annot.as_ref(), size),
    ])
}

fn return_t<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    ret: &ReturnT<L>,
    size: usize,
) -> LayoutNode {
    match ret {
        ReturnT::ReturnType(t) => type_impl(opts, depth, t.as_ref(), size),
        ReturnT::TypeGuard(implies, x, t) => {
            let mut elements = Vec::new();
            if *implies {
                elements.push(LayoutNode::atom("implies".to_string()));
            }
            elements.push(LayoutNode::atom(x.to_string()));
            elements.push(LayoutNode::atom("is".to_string()));
            elements.push(type_impl(opts, depth, t.as_ref(), size));
            layout::fuse_with_space(elements)
        }
    }
}

fn type_parameter<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    params: &[TypeParam<L>],
    mut size: usize,
) -> LayoutNode {
    let mut elements = Vec::new();
    for param in params.iter() {
        if size == 0 {
            elements.push(crop_atom());
            break;
        }
        size -= 1;
        elements.push(type_param(opts, depth, param, size));
    }

    layout::list(
        None,
        Some((
            LayoutNode::atom("<".to_string()),
            LayoutNode::atom(">".to_string()),
        )),
        Some(LayoutNode::atom(",".to_string())),
        false,
        None,
        None,
        elements,
    )
}

fn type_param<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    param: &TypeParam<L>,
    size: usize,
) -> LayoutNode {
    let const_prefix = if param.tp_const {
        layout::fuse(vec![LayoutNode::atom("const".to_string()), layout::space()])
    } else {
        LayoutNode::empty()
    };

    let bound = match &param.tp_bound {
        Some(t) if size > 0 => type_annotation(opts, depth, t.as_ref(), size),
        _ => LayoutNode::empty(),
    };

    let default = match &param.tp_default {
        Some(t) if size > 0 => layout::fuse(vec![
            layout::pretty_space(),
            LayoutNode::atom("=".to_string()),
            layout::pretty_space(),
            type_impl(opts, depth, t.as_ref(), size),
        ]),
        _ => LayoutNode::empty(),
    };

    layout::fuse(vec![
        const_prefix,
        variance(param.tp_polarity),
        LayoutNode::atom(param.tp_name.to_string()),
        bound,
        default,
    ])
}

fn type_annotation<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    t: &Ty<L>,
    size: usize,
) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom(":".to_string()),
        layout::pretty_space(),
        type_impl(opts, depth, t, size),
    ])
}

fn type_object<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    obj: &ObjT<L>,
    mut size: usize,
) -> LayoutNode {
    let s_exact = if matches!(obj.obj_kind, ObjKind::ExactObj) && !opts.exact_by_default {
        LayoutNode::atom("|".to_string())
    } else {
        LayoutNode::empty()
    };

    let mut props = Vec::new();
    for prop in obj.obj_props.iter() {
        if size == 0 {
            props.push(crop_atom());
            break;
        }
        size -= 1;
        props.push(type_object_property(opts, depth, prop, size));
    }

    match &obj.obj_kind {
        ObjKind::IndexedObj(d) if size > 0 => {
            size -= 1;
            props.insert(0, type_dict(opts, depth, d, size));
        }
        ObjKind::InexactObj => {
            props.push(LayoutNode::atom("...".to_string()));
        }
        _ => {}
    }

    layout::list(
        None,
        Some((
            layout::fuse(vec![LayoutNode::atom("{".to_string()), s_exact.clone()]),
            layout::fuse(vec![s_exact, LayoutNode::atom("}".to_string())]),
        )),
        Some(LayoutNode::atom(",".to_string())),
        false,
        None,
        None,
        props,
    )
}

fn type_object_property<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    prop: &Prop<L>,
    size: usize,
) -> LayoutNode {
    let to_key = |name: &Name| -> LayoutNode {
        let name_str = name.to_string();
        if property_key_quotes_needed(&name_str) {
            let quote = better_quote(opts.prefer_single_quotes, &name_str);
            layout::fuse(vec![
                LayoutNode::atom(quote.clone()),
                LayoutNode::atom(utf8_escape(&quote, &name_str)),
                LayoutNode::atom(quote),
            ])
        } else {
            identifier(name)
        }
    };

    match prop {
        Prop::NamedProp {
            name,
            prop: named_prop,
            ..
        } => match named_prop {
            NamedProp::Field {
                t,
                polarity,
                optional,
            } => layout::fuse(vec![
                variance(*polarity),
                to_key(name),
                if *optional {
                    LayoutNode::atom("?".to_string())
                } else {
                    LayoutNode::empty()
                },
                LayoutNode::atom(":".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, t.as_ref(), size),
            ]),
            NamedProp::Method(func) => layout::fuse(vec![
                to_key(name),
                type_function(opts, depth, &LayoutNode::atom(":".to_string()), func, size),
            ]),
            NamedProp::Get(t) => layout::group(vec![layout::fuse(vec![
                LayoutNode::atom("get".to_string()),
                layout::space(),
                to_key(name),
                LayoutNode::atom("(".to_string()),
                layout::softline(),
                LayoutNode::atom(")".to_string()),
                LayoutNode::atom(":".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, t.as_ref(), size),
            ])]),
            NamedProp::Set(t) => layout::group(vec![layout::fuse(vec![
                LayoutNode::atom("set".to_string()),
                layout::space(),
                to_key(name),
                layout::wrap_and_indent(
                    (
                        LayoutNode::atom("(".to_string()),
                        LayoutNode::atom(")".to_string()),
                    ),
                    None,
                    vec![type_impl(opts, depth, t.as_ref(), size)],
                ),
                LayoutNode::atom(":".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, &Ty::<L>::Void, size),
            ])]),
        },
        Prop::CallProp(func) => {
            type_function(opts, depth, &LayoutNode::atom(":".to_string()), func, size)
        }
        Prop::SpreadProp(t) => layout::fuse(vec![
            LayoutNode::atom("...".to_string()),
            type_impl(opts, depth, t.as_ref(), size),
        ]),
        Prop::MappedTypeProp {
            key_tparam,
            source,
            prop,
            flags,
            homomorphic,
        } => {
            let optional_modifier = match flags.optional {
                MappedTypeOptionalFlag::RemoveOptional => LayoutNode::atom("-?".to_string()),
                MappedTypeOptionalFlag::MakeOptional => LayoutNode::atom("?".to_string()),
                MappedTypeOptionalFlag::KeepOptionality => LayoutNode::empty(),
            };
            let variance_token = match flags.variance {
                MappedTypeVariance::OverrideVariance(pol) => variance(pol),
                MappedTypeVariance::RemoveVariance(Polarity::Positive) => layout::fuse(vec![
                    LayoutNode::atom("-".to_string()),
                    LayoutNode::atom("readonly ".to_string()),
                ]),
                MappedTypeVariance::RemoveVariance(Polarity::Negative)
                | MappedTypeVariance::RemoveVariance(Polarity::Neutral)
                | MappedTypeVariance::KeepVariance => LayoutNode::empty(),
            };

            layout::fuse(vec![
                variance_token,
                LayoutNode::atom("[".to_string()),
                LayoutNode::atom(key_tparam.tp_name.to_string()),
                LayoutNode::atom(" in ".to_string()),
                match homomorphic {
                    MappedTypeHomomorphicFlag::Homomorphic => {
                        LayoutNode::atom("keyof ".to_string())
                    }
                    MappedTypeHomomorphicFlag::SemiHomomorphic(_)
                    | MappedTypeHomomorphicFlag::Unspecialized => LayoutNode::empty(),
                },
                type_impl(opts, depth, source.as_ref(), size),
                LayoutNode::atom("]".to_string()),
                optional_modifier,
                LayoutNode::atom(":".to_string()),
                layout::pretty_space(),
                type_impl(opts, depth, prop.as_ref(), size),
            ])
        }
    }
}

fn type_dict<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    dict: &Dict<L>,
    size: usize,
) -> LayoutNode {
    let name_part = match &dict.dict_name {
        Some(id) => layout::fuse(vec![
            LayoutNode::atom(id.to_string()),
            LayoutNode::atom(":".to_string()),
            layout::pretty_space(),
        ]),
        None => LayoutNode::empty(),
    };

    layout::fuse(vec![
        variance(dict.dict_polarity),
        LayoutNode::atom("[".to_string()),
        name_part,
        type_impl(opts, depth, dict.dict_key.as_ref(), size),
        LayoutNode::atom("]".to_string()),
        LayoutNode::atom(":".to_string()),
        layout::pretty_space(),
        type_impl(opts, depth, dict.dict_value.as_ref(), size),
    ])
}

fn type_interface<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    extends: &[GenericT<L>],
    props: &[Prop<L>],
    dict: &Option<Dict<L>>,
    mut size: usize,
) -> LayoutNode {
    let extends_node = if extends.is_empty() {
        LayoutNode::empty()
    } else {
        let mut extend_list = Vec::new();
        for g in extends.iter() {
            if size == 0 {
                extend_list.push(crop_atom());
                break;
            }
            size -= 1;
            extend_list.push(type_generic(opts, depth, g, size));
        }
        layout::fuse_with_space(vec![
            LayoutNode::atom("extends".to_string()),
            layout::list(
                None,
                None,
                Some(LayoutNode::atom(",".to_string())),
                false,
                None,
                None,
                extend_list,
            ),
        ])
    };

    let mut properties = Vec::new();
    for prop in props.iter() {
        if size == 0 {
            properties.push(crop_atom());
            break;
        }
        size -= 1;
        properties.push(type_object_property(opts, depth, prop, size));
    }

    if let Some(d) = dict {
        if size > 0 {
            size -= 1;
            properties.insert(0, type_dict(opts, depth, d, size));
        }
    }

    let body = layout::list(
        None,
        Some((
            LayoutNode::atom("{".to_string()),
            LayoutNode::atom("}".to_string()),
        )),
        Some(LayoutNode::atom(";".to_string())),
        false,
        None,
        None,
        properties,
    );

    layout::fuse_with_space(vec![
        LayoutNode::atom("interface".to_string()),
        extends_node,
        body,
    ])
}

fn type_component_sig<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    regular_props: &ComponentProps<L>,
    renders: Option<&Arc<Ty<L>>>,
    mut size: usize,
) -> LayoutNode {
    let to_key = |name: &Name| -> LayoutNode {
        let name_str = name.to_string();
        if property_key_quotes_needed(&name_str) {
            let quote = better_quote(opts.prefer_single_quotes, &name_str);
            layout::fuse(vec![
                LayoutNode::atom(quote.clone()),
                LayoutNode::atom(utf8_escape(&quote, &name_str)),
                LayoutNode::atom(quote),
            ])
        } else {
            LayoutNode::atom(name_str)
        }
    };

    let params = match regular_props {
        ComponentProps::UnflattenedComponentProps(t) => {
            vec![layout::fuse(vec![
                LayoutNode::atom("...".to_string()),
                type_impl(opts, depth, t.as_ref(), size),
            ])]
        }
        ComponentProps::FlattenedComponentProps { props, inexact } => {
            let mut params_list = Vec::new();
            for prop in props.iter() {
                if size == 0 {
                    params_list.push(crop_atom());
                    break;
                }
                size -= 1;
                let FlattenedComponentProp::FlattenedComponentProp {
                    name, optional, t, ..
                } = prop;
                params_list.push(layout::fuse(vec![
                    to_key(name),
                    if *optional {
                        LayoutNode::atom("?".to_string())
                    } else {
                        LayoutNode::empty()
                    },
                    LayoutNode::atom(":".to_string()),
                    layout::pretty_space(),
                    type_impl(opts, depth, t.as_ref(), size),
                ]));
            }
            if *inexact {
                params_list.push(LayoutNode::atom("...{...}".to_string()));
            }
            params_list
        }
    };

    let renders_node = match renders {
        None => LayoutNode::empty(),
        Some(t) => match t.as_ref() {
            Ty::Renders(_, _) => layout::fuse(vec![
                layout::space(),
                type_impl(opts, depth, t.as_ref(), size),
            ]),
            _ => layout::fuse(vec![
                layout::space(),
                LayoutNode::atom("renders".to_string()),
                layout::space(),
                type_with_parens(opts, depth, t.as_ref(), size),
            ]),
        },
    };

    layout::fuse(vec![
        layout::list(
            None,
            Some((
                LayoutNode::atom("(".to_string()),
                LayoutNode::atom(")".to_string()),
            )),
            Some(LayoutNode::atom(",".to_string())),
            false,
            None,
            None,
            params,
        ),
        renders_node,
    ])
}

fn tuple_element<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    idx: usize,
    elem: &TupleElement<L>,
    size: usize,
) -> LayoutNode {
    match elem {
        TupleElement::TupleElement {
            name,
            t,
            polarity,
            optional,
        } => {
            let opt_node = if *optional {
                LayoutNode::atom("?".to_string())
            } else {
                LayoutNode::empty()
            };

            let id_nodes = |id: &str| -> Vec<LayoutNode> {
                vec![
                    LayoutNode::atom(id.to_string()),
                    opt_node.clone(),
                    LayoutNode::atom(":".to_string()),
                    layout::pretty_space(),
                ]
            };

            let name_nodes = match (name, polarity, optional) {
                (Some(n), _, _) => id_nodes(n.as_str()),
                (None, Polarity::Neutral, false) => vec![],
                // We cannot omit the name of a non-neutral variance or optional
                // element. If the name is missing, add a default name "_".
                (None, _, _) => id_nodes(&format!("_{}", idx)),
            };

            let mut nodes = vec![variance(*polarity)];
            nodes.extend(name_nodes);
            nodes.push(type_impl(opts, depth, t.as_ref(), size));
            layout::fuse(nodes)
        }
        TupleElement::TupleSpread { name, t } => {
            let name_part = match name {
                Some(n) => layout::fuse(vec![
                    LayoutNode::atom(n.to_string()),
                    LayoutNode::atom(":".to_string()),
                    layout::pretty_space(),
                ]),
                None => LayoutNode::empty(),
            };
            layout::fuse(vec![
                LayoutNode::atom("...".to_string()),
                name_part,
                type_impl(opts, depth, t.as_ref(), size),
            ])
        }
    }
}

fn utility<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    u: &Utility<L>,
    size: usize,
) -> LayoutNode {
    use crate::ty::string_of_utility_ctor;
    use crate::ty::types_of_utility;

    match u {
        Utility::ReadOnly(t) if opts.ts_syntax => {
            if matches!(t.as_ref(), Ty::Tup { .. }) {
                layout::fuse(vec![
                    LayoutNode::atom("readonly".to_string()),
                    layout::space(),
                    type_impl(opts, depth, t.as_ref(), size),
                ])
            } else {
                let ctor = string_of_utility_ctor(u);
                let ts = types_of_utility(u);
                type_reference(
                    opts,
                    depth,
                    identifier(&Name::new(ctor)),
                    ts.as_deref(),
                    size,
                )
            }
        }
        Utility::Keys(t) => layout::fuse(vec![
            LayoutNode::atom("keyof".to_string()),
            layout::space(),
            type_with_parens(opts, depth, t.as_ref(), size),
        ]),
        _ => {
            let ctor = string_of_utility_ctor(u);
            let ts = types_of_utility(u);
            type_reference(
                opts,
                depth,
                identifier(&Name::new(ctor)),
                ts.as_deref(),
                size,
            )
        }
    }
}

fn type_array<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    arr: &ArrT<L>,
    size: usize,
) -> LayoutNode {
    let arr_name = if arr.arr_readonly {
        "ReadonlyArray"
    } else {
        "Array"
    };
    layout::fuse(vec![
        LayoutNode::atom(arr_name.to_string()),
        LayoutNode::atom("<".to_string()),
        type_impl(opts, depth, &arr.arr_elt_t, size),
        LayoutNode::atom(">".to_string()),
    ])
}

fn type_generic<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    g: &GenericT<L>,
    size: usize,
) -> LayoutNode {
    let (symbol, _, targs) = g;
    let name = identifier(&local_name_of_symbol(symbol));
    type_reference(opts, depth, name, targs.as_deref(), size)
}

fn type_reference<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    name: LayoutNode,
    targs: Option<&[std::sync::Arc<Ty<L>>]>,
    size: usize,
) -> LayoutNode {
    let targs_layout = match targs {
        Some(args) => type_args(opts, depth, args, size),
        None => LayoutNode::empty(),
    };
    layout::fuse(vec![name, targs_layout])
}

fn type_args<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    targs: &[std::sync::Arc<Ty<L>>],
    mut size: usize,
) -> LayoutNode {
    let mut elements = Vec::new();
    for arg in targs.iter() {
        if size == 0 {
            elements.push(crop_atom());
            break;
        }
        size -= 1;
        elements.push(type_impl(opts, depth, arg.as_ref(), size));
    }
    layout::list(
        None, // break_mode
        Some((
            LayoutNode::atom("<".to_string()),
            LayoutNode::atom(">".to_string()),
        )), // wrap
        Some(LayoutNode::atom(",".to_string())), // sep
        true, // trailing
        None, // inline
        None, // indent
        elements,
    )
}

fn type_union<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    types: &[&Ty<L>],
    mut size: usize,
) -> LayoutNode {
    let has_null = types.iter().any(|t| matches!(t, Ty::Null));
    let has_void = types.iter().any(|t| matches!(t, Ty::Void));

    let (prefix, filtered_types): (LayoutNode, Vec<&Ty<L>>) =
        if has_null && has_void && types.len() > 2 {
            let filtered: Vec<&Ty<L>> = types
                .iter()
                .copied()
                .filter(|t| !matches!(t, Ty::Null | Ty::Void))
                .collect();
            let final_types = if filtered.is_empty() {
                vec![&Ty::Bot(BotKind::EmptyType) as &Ty<L>]
            } else {
                filtered
            };
            (LayoutNode::atom("?".to_string()), final_types)
        } else {
            (LayoutNode::empty(), types.to_vec())
        };

    let elements = intersperse_pretty_line(opts, depth, "|", &filtered_types, &mut size);
    layout::group(vec![layout::fuse(
        std::iter::once(prefix).chain(elements).collect(),
    )])
}

fn type_intersection<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    types: &[&Ty<L>],
    mut size: usize,
) -> LayoutNode {
    let elements = intersperse_pretty_line(opts, depth, "&", types, &mut size);
    layout::group(vec![layout::fuse(elements)])
}

fn intersperse_pretty_line<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    sep: &str,
    types: &[&Ty<L>],
    size: &mut usize,
) -> Vec<LayoutNode> {
    let mut elts = Vec::new();
    for (i, t) in types.iter().enumerate() {
        if *size == 0 {
            elts.push(crop_atom());
            break;
        }
        *size -= 1;

        let typ = type_with_parens(opts, depth, t, *size);
        if i == 0 {
            elts.push(typ);
        } else {
            elts.push(layout::fuse(vec![
                LayoutNode::atom(sep.to_string()),
                layout::space(),
                typ,
            ]));
        }
    }

    let len = elts.len();
    elts.into_iter()
        .enumerate()
        .map(|(i, elt)| {
            layout::fuse(vec![
                elt,
                if i == len - 1 {
                    LayoutNode::empty()
                } else {
                    layout::pretty_line()
                },
            ])
        })
        .collect()
}

fn type_with_parens<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    t: &Ty<L>,
    size: usize,
) -> LayoutNode {
    match t {
        Ty::Fun(_) | Ty::Union(_, _, _, _) | Ty::Inter(_, _, _) | Ty::Conditional { .. } => {
            wrap_in_parens(type_impl(opts, depth, t, size))
        }
        _ => type_impl(opts, depth, t, size),
    }
}

fn class_decl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    s: &Symbol<L>,
    type_parameters: &Option<Arc<[TypeParam<L>]>>,
) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom("class".to_string()),
        layout::space(),
        identifier(&local_name_of_symbol(s)),
        option(
            |tparams: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tparams, MAX_SIZE),
            type_parameters,
        ),
    ])
}

fn interface_decl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    s: &Symbol<L>,
    type_parameters: &Option<Arc<[TypeParam<L>]>>,
) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom("interface".to_string()),
        layout::space(),
        identifier(&local_name_of_symbol(s)),
        option(
            |tparams: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tparams, MAX_SIZE),
            type_parameters,
        ),
    ])
}

fn record_decl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    s: &Symbol<L>,
    tparams: &Option<Arc<[TypeParam<L>]>>,
) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom("record".to_string()),
        layout::space(),
        identifier(&local_name_of_symbol(s)),
        option(
            |tp: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tp, MAX_SIZE),
            tparams,
        ),
    ])
}

fn nominal_component_decl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    s: &Symbol<L>,
    type_parameters: &Option<Arc<[TypeParam<L>]>>,
    targs: &Option<Arc<[Arc<Ty<L>>]>>,
    regular_props: &ComponentProps<L>,
    renders: &Option<Ty<L>>,
    is_type: bool,
) -> LayoutNode {
    if is_type {
        // Prefer displaying type arguments if they exist
        let type_args_or_params = match targs {
            Some(ts) => type_args(opts, depth, ts, MAX_SIZE),
            None => option(
                |tparams: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tparams, MAX_SIZE),
                type_parameters,
            ),
        };

        layout::fuse(vec![
            LayoutNode::atom("React$RendersExactly".to_string()),
            layout::list(
                None, // break_mode
                Some((
                    LayoutNode::atom("<".to_string()),
                    LayoutNode::atom(">".to_string()),
                )), // wrap
                Some(LayoutNode::atom(",".to_string())), // sep
                false, // trailing
                None, // inline
                None, // indent
                vec![layout::fuse(vec![
                    LayoutNode::atom("typeof".to_string()),
                    layout::space(),
                    identifier(&local_name_of_symbol(s)),
                    type_args_or_params,
                ])],
            ),
        ])
    } else {
        // component ComponentName<...>(props) renders Type
        // Prefer displaying type arguments if they exist
        let type_args_or_params = match targs {
            Some(ts) => type_args(opts, depth, ts, MAX_SIZE),
            None => option(
                |tparams: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tparams, MAX_SIZE),
                type_parameters,
            ),
        };

        layout::fuse(vec![
            LayoutNode::atom("component".to_string()),
            layout::space(),
            identifier(&local_name_of_symbol(s)),
            type_args_or_params,
            type_component_sig(
                opts,
                depth,
                regular_props,
                renders.as_ref().map(|t| Arc::new(t.clone())).as_ref(),
                MAX_SIZE,
            ),
        ])
    }
}

fn type_alias<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    name: &Symbol<L>,
    tparams: &Option<Arc<[TypeParam<L>]>>,
    t_opt: &Option<Arc<Ty<L>>>,
) -> LayoutNode {
    let name_str = &name.sym_name;
    let tparams_node = option(
        |tp: &Arc<[TypeParam<L>]>| type_parameter(opts, depth, tp, MAX_SIZE),
        tparams,
    );
    let body = match t_opt {
        Some(t) => layout::fuse(vec![
            layout::pretty_space(),
            LayoutNode::atom("=".to_string()),
            layout::pretty_space(),
            type_impl(opts, depth, t, MAX_SIZE),
        ]),
        None => LayoutNode::empty(),
    };

    layout::fuse(vec![
        LayoutNode::atom("type".to_string()),
        layout::space(),
        identifier(name_str),
        tparams_node,
        body,
    ])
}

fn variable_decl<L: Dupe>(
    opts: &PrinterOptions,
    depth: usize,
    name: &Name,
    t: &Ty<L>,
) -> LayoutNode {
    layout::fuse(vec![
        LayoutNode::atom("declare".to_string()),
        layout::space(),
        LayoutNode::atom("var".to_string()),
        layout::space(),
        identifier(name),
        LayoutNode::atom(":".to_string()),
        layout::space(),
        type_impl(opts, depth, t, MAX_SIZE),
    ])
}

fn enum_decl<L: Dupe>(
    s: &Symbol<L>,
    members: &Option<Arc<[FlowSmolStr]>>,
    has_unknown_members: bool,
    truncated_members_count: i64,
) -> LayoutNode {
    let base = layout::fuse(vec![
        LayoutNode::atom("enum".to_string()),
        layout::space(),
        identifier(&local_name_of_symbol(s)),
    ]);
    match members {
        None => base,
        Some(ms) => {
            let mut member_nodes: Vec<_> = ms
                .iter()
                .map(|member| LayoutNode::atom(member.to_string()))
                .collect();
            if truncated_members_count > 0 {
                member_nodes.push(LayoutNode::atom(format!(
                    "/* ... {} more members */",
                    truncated_members_count
                )));
            } else if has_unknown_members {
                member_nodes.push(LayoutNode::atom("...".to_string()));
            }
            layout::fuse(vec![
                base,
                layout::space(),
                layout::list(
                    None,
                    Some((
                        LayoutNode::atom("{".to_string()),
                        LayoutNode::atom("}".to_string()),
                    )),
                    Some(LayoutNode::atom(",".to_string())),
                    false,
                    None,
                    None,
                    member_nodes,
                ),
            ])
        }
    }
}

fn namespace<L: Dupe>(name: &Option<Symbol<L>>) -> LayoutNode {
    let name_node = match name {
        Some(sym) => layout::fuse(vec![
            layout::space(),
            LayoutNode::atom(sym.sym_name.to_string()),
        ]),
        None => LayoutNode::empty(),
    };

    layout::fuse(vec![LayoutNode::atom("namespace".to_string()), name_node])
}

fn module_<L: Dupe>(opts: &PrinterOptions, name: &Option<Symbol<L>>) -> LayoutNode {
    let name_node = match name {
        Some(sym) => {
            let name_str = sym.sym_name.to_string();
            layout::fuse(vec![
                layout::space(),
                layout::fuse(in_quotes(opts.prefer_single_quotes, &name_str)),
            ])
        }
        None => LayoutNode::empty(),
    };

    layout::fuse(vec![LayoutNode::atom("module".to_string()), name_node])
}

fn decl<L: Dupe>(opts: &PrinterOptions, depth: usize, d: &Decl<L>) -> LayoutNode {
    match d {
        Decl::VariableDecl(box (name, t)) => variable_decl(opts, depth, name, t),
        Decl::TypeAliasDecl(box DeclTypeAliasDeclData {
            name,
            tparams,
            type_,
            ..
        }) => type_alias(opts, depth, name, tparams, type_),
        Decl::ClassDecl(box (s, ps)) => class_decl(opts, depth, s, ps),
        Decl::InterfaceDecl(box (s, ps)) => interface_decl(opts, depth, s, ps),
        Decl::RecordDecl(box (s, ps)) => record_decl(opts, depth, s, ps),
        Decl::EnumDecl(box DeclEnumDeclData {
            name,
            members,
            has_unknown_members,
            truncated_members_count,
        }) => enum_decl(
            name,
            members,
            *has_unknown_members,
            *truncated_members_count,
        ),
        Decl::NominalComponentDecl(box DeclNominalComponentDeclData {
            name,
            tparams,
            targs,
            props,
            renders,
            is_type,
        }) => nominal_component_decl(opts, depth, name, tparams, targs, props, renders, *is_type),
        Decl::NamespaceDecl(box DeclNamespaceDeclData { name, .. }) => namespace(name),
        Decl::ModuleDecl(box DeclModuleDeclData { name, .. }) => module_(opts, name),
    }
}

#[derive(Debug, Clone)]
pub struct PrinterOptions {
    pub prefer_single_quotes: bool,
    pub size: usize,
    pub with_comments: bool,
    pub exact_by_default: bool,
    pub ts_syntax: bool,
}

impl Default for PrinterOptions {
    fn default() -> Self {
        PrinterOptions {
            prefer_single_quotes: false,
            size: 5000,
            with_comments: true,
            exact_by_default: true,
            ts_syntax: false,
        }
    }
}

pub fn string_of_elt<L: Dupe>(elt: &Elt<L>, opts: &PrinterOptions) -> String {
    let layout = layout_of_elt(opts, elt);
    pretty_printer::print(true, &layout).contents()
}

pub fn string_of_t<L: Dupe>(t: &Ty<L>, opts: &PrinterOptions) -> String {
    string_of_elt(&Elt::Type(Arc::new(t.clone())), opts)
}

pub fn string_of_elt_single_line<L: Dupe>(elt: &Elt<L>, opts: &PrinterOptions) -> String {
    let layout = layout_of_elt(opts, elt);
    layout::print_single_line(&layout)
}

pub fn string_of_t_single_line<L: Dupe>(t: &Ty<L>, opts: &PrinterOptions) -> String {
    string_of_elt_single_line(&Elt::Type(Arc::new(t.clone())), opts)
}

pub fn string_of_decl_single_line<L: Dupe>(d: &Decl<L>, opts: &PrinterOptions) -> String {
    string_of_elt_single_line(&Elt::Decl(d.clone()), opts)
}

pub fn string_of_symbol_set<L: Clone + Ord>(
    syms: &std::collections::BTreeSet<Symbol<L>>,
) -> Vec<(String, L)> {
    let mut elems: Vec<_> = syms.iter().collect();
    elems.sort_by(|s1, s2| s1.sym_name.as_str().cmp(s2.sym_name.as_str()));
    elems
        .into_iter()
        .map(|sym| {
            let Symbol {
                sym_name,
                sym_def_loc,
                ..
            } = sym;
            (sym_name.as_str().to_string(), sym_def_loc.clone())
        })
        .collect()
}

pub fn string_of_type_at_pos_result<R: Clone + Ord>(
    unevaluated: &Elt<ALoc>,
    evaluated: &Option<Elt<ALoc>>,
    refs: &Option<std::collections::BTreeSet<Symbol<R>>>,
    opts: &PrinterOptions,
) -> (String, Option<Vec<(String, R)>>) {
    let layout = layout_of_type_at_pos_types(opts, unevaluated, evaluated);
    let type_str = pretty_printer::print(true, &layout).contents();
    let refs = refs.as_ref().map(|r| string_of_symbol_set(r));
    (type_str, refs)
}

fn layout_of_type_at_pos_types(
    opts: &PrinterOptions,
    unevaluated: &Elt<ALoc>,
    evaluated: &Option<Elt<ALoc>>,
) -> LayoutNode {
    let layout_unevaluated = layout_of_elt(opts, unevaluated);

    match (unevaluated, evaluated) {
        (_, None) => layout_unevaluated,
        (unevaluated_elt, Some(evaluated_elt))
            if ty_utils::elt_equal(unevaluated_elt, evaluated_elt) =>
        {
            layout_unevaluated
        }
        (_, Some(evaluated_elt)) => {
            // Unwrap TypeAliasDecl to show the underlying type
            let evaluated_to_print = match evaluated_elt {
                Elt::Decl(Decl::TypeAliasDecl(box DeclTypeAliasDeclData {
                    type_: Some(t),
                    ..
                })) => Elt::Type(t.clone()),
                x => x.clone(),
            };

            let layout_evaluated = layout_of_elt(opts, &evaluated_to_print);

            layout::fuse(vec![
                layout_unevaluated,
                layout::hardline(),
                LayoutNode::atom("=".to_string()),
                layout::space(),
                layout_evaluated,
            ])
        }
    }
}
