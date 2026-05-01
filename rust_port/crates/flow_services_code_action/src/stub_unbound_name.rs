/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_analysis::scope_builder;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;

use crate::refactor_extract;
use crate::refactor_extract_utils::type_synthesizer;

type TypeParamSet = BTreeSet<TypeParam>;

fn type_param_set_add_all(list: &[TypeParam], set: &mut TypeParamSet) {
    for t in list {
        set.insert(t.dupe());
    }
}

enum Attribute {
    BoolAttribute,
    StringAttribute,
    EmptyAttribute,
    TypedAttribute(ALoc),
}

fn attribute_of_jsx_value(
    value: &Option<ast::jsx::attribute::Value<ALoc, (ALoc, Type)>>,
) -> Attribute {
    match value {
        None => Attribute::BoolAttribute,
        Some(ast::jsx::attribute::Value::StringLiteral(_)) => Attribute::StringAttribute,
        Some(ast::jsx::attribute::Value::ExpressionContainer((
            _,
            ast::jsx::ExpressionContainer {
                expression: ast::jsx::expression_container::Expression::EmptyExpression,
                ..
            },
        ))) => Attribute::EmptyAttribute,
        Some(ast::jsx::attribute::Value::ExpressionContainer((
            _,
            ast::jsx::ExpressionContainer {
                expression: ast::jsx::expression_container::Expression::Expression(expr),
                ..
            },
        ))) => {
            let (typed_loc, _) = expr.loc();
            Attribute::TypedAttribute(typed_loc.dupe())
        }
    }
}

struct FoundTarget(String, BTreeMap<String, Attribute>);

struct TargetFinder<'a> {
    is_valid_target: &'a dyn Fn(&ALoc) -> bool,
}

impl<'a, 'ast> AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, FoundTarget> for TargetFinder<'a> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
        &type_.0
    }

    fn jsx_element(
        &mut self,
        annot: &'ast (ALoc, Type),
        expr: &'ast ast::jsx::Element<ALoc, (ALoc, Type)>,
    ) -> Result<(), FoundTarget> {
        if let ast::jsx::Name::Identifier(id) = &expr.opening_element.name {
            if expr.opening_element.targs.is_none() && expr.children.1.is_empty() {
                let name = &id.name;
                let (id_aloc, _) = &id.loc;
                let name_str = name.to_string();
                let capitalized = {
                    let mut chars = name_str.chars();
                    match chars.next() {
                        None => String::new(),
                        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
                    }
                };
                if name_str == capitalized && (self.is_valid_target)(id_aloc) {
                    let mut all_explicit_attributes: Option<BTreeMap<String, Attribute>> =
                        Some(BTreeMap::new());
                    for attr in expr.opening_element.attributes.iter() {
                        match attr {
                            ast::jsx::OpeningAttribute::Attribute(a) => match &a.name {
                                ast::jsx::attribute::Name::Identifier(ident) => {
                                    let prop_name = ident.name.to_string();
                                    if let Some(ref mut acc) = all_explicit_attributes {
                                        acc.insert(prop_name, attribute_of_jsx_value(&a.value));
                                    }
                                }
                                ast::jsx::attribute::Name::NamespacedName(_) => {
                                    all_explicit_attributes = None;
                                    break;
                                }
                            },
                            ast::jsx::OpeningAttribute::SpreadAttribute(_) => {
                                all_explicit_attributes = None;
                                break;
                            }
                        }
                    }
                    match all_explicit_attributes {
                        None => {}
                        Some(attributes) => {
                            return Err(FoundTarget(name_str, attributes));
                        }
                    }
                }
            }
        }
        ast_visitor::jsx_element_default(self, annot, expr)
    }
}

pub fn stub<'a, 'b>(
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'a>,
    file: &FileKey,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    loc_of_aloc: &'b dyn Fn(&ALoc) -> Loc,
    get_ast_from_shared_mem: &'b dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    get_haste_module_info: &'b dyn Fn(&FileKey) -> Option<flow_common_modulename::HasteModuleInfo>,
    get_type_sig: &'b dyn Fn(
        &FileKey,
    ) -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    >,
    cursor_loc: Loc,
) -> Option<refactor_extract::Refactor> {
    let scope_info_lazy = std::cell::OnceCell::new();
    let is_valid_target = |id_loc: &ALoc| -> bool {
        let id_loc = loc_of_aloc(id_loc);
        id_loc.contains(&cursor_loc) && {
            let scope_info = scope_info_lazy
                .get_or_init(|| scope_builder::program(cx.enable_enums(), true, ast));
            !scope_info.is_local_use(&id_loc)
        }
    };
    let mut finder = TargetFinder {
        is_valid_target: &is_valid_target,
    };
    let (new_component_name, attributes) = match finder.program(typed_ast) {
        Ok(()) => return None,
        Err(FoundTarget(name, attributes)) => (name, attributes),
    };
    let mut locs = BTreeSet::new();
    for attr in attributes.values() {
        match attr {
            Attribute::BoolAttribute | Attribute::StringAttribute | Attribute::EmptyAttribute => {}
            Attribute::TypedAttribute(loc) => {
                locs.insert(loc_of_aloc(loc));
            }
        }
    }
    let synthesizer_context = type_synthesizer::create_synthesizer_context(
        cx.dupe(),
        file.dupe(),
        file_sig.dupe(),
        typed_ast.clone(),
        loc_of_aloc,
        get_ast_from_shared_mem,
        get_haste_module_info,
        get_type_sig,
        &locs,
    );
    let type_synth =
        type_synthesizer::create_type_synthesizer_with_import_adder(synthesizer_context);
    let mut tparams_set: TypeParamSet = BTreeSet::new();
    let mut typed_props: BTreeMap<String, ast::types::Type<Loc, Loc>> = BTreeMap::new();
    for (key, attr) in &attributes {
        let (tparams, type_) = match attr {
            Attribute::BoolAttribute => (
                vec![],
                ast::types::Type::new(ast::types::TypeInner::Boolean {
                    loc: Loc::none(),
                    raw: ast::types::BooleanRaw::Boolean,
                    comments: None,
                }),
            ),
            Attribute::StringAttribute => (
                vec![],
                ast::types::Type::new(ast::types::TypeInner::String {
                    loc: Loc::none(),
                    comments: None,
                }),
            ),
            Attribute::EmptyAttribute => (vec![], ast_builder::types::mixed(None, None)),
            Attribute::TypedAttribute(loc) => {
                let result = (type_synth.type_synthesizer)(&loc_of_aloc(loc));
                match result {
                    Ok(Some((tparams_rev, type_))) => (tparams_rev, type_),
                    Ok(None) | Err(_) => (
                        vec![],
                        ast::types::Type::new(ast::types::TypeInner::Any {
                            loc: Loc::none(),
                            comments: None,
                        }),
                    ),
                }
            }
        };
        type_param_set_add_all(&tparams, &mut tparams_set);
        typed_props.insert(key.clone(), type_);
    }
    let tparams_list: Vec<TypeParam> = tparams_set.into_iter().collect();
    let mut acc = Vec::new();
    let mut tparams_failed = false;
    for tparam in &tparams_list {
        match (type_synth.type_param_synthesizer)(tparam) {
            Ok(tp) => acc.push(tp),
            Err(_) => {
                tparams_failed = true;
                break;
            }
        }
    }
    let tparams = if tparams_failed {
        return None;
    } else if acc.is_empty() {
        None
    } else {
        Some(ast_builder::types::type_params(None, None, acc))
    };
    let typed_props: Vec<(&str, ast::types::Type<Loc, Loc>)> = typed_props
        .iter()
        .rev()
        .map(|(k, v)| (k.as_str(), v.dupe()))
        .collect();
    let added_imports = (type_synth.added_imports)();
    let component_declaration_statement =
        ast_builder::statements::synthesized_component_declaration(
            cx.component_syntax(),
            tparams,
            typed_props,
            vec![ast_builder::statements::return_(None, None, None)],
            &new_component_name,
        );
    let new_statements: Arc<[_]> = ast
        .statements
        .iter()
        .flat_map(|s| {
            let loc = s.loc();
            if loc.contains(&cursor_loc) {
                vec![component_declaration_statement.dupe(), s.dupe()]
            } else {
                vec![s.dupe()]
            }
        })
        .collect();
    let new_ast = ast::Program {
        statements: new_statements,
        ..ast.clone()
    };
    Some(refactor_extract::Refactor {
        title: format!(
            "Add missing React component declaration `{}`",
            new_component_name
        ),
        new_ast,
        added_imports,
    })
}
