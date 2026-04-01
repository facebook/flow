/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::statement;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::flow_ast_differ;
use flow_typing_context::Context;

use crate::ast_extraction_utils::ast_extractor;
use crate::ast_extraction_utils::insertion_point_collectors;
use crate::autofix_imports;
use crate::refactor_extract_utils::information_collectors;
use crate::refactor_extract_utils::refactor_program_mappers;
use crate::refactor_extract_utils::type_synthesizer;
use crate::refactor_extract_utils::variable_analysis;

fn union_loc(acc: Option<Loc>, loc: Loc) -> Option<Loc> {
    match acc {
        None => Some(loc),
        Some(existing_loc) => Some(Loc::between(&existing_loc, &loc)),
    }
}

fn create_unique_name(
    support_experimental_snippet_text_edit: bool,
    used_names: &BTreeSet<FlowSmolStr>,
    prefix: &str,
) -> String {
    let unique_name = if used_names.contains(prefix) {
        fn number_suffixed_name(
            prefix: &str,
            used_names: &BTreeSet<FlowSmolStr>,
            i: usize,
        ) -> String {
            let name = format!("{prefix}{i}");
            if used_names.contains(name.as_str()) {
                number_suffixed_name(prefix, used_names, i + 1)
            } else {
                name
            }
        }
        number_suffixed_name(prefix, used_names, 1)
    } else {
        prefix.to_string()
    };
    if support_experimental_snippet_text_edit {
        format!("${{0:{unique_name}}}")
    } else {
        unique_name
    }
}

type TypeParamSet = BTreeSet<flow_typing_type::type_::TypeParam>;

fn type_param_set_add_all(list: &[flow_typing_type::type_::TypeParam], set: &mut TypeParamSet) {
    for t in list {
        set.insert(t.dupe());
    }
}

fn create_extracted_function(
    type_param_synthesizer: &dyn Fn(
        &flow_typing_type::type_::TypeParam,
    ) -> Result<
        ast::types::TypeParam<Loc, Loc>,
        crate::insert_type::Expected,
    >,
    type_synthesizer: &dyn Fn(
        &Loc,
    ) -> Result<
        Option<(
            Vec<flow_typing_type::type_::TypeParam>,
            ast::types::Type<Loc, Loc>,
        )>,
        crate::insert_type::Expected,
    >,
    target_tparams_rev: &[flow_typing_type::type_::TypeParam],
    undefined_variables: &[(FlowSmolStr, Loc)],
    escaping_definitions: &variable_analysis::EscapingDefinitions,
    vars_with_shadowed_local_reassignments: &[(FlowSmolStr, Loc)],
    async_function: bool,
    name: &str,
    extracted_statements: &[statement::Statement<Loc, Loc>],
) -> Result<ast::function::Function<Loc, Loc>, crate::insert_type::Expected> {
    let id = Some(ast_builder::identifiers::identifier(None, name));
    let mut params_list = Vec::new();
    let mut used_tparam_set: TypeParamSet = BTreeSet::new();
    for (v, loc) in undefined_variables.iter() {
        let annot = match type_synthesizer(loc) {
            Err(_) | Ok(None) => ast::types::AnnotationOrHint::Missing(Loc::none()),
            Ok(Some((tparams_rev, type_))) => {
                type_param_set_add_all(&tparams_rev, &mut used_tparam_set);
                ast::types::AnnotationOrHint::Available(ast::types::Annotation {
                    loc: Loc::none(),
                    annotation: type_,
                })
            }
        };
        let param = ast_builder::functions::param(
            None,
            None,
            ast_builder::patterns::identifier(None, Some(annot), v),
        );
        params_list.push(param);
    }
    let params = ast_builder::functions::params(None, None, None, None, params_list);
    let mut all_returned: Vec<(FlowSmolStr, Loc)> = Vec::new();
    all_returned.extend(escaping_definitions.escaping_variables.iter().cloned());
    all_returned.extend(vars_with_shadowed_local_reassignments.iter().cloned());
    let mut returned_variables: Vec<(FlowSmolStr, ast::types::Type<Loc, Loc>)> = Vec::new();
    for (v, loc) in all_returned.iter() {
        let type_ = match type_synthesizer(loc) {
            Err(_) | Ok(None) => ast::types::Type::new(ast::types::TypeInner::Any {
                loc: Loc::none(),
                comments: None,
            }),
            Ok(Some((tparams_rev, type_))) => {
                type_param_set_add_all(&tparams_rev, &mut used_tparam_set);
                type_
            }
        };
        returned_variables.push((v.clone(), type_));
    }
    let unbound_tparams: Vec<_> = {
        let target_set: TypeParamSet = target_tparams_rev.iter().duped().collect();
        used_tparam_set.difference(&target_set).duped().collect()
    };
    let synthesized: Vec<ast::types::TypeParam<Loc, Loc>> = unbound_tparams
        .iter()
        .map(type_param_synthesizer)
        .collect::<Result<Vec<_>, _>>()?;
    let tparams: Option<ast::types::TypeParams<Loc, Loc>> = if synthesized.is_empty() {
        None
    } else {
        Some(ast_builder::types::type_params(None, None, synthesized))
    };
    let (body_statements, return_type): (
        Vec<statement::Statement<Loc, Loc>>,
        ast::types::Type<Loc, Loc>,
    ) = match returned_variables.as_slice() {
        [] => (
            extracted_statements.to_vec(),
            ast_builder::types::void(None, None),
        ),
        [(only_returned_variable, return_type)] => (
            {
                let mut stmts = extracted_statements.to_vec();
                stmts.push(ast_builder::statements::return_(
                    None,
                    None,
                    Some(ast_builder::expressions::identifier(
                        None,
                        None,
                        only_returned_variable,
                    )),
                ));
                stmts
            },
            return_type.dupe(),
        ),
        _ => {
            let mut stmts = extracted_statements.to_vec();
            let obj_props: Vec<expression::object::Property<Loc, Loc>> = returned_variables
                .iter()
                .map(|(def, _)| {
                    ast_builder::expressions::object_property(
                        Some(true), // shorthand
                        None,
                        ast_builder::expressions::object_property_key(None, def),
                        ast_builder::expressions::identifier(None, None, def),
                    )
                })
                .collect();
            stmts.push(ast_builder::statements::return_(
                None,
                None,
                Some(ast_builder::expressions::object_(None, None, obj_props)),
            ));
            let type_obj_props: Vec<ast::types::object::Property<Loc, Loc>> = returned_variables
                .iter()
                .map(|(v, type_)| {
                    ast::types::object::Property::NormalProperty(
                        ast_builder::types::objects::property(
                            None,
                            None,
                            None,
                            None,
                            None,
                            None,
                            None,
                            None,
                            None,
                            ast_builder::expressions::object_property_key(None, v),
                            ast::types::object::PropertyValue::Init(Some(type_.dupe())),
                            None,
                        ),
                    )
                })
                .collect();
            let return_type = ast_builder::types::object_(None, None, None, type_obj_props);
            (stmts, return_type)
        }
    };
    let return_annot = if async_function {
        ast::function::ReturnAnnot::Available(ast_builder::types::annotation(
            ast_builder::types::unqualified_generic(
                None,
                None,
                Some(ast_builder::types::type_args(None, None, vec![return_type])),
                "Promise",
            ),
        ))
    } else {
        ast::function::ReturnAnnot::Available(ast_builder::types::annotation(return_type))
    };
    let body = ast_builder::functions::body(None, None, body_statements);
    Ok(ast_builder::functions::make(
        id,
        Some(params),
        tparams,
        Some(return_annot),
        None,
        None,
        Some(async_function),
        Some(body),
    ))
}

fn create_extracted_function_call(
    undefined_variables: &[(FlowSmolStr, Loc)],
    escaping_definitions: &variable_analysis::EscapingDefinitions,
    vars_with_shadowed_local_reassignments: &[(FlowSmolStr, Loc)],
    async_function: bool,
    is_method: bool,
    new_function_name: &str,
    extracted_statements_loc: &Loc,
) -> Vec<statement::Statement<Loc, Loc>> {
    let caller = if is_method {
        ast_builder::expressions::member(
            None,
            ast_builder::expressions::members::identifier_by_name(
                None,
                new_function_name,
                ast_builder::expressions::this(None, None),
            ),
        )
    } else {
        ast_builder::expressions::identifier(None, None, new_function_name)
    };
    let args: Vec<expression::ExpressionOrSpread<Loc, Loc>> = undefined_variables
        .iter()
        .map(|(v, _)| {
            ast_builder::expressions::expression_or_spread(ast_builder::expressions::identifier(
                None, None, v,
            ))
        })
        .collect();
    let call = ast_builder::expressions::call(
        Some(extracted_statements_loc.dupe()),
        Some(ast_builder::expressions::arg_list(None, None, args)),
        caller,
    );
    let call = if async_function {
        ast_builder::expressions::unary(None, None, expression::UnaryOperator::Await, call)
    } else {
        call
    };
    let has_vars_with_shadowed_local_reassignments =
        !vars_with_shadowed_local_reassignments.is_empty();
    let let_declarations: Vec<statement::Statement<Loc, Loc>> =
        if has_vars_with_shadowed_local_reassignments {
            escaping_definitions
                .escaping_variables
                .iter()
                .map(|(v, _)| {
                    ast_builder::statements::let_declaration(
                        None,
                        vec![ast_builder::statements::variable_declarator(
                            None, None, None, v,
                        )],
                    )
                })
                .collect()
        } else {
            vec![]
        };
    let mut returned_variables: Vec<(FlowSmolStr, Loc)> = Vec::new();
    returned_variables.extend(escaping_definitions.escaping_variables.iter().cloned());
    returned_variables.extend(vars_with_shadowed_local_reassignments.iter().cloned());
    let function_call_statement_with_collector = match returned_variables.as_slice() {
        [] => ast_builder::statements::expression(
            Some(extracted_statements_loc.dupe()),
            None,
            None,
            call,
        ),
        [(only_returned_variable, _)] => {
            if has_vars_with_shadowed_local_reassignments {
                ast_builder::statements::expression(
                    Some(extracted_statements_loc.dupe()),
                    None,
                    None,
                    ast_builder::expressions::assignment(
                        None,
                        None,
                        ast_builder::patterns::identifier(None, None, only_returned_variable),
                        None,
                        call,
                    ),
                )
            } else {
                let declarations = vec![ast_builder::statements::variable_declarator(
                    None,
                    Some(call),
                    None,
                    only_returned_variable,
                )];
                if escaping_definitions.has_external_writes {
                    ast_builder::statements::let_declaration(
                        Some(extracted_statements_loc.dupe()),
                        declarations,
                    )
                } else {
                    ast_builder::statements::const_declaration(
                        Some(extracted_statements_loc.dupe()),
                        None,
                        declarations,
                    )
                }
            }
        }
        _ => {
            let properties: Vec<ast::pattern::object::Property<Loc, Loc>> = returned_variables
                .iter()
                .map(|(def, _)| {
                    ast::pattern::object::Property::NormalProperty(
                        ast::pattern::object::NormalProperty {
                            loc: Loc::none(),
                            key: ast::pattern::object::Key::Identifier(
                                ast_builder::identifiers::identifier(None, def),
                            ),
                            pattern: ast_builder::patterns::identifier(None, None, def),
                            default: None,
                            shorthand: true,
                        },
                    )
                })
                .collect();
            let pattern = ast::pattern::Pattern::Object {
                loc: Loc::none(),
                inner: Arc::new(ast::pattern::Object {
                    properties: properties.into(),
                    annot: ast::types::AnnotationOrHint::Missing(Loc::none()),
                    optional: false,
                    comments: None,
                }),
            };
            if has_vars_with_shadowed_local_reassignments {
                ast_builder::statements::expression(
                    Some(extracted_statements_loc.dupe()),
                    None,
                    None,
                    ast_builder::expressions::assignment(None, None, pattern, None, call),
                )
            } else {
                let declarations = vec![ast_builder::statements::variable_declarator_generic(
                    None,
                    pattern,
                    Some(call),
                )];
                if escaping_definitions.has_external_writes {
                    ast_builder::statements::let_declaration(
                        Some(extracted_statements_loc.dupe()),
                        declarations,
                    )
                } else {
                    ast_builder::statements::const_declaration(
                        Some(extracted_statements_loc.dupe()),
                        None,
                        declarations,
                    )
                }
            }
        }
    };
    let mut result = let_declarations;
    result.push(function_call_statement_with_collector);
    result
}

fn create_refactor_for_statements<'a, 'b>(
    scope_info: &flow_analysis::scope_api::ScopeInfo<Loc>,
    defs_with_scopes_of_local_uses: &[(
        flow_analysis::scope_api::Def<Loc>,
        flow_analysis::scope_api::Scope<Loc>,
    )],
    escaping_definitions: &variable_analysis::EscapingDefinitions,
    vars_with_shadowed_local_reassignments: &[(FlowSmolStr, Loc)],
    type_synthesizer_context: type_synthesizer::SynthesizerContext<'b, 'a>,
    async_function: bool,
    is_method: bool,
    new_function_name: &str,
    ast: &ast::Program<Loc, Loc>,
    extracted_statements: &[statement::Statement<Loc, Loc>],
    extracted_statements_loc: &Loc,
    target_body_loc: &Loc,
    target_tparams_rev: &[flow_typing_type::type_::TypeParam],
) -> Result<
    (
        ast::Program<Loc, Loc>,
        Vec<(String, autofix_imports::Bindings)>,
    ),
    crate::insert_type::Expected,
> {
    // Do not pass in target loc if the target is toplevel.
    let new_function_target_scope_loc = if *target_body_loc == ast.loc {
        None
    } else {
        Some(target_body_loc.dupe())
    };
    let undefined_variables = variable_analysis::undefined_variables_after_extraction(
        scope_info,
        defs_with_scopes_of_local_uses,
        new_function_target_scope_loc.as_ref(),
        extracted_statements_loc,
    );
    let type_synth =
        type_synthesizer::create_type_synthesizer_with_import_adder(type_synthesizer_context);
    let function_call_statements = create_extracted_function_call(
        &undefined_variables,
        escaping_definitions,
        vars_with_shadowed_local_reassignments,
        async_function,
        is_method,
        new_function_name,
        extracted_statements_loc,
    );
    let new_ast = if is_method {
        let extracted_function = create_extracted_function(
            &*type_synth.type_param_synthesizer,
            &*type_synth.type_synthesizer,
            target_tparams_rev,
            &undefined_variables,
            escaping_definitions,
            vars_with_shadowed_local_reassignments,
            async_function,
            new_function_name,
            extracted_statements,
        )?;
        let method_declaration =
            ast_builder::classes::method_(None, None, None, new_function_name, extracted_function);
        refactor_program_mappers::extract_statements_to_method(
            target_body_loc,
            extracted_statements_loc,
            &function_call_statements,
            &method_declaration,
            ast,
        )
    } else {
        let extracted_function = create_extracted_function(
            &*type_synth.type_param_synthesizer,
            &*type_synth.type_synthesizer,
            target_tparams_rev,
            &undefined_variables,
            escaping_definitions,
            vars_with_shadowed_local_reassignments,
            async_function,
            new_function_name,
            extracted_statements,
        )?;
        let function_declaration_statement =
            statement::Statement::new(statement::StatementInner::FunctionDeclaration {
                loc: Loc::none(),
                inner: Arc::new(extracted_function),
            });
        refactor_program_mappers::extract_statements_to_function(
            target_body_loc,
            extracted_statements_loc,
            &function_call_statements,
            &function_declaration_statement,
            ast,
        )
    };
    Ok((new_ast, (type_synth.added_imports)()))
}

pub struct Refactor {
    pub title: String,
    pub new_ast: ast::Program<Loc, Loc>,
    pub added_imports: Vec<(String, autofix_imports::Bindings)>,
}

fn available_refactors_for_statements<'a, 'b>(
    scope_info: &flow_analysis::scope_api::ScopeInfo<Loc>,
    defs_with_scopes_of_local_uses: &[(
        flow_analysis::scope_api::Def<Loc>,
        flow_analysis::scope_api::Scope<Loc>,
    )],
    escaping_definitions: &variable_analysis::EscapingDefinitions,
    vars_with_shadowed_local_reassignments: &[(FlowSmolStr, Loc)],
    type_synthesizer_context: type_synthesizer::SynthesizerContext<'b, 'a>,
    async_function: bool,
    has_this_super: bool,
    typed_ast: &ast::Program<ALoc, (ALoc, flow_typing_type::type_::Type)>,
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    create_unique_name_fn: &dyn Fn(&str) -> String,
    ast: &ast::Program<Loc, Loc>,
    extracted_statements: &[statement::Statement<Loc, Loc>],
    extracted_statements_loc: &Loc,
) -> Vec<Refactor> {
    let create_refactor = |is_method: bool,
                           new_function_name: &str,
                           target_body_loc: &Loc,
                           target_tparams_rev: &[flow_typing_type::type_::TypeParam]|
     -> Result<
        (
            ast::Program<Loc, Loc>,
            Vec<(String, autofix_imports::Bindings)>,
        ),
        crate::insert_type::Expected,
    > {
        create_refactor_for_statements(
            scope_info,
            defs_with_scopes_of_local_uses,
            escaping_definitions,
            vars_with_shadowed_local_reassignments,
            type_synthesizer_context.clone(),
            async_function,
            is_method,
            new_function_name,
            ast,
            extracted_statements,
            extracted_statements_loc,
            target_body_loc,
            target_tparams_rev,
        )
    };

    let extract_to_method_refactors: Vec<Refactor> =
        match insertion_point_collectors::find_closest_enclosing_class(
            typed_ast,
            loc_of_aloc,
            extracted_statements_loc.dupe(),
        ) {
            None => vec![],
            Some(cip) => {
                match create_refactor(
                    true,                                // ~is_method:true
                    &create_unique_name_fn("newMethod"), // ~new_function_name:(create_unique_name "newMethod")
                    &cip.body_loc,                       // ~target_body_loc
                    &cip.tparams_rev,                    // ~target_tparams_rev:tparams_rev
                ) {
                    Ok((new_ast, added_imports)) => {
                        let title = match &cip.class_name {
                            None => "Extract to method in anonymous class declaration".to_string(),
                            Some(name) => format!("Extract to method in class '{name}'"),
                        };
                        vec![Refactor {
                            title,
                            new_ast,
                            added_imports,
                        }]
                    }
                    Err(_) => vec![],
                }
            }
        };

    if has_this_super {
        return extract_to_method_refactors;
    }
    let new_function_name = create_unique_name_fn("newFunction");

    let create_inner_function_refactor =
        |point: &insertion_point_collectors::FunctionInsertionPoint|
         -> Result<Refactor, crate::insert_type::Expected> {
            let title = format!(
                "Extract to inner function in {} '{}'",
                if point.is_method { "method" } else { "function" },
                point.function_name
            );
            let (new_ast, added_imports) = create_refactor(
                false, // ~is_method:false
                &new_function_name, // ~new_function_name
                &point.body_loc, // ~target_body_loc
                &point.tparams_rev, // ~target_tparams_rev:tparams_rev
            )?;
            Ok(Refactor {
                title,
                new_ast,
                added_imports,
            })
        };

    let top_level_function_refactor: Result<Refactor, _> = create_refactor(
        false,              // ~is_method:false
        &new_function_name, // ~new_function_name
        &ast.loc,           // ~target_body_loc:(fst ast)
        &[],                // ~target_tparams_rev:[]
    )
    .map(|(new_ast, added_imports)| Refactor {
        title: "Extract to function in module scope".to_string(),
        new_ast,
        added_imports,
    });

    let inner_points = insertion_point_collectors::collect_function_method_inserting_points(
        typed_ast,
        loc_of_aloc,
        extracted_statements_loc.dupe(),
    );
    let mut extract_to_functions_refactor_results: Vec<Result<Refactor, _>> =
        vec![top_level_function_refactor];
    extract_to_functions_refactor_results
        .extend(inner_points.iter().map(create_inner_function_refactor));

    let extract_to_functions_refactors: Vec<Refactor> = extract_to_functions_refactor_results
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    let mut result = extract_to_method_refactors;
    result.extend(extract_to_functions_refactors);
    result
}

fn extract_from_statements_refactors<'a, 'b>(
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'a>,
    file: &FileKey,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, flow_typing_type::type_::Type)>,
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
    create_unique_name_fn: &dyn Fn(&str) -> String,
    extracted_statements: &[statement::Statement<Loc, Loc>],
) -> Vec<Refactor> {
    let info = information_collectors::collect_statements_information(extracted_statements);
    if info.has_unwrapped_control_flow {
        return vec![];
    }
    let extracted_statements_locations: Vec<Loc> = extracted_statements
        .iter()
        .map(flow_ast_differ::expand_statement_comment_bounds)
        .collect();
    match extracted_statements_locations.as_slice() {
        [] => vec![],
        [insert_new_function_call_loc, rest_statements_locations @ ..] => {
            let rest_statements_loc_union = rest_statements_locations
                .iter()
                .fold(None, |acc, loc| union_loc(acc, loc.dupe()));
            let extracted_statements_loc = match rest_statements_loc_union {
                None => insert_new_function_call_loc.dupe(),
                Some(loc) => Loc::between(insert_new_function_call_loc, &loc),
            };
            let scope_info = flow_analysis::scope_builder::program(cx.enable_enums(), true, ast);
            let (_ssa_abnormal, (ssa_values, _possible_globals)) =
                flow_analysis::ssa_builder::program_with_scope(cx.enable_enums(), ast);
            let relevant_defs = variable_analysis::collect_relevant_defs_with_scope(
                &scope_info,
                &ssa_values,
                &extracted_statements_loc,
            );
            let escaping_definitions = variable_analysis::collect_escaping_local_defs(
                &scope_info,
                &ssa_values,
                &extracted_statements_loc,
            );
            let locs: BTreeSet<Loc> = relevant_defs
                .defs_with_scopes_of_local_uses
                .iter()
                .map(|(def, _)| def.locs.first().dupe())
                .collect();
            let mut locs = locs;
            for (_, loc) in relevant_defs
                .vars_with_shadowed_local_reassignments
                .iter()
                .chain(escaping_definitions.escaping_variables.iter())
            {
                locs.insert(loc.dupe());
            }
            let type_synthesizer_context = type_synthesizer::create_synthesizer_context(
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
            available_refactors_for_statements(
                &scope_info,
                &relevant_defs.defs_with_scopes_of_local_uses,
                &escaping_definitions,
                &relevant_defs.vars_with_shadowed_local_reassignments,
                type_synthesizer_context,
                info.async_function,
                info.has_this_super,
                typed_ast,
                loc_of_aloc,
                create_unique_name_fn,
                ast,
                extracted_statements,
                &extracted_statements_loc,
            )
        }
    }
}

fn create_extract_expression_to_class_field_refactors(
    scope_info: &flow_analysis::scope_api::ScopeInfo<Loc>,
    defs_with_scopes_of_local_uses: &[(
        flow_analysis::scope_api::Def<Loc>,
        flow_analysis::scope_api::Scope<Loc>,
    )],
    extracted_expression_loc: &Loc,
    expression: &ast::expression::Expression<Loc, Loc>,
    new_property_name: &str,
    ast: &ast::Program<Loc, Loc>,
    class_name: Option<&str>,
    body_loc: &Loc,
) -> Vec<Refactor> {
    let undefined_variables = variable_analysis::undefined_variables_after_extraction(
        scope_info,
        defs_with_scopes_of_local_uses,
        Some(body_loc),
        extracted_expression_loc,
    );
    if !undefined_variables.is_empty() {
        return vec![];
    }
    let title = match class_name {
        None => "Extract to field in anonymous class declaration".to_string(),
        Some(name) => format!("Extract to field in class '{name}'"),
    };
    let expression_replacement = ast_builder::expressions::member(
        None,
        ast_builder::expressions::members::identifier_by_name(
            None,
            new_property_name,
            ast_builder::expressions::this(None, None),
        ),
    );
    let field_definition = ast_builder::classes::property(
        None,
        None,
        None,
        None,
        None,
        new_property_name,
        expression.dupe(),
    );
    let new_ast = refactor_program_mappers::extract_expression_to_class_field(
        body_loc,
        extracted_expression_loc,
        &expression_replacement,
        &field_definition,
        ast,
    );
    vec![Refactor {
        title,
        new_ast,
        added_imports: vec![],
    }]
}

fn create_expression_extract_to_constant_refactor(
    scope_info: &flow_analysis::scope_api::ScopeInfo<Loc>,
    defs_with_scopes_of_local_uses: &[(
        flow_analysis::scope_api::Def<Loc>,
        flow_analysis::scope_api::Scope<Loc>,
    )],
    extracted_expression_loc: &Loc,
    expression: &ast::expression::Expression<Loc, Loc>,
    new_local_name: &str,
    ast: &ast::Program<Loc, Loc>,
    title: &str,
    function_body_loc: Option<&Loc>,
    statement_loc: &Loc,
) -> Option<Refactor> {
    let undefined_variables = variable_analysis::undefined_variables_after_extraction(
        scope_info,
        defs_with_scopes_of_local_uses,
        function_body_loc,
        extracted_expression_loc,
    );
    if !undefined_variables.is_empty() {
        return None;
    }
    let expression_replacement = ast_builder::expressions::identifier(None, None, new_local_name);
    let constant_definition = ast_builder::statements::const_declaration(
        None,
        None,
        vec![ast_builder::statements::variable_declarator(
            None,
            Some(expression.dupe()),
            None,
            new_local_name,
        )],
    );
    let new_ast = refactor_program_mappers::extract_expression_to_constant(
        statement_loc,
        extracted_expression_loc,
        &expression_replacement,
        &constant_definition,
        ast,
    );
    Some(Refactor {
        title: title.to_string(),
        new_ast,
        added_imports: vec![],
    })
}

fn create_expression_extract_to_react_component_refactor<'a, 'b>(
    use_component_syntax: bool,
    scope_info: &flow_analysis::scope_api::ScopeInfo<Loc>,
    defs_with_scopes_of_local_uses: &[(
        flow_analysis::scope_api::Def<Loc>,
        flow_analysis::scope_api::Scope<Loc>,
    )],
    type_synthesizer_context: type_synthesizer::SynthesizerContext<'b, 'a>,
    extracted_expression_loc: &Loc,
    expression: &ast::expression::Expression<Loc, Loc>,
    new_component_name: &str,
    ast: &ast::Program<Loc, Loc>,
) -> Option<Refactor> {
    let undefined_variables = variable_analysis::undefined_variables_after_extraction(
        scope_info,
        defs_with_scopes_of_local_uses,
        None,
        extracted_expression_loc,
    );
    let type_synth =
        type_synthesizer::create_type_synthesizer_with_import_adder(type_synthesizer_context);
    let mut typed_props: Vec<(FlowSmolStr, ast::types::Type<Loc, Loc>)> = Vec::new();
    let mut tparams_set: TypeParamSet = BTreeSet::new();
    for (def, loc) in undefined_variables.iter() {
        match (type_synth.type_synthesizer)(loc) {
            Ok(Some((tparams_rev, type_))) => {
                typed_props.push((def.clone(), type_));
                type_param_set_add_all(&tparams_rev, &mut tparams_set);
            }
            _ => return None,
        }
    }
    let tparams: Option<ast::types::TypeParams<Loc, Loc>> = {
        let tparams_list: Vec<_> = tparams_set.iter().collect();
        if tparams_list.is_empty() {
            None
        } else {
            let mut synthesized = Vec::new();
            for tp in &tparams_list {
                match (type_synth.type_param_synthesizer)(tp) {
                    Ok(synth_tp) => synthesized.push(synth_tp),
                    Err(_) => return None,
                }
            }
            Some(ast_builder::types::type_params(None, None, synthesized))
        }
    };
    let added_imports = (type_synth.added_imports)();
    let typed_props_refs: Vec<(&str, ast::types::Type<Loc, Loc>)> = typed_props
        .iter()
        .map(|(name, t)| (name.as_str(), t.dupe()))
        .collect();
    let component_declaration_statement =
        ast_builder::statements::synthesized_component_declaration(
            use_component_syntax,
            tparams,
            typed_props_refs,
            vec![ast_builder::statements::return_(
                None,
                None,
                Some(expression.dupe()),
            )],
            new_component_name,
        );
    use dupe::Dupe;
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;
    let attrs: Vec<ast::jsx::OpeningAttribute<Loc, Loc>> = typed_props
        .iter()
        .map(|(name, _)| {
            let container = ast::jsx::attribute::Value::ExpressionContainer((
                Loc::none(),
                ast::jsx::ExpressionContainer {
                    expression: ast::jsx::expression_container::Expression::Expression(
                        ast_builder::expressions::identifier(None, None, name),
                    ),
                    comments: None,
                },
            ));
            ast_builder::jsxs::attr(
                None,
                ast_builder::jsxs::attr_identifier(None, None, FlowSmolStr::from(name.as_str())),
                Some(container),
            )
        })
        .collect();
    let expression_replacement =
        expression::Expression::new(expression::ExpressionInner::JSXElement {
            loc: Loc::none(),
            inner: Arc::new(ast_builder::jsxs::element(
                Some(true),
                None,
                Some(attrs),
                None,
                None,
                ast_builder::jsxs::identifier(None, FlowSmolStr::from(new_component_name)),
            )),
        });
    let new_ast = refactor_program_mappers::extract_expression_to_react_component(
        extracted_expression_loc,
        &expression_replacement,
        &component_declaration_statement,
        ast,
    );
    Some(Refactor {
        title: "Extract to react component".to_string(),
        new_ast,
        added_imports,
    })
}

fn extract_from_expression_refactors<'a, 'b>(
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'a>,
    file: &FileKey,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, flow_typing_type::type_::Type)>,
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
    create_unique_name_fn: &dyn Fn(&str) -> String,
    constant_insertion_points: &[ast_extractor::ConstantInsertionPoint],
    expression: &ast::expression::Expression<Loc, Loc>,
) -> Vec<Refactor> {
    let info = information_collectors::collect_expression_information(expression);
    let extracted_expression_loc = expression.loc().dupe();
    let scope_info = flow_analysis::scope_builder::program(cx.enable_enums(), true, ast);
    let (_ssa_abnormal, (ssa_values, _possible_globals)) =
        flow_analysis::ssa_builder::program_with_scope(cx.enable_enums(), ast);
    let relevant_defs = variable_analysis::collect_relevant_defs_with_scope(
        &scope_info,
        &ssa_values,
        &extracted_expression_loc,
    );
    let class_insertion_point = insertion_point_collectors::find_closest_enclosing_class(
        typed_ast,
        loc_of_aloc,
        extracted_expression_loc.dupe(),
    );
    let extract_to_react_component_refactors: Vec<Refactor> =
        match (info.has_this_super, &**expression) {
            (false, expression::ExpressionInner::JSXElement { .. })
            | (false, expression::ExpressionInner::JSXFragment { .. }) => {
                let locs: BTreeSet<Loc> = relevant_defs
                    .defs_with_scopes_of_local_uses
                    .iter()
                    .map(|(def, _)| def.locs.first().dupe())
                    .collect();
                let type_synthesizer_context = type_synthesizer::create_synthesizer_context(
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
                let new_component_name = create_unique_name_fn("NewComponent");
                create_expression_extract_to_react_component_refactor(
                    cx.component_syntax(),
                    &scope_info,
                    &relevant_defs.defs_with_scopes_of_local_uses,
                    type_synthesizer_context,
                    &extracted_expression_loc,
                    expression,
                    &new_component_name,
                    ast,
                )
                .into_iter()
                .collect()
            }
            _ => Vec::new(),
        };
    let new_local_name = create_unique_name_fn("newLocal");
    match class_insertion_point {
        None => {
            if info.has_this_super {
                vec![]
            } else {
                let mut result = extract_to_react_component_refactors;
                for cip in constant_insertion_points {
                    if let Some(refactor) = create_expression_extract_to_constant_refactor(
                        &scope_info,
                        &relevant_defs.defs_with_scopes_of_local_uses,
                        &extracted_expression_loc,
                        expression,
                        &new_local_name,
                        ast,
                        &cip.title,
                        cip.function_body_loc.as_ref(),
                        &cip.statement_loc,
                    ) {
                        result.push(refactor);
                    }
                }
                result
            }
        }
        Some(ref cip) => {
            let class_body_loc = &cip.body_loc;
            let new_property_name = create_unique_name_fn("newProperty");
            let extract_to_class_field_refactors =
                create_extract_expression_to_class_field_refactors(
                    &scope_info,
                    &relevant_defs.defs_with_scopes_of_local_uses,
                    &extracted_expression_loc,
                    expression,
                    &new_property_name,
                    ast,
                    cip.class_name.as_deref(),
                    class_body_loc,
                );
            if info.has_this_super {
                let mut result = extract_to_class_field_refactors;
                for c in constant_insertion_points {
                    // As long as the statement is still inside the class, we allow extraction to constant,
                    // since `this`/`super` is still bound.
                    if Loc::contains(class_body_loc, &c.statement_loc) {
                        if let Some(refactor) = create_expression_extract_to_constant_refactor(
                            &scope_info,
                            &relevant_defs.defs_with_scopes_of_local_uses,
                            &extracted_expression_loc,
                            expression,
                            &new_local_name,
                            ast,
                            &c.title,
                            c.function_body_loc.as_ref(),
                            &c.statement_loc,
                        ) {
                            result.push(refactor);
                        }
                    }
                }
                result
            } else {
                let mut result = extract_to_react_component_refactors;
                result.extend(extract_to_class_field_refactors);
                for c in constant_insertion_points {
                    if let Some(refactor) = create_expression_extract_to_constant_refactor(
                        &scope_info,
                        &relevant_defs.defs_with_scopes_of_local_uses,
                        &extracted_expression_loc,
                        expression,
                        &new_local_name,
                        ast,
                        &c.title,
                        c.function_body_loc.as_ref(),
                        &c.statement_loc,
                    ) {
                        result.push(refactor);
                    }
                }
                result
            }
        }
    }
}

fn extract_from_type_refactors<'a, 'b>(
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'a>,
    file: &FileKey,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, flow_typing_type::type_::Type)>,
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
    create_unique_name_fn: &dyn Fn(&str) -> String,
    directly_containing_statement_loc: &Loc,
    type_: &ast::types::Type<Loc, Loc>,
) -> Vec<Refactor> {
    let type_loc = type_.loc().dupe();
    let available_tparams = {
        let points = insertion_point_collectors::collect_function_method_inserting_points(
            typed_ast,
            loc_of_aloc,
            type_loc.dupe(),
        );
        match points.first() {
            None => vec![],
            Some(first) => first.tparams_rev.clone(),
        }
    };
    let new_ast_result: Result<ast::Program<Loc, Loc>, crate::insert_type::Expected> = (|| {
        let locs: BTreeSet<Loc> = std::iter::once(type_loc.dupe()).collect();
        let type_synthesizer_context = type_synthesizer::create_synthesizer_context(
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
            type_synthesizer::create_type_synthesizer_with_import_adder(type_synthesizer_context);

        let used_tparams: TypeParamSet = {
            let synth_result = (type_synth.type_synthesizer)(&type_loc)?;
            match synth_result {
                None => BTreeSet::new(),
                Some((tparams_list, _)) => tparams_list.into_iter().collect(),
            }
        };

        let available_set: TypeParamSet = available_tparams.iter().duped().collect();
        let type_params_to_add: Vec<_> = used_tparams.difference(&available_set).duped().collect();

        let new_type_name = create_unique_name_fn("NewType");

        let targs = if type_params_to_add.is_empty() {
            None
        } else {
            let args: Vec<_> = type_params_to_add
                .iter()
                .map(|tp| {
                    // Subst_name.string_of_subst_name name
                    let name_str = tp.name.string_of_subst_name().to_string();
                    ast_builder::types::unqualified_generic(None, None, None, &name_str)
                })
                .collect();
            Some(ast_builder::types::type_args(None, None, args))
        };
        let type_replacement =
            ast_builder::types::unqualified_generic(None, None, targs, &new_type_name);

        let tparams: Option<ast::types::TypeParams<Loc, Loc>> = if type_params_to_add.is_empty() {
            None
        } else {
            let mut synthesized = Vec::new();
            for tp in &type_params_to_add {
                synthesized.push((type_synth.type_param_synthesizer)(tp)?);
            }
            Some(ast_builder::types::type_params(None, None, synthesized))
        };
        let type_alias =
            ast_builder::statements::type_alias(None, None, tparams, &new_type_name, type_.dupe());

        Ok(refactor_program_mappers::extract_type_to_type_alias(
            directly_containing_statement_loc,
            &type_loc,
            &type_replacement,
            &type_alias,
            ast,
        ))
    })();

    match new_ast_result {
        Ok(new_ast) => vec![Refactor {
            title: "Extract to type alias".to_string(),
            new_ast,
            added_imports: vec![],
        }],
        Err(_) => vec![],
    }
}

pub fn provide_available_refactors<'a, 'b>(
    tokens: &[flow_parser::TokenSinkResult],
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'a>,
    file: &FileKey,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, flow_typing_type::type_::Type)>,
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
    support_experimental_snippet_text_edit: bool,
    extract_range: &Loc,
) -> Vec<Refactor> {
    let extracted = ast_extractor::extract(tokens, ast, extract_range.dupe());
    let used_names = variable_analysis::collect_used_names(ast);
    let create_unique_name_fn = |prefix: &str| {
        create_unique_name(support_experimental_snippet_text_edit, &used_names, prefix)
    };
    let stmts_refactors = match &extracted.extracted_statements {
        None => vec![],
        Some(stmts) => extract_from_statements_refactors(
            ast,
            cx,
            file,
            file_sig.dupe(),
            typed_ast,
            loc_of_aloc,
            get_ast_from_shared_mem,
            get_haste_module_info,
            get_type_sig,
            &create_unique_name_fn,
            stmts,
        ),
    };
    let expr_refactors = match &extracted.extracted_expression {
        None => vec![],
        Some(expr_with_cips) => extract_from_expression_refactors(
            ast,
            cx,
            file,
            file_sig.dupe(),
            typed_ast,
            loc_of_aloc,
            get_ast_from_shared_mem,
            get_haste_module_info,
            get_type_sig,
            &create_unique_name_fn,
            &expr_with_cips.constant_insertion_points,
            &expr_with_cips.expression,
        ),
    };
    let type_refactors = match &extracted.extracted_type {
        None => vec![],
        Some(type_with_loc) => extract_from_type_refactors(
            ast,
            cx,
            file,
            file_sig,
            typed_ast,
            loc_of_aloc,
            get_ast_from_shared_mem,
            get_haste_module_info,
            get_type_sig,
            &create_unique_name_fn,
            &type_with_loc.directly_containing_statement_loc,
            &type_with_loc.type_,
        ),
    };
    let mut result = stmts_refactors;
    result.extend(expr_refactors);
    result.extend(type_refactors);
    result
}
