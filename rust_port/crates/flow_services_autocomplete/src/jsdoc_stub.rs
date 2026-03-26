/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::ast;
use flow_parser::jsdoc;
use flow_parser::loc::Loc;

pub struct T {
    pub description: String,
    pub params: jsdoc::Params,
}

fn jsdoc_param_of_pattern(pattern: &ast::pattern::Pattern<Loc, Loc>) -> (String, jsdoc::param::T) {
    match pattern {
        ast::pattern::Pattern::Identifier { inner, .. } => {
            (inner.as_ref().name.name.to_string(), vec![])
        }
        // TODO: handle other kinds of patterns
        ast::pattern::Pattern::Object { .. }
        | ast::pattern::Pattern::Array { .. }
        | ast::pattern::Pattern::Expression { .. } => ("".to_string(), vec![]),
    }
}

pub fn stub_for_function(func: &ast::function::Function<Loc, Loc>) -> T {
    let params = {
        let add_this_param =
            |this_: &Option<ast::function::ThisParam<Loc, Loc>>,
             mut jsdoc_params: Vec<(String, jsdoc::param::T)>| {
                match this_ {
                    Some(_) => {
                        jsdoc_params.push(("this".to_string(), vec![]));
                        jsdoc_params
                    }
                    None => jsdoc_params,
                }
            };

        let add_params = |params: &[ast::function::Param<Loc, Loc>],
                          jsdoc_params: Vec<(String, jsdoc::param::T)>| {
            params
                .iter()
                .rev()
                .fold(jsdoc_params, |mut jsdoc_params, param| {
                    match param {
                        ast::function::Param::RegularParam { argument, .. } => {
                            jsdoc_params.push(jsdoc_param_of_pattern(argument));
                        }
                        ast::function::Param::ParamProperty { property, .. } => {
                            let name = match &property.key {
                                ast::expression::object::Key::Identifier(id) => id.name.to_string(),
                                _ => "param".to_string(),
                            };
                            jsdoc_params.push((name, vec![]));
                        }
                    }
                    jsdoc_params
                })
        };

        let add_rest_param =
            |rest: &Option<ast::function::RestParam<Loc, Loc>>,
             mut jsdoc_params: Vec<(String, jsdoc::param::T)>| {
                if let Some(rest) = rest {
                    jsdoc_params.push(jsdoc_param_of_pattern(&rest.argument));
                }
                jsdoc_params
            };

        let params_rev = Vec::new();
        let params_rev = add_this_param(&func.params.this_, params_rev);
        let params_rev = add_params(&func.params.params, params_rev);
        let params_rev = add_rest_param(&func.params.rest, params_rev);
        params_rev.into_iter().rev().collect()
    };

    T {
        description: "$0".to_string(),
        params: jsdoc::Params(params),
    }
}

pub fn string_of_stub(stub: &T, use_snippets: bool) -> String {
    let params = stub
        .params
        .0
        .iter()
        .enumerate()
        .map(|(i, (name, _))| {
            let desc = if use_snippets {
                format!("${}", i + 1)
            } else {
                String::new()
            };
            format!("\n * @param {} {}", name, desc)
        })
        .collect::<Vec<_>>()
        .join("");
    format!("*\n * {}{}\n", stub.description, params)
}

pub fn string_of_stub_default(stub: &T) -> String {
    string_of_stub(stub, true)
}
