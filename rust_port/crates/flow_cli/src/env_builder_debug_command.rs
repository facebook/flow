/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_aloc::LocToALocMapper;
use flow_common::options::JsxMode;
use flow_common::options::ReactRuntime;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_env_builder::env_api::AutocompleteHooks;
use flow_env_builder::env_api::DefLocType;
use flow_env_builder::env_api::EnvKey;
use flow_env_builder::env_api::EnvMap;
use flow_env_builder::name_def;
use flow_env_builder::name_def_types::ScopeKind;
use flow_env_builder_resolver::dependency_sigs::Context;
use flow_env_builder_resolver::dependency_sigs::Flow;
use flow_env_builder_resolver::name_def_ordering;
use flow_env_builder_resolver::name_resolver;
use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser::parse_error::ParseError;
use flow_parser::polymorphic_ast_mapper;
use flow_typing_errors::error_message::ErrorMessage;
use vec1::Vec1;

struct TestCx;

impl Context for TestCx {
    fn enable_enums(&self) -> bool {
        true
    }

    fn file(&self) -> FileKey {
        FileKey::new(FileKeyInner::SourceFile("test.js".to_string()))
    }

    fn jsx(&self) -> JsxMode {
        JsxMode::JsxReact
    }

    fn react_runtime(&self) -> ReactRuntime {
        ReactRuntime::Classic
    }

    fn enable_const_params(&self) -> bool {
        false
    }

    fn stylex_shorthand_prop(&self) -> Option<&str> {
        None
    }

    fn add_exhaustive_check(&self, _loc: ALoc, _cases: (Vec<ALoc>, bool)) {}

    fn exhaustive_check(&self, _loc: &ALoc) -> Option<(Vec<ALoc>, bool)> {
        None
    }
}

struct TestFlow;

impl Flow for TestFlow {
    type Cx = TestCx;

    fn add_output(_cx: &Self::Cx, _error: ErrorMessage<ALoc>) {}
}

fn print_graph(graph: Vec<(String, String)>) {
    println!("digraph {{\n");
    for (from, to_) in graph {
        println!("  \"{}\" -> \"{}\"", from, to_);
    }
    println!("}}");
}

fn autocomplete_hooks() -> AutocompleteHooks<ALoc> {
    AutocompleteHooks {
        id_hook: Box::new(|_, _| false),
        literal_hook: Box::new(|_| false),
        obj_prop_decl_hook: Box::new(|_, _| false),
    }
}

fn parse(contents: &str) -> (flow_parser::ast::Program<Loc, Loc>, Vec<(Loc, ParseError)>) {
    let parse_options = Some(PERMISSIVE_PARSE_OPTIONS);
    flow_parser::parse_program_without_file(false, None, parse_options, Ok(contents))
}

fn parse_with_alocs(
    contents: &str,
) -> (
    flow_parser::ast::Program<ALoc, ALoc>,
    Vec<(Loc, ParseError)>,
) {
    let (loc_ast, errors) = parse(contents);
    let mut mapper = LocToALocMapper;
    let Ok(aloc_ast) = polymorphic_ast_mapper::program(&mut mapper, &loc_ast);
    (aloc_ast, errors)
}

pub fn main(filename: &str) {
    let content = match std::fs::read_to_string(filename) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("Program does not parse.");
            return;
        }
    };
    let (ast, errors) = parse_with_alocs(&content);
    if errors.is_empty() {
        let (_, env) = name_resolver::program_with_scope::<TestCx, TestFlow>(
            &TestCx,
            false,
            FlowOrdSet::new(),
            &ast,
        );
        let env = env.to_env_info();
        let hooks = autocomplete_hooks();
        let (inits, _) = name_def::find_defs(&hooks, true, &env, ScopeKind::Module, &ast);

        // Connect read -> write edges and write -> read edges to form a graph
        let graph =
            name_def_ordering::build_graph::<_, _, TestCx, TestFlow>(&TestCx, &hooks, &env, &inits);
        match graph {
            Ok(g) => {
                let order_graph = format_graph(&g);
                print_graph(order_graph);
            }
            Err(_) => {
                eprintln!("Cannot generate graph.");
            }
        }
    } else {
        eprintln!("Program does not parse.");
    }
}

fn format_graph(graph: &EnvMap<ALoc, EnvMap<ALoc, Vec1<ALoc>>>) -> Vec<(String, String)> {
    fn show_def_loc_type_with_path(def_loc_type: DefLocType) -> String {
        format!("Env_api.Make.{}", def_loc_type.show())
    }

    fn print_kind_and_loc(def_loc_type: DefLocType, loc: &ALoc) -> String {
        if def_loc_type == DefLocType::OrdinaryNameLoc {
            loc.debug_to_string(false)
        } else {
            format!(
                "{} ({})",
                loc.debug_to_string(false),
                show_def_loc_type_with_path(def_loc_type)
            )
        }
    }

    fn print_write_kind_and_loc(key: &EnvKey<ALoc>) -> String {
        format!("write: {}", print_kind_and_loc(key.def_loc_type, &key.loc))
    }

    fn print_read_loc(loc: &ALoc) -> String {
        format!("read: {}", loc.debug_to_string(false))
    }

    let mut edges = Vec::new();
    for (w_from, deps) in graph.iter() {
        for (w_to, whys) in deps.iter() {
            for why in whys.iter().rev() {
                edges.push((print_write_kind_and_loc(w_from), print_read_loc(why)));
                edges.push((print_read_loc(why), print_write_kind_and_loc(w_to)));
            }
        }
    }
    edges
}
