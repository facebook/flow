/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Read;
use std::path::Path;

use flow_aloc::ALoc;
use flow_aloc::LocToALocMapper;
use flow_common::options::JsxMode;
use flow_common::options::ReactRuntime;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_env_builder::env_api::AutocompleteHooks;
use flow_env_builder::env_api::DefLocType;
use flow_env_builder::env_api::EnvKey;
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
use flow_server_utils::file_input::FileInput;
use flow_typing_errors::error_message::ErrorMessage;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "env-builder-debug",
        "Print the env-builder result as a dependency graph for debugging purposes",
        command_spec::Visibility::Internal,
        format!(
            "Usage: {exe_name} env-builder-debug [OPTION]... [FILE]\n\ne.g. {exe_name} env-builder-debug foo.js\nor   {exe_name} env-builder-debug < foo.js\n"
        ),
    );
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_path_flag(spec);
    spec.anon("file", &arg_spec::optional(arg_spec::string()))
}

fn get_file(path: Option<String>, filename: Option<String>) -> FileInput {
    match filename {
        Some(filename) => FileInput::FileName(command_utils::expand_path(&filename)),
        None => {
            let mut content = String::new();
            std::io::stdin()
                .read_to_string(&mut content)
                .expect("failed to read stdin");
            FileInput::FileContent(path, content)
        }
    }
}

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

fn autocomplete_hooks() -> AutocompleteHooks<'static, ALoc> {
    AutocompleteHooks {
        id_hook: Box::new(|_, _| false),
        literal_hook: Box::new(|_| false),
        obj_prop_decl_hook: Box::new(|_, _| false),
    }
}

pub fn main(path: Option<String>, filename: Option<String>) {
    let use_relative_path = filename
        .as_ref()
        .is_some_and(|filename| Path::new(filename).is_relative());
    let file = get_file(path, filename);
    let content = file.content_of_file_input_unsafe();
    let parse_options = Some(PERMISSIVE_PARSE_OPTIONS);
    let filename = file.path_of_file_input().map(str::to_owned);
    let filename = if use_relative_path {
        filename.as_ref().map(|filename| {
            flow_common::files::relative_path(
                &std::env::current_dir().expect("failed to get current directory"),
                filename,
            )
        })
    } else {
        filename
    };

    let filekey = filename.map(|filename| FileKey::new(FileKeyInner::SourceFile(filename)));
    let (ast, errors): (flow_parser::ast::Program<Loc, Loc>, Vec<(Loc, ParseError)>) = match filekey
    {
        Some(filekey) => {
            flow_parser::parse_program_file::<()>(false, None, parse_options, filekey, Ok(&content))
        }
        None => flow_parser::parse_program_without_file(false, None, parse_options, Ok(&content)),
    };
    let mut mapper = LocToALocMapper;
    let Ok(ast) = polymorphic_ast_mapper::program(&mut mapper, &ast);

    if errors.is_empty() {
        // Compute read -> write edges
        let (_, env) = name_resolver::program_with_scope::<TestCx, TestFlow>(
            &TestCx,
            false,
            FlowOrdSet::new(),
            &ast,
        );
        let env = env.to_env_info();
        // Compute write -> read edges
        let autocomplete_hooks = autocomplete_hooks();
        let (inits, _) =
            name_def::find_defs(&autocomplete_hooks, true, &env, ScopeKind::Module, &ast);

        // Connect read -> write edges and write -> read edges to form a graph
        let Ok(graph) = name_def_ordering::build_graph::<_, _, TestCx, TestFlow>(
            &TestCx,
            &autocomplete_hooks,
            &env,
            &inits,
        ) else {
            eprintln!("Cannot generate graph.");
            return;
        };

        let order_graph = {
            fn print_kind_and_loc(def_loc_type: DefLocType, loc: &ALoc) -> String {
                if def_loc_type == DefLocType::OrdinaryNameLoc {
                    loc.debug_to_string(false)
                } else {
                    format!("{} ({})", loc.debug_to_string(false), def_loc_type.show())
                }
            }

            fn print_write_kind_and_loc(key: &EnvKey<ALoc>) -> String {
                format!("write: {}", print_kind_and_loc(key.def_loc_type, &key.loc))
            }

            fn print_read_loc(loc: &ALoc) -> String {
                format!("read: {}", loc.debug_to_string(false))
            }

            let mut result = Vec::new();
            for (w_from, deps) in graph.iter() {
                for (w_to, whys) in deps.iter() {
                    for why in whys.iter() {
                        result.push((print_write_kind_and_loc(w_from), print_read_loc(why)));
                        result.push((print_read_loc(why), print_write_kind_and_loc(w_to)));
                    }
                }
            }
            result
        };

        print_graph(order_graph);
    } else {
        eprintln!("Program does not parse.");
    }
}

fn command_main(args: &arg_spec::Values) {
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let filename =
        command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();
    main(path, filename);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), command_main)
}
