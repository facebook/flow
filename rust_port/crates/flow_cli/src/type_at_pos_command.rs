/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// ***********************************************************************
// flow type-at-pos command
// ***********************************************************************

use std::path::Path;

use flow_common_errors::error_utils;
use flow_parser::loc_sig::LocSig;
use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;
fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "type-at-pos",
        "Shows the type at a given file and position",
        format!(
            "Usage: {exe_name} type-at-pos [OPTION]... [FILE] LINE COLUMN\n\ne.g. {exe_name} type-at-pos foo.js 12 3\nor   {exe_name} type-at-pos 12 3 < foo.js\n"
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_verbose_flags(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_path_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
            "--expand-json-output",
            &arg_spec::truthy(),
            "Includes an expanded version of the returned JSON type (implies --json)",
            None,
        )
        .flag(
            "--omit-typearg-defaults",
            &arg_spec::truthy(),
            "Omit type arguments when defaults exist and match the provided type argument",
            None,
        )
        .flag(
            "--max-depth",
            &arg_spec::required(Some(40), arg_spec::int()),
            "Maximum depth of type (default 40)",
            None,
        )
        .flag(
            "--verbose-normalizer",
            &arg_spec::truthy(),
            "Print verbose info during normalization",
            None,
        )
        .flag(
            "--debug-print-internal-repr",
            &arg_spec::truthy(),
            "Print internal representation of the type for debugging purposes. You should not depend on the output.",
            None,
        )
        // internal flag for regression purposes
        .flag(
            "--do_not_use_typed_AST_for_imports",
            &arg_spec::truthy(),
            "",
            None,
        )
        .anon("args", &arg_spec::list_of(arg_spec::string()))
}

pub(crate) fn handle_friendly_result(
    strip_root: Option<&str>,
    loc: &flow_parser::loc::Loc,
    tys: &Option<server_prot::response::infer_type::FriendlyResponse>,
    refining_locs: &[flow_parser::loc::Loc],
    documentation: Option<&str>,
) {
    let (ty, refs) = match tys {
        Some(server_prot::response::infer_type::FriendlyResponse { type_str, refs }) => {
            (type_str.as_str(), refs.as_ref())
        }
        None => ("(unknown)", None),
    };
    let doc = documentation
        .map(|documentation| format!("{documentation}\n"))
        .unwrap_or_default();
    let range = if *loc == flow_parser::loc::Loc::none() {
        String::new()
    } else {
        format!(
            "\n{}",
            flow_common::reason::range_string_of_loc(strip_root, loc)
        )
    };
    let str_of_loc = |loc: &flow_parser::loc::Loc| -> Option<String> {
        let source = loc.source.as_ref()?;
        let source_str = source.to_absolute();
        let basename = Path::new(&source_str)
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or(&source_str);
        let lib = if source.is_lib_file() { "(lib) " } else { "" };
        Some(format!(
            "{}{}:{}:{}",
            lib, basename, loc.start.line, loc.start.column
        ))
    };
    let refs = match refs {
        None => String::new(),
        Some(refs) => {
            let refs = refs
                .iter()
                .filter_map(|(name, loc)| {
                    str_of_loc(loc).map(|loc| format!("'{}' defined at {}", name, loc))
                })
                .collect::<Vec<_>>();
            match refs.as_slice() {
                [] => String::new(),
                _ => format!("\n\n{}\n", refs.join("\n")),
            }
        }
    };
    let refinement_info = {
        let refining_locs = refining_locs
            .iter()
            .filter_map(str_of_loc)
            .collect::<Vec<_>>();
        match refining_locs.as_slice() {
            [] => String::new(),
            _ => format!("\n\nRefined at {}\n", refining_locs.join(", ")),
        }
    };
    println!("{doc}{ty}{refs}{refinement_info}{range}");
}

fn handle_response(
    _file_contents: Option<String>,
    pretty: bool,
    strip_root: Option<&str>,
    response: server_prot::response::infer_type::T,
) {
    let server_prot::response::infer_type::T {
        loc,
        tys,
        refining_locs,
        refinement_invalidated: _,
        documentation,
    } = response;
    match tys {
        server_prot::response::infer_type::Payload::Json(types) => {
            let mut json = serde_json::Map::new();
            json.insert("types".to_string(), types);
            json.insert("reasons".to_string(), serde_json::Value::Array(vec![]));
            json.insert(
                "loc".to_string(),
                command_utils::json_of_loc_with_offset(None, strip_root, &loc),
            );
            json.extend(error_utils::deprecated_json_props_of_loc(strip_root, &loc));
            if let Some(documentation) = documentation {
                json.insert(
                    "documentation".to_string(),
                    serde_json::Value::String(documentation),
                );
            }
            flow_hh_json::print_json_endline(pretty, &serde_json::Value::Object(json));
        }
        server_prot::response::infer_type::Payload::Friendly(tys) => {
            handle_friendly_result(
                strip_root,
                &loc,
                &tys,
                &refining_locs,
                documentation.as_deref(),
            );
        }
    }
}

fn handle_error(err: String, json: bool, pretty: bool) {
    if json {
        flow_hh_json::prerr_json_endline(
            pretty,
            &serde_json::json!({
                "error": err,
            }),
        );
    } else {
        eprintln!("{}", err);
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let verbose = command_utils::verbose_flags(args);
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let expanded = command_spec::get(args, "--expand-json-output", &arg_spec::truthy()).unwrap();
    let json = json_flags.json || json_flags.pretty || expanded;
    let omit_targ_defaults =
        command_spec::get(args, "--omit-typearg-defaults", &arg_spec::truthy()).unwrap();
    let max_depth = command_spec::get(
        args,
        "--max-depth",
        &arg_spec::required(Some(40), arg_spec::int()),
    )
    .unwrap();
    let verbose_normalizer =
        command_spec::get(args, "--verbose-normalizer", &arg_spec::truthy()).unwrap();
    let debug_print_internal_repr =
        command_spec::get(args, "--debug-print-internal-repr", &arg_spec::truthy()).unwrap();
    let no_typed_ast_for_imports = command_spec::get(
        args,
        "--do_not_use_typed_AST_for_imports",
        &arg_spec::truthy(),
    )
    .unwrap();
    let raw_args = command_spec::get(args, "args", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();

    let cmd = command_spec::command(spec(), |_| {});
    let (file, line, column) =
        command_utils::parse_location_with_optional_filename(&cmd, path.as_deref(), &raw_args);
    let root = command_utils::find_a_root(
        &base_flags.flowconfig_name,
        root_arg.as_deref(),
        Some(&file),
    );
    let strip_root = if strip_root { Some(root.clone()) } else { None };
    if !json && verbose.is_some() {
        eprintln!("NOTE: --verbose writes to the server log file");
    }
    let request = server_prot::request::Command::INFER_TYPE(server_prot::infer_type_options::T {
        input: file.clone(),
        line,
        r#char: column,
        verbose,
        omit_targ_defaults,
        wait_for_recheck,
        verbose_normalizer,
        max_depth,
        json,
        strip_root: strip_root.clone(),
        expanded,
        debug_print_internal_repr,
        no_typed_ast_for_imports,
    });
    let response = command_utils::connect_and_make_request(
        &base_flags.flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::INFER_TYPE(Err(err)) => {
            handle_error(err, json, json_flags.pretty)
        }
        server_prot::response::Response::INFER_TYPE(Ok(response)) => {
            let file_contents = file.content_of_file_input().ok();
            let strip_root_string = strip_root
                .as_ref()
                .map(|root| root.to_string_lossy().to_string());
            handle_response(
                file_contents,
                json_flags.pretty,
                strip_root_string.as_deref(),
                response,
            );
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
