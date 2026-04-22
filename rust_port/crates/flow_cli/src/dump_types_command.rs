/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common_errors::error_utils;
use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow dump-types command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    // Outputs list of all types in the file
    let spec = command_spec::Spec::new(
        "dump-types",
        "",
        format!(
            "Usage: {exe_name} dump-types [OPTION]... [FILE]\n\ne.g. {exe_name} dump-types foo.js\nor   {exe_name} dump-types < foo.js\n"
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_path_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
        "--evaluate-type-destructors",
        &arg_spec::truthy(),
        "Use the result of type destructor evaluation if available",
        None,
    )
    .flag(
        "--for-tool",
        &arg_spec::optional_value_with_default(10, arg_spec::int()),
        "",
        None,
    )
    .anon("file", &arg_spec::optional(arg_spec::string()))
}

fn types_to_json(
    file_content: Option<&String>,
    strip_root: Option<&str>,
    types: &[(flow_parser::loc::Loc, String)],
) -> serde_json::Value {
    // let offset_table =
    //   Base.Option.map file_content ~f:(Offset_utils.make ~kind:Offset_utils.Utf8)
    let offset_table =
        file_content.map(|s| flow_parser::offset_utils::OffsetTable::make(s.as_str()));
    // let types_json =
    //   types
    //   |> Base.List.map ~f:(fun (loc, t) ->
    //          let json_assoc =
    //            ("type", JSON_String t)
    //            :: ("reasons", JSON_Array [])
    //            :: ("loc", json_of_loc ~strip_root ~offset_table loc)
    //            :: Flow_errors_utils.deprecated_json_props_of_loc ~strip_root loc
    //          in
    //          JSON_Object json_assoc
    //      )
    // in
    // JSON_Array types_json
    serde_json::Value::Array(
        types
            .iter()
            .map(|(loc, t)| {
                let mut json = serde_json::Map::new();
                json.insert("type".to_string(), serde_json::Value::String(t.clone()));
                json.insert("reasons".to_string(), serde_json::Value::Array(vec![]));
                json.insert(
                    "loc".to_string(),
                    flow_common::reason::json_of_loc(strip_root, false, offset_table.as_ref(), loc),
                );
                json.extend(error_utils::deprecated_json_props_of_loc(strip_root, loc));
                serde_json::Value::Object(json)
            })
            .collect(),
    )
}

fn handle_response(
    types: Vec<(flow_parser::loc::Loc, String)>,
    json: bool,
    file_content: Option<&String>,
    pretty: bool,
    strip_root: Option<&str>,
) {
    if json {
        flow_hh_json::print_json_endline(pretty, &types_to_json(file_content, strip_root, &types));
    } else {
        let out = types
            .into_iter()
            .map(|(loc, ty)| {
                format!(
                    "{}: {}",
                    flow_common::reason::string_of_loc(strip_root, &loc),
                    ty
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        println!("{}", out);
    }
}

fn handle_error(
    err: String,
    file_content: Option<&String>,
    json: bool,
    pretty: bool,
    strip_root: Option<&str>,
) {
    if json {
        flow_hh_json::prerr_json_endline(
            pretty,
            &serde_json::json!({
                "error": err,
            }),
        );
        // also output an empty array on stdout, for JSON parsers
        handle_response(vec![], json, file_content, pretty, strip_root);
    } else {
        eprintln!("{}", err);
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let evaluate_type_destructors =
        command_spec::get(args, "--evaluate-type-destructors", &arg_spec::truthy()).unwrap();
    let for_tool = command_spec::get(
        args,
        "--for-tool",
        &arg_spec::optional_value_with_default(10, arg_spec::int()),
    )
    .unwrap();
    let filename =
        command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();

    let json = json_flags.json || json_flags.pretty;
    let file = command_utils::get_file_from_filename_or_stdin(
        "dump-types",
        filename.as_deref(),
        path.as_deref(),
    );
    let file_content = file.content_of_file_input().ok();
    let root = command_utils::guess_root(
        &flowconfig_name,
        match root_arg.as_deref() {
            Some(root) => Some(root),
            None => file.path_of_file_input(),
        },
    );
    let strip_root = if strip_root {
        Some(root.to_string_lossy().to_string())
    } else {
        None
    };
    let request = server_prot::request::Command::DUMP_TYPES {
        input: file,
        evaluate_type_destructors,
        for_tool,
        wait_for_recheck,
    };
    let response =
        command_utils::connect_and_make_request(&flowconfig_name, &connect_flags, &root, &request);
    match response {
        server_prot::response::Response::DUMP_TYPES(Err(err)) => {
            handle_error(
                err,
                file_content.as_ref(),
                json,
                json_flags.pretty,
                strip_root.as_deref(),
            );
        }
        server_prot::response::Response::DUMP_TYPES(Ok(response)) => {
            handle_response(
                response,
                json,
                file_content.as_ref(),
                json_flags.pretty,
                strip_root.as_deref(),
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
