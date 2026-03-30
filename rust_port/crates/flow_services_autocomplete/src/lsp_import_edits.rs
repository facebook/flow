/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use flow_common::files;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils_output::js_layout_generator;
use flow_services_export::export_index;
use flow_typing_context::Context;
use lsp_types::Position as LspPosition;
use lsp_types::Range as LspRange;
use lsp_types::TextEdit;

use crate::autofix_imports;
use crate::code_action_text_edits::CodeActionTextEdits;
use crate::module_system_info::LspModuleSystemInfo;

pub fn is_available_autoimport_result<'a>(
    cx: &'a Context<'_>,
) -> impl Fn(&str, &export_index::Source) -> bool + 'a {
    let available_globals = cx.builtins().builtin_ordinary_name_set();
    let available_modules = cx.builtins().builtin_modules_set();
    move |name: &str, source: &export_index::Source| match source {
        export_index::Source::Global => available_globals.contains(name),
        export_index::Source::Builtin(mref) => available_modules.contains(mref.display()),
        export_index::Source::FileKey(_) => true,
    }
}

fn flow_position_to_lsp_position(pos: Position) -> LspPosition {
    LspPosition::new(pos.line.saturating_sub(1) as u32, pos.column.max(0) as u32)
}

fn loc_to_lsp_range(loc: Loc) -> LspRange {
    LspRange::new(
        flow_position_to_lsp_position(loc.start),
        flow_position_to_lsp_position(loc.end),
    )
}

fn main_of_package(
    get_package_info: &dyn Fn(
        &FileKey,
    )
        -> Option<Result<flow_parser_utils::package_json::PackageJson, ()>>,
    package_dir: &str,
) -> Option<String> {
    let file_key = FileKey::new(FileKeyInner::JsonFile(
        Path::new(package_dir)
            .join("package.json")
            .to_string_lossy()
            .into_owned(),
    ));
    match get_package_info(&file_key) {
        Some(Ok(package)) => package.main().map(|main| main.to_string()),
        Some(Err(())) | None => None,
    }
}

// [find_ancestor_rev a_parts b_parts], where [a_parts] and [b_parts] are two paths split
// into segments (see [Files.split_path]), returns [(ancestor_parts, a_relative, b_relative)],
// where [ancestor_parts] are the common prefix parts **reversed**, [a_relative] is the
// remaining parts from the ancestor to [a_parts], and [b_relative] is the remaining parts
// from the ancestor to [b_parts].
//
// for example, [find_ancestor_rev ["/a"; "b"; "c"; "d"] ["/a"; "b"; "e"; "f"]] returns
// [(["b"; "/a"], ["c"; "d"], ["e"; "f"])]
fn find_ancestor_rev(
    a_parts: &[String],
    b_parts: &[String],
) -> (Vec<String>, Vec<String>, Vec<String>) {
    let mut acc = Vec::new();
    let mut i = 0;
    while i < a_parts.len() && i < b_parts.len() && a_parts[i] == b_parts[i] {
        acc.push(a_parts[i].clone());
        i += 1;
    }
    acc.reverse();
    (acc, a_parts[i..].to_vec(), b_parts[i..].to_vec())
}

// [path_matches expected actual] returns true if [actual] is the same as [expected], ignoring
// a potential leading [./] on [actual].
fn path_matches(expected: &str, actual: &str) -> bool {
    expected == actual || Path::new(actual).is_relative() && actual == format!("./{expected}")
}

fn string_of_path_parts(parts: &[String]) -> String {
    let str_ = parts.join("/");
    let without_index = str_.strip_suffix("/index.js").unwrap_or(&str_);
    without_index
        .strip_suffix(".js")
        .unwrap_or(without_index)
        .to_string()
}

fn path_parts_rev_to_absolute(dir_rev: &[String]) -> String {
    dir_rev
        .iter()
        .rev()
        .cloned()
        .collect::<Vec<_>>()
        .join(std::path::MAIN_SEPARATOR_STR)
}

// For a `package_absolute_path` already decided to contain a package.json, we decide whether a given
// file path (broken down into parts and reversed as `src_rev`) can import it as a node_package like
// `package_dir` or `package_dir/nested/module`, instead of a full relative part import.
//
// Here, we follow the node resolution algoritim wrt node_modules. If in foo/bar/baz.js,
// we import package_a, then node will try to look for the package in the following order
//
// - foo/bar/node_modules
// - foo/node_modules
// - node_modules
//
// For each candidate, we call realpath to find out whether the candidate path, if exists, resolves
// to the same package_absolute_path.
fn can_import_as_node_package(
    node_resolver_dirnames: &[String],
    resolves_to_real_path: &dyn Fn(&str, &str) -> bool,
    package_absolute_path: &str,
    package_dir: &str,
    src_dir_rev_nel: Option<&[String]>,
) -> bool {
    match src_dir_rev_nel {
        None => false,
        Some([]) => false,
        Some(src_dir_rev) => {
            let inner_most_dir = &src_dir_rev[0];
            let src_dir_rev_list = &src_dir_rev[1..];
            node_resolver_dirnames.iter().any(|node_modules| {
                let from = path_parts_rev_to_absolute(
                    &[
                        vec![
                            package_dir.to_string(),
                            node_modules.clone(),
                            inner_most_dir.clone(),
                        ],
                        src_dir_rev_list.to_vec(),
                    ]
                    .concat(),
                );
                resolves_to_real_path(&from, package_absolute_path)
            }) || can_import_as_node_package(
                node_resolver_dirnames,
                resolves_to_real_path,
                package_absolute_path,
                package_dir,
                if src_dir_rev_list.is_empty() {
                    None
                } else {
                    Some(src_dir_rev_list)
                },
            )
        }
    }
}

fn chop_prefix_opt(root_parts: &[String], require_path: &[String]) -> Option<Vec<String>> {
    match (root_parts.split_first(), require_path.split_first()) {
        (None, _) => Some(require_path.to_vec()),
        (Some(_), None) => None,
        (Some((root_hd, root_tl)), Some((req_hd, req_tl))) => {
            if root_hd == req_hd {
                chop_prefix_opt(root_tl, req_tl)
            } else {
                None
            }
        }
    }
}

// [node_path ~node_resolver_dirnames ~reader src_dir require_path] converts absolute path
// [require_path] into a Node-compatible "require" path relative to [src_dir], taking into
// account node's hierarchical search for [node_modules].
//
// That is, if [require_path] is within a [node_modules] folder in [src_dir] or one of
// [src_dir]'s parents, then the [node_modules] prefix is removed. If the package's
// [package.json] has a [main] field, that suffix is also removed.
//
// If not part of [node_modules], then [require_path] is relativized with respect to
// [src_dir].
//
// Lastly, if the path ends with [index.js] or [.js], those default suffixes are also
// removed.
fn node_path(
    node_resolver_dirnames: &[String],
    node_resolver_root_relative_dirnames: &[(Option<String>, String)],
    module_declaration_dirnames: &[String],
    get_package_info: &dyn Fn(
        &FileKey,
    )
        -> Option<Result<flow_parser_utils::package_json::PackageJson, ()>>,
    resolves_to_real_path: &dyn Fn(&str, &str) -> bool,
    src_dir: &str,
    require_path: &str,
) -> String {
    let require_path = require_path
        .strip_suffix(files::FLOW_EXT)
        .unwrap_or(require_path);
    let src_parts = files::split_path(src_dir);
    let req_parts = files::split_path(require_path);
    let (ancestor_rev, to_src, to_req) = find_ancestor_rev(&src_parts, &req_parts);
    let src_rev = [to_src.clone(), ancestor_rev.clone()]
        .concat()
        .into_iter()
        .rev()
        .collect::<Vec<_>>();

    // In this function, we will check whether any of the ancestor directory of the required file
    // is a package. If so, we call can_import_as_node_package to see whether we can import it as
    // a node package.
    fn node_modules_package_import_path(
        ancestor_rev: &[String],
        to_req: &[String],
        node_resolver_dirnames: &[String],
        resolves_to_real_path: &dyn Fn(&str, &str) -> bool,
        get_package_info: &dyn Fn(
            &FileKey,
        ) -> Option<
            Result<flow_parser_utils::package_json::PackageJson, ()>,
        >,
        package_absolute_path: &str,
        src_rev: &[String],
    ) -> Option<String> {
        let (package_dir, rest) = to_req.split_first()?;
        let package_dir_rev = [vec![package_dir.clone()], ancestor_rev.to_vec()].concat();
        let package_json = FileKey::new(FileKeyInner::JsonFile(path_parts_rev_to_absolute(
            &[vec!["package.json".to_string()], package_dir_rev.clone()].concat(),
        )));
        match get_package_info(&package_json) {
            Some(Ok(package_info))
                if can_import_as_node_package(
                    node_resolver_dirnames,
                    resolves_to_real_path,
                    package_absolute_path,
                    package_dir,
                    Some(src_rev),
                ) =>
            {
                let rest_joined = rest.join("/");
                match package_info.main() {
                    Some(main) if path_matches(&rest_joined, main.as_ref()) => {
                        Some(package_dir.clone())
                    }
                    _ => Some(string_of_path_parts(
                        &[vec![package_dir.clone()], rest.to_vec()].concat(),
                    )),
                }
            }
            _ => node_modules_package_import_path(
                &[vec![package_dir.clone()], ancestor_rev.to_vec()].concat(),
                rest,
                node_resolver_dirnames,
                resolves_to_real_path,
                get_package_info,
                package_absolute_path,
                src_rev,
            ),
        }
    }

    // Similar to the function above that tries to prettify import path of a node module, here
    // we try to prettify the import path for root relative modules.
    let root_relative_import_path = |require_path: &[String]| {
        node_resolver_root_relative_dirnames.iter().find_map(
            |(src_prefix_opt, root_relative_dirname)| {
                let prefix_matches = match src_prefix_opt {
                    None => true,
                    Some(prefix) => {
                        files::is_prefix(prefix, src_dir)
                            || src_dir.ends_with(&format!("/{prefix}"))
                            || src_dir.contains(&format!("/{prefix}/"))
                    }
                };
                if prefix_matches {
                    let root_relative_dirname_parts = files::split_path(root_relative_dirname);
                    chop_prefix_opt(&root_relative_dirname_parts, require_path)
                        .map(|parts| string_of_path_parts(&parts))
                } else {
                    None
                }
            },
        )
    };

    // Similar to the function above that tries to prettify import path of a node module, here
    // we try to prettify the import path for @flowtyped modules.
    let module_declaration_import_path = |require_path: &[String]| {
        module_declaration_dirnames.iter().find_map(|dirname| {
            let dirname_parts = files::split_path(dirname);
            chop_prefix_opt(&dirname_parts, require_path).map(|parts| string_of_path_parts(&parts))
        })
    };

    let package_absolute_path = path_parts_rev_to_absolute(&ancestor_rev);
    if let Some(path) = node_modules_package_import_path(
        &ancestor_rev,
        &to_req,
        node_resolver_dirnames,
        resolves_to_real_path,
        get_package_info,
        &package_absolute_path,
        &src_rev,
    ) {
        return path;
    }
    if let Some(path) = root_relative_import_path(&req_parts) {
        return path;
    }
    if let Some(path) = module_declaration_import_path(&req_parts) {
        return path;
    }
    let parts = if to_src.is_empty() {
        [vec![".".to_string()], to_req.clone()].concat()
    } else {
        let mut path = to_req.clone();
        for _ in &to_src {
            path.insert(0, "..".to_string());
        }
        path
    };
    string_of_path_parts(&parts)
}

// [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
// suitable for importing [t] from a file in [src_dir]. that is, if it is a
// filename, returns the path relative to [src_dir].
fn path_of_modulename(
    node_resolver_dirnames: &[String],
    node_resolver_root_relative_dirnames: &[(Option<String>, String)],
    module_declaration_dirnames: &[String],
    get_package_info: &dyn Fn(
        &FileKey,
    )
        -> Option<Result<flow_parser_utils::package_json::PackageJson, ()>>,
    resolves_to_real_path: &dyn Fn(&str, &str) -> bool,
    src_dir: Option<&str>,
    file_key: &FileKey,
    string_module_name: Option<String>,
) -> Option<String> {
    match string_module_name {
        Some(module_name) => Some(module_name),
        None => src_dir.map(|src_dir| {
            let path = files::chop_flow_ext(file_key).as_str().to_string();
            node_path(
                node_resolver_dirnames,
                node_resolver_root_relative_dirnames,
                module_declaration_dirnames,
                get_package_info,
                resolves_to_real_path,
                src_dir,
                &path,
            )
        }),
    }
}

fn haste_package_path(
    module_system_info: &LspModuleSystemInfo,
    src_dir: &str,
    require_path: &str,
) -> Option<String> {
    let mut parts = files::split_path(require_path);
    parts.reverse();
    let base = parts.first().cloned()?;
    let parent_dir_names = &parts[1..];
    let src_parts = files::split_path(src_dir);
    fn f(
        acc: Vec<String>,
        remaining: &[String],
        module_system_info: &LspModuleSystemInfo,
        src_parts: &[String],
        require_path: &str,
    ) -> Option<String> {
        let (package_name_candidate, parent_dir_names) = remaining.split_first()?;
        if (module_system_info.is_package_file)(require_path, package_name_candidate) {
            let package_path_parts = [
                vec![package_name_candidate.clone()],
                parent_dir_names.to_vec(),
            ]
            .concat()
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
            let within_package = matches!(find_ancestor_rev(&package_path_parts, src_parts), (_, to_package, _) if to_package.is_empty());
            if within_package {
                None
            } else {
                let package_dir = package_path_parts.join(std::path::MAIN_SEPARATOR_STR);
                Some(
                    match main_of_package(&*module_system_info.get_package_info, &package_dir) {
                        Some(main) if path_matches(&acc.join("/"), main.as_ref()) => {
                            package_name_candidate.clone()
                        }
                        _ => string_of_path_parts(
                            &[vec![package_name_candidate.clone()], acc].concat(),
                        ),
                    },
                )
            }
        } else {
            let mut next_acc = acc;
            next_acc.push(package_name_candidate.clone());
            f(
                next_acc,
                parent_dir_names,
                module_system_info,
                src_parts,
                require_path,
            )
        }
    }
    f(
        vec![base],
        parent_dir_names,
        module_system_info,
        &src_parts,
        require_path,
    )
}

fn from_of_source(
    module_system_info: &LspModuleSystemInfo,
    src_dir: Option<&str>,
    source: &export_index::Source,
) -> Option<String> {
    match source {
        export_index::Source::Global => None,
        export_index::Source::Builtin(from) => Some(from.display().to_string()),
        export_index::Source::FileKey(from) => {
            let module_name = match (module_system_info.get_haste_module_info)(from) {
                Some(haste_module_info) => Some(files::chop_platform_suffix_for_haste_module(
                    &module_system_info.file_options,
                    haste_module_info.module_name().as_str(),
                )),
                None if module_system_info.haste_module_system => src_dir.and_then(|src_dir| {
                    haste_package_path(
                        module_system_info,
                        src_dir,
                        files::chop_flow_ext(from).as_str(),
                    )
                }),
                None => None,
            };
            let node_resolver_dirnames =
                files::node_resolver_dirnames(&module_system_info.file_options).to_vec();
            let module_declaration_dirnames =
                files::module_declaration_dirnames(&module_system_info.file_options).to_vec();
            path_of_modulename(
                &node_resolver_dirnames,
                &module_system_info.node_resolver_root_relative_dirnames,
                &module_declaration_dirnames,
                &*module_system_info.get_package_info,
                &*module_system_info.resolves_to_real_path,
                src_dir,
                from,
                module_name,
            )
        }
    }
}

pub fn text_edits_of_import(
    layout_options: &js_layout_generator::Opts,
    module_system_info: &LspModuleSystemInfo,
    src_dir: Option<&str>,
    ast: &ast::Program<Loc, Loc>,
    kind: &export_index::Kind,
    name: &str,
    source: &export_index::Source,
) -> Option<CodeActionTextEdits> {
    let from = from_of_source(module_system_info, src_dir, source)?;
    // Hardcode React default import to namespace import
    let kind = if *kind == export_index::Kind::Default && name == "React" && from == "react" {
        export_index::Kind::Namespace
    } else {
        kind.clone()
    };
    let title = match kind {
        export_index::Kind::DefaultType => format!("Import default type from {from}"),
        export_index::Kind::Default => format!("Import default from {from}"),
        export_index::Kind::Named => format!("Import from {from}"),
        export_index::Kind::NamedType => format!("Import type from {from}"),
        export_index::Kind::Namespace => format!("Import * from {from}"),
    };
    let bindings = match kind {
        export_index::Kind::DefaultType => autofix_imports::Bindings::DefaultType(name.to_string()),
        export_index::Kind::Default => autofix_imports::Bindings::Default(name.to_string()),
        export_index::Kind::Named => {
            autofix_imports::Bindings::Named(vec![autofix_imports::NamedBinding {
                remote_name: name.to_string(),
                local_name: None,
            }])
        }
        export_index::Kind::NamedType => {
            autofix_imports::Bindings::NamedType(vec![autofix_imports::NamedBinding {
                remote_name: name.to_string(),
                local_name: None,
            }])
        }
        export_index::Kind::Namespace => autofix_imports::Bindings::Namespace(name.to_string()),
    };
    let edits = autofix_imports::add_import(layout_options, &bindings, &from, ast)
        .into_iter()
        .map(|(loc, text)| TextEdit {
            range: loc_to_lsp_range(loc),
            new_text: text,
        })
        .collect();
    Some(CodeActionTextEdits { title, edits, from })
}

pub mod for_tests {
    use flow_parser::file_key::FileKey;

    use super::path_of_modulename;

    pub fn path_of_modulename_for_tests(
        node_resolver_dirnames: &[String],
        node_resolver_root_relative_dirnames: &[(Option<String>, String)],
        module_declaration_dirnames: &[String],
        get_package_info: &dyn Fn(
            &FileKey,
        ) -> Option<
            Result<flow_parser_utils::package_json::PackageJson, ()>,
        >,
        resolves_to_real_path: &dyn Fn(&str, &str) -> bool,
        src_dir: Option<&str>,
        file_key: &FileKey,
        string_module_name: Option<String>,
    ) -> Option<String> {
        path_of_modulename(
            node_resolver_dirnames,
            node_resolver_root_relative_dirnames,
            module_declaration_dirnames,
            get_package_info,
            resolves_to_real_path,
            src_dir,
            file_key,
            string_module_name,
        )
    }
}
