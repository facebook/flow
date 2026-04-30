/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;

use flow_common::files;
use flow_common::files::FileOptions;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow ls (list files) command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "ls",
        "Lists files visible to Flow",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} ls [OPTION]... [FILE]...\n\nLists files visible to Flow\n",
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_ignore_version_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_ignore_flag(spec);
    let spec = command_utils::add_include_flag(spec);
    let spec = command_utils::add_untyped_flag(spec);
    let spec = command_utils::add_declaration_flag(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_json_flags(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = spec.flag(
        "--all",
        &arg_spec::truthy(),
        "Even list ignored files",
        None,
    );
    let spec = spec.flag(
        "--imaginary",
        &arg_spec::truthy(),
        "Even list non-existent specified files (normally they are silently dropped). Non-existent files are never considered to be libs.",
        None,
    );
    let spec = spec.flag(
        "--explain",
        &arg_spec::truthy(),
        "Output what kind of file each file is and why Flow cares about it",
        None,
    );
    let spec = command_utils::add_input_file_flag(spec, "ls");
    spec.anon("files or dirs", &arg_spec::list_of(arg_spec::string()))
}

enum FileResult {
    ImplicitlyIncluded,
    ExplicitlyIncluded,
    ImplicitlyIgnored,
    ExplicitlyIgnored(Option<String>),
    ImplicitLib,
    ExplicitLib,
    ConfigFile,
}

fn string_of_file_result(r: &FileResult) -> &'static str {
    match r {
        FileResult::ImplicitlyIncluded => "ImplicitlyIncluded",
        FileResult::ExplicitlyIncluded => "ExplicitlyIncluded",
        FileResult::ImplicitlyIgnored => "ImplicitlyIgnored",
        FileResult::ExplicitlyIgnored(_) => "ExplicitlyIgnored",
        FileResult::ImplicitLib => "ImplicitLib",
        FileResult::ExplicitLib => "ExplicitLib",
        FileResult::ConfigFile => "ConfigFile",
    }
}

fn string_of_file_result_with_padding(r: &FileResult) -> &'static str {
    match r {
        FileResult::ImplicitlyIncluded => "ImplicitlyIncluded",
        FileResult::ExplicitlyIncluded => "ExplicitlyIncluded",
        FileResult::ImplicitlyIgnored => "ImplicitlyIgnored ",
        FileResult::ExplicitlyIgnored(_) => "ExplicitlyIgnored ",
        FileResult::ImplicitLib => "ImplicitLib       ",
        FileResult::ExplicitLib => "ExplicitLib       ",
        FileResult::ConfigFile => "ConfigFile        ",
    }
}

fn explain(
    flowconfig_name: &str,
    root: &Path,
    options: &FileOptions,
    libs: &BTreeSet<String>,
    raw_file: &str,
) -> (String, FileResult) {
    let file = std::fs::canonicalize(raw_file)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| raw_file.to_string());
    let root_str = root.to_string_lossy().to_string();
    let result = {
        let (is_ignored, backup) = files::is_ignored(options, &file);
        if libs.contains(&file) {
            // This is a lib file
            let flowtyped_path = files::get_flowtyped_path(root);
            if file.starts_with(&flowtyped_path.to_string_lossy().to_string()) {
                FileResult::ImplicitLib
            } else {
                FileResult::ExplicitLib
            }
        } else if flow_server_files::server_files_js::config_file(flowconfig_name, root) == file {
            FileResult::ConfigFile
        } else if is_ignored {
            FileResult::ExplicitlyIgnored(backup)
        } else if file.starts_with(&root_str) {
            FileResult::ImplicitlyIncluded
        } else if files::is_included(options, &file) {
            FileResult::ExplicitlyIncluded
        } else {
            FileResult::ImplicitlyIgnored
        }
    };
    (raw_file.to_string(), result)
}

fn json_of_files_with_explanations(
    files_with_results: &[(String, FileResult)],
) -> serde_json::Value {
    let mut map = serde_json::Map::new();
    for (file, res) in files_with_results {
        let value = match res {
            FileResult::ExplicitlyIgnored(Some(backup)) => {
                serde_json::json!({
                    "explanation": string_of_file_result(res),
                    "backup": backup,
                })
            }
            _ => {
                serde_json::json!({
                    "explanation": string_of_file_result(res),
                })
            }
        };
        map.insert(file.clone(), value);
    }
    serde_json::Value::Object(map)
}

fn iter_get_next(f: &mut dyn FnMut(&str), get_next: &mut dyn FnMut() -> Vec<String>) {
    loop {
        let result = get_next();
        if result.is_empty() {
            break;
        }
        for item in &result {
            f(item);
        }
    }
}

fn make_options(
    flowconfig: &flow_config::FlowConfig,
    root: &Path,
    ignore_flag: Option<String>,
    include_flag: Option<String>,
    untyped_flag: Option<String>,
    declaration_flag: Option<String>,
) -> Arc<FileOptions> {
    if ignore_flag.is_none()
        && include_flag.is_none()
        && untyped_flag.is_none()
        && declaration_flag.is_none()
    {
        return command_utils::file_options_of_flowconfig(root, flowconfig);
    }

    let includes = command_utils::list_of_string_arg(include_flag);
    let ignores: Vec<(String, Option<String>)> = command_utils::list_of_string_arg(ignore_flag)
        .into_iter()
        .map(|ignore| (ignore, None))
        .collect();
    let untyped = command_utils::list_of_string_arg(untyped_flag);
    let declarations = command_utils::list_of_string_arg(declaration_flag);
    let libs: Vec<String> = vec![];
    let temp_dir = command_utils::get_temp_dir(&None);
    command_utils::file_options(
        flowconfig,
        root,
        true,
        Path::new(&temp_dir),
        ignores,
        includes,
        libs,
        untyped,
        declarations,
    )
}

// The problem with Files.wanted is that it says yes to everything except ignored files and libs.
// So implicitly ignored files (like files in another directory) pass the Files.wanted check
fn wanted(
    root: &Path,
    options: &FileOptions,
    all_unordered_libs: &BTreeSet<String>,
    file: &str,
) -> bool {
    files::wanted(options, false, all_unordered_libs, file) && {
        let root_str = format!("{}/", root.to_string_lossy());
        file.starts_with(&root_str) || files::is_included(options, file)
    }
}

// Directories will return a closure that returns every file under that
// directory. Individual files will return a closure that returns just that file
fn get_ls_files(
    root: &Path,
    all: bool,
    options: Arc<FileOptions>,
    all_unordered_libs: Arc<BTreeSet<String>>,
    node_modules_containers: &RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>,
    imaginary: bool,
    file_or_dir: Option<&str>,
) -> Box<dyn FnMut() -> Vec<String>> {
    match file_or_dir {
        None => {
            let mut all_files = Vec::new();
            files::make_next_files(
                root,
                None,
                all,
                true,
                options,
                false,
                all_unordered_libs,
                node_modules_containers,
                |chunk| {
                    for p in chunk {
                        all_files.push(p.to_string_lossy().to_string());
                    }
                },
            );
            let mut returned = false;
            Box::new(move || {
                if returned {
                    vec![]
                } else {
                    returned = true;
                    std::mem::take(&mut all_files)
                }
            })
        }
        Some(dir) if Path::new(dir).is_dir() => {
            let subdir = Path::new(dir).to_path_buf();
            let mut all_files = Vec::new();
            files::make_next_files(
                root,
                Some(&subdir),
                all,
                true,
                options,
                false,
                all_unordered_libs,
                node_modules_containers,
                |chunk| {
                    for p in chunk {
                        all_files.push(p.to_string_lossy().to_string());
                    }
                },
            );
            let mut returned = false;
            Box::new(move || {
                if returned {
                    vec![]
                } else {
                    returned = true;
                    std::mem::take(&mut all_files)
                }
            })
        }
        Some(file) => {
            if (Path::new(file).exists() || imaginary)
                // Make flow ls never report flowlib files
                && !files::is_in_flowlib(&options, file)
                && (all || wanted(root, &options, &all_unordered_libs, file))
            {
                let file = std::fs::canonicalize(file)
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| file.to_string());
                let mut returned = false;
                Box::new(move || {
                    if returned {
                        vec![]
                    } else {
                        returned = true;
                        vec![file.clone()]
                    }
                })
            } else {
                Box::new(Vec::new)
            }
        }
    }
}

// We have a list of get_next() functions. This combines them into a single
// get_next function
fn concat_get_next(
    mut get_nexts: Vec<Box<dyn FnMut() -> Vec<String>>>,
) -> Box<dyn FnMut() -> Vec<String>> {
    let mut index = 0;
    Box::new(move || {
        while index < get_nexts.len() {
            let result = get_nexts[index]();
            if result.is_empty() {
                index += 1;
            } else {
                return result;
            }
        }
        vec![]
    })
}

// Append a constant list of files to the get_next function
fn get_next_append_const(
    mut get_next: Box<dyn FnMut() -> Vec<String>>,
    items: Vec<String>,
) -> Box<dyn FnMut() -> Vec<String>> {
    let mut remaining = Some(items);
    Box::new(move || {
        let result = get_next();
        if !result.is_empty() {
            return result;
        }
        remaining.take().unwrap_or_default()
    })
}

fn main_impl(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let ignore_version = command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let ignore_flag =
        command_spec::get(args, "--ignore", &arg_spec::optional(arg_spec::string())).unwrap();
    let include_flag =
        command_spec::get(args, "--include", &arg_spec::optional(arg_spec::string())).unwrap();
    let untyped_flag =
        command_spec::get(args, "--untyped", &arg_spec::optional(arg_spec::string())).unwrap();
    let declaration_flag = command_spec::get(
        args,
        "--declaration",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let root_flag =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let json_flags = command_utils::get_json_flags(args);
    let json = json_flags.json;
    let pretty = json_flags.pretty;
    let all = command_spec::get(args, "--all", &arg_spec::truthy()).unwrap();
    let imaginary = command_spec::get(args, "--imaginary", &arg_spec::truthy()).unwrap();
    let reason = command_spec::get(args, "--explain", &arg_spec::truthy()).unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let root_or_files = command_spec::get(
        args,
        "files or dirs",
        &arg_spec::list_of(arg_spec::string()),
    )
    .unwrap()
    .unwrap_or_default();

    let files_or_dirs =
        command_utils::get_filenames_from_input(true, input_file.as_deref(), Some(&root_or_files));
    let root_hint: Option<String> = match &root_flag {
        Some(r) => Some(r.clone()),
        None => match files_or_dirs.first() {
            Some(first_file) => {
                // If the first_file doesn't exist or if we can't find a .flowconfig, we'll error. If
                // --strip-root is passed, we want the error to contain a relative path.
                let first_file = if strip_root {
                    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
                    files::relative_path(&cwd, first_file)
                } else {
                    first_file.to_string()
                };
                Some(first_file)
            }
            None => None,
        },
    };
    let root = command_utils::guess_root(&flowconfig_name, root_hint.as_deref());
    let config_path = root.join(&flowconfig_name);
    let config_path_str = config_path.to_string_lossy().to_string();
    let (flowconfig, warnings, _hash) =
        match flow_config::get_with_ignored_version(&config_path_str, ignore_version) {
            Ok(r) => r,
            Err(flow_config::Error(line, msg)) => {
                eprintln!(".flowconfig:{} {}", line, msg);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::InvalidFlowconfig,
                );
            }
        };
    if !ignore_version && !warnings.is_empty() {
        for flow_config::Warning(line, message) in &warnings {
            eprintln!(".flowconfig:{} {}", line, message);
        }
        std::process::exit(8);
    }
    let options = make_options(
        &flowconfig,
        &root,
        ignore_flag,
        include_flag,
        untyped_flag,
        declaration_flag,
    );
    let (_ordered_libs, all_unordered_libs) = files::ordered_and_unordered_lib_paths(&options);
    let all_unordered_libs = Arc::new(all_unordered_libs);

    // `flow ls` and `flow ls dir` will list out all the flow files. We want to include lib files, so
    // we pass in ~libs:SSet.empty, which means we won't filter out any lib files
    let empty_libs: Arc<BTreeSet<String>> = Arc::new(BTreeSet::new());
    let node_modules_containers: RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>> =
        RwLock::new(BTreeMap::new());

    let mut next_files: Box<dyn FnMut() -> Vec<String>> = if files_or_dirs.is_empty() {
        get_ls_files(
            &root,
            all,
            Arc::clone(&options),
            empty_libs.clone(),
            &node_modules_containers,
            imaginary,
            None,
        )
    } else {
        let get_nexts: Vec<Box<dyn FnMut() -> Vec<String>>> = files_or_dirs
            .iter()
            .map(|f| {
                get_ls_files(
                    &root,
                    all,
                    Arc::clone(&options),
                    empty_libs.clone(),
                    &node_modules_containers,
                    imaginary,
                    Some(f),
                )
            })
            .collect();
        concat_get_next(get_nexts)
    };
    let root_str = format!("{}/", root.to_string_lossy());
    let config_file_absolute =
        flow_server_files::server_files_js::config_file(&flowconfig_name, &root);
    let config_file_relative = files::relative_path(Path::new(&root_str), &config_file_absolute);
    let include_config_file = files_or_dirs.is_empty()
        || files_or_dirs
            .iter()
            .any(|f| f == &config_file_relative || root_str.starts_with(f.as_str()));
    if include_config_file {
        next_files = get_next_append_const(next_files, vec![config_file_absolute.clone()]);
    }
    let normalize_filename = |filename: &str| -> String {
        if !strip_root {
            filename.to_string()
        } else {
            files::relative_path(Path::new(&root_str), filename)
        }
    };
    if json || pretty {
        let all_files = files::get_all(&mut *next_files);
        let files: Vec<String> = all_files.into_iter().collect();

        let json_value = if reason {
            // Mapping may cause a stack overflow. To avoid that, we always use rev_map.
            // Since the amount of rev_maps we use is odd, we reverse the list once more
            // at the end
            let files_with_results: Vec<(String, FileResult)> = files
                .iter()
                .map(|f| {
                    let (file, result) =
                        explain(&flowconfig_name, &root, &options, &all_unordered_libs, f);
                    (normalize_filename(&file), result)
                })
                .collect();
            json_of_files_with_explanations(&files_with_results)
        } else {
            let arr: Vec<serde_json::Value> = files
                .iter()
                .map(|f| serde_json::Value::String(normalize_filename(f)))
                .collect();
            serde_json::Value::Array(arr)
        };

        if pretty {
            println!("{}", serde_json::to_string_pretty(&json_value).unwrap());
        } else {
            println!("{}", serde_json::to_string(&json_value).unwrap());
        }
    } else {
        if reason {
            iter_get_next(
                &mut |filename: &str| {
                    let (f, r) = explain(
                        &flowconfig_name,
                        &root,
                        &options,
                        &all_unordered_libs,
                        filename,
                    );
                    println!(
                        "{}    {}",
                        string_of_file_result_with_padding(&r),
                        normalize_filename(&f)
                    );
                },
                &mut *next_files,
            );
        } else {
            iter_get_next(
                &mut |filename: &str| {
                    println!("{}", normalize_filename(filename));
                },
                &mut *next_files,
            );
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main_impl)
}
