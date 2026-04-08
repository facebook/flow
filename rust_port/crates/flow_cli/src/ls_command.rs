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
use crate::command_utils;

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
            let flowtyped_path = files::get_flowtyped_path(root);
            if file.starts_with(&flowtyped_path.to_string_lossy().to_string()) {
                FileResult::ImplicitLib
            } else {
                FileResult::ExplicitLib
            }
        } else if root.join(flowconfig_name).to_string_lossy() == file {
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

fn main_impl(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &command_spec::truthy()).unwrap();
    let root_flag = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let json = command_spec::get(args, "--json", &command_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &command_spec::truthy()).unwrap();
    let all = command_spec::get(args, "--all", &command_spec::truthy()).unwrap();
    let imaginary = command_spec::get(args, "--imaginary", &command_spec::truthy()).unwrap();
    let reason = command_spec::get(args, "--explain", &command_spec::truthy()).unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let root_or_files = command_spec::get(
        args,
        "files or dirs",
        &command_spec::list_of(command_spec::string()),
    )
    .unwrap();

    let files_or_dirs = command_utils::get_filenames_from_input_with_allow_imaginary(
        input_file.as_deref(),
        Some(&root_or_files),
        true,
    );

    let root_hint: Option<String> = match &root_flag {
        Some(r) => Some(r.clone()),
        None => match files_or_dirs.first() {
            Some(first_file) => {
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
    let (flowconfig, warnings, _hash) = match flow_config::get(&config_path_str) {
        Ok(r) => r,
        Err(flow_config::Error(line, msg)) => {
            eprintln!(".flowconfig:{} {}", line, msg);
            std::process::exit(1);
        }
    };
    if !ignore_version && !warnings.is_empty() {
        for flow_config::Warning(line, message) in &warnings {
            eprintln!(".flowconfig:{} {}", line, message);
        }
        std::process::exit(8);
    }

    let options = crate::command_utils::make_options(
        flowconfig,
        _hash,
        flowconfig_name.clone(),
        root.clone(),
        std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned()),
        true, // no_flowlib for ls command
        crate::command_utils::MakeOptionsOverrides::default(),
    );
    let options = Arc::new(options);

    let (_ordered_libs, all_unordered_libs) =
        files::ordered_and_unordered_lib_paths(&options.file_options);
    let all_unordered_libs = Arc::new(all_unordered_libs);

    let empty_libs: Arc<BTreeSet<String>> = Arc::new(BTreeSet::new());
    let node_modules_containers: RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>> =
        RwLock::new(BTreeMap::new());

    let mut next_files: Box<dyn FnMut() -> Vec<String>> = if files_or_dirs.is_empty() {
        get_ls_files(
            &root,
            all,
            options.file_options.clone(),
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
                    options.file_options.clone(),
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
    let config_file_absolute = root.join(&flowconfig_name).to_string_lossy().to_string();
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
            let files_with_results: Vec<(String, FileResult)> = files
                .iter()
                .map(|f| {
                    let (file, result) = explain(
                        &flowconfig_name,
                        &root,
                        &options.file_options,
                        &all_unordered_libs,
                        f,
                    );
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
                        &options.file_options,
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

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "ls",
        "Lists files visible to Flow",
        "Usage: flow ls [OPTION]... [FILES/DIRS]\n\nLists files visible to Flow".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--ignore-version",
        &command_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
    .flag(
        "--strip-root",
        &command_spec::truthy(),
        "Print paths without the root",
        None,
    )
    .flag(
        "--root",
        &command_spec::optional(command_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
    .flag(
        "--json",
        &command_spec::truthy(),
        "Output results in JSON format",
        None,
    )
    .flag(
        "--pretty",
        &command_spec::truthy(),
        "Pretty-print JSON output (implies --json)",
        None,
    )
    .flag("--all", &command_spec::truthy(), "Even list ignored files", None)
    .flag(
        "--imaginary",
        &command_spec::truthy(),
        "Treat missing paths from input as imaginary files",
        None,
    )
    .flag(
        "--explain",
        &command_spec::truthy(),
        "Explain why each file is or is not visible to Flow",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .flag(
        "--input-file",
        &command_spec::optional(command_spec::string()),
        "File containing list of files to ls, one per line. If -, list of files is read from the standard input.",
        None,
    )
    .anon(
        "files or dirs",
        &command_spec::list_of(command_spec::string()),
    )
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main_impl)
}
