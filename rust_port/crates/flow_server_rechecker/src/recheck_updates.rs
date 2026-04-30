/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::path::Path;

use flow_common::files;
use flow_common::files::FileOptions;
use flow_common::flow_version;
use flow_common::options::Options;
use flow_common_exit_status::FlowExitStatus;
use flow_common_semver::semver;
use flow_config::FlowConfig;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parsing::parsing_service;
use flow_server_files::server_files_js;
use flow_services_module::PackageIncompatibleReturn;

pub enum Error {
    RecoverableShouldReinitNonLazily {
        msg: String,
        updates: FlowOrdSet<FileKey>,
    },
    Unrecoverable {
        msg: String,
        exit_status: FlowExitStatus,
    },
}

// WARNING! Be careful when adding new incompatibilities to this function. While dfind will
// return any file which changes within the watched directories, watchman only watches for
// specific extensions and files. Make sure to update the watchman_expression_terms in our
// watchman file watcher!
fn is_incompatible_package_json(
    options: &Options,
    shared_mem: &SharedMem,
    want: &dyn Fn(&str) -> bool,
    sroot: &str,
    file_options: &FileOptions,
    f: &str,
) -> PackageIncompatibleReturn {
    let is_incompatible = |filename_str: &str| -> PackageIncompatibleReturn {
        let filename = FileKey::json_file_of_absolute(filename_str);
        match std::fs::read_to_string(filename_str).ok() {
            None => PackageIncompatibleReturn::Incompatible(
                flow_services_module::PackageIncompatibleReason::Unknown,
            ),
            Some(content) => {
                let result = parsing_service::parse_package_json_file(
                    options,
                    Ok(content.as_str()),
                    &filename,
                )
                .map_err(|_| ());
                let old_package = shared_mem
                    .get_package_info(&filename)
                    .map(|pkg| Ok((*pkg).clone()));
                flow_services_module::package_incompatible(&filename, old_package, result)
            }
        }
    };
    if (f.starts_with(sroot) || files::is_included(file_options, f))
        && Path::new(f)
            .file_name()
            .is_some_and(|n| n == "package.json")
        && want(f)
    {
        is_incompatible(f)
    } else {
        PackageIncompatibleReturn::Compatible
    }
}

fn get_updated_flowconfig(config_path: &str) -> Result<(FlowConfig, String), Error> {
    match flow_config::get(config_path) {
        Ok((config, _warnings, hash)) => Ok((config, hash)),
        Err(_) => Err(Error::Unrecoverable {
            msg: "Config changed in an incompatible way".to_string(),
            exit_status: FlowExitStatus::FlowconfigChanged,
        }),
    }
}

fn assert_compatible_flowconfig_version(config: &FlowConfig) -> Result<(), Error> {
    let not_satisfied = |version_constraint: &str| -> bool {
        match semver::satisfies(Some(true), version_constraint, flow_version::VERSION) {
            Ok(result) => !result,
            Err(_) => true,
        }
    };
    match config.version.as_deref() {
        Some(version_constraint) if not_satisfied(version_constraint) => {
            let msg = format!(
                "Wrong version of Flow. The config specifies version {} but this is version {}",
                version_constraint,
                flow_version::VERSION,
            );
            Err(Error::Unrecoverable {
                msg,
                exit_status: FlowExitStatus::FlowconfigChanged,
            })
        }
        _ => Ok(()),
    }
}

fn assert_compatible_flowconfig_change(options: &Options, config_path: &str) -> Result<(), Error> {
    let old_hash = &options.flowconfig_hash;
    let (new_config, new_hash) = get_updated_flowconfig(config_path)?;
    if old_hash.as_str() == new_hash.as_str() {
        Ok(())
    } else {
        flow_hh_logger::error!(
            "Flowconfig hash changed from {:?} to {:?}",
            old_hash,
            new_hash
        );
        assert_compatible_flowconfig_version(&new_config)?;
        Err(Error::Unrecoverable {
            msg: "Config changed in an incompatible way".to_string(),
            exit_status: FlowExitStatus::FlowconfigChanged,
        })
    }
}

fn check_for_flowconfig_change(
    options: &Options,
    skip_incompatible: bool,
    config_path: &str,
    updates: &BTreeSet<String>,
) -> Result<(), Error> {
    let config_changed = !skip_incompatible && updates.contains(config_path);
    if !config_changed {
        Ok(())
    } else {
        assert_compatible_flowconfig_change(options, config_path)
    }
}

fn check_for_package_json_changes(
    is_incompatible_package_json: &dyn Fn(&str) -> PackageIncompatibleReturn,
    skip_incompatible: bool,
    updates: &BTreeSet<String>,
) -> Result<(), Error> {
    let incompatible_packages: Vec<(&String, flow_services_module::PackageIncompatibleReason)> =
        updates
            .iter()
            .filter_map(|file| match is_incompatible_package_json(file) {
                PackageIncompatibleReturn::Compatible => None,
                PackageIncompatibleReturn::Incompatible(reason) => Some((file, reason)),
            })
            .collect();
    if !skip_incompatible && !incompatible_packages.is_empty() {
        let messages: String = incompatible_packages
            .iter()
            .rev()
            .map(|(file, reason)| format!("Modified package: {} ({})", file, reason))
            .collect::<Vec<_>>()
            .join("\n");
        Err(Error::Unrecoverable {
            msg: format!("{}\nPackages changed in an incompatible way", messages),
            exit_status: FlowExitStatus::FlowconfigChanged,
        })
    } else {
        Ok(())
    }
}

fn did_content_change(shared_mem: &SharedMem, filename: &str) -> bool {
    let file = FileKey::lib_file_of_absolute(filename);
    match std::fs::read_to_string(filename).ok() {
        None => true,
        Some(content) => {
            !parsing_service::does_content_match_file_hash(shared_mem, &file, &content)
        }
    }
}

fn check_for_lib_changes(
    shared_mem: &SharedMem,
    all_libs: &BTreeSet<String>,
    root: &Path,
    skip_incompatible: bool,
    filter_wanted_updates: &dyn Fn(&BTreeSet<String>) -> FlowOrdSet<FileKey>,
    updates: &BTreeSet<String>,
) -> Result<(), Error> {
    let flow_typed_path = files::get_flowtyped_path(root)
        .to_string_lossy()
        .to_string();
    let is_changed_lib = |filename: &String| -> bool {
        let is_lib = all_libs.contains(filename) || *filename == flow_typed_path;
        is_lib && did_content_change(shared_mem, filename)
    };
    let libs: BTreeSet<String> = updates
        .iter()
        .filter(|f| is_changed_lib(f))
        .cloned()
        .collect();
    if !skip_incompatible && !libs.is_empty() {
        let messages: String = libs
            .iter()
            .rev()
            .map(|f| format!("Modified lib file: {}", f))
            .collect::<Vec<_>>()
            .join("\n");
        let updates = filter_wanted_updates(updates);
        Err(Error::RecoverableShouldReinitNonLazily {
            msg: format!("{}\nLib files changed in an incompatible way", messages),
            updates,
        })
    } else {
        Ok(())
    }
}

// Note: is_included may be expensive. Check the in-root match first.
// At this point, we already checked whether libdef files are touched.
// If so, we already decided that the server should restart, and this
// function won't be called.
fn filter_wanted_updates(
    file_options: &FileOptions,
    sroot: &str,
    want: &dyn Fn(&str) -> bool,
    updates: &BTreeSet<String>,
) -> FlowOrdSet<FileKey> {
    let empty_libs = BTreeSet::new();
    let mut acc = FlowOrdSet::new();
    for f in updates {
        if files::is_flow_file(file_options, f)
            && ((file_options.implicitly_include_root && f.starts_with(sroot))
                || files::is_included(file_options, f))
            && want(f)
        {
            let filename = files::filename_from_string(file_options, false, &empty_libs, f);
            acc.insert(filename);
        }
    }
    acc
}

pub fn process_updates(
    skip_incompatible: bool,
    options: &Options,
    previous_all_unordered_libs: &BTreeSet<FlowSmolStr>,
    shared_mem: &SharedMem,
    updates: &BTreeSet<String>,
) -> Result<FlowOrdSet<FileKey>, Error> {
    let file_options = &options.file_options;
    let all_libs = {
        let known_libs: BTreeSet<String> = previous_all_unordered_libs
            .iter()
            .map(|s| s.to_string())
            .collect();
        let (_ordered, maybe_new_libs) = files::ordered_and_unordered_lib_paths(file_options);
        let mut all = known_libs;
        for lib in &maybe_new_libs {
            all.insert(lib.clone());
        }
        all
    };
    let root = &*options.root;
    let config_path = server_files_js::config_file(&options.flowconfig_name, root);
    let sroot = format!("{}{}", root.to_string_lossy(), std::path::MAIN_SEPARATOR);
    let want = |f: &str| -> bool { files::wanted(file_options, false, &all_libs, f) };
    let do_filter = |updates: &BTreeSet<String>| -> FlowOrdSet<FileKey> {
        filter_wanted_updates(file_options, &sroot, &want, updates)
    };
    let is_incompatible_pj = |f: &str| -> PackageIncompatibleReturn {
        is_incompatible_package_json(options, shared_mem, &want, &sroot, file_options, f)
    };
    check_for_flowconfig_change(options, skip_incompatible, &config_path, updates)?;
    check_for_package_json_changes(&is_incompatible_pj, skip_incompatible, updates)?;
    check_for_lib_changes(
        shared_mem,
        &all_libs,
        root,
        skip_incompatible,
        &do_filter,
        updates,
    )?;
    Ok(do_filter(updates))
}
