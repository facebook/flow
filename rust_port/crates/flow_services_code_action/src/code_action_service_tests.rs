/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/__tests__/code_action_service_tests.ml`

use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser_utils::package_json::PackageJson;
use flow_services_autocomplete::lsp_import_edits::for_tests::path_of_modulename_for_tests;

fn string_opt(o: Option<String>) -> String {
    match o {
        Some(x) => x,
        None => "(None)".to_string(),
    }
}

fn node_resolver_dirnames() -> Vec<String> {
    vec!["node_modules".to_string()]
}

fn module_declaration_dirnames() -> Vec<String> {
    vec!["/path/to/root/@flowtyped".to_string()]
}

fn node_resolver_root_relative_dirnames() -> Vec<(Option<String>, String)> {
    vec![
        (None, "/path/to/root/root_relative_for_all".to_string()),
        (
            Some("path/to/root/some".to_string()),
            "/path/to/root/root_relative_for_some".to_string(),
        ),
    ]
}

fn resolves_to_real_path(_from: &str, _to_real_path: &str) -> bool {
    true
}

fn with_package<F, R>(package_json_path: &str, pkg: PackageJson, f: F) -> R
where
    F: FnOnce(&dyn Fn(&FileKey) -> Option<Result<PackageJson, ()>>) -> R,
{
    let expected_key = FileKey::new(FileKeyInner::JsonFile(package_json_path.to_string()));
    let get_package_info = move |file_key: &FileKey| -> Option<Result<PackageJson, ()>> {
        if file_key.as_str() == expected_key.as_str() {
            Some(Ok(pkg.clone()))
        } else {
            None
        }
    };
    f(&get_package_info)
}

// get_package_info that always returns None (no packages registered)
fn no_package_info(_file_key: &FileKey) -> Option<Result<PackageJson, ()>> {
    None
}

//   >::: [

#[test]
fn removes_js_extension() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/foo/bar.js".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("./foo/bar".to_string())));
}

#[test]
fn removes_index_js() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/foo/index.js".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("./foo".to_string())));
}

#[test]
fn leaves_json_extension() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/foo/bar.json".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root"),
        &file_key,
        None,
    );
    assert_eq!(
        string_opt(path),
        string_opt(Some("./foo/bar.json".to_string()))
    );
}

#[test]
fn leaves_index_json_extension() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/foo/index.json".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root"),
        &file_key,
        None,
    );
    assert_eq!(
        string_opt(path),
        string_opt(Some("./foo/index.json".to_string()))
    );
}

#[test]
fn removes_node_modules_in_parent() {
    let pkg = PackageJson::create(None, None, None, false, None);
    with_package(
        "/path/to/root/a/node_modules/module/package.json",
        pkg,
        |get_package_info| {
            let file_key = FileKey::new(FileKeyInner::SourceFile(
                "/path/to/root/a/node_modules/module/index.js".to_string(),
            ));
            let path = path_of_modulename_for_tests(
                &node_resolver_dirnames(),
                &node_resolver_root_relative_dirnames(),
                &module_declaration_dirnames(),
                get_package_info,
                &resolves_to_real_path,
                Some("/path/to/root/a/b"),
                &file_key,
                None,
            );
            assert_eq!(string_opt(path), string_opt(Some("module".to_string())));
        },
    );
}

#[test]
fn removes_node_modules_in_self() {
    let pkg = PackageJson::create(None, None, None, false, None);
    with_package(
        "/path/to/root/a/node_modules/module/package.json",
        pkg,
        |get_package_info| {
            let file_key = FileKey::new(FileKeyInner::SourceFile(
                "/path/to/root/a/node_modules/module/index.js".to_string(),
            ));
            let path = path_of_modulename_for_tests(
                &node_resolver_dirnames(),
                &node_resolver_root_relative_dirnames(),
                &module_declaration_dirnames(),
                get_package_info,
                &resolves_to_real_path,
                Some("/path/to/root/a"),
                &file_key,
                None,
            );
            assert_eq!(string_opt(path), string_opt(Some("module".to_string())));
        },
    );
}

#[test]
fn does_not_remove_node_modules_in_child() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/a/b/node_modules/module/index.js".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/a"),
        &file_key,
        None,
    );
    assert_eq!(
        string_opt(path),
        string_opt(Some("./b/node_modules/module".to_string()))
    );
}

#[test]
fn does_not_remove_node_modules_in_cousin() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/a/b/node_modules/module/index.js".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/a/c"),
        &file_key,
        None,
    );
    assert_eq!(
        string_opt(path),
        string_opt(Some("../b/node_modules/module".to_string()))
    );
}

#[test]
fn supports_package_json_main() {
    let pkg = PackageJson::create(None, Some("main.js".into()), None, false, None);
    with_package(
        "/path/to/root/node_modules/pkg_with_main/package.json",
        pkg,
        |get_package_info| {
            let file_key = FileKey::new(FileKeyInner::SourceFile(
                "/path/to/root/node_modules/pkg_with_main/main.js".to_string(),
            ));
            let path = path_of_modulename_for_tests(
                &node_resolver_dirnames(),
                &node_resolver_root_relative_dirnames(),
                &module_declaration_dirnames(),
                get_package_info,
                &resolves_to_real_path,
                Some("/path/to/root/a/c"),
                &file_key,
                None,
            );
            assert_eq!(
                string_opt(path),
                string_opt(Some("pkg_with_main".to_string()))
            );
        },
    );
}

#[test]
fn supports_package_json_relative_main() {
    let pkg = PackageJson::create(None, Some("./main.js".into()), None, false, None);
    with_package(
        "/path/to/root/node_modules/pkg_with_relative_main/package.json",
        pkg,
        |get_package_info| {
            let file_key = FileKey::new(FileKeyInner::SourceFile(
                "/path/to/root/node_modules/pkg_with_relative_main/main.js".to_string(),
            ));
            let path = path_of_modulename_for_tests(
                &node_resolver_dirnames(),
                &node_resolver_root_relative_dirnames(),
                &module_declaration_dirnames(),
                get_package_info,
                &resolves_to_real_path,
                Some("/path/to/root/a/c"),
                &file_key,
                None,
            );
            assert_eq!(
                string_opt(path),
                string_opt(Some("pkg_with_relative_main".to_string()))
            );
        },
    );
}

#[test]
fn supports_package_json_nested_main() {
    let pkg = PackageJson::create(None, Some("dist/main.js".into()), None, false, None);
    with_package(
        "/path/to/root/node_modules/pkg_with_nested_main/package.json",
        pkg,
        |get_package_info| {
            let file_key = FileKey::new(FileKeyInner::SourceFile(
                "/path/to/root/node_modules/pkg_with_nested_main/dist/main.js".to_string(),
            ));
            let path = path_of_modulename_for_tests(
                &node_resolver_dirnames(),
                &node_resolver_root_relative_dirnames(),
                &module_declaration_dirnames(),
                get_package_info,
                &resolves_to_real_path,
                Some("/path/to/root/a/c"),
                &file_key,
                None,
            );
            assert_eq!(
                string_opt(path),
                string_opt(Some("pkg_with_nested_main".to_string()))
            );
        },
    );
}

#[test]
fn supports_root_relative_modules() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/root_relative_for_some/foo".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/some"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("foo".to_string())));

    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/root_relative_for_some/foo".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/none"),
        &file_key,
        None,
    );
    assert_eq!(
        string_opt(path),
        string_opt(Some("../root_relative_for_some/foo".to_string()))
    );

    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/root_relative_for_all/foo".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/some"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("foo".to_string())));

    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/root_relative_for_all/foo".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/none"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("foo".to_string())));
}

#[test]
fn supports_flowtyped_modules() {
    let file_key = FileKey::new(FileKeyInner::SourceFile(
        "/path/to/root/@flowtyped/foo".to_string(),
    ));
    let path = path_of_modulename_for_tests(
        &node_resolver_dirnames(),
        &node_resolver_root_relative_dirnames(),
        &module_declaration_dirnames(),
        &no_package_info,
        &resolves_to_real_path,
        Some("/path/to/root/a/c"),
        &file_key,
        None,
    );
    assert_eq!(string_opt(path), string_opt(Some("foo".to_string())));
}
