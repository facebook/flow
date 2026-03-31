/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::file_key::FileKey;
use regex::Regex;

fn normalize_filename_dir_sep(path: &str) -> String {
    if cfg!(windows) {
        path.replace('\\', "/")
    } else {
        path.to_string()
    }
}

/// Checks if Relay integration is enabled for a given file.
/// Returns true if the file path does not match any of the exclude patterns.
pub fn enabled_for_file(excludes: &[Regex], file: &FileKey) -> bool {
    let path = normalize_filename_dir_sep(&file.to_absolute());
    !excludes.iter().any(|r| r.is_match(&path))
}

/// Determines the module prefix for a file based on include patterns.
/// If a module prefix is provided and the file matches any include pattern,
/// returns Some(module_prefix), otherwise returns None.
pub fn module_prefix_for_file(
    includes: &[Regex],
    file: &FileKey,
    module_prefix: Option<&str>,
) -> Option<String> {
    match module_prefix {
        Some(prefix) => {
            let path = normalize_filename_dir_sep(&file.to_absolute());
            if includes.iter().any(|r| r.is_match(&path)) {
                Some(prefix.to_string())
            } else {
                None
            }
        }
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use flow_parser::file_key::FileKey;
    use flow_parser::file_key::FileKeyInner;

    use super::*;

    static INIT: Once = Once::new();
    fn init_roots() {
        INIT.call_once(|| {
            flow_parser::file_key::set_project_root("");
            flow_parser::file_key::set_flowlib_root("");
        });
    }

    #[test]
    fn test_normalize_filename_dir_sep() {
        // On non-Windows platforms, backslashes should be converted to forward slashes
        let path = "path\\to\\file.js";
        let normalized = normalize_filename_dir_sep(path);
        if cfg!(windows) {
            assert_eq!(normalized, "path/to/file.js");
        } else {
            assert_eq!(normalized, path);
        }
    }

    #[test]
    fn test_enabled_for_file_with_no_excludes() {
        init_roots();
        let excludes: Vec<Regex> = vec![];
        let file = FileKey::new(FileKeyInner::SourceFile("src/test.js".to_string()));
        assert!(enabled_for_file(&excludes, &file));
    }

    #[test]
    fn test_enabled_for_file_with_non_matching_exclude() {
        init_roots();
        let excludes = vec![Regex::new("^node_modules/").unwrap()];
        let file = FileKey::new(FileKeyInner::SourceFile("src/test.js".to_string()));
        assert!(enabled_for_file(&excludes, &file));
    }

    #[test]
    fn test_enabled_for_file_with_matching_exclude() {
        init_roots();
        let excludes = vec![Regex::new("^node_modules/").unwrap()];
        let file = FileKey::new(FileKeyInner::SourceFile("node_modules/test.js".to_string()));
        assert!(!enabled_for_file(&excludes, &file));
    }

    #[test]
    fn test_module_prefix_for_file_with_no_prefix() {
        init_roots();
        let includes = vec![Regex::new("^src/").unwrap()];
        let file = FileKey::new(FileKeyInner::SourceFile("src/test.js".to_string()));
        assert_eq!(module_prefix_for_file(&includes, &file, None), None);
    }

    #[test]
    fn test_module_prefix_for_file_with_matching_include() {
        init_roots();
        let includes = vec![Regex::new("^src/").unwrap()];
        let file = FileKey::new(FileKeyInner::SourceFile("src/test.js".to_string()));
        let result = module_prefix_for_file(&includes, &file, Some("MyPrefix"));
        assert_eq!(result, Some("MyPrefix".to_string()));
    }

    #[test]
    fn test_module_prefix_for_file_with_non_matching_include() {
        init_roots();
        let includes = vec![Regex::new("^src/").unwrap()];
        let file = FileKey::new(FileKeyInner::SourceFile("lib/test.js".to_string()));
        let result = module_prefix_for_file(&includes, &file, Some("MyPrefix"));
        assert_eq!(result, None);
    }

    #[test]
    fn test_module_prefix_for_file_with_multiple_includes() {
        init_roots();
        let includes = vec![
            Regex::new("^src/").unwrap(),
            Regex::new("^components/").unwrap(),
        ];
        let file1 = FileKey::new(FileKeyInner::SourceFile("src/test.js".to_string()));
        let file2 = FileKey::new(FileKeyInner::SourceFile("components/Button.js".to_string()));
        let file3 = FileKey::new(FileKeyInner::SourceFile("lib/utils.js".to_string()));

        assert_eq!(
            module_prefix_for_file(&includes, &file1, Some("Prefix")),
            Some("Prefix".to_string())
        );
        assert_eq!(
            module_prefix_for_file(&includes, &file2, Some("Prefix")),
            Some("Prefix".to_string())
        );
        assert_eq!(
            module_prefix_for_file(&includes, &file3, Some("Prefix")),
            None
        );
    }
}
