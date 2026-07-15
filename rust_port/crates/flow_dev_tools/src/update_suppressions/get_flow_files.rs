/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::path::Path;

use crate::utils::r#async::exec_manual;

pub(crate) fn normalize_flow_path_key(path: &str) -> String {
    path.trim_end_matches('\r').replace('\\', "/")
}

pub(crate) fn get_flow_files(
    bin: &str,
    root: &Path,
    flowconfig_name: &str,
) -> io::Result<Vec<String>> {
    let args = vec![
        "ls".to_string(),
        "--flowconfig-name".to_string(),
        flowconfig_name.to_string(),
    ];
    let output = exec_manual(bin, &args, Some(root), None)?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout)
            .lines()
            .map(normalize_flow_path_key)
            .filter(|file| !file.is_empty())
            .collect())
    } else {
        Err(io::Error::other(format!(
            "flow ls failed with status {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        )))
    }
}
