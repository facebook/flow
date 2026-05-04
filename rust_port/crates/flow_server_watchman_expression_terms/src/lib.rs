/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::options::Options;
use flow_server_file_watcher_spec::file_watcher_spec;
use serde_json::Value;
use serde_json::json;

fn strlist(strs: &[&str]) -> Value {
    Value::Array(strs.iter().map(|s| json!(s)).collect())
}

fn pred(name: &str, terms: Vec<Value>) -> Value {
    let mut arr = vec![json!(name)];
    arr.extend(terms);
    Value::Array(arr)
}

fn assoc_strlist(key: &str, values: &[String]) -> Value {
    let mut arr: Vec<Value> = vec![json!(key)];
    for v in values {
        arr.push(json!(v));
    }
    Value::Array(arr)
}

pub fn file_name_terms(options: &Options) -> Value {
    let basenames = file_watcher_spec::get_file_names(options);
    assoc_strlist("name", &basenames)
}

pub fn make(options: &Options) -> Vec<Value> {
    let file_options = &options.file_options;
    let suffixes = file_watcher_spec::get_suffixes(file_options);
    let basenames = file_watcher_spec::get_file_names(options);
    vec![
        strlist(&["type", "f"]),
        pred(
            "anyof",
            vec![
                assoc_strlist("suffix", &suffixes),
                assoc_strlist("name", &basenames),
            ],
        ),
        pred(
            "not",
            vec![pred(
                "anyof",
                file_watcher_spec::EXCLUDE_DIRS
                    .iter()
                    .map(|dir| strlist(&["dirname", dir]))
                    .collect(),
            )],
        ),
    ]
}
