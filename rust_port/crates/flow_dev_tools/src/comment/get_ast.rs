/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;

use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::estree_translator;
use flow_parser::offset_utils::OffsetTable;
use serde_json::Value;

pub(crate) fn get_ast(code: &str) -> io::Result<Value> {
    let offset_table = OffsetTable::make(code);
    let (ast, errors) = flow_parser::parse_program_without_file(
        false,
        None,
        Some(PERMISSIVE_PARSE_OPTIONS),
        Ok(code),
    );
    let config = estree_translator::Config {
        include_locs: true,
        include_filename: true,
        offset_style: estree_translator::OffsetStyle::Utf8Bytes,
    };

    match estree_translator::program(&offset_table, &config, &ast) {
        Value::Object(mut params) => {
            let mut properties = serde_json::Map::new();
            properties.insert(
                "errors".to_string(),
                estree_translator::errors(
                    &offset_table,
                    config.include_filename,
                    config.offset_style,
                    &errors,
                ),
            );
            properties.insert("tokens".to_string(), Value::Array(Vec::new()));
            properties.append(&mut params);
            Ok(Value::Object(properties))
        }
        _ => Err(io::Error::other(
            "expected translated AST to be a JSON object",
        )),
    }
}
