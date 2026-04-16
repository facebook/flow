/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub fn json_string_of_value(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::Array(values) => format!(
            "[{}]",
            values
                .iter()
                .map(json_string_of_value)
                .collect::<Vec<_>>()
                .join(",")
        ),
        serde_json::Value::Object(props) => format!(
            "{{{}}}",
            props
                .iter()
                .map(|(key, value)| format!(
                    "{}:{}",
                    serde_json::to_string(key).unwrap(),
                    json_string_of_value(value)
                ))
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => serde_json::to_string(value).unwrap(),
    }
}

pub fn json_to_multiline(value: &serde_json::Value) -> String {
    fn loop_(indent: &str, value: &serde_json::Value) -> String {
        let single = json_string_of_value(value);
        if single.len() < 80 {
            single
        } else {
            match value {
                serde_json::Value::Array(values) => {
                    let next_indent = format!("{indent}  ");
                    let rendered = values
                        .iter()
                        .map(|value| loop_(&next_indent, value))
                        .collect::<Vec<_>>();
                    format!(
                        "[\n{next_indent}{}\n{indent}]",
                        rendered.join(&format!(",\n{next_indent}"))
                    )
                }
                serde_json::Value::Object(props) => {
                    let next_indent = format!("{indent}  ");
                    let rendered = props
                        .iter()
                        .map(|(key, value)| {
                            format!(
                                "{next_indent}{}:{}",
                                serde_json::to_string(key).unwrap(),
                                loop_(&next_indent, value)
                            )
                        })
                        .collect::<Vec<_>>();
                    format!("{{\n{}\n{indent}}}", rendered.join(",\n"))
                }
                _ => single,
            }
        }
    }

    loop_("", value)
}

pub fn print_json_endline(pretty: bool, value: &serde_json::Value) {
    if pretty {
        println!("{}", json_to_multiline(value));
    } else {
        println!("{}", json_string_of_value(value));
    }
}

pub fn prerr_json_endline(pretty: bool, value: &serde_json::Value) {
    if pretty {
        eprintln!("{}", json_to_multiline(value));
    } else {
        eprintln!("{}", json_string_of_value(value));
    }
}
