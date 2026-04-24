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

pub fn json_to_multiline_at_indent(value: &serde_json::Value, indent: &str) -> String {
    let single = json_string_of_value(value);
    if single.len() < 80 {
        single
    } else {
        match value {
            serde_json::Value::Array(values) => {
                let next_indent = format!("{indent}  ");
                let rendered = values
                    .iter()
                    .map(|value| json_to_multiline_at_indent(value, &next_indent))
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
                            json_to_multiline_at_indent(value, &next_indent)
                        )
                    })
                    .collect::<Vec<_>>();
                format!("{{\n{}\n{indent}}}", rendered.join(",\n"))
            }
            _ => single,
        }
    }
}

pub fn json_to_multiline(value: &serde_json::Value) -> String {
    json_to_multiline_at_indent(value, "")
}

/// Re-format a compact JSON string into OCaml `Hh_json.json_to_multiline`
/// style, where each object/array is inlined when its compact form is
/// shorter than 80 characters and otherwise broken across multiple lines
/// at the given `indent`. Preserves duplicate keys (the input string is the
/// source of truth — this function only adds whitespace around structural
/// boundaries).
pub fn pretty_print_compact_at_indent(compact: &str, indent: &str) -> String {
    if compact.len() < 80 {
        return compact.to_string();
    }
    let bytes = compact.as_bytes();
    let Some(&first) = bytes.first() else {
        return String::new();
    };
    if first != b'{' && first != b'[' {
        return compact.to_string();
    }
    let inner = &compact[1..compact.len() - 1];
    let next_indent = format!("{indent}  ");
    let pieces = split_top_level_commas(inner);
    let parts: Vec<String> = if first == b'{' {
        pieces
            .iter()
            .map(|piece| {
                let (key, value) = split_at_top_level_colon(piece);
                format!(
                    "{next_indent}{key}:{}",
                    pretty_print_compact_at_indent(value, &next_indent)
                )
            })
            .collect()
    } else {
        pieces
            .iter()
            .map(|piece| {
                format!(
                    "{next_indent}{}",
                    pretty_print_compact_at_indent(piece, &next_indent)
                )
            })
            .collect()
    };
    let close = if first == b'{' { '}' } else { ']' };
    format!(
        "{}\n{}\n{indent}{close}",
        if first == b'{' { '{' } else { '[' },
        parts.join(",\n")
    )
}

fn split_top_level_commas(s: &str) -> Vec<&str> {
    let bytes = s.as_bytes();
    let mut result = Vec::new();
    let mut start = 0;
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if in_string {
            if c == b'\\' {
                i += 2;
                continue;
            } else if c == b'"' {
                in_string = false;
            }
        } else {
            match c {
                b'"' => in_string = true,
                b'{' | b'[' => depth += 1,
                b'}' | b']' => depth -= 1,
                b',' if depth == 0 => {
                    result.push(&s[start..i]);
                    start = i + 1;
                }
                _ => {}
            }
        }
        i += 1;
    }
    if start <= bytes.len() {
        result.push(&s[start..]);
    }
    result
}

fn split_at_top_level_colon(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if in_string {
            if c == b'\\' {
                i += 2;
                continue;
            } else if c == b'"' {
                in_string = false;
            }
        } else {
            match c {
                b'"' => in_string = true,
                b'{' | b'[' => depth += 1,
                b'}' | b']' => depth -= 1,
                b':' if depth == 0 => {
                    return (&s[..i], &s[i + 1..]);
                }
                _ => {}
            }
        }
        i += 1;
    }
    (s, "")
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
