/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use regex::Regex;

static PERCENT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"%([0-9a-fA-F]?[0-9a-fA-F]?)").unwrap());
static DOS_URL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([a-zA-Z])[:|]([/\\].*)$").unwrap());
static URL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^file://([^/?#]*)/([^?#]*)(.*)$").unwrap());
static DOS_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([a-zA-Z]):([/\\].*)$").unwrap());

const PATH_SAFE_CHARS: &str =
    "/-._~!$&'()*+,;=@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

fn decode(s: &str) -> Result<String, String> {
    let mut err = None;
    let s = PERCENT_RE
        .replace_all(s, |caps: &regex::Captures<'_>| {
            let hex = &caps[1];
            if hex.len() != 2 {
                err = Some(format!("incorrect %-escape in {}", s));
                return String::new();
            }
            let code = i32::from_str_radix(hex, 16).unwrap();
            if !(32..=127).contains(&code) {
                err = Some(format!("only 7bit ascii allowed in {}", s));
                return String::new();
            }
            char::from_u32(code as u32).unwrap().to_string()
        })
        .into_owned();
    if let Some(err) = err {
        Err(err)
    } else if cfg!(windows) {
        Ok(s.replace('/', "\\"))
    } else {
        Ok(s)
    }
}

fn encode(safe_chars: &str, s: &str) -> String {
    let mut buf = String::with_capacity(s.len() * 2);
    for c in s.chars() {
        if cfg!(windows) && c == '\\' {
            buf.push('/');
        } else if safe_chars.contains(c) {
            buf.push(c);
        } else {
            let code = c as u32;
            if !(32..=127).contains(&code) {
                panic!("only 7bit ascii allowed in {}", s);
            }
            buf.push_str(&format!("%{code:02X}"));
        }
    }
    buf
}

pub fn parse(uri: &str) -> Result<String, String> {
    let caps = URL_RE
        .captures(uri)
        .ok_or_else(|| format!("not a file url - {}", uri))?;
    let host = caps.get(1).unwrap().as_str();
    let path = caps.get(2).unwrap().as_str();
    let query_fragment = caps.get(3).unwrap().as_str();
    let path = decode(path)?;
    if !host.is_empty() && host != "localhost" {
        return Err(format!("not localhost - {}", uri));
    }
    if !query_fragment.is_empty() {
        return Err(format!("file url can't have query/fragment - {}", uri));
    }
    if let Some(caps) = DOS_URL_RE.captures(&path) {
        let drive_letter = caps.get(1).unwrap().as_str().to_ascii_uppercase();
        let rest = caps.get(2).unwrap().as_str();
        Ok(format!("{}:{}", drive_letter, rest))
    } else if !path.is_empty() && path.as_bytes()[0] == b'/' {
        Err(format!("UNC file urls not supported - {}", uri))
    } else {
        Ok(format!("/{}", path))
    }
}

pub fn create(path: &str) -> String {
    let absolute_path = if let Some(caps) = DOS_RE.captures(path) {
        let drive_letter = caps.get(1).unwrap().as_str().to_ascii_lowercase();
        let rest = caps.get(2).unwrap().as_str();
        format!("{}:{}", drive_letter, rest)
    } else if let Some(rest) = path.strip_prefix('/') {
        rest.to_string()
    } else {
        panic!("Not an absolute filepath - {}", path);
    };
    format!("file:///{}", encode(PATH_SAFE_CHARS, &absolute_path))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_parse() {
        let examples = [
            ("file://localhost/etc/fstab", "/etc/fstab"),
            ("file:///etc/fstab", "/etc/fstab"),
            (
                "file://localhost/c:/WINDOWS/clock.avi",
                "C:/WINDOWS/clock.avi",
            ),
            ("file:///c:/WINDOWS/clock.avi", "C:/WINDOWS/clock.avi"),
            ("file:///c%3A/WINDOWS/clock.avi", "C:/WINDOWS/clock.avi"),
            ("file:///c|/WINDOWS/clock.avi", "C:/WINDOWS/clock.avi"),
            (
                "file://localhost/path/to/the%20file.txt",
                "/path/to/the file.txt",
            ),
            (
                "file:///c:/path/to/the%20file.txt",
                "C:/path/to/the file.txt",
            ),
            ("file:///C:/Program%20Files", "C:/Program Files"),
            ("file:///", "/"),
            ("file://localhost/", "/"),
            ("file:///c:", "/c:"),
            ("file:///wh/at!/ev%20/er", "/wh/at!/ev /er"),
            ("file:///fi%6Ce", "/file"),
            ("file:///fi%6ce", "/file"),
        ];

        for (uri, expected) in examples {
            assert_eq!(expected, parse(uri).unwrap());
        }
    }

    #[test]
    fn test_invalid_parse() {
        let examples = [
            "file",
            "file:",
            "file:/",
            "file://",
            "file:path/path",
            "file:/path/path",
            "file:c|/path",
            "file:/C:/config.sys",
            "file://C:/config.sys",
            "file://alpha.hut.fi/u/lai/tik/tik76002/public_html/lerman.files/chaps",
            "file:///C:/Program%20Files/Music/Web%20Sys/main.html?REQUEST=RADIO",
            "fool:///etc/hosts",
            "file://localhost/etc/%C3%B2.txt",
            "file:///c:/?",
            "file://///server/share/path",
            "file:////fileURLs/testof%23.txt",
            "fi+le:",
            "fi+le:///path",
            "file:///fi%6ge",
            "file:///fi%6/stuff",
            "file:///fi%6",
            "file:///fi%ge",
            "file:///fi%/stuff",
            "file:///fi%",
        ];

        for uri in examples {
            assert!(parse(uri).is_err(), "Expected '{}' not to parse", uri);
        }
    }

    #[test]
    fn test_create() {
        let examples = [
            (
                "c:\\autoexec.bat",
                if cfg!(windows) {
                    "file:///c%3A/autoexec.bat"
                } else {
                    "file:///c%3A%5Cautoexec.bat"
                },
            ),
            ("c:/autoexec.bat", "file:///c%3A/autoexec.bat"),
            (
                "c:\\\\autoexec.bat",
                if cfg!(windows) {
                    "file:///c%3A//autoexec.bat"
                } else {
                    "file:///c%3A%5C%5Cautoexec.bat"
                },
            ),
            ("c://autoexec.bat", "file:///c%3A//autoexec.bat"),
            ("/etc/dollar$dollar", "file:///etc/dollar$dollar"),
            ("/etc/hash#hash", "file:///etc/hash%23hash"),
            ("/etc/question?question", "file:///etc/question%3Fquestion"),
            (
                "/etc/braces{}/backtick`/caret^/space /file",
                "file:///etc/braces%7B%7D/backtick%60/caret%5E/space%20/file",
            ),
        ];

        for (file, expected) in examples {
            assert_eq!(expected, create(file));
        }
    }
}
