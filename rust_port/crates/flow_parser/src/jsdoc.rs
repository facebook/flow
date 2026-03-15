/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::ast;

pub mod param {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Optionality {
        NotOptional,
        Optional,
        OptionalWithDefault(String),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Info {
        pub description: Option<String>,
        pub optional: Optionality,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Path {
        Name,
        Element(Box<Path>),
        Member(Box<Path>, String),
    }

    pub type T = Vec<(Path, Info)>;
}

pub struct Params(pub Vec<(String, param::T)>);

pub struct UnrecognizedTags(pub Vec<(String, Option<String>)>);

pub struct Jsdoc {
    description: Option<String>,
    params: Params,
    deprecated: Option<String>,
    unrecognized_tags: UnrecognizedTags,
}

// *************
// * accessors *
// *************

impl Jsdoc {
    pub fn description(&self) -> &Option<String> {
        &self.description
    }

    pub fn params(&self) -> &Params {
        &self.params
    }

    pub fn deprecated(&self) -> Option<&String> {
        self.deprecated.as_ref()
    }

    pub fn unrecognized_tags(&self) -> &UnrecognizedTags {
        &self.unrecognized_tags
    }
}

// ***********
// * parsing *
// ***********

mod parser {
    use super::*;

    fn is_whitespace(c: char) -> bool {
        matches!(
            c,
            '\u{0009}'
                | '\u{000B}'
                | '\u{000C}'
                | '\u{0020}'
                | '\u{00A0}'
                | '\u{FEFF}'
                | '\u{1680}'
                | '\u{2000}'..='\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}'
        )
    }

    fn is_line_terminator(c: char) -> bool {
        matches!(c, '\n' | '\r' | '\u{2028}' | '\u{2029}')
    }

    fn is_identifier_char(c: char) -> bool {
        !c.is_whitespace() && !matches!(c, '[' | '.' | ']' | '=' | '{')
    }

    // Helpers

    fn empty() -> Jsdoc {
        Jsdoc {
            description: None,
            params: Params(vec![]),
            deprecated: None,
            unrecognized_tags: UnrecognizedTags(vec![]),
        }
    }

    fn trim_end(s: &str) -> &str {
        s.trim_end_matches([' ', '\x0C', '\n', '\r', '\t'])
    }

    fn trimmed_string_of_buffer(buffer: &str) -> &str {
        trim_end(buffer)
    }

    fn description_of_desc_buf(desc_buf: &str) -> Option<String> {
        let s = trimmed_string_of_buffer(desc_buf);
        if s.is_empty() {
            None
        } else {
            Some(s.to_owned())
        }
    }

    // like Base.List.Assoc.add, but maintains ordering differently:
    // - if k is already in the list, keeps it in that position and updates the value
    // - if k isn't in the list, adds it to the end
    fn add_assoc<K: PartialEq + Clone, V>(list: &mut Vec<(K, V)>, k: K, v: V) {
        for entry in list.iter_mut() {
            if entry.0 == k {
                entry.1 = v;
                return;
            }
        }
        list.push((k, v));
    }

    fn add_param(
        jsdoc: &mut Jsdoc,
        name: String,
        path: param::Path,
        description: Option<String>,
        optional: param::Optionality,
    ) {
        let old_param_infos: param::T = jsdoc
            .params
            .0
            .iter()
            .find(|(k, _)| k == &name)
            .map(|(_, v)| v.clone())
            .unwrap_or_default();
        let mut new_param_infos = old_param_infos;
        add_assoc(
            &mut new_param_infos,
            path,
            param::Info {
                description,
                optional,
            },
        );
        add_assoc(&mut jsdoc.params.0, name, new_param_infos);
    }

    fn add_unrecognized_tag(jsdoc: &mut Jsdoc, name: String, description: Option<String>) {
        jsdoc.unrecognized_tags.0.push((name, description));
    }

    // A char-by-char lexer state to mimic sedlex
    struct Lexer<'a> {
        chars: std::str::Chars<'a>,
        peeked: Option<Option<char>>,
    }

    impl<'a> Lexer<'a> {
        fn new(s: &'a str) -> Self {
            Lexer {
                chars: s.chars(),
                peeked: None,
            }
        }

        fn peek(&mut self) -> Option<char> {
            if self.peeked.is_none() {
                self.peeked = Some(self.chars.next());
            }
            self.peeked.unwrap()
        }

        fn next(&mut self) -> Option<char> {
            match self.peeked.take() {
                Some(c) => c,
                None => self.chars.next(),
            }
        }

        fn read_identifier(&mut self) -> String {
            let mut buf = String::new();
            while let Some(c) = self.peek() {
                if is_identifier_char(c) {
                    buf.push(c);
                    self.next();
                } else {
                    break;
                }
            }
            buf
        }
    }

    // Parsing functions

    // `description`, `description_or_tag`, and `description_startline` are
    // helpers for parsing descriptions: a description is a possibly-multiline
    // string terminated by EOF or a new tag. The beginning of each line could
    // contain whitespace and asterisks, which are stripped out when parsing.
    fn description(desc_buf: &mut String, lexer: &mut Lexer) -> Option<String> {
        loop {
            match lexer.peek() {
                Some(c) if is_line_terminator(c) => {
                    lexer.next();
                    desc_buf.push(c);
                    if c == '\r' {
                        if let Some('\n') = lexer.peek() {
                            lexer.next();
                            desc_buf.push('\n');
                        }
                    }
                    return description_startline(desc_buf, lexer);
                }
                Some(c) => {
                    lexer.next();
                    desc_buf.push(c);
                }
                None => {
                    return description_of_desc_buf(desc_buf);
                }
            }
        }
    }

    fn description_or_tag(desc_buf: &mut String, lexer: &mut Lexer) -> Option<String> {
        let mut acc = 0;
        loop {
            match lexer.peek() {
                Some(c) if is_whitespace(c) => {
                    lexer.next();
                    acc += 1;
                }
                Some('@') => {
                    return description_of_desc_buf(desc_buf);
                }
                _ => {
                    break;
                }
            }
        }
        for _ in 0..acc {
            desc_buf.push(' ');
        }
        description(desc_buf, lexer)
    }

    fn description_startline(desc_buf: &mut String, lexer: &mut Lexer) -> Option<String> {
        loop {
            match lexer.peek() {
                Some('*') => {
                    lexer.next();
                    return description_or_tag(desc_buf, lexer);
                }
                Some(c) if is_whitespace(c) => {
                    lexer.next();
                }
                _ => {
                    return description_or_tag(desc_buf, lexer);
                }
            }
        }
    }

    fn param_path(lexer: &mut Lexer, path: param::Path) -> param::Path {
        match lexer.peek() {
            Some('[') => {
                lexer.next();
                if let Some(']') = lexer.peek() {
                    lexer.next();
                    param_path(lexer, param::Path::Element(Box::new(path)))
                } else {
                    // Not "[]", just return what we have
                    path
                }
            }
            Some('.') => {
                lexer.next();
                let member = lexer.read_identifier();
                if member.is_empty() {
                    return path;
                }
                param_path(lexer, param::Path::Member(Box::new(path), member))
            }
            _ => path,
        }
    }

    fn skip_tag(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        loop {
            match lexer.peek() {
                Some('@') => {
                    return tag(jsdoc, lexer);
                }
                Some(_) => {
                    lexer.next();
                }
                None => return,
            }
        }
    }

    fn param_tag_description(
        jsdoc: &mut Jsdoc,
        name: String,
        path: param::Path,
        optional: param::Optionality,
        lexer: &mut Lexer,
    ) {
        let mut desc_buf = String::new();
        let desc = description(&mut desc_buf, lexer);
        add_param(jsdoc, name, path, desc, optional);
        tag(jsdoc, lexer);
    }

    fn param_tag_pre_description(
        jsdoc: &mut Jsdoc,
        name: String,
        path: param::Path,
        optional: param::Optionality,
        lexer: &mut Lexer,
    ) {
        loop {
            match lexer.peek() {
                Some(' ') => {
                    lexer.next();
                }
                Some('-') => {
                    lexer.next();
                    return param_tag_description(jsdoc, name, path, optional, lexer);
                }
                _ => {
                    return param_tag_description(jsdoc, name, path, optional, lexer);
                }
            }
        }
    }

    fn param_tag_optional_default(
        jsdoc: &mut Jsdoc,
        name: String,
        path: param::Path,
        lexer: &mut Lexer,
    ) {
        let mut def_buf = String::new();
        loop {
            match lexer.peek() {
                Some(']') => {
                    lexer.next();
                    return param_tag_pre_description(
                        jsdoc,
                        name,
                        path,
                        param::Optionality::OptionalWithDefault(def_buf),
                        lexer,
                    );
                }
                Some(c) => {
                    lexer.next();
                    def_buf.push(c);
                }
                None => {
                    return param_tag_pre_description(
                        jsdoc,
                        name,
                        path,
                        param::Optionality::OptionalWithDefault(def_buf),
                        lexer,
                    );
                }
            }
        }
    }

    fn param_tag_optional(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        match lexer.peek() {
            Some(c) if is_identifier_char(c) => {
                let name = lexer.read_identifier();
                let path = param_path(lexer, param::Path::Name);
                match lexer.peek() {
                    Some(']') => {
                        lexer.next();
                        param_tag_pre_description(
                            jsdoc,
                            name,
                            path,
                            param::Optionality::Optional,
                            lexer,
                        );
                    }
                    Some('=') => {
                        lexer.next();
                        param_tag_optional_default(jsdoc, name, path, lexer);
                    }
                    _ => {
                        param_tag_pre_description(
                            jsdoc,
                            name,
                            path,
                            param::Optionality::Optional,
                            lexer,
                        );
                    }
                }
            }
            _ => {
                skip_tag(jsdoc, lexer);
            }
        }
    }

    // ignore jsdoc type annotation
    fn param_tag_type(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        loop {
            match lexer.peek() {
                Some('}') => {
                    lexer.next();
                    return param_tag(jsdoc, lexer);
                }
                Some(_) => {
                    lexer.next();
                }
                None => return,
            }
        }
    }

    fn param_tag(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        loop {
            match lexer.peek() {
                Some(' ') => {
                    lexer.next();
                }
                Some('{') => {
                    lexer.next();
                    return param_tag_type(jsdoc, lexer);
                }
                Some('[') => {
                    lexer.next();
                    return param_tag_optional(jsdoc, lexer);
                }
                Some(c) if is_identifier_char(c) => {
                    let name = lexer.read_identifier();
                    let path = param_path(lexer, param::Path::Name);
                    return param_tag_pre_description(
                        jsdoc,
                        name,
                        path,
                        param::Optionality::NotOptional,
                        lexer,
                    );
                }
                _ => {
                    return skip_tag(jsdoc, lexer);
                }
            }
        }
    }

    fn description_tag(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        let mut desc_buf = String::new();
        let desc = description(&mut desc_buf, lexer);
        jsdoc.description = desc;
        tag(jsdoc, lexer);
    }

    fn deprecated_tag(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        let mut deprecated_tag_buf = String::new();
        let deprecated = Some(description(&mut deprecated_tag_buf, lexer).unwrap_or_default());
        jsdoc.deprecated = deprecated;
    }

    fn unrecognized_tag(jsdoc: &mut Jsdoc, name: String, lexer: &mut Lexer) {
        let mut desc_buf = String::new();
        let desc = description(&mut desc_buf, lexer);
        add_unrecognized_tag(jsdoc, name, desc);
        tag(jsdoc, lexer);
    }

    fn tag(jsdoc: &mut Jsdoc, lexer: &mut Lexer) {
        // The caller (description_or_tag) already detected '@' but didn't consume it.
        // We need to consume '@' first.
        match lexer.peek() {
            Some('@') => {
                lexer.next();
            }
            _ => {
                // No more tags
                return;
            }
        }
        let name = lexer.read_identifier();
        match name.as_str() {
            "param" | "arg" | "argument" => {
                param_tag(jsdoc, lexer);
            }
            "description" | "desc" => {
                description_tag(jsdoc, lexer);
            }
            "deprecated" => {
                deprecated_tag(jsdoc, lexer);
            }
            _ if !name.is_empty() => {
                unrecognized_tag(jsdoc, name, lexer);
            }
            _ => {
                skip_tag(jsdoc, lexer);
            }
        }
    }

    pub fn initial(text: &str) -> Option<Jsdoc> {
        let mut lexer = Lexer::new(text);
        match lexer.next() {
            Some('*') => {}
            _ => return None,
        }
        match lexer.peek() {
            Some('*') => return None,
            _ => {}
        }
        let mut desc_buf = String::new();
        let desc = description_or_tag(&mut desc_buf, &mut lexer);
        let mut jsdoc = empty();
        jsdoc.description = desc;
        tag(&mut jsdoc, &mut lexer);
        Some(jsdoc)
    }
}

pub fn parse(text: &str) -> Option<Jsdoc> {
    parser::initial(text)
}

/// find and parse the last jsdoc-containing comment in the list if exists
pub fn of_comments<M: Dupe, T: Dupe>(comments: Option<&ast::Syntax<M, T>>) -> Option<(M, Jsdoc)> {
    fn of_comment<M: Dupe>(comment: &ast::Comment<M>) -> Option<(M, Jsdoc)> {
        match comment.kind {
            ast::CommentKind::Block => parse(&comment.text).map(|d| (comment.loc.dupe(), d)),
            ast::CommentKind::Line => None,
        }
    }

    fn of_comment_list<M: Dupe>(comments: &[ast::Comment<M>]) -> Option<(M, Jsdoc)> {
        match comments {
            [] => None,
            [c, cs @ ..] => match of_comment_list(cs) {
                some @ Some(_) => some,
                None => of_comment(c),
            },
        }
    }

    let syntax = comments?;
    of_comment_list(&syntax.leading)
}
