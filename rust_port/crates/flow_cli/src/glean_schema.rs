/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde_json::Value;
use serde_json::json;

fn key(v: Value) -> Value {
    json!({"key": v})
}

pub(crate) mod src {
    pub(crate) mod file {
        use serde_json::Value;

        use super::super::key;

        pub(crate) type T = String;

        pub(crate) fn to_json(str: &str) -> Value {
            key(Value::String(str.to_string()))
        }
    }

    pub(crate) mod file_lines {
        use serde_json::Value;
        use serde_json::json;

        use super::super::key;
        use super::file;

        pub(crate) struct T {
            pub(crate) file: file::T,
            pub(crate) lengths: Vec<i32>,
            pub(crate) ends_in_newline: bool,
            pub(crate) has_unicode_or_tabs: bool,
        }

        pub(crate) fn to_json(
            T {
                file,
                lengths,
                ends_in_newline,
                has_unicode_or_tabs,
            }: &T,
        ) -> Value {
            key(json!({
                "file": file::to_json(file),
                "lengths": lengths.iter().map(|n| json!(*n)).collect::<Vec<Value>>(),
                "endsInNewline": ends_in_newline,
                "hasUnicodeOrTabs": has_unicode_or_tabs,
            }))
        }
    }

    pub(crate) mod byte_span {
        use serde_json::Value;
        use serde_json::json;

        pub(crate) struct T {
            pub(crate) start: i32,
            pub(crate) length: i32,
        }

        pub(crate) fn to_json(T { start, length }: &T) -> Value {
            json!({
                "start": start,
                "length": length,
            })
        }
    }
}

pub(crate) mod module_ {
    use flow_common_modulename::Modulename;
    use flow_parser::file_key::FileKey;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::src;

    #[derive(Clone)]
    pub(crate) enum T {
        File(src::file::T),
        Builtin,
        Lib(String),
        NoSource,
        String(String),
    }

    pub(crate) fn of_file_key(root: &str, write_root: &str, file_key: &FileKey) -> T {
        let str = flow_common::reason::string_of_source(Some(root), file_key);
        match str.as_str() {
            "(builtin)" => T::Builtin,
            s if s.len() >= 5 && &s[..5] == "[LIB]" => T::Lib(s[6..].to_string()),
            s => {
                let path = std::path::Path::new(write_root).join(s);
                T::File(path.to_string_lossy().to_string())
            }
        }
    }

    pub(crate) fn of_loc_source(root: &str, write_root: &str, source: Option<&FileKey>) -> T {
        match source {
            None => T::NoSource,
            Some(file_key) => of_file_key(root, write_root, file_key),
        }
    }

    pub(crate) fn of_modulename(root: &str, write_root: &str, modulename: &Modulename) -> T {
        match modulename {
            Modulename::Haste(haste_module_info) => {
                T::String(haste_module_info.module_name().as_str().to_string())
            }
            Modulename::Filename(file_key) => of_file_key(root, write_root, file_key),
        }
    }

    pub(crate) fn to_json(module_: &T) -> Value {
        let (k, v) = match module_ {
            T::File(file) => ("file", src::file::to_json(file)),
            T::Builtin => ("builtin", json!({})),
            T::Lib(lib) => ("lib", Value::String(lib.clone())),
            T::NoSource => ("noSource", json!({})),
            T::String(str) => ("string_", Value::String(str.clone())),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        key(Value::Object(map))
    }
}

pub(crate) mod file_of_string_module {
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::src;

    pub(crate) struct T {
        pub(crate) file: src::file::T,
        pub(crate) string: String,
    }

    pub(crate) fn to_json(T { file, string }: &T) -> Value {
        key(json!({
            "file": src::file::to_json(file),
            "string_": string,
        }))
    }
}

pub(crate) mod range {
    use flow_parser::loc::Loc;
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::module_;
    use super::src;

    pub(crate) type T = Loc;

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        Loc {
            source,
            start,
            end: _end,
        }: &Loc,
    ) -> Value {
        let span = match source.as_ref().and_then(offset_table_of_file_key) {
            None => src::byte_span::T {
                start: 0,
                length: 0,
            },
            Some(table) => {
                let start_offset = table.offset(*start).unwrap_or(0) as i32;
                let end_offset = table.offset(*_end).unwrap_or(0) as i32;
                let length = end_offset - start_offset;
                src::byte_span::T {
                    start: start_offset,
                    length,
                }
            }
        };
        key(json!({
            "module": module_::to_json(&module_::of_loc_source(root, write_root, source.as_ref())),
            "span": src::byte_span::to_json(&span),
        }))
    }
}

pub(crate) mod name {
    use serde_json::Value;

    use super::key;

    pub(crate) type T = String;

    pub(crate) fn to_json(name: &str) -> Value {
        key(Value::String(name.to_string()))
    }
}

pub(crate) mod type_ {
    use serde_json::Value;

    use super::key;

    pub(crate) type T = String;

    pub(crate) fn to_json(name: &str) -> Value {
        key(Value::String(name.to_string()))
    }
}

pub(crate) mod documentation {
    use flow_parser::loc::Loc;
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;

    use super::key;
    use super::range;

    pub(crate) type T = range::T;

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        loc: &Loc,
    ) -> Value {
        key(range::to_json(
            root,
            write_root,
            offset_table_of_file_key,
            loc,
        ))
    }
}

pub(crate) mod declaration {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::name;
    use super::range;

    #[derive(Clone)]
    pub(crate) struct T {
        pub(crate) name: name::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T { name: n, loc }: &T,
    ) -> Value {
        key(json!({
            "name": name::to_json(n),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod declaration_info {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;

    use super::declaration;
    use super::documentation;
    use super::key;
    use super::range;
    use super::type_;

    pub(crate) struct T {
        pub(crate) declaration: declaration::T,
        pub(crate) type_: type_::T,
        pub(crate) documentation: Option<documentation::T>,
        pub(crate) span: Option<range::T>,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            declaration: decl,
            type_: ty,
            documentation: doc,
            span,
        }: &T,
    ) -> Value {
        let mut fields: Vec<(String, Value)> = vec![
            (
                "declaration".to_string(),
                declaration::to_json(root, write_root, offset_table_of_file_key, decl),
            ),
            ("type".to_string(), type_::to_json(ty)),
        ];
        match doc {
            None => {}
            Some(loc) => {
                fields.push((
                    "documentation".to_string(),
                    documentation::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        match span {
            None => {}
            Some(loc) => {
                fields.push((
                    "span".to_string(),
                    range::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        key(Value::Object(fields.into_iter().collect()))
    }
}

pub(crate) mod local_declaration_reference {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::declaration;
    use super::key;
    use super::range;

    pub(crate) struct T {
        pub(crate) declaration: declaration::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            declaration: decl,
            loc,
        }: &T,
    ) -> Value {
        key(json!({
            "declaration": declaration::to_json(root, write_root, offset_table_of_file_key, decl),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod member_declaration {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::name;
    use super::range;

    #[derive(Clone)]
    pub(crate) struct T {
        pub(crate) name: name::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T { name: n, loc }: &T,
    ) -> Value {
        key(json!({
            "name": name::to_json(n),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod member_declaration_reference {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::member_declaration;
    use super::range;

    pub(crate) struct T {
        pub(crate) member_declaration: member_declaration::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            member_declaration: member_decl,
            loc,
        }: &T,
    ) -> Value {
        key(json!({
            "memberDeclaration": member_declaration::to_json(root, write_root, offset_table_of_file_key, member_decl),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod member_declaration_info {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;

    use super::documentation;
    use super::key;
    use super::member_declaration;
    use super::range;
    use super::type_;

    pub(crate) struct T {
        pub(crate) member_declaration: member_declaration::T,
        pub(crate) type_: type_::T,
        pub(crate) documentation: Option<documentation::T>,
        pub(crate) span: Option<range::T>,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            member_declaration: member_decl,
            type_: ty,
            documentation: doc,
            span,
        }: &T,
    ) -> Value {
        let mut fields: Vec<(String, Value)> = vec![
            (
                "memberDeclaration".to_string(),
                member_declaration::to_json(
                    root,
                    write_root,
                    offset_table_of_file_key,
                    member_decl,
                ),
            ),
            ("type".to_string(), type_::to_json(ty)),
        ];
        match doc {
            None => {}
            Some(loc) => {
                fields.push((
                    "documentation".to_string(),
                    documentation::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        match span {
            None => {}
            Some(loc) => {
                fields.push((
                    "span".to_string(),
                    range::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        key(Value::Object(fields.into_iter().collect()))
    }
}

pub(crate) mod module_doc {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::documentation;
    use super::key;
    use super::src;

    pub(crate) struct T {
        pub(crate) documentation: documentation::T,
        pub(crate) file: src::file::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            documentation: doc,
            file,
        }: &T,
    ) -> Value {
        key(json!({
            "file": src::file::to_json(file),
            "documentation": documentation::to_json(root, write_root, offset_table_of_file_key, doc),
        }))
    }
}

pub(crate) mod export {
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::module_;
    use super::name;

    #[derive(Clone)]
    pub(crate) enum T {
        CommonJS,
        CommonJSMember(name::T),
        Named(name::T),
        Default,
        Star(module_::T),
    }

    pub(crate) fn to_json(export: &T) -> Value {
        let (k, v) = match export {
            T::CommonJS => ("commonJS", json!({})),
            T::CommonJSMember(n) => ("commonJSMember", name::to_json(n)),
            T::Named(n) => ("named", name::to_json(n)),
            T::Default => ("default_", json!({})),
            T::Star(m) => ("star", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        key(Value::Object(map))
    }
}

pub(crate) mod module_export {
    use serde_json::Value;
    use serde_json::json;

    use super::export;
    use super::key;
    use super::module_;

    #[derive(Clone)]
    pub(crate) struct T {
        pub(crate) module_: module_::T,
        pub(crate) export: export::T,
    }

    pub(crate) fn to_json(
        T {
            module_: m,
            export: e,
        }: &T,
    ) -> Value {
        key(json!({
            "module": module_::to_json(m),
            "export_": export::to_json(e),
        }))
    }
}

pub(crate) mod import_declaration {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::declaration;
    use super::key;
    use super::module_;
    use super::module_export;

    #[derive(Clone)]
    pub(crate) enum Import {
        ModuleExport(module_export::T),
        ModuleNamespace(module_::T),
    }

    pub(crate) struct T {
        pub(crate) declaration: declaration::T,
        pub(crate) import: Import,
    }

    fn import_to_json(import: &Import) -> Value {
        let (k, v) = match import {
            Import::ModuleExport(module_export) => {
                ("moduleExport", module_export::to_json(module_export))
            }
            Import::ModuleNamespace(m) => ("moduleNamespace", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        Value::Object(map)
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            declaration: decl,
            import,
        }: &T,
    ) -> Value {
        key(json!({
            "declaration": declaration::to_json(root, write_root, offset_table_of_file_key, decl),
            "import_": import_to_json(import),
        }))
    }
}

pub(crate) mod source_of_export {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::declaration;
    use super::key;
    use super::member_declaration;
    use super::module_;
    use super::module_export;

    pub(crate) enum Source {
        Declaration(declaration::T),
        MemberDeclaration(member_declaration::T),
        ModuleExport(module_export::T),
        ModuleNamespace(module_::T),
    }

    pub(crate) struct T {
        pub(crate) module_export: module_export::T,
        pub(crate) source: Source,
    }

    fn source_to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        source: &Source,
    ) -> Value {
        let (k, v) = match source {
            Source::Declaration(decl) => (
                "declaration",
                declaration::to_json(root, write_root, offset_table_of_file_key, decl),
            ),
            Source::MemberDeclaration(member_decl) => (
                "memberDeclaration",
                member_declaration::to_json(
                    root,
                    write_root,
                    offset_table_of_file_key,
                    member_decl,
                ),
            ),
            Source::ModuleExport(module_export) => {
                ("moduleExport", module_export::to_json(module_export))
            }
            Source::ModuleNamespace(m) => ("moduleNamespace", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        Value::Object(map)
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            module_export,
            source,
        }: &T,
    ) -> Value {
        key(json!({
            "moduleExport": module_export::to_json(module_export),
            "source": source_to_json(root, write_root, offset_table_of_file_key, source),
        }))
    }
}

pub(crate) mod type_declaration {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::name;
    use super::range;

    #[derive(Clone)]
    pub(crate) struct T {
        pub(crate) name: name::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T { name: n, loc }: &T,
    ) -> Value {
        key(json!({
            "name": name::to_json(n),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod type_declaration_reference {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::range;
    use super::type_declaration;

    pub(crate) struct T {
        pub(crate) type_declaration: type_declaration::T,
        pub(crate) loc: range::T,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            type_declaration: type_decl,
            loc,
        }: &T,
    ) -> Value {
        key(json!({
            "typeDeclaration": type_declaration::to_json(root, write_root, offset_table_of_file_key, type_decl),
            "loc": range::to_json(root, write_root, offset_table_of_file_key, loc),
        }))
    }
}

pub(crate) mod type_declaration_info {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;

    use super::documentation;
    use super::key;
    use super::range;
    use super::type_;
    use super::type_declaration;

    pub(crate) struct T {
        pub(crate) type_declaration: type_declaration::T,
        pub(crate) type_: type_::T,
        pub(crate) documentation: Option<documentation::T>,
        pub(crate) span: Option<range::T>,
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            type_declaration: type_decl,
            type_: ty,
            documentation: doc,
            span,
        }: &T,
    ) -> Value {
        let mut fields: Vec<(String, Value)> = vec![
            (
                "typeDeclaration".to_string(),
                type_declaration::to_json(root, write_root, offset_table_of_file_key, type_decl),
            ),
            ("type".to_string(), type_::to_json(ty)),
        ];
        match doc {
            None => {}
            Some(loc) => {
                fields.push((
                    "documentation".to_string(),
                    documentation::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        match span {
            None => {}
            Some(loc) => {
                fields.push((
                    "span".to_string(),
                    range::to_json(root, write_root, offset_table_of_file_key, loc),
                ));
            }
        }
        key(Value::Object(fields.into_iter().collect()))
    }
}

pub(crate) mod type_export {
    use serde_json::Value;

    use super::key;
    use super::module_;
    use super::name;

    #[derive(Clone)]
    pub(crate) enum T {
        Named(name::T),
        Star(module_::T),
    }

    pub(crate) fn to_json(type_export: &T) -> Value {
        let (k, v) = match type_export {
            T::Named(n) => ("named", name::to_json(n)),
            T::Star(m) => ("star", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        key(Value::Object(map))
    }
}

pub(crate) mod module_type_export {
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::module_;
    use super::type_export;

    #[derive(Clone)]
    pub(crate) struct T {
        pub(crate) module_: module_::T,
        pub(crate) type_export: type_export::T,
    }

    pub(crate) fn to_json(
        T {
            module_: m,
            type_export: te,
        }: &T,
    ) -> Value {
        key(json!({
            "module": module_::to_json(m),
            "typeExport": type_export::to_json(te),
        }))
    }
}

pub(crate) mod type_import_declaration {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::module_;
    use super::module_export;
    use super::module_type_export;
    use super::type_declaration;

    #[derive(Clone)]
    pub(crate) enum Import {
        Type(module_type_export::T),
        Typeof(module_export::T),
        ModuleTypeof(module_::T),
    }

    pub(crate) struct T {
        pub(crate) type_declaration: type_declaration::T,
        pub(crate) import: Import,
    }

    fn import_to_json(import: &Import) -> Value {
        let (k, v) = match import {
            Import::Type(type_) => ("type", module_type_export::to_json(type_)),
            Import::Typeof(typeof_) => ("typeof_", module_export::to_json(typeof_)),
            Import::ModuleTypeof(m) => ("moduleTypeof", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        Value::Object(map)
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            type_declaration: type_decl,
            import,
        }: &T,
    ) -> Value {
        key(json!({
            "typeDeclaration": type_declaration::to_json(root, write_root, offset_table_of_file_key, type_decl),
            "import_": import_to_json(import),
        }))
    }
}

pub(crate) mod source_of_type_export {
    use flow_parser::offset_utils::OffsetTable;
    use serde_json::Value;
    use serde_json::json;

    use super::key;
    use super::module_;
    use super::module_type_export;
    use super::type_declaration;

    pub(crate) enum Source {
        TypeDeclaration(type_declaration::T),
        ModuleTypeExport(module_type_export::T),
        ModuleNamespace(module_::T),
    }

    pub(crate) struct T {
        pub(crate) module_type_export: module_type_export::T,
        pub(crate) source: Source,
    }

    fn source_to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        source: &Source,
    ) -> Value {
        let (k, v) = match source {
            Source::TypeDeclaration(type_decl) => (
                "typeDeclaration",
                type_declaration::to_json(root, write_root, offset_table_of_file_key, type_decl),
            ),
            Source::ModuleTypeExport(module_type_export) => (
                "moduleTypeExport",
                module_type_export::to_json(module_type_export),
            ),
            Source::ModuleNamespace(m) => ("moduleNamespace", module_::to_json(m)),
        };
        let mut map = serde_json::Map::new();
        map.insert(k.to_string(), v);
        Value::Object(map)
    }

    pub(crate) fn to_json(
        root: &str,
        write_root: &str,
        offset_table_of_file_key: &dyn Fn(&flow_parser::file_key::FileKey) -> Option<OffsetTable>,
        T {
            module_type_export,
            source,
        }: &T,
    ) -> Value {
        key(json!({
            "moduleTypeExport": module_type_export::to_json(module_type_export),
            "source": source_to_json(root, write_root, offset_table_of_file_key, source),
        }))
    }
}
