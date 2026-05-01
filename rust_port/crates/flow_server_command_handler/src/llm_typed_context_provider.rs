/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use flow_aloc::ALoc;
use flow_common::reason;
use flow_common_ty::ty;
use flow_common_ty::ty::Elt;
use flow_common_ty::ty_printer;
use flow_common_ty::ty_printer::PrinterOptions;
use flow_common_ty::ty_symbol::Symbol;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::ast;
use flow_parser::ast::statement::StatementInner;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::FileSigOptions;
use flow_parser_utils::file_sig::Require;
use flow_server_env::lsp_mapper::llm_context;
use flow_typing::ty_normalizer_flow;
use flow_typing_context::Context;
use flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode;
use flow_typing_ty_normalizer::env::Options as NormalizerOptions;
use flow_typing_type::type_::Type;

pub fn count_tokens(s: &str) -> i32 {
    (s.len() as f64 / 3.5).ceil() as i32
}

fn name_of_typed_identifier(id: &ast::Identifier<ALoc, (ALoc, Type)>) -> &str {
    &id.name
}

fn name_of_typed_pattern(pattern: &ast::pattern::Pattern<ALoc, (ALoc, Type)>) -> Option<&str> {
    match pattern {
        ast::pattern::Pattern::Identifier { inner, .. } => {
            Some(name_of_typed_identifier(&inner.name))
        }
        _ => None,
    }
}

fn normalize_type(
    cx: &Context<'_>,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    shared_mem: &SharedMem,
    t: &Type,
) -> (String, Option<Vec<(String, Loc)>>) {
    let options = NormalizerOptions {
        expand_internal_types: false,
        expand_enum_members: false,
        evaluate_type_destructors: EvaluateTypeDestructorsMode::EvaluateNone,
        optimize_types: true,
        omit_targ_defaults_option: false,
        merge_bot_and_any_kinds: true,
        verbose_normalizer: false,
        max_depth: Some(10),
        toplevel_is_type_identifier_reference: false,
    };
    let genv = ty_normalizer_flow::mk_genv(options, cx, Some(typed_ast), file_sig.clone());
    match ty_normalizer_flow::from_type(&genv, t) {
        Ok(elt) => {
            let loc_of_aloc = |aloc: &ALoc| shared_mem.loc_of_aloc(aloc);
            let refs: BTreeSet<Symbol<Loc>> = ty::symbols_of_elt(loc_of_aloc, &elt);
            let refs_some = Some(refs);
            let opts = PrinterOptions {
                exact_by_default: true,
                ts_syntax: false,
                ..Default::default()
            };
            let (type_str, refs) = ty_printer::string_of_type_at_pos_result::<Loc>(
                &elt,
                &None::<Elt<ALoc>>,
                &refs_some,
                &opts,
            );
            (type_str, refs)
        }
        Err(_) => ("<unknown>".to_string(), None),
    }
}

fn format_refs(strip_root: Option<&str>, refs: &Option<Vec<(String, Loc)>>) -> String {
    match refs {
        None => String::new(),
        Some(refs_list) => {
            if refs_list.is_empty() {
                return String::new();
            }
            let ref_strs: Vec<String> = refs_list
                .iter()
                .filter_map(|(name, loc)| match &loc.source {
                    Some(_) => {
                        let loc_str = reason::string_of_loc(strip_root, loc);
                        Some(format!("'{}' is defined at {}", name, loc_str))
                    }
                    None => None,
                })
                .collect();
            if ref_strs.is_empty() {
                String::new()
            } else {
                format!("\nwhere\n{}", ref_strs.join("\n"))
            }
        }
    }
}

struct ContextExtractor<'a, 'cx> {
    strip_root: Option<&'a str>,
    cx: &'a Context<'cx>,
    file_sig: &'a Arc<FileSig>,
    typed_ast: &'a ast::Program<ALoc, (ALoc, Type)>,
    shared_mem: &'a SharedMem,
    exports: Vec<String>,
}

impl<'a, 'cx> ContextExtractor<'a, 'cx> {
    fn new(
        strip_root: Option<&'a str>,
        cx: &'a Context<'cx>,
        file_sig: &'a Arc<FileSig>,
        typed_ast: &'a ast::Program<ALoc, (ALoc, Type)>,
        shared_mem: &'a SharedMem,
    ) -> Self {
        Self {
            strip_root,
            cx,
            file_sig,
            typed_ast,
            shared_mem,
            exports: Vec::new(),
        }
    }

    fn get_exports(self) -> Vec<String> {
        self.exports
    }

    fn add_export(&mut self, s: String) {
        self.exports.push(s);
    }

    fn extract_variables(
        &mut self,
        kind: &ast::VariableKind,
        decls: &[ast::statement::variable::Declarator<ALoc, (ALoc, Type)>],
    ) {
        let kind_str = match kind {
            ast::VariableKind::Var => "var",
            ast::VariableKind::Let => "let",
            ast::VariableKind::Const => "const",
        };
        for decl in decls {
            if let Some(name) = name_of_typed_pattern(&decl.id) {
                self.add_export(format!("export {} {}", kind_str, name));
            }
        }
    }

    fn visit_export_named_declaration(
        &mut self,
        decl: &ast::statement::ExportNamedDeclaration<ALoc, (ALoc, Type)>,
    ) {
        match &decl.declaration {
            Some(stmt) => match &**stmt {
                StatementInner::VariableDeclaration { inner, .. } => {
                    self.extract_variables(&inner.kind, &inner.declarations);
                }
                StatementInner::FunctionDeclaration { inner, .. } => {
                    if let Some(id) = &inner.id {
                        let func_type = &id.loc.1;
                        let name = &id.name;
                        let (type_str, refs) = normalize_type(
                            self.cx,
                            self.file_sig,
                            self.typed_ast,
                            self.shared_mem,
                            func_type,
                        );
                        let refs_str = format_refs(self.strip_root, &refs);
                        self.add_export(format!(
                            "export function {}: {}{}",
                            name, type_str, refs_str
                        ));
                    }
                }
                StatementInner::ClassDeclaration { inner, .. } => {
                    if let Some(id) = &inner.id {
                        let name = name_of_typed_identifier(id);
                        self.add_export(format!("export class {}", name));
                    }
                }
                StatementInner::ComponentDeclaration { inner, .. } => {
                    let component_type = &inner.id.loc.1;
                    let name = &inner.id.name;
                    let (type_str, refs) = normalize_type(
                        self.cx,
                        self.file_sig,
                        self.typed_ast,
                        self.shared_mem,
                        component_type,
                    );
                    let refs_str = format_refs(self.strip_root, &refs);
                    self.add_export(format!(
                        "export component {}: {}{}",
                        name, type_str, refs_str
                    ));
                }
                StatementInner::TypeAlias { inner, .. } => {
                    let name = name_of_typed_identifier(&inner.id);
                    self.add_export(format!("export type {} = ...", name));
                }
                StatementInner::InterfaceDeclaration { inner, .. } => {
                    let name = name_of_typed_identifier(&inner.id);
                    self.add_export(format!("export interface {}", name));
                }
                StatementInner::EnumDeclaration { inner, .. } => {
                    let name = name_of_typed_identifier(&inner.id);
                    self.add_export(format!("export enum {}", name));
                }
                _ => {}
            },
            None => {}
        }
    }

    fn visit_export_default_declaration(
        &mut self,
        decl: &ast::statement::ExportDefaultDeclaration<ALoc, (ALoc, Type)>,
    ) {
        match &decl.declaration {
            ast::statement::export_default_declaration::Declaration::Declaration(stmt) => {
                match &**stmt {
                    StatementInner::ComponentDeclaration { inner, .. } => {
                        let component_type = &inner.id.loc.1;
                        let name = &inner.id.name;
                        let (type_str, refs) = normalize_type(
                            self.cx,
                            self.file_sig,
                            self.typed_ast,
                            self.shared_mem,
                            component_type,
                        );
                        let refs_str = format_refs(self.strip_root, &refs);
                        self.add_export(format!(
                            "export default component {}: {}{}",
                            name, type_str, refs_str
                        ));
                    }
                    StatementInner::FunctionDeclaration { inner, .. } => match &inner.id {
                        Some(id) => {
                            let func_type = &id.loc.1;
                            let name = &id.name;
                            let (type_str, refs) = normalize_type(
                                self.cx,
                                self.file_sig,
                                self.typed_ast,
                                self.shared_mem,
                                func_type,
                            );
                            let refs_str = format_refs(self.strip_root, &refs);
                            self.add_export(format!(
                                "export default function {}: {}{}",
                                name, type_str, refs_str
                            ));
                        }
                        None => {
                            self.add_export("export default function(...)".to_string());
                        }
                    },
                    StatementInner::ClassDeclaration { inner, .. } => match &inner.id {
                        Some(id) => {
                            let name = name_of_typed_identifier(id);
                            self.add_export(format!("export default class {}", name));
                        }
                        None => {
                            self.add_export("export default class".to_string());
                        }
                    },
                    _ => {
                        self.add_export("export default ...".to_string());
                    }
                }
            }
            ast::statement::export_default_declaration::Declaration::Expression(_) => {
                self.add_export("export default ...".to_string());
            }
        }
    }

    fn visit_program(&mut self) {
        for stmt in self.typed_ast.statements.iter() {
            match &**stmt {
                StatementInner::ExportNamedDeclaration { inner, .. } => {
                    self.visit_export_named_declaration(inner);
                }
                StatementInner::ExportDefaultDeclaration { inner, .. } => {
                    self.visit_export_default_declaration(inner);
                }
                _ => {}
            }
        }
    }
}

pub fn extract_imports(
    file_key: &FileKey,
    ast: &ast::Program<Loc, Loc>,
    opts: &FileSigOptions,
) -> Vec<String> {
    let file_sig = FileSig::from_program(file_key, ast, opts);
    let requires = file_sig.requires();
    requires
        .iter()
        .rev()
        .filter_map(|require| match require {
            Require::Require { source, .. } => Some(format!("require('{}')", source.1)),
            Require::Import { source, .. } | Require::Import0 { source } => {
                Some(format!("import ... from '{}'", source.1))
            }
            Require::ImportDynamic { source, .. } => Some(format!("import('{}')", source.1)),
            Require::ExportFrom { source } => Some(format!("export ... from '{}'", source.1)),
            Require::ImportSyntheticUserland { .. } | Require::ImportSyntheticHaste { .. } => None,
        })
        .collect()
}

pub fn extract_declarations(
    strip_root: Option<&str>,
    cx: &Context<'_>,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    shared_mem: &SharedMem,
) -> Vec<String> {
    let mut extractor = ContextExtractor::new(strip_root, cx, file_sig, typed_ast, shared_mem);
    extractor.visit_program();
    extractor.get_exports()
}

pub fn format_file_context(
    strip_root: Option<&str>,
    file_key: &FileKey,
    imports: &[String],
    declarations: &[String],
) -> String {
    let file_path = reason::string_of_source(strip_root, file_key);
    let mut buf = String::with_capacity(256);
    buf.push_str(&format!("=== File: {} ===\n\n", file_path));
    if !imports.is_empty() {
        buf.push_str("Imports:\n");
        for imp in imports {
            buf.push_str(&format!("  {}\n", imp));
        }
        buf.push('\n');
    }
    if !declarations.is_empty() {
        buf.push_str("Declarations:\n");
        for decl in declarations {
            buf.push_str(&format!("  {}\n", decl));
        }
    }
    buf
}

pub fn generate_file_context(
    strip_root: Option<&str>,
    file_key: &FileKey,
    ast: &ast::Program<Loc, Loc>,
    cx: &Context<'_>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    shared_mem: &SharedMem,
    opts: &FileSigOptions,
) -> String {
    let imports = extract_imports(file_key, ast, opts);
    let file_sig = Arc::new(FileSig::from_program(file_key, ast, opts));
    let declarations = extract_declarations(strip_root, cx, &file_sig, typed_ast, shared_mem);
    format_file_context(strip_root, file_key, &imports, &declarations)
}

pub struct FileContext {
    pub path: String,
    pub context: String,
    pub tokens: i32,
}

pub struct TypedFileInfo<'a, 'cx> {
    pub file_key: FileKey,
    pub ast: ast::Program<Loc, Loc>,
    pub cx: &'a Context<'cx>,
    pub typed_ast: ast::Program<ALoc, (ALoc, Type)>,
    pub shared_mem: &'a SharedMem,
}

pub fn legacy_syntax_header() -> String {
    [
        "IMPORTANT:",
        "The type `$ReadOnly<T>` is deprecated, use `Readonly<T>` instead.",
        "The type `$ReadOnlyArray<T>` is deprecated, use `ReadonlyArray<T>` instead.",
        "The type `mixed` is deprecated, use `unknown` instead.",
    ]
    .join("\n")
}

pub fn generate_context<'a, 'cx>(
    strip_root: Option<&str>,
    files: &'a [TypedFileInfo<'a, 'cx>],
    token_budget: i32,
    include_imports: bool,
    file_sig_opts: &FileSigOptions,
) -> llm_context::Result {
    let file_contexts: Vec<FileContext> = if include_imports {
        files
            .iter()
            .map(|info| {
                let context = generate_file_context(
                    strip_root,
                    &info.file_key,
                    &info.ast,
                    info.cx,
                    &info.typed_ast,
                    info.shared_mem,
                    file_sig_opts,
                );
                let tokens = count_tokens(&context);
                let path = reason::string_of_source(strip_root, &info.file_key);
                FileContext {
                    path,
                    context,
                    tokens,
                }
            })
            .collect()
    } else {
        vec![]
    };

    fn accumulate(
        remaining_budget: i32,
        acc_context: String,
        acc_files: Vec<String>,
        truncated: bool,
        file_contexts: &[FileContext],
    ) -> (String, Vec<String>, bool) {
        if file_contexts.is_empty() {
            return (acc_context, acc_files, truncated);
        }
        let fc = &file_contexts[0];
        let rest = &file_contexts[1..];
        if fc.tokens <= remaining_budget {
            let mut new_files = acc_files;
            new_files.push(fc.path.clone());
            accumulate(
                remaining_budget - fc.tokens,
                format!("{}{}\n", acc_context, fc.context),
                new_files,
                truncated,
                rest,
            )
        } else {
            (acc_context, acc_files, true)
        }
    }

    let header = legacy_syntax_header();
    let header_tokens = count_tokens(&header);
    let (context, files_processed, truncated) = accumulate(
        token_budget - header_tokens,
        String::new(),
        Vec::new(),
        false,
        &file_contexts,
    );
    let full_context = format!("{}\n{}", header, context);
    let tokens_used = count_tokens(&full_context);
    llm_context::Result {
        llm_context: full_context,
        files_processed,
        tokens_used,
        truncated,
    }
}
