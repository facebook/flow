/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/document_paste.ml`

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_analysis::scope_api::ScopeInfo;
use flow_analysis::scope_builder;
use flow_common::flow_import_specifier::Userland;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper::LocMapper;
use flow_parser_utils_output::js_layout_generator;
use flow_services_autocomplete::lsp_import_edits;
use flow_services_autocomplete::module_system_info::LspModuleSystemInfo;
use flow_services_export::export_index;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_type::type_::Type;
use vec1::Vec1;

use crate::autofix_imports;

// NOTE: These types are defined in Lsp.DocumentPaste in OCaml (hack_forked/utils/lsp/lsp.ml).
// They are reproduced here faithfully since the Lsp module is not yet ported to Rust.
#[derive(Debug, Clone)]
pub enum ImportType {
    ImportNamedValue,
    ImportValueAsNamespace,
    ImportNamedType,
    ImportNamedTypeOf,
    ImportTypeOfAsNamespace,
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub remote_name: String,
    pub local_name: Option<String>,
    pub import_type: ImportType,
    pub import_source: String,
    pub import_source_is_resolved: bool,
}

struct ImportedDefCollector<'a> {
    scope: &'a ScopeInfo<Loc>,
    ranges: &'a [Loc],
    import_def_locs: Vec<(String, Vec1<Loc>)>,
}

impl<'a> ImportedDefCollector<'a> {
    fn loc_within_range(&self, l: &Loc) -> bool {
        self.ranges.iter().any(|range| range.contains(l))
    }

    fn collect_relevant_def_loc_of_imported_identifier(&mut self, use_loc: &Loc) {
        if self.loc_within_range(use_loc) {
            match self.scope.def_of_use_opt(use_loc) {
                Some(def) => {
                    if !def.locs.iter().any(|l| self.loc_within_range(l)) {
                        self.import_def_locs
                            .push((def.actual_name.to_string(), def.locs.clone()));
                    }
                }
                None => {}
            }
        }
    }
}

impl<'ast, 'a> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for ImportedDefCollector<'a> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
        self.collect_relevant_def_loc_of_imported_identifier(&id.loc);
        Ok(())
    }

    fn jsx_identifier(&mut self, id: &'ast ast::jsx::Identifier<Loc, Loc>) -> Result<(), !> {
        self.collect_relevant_def_loc_of_imported_identifier(&id.loc);
        Ok(())
    }

    fn statement(&mut self, stmt: &'ast ast::statement::Statement<Loc, Loc>) -> Result<(), !> {
        ast_visitor::statement_default(self, stmt)
    }

    fn expression(&mut self, expr: &'ast ast::expression::Expression<Loc, Loc>) -> Result<(), !> {
        ast_visitor::expression_default(self, expr)
    }

    fn program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> Result<(), !> {
        ast_visitor::program_default(self, program)
    }
}

struct ImportInformationExtractor<'a, 'cx> {
    cx: &'a Context<'cx>,
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    relevant_imported_defs: &'a [(String, Vec1<Loc>)],
    import_items: Vec<ImportItem>,
}

impl<'a> LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), ()>
    for ImportInformationExtractor<'a, '_>
{
    fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, ()> {
        Ok(loc.dupe())
    }

    fn on_type_annot(&mut self, x: &(ALoc, Type)) -> Result<(ALoc, Type), ()> {
        Ok(x.dupe())
    }
}

impl<'a> ImportInformationExtractor<'a, '_> {
    fn import_declaration(
        &mut self,
        _loc: &ALoc,
        decl: &ast::statement::ImportDeclaration<ALoc, (ALoc, Type)>,
    ) {
        use ast::statement::ImportKind;
        use ast::statement::import_declaration::*;
        let ast::statement::ImportDeclaration {
            import_kind,
            source,
            specifiers,
            default,
            ..
        } = decl;
        let lazy_import_source_info = std::cell::OnceCell::new();
        let source_loc = &source.0.0;
        let source_value = &source.1.value;
        let compute_import_source_info = || -> (FlowSmolStr, bool) {
            let mref = Userland::from_smol_str(source_value.dupe());
            match flow_js_utils::import_export_utils::get_module_type_or_any(
                self.cx,
                false, // ~perform_platform_validation:false
                None,  // ~import_kind_for_untyped_import_validation:None
                source_loc.dupe(),
                mref,
            ) {
                Err(_) => (source_value.dupe(), false),
                Ok(Ok(m)) => {
                    // m.Type.module_reason |> Reason.def_loc_of_reason |> ALoc.source
                    let def_loc = m.module_reason.def_loc();
                    match def_loc.source() {
                        Some(f) => {
                            if self.cx.file() == f || f.is_lib_file() {
                                (source_value.dupe(), false)
                            } else {
                                (FlowSmolStr::from(f.to_absolute()), true)
                            }
                        }
                        None => (source_value.dupe(), false),
                    }
                }
                // Error case from get_module_type_or_any inner result
                Ok(Err(_)) => (source_value.dupe(), false),
            }
        };
        let collect =
            |import_items: &mut Vec<ImportItem>,
             import_type: ImportType,
             remote: &ast::Identifier<ALoc, (ALoc, Type)>,
             local_opt: Option<&ast::Identifier<ALoc, (ALoc, Type)>>| {
                let effective = local_opt.unwrap_or(remote);
                let (l, _) = &effective.loc;
                let name = &effective.name;
                let loc = (self.loc_of_aloc)(l);
                if self
                    .relevant_imported_defs
                    .iter()
                    .any(|(n, locs)| name.as_str() == n && locs.contains(&loc))
                {
                    let (import_source, import_source_is_resolved) =
                        lazy_import_source_info.get_or_init(&compute_import_source_info);
                    let import_item = ImportItem {
                        remote_name: remote.name.to_string(),
                        local_name: local_opt.map(|id| id.name.to_string()),
                        import_type,
                        import_source: import_source.to_string(),
                        import_source_is_resolved: *import_source_is_resolved,
                    };
                    import_items.push(import_item);
                }
            };
        if let Some(specifiers) = specifiers {
            match specifiers {
                Specifier::ImportNamedSpecifiers(named_specifiers) => {
                    for spec in named_specifiers.iter() {
                        let kind = spec.kind.as_ref().unwrap_or(import_kind);
                        let import_type = match kind {
                            ImportKind::ImportType => ImportType::ImportNamedType,
                            ImportKind::ImportTypeof => ImportType::ImportNamedTypeOf,
                            ImportKind::ImportValue => ImportType::ImportNamedValue,
                        };
                        collect(
                            &mut self.import_items,
                            import_type,
                            &spec.remote,
                            spec.local.as_ref(),
                        );
                    }
                }
                Specifier::ImportNamespaceSpecifier((_, id)) => match import_kind {
                    ImportKind::ImportType => {}
                    ImportKind::ImportTypeof => {
                        collect(
                            &mut self.import_items,
                            ImportType::ImportTypeOfAsNamespace,
                            id,
                            None,
                        );
                    }
                    ImportKind::ImportValue => {
                        collect(
                            &mut self.import_items,
                            ImportType::ImportValueAsNamespace,
                            id,
                            None,
                        );
                    }
                },
            }
        }
        if let Some(default_spec) = default {
            let import_type = match import_kind {
                ImportKind::ImportType => ImportType::ImportNamedType,
                ImportKind::ImportTypeof => ImportType::ImportNamedTypeOf,
                ImportKind::ImportValue => ImportType::ImportNamedValue,
            };
            // We need to construct a synthetic remote identifier with name "default"
            // The OCaml creates (fst identifier, { name = "default"; comments = None })
            // In Rust we create a synthetic identifier
            let synthetic_remote = flow_parser::ast_utils::ident_of_source(
                None,
                default_spec.identifier.loc.dupe(),
                FlowSmolStr::from("default"),
            );
            collect(
                &mut self.import_items,
                import_type,
                &synthetic_remote,
                Some(&default_spec.identifier),
            );
        }
    }
}

pub fn prepare_document_paste(
    cx: &Context,
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    ast: &ast::Program<Loc, Loc>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    ranges: &[Loc],
) -> Vec<ImportItem> {
    let scope = scope_builder::program(cx.enable_enums(), true, ast);
    let mut collector = ImportedDefCollector {
        scope: &scope,
        ranges,
        import_def_locs: vec![],
    };
    let Ok(()) = collector.program(ast);
    let relevant_imported_defs = collector.import_def_locs;
    let mut extractor = ImportInformationExtractor {
        cx,
        loc_of_aloc,
        relevant_imported_defs: &relevant_imported_defs,
        import_items: vec![],
    };
    for stmt in typed_ast.statements.iter() {
        if let ast::statement::StatementInner::ImportDeclaration { loc, inner } = &**stmt {
            extractor.import_declaration(loc, inner);
        }
    }
    let mut import_items = extractor.import_items;
    import_items.reverse();
    import_items
}

pub fn provide_document_paste_edits(
    layout_options: &js_layout_generator::Opts,
    module_system_info: &LspModuleSystemInfo,
    src_dir: Option<&str>,
    ast: &ast::Program<Loc, Loc>,
    import_items: &[ImportItem],
) -> Vec<(Loc, String)> {
    let scope_info = scope_builder::program(true, true, ast);
    // Scope_api.With_Loc.toplevel_scopes is [0] in OCaml.
    // ScopeId(0) is pub(super), but scopes is a public BTreeMap<ScopeId, Scope>.
    // The first entry (smallest key) is ScopeId(0) — the toplevel scope.
    let mut module_scope_defs: BTreeSet<String> = BTreeSet::new();
    if let Some((_, scope)) = scope_info.scopes.iter().next() {
        for name in scope.defs.keys() {
            module_scope_defs.insert(name.to_string());
        }
    }
    let added_imports: Vec<(String, autofix_imports::Bindings)> = import_items
        .iter()
        .filter_map(|item| {
            let ImportItem {
                remote_name,
                local_name,
                import_type,
                import_source,
                import_source_is_resolved,
            } = item;
            let effective_name = local_name.as_deref().unwrap_or(remote_name.as_str());
            if module_scope_defs.contains(effective_name) {
                return None;
            }
            let source = if *import_source_is_resolved {
                export_index::Source::FileKey(FileKey::new(FileKeyInner::SourceFile(
                    import_source.clone(),
                )))
            } else {
                export_index::Source::Builtin(Userland::from_smol_str(FlowSmolStr::from(
                    import_source.as_str(),
                )))
            };
            let from = lsp_import_edits::from_of_source(module_system_info, src_dir, &source)?;
            match import_type {
                ImportType::ImportNamedType => {
                    if remote_name == "default" {
                        Some((
                            from,
                            autofix_imports::Bindings::DefaultType(local_name.clone().unwrap()),
                        ))
                    } else {
                        Some((
                            from,
                            autofix_imports::Bindings::NamedType(vec![
                                autofix_imports::NamedBinding {
                                    remote_name: remote_name.clone(),
                                    local_name: local_name.clone(),
                                },
                            ]),
                        ))
                    }
                }
                ImportType::ImportNamedTypeOf => {
                    if remote_name == "default" {
                        Some((
                            from,
                            autofix_imports::Bindings::DefaultTypeof(local_name.clone().unwrap()),
                        ))
                    } else {
                        Some((
                            from,
                            autofix_imports::Bindings::NamedTypeof(vec![
                                autofix_imports::NamedBinding {
                                    remote_name: remote_name.clone(),
                                    local_name: local_name.clone(),
                                },
                            ]),
                        ))
                    }
                }
                ImportType::ImportNamedValue => {
                    if remote_name == "default" {
                        Some((
                            from,
                            autofix_imports::Bindings::Default(local_name.clone().unwrap()),
                        ))
                    } else {
                        Some((
                            from,
                            autofix_imports::Bindings::Named(vec![autofix_imports::NamedBinding {
                                remote_name: remote_name.clone(),
                                local_name: local_name.clone(),
                            }]),
                        ))
                    }
                }
                ImportType::ImportTypeOfAsNamespace => Some((
                    from,
                    autofix_imports::Bindings::TypeofNamespace(remote_name.clone()),
                )),
                ImportType::ImportValueAsNamespace => Some((
                    from,
                    autofix_imports::Bindings::Namespace(remote_name.clone()),
                )),
            }
        })
        .collect();
    autofix_imports::add_imports(layout_options, &added_imports, ast)
}
