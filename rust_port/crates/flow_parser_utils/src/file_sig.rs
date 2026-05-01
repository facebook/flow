/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_analysis::scope_api::ScopeInfo;
use flow_analysis::scope_builder;
use flow_common::bitset::Bitset;
use flow_common::files::FileOptions;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_projects::ProjectsOptions;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use vec1::Vec1;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Source(pub Loc, pub FlowSmolStr);

impl Source {
    pub fn loc(&self) -> &Loc {
        &self.0
    }

    pub fn name(&self) -> &FlowSmolStr {
        &self.1
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Identifier(pub Loc, pub FlowSmolStr);

impl Identifier {
    pub fn loc(&self) -> &Loc {
        &self.0
    }

    pub fn name(&self) -> &FlowSmolStr {
        &self.1
    }
}

#[derive(Default)]
pub struct FileSigOptions {
    pub enable_enums: bool,
    pub enable_jest_integration: bool,
    pub enable_relay_integration: bool,
    pub explicit_available_platforms: Option<Vec<FlowSmolStr>>,
    pub file_options: Arc<FileOptions>,
    pub haste_module_ref_prefix: Option<FlowSmolStr>,
    pub project_options: Arc<ProjectsOptions>,
    pub relay_integration_module_prefix: Option<FlowSmolStr>,
}

// We track information about dependencies for each unique module reference in a
// file. For example, `import X from "foo"` and `require("foo")` both induce
// dependencies on the same module and have the same module ref.
//
// Note that different refs can point to the same module, but we haven't
// resolved modules yet, so we don't know where the ref actually points.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Require {
    /// require('foo');
    Require {
        /// location of module ref
        source: Source,
        require_loc: Loc,
        /// Note: These are best-effort.
        /// DO NOT use these for typechecking.
        bindings: Option<RequireBindings>,
        prefix: Option<FlowSmolStr>,
    },
    /// import('foo').then(...)
    ImportDynamic {
        source: Source,
        import_loc: Loc,
    },
    /// import declaration without specifiers
    ///
    /// Note that this is equivalent to the Import variant below with all fields
    /// empty, but modeled as a separate variant to ensure use sites handle this
    /// case if necessary.
    Import0 {
        source: Source,
    },
    /// A synthetic import declaration without a source location.
    /// This variant can only be used if the synthetic import can also be written down in the userland.
    ImportSyntheticUserland {
        source: FlowSmolStr,
    },
    /// A synthetic Haste import declaration without a source location.
    /// This variant is used to generate synthetic Haste imports for multiplatform Haste purposes.
    ImportSyntheticHaste {
        namespace: Bitset,
        name: FlowSmolStr,
        allow_implicit_platform_specific_import: bool,
    },
    /// import declaration with specifiers
    Import {
        import_loc: Loc,
        /// location of module ref
        source: Source,
        /// map from remote name to local names of value imports
        /// source: import {A, B as C} from "foo";
        /// result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}}
        ///
        /// Multiple locations for a given (remoteName, localName) pair are not typical, but they can
        /// occur e.g. with the code `import {foo, foo} from 'bar';`. This code would cause an error
        /// later because of the duplicate local name, but we should handle it here since it does parse.
        named: BTreeMap<FlowSmolStr, BTreeMap<FlowSmolStr, Vec1<ImportedLocs>>>,
        /// optional pair of location of namespace import and local name
        /// source: import * as X from "foo";
        /// result: loc, X
        ns: Option<Identifier>,
        /// map from remote name to local names of type imports
        /// source: import type {A, B as C} from "foo";
        /// source: import {type A, type B as C} from "foo";
        /// result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}}
        types: BTreeMap<FlowSmolStr, BTreeMap<FlowSmolStr, Vec1<ImportedLocs>>>,
        /// map from remote name to local names of typeof imports
        /// source: import typeof {A, B as C} from "foo";
        /// source: import {typeof A, typeof B as C} from "foo";
        /// result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}}
        typesof: BTreeMap<FlowSmolStr, BTreeMap<FlowSmolStr, Vec1<ImportedLocs>>>,
        /// optional pair of location of namespace typeof import and local name
        /// source: import typeof * as X from "foo";
        /// result: loc, X
        typesof_ns: Option<Identifier>,
        /// optional pair of location of namespace type import and local name
        /// source: import type * as X from "foo";
        /// result: loc, X
        type_ns: Option<Identifier>,
    },
    ExportFrom {
        source: Source,
    },
}

#[derive(Debug, Clone, Dupe, serde::Serialize, serde::Deserialize)]
pub struct ImportedLocs {
    pub remote_loc: Loc,
    pub local_loc: Loc,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum RequireBindings {
    /// source: const bar = require('./foo');
    /// result: bar
    BindIdent(Identifier),
    /// map from remote name to local names of requires
    /// source: const {a, b: c} = require('./foo');
    /// result: {a: (a_loc, a), b: (c_loc, c)}
    BindNamed(Vec<(Identifier, RequireBindings)>),
}

// We can extract the observable interface of a module by extracting information
// about what it requires and what it exports.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct FileSig(Vec<Require>);

fn require_bindings_to_debug_string(bindings: &RequireBindings) -> String {
    match bindings {
        RequireBindings::BindIdent(id) => format!("BindIdent ({})", id.1),
        RequireBindings::BindNamed(named) => {
            let items: Vec<String> = named
                .iter()
                .map(|(remote, nested)| {
                    format!("{}: {}", remote.1, require_bindings_to_debug_string(nested))
                })
                .collect();
            format!("BindNamed [{}]", items.join(", "))
        }
    }
}

impl FileSig {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    /// Use for debugging; not for exposing info to the end user
    pub fn to_debug_string(&self) -> String {
        let items: Vec<String> = self
            .0
            .iter()
            .map(|req| match req {
                Require::Require {
                    source,
                    require_loc: _,
                    bindings,
                    prefix,
                } => {
                    let bindings_str = match bindings {
                        None => "None".to_string(),
                        Some(b) => format!("Some ({})", require_bindings_to_debug_string(b)),
                    };
                    let prefix_str = match prefix {
                        None => "None".to_string(),
                        Some(p) => format!("Some ({})", p),
                    };
                    format!(
                        "Require {{ source = {}; bindings = {}; prefix = {} }}",
                        source.1, bindings_str, prefix_str
                    )
                }
                Require::ImportDynamic {
                    source,
                    import_loc: _,
                } => format!("ImportDynamic {{ source = {} }}", source.1),
                Require::Import0 { source } => format!("Import0 {{ source = {} }}", source.1),
                Require::ImportSyntheticUserland { source } => {
                    format!("ImportSyntheticUserland {{ source = {} }}", source)
                }
                Require::ImportSyntheticHaste {
                    namespace: _,
                    name,
                    allow_implicit_platform_specific_import,
                } => {
                    format!(
                        "ImportSyntheticHaste {{ name = {}; allow_implicit = {} }}",
                        name, allow_implicit_platform_specific_import
                    )
                }
                Require::Import {
                    import_loc: _,
                    source,
                    named,
                    ns,
                    types,
                    typesof,
                    typesof_ns,
                    type_ns,
                } => {
                    let named_str = if named.is_empty() {
                        "{}".to_string()
                    } else {
                        format!("{{ {} entries }}", named.len())
                    };
                    let ns_str = match ns {
                        None => "None".to_string(),
                        Some(id) => format!("Some ({})", id.1),
                    };
                    let types_str = if types.is_empty() {
                        "{}".to_string()
                    } else {
                        format!("{{ {} entries }}", types.len())
                    };
                    let typesof_str = if typesof.is_empty() {
                        "{}".to_string()
                    } else {
                        format!("{{ {} entries }}", typesof.len())
                    };
                    let typesof_ns_str = match typesof_ns {
                        None => "None".to_string(),
                        Some(id) => format!("Some ({})", id.1),
                    };
                    let type_ns_str = match type_ns {
                        None => "None".to_string(),
                        Some(id) => format!("Some ({})", id.1),
                    };
                    format!(
                        "Import {{ source = {}; named = {}; ns = {}; types = {}; typesof = {}; typesof_ns = {}; type_ns = {} }}",
                        source.1, named_str, ns_str, types_str, typesof_str, typesof_ns_str, type_ns_str
                    )
                }
                Require::ExportFrom { source } => {
                    format!("ExportFrom {{ source = {} }}", source.1)
                }
            })
            .collect();

        format!("[\n  {};\n]", items.join(";\n  "))
    }

    pub fn require_loc_map(&self) -> BTreeMap<FlowImportSpecifier, Vec<Loc>> {
        let mut map: BTreeMap<FlowImportSpecifier, Vec<Loc>> = BTreeMap::new();
        for req in &self.0 {
            match req {
                Require::ImportSyntheticUserland { source } => {
                    let specifier = FlowImportSpecifier::userland(source.dupe());
                    map.entry(specifier).or_insert_with(Vec::new);
                }
                Require::ImportSyntheticHaste {
                    namespace,
                    name,
                    allow_implicit_platform_specific_import,
                } => {
                    let specifier = FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                        namespace: *namespace,
                        name: name.dupe(),
                        allow_implicit_platform_specific_import:
                            *allow_implicit_platform_specific_import,
                    };
                    map.entry(specifier).or_insert_with(Vec::new);
                }
                Require::Require {
                    source: Source(loc, mref),
                    ..
                }
                | Require::ImportDynamic {
                    source: Source(loc, mref),
                    ..
                }
                | Require::Import0 {
                    source: Source(loc, mref),
                }
                | Require::Import {
                    source: Source(loc, mref),
                    ..
                }
                | Require::ExportFrom {
                    source: Source(loc, mref),
                } => {
                    let specifier = FlowImportSpecifier::userland(mref.dupe());
                    map.entry(specifier)
                        .or_insert_with(Vec::new)
                        .push(loc.dupe());
                }
            }
        }
        map
    }

    pub fn require_set(&self) -> HashSet<FlowImportSpecifier> {
        self.require_loc_map().into_keys().collect()
    }

    pub fn requires(&self) -> &[Require] {
        &self.0
    }

    pub fn from_program(
        file_key: &FileKey,
        ast: &ast::Program<Loc, Loc>,
        opts: &FileSigOptions,
    ) -> Self {
        let mut calc = RequiresCalculator::new(file_key, ast, opts);
        if !file_key.is_lib_file() {
            calc.add_haste_synthetic_imports();
            calc.add_multiplatform_synthetic_imports();
            let Ok(()) = calc.program(ast);
            calc.add_synthetic_imports_for_strict_boundary_import_pattern_opt_outs();
        }
        Self(calc.requires)
    }
}

// Subclass of the AST visitor class that calculates requires and exports. Initializes with the
// scope builder class.
struct RequiresCalculator<'a> {
    file_key: &'a FileKey,
    opts: &'a FileSigOptions,
    requires: Vec<Require>,
    // This ensures that we do not add a `require` with no bindings to `requires` (when processing a
    // `call`) when we have already added that `require` with bindings (when processing a
    // `variable_declarator`).
    visited_requires_with_bindings: HashSet<Loc>,
    scope_info: ScopeInfo<Loc>,
}

impl<'a> RequiresCalculator<'a> {
    fn new(file_key: &'a FileKey, ast: &ast::Program<Loc, Loc>, opts: &'a FileSigOptions) -> Self {
        let scope_info = scope_builder::program(opts.enable_enums, true, ast);
        Self {
            file_key,
            opts,
            requires: Vec::new(),
            visited_requires_with_bindings: HashSet::new(),
            scope_info,
        }
    }

    fn visited_requires_with_bindings(
        &self,
        loc: &Loc,
        bindings: &Option<RequireBindings>,
    ) -> bool {
        bindings.is_none() && self.visited_requires_with_bindings.contains(loc)
    }

    fn visit_requires_with_bindings(&mut self, loc: &Loc, bindings: &Option<RequireBindings>) {
        if bindings.is_some() {
            self.visited_requires_with_bindings.insert(loc.clone());
        }
    }

    fn add_require(&mut self, require: Require) {
        self.requires.push(require);
    }

    fn add_exports(&mut self, kind: ast::statement::ExportKind, source: Option<Source>) {
        match (kind, source) {
            (ast::statement::ExportKind::ExportValue, Some(source)) => {
                self.add_require(Require::ExportFrom { source });
            }
            (ast::statement::ExportKind::ExportValue, None) => {}
            (ast::statement::ExportKind::ExportType, None) => {}
            (ast::statement::ExportKind::ExportType, Some(source)) => {
                self.add_require(Require::ExportFrom { source });
            }
        }
    }

    fn add_synthetic_imports_for_strict_boundary_import_pattern_opt_outs(&mut self) {
        use flow_common::files;
        use flow_common::flow_projects::FlowProjects;

        if !self
            .opts
            .project_options
            .projects_strict_boundary_validate_import_pattern_opt_outs()
        {
            return;
        }

        if files::haste_name_opt(&self.opts.file_options, self.file_key).is_none() {
            return;
        }

        let file = self.file_key.as_str();
        if !self.opts.project_options.is_common_code_path(file) {
            return;
        }

        // Collect the import specifiers that need synthetic imports
        let mut import_specifiers_to_add = Vec::new();
        for require in &self.requires {
            let import_specifier = match require {
                Require::Require { source, .. }
                | Require::ImportDynamic { source, .. }
                | Require::Import0 { source }
                | Require::Import { source, .. }
                | Require::ExportFrom { source } => source.1.dupe(),
                Require::ImportSyntheticUserland { .. } | Require::ImportSyntheticHaste { .. } => {
                    continue;
                }
            };

            if self
                .opts
                .project_options
                .is_import_specifier_that_opt_out_of_strict_boundary(import_specifier.as_str())
            {
                import_specifiers_to_add.push(import_specifier);
            }
        }

        // Now add the synthetic imports
        if let Some(projects_bitset) = FlowProjects::from_path(&self.opts.project_options, file) {
            if let Some(individual_projects) = self
                .opts
                .project_options
                .individual_projects_bitsets_from_common_project_bitset_excluding_first(
                    projects_bitset,
                )
            {
                for import_specifier in import_specifiers_to_add {
                    for project in &individual_projects {
                        self.add_require(Require::ImportSyntheticHaste {
                            namespace: project.to_bitset(),
                            name: import_specifier.dupe(),
                            allow_implicit_platform_specific_import: true,
                        });
                    }
                }
            }
        }
    }

    fn add_haste_synthetic_imports(&mut self) {
        use flow_common::files;
        use flow_common::flow_projects::FlowProjects;

        if let Some(haste_name) = files::haste_name_opt(&self.opts.file_options, self.file_key) {
            if let Some(projects_bitset) =
                FlowProjects::from_path(&self.opts.project_options, self.file_key.as_str())
            {
                if let Some(individual_projects) = self
                    .opts
                    .project_options
                    .individual_projects_bitsets_from_common_project_bitset(projects_bitset)
                {
                    for project in individual_projects {
                        self.add_require(Require::ImportSyntheticHaste {
                            namespace: project.to_bitset(),
                            name: FlowSmolStr::from(haste_name.as_str()),
                            allow_implicit_platform_specific_import: false,
                        });
                    }
                }
            }
        }
    }

    fn add_multiplatform_synthetic_imports(&mut self) {
        use flow_common::files;
        use flow_common::platform_set;

        let platform_set = platform_set::available_platforms(
            &self.opts.file_options,
            &self.opts.project_options,
            self.file_key.as_str(),
            self.opts.explicit_available_platforms.as_deref(),
        );

        match platform_set::platform_specific_implementation_mrefs_of_possibly_interface_file(
            &self.opts.file_options,
            platform_set.as_ref(),
            self.file_key,
        ) {
            Some((unconditional_extensions, grouped_extensions_with_conditional_extensions)) => {
                // Regardless of whether they are actually required, in file_sig, we will synthesize
                // imports for all of them. Later in merge_js, we will only error on missing required
                // ones.
                for source in unconditional_extensions {
                    self.add_require(Require::ImportSyntheticUserland {
                        source: FlowSmolStr::from(source.as_str()),
                    });
                }

                for (grouped, conditional) in grouped_extensions_with_conditional_extensions {
                    self.add_require(Require::ImportSyntheticUserland {
                        source: FlowSmolStr::from(grouped.as_str()),
                    });
                    for cond_source in conditional {
                        self.add_require(Require::ImportSyntheticUserland {
                            source: FlowSmolStr::from(cond_source.as_str()),
                        });
                    }
                }
            }
            None => {
                if let Some(imported_interface_module_name) =
                    files::relative_interface_mref_of_possibly_platform_specific_file(
                        &self.opts.file_options,
                        self.file_key,
                    )
                {
                    self.add_require(Require::ImportSyntheticUserland {
                        source: FlowSmolStr::from(imported_interface_module_name.as_str()),
                    });
                }
            }
        }
    }

    fn require_pattern(pattern: &ast::pattern::Pattern<Loc, Loc>) -> Option<RequireBindings> {
        match pattern {
            ast::pattern::Pattern::Identifier { inner, .. } => {
                let ident = Identifier(inner.name.loc.dupe(), inner.name.name.dupe());
                Some(RequireBindings::BindIdent(ident))
            }
            ast::pattern::Pattern::Object { inner, .. } => {
                let mut named_bindings = Vec::new();
                for prop in inner.properties.iter() {
                    match prop {
                        ast::pattern::object::Property::NormalProperty(normal_prop) => {
                            if let ast::pattern::object::Key::Identifier(remote_ident) =
                                &normal_prop.key
                            {
                                if let Some(bindings) = Self::require_pattern(&normal_prop.pattern)
                                {
                                    let remote = Identifier(
                                        remote_ident.loc.dupe(),
                                        remote_ident.name.dupe(),
                                    );
                                    named_bindings.push((remote, bindings));
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        ast::pattern::object::Property::RestElement(_) => {
                            // Can't handle rest elements in require patterns
                            return None;
                        }
                    }
                }
                Some(RequireBindings::BindNamed(named_bindings))
            }
            _ => None,
        }
    }

    fn handle_require(
        &mut self,
        left: &ast::pattern::Pattern<Loc, Loc>,
        right: &ast::expression::Expression<Loc, Loc>,
    ) {
        let bindings = Self::require_pattern(left);
        if let ExpressionInner::Call { loc, inner } = right.deref() {
            self.handle_call(
                loc.dupe(),
                &inner.callee,
                &inner.targs,
                &inner.arguments,
                bindings,
            );
        }
    }

    fn handle_call(
        &mut self,
        call_loc: Loc,
        callee: &ast::expression::Expression<Loc, Loc>,
        targs: &Option<ast::expression::CallTypeArgs<Loc, Loc>>,
        arguments: &ast::expression::ArgList<Loc, Loc>,
        bindings: Option<RequireBindings>,
    ) {
        if self.visited_requires_with_bindings(&call_loc, &bindings) {
            return;
        }
        self.visit_requires_with_bindings(&call_loc, &bindings);

        // Check for require('module')
        if let (
            ExpressionInner::Identifier {
                loc: require_loc,
                inner: callee_id,
            },
            None,
        ) = (callee.deref(), targs)
            && callee_id.name.as_str() == "require"
            && arguments.arguments.len() == 1
            && let Some(ast::expression::ExpressionOrSpread::Expression(arg)) =
                arguments.arguments.first()
        {
            let module_name = match arg.deref() {
                ExpressionInner::StringLiteral { loc, inner } => {
                    Some(Source(loc.dupe(), inner.value.dupe()))
                }
                ExpressionInner::TemplateLiteral { loc, inner }
                    if inner.quasis.len() == 1 && inner.expressions.is_empty() =>
                {
                    Some(Source(loc.dupe(), inner.quasis[0].value.cooked.dupe()))
                }
                _ => None,
            };

            if let Some(source) = module_name {
                if !self.scope_info.is_local_use(require_loc) {
                    self.add_require(Require::Require {
                        source,
                        require_loc: call_loc,
                        bindings,
                        prefix: None,
                    });
                }
            }
        }

        if self.opts.enable_jest_integration {
            if let Some((jest_loc, source_loc, name)) =
                ast_utils::get_call_to_jest_module_mocking_fn(callee, arguments)
                && !self.scope_info.is_local_use(jest_loc)
            {
                self.add_require(Require::Import0 {
                    source: Source(source_loc.dupe(), name.clone()),
                });
            }
        }
    }
}

impl<'a> flow_parser::ast_visitor::AstVisitor<'_, Loc> for RequiresCalculator<'a> {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn call(&mut self, call_loc: &Loc, expr: &ast::expression::Call<Loc, Loc>) -> Result<(), !> {
        self.handle_call(
            call_loc.clone(),
            &expr.callee,
            &expr.targs,
            &expr.arguments,
            None,
        );
        let Ok(()) = ast_visitor::call_default(self, call_loc, expr);
        Ok(())
    }

    fn module_ref_literal(&mut self, lit: &ast::ModuleRefLiteral<Loc>) -> Result<(), !> {
        let loc = &lit.require_loc;
        let mref = &lit.value[lit.prefix_len..];
        let prefix = if lit.prefix_len > 0 {
            Some(FlowSmolStr::from(&lit.value[..lit.prefix_len]))
        } else {
            None
        };
        self.add_require(Require::Require {
            source: Source(loc.dupe(), FlowSmolStr::from(mref)),
            require_loc: loc.dupe(),
            bindings: None,
            prefix,
        });
        let Ok(()) = ast_visitor::module_ref_literal_default(self, lit);
        Ok(())
    }

    fn generic_identifier_import_type(
        &mut self,
        import_type: &ast::types::generic::ImportType<Loc, Loc>,
    ) -> Result<(), !> {
        let (loc, string_lit) = &import_type.argument;
        self.add_require(Require::Import0 {
            source: Source(loc.dupe(), FlowSmolStr::from(string_lit.value.as_str())),
        });
        let Ok(()) = ast_visitor::generic_identifier_import_type_default(self, import_type);
        Ok(())
    }

    fn tagged_template(
        &mut self,
        loc: &Loc,
        expr: &ast::expression::TaggedTemplate<Loc, Loc>,
    ) -> Result<(), !> {
        if self.opts.enable_relay_integration {
            if let ExpressionInner::Identifier { inner, .. } = expr.tag.deref()
                && inner.name.as_str() == "graphql"
            {
                if let Ok(module_name) = crate::graphql::extract_module_name(
                    &expr.quasi.1,
                    self.opts.relay_integration_module_prefix.as_deref(),
                ) {
                    self.add_require(Require::Require {
                        source: Source(loc.dupe(), FlowSmolStr::from(module_name.as_str())),
                        require_loc: loc.dupe(),
                        bindings: None,
                        prefix: self
                            .opts
                            .relay_integration_module_prefix
                            .as_ref()
                            .map(|s| FlowSmolStr::from(s.as_str())),
                    });
                    return Ok(());
                }
            }
        }
        let Ok(()) = ast_visitor::tagged_template_default(self, loc, expr);
        Ok(())
    }

    fn expression(&mut self, expr: &ast::expression::Expression<Loc, Loc>) -> Result<(), !> {
        if let ExpressionInner::Import {
            loc: import_loc,
            inner,
        } = expr.deref()
        {
            let module_name = match inner.argument.deref() {
                ExpressionInner::StringLiteral { loc, inner } => {
                    Some(Source(loc.dupe(), inner.value.dupe()))
                }
                ExpressionInner::TemplateLiteral { loc, inner }
                    if inner.quasis.len() == 1 && inner.expressions.is_empty() =>
                {
                    Some(Source(loc.dupe(), inner.quasis[0].value.cooked.dupe()))
                }
                _ => None,
            };
            if let Some(source) = module_name {
                self.add_require(Require::ImportDynamic {
                    source,
                    import_loc: import_loc.dupe(),
                });
            }
        }
        let Ok(()) = ast_visitor::expression_default(self, expr);
        Ok(())
    }

    fn import_declaration(
        &mut self,
        import_loc: &Loc,
        decl: &ast::statement::ImportDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let source = Source(decl.source.0.dupe(), decl.source.1.value.dupe());

        let import = match (&decl.default, &decl.specifiers) {
            (None, None) => Require::Import0 { source },
            _ => {
                let mut named = BTreeMap::new();
                let mut ns = None;
                let mut types = BTreeMap::new();
                let mut typesof = BTreeMap::new();
                let mut typesof_ns = None;
                let mut type_ns = None;

                fn add_named(
                    map: &mut BTreeMap<FlowSmolStr, BTreeMap<FlowSmolStr, Vec1<ImportedLocs>>>,
                    remote: FlowSmolStr,
                    local: FlowSmolStr,
                    locs: ImportedLocs,
                ) {
                    let local_map = map.entry(remote).or_insert_with(BTreeMap::new);
                    if let Some(existing) = local_map.get_mut(&local) {
                        existing.push(locs)
                    } else {
                        local_map.insert(local, Vec1::new(locs));
                    }
                }

                if let Some(default) = &decl.default {
                    let loc = default.identifier.loc.dupe();
                    let local = default.identifier.name.dupe();
                    let target_map = match decl.import_kind {
                        ast::statement::ImportKind::ImportType => &mut types,
                        ast::statement::ImportKind::ImportTypeof => &mut typesof,
                        ast::statement::ImportKind::ImportValue => &mut named,
                    };
                    add_named(
                        target_map,
                        FlowSmolStr::new_inline("default"),
                        local,
                        ImportedLocs {
                            remote_loc: loc.dupe(),
                            local_loc: loc,
                        },
                    );
                }
                if let Some(specifiers) = &decl.specifiers {
                    match specifiers {
                        ast::statement::import_declaration::Specifier::ImportNamespaceSpecifier(
                            (_, ident),
                        ) => {
                            let target = match decl.import_kind {
                                ast::statement::ImportKind::ImportType => Some(&mut type_ns),
                                ast::statement::ImportKind::ImportTypeof => Some(&mut typesof_ns),
                                ast::statement::ImportKind::ImportValue => Some(&mut ns),
                            };
                            if let Some(target) = target {
                                *target = Some(Identifier(ident.loc.dupe(), ident.name.dupe()));
                            }
                        }
                        ast::statement::import_declaration::Specifier::ImportNamedSpecifiers(
                            named_specifiers,
                        ) => {
                            for ast::statement::import_declaration::NamedSpecifier {
                                local,
                                remote,
                                remote_name_def_loc: _,
                                kind,
                                kind_loc: _,
                            } in named_specifiers
                            {
                                let import_kind = kind.unwrap_or(decl.import_kind);
                                let local_ident = local.as_ref().unwrap_or(remote);
                                let remote_loc = remote.loc.dupe();
                                let remote_name = remote.name.dupe();
                                let local_loc = local_ident.loc.dupe();
                                let local_name = local_ident.name.dupe();

                                let target_map = match import_kind {
                                    ast::statement::ImportKind::ImportType => &mut types,
                                    ast::statement::ImportKind::ImportTypeof => &mut typesof,
                                    ast::statement::ImportKind::ImportValue => &mut named,
                                };
                                add_named(
                                    target_map,
                                    remote_name,
                                    local_name,
                                    ImportedLocs {
                                        remote_loc,
                                        local_loc,
                                    },
                                );
                            }
                        }
                    }
                }

                Require::Import {
                    import_loc: import_loc.dupe(),
                    source,
                    named,
                    ns,
                    types,
                    typesof,
                    typesof_ns,
                    type_ns,
                }
            }
        };

        self.add_require(import);
        let Ok(()) = ast_visitor::import_declaration_default(self, import_loc, decl);
        Ok(())
    }

    fn import_equals_declaration(
        &mut self,
        _loc: &Loc,
        decl: &ast::statement::ImportEqualsDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        match &decl.module_reference {
            ast::statement::import_equals_declaration::ModuleReference::ExternalModuleReference(
                source_loc,
                lit,
            ) => {
                self.add_require(Require::Import0 {
                    source: Source(source_loc.dupe(), lit.value.dupe()),
                });
            }
            ast::statement::import_equals_declaration::ModuleReference::Identifier(_) => {}
        }
        ast_visitor::import_equals_declaration_default(self, _loc, decl)
    }

    fn export_default_declaration(
        &mut self,
        _stmt_loc: &Loc,
        _decl: &ast::statement::ExportDefaultDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        self.add_exports(ast::statement::ExportKind::ExportValue, None);
        let Ok(()) = ast_visitor::export_default_declaration_default(self, _stmt_loc, _decl);
        Ok(())
    }

    fn export_named_declaration(
        &mut self,
        _stmt_loc: &Loc,
        decl: &ast::statement::ExportNamedDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let source = decl
            .source
            .as_ref()
            .map(|(loc, lit)| Source(loc.dupe(), lit.value.dupe()));
        if decl.declaration.is_some() {
            self.add_exports(decl.export_kind.clone(), source.clone());
        }
        match &decl.specifiers {
            None => {}
            Some(ast::statement::export_named_declaration::Specifier::ExportSpecifiers(specs)) => {
                if flow_parser::ast_utils::export_specifiers_has_value_export(
                    decl.export_kind,
                    specs,
                ) {
                    self.add_exports(ast::statement::ExportKind::ExportValue, source);
                } else {
                    self.add_exports(ast::statement::ExportKind::ExportType, source);
                }
            }
            Some(ast::statement::export_named_declaration::Specifier::ExportBatchSpecifier(_)) => {
                self.add_exports(decl.export_kind.clone(), source);
            }
        }
        let Ok(()) = ast_visitor::export_named_declaration_default(self, _stmt_loc, decl);
        Ok(())
    }

    fn declare_export_declaration(
        &mut self,
        stmt_loc: &Loc,
        decl: &ast::statement::DeclareExportDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let source = decl
            .source
            .as_ref()
            .map(|(loc, lit)| Source(loc.dupe(), lit.value.dupe()));

        if let Some(declaration) = &decl.declaration {
            use ast::statement::declare_export_declaration::Declaration;
            let export_kind = match declaration {
                Declaration::Variable { .. }
                | Declaration::Function { .. }
                | Declaration::Class { .. }
                | Declaration::Component { .. }
                | Declaration::DefaultType { .. }
                | Declaration::Enum { .. }
                | Declaration::Namespace { .. } => ast::statement::ExportKind::ExportValue,
                Declaration::NamedType { .. }
                | Declaration::NamedOpaqueType { .. }
                | Declaration::Interface { .. } => ast::statement::ExportKind::ExportType,
            };
            self.add_exports(export_kind, source.clone());
        }

        if decl.specifiers.is_some() {
            self.add_exports(ast::statement::ExportKind::ExportValue, source);
        }
        let Ok(()) = ast_visitor::declare_export_declaration_default(self, stmt_loc, decl);
        Ok(())
    }

    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        decl: &ast::statement::variable::Declarator<Loc, Loc>,
    ) -> Result<(), !> {
        if let Some(init) = &decl.init {
            self.handle_require(&decl.id, init);
        }
        let Ok(()) = ast_visitor::variable_declarator_default(self, kind, decl);
        Ok(())
    }

    fn declare_module(
        &mut self,
        _loc: &Loc,
        _m: &ast::statement::DeclareModule<Loc, Loc>,
    ) -> Result<(), !> {
        // Skip declare module
        Ok(())
    }
}
