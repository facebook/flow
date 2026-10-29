/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::mem;
use std::ops::Deref;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_analysis::bindings::Kind;
use flow_analysis::scope_api::Def;
use flow_analysis::scope_api::ScopeInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::class::BodyElement;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::ExpressionOrSpread;
use flow_parser::ast::expression::member;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::ImportedLocs;
use flow_parser_utils::file_sig::Require;
use flow_parser_utils::file_sig::RequireBindings;
use vec1::Vec1;

use crate::find_providers::State;
use crate::provider_api::Info as ProviderInfo;

/// Classification of an assertion function, shared by the targeted callee
/// analysis (producer) and name resolution (consumer).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssertionKind {
    Bare,
    TypeGuard,
}

/// The value asserted by an assertion function.
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub enum AssertionTarget {
    Parameter(usize),
    Receiver,
}

/// The asserted value and whether the assertion carries a type guard.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssertionInfo {
    pub target: AssertionTarget,
    pub kind: AssertionKind,
}

/// Whether a spread precedes the asserted parameter index. Spreads shift
/// every later positional argument, so the argument at the asserted index
/// is not necessarily the asserted one.
pub fn spread_before_index<M: Dupe, T: Dupe>(
    arguments: &[ExpressionOrSpread<M, T>],
    parameter_index: usize,
) -> bool {
    arguments
        .iter()
        .take(parameter_index)
        .any(|argument| matches!(argument, ExpressionOrSpread::Spread(_)))
}

/// The argument asserted by a call to a proven bare assertion function
/// (`asserts x`), when it is positionally known: no spread precedes it and it
/// is not itself a spread.
pub fn bare_asserted_argument<M: Dupe, T: Dupe>(
    assertion: AssertionInfo,
    arguments: &[ExpressionOrSpread<M, T>],
) -> Option<&Expression<M, T>> {
    let AssertionTarget::Parameter(parameter_index) = assertion.target else {
        return None;
    };
    if assertion.kind != AssertionKind::Bare || spread_before_index(arguments, parameter_index) {
        return None;
    }
    match arguments.get(parameter_index)? {
        ExpressionOrSpread::Expression(argument) => Some(argument),
        ExpressionOrSpread::Spread(_) => None,
    }
}

/// Whether a call to a proven bare assertion function (`asserts x`) provably
/// never returns: no spread precedes the asserted index, and the asserted
/// argument is present and is the literal `false`. A missing argument may
/// hit a callee default, so those calls can return normally.
pub fn bare_assertion_call_always_throws<M: Dupe, T: Dupe>(
    assertion: AssertionInfo,
    arguments: &[ExpressionOrSpread<M, T>],
) -> bool {
    // An omitted argument is not necessarily falsy: the callee may supply a
    // default parameter value, in which case the call can return normally.
    bare_asserted_argument(assertion, arguments).is_some_and(|argument| {
        matches!(argument.deref(), ExpressionInner::BooleanLiteral { inner, .. } if !inner.value)
    })
}

/// Which export an imported callee root refers to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportedName {
    CommonJS,
    Default,
    Named(FlowSmolStr),
    Namespace,
}

/// Import identity of a callee root, collected alongside the target so the
/// assertion analysis can resolve imported callees without the ordinary
/// definition graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportedCallee {
    pub source: FlowSmolStr,
    pub remote: ImportedName,
    /// True for value imports; type and typeof imports never classify.
    pub is_value: bool,
}

/// A call whose callee is rooted in a binding that could hold an assertion
/// function: an import, a named definition, an annotated provider, a plain
/// lexical value binding (classified later from its signature type, when it
/// has one), or a global reference such as a lib declaration (classified by
/// name through builtins).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalleeTarget<L> {
    pub call_loc: L,
    pub callee_loc: L,
    pub root_use_loc: L,
    pub root_binding_loc: L,
    pub root_name: FlowSmolStr,
    pub is_global: bool,
    pub provider_locs: Vec<L>,
    pub property_path: Vec<FlowSmolStr>,
    pub import: Option<ImportedCallee>,
    /// How to obtain the receiver from the root signature type.
    pub root_kind: CalleeRootKind,
    /// Property-path prefix lengths checked by optional chain steps.
    pub optional_receiver_paths: Vec<usize>,
}

/// Receiver selected from a binding or an enclosing class signature.
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub enum CalleeRootKind {
    Binding,
    InstanceThis,
    StaticThis,
    InstanceSuper,
    StaticSuper,
}

struct Collector<'a, L: LocSig> {
    scope_info: &'a ScopeInfo<L>,
    provider_info: &'a ProviderInfo<L>,
    /// `import` bindings and `X = require('source')` bindings, keyed by the
    /// local identifier location.
    imports: BTreeMap<L, ImportedCallee>,
    targets: Vec<CalleeTarget<L>>,
    class_binding: Option<(L, FlowSmolStr)>,
    receiver: Option<(L, FlowSmolStr, CalleeRootKind)>,
}

impl<'a, L: LocSig> Collector<'a, L> {
    fn collect_assertion_flow_calls(&mut self, expression: &Expression<L, L>) {
        match &**expression {
            ExpressionInner::Call { loc, inner } => {
                if let Some(target) = self.target_for_call(loc, &inner.callee) {
                    self.targets.push(target);
                }
            }
            ExpressionInner::OptionalCall { loc, inner } => {
                if let Some(mut target) = self.target_for_call(loc, &inner.call.callee) {
                    if matches!(inner.optional, ast::expression::OptionalCallKind::Optional) {
                        target
                            .optional_receiver_paths
                            .push(target.property_path.len());
                    }
                    self.targets.push(target);
                }
            }
            ExpressionInner::Sequence { inner, .. } => {
                for expression in inner.expressions.iter() {
                    self.collect_assertion_flow_calls(expression);
                }
            }
            _ => {}
        }
    }

    fn callee_path(
        callee: &Expression<L, L>,
    ) -> Option<(&Expression<L, L>, Vec<FlowSmolStr>, Vec<usize>)> {
        match &**callee {
            ExpressionInner::Identifier { .. }
            | ExpressionInner::This { .. }
            | ExpressionInner::Super { .. } => Some((callee, vec![], vec![])),
            ExpressionInner::Member { inner, .. } => {
                let member::Property::PropertyIdentifier(property) = &inner.property else {
                    return None;
                };
                let (root, mut path, optional) = Self::callee_path(&inner.object)?;
                path.push(property.name.dupe());
                Some((root, path, optional))
            }
            ExpressionInner::OptionalMember { inner, .. } => {
                let member::Property::PropertyIdentifier(property) = &inner.member.property else {
                    return None;
                };
                let (root, mut path, mut optional) = Self::callee_path(&inner.member.object)?;
                if matches!(
                    inner.optional,
                    ast::expression::OptionalMemberKind::Optional
                ) {
                    optional.push(path.len());
                }
                path.push(property.name.dupe());
                Some((root, path, optional))
            }
            _ => None,
        }
    }

    fn target_for_call(&self, call_loc: &L, callee: &Expression<L, L>) -> Option<CalleeTarget<L>> {
        let (root, property_path, optional_receiver_paths) = Self::callee_path(callee)?;
        let root_use_loc = root.loc();
        if matches!(
            &**root,
            ExpressionInner::This { .. } | ExpressionInner::Super { .. }
        ) {
            let (binding_loc, name, mut root_kind) = self.receiver.dupe()?;
            if matches!(&**root, ExpressionInner::Super { .. }) {
                root_kind = match root_kind {
                    CalleeRootKind::InstanceThis => CalleeRootKind::InstanceSuper,
                    CalleeRootKind::StaticThis => CalleeRootKind::StaticSuper,
                    _ => return None,
                };
            }
            return Some(CalleeTarget {
                call_loc: call_loc.dupe(),
                callee_loc: callee.loc().dupe(),
                root_use_loc: root_use_loc.dupe(),
                root_binding_loc: binding_loc,
                root_name: name,
                is_global: false,
                provider_locs: Vec::new(),
                property_path,
                import: None,
                root_kind,
                optional_receiver_paths,
            });
        }
        let Some(def) = self.scope_info.def_of_use_opt(root_use_loc) else {
            // No scope definition: a global reference (e.g. a lib
            // declaration), classified by name through builtins. Only plain
            // identifiers qualify; member roots on unbound objects stay out.
            let ExpressionInner::Identifier { inner, .. } = &**callee else {
                return None;
            };
            return Some(CalleeTarget {
                call_loc: call_loc.dupe(),
                callee_loc: callee.loc().dupe(),
                root_use_loc: root_use_loc.dupe(),
                root_binding_loc: root_use_loc.dupe(),
                root_name: inner.name.dupe(),
                is_global: true,
                provider_locs: Vec::new(),
                property_path,
                import: None,
                root_kind: CalleeRootKind::Binding,
                optional_receiver_paths,
            });
        };
        let is_import = matches!(def.kind, Kind::Import { .. } | Kind::TsImport);
        // Functions and classes classify from their merged types, so they
        // are collected even when provider analysis does not attribute an
        // annotated provider to them.
        let is_named_definition = matches!(
            def.kind,
            Kind::Function | Kind::Class | Kind::DeclaredFunction | Kind::DeclaredClass
        );
        // Plain lexical value bindings. Type-like and internal machinery
        // bindings can never hold an assertion function.
        let is_lexical_value = matches!(
            def.kind,
            Kind::Var
                | Kind::Let
                | Kind::Const
                | Kind::DeclaredVar
                | Kind::DeclaredLet
                | Kind::DeclaredConst
                | Kind::Parameter
                | Kind::CatchParameter
                | Kind::ComponentParameter
        );
        let provider_locs = annotated_provider_locs(self.provider_info, def);
        if !is_import && !is_named_definition && !is_lexical_value && provider_locs.is_empty() {
            return None;
        }

        let root_binding_loc = def.locs.first().dupe();
        // The map only holds entries for `import` and `require` bindings, so a
        // hit here identifies the callee root as one of those regardless of
        // the binding kind.
        let import = self.imports.get(&root_binding_loc).cloned();

        Some(CalleeTarget {
            call_loc: call_loc.dupe(),
            callee_loc: callee.loc().dupe(),
            root_use_loc: root_use_loc.dupe(),
            root_binding_loc,
            root_name: def.actual_name.dupe(),
            is_global: false,
            provider_locs,
            property_path,
            import,
            root_kind: CalleeRootKind::Binding,
            optional_receiver_paths,
        })
    }

    fn with_function_receiver<'ast>(
        &mut self,
        loc: &'ast L,
        func: &'ast ast::function::Function<L, L>,
    ) -> Result<(), !> {
        let receiver = func.params.this_.as_ref().map(|param| {
            (
                param.loc.dupe(),
                FlowSmolStr::new_inline("this"),
                CalleeRootKind::Binding,
            )
        });
        let previous = mem::replace(&mut self.receiver, receiver);
        let result = ast_visitor::function_default(self, loc, func);
        self.receiver = previous;
        result
    }
}

/// The annotated, non-contextual providers of `def`.
fn annotated_provider_locs<L: LocSig>(provider_info: &ProviderInfo<L>, def: &Def<L>) -> Vec<L> {
    def.locs
        .iter()
        .filter_map(|def_loc| provider_info.providers_of_def(def_loc))
        .filter(|providers| matches!(providers.state, State::AnnotatedVar { contextual: false }))
        .flat_map(|providers| providers.providers.iter())
        .map(|provider| provider.reason.loc().dupe())
        .filter(|provider_loc| provider_info.is_provider_of_annotated(provider_loc))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// Whether the callee root used at `root_use_loc` has a declared type: a
/// global, an import, a named definition, or a binding with an annotated
/// provider. A call whose root lacks one cannot be classified unless its
/// signature type is available.
pub fn callee_root_has_declared_type<L: LocSig>(
    scope_info: &ScopeInfo<L>,
    provider_info: &ProviderInfo<L>,
    root_use_loc: &L,
) -> bool {
    let Some(def) = scope_info.def_of_use_opt(root_use_loc) else {
        return true;
    };
    matches!(
        def.kind,
        Kind::Import { .. }
            | Kind::TsImport
            | Kind::Function
            | Kind::Class
            | Kind::DeclaredFunction
            | Kind::DeclaredClass
    ) || !annotated_provider_locs(provider_info, def).is_empty()
}

/// Records the local bindings of one named-import map (value, type, or
/// typeof imports) as descriptors.
fn record_named_imports(
    imports: &mut BTreeMap<ALoc, ImportedCallee>,
    source: &FlowSmolStr,
    map: &BTreeMap<FlowSmolStr, BTreeMap<FlowSmolStr, Vec1<ImportedLocs>>>,
    is_value: bool,
) {
    for (remote, locals) in map {
        let remote = if remote.as_str() == "default" {
            ImportedName::Default
        } else {
            ImportedName::Named(remote.dupe())
        };
        for locs in locals.values().flat_map(|locs| locs.iter()) {
            imports.insert(
                ALoc::of_loc(locs.local_loc.dupe()),
                ImportedCallee {
                    source: source.dupe(),
                    remote: remote.clone(),
                    is_value,
                },
            );
        }
    }
}

/// Builds the import/require descriptor map from the file's signature: each
/// import-bound local and each plain `X = require('source')` binding, keyed
/// by its binding location. Destructured requires are skipped; each plain
/// require binding is described as the CommonJS export of its module.
pub fn file_sig_descriptors(file_sig: &FileSig) -> BTreeMap<ALoc, ImportedCallee> {
    let mut imports = BTreeMap::new();
    for require in file_sig.requires() {
        match require {
            Require::Import {
                source,
                named,
                ns,
                types,
                typesof,
                typesof_ns,
                type_ns,
                ..
            } => {
                record_named_imports(&mut imports, source.name(), named, true);
                record_named_imports(&mut imports, source.name(), types, false);
                record_named_imports(&mut imports, source.name(), typesof, false);
                for id in ns.iter() {
                    imports.insert(
                        ALoc::of_loc(id.loc().dupe()),
                        ImportedCallee {
                            source: source.name().dupe(),
                            remote: ImportedName::Namespace,
                            is_value: true,
                        },
                    );
                }
                for id in typesof_ns.iter().chain(type_ns.iter()) {
                    imports.insert(
                        ALoc::of_loc(id.loc().dupe()),
                        ImportedCallee {
                            source: source.name().dupe(),
                            remote: ImportedName::Namespace,
                            is_value: false,
                        },
                    );
                }
            }
            Require::Require {
                source,
                bindings: Some(RequireBindings::BindIdent(id)),
                ..
            } => {
                imports.insert(
                    ALoc::of_loc(id.loc().dupe()),
                    ImportedCallee {
                        source: source.name().dupe(),
                        remote: ImportedName::CommonJS,
                        is_value: true,
                    },
                );
            }
            _ => {}
        }
    }
    imports
}

impl<'ast, L: LocSig> AstVisitor<'ast, L> for Collector<'_, L> {
    fn normalize_loc(loc: &'ast L) -> &'ast L {
        loc
    }

    fn normalize_type(type_: &'ast L) -> &'ast L {
        type_
    }

    fn class_(&mut self, loc: &'ast L, class: &'ast ast::class::Class<L, L>) -> Result<(), !> {
        let binding = class.id.as_ref().map_or_else(
            || (loc.dupe(), FlowSmolStr::new_inline("<anonymous class>")),
            |id| (id.loc.dupe(), id.name.dupe()),
        );
        let previous = self.class_binding.replace(binding);
        let result = ast_visitor::class_default(self, loc, class);
        self.class_binding = previous;
        result
    }

    fn class_element(&mut self, element: &'ast BodyElement<L, L>) -> Result<(), !> {
        let static_ = match element {
            BodyElement::Method(method) => {
                self.object_key(&method.key)?;
                method.static_
            }
            BodyElement::Property(property) => {
                self.object_key(&property.key)?;
                property.static_
            }
            BodyElement::PrivateField(property) => property.static_,
            BodyElement::StaticBlock(_) => true,
            _ => return ast_visitor::class_element_default(self, element),
        };
        let receiver = self.class_binding.as_ref().map(|(loc, name)| {
            (
                loc.dupe(),
                name.dupe(),
                if static_ {
                    CalleeRootKind::StaticThis
                } else {
                    CalleeRootKind::InstanceThis
                },
            )
        });
        let previous_receiver = mem::replace(&mut self.receiver, receiver);
        let result = match element {
            BodyElement::Method(method) => {
                ast_visitor::function_default(self, &method.value.0, &method.value.1)
            }
            BodyElement::Property(property) => self.class_property_value(&property.value),
            BodyElement::PrivateField(property) => self.class_property_value(&property.value),
            BodyElement::StaticBlock(block) => ast_visitor::class_static_block_default(self, block),
            _ => Ok(()),
        };
        self.receiver = previous_receiver;
        result?;
        let decorators = match element {
            BodyElement::Method(method) => &method.decorators,
            BodyElement::Property(property) => &property.decorators,
            BodyElement::PrivateField(property) => &property.decorators,
            _ => return Ok(()),
        };
        for decorator in decorators.iter() {
            self.class_decorator(decorator)?;
        }
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &'ast L,
        func: &'ast ast::function::Function<L, L>,
    ) -> Result<(), !> {
        self.with_function_receiver(loc, func)
    }

    fn function_expression_or_method(
        &mut self,
        loc: &'ast L,
        func: &'ast ast::function::Function<L, L>,
    ) -> Result<(), !> {
        self.with_function_receiver(loc, func)
    }

    fn expression_statement(
        &mut self,
        loc: &'ast L,
        statement: &'ast ast::statement::Expression<L, L>,
    ) -> Result<(), !> {
        // The expression itself, or direct operands of a comma expression, must be a call.
        self.collect_assertion_flow_calls(&statement.expression);
        ast_visitor::expression_statement_default(self, loc, statement)
    }

    fn match_expression(
        &mut self,
        loc: &'ast L,
        expression: &'ast ast::expression::MatchExpression<L, L>,
    ) -> Result<(), !> {
        // Each case body is the result position of its branch, so a direct
        // assertion call is statement-like for abnormal control flow.
        for case in expression.cases.iter() {
            self.collect_assertion_flow_calls(&case.body);
        }
        ast_visitor::match_expression_default(self, loc, expression)
    }

    fn return_(&mut self, loc: &'ast L, ret: &'ast ast::statement::Return<L, L>) -> Result<(), !> {
        // A direct assertion call in return position never produces a value,
        // so it is statement-like for abnormal control flow.
        if let Some(argument) = ret.argument.as_ref() {
            self.collect_assertion_flow_calls(argument);
        }
        ast_visitor::return_default(self, loc, ret)
    }

    fn arrow_function(
        &mut self,
        loc: &'ast L,
        func: &'ast ast::function::Function<L, L>,
    ) -> Result<(), !> {
        // An arrow expression body is an implicit return, so a direct
        // assertion call there is statement-like for abnormal control flow.
        if let ast::function::Body::BodyExpression(body) = &func.body {
            self.collect_assertion_flow_calls(body);
        }
        ast_visitor::arrow_function_default(self, loc, func)
    }
}

/// Collects statically named callees that can be resolved from annotations alone.
/// `imports` maps import- and require-bound locals to their source module
/// (see `file_sig_descriptors`), keyed by binding location.
pub fn collect<L: LocSig>(
    program: &ast::Program<L, L>,
    scope_info: &ScopeInfo<L>,
    provider_info: &ProviderInfo<L>,
    imports: BTreeMap<L, ImportedCallee>,
) -> Vec<CalleeTarget<L>> {
    let mut collector = Collector {
        scope_info,
        provider_info,
        imports,
        targets: vec![],
        class_binding: None,
        receiver: None,
    };
    let Ok(()) = collector.program(program);
    collector.targets
}
