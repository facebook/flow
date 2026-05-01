/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub struct NamedBinding {
    pub remote_name: String,
    pub local_name: Option<String>,
}

pub enum Bindings {
    DefaultType(String),
    DefaultTypeof(String),
    Default(String),
    Named(Vec<NamedBinding>),
    NamedType(Vec<NamedBinding>),
    NamedTypeof(Vec<NamedBinding>),
    Namespace(String),
    TypeofNamespace(String),
}

#[derive(Clone, Copy)]
enum Placement {
    Above { skip_line: bool },
    Below { skip_line: bool },
    Replace,
}

use std::cmp::Ordering;
use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast::statement::import_declaration;
use flow_parser::ast_utils;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::comment_attachment;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper;
use flow_parser::polymorphic_ast_mapper::LocMapper;
use flow_parser_utils::ast_builder;
use flow_parser_utils::flow_ast_differ;
use flow_parser_utils_output::js_layout_generator;
use flow_parser_utils_output::pretty_printer;

use crate::contains_mapper::ContainsMapper;

mod import_source {
    use super::*;

    pub fn compare(a: &Option<String>, b: &Option<String>) -> Ordering {
        // TODO: sort global modules above ../ above ./
        a.cmp(b)
    }

    pub fn of_statement(stmt: &statement::Statement<Loc, Loc>) -> Option<String> {
        match &**stmt {
            StatementInner::ImportDeclaration { loc: _, inner } => {
                Some(inner.source.1.value.to_string())
            }
            _ => None,
        }
    }

    pub fn is_lower(source: &str) -> bool {
        source.len() > 1 && source.as_bytes()[0].is_ascii_lowercase()
    }
}

mod section {
    use super::*;

    #[derive(Clone, Copy)]
    pub enum T {
        ImportType,
        ImportValueFromRelative,
        ImportValueFromModule,
        Require,
    }

    pub fn compare(a: T, b: T) -> Ordering {
        match (a, b) {
            (T::ImportType, T::ImportType) => Ordering::Equal,
            (T::ImportValueFromRelative, T::ImportValueFromRelative) => Ordering::Equal,
            (T::ImportValueFromModule, T::ImportValueFromModule) => Ordering::Equal,
            (T::Require, T::Require) => Ordering::Equal,
            (T::ImportType, _) => Ordering::Less,
            (_, T::ImportType) => Ordering::Greater,
            (T::ImportValueFromRelative, T::ImportValueFromModule) => Ordering::Less,
            (T::ImportValueFromModule, T::ImportValueFromRelative) => Ordering::Greater,
            (_, T::Require) => Ordering::Less,
            (T::Require, _) => Ordering::Greater,
        }
    }

    pub fn of_statement(stmt: &statement::Statement<Loc, Loc>) -> T {
        match &**stmt {
            StatementInner::ImportDeclaration { loc: _, inner } => match inner.import_kind {
                statement::ImportKind::ImportType | statement::ImportKind::ImportTypeof => {
                    T::ImportType
                }
                statement::ImportKind::ImportValue => {
                    if import_source::is_lower(inner.source.1.value.as_str()) {
                        T::ImportValueFromModule
                    } else {
                        T::ImportValueFromRelative
                    }
                }
            },
            _ => T::Require,
        }
    }
}

fn loc_with_comments(stmt: &statement::Statement<Loc, Loc>) -> Loc {
    let bounds = comment_attachment::statement_comment_bounds(stmt);
    comment_attachment::expand_loc_with_comment_bounds(stmt.loc(), &bounds)
}

fn mk_default_import(
    loc: Option<Loc>,
    comments: Option<ast::Syntax<Loc, ()>>,
    import_kind: statement::ImportKind,
    from: &str,
    name: &str,
) -> statement::Statement<Loc, Loc> {
    let source = (LOC_NONE, ast_builder::string_literal(None, from));
    let default = Some(import_declaration::DefaultIdentifier {
        identifier: ast_builder::identifiers::identifier(None, name),
        remote_default_name_def_loc: None,
    });
    let specifiers = None;
    ast_builder::statements::import_declaration(
        loc,
        comments,
        None,
        import_kind,
        source,
        default,
        specifiers,
    )
}

fn mk_named_import(
    loc: Option<Loc>,
    comments: Option<ast::Syntax<Loc, ()>>,
    import_kind: statement::ImportKind,
    from: &str,
    names: &[NamedBinding],
) -> statement::Statement<Loc, Loc> {
    let source = (LOC_NONE, ast_builder::string_literal(None, from));
    let default = None;
    let specifiers = {
        let specifiers = names
            .iter()
            .map(
                |NamedBinding {
                     remote_name,
                     local_name,
                 }| {
                    let remote = ast_builder::identifiers::identifier(None, remote_name);
                    match local_name {
                        None => ast_builder::statements::named_import_specifier(
                            None, None, None, remote,
                        ),
                        Some(name) => ast_builder::statements::named_import_specifier(
                            None,
                            None,
                            Some(ast_builder::identifiers::identifier(None, name)),
                            remote,
                        ),
                    }
                },
            )
            .collect::<Vec<_>>();
        Some(import_declaration::Specifier::ImportNamedSpecifiers(
            specifiers,
        ))
    };
    ast_builder::statements::import_declaration(
        loc,
        comments,
        None,
        import_kind,
        source,
        default,
        specifiers,
    )
}

fn mk_namespace_import(
    loc: Option<Loc>,
    comments: Option<ast::Syntax<Loc, ()>>,
    import_kind: statement::ImportKind,
    from: &str,
    name: &str,
) -> statement::Statement<Loc, Loc> {
    let source = (LOC_NONE, ast_builder::string_literal(None, from));
    let default = None;
    let specifiers = {
        let id = ast_builder::identifiers::identifier(None, name);
        Some(import_declaration::Specifier::ImportNamespaceSpecifier((
            LOC_NONE, id,
        )))
    };
    ast_builder::statements::import_declaration(
        loc,
        comments,
        None,
        import_kind,
        source,
        default,
        specifiers,
    )
}

pub fn mk_import(bindings: &Bindings, from: &str) -> statement::Statement<Loc, Loc> {
    match bindings {
        Bindings::DefaultType(id_name) => {
            mk_default_import(None, None, statement::ImportKind::ImportType, from, id_name)
        }
        Bindings::DefaultTypeof(id_name) => mk_default_import(
            None,
            None,
            statement::ImportKind::ImportTypeof,
            from,
            id_name,
        ),
        Bindings::Default(id_name) => mk_default_import(
            None,
            None,
            statement::ImportKind::ImportValue,
            from,
            id_name,
        ),
        Bindings::Named(id_names) => mk_named_import(
            None,
            None,
            statement::ImportKind::ImportValue,
            from,
            id_names,
        ),
        Bindings::NamedType(id_names) => mk_named_import(
            None,
            None,
            statement::ImportKind::ImportType,
            from,
            id_names,
        ),
        Bindings::NamedTypeof(id_names) => mk_named_import(
            None,
            None,
            statement::ImportKind::ImportTypeof,
            from,
            id_names,
        ),
        Bindings::Namespace(id_name) => mk_namespace_import(
            None,
            None,
            statement::ImportKind::ImportValue,
            from,
            id_name,
        ),
        Bindings::TypeofNamespace(id_name) => mk_namespace_import(
            None,
            None,
            statement::ImportKind::ImportTypeof,
            from,
            id_name,
        ),
    }
}

fn string_of_statement(
    options: &js_layout_generator::Opts,
    stmt: &statement::Statement<Loc, Loc>,
) -> String {
    let layout = js_layout_generator::statement(options, false, stmt);
    let src = pretty_printer::print(true, &layout);
    src.contents()
}

fn sorted_insert<T: Clone>(cmp: &dyn Fn(&T, &T) -> Ordering, item: T, items: &[T]) -> Vec<T> {
    let (mut rev, inserted) =
        items
            .iter()
            .fold((Vec::new(), false), |(mut acc, inserted), next| {
                if !inserted && cmp(&item, next) != Ordering::Greater {
                    acc.push(item.clone());
                    acc.push(next.clone());
                    (acc, true)
                } else {
                    acc.push(next.clone());
                    (acc, inserted)
                }
            });
    if !inserted {
        rev.push(item);
    }
    rev
}

fn compare_specifiers(
    a: &import_declaration::NamedSpecifier<Loc, Loc>,
    b: &import_declaration::NamedSpecifier<Loc, Loc>,
) -> Ordering {
    let a_name = &a.remote.name;
    let b_name = &b.remote.name;
    a_name.cmp(b_name)
}

fn update_import(
    bindings: &Bindings,
    stmt: &statement::Statement<Loc, Loc>,
) -> (Loc, (Placement, statement::Statement<Loc, Loc>)) {
    let loc = loc_with_comments(stmt);
    match &**stmt {
        StatementInner::ImportDeclaration { loc: _, inner } => {
            let import_kind = inner.import_kind;
            let source = &inner.source;
            let default = &inner.default;
            let specifiers = &inner.specifiers;
            let comments = &inner.comments;
            let from = inner.source.1.value.as_str();
            let edit = match (bindings, default, specifiers) {
                (Bindings::DefaultType(bound_name), Some(def), _)
                | (Bindings::DefaultTypeof(bound_name), Some(def), _)
                | (Bindings::Default(bound_name), Some(def), _) => {
                    let name = &def.identifier.name;
                    if bound_name.as_str() == name.as_str() {
                        (Placement::Replace, stmt.dupe())
                    } else {
                        let placement = if bound_name.as_str() < name.as_str() {
                            Placement::Above { skip_line: false }
                        } else {
                            Placement::Below { skip_line: false }
                        };
                        (placement, mk_import(bindings, from))
                    }
                }
                // Namespace/TypeofNamespace with namespace specifier
                (
                    Bindings::Namespace(bound_name),
                    None,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier((_, ns_id))),
                )
                | (
                    Bindings::TypeofNamespace(bound_name),
                    None,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier((_, ns_id))),
                ) => {
                    let name = &ns_id.name;
                    if bound_name.as_str() == name.as_str() {
                        (Placement::Replace, stmt.dupe())
                    } else {
                        let placement = if bound_name.as_str() < name.as_str() {
                            Placement::Above { skip_line: false }
                        } else {
                            Placement::Below { skip_line: false }
                        };
                        (placement, mk_import(bindings, from))
                    }
                }
                (Bindings::DefaultType(_), None, Some(_))
                | (Bindings::DefaultTypeof(_), None, Some(_))
                | (Bindings::Default(_), None, Some(_)) => (
                    Placement::Above { skip_line: false },
                    mk_import(bindings, from),
                ),
                (
                    Bindings::Named(bound_names),
                    _,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(named_specifiers)),
                )
                | (
                    Bindings::NamedType(bound_names),
                    _,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(named_specifiers)),
                )
                | (
                    Bindings::NamedTypeof(bound_names),
                    _,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(named_specifiers)),
                ) => {
                    let kind = match (import_kind, bindings) {
                        (statement::ImportKind::ImportValue, Bindings::NamedType(_)) => {
                            Some(statement::ImportKind::ImportType)
                        }
                        (statement::ImportKind::ImportValue, Bindings::NamedTypeof(_)) => {
                            Some(statement::ImportKind::ImportTypeof)
                        }
                        _ => None,
                    };
                    let new_specifiers: Vec<import_declaration::NamedSpecifier<Loc, Loc>> =
                        bound_names
                            .iter()
                            .map(
                                |NamedBinding {
                                     remote_name,
                                     local_name,
                                 }| {
                                    import_declaration::NamedSpecifier {
                                        kind,
                                        kind_loc: None,
                                        local: local_name.as_ref().map(|name| {
                                            ast_builder::identifiers::identifier(None, name)
                                        }),
                                        remote: ast_builder::identifiers::identifier(
                                            None,
                                            remote_name,
                                        ),
                                        remote_name_def_loc: None,
                                    }
                                },
                            )
                            .collect();
                    let new_specifiers = if named_specifiers
                        .windows(2)
                        .all(|w| compare_specifiers(&w[0], &w[1]) != Ordering::Greater)
                    {
                        // sorted: fold new specifiers into existing via sorted_insert
                        new_specifiers.into_iter().fold(
                            named_specifiers.clone(),
                            |specifiers, new_specifier| {
                                sorted_insert(&compare_specifiers, new_specifier, &specifiers)
                            },
                        )
                    } else {
                        // not sorted: just append
                        let mut result = named_specifiers.clone();
                        result.extend(new_specifiers);
                        result
                    };
                    let new_specifiers_wrapped = Some(
                        import_declaration::Specifier::ImportNamedSpecifiers(new_specifiers),
                    );
                    let new_stmt = statement::Statement::new(StatementInner::ImportDeclaration {
                        loc: loc.dupe(),
                        inner: Arc::new(statement::ImportDeclaration {
                            import_kind,
                            source: source.clone(),
                            default: default.clone(),
                            specifiers: new_specifiers_wrapped,
                            attributes: None,
                            comments: comments.clone(),
                        }),
                    });
                    (Placement::Replace, new_stmt)
                }
                (Bindings::Named(_), Some(_), _)
                | (
                    Bindings::Named(_),
                    None,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                )
                | (Bindings::NamedType(_), Some(_), _)
                | (
                    Bindings::NamedType(_),
                    None,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                )
                | (Bindings::NamedTypeof(_), Some(_), _)
                | (
                    Bindings::NamedTypeof(_),
                    None,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                ) => (
                    Placement::Below { skip_line: false },
                    mk_import(bindings, from),
                ),
                (Bindings::Namespace(_), Some(_), _)
                | (Bindings::TypeofNamespace(_), Some(_), _) => (
                    Placement::Below { skip_line: false },
                    mk_import(bindings, from),
                ),
                (
                    Bindings::Namespace(_),
                    None,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                )
                | (
                    Bindings::TypeofNamespace(_),
                    None,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                ) => (
                    Placement::Above { skip_line: false },
                    mk_import(bindings, from),
                ),
                (_, None, None) => {
                    panic!("unexpected import with neither a default nor specifiers")
                }
            };
            (loc, edit)
        }
        _ => panic!("trying to update a non-import"),
    }
}

fn compare_kind_of_import(
    a: &statement::Statement<Loc, Loc>,
    b: &statement::Statement<Loc, Loc>,
) -> Ordering {
    fn compare_identifier(
        a: &ast::Identifier<Loc, Loc>,
        b: &ast::Identifier<Loc, Loc>,
    ) -> Ordering {
        a.name.cmp(&b.name)
    }
    match (&**a, &**b) {
        (
            StatementInner::ImportDeclaration {
                loc: _,
                inner: a_inner,
            },
            StatementInner::ImportDeclaration {
                loc: _,
                inner: b_inner,
            },
        ) => {
            let a_default = &a_inner.default;
            let a_specifiers = &a_inner.specifiers;
            let b_default = &b_inner.default;
            let b_specifiers = &b_inner.specifiers;
            match (a_default, b_default) {
                (Some(a_def), Some(b_def)) => {
                    compare_identifier(&a_def.identifier, &b_def.identifier)
                }
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => match (a_specifiers, b_specifiers) {
                    (
                        Some(import_declaration::Specifier::ImportNamespaceSpecifier((_, a_id))),
                        Some(import_declaration::Specifier::ImportNamespaceSpecifier((_, b_id))),
                    ) => compare_identifier(a_id, b_id),
                    (
                        Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                        Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                    ) => Ordering::Less,
                    (Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)), None) => {
                        Ordering::Less
                    }
                    (
                        Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                        Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                    ) => Ordering::Equal,
                    (
                        Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                        Some(import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                    ) => Ordering::Greater,
                    (Some(import_declaration::Specifier::ImportNamedSpecifiers(_)), None) => {
                        Ordering::Less
                    }
                    (None, None) => Ordering::Equal,
                    (None, Some(_)) => Ordering::Less,
                },
            }
        }
        _ => Ordering::Equal,
    }
}

fn compare_imports(
    a: &statement::Statement<Loc, Loc>,
    b: &statement::Statement<Loc, Loc>,
) -> Ordering {
    let k = section::compare(section::of_statement(a), section::of_statement(b));
    if k == Ordering::Equal {
        let k = import_source::compare(
            &import_source::of_statement(a),
            &import_source::of_statement(b),
        );
        if k == Ordering::Equal {
            compare_kind_of_import(a, b)
        } else {
            k
        }
    } else {
        k
    }
}

fn sorted_insertion_point(
    import: &statement::Statement<Loc, Loc>,
    imports: &[statement::Statement<Loc, Loc>],
) -> Option<(Loc, Placement)> {
    fn relative_placement(
        import: &statement::Statement<Loc, Loc>,
        current: &statement::Statement<Loc, Loc>,
    ) -> Placement {
        let k = section::compare(
            section::of_statement(import),
            section::of_statement(current),
        );
        if k == Ordering::Equal {
            let k = import_source::compare(
                &import_source::of_statement(import),
                &import_source::of_statement(current),
            );
            if k == Ordering::Less {
                Placement::Above { skip_line: false }
            } else {
                Placement::Below { skip_line: false }
            }
        } else if k == Ordering::Less {
            Placement::Above { skip_line: true }
        } else {
            Placement::Below { skip_line: true }
        }
    }

    fn helper(
        mut best: Option<(Loc, Placement)>,
        import: &statement::Statement<Loc, Loc>,
        remaining: &[statement::Statement<Loc, Loc>],
    ) -> Option<(Loc, Placement)> {
        if remaining.is_empty() {
            return best;
        }
        let current = &remaining[0];
        let rest = &remaining[1..];
        let placement = relative_placement(import, current);
        match placement {
            Placement::Above { skip_line: true } => match best {
                Some(_) => best,
                None => Some((current.loc().dupe(), placement)),
            },
            Placement::Above { skip_line: false } | Placement::Replace => {
                Some((current.loc().dupe(), placement))
            }
            Placement::Below { .. } => {
                best = Some((current.loc().dupe(), placement));
                helper(best, import, rest)
            }
        }
    }

    if imports
        .windows(2)
        .all(|w| compare_imports(&w[0], &w[1]) != Ordering::Greater)
    {
        helper(None, import, imports)
    } else {
        None
    }
}

fn last_loc(stmts: &[statement::Statement<Loc, Loc>]) -> Loc {
    stmts.last().expect("empty list").loc().dupe()
}

fn insertion_point(
    program_loc: &Loc,
    imports: &[statement::Statement<Loc, Loc>],
    body: &[statement::Statement<Loc, Loc>],
    import: &statement::Statement<Loc, Loc>,
) -> (Loc, Placement) {
    match sorted_insertion_point(import, imports) {
        Some(edit) => edit,
        None => match (imports, body) {
            ([], []) => (program_loc.dupe(), Placement::Below { skip_line: false }),
            ([_, ..], _) => {
                let loc = last_loc(imports);
                (loc, Placement::Below { skip_line: false })
            }
            ([], [first, ..]) => (first.loc().dupe(), Placement::Above { skip_line: true }),
        },
    }
}

fn section_matches_bindings(bindings: &Bindings, section: section::T) -> bool {
    match (section, bindings) {
        (
            section::T::ImportType,
            Bindings::DefaultType(_)
            | Bindings::DefaultTypeof(_)
            | Bindings::NamedType(_)
            | Bindings::NamedTypeof(_)
            | Bindings::TypeofNamespace(_),
        ) => true,
        (
            section::T::ImportType,
            Bindings::Default(_) | Bindings::Named(_) | Bindings::Namespace(_),
        ) => false,
        (section::T::Require, Bindings::Default(_)) => true,
        (section::T::Require, _) => false,
        (
            section::T::ImportValueFromRelative | section::T::ImportValueFromModule,
            Bindings::DefaultType(_)
            | Bindings::DefaultTypeof(_)
            | Bindings::NamedType(_)
            | Bindings::NamedTypeof(_)
            | Bindings::TypeofNamespace(_),
        ) => false,
        (
            section::T::ImportValueFromRelative | section::T::ImportValueFromModule,
            Bindings::Default(_) | Bindings::Named(_) | Bindings::Namespace(_),
        ) => true,
    }
}

fn existing_import<'a>(
    bindings: &Bindings,
    from: &str,
    imports: &'a [statement::Statement<Loc, Loc>],
) -> Option<&'a statement::Statement<Loc, Loc>> {
    let potentials: Vec<&statement::Statement<Loc, Loc>> = imports
        .iter()
        .filter(|stmt| {
            section_matches_bindings(bindings, section::of_statement(stmt))
                && import_source::of_statement(stmt).as_deref() == Some(from)
        })
        .collect();
    let bindings_type_matches =
        |default: &Option<import_declaration::DefaultIdentifier<Loc, Loc>>,
         specifiers: &Option<import_declaration::Specifier<Loc, Loc>>| {
            match (bindings, default, specifiers) {
                (Bindings::Default(_), Some(_), _) => true,
                (
                    Bindings::Named(_),
                    _,
                    Some(import_declaration::Specifier::ImportNamedSpecifiers(_)),
                ) => true,
                _ => false,
            }
        };
    fn closest<'a>(
        bindings_type_matches: &dyn Fn(
            &Option<import_declaration::DefaultIdentifier<Loc, Loc>>,
            &Option<import_declaration::Specifier<Loc, Loc>>,
        ) -> bool,
        potentials: &[&'a statement::Statement<Loc, Loc>],
    ) -> Option<&'a statement::Statement<Loc, Loc>> {
        match potentials {
            [] => None,
            [stmt] => Some(stmt),
            [stmt, stmts @ ..] => match &***stmt {
                StatementInner::ImportDeclaration { loc: _, inner }
                    if bindings_type_matches(&inner.default, &inner.specifiers) =>
                {
                    Some(stmt)
                }
                _ => closest(bindings_type_matches, stmts),
            },
        }
    }
    closest(&bindings_type_matches, &potentials)
}

fn get_change(
    program_loc: &Loc,
    imports: &[statement::Statement<Loc, Loc>],
    body: &[statement::Statement<Loc, Loc>],
    from: &str,
    bindings: &Bindings,
) -> (Loc, (Placement, statement::Statement<Loc, Loc>)) {
    let (loc, placement, stmt) = match existing_import(bindings, from, imports) {
        Some(stmt) => {
            let (loc, (placement, stmt)) = update_import(bindings, stmt);
            (loc, placement, stmt)
        }
        None => {
            let new_import = mk_import(bindings, from);
            let (loc, placement) = insertion_point(program_loc, imports, body, &new_import);
            (loc, placement, new_import)
        }
    };
    let loc = match placement {
        Placement::Above { .. } => loc.start_loc(),
        Placement::Below { .. } => loc.end_loc(),
        Placement::Replace => loc,
    };
    (loc, (placement, stmt))
}

fn string_of_change(
    options: &js_layout_generator::Opts,
    (loc, (placement, stmt)): &(Loc, (Placement, statement::Statement<Loc, Loc>)),
) -> (Loc, String) {
    let str = string_of_statement(options, stmt);
    let edit = match placement {
        Placement::Above { skip_line } => {
            if *skip_line {
                format!("{}\n\n", str)
            } else {
                format!("{}\n", str)
            }
        }
        Placement::Below { skip_line } => {
            if *skip_line {
                format!("\n\n{}", str)
            } else {
                format!("\n{}", str)
            }
        }
        Placement::Replace => str,
    };
    (loc.dupe(), edit)
}

fn compare_placement(p1: Placement, p2: Placement) -> Ordering {
    match (p1, p2) {
        (Placement::Above { skip_line: s1 }, Placement::Above { skip_line: s2 }) => s2.cmp(&s1),
        (Placement::Above { .. }, _) => Ordering::Less,
        (Placement::Below { skip_line: s1 }, Placement::Below { skip_line: s2 }) => s1.cmp(&s2),
        (Placement::Below { .. }, _) => Ordering::Greater,
        (Placement::Replace, Placement::Replace) => Ordering::Equal,
        (Placement::Replace, Placement::Above { .. }) => Ordering::Greater,
        (Placement::Replace, Placement::Below { .. }) => Ordering::Less,
    }
}

fn compare_changes(
    a: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
    b: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
) -> Ordering {
    let (loc1, (placement1, stmt1)) = a;
    let (loc2, (placement2, stmt2)) = b;
    let k = loc1.cmp(loc2);
    if k == Ordering::Equal {
        let k = compare_placement(*placement1, *placement2);
        if k == Ordering::Equal {
            compare_imports(stmt1, stmt2)
        } else {
            k
        }
    } else {
        k
    }
}

fn adjust_placements(
    changes: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
    if changes.is_empty() {
        return Vec::new();
    }

    fn loop_helper(
        mut prev: (Loc, (Placement, statement::Statement<Loc, Loc>)),
        mut acc: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
        rest: &[(Loc, (Placement, statement::Statement<Loc, Loc>))],
    ) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
        if rest.is_empty() {
            acc.push(prev);
            return acc;
        }
        let mut next = rest[0].clone();
        let remaining = &rest[1..];
        if prev.0 == next.0
            && compare_placement(prev.1.0, next.1.0) == Ordering::Equal
            && section::compare(
                section::of_statement(&prev.1.1),
                section::of_statement(&next.1.1),
            ) == Ordering::Equal
        {
            match prev.1.0 {
                Placement::Above { .. } => {
                    prev.1.0 = Placement::Above { skip_line: false };
                }
                Placement::Below { .. } => {
                    next.1.0 = Placement::Below { skip_line: false };
                }
                Placement::Replace => {}
            }
        }
        acc.push(prev);
        loop_helper(next, acc, remaining)
    }

    let mut iter = changes.into_iter();
    let hd = iter.next().unwrap();
    let tl: Vec<_> = iter.collect();
    loop_helper(hd, Vec::new(), &tl)
}

pub fn add_imports(
    options: &js_layout_generator::Opts,
    added_imports: &[(String, Bindings)],
    ast: &ast::Program<Loc, Loc>,
) -> Vec<(Loc, String)> {
    let program_loc = &ast.loc;
    let partition = flow_ast_differ::partition_imports(&ast.statements);
    let imports = &partition.imports;
    let body = &partition.body;
    let mut changes: Vec<_> = added_imports
        .iter()
        .map(|(from, bindings)| get_change(program_loc, imports, body, from, bindings))
        .collect();
    changes.sort_by(compare_changes);
    let changes = adjust_placements(changes);
    changes
        .iter()
        .map(|change| string_of_change(options, change))
        .collect()
}

pub fn add_import(
    options: &js_layout_generator::Opts,
    bindings: Bindings,
    from: String,
    ast: &ast::Program<Loc, Loc>,
) -> Vec<(Loc, String)> {
    add_imports(options, &[(from, bindings)], ast)
}

fn merge_imports(
    a: &statement::Statement<Loc, Loc>,
    b: &statement::Statement<Loc, Loc>,
) -> statement::Statement<Loc, Loc> {
    fn merge_comments(
        a: &Option<ast::Syntax<Loc, ()>>,
        b: &Option<ast::Syntax<Loc, ()>>,
    ) -> Option<ast::Syntax<Loc, ()>> {
        match (a, b) {
            (None, c) | (c, None) => c.clone(),
            (Some(a), Some(b)) => {
                let leading: Arc<[ast::Comment<Loc>]> =
                    a.leading.iter().chain(b.leading.iter()).cloned().collect();
                let trailing: Arc<[ast::Comment<Loc>]> = a
                    .trailing
                    .iter()
                    .chain(b.trailing.iter())
                    .cloned()
                    .collect();
                ast_utils::mk_comments_opt(Some(leading), Some(trailing))
            }
        }
    }

    fn merge_defaults(
        a: &Option<import_declaration::DefaultIdentifier<Loc, Loc>>,
        b: &Option<import_declaration::DefaultIdentifier<Loc, Loc>>,
    ) -> Option<import_declaration::DefaultIdentifier<Loc, Loc>> {
        match (a, b) {
            (Some(default), None) | (None, Some(default)) => Some(default.clone()),
            (None, None) => None,
            (Some(a_def), Some(b_def)) => {
                let name = &a_def.identifier.name;
                let b_name = &b_def.identifier.name;
                if name != b_name {
                    panic!("Can't merge imports with different defaults from the same file");
                }
                let comments =
                    merge_comments(&a_def.identifier.comments, &b_def.identifier.comments);
                Some(import_declaration::DefaultIdentifier {
                    identifier: ast::Identifier::new(ast::IdentifierInner {
                        loc: LOC_NONE,
                        name: name.dupe(),
                        comments,
                    }),
                    remote_default_name_def_loc: None,
                })
            }
        }
    }

    fn merge_named_specifiers(
        a: &[import_declaration::NamedSpecifier<Loc, Loc>],
        b: &[import_declaration::NamedSpecifier<Loc, Loc>],
    ) -> Vec<import_declaration::NamedSpecifier<Loc, Loc>> {
        let mut result: Vec<_> = a.iter().rev().chain(b.iter()).cloned().collect();
        result.sort_by(compare_specifiers);
        result
    }

    fn merge_namespace_specifier(
        a: &(Loc, ast::Identifier<Loc, Loc>),
        b: &(Loc, ast::Identifier<Loc, Loc>),
    ) -> (Loc, ast::Identifier<Loc, Loc>) {
        let name = &a.1.name;
        let b_name = &b.1.name;
        if name != b_name {
            panic!("Can't merge imports with different namespace imports from the same file");
        }
        let comments = merge_comments(&a.1.comments, &b.1.comments);
        (
            LOC_NONE,
            ast::Identifier::new(ast::IdentifierInner {
                loc: LOC_NONE,
                name: name.dupe(),
                comments,
            }),
        )
    }

    fn merge_specifiers(
        a: &Option<import_declaration::Specifier<Loc, Loc>>,
        b: &Option<import_declaration::Specifier<Loc, Loc>>,
    ) -> Option<import_declaration::Specifier<Loc, Loc>> {
        match (a, b) {
            (None, None) => None,
            (Some(_), None) => a.clone(),
            (None, Some(_)) => b.clone(),
            (
                Some(import_declaration::Specifier::ImportNamedSpecifiers(a_named)),
                Some(import_declaration::Specifier::ImportNamedSpecifiers(b_named)),
            ) => Some(import_declaration::Specifier::ImportNamedSpecifiers(
                merge_named_specifiers(a_named, b_named),
            )),
            (
                Some(import_declaration::Specifier::ImportNamespaceSpecifier(a_ns)),
                Some(import_declaration::Specifier::ImportNamespaceSpecifier(b_ns)),
            ) => Some(import_declaration::Specifier::ImportNamespaceSpecifier(
                merge_namespace_specifier(a_ns, b_ns),
            )),
            _ => panic!("Can't merge imports with named and namespace specifiers"),
        }
    }

    match (&**a, &**b) {
        (
            StatementInner::ImportDeclaration {
                loc: _,
                inner: a_inner,
            },
            StatementInner::ImportDeclaration {
                loc: _,
                inner: b_inner,
            },
        ) => {
            let a_from = a_inner.source.1.value.as_str();
            let b_from = b_inner.source.1.value.as_str();
            if a_from != b_from {
                panic!("Can't merge imports from different files");
            }
            if a_inner.import_kind != b_inner.import_kind {
                panic!("Can't merge imports of different kinds");
            }
            let default = merge_defaults(&a_inner.default, &b_inner.default);
            let specifiers = merge_specifiers(&a_inner.specifiers, &b_inner.specifiers);
            let comments = merge_comments(&a_inner.comments, &b_inner.comments);
            statement::Statement::new(StatementInner::ImportDeclaration {
                loc: LOC_NONE,
                inner: Arc::new(statement::ImportDeclaration {
                    import_kind: a_inner.import_kind,
                    source: a_inner.source.clone(),
                    default,
                    specifiers,
                    attributes: None,
                    comments,
                }),
            })
        }
        _ => panic!("Can only merge 2 ImportDeclarations"),
    }
}

fn merge_changes(
    a: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
    b: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
) -> (Loc, (Placement, statement::Statement<Loc, Loc>)) {
    let (a_loc, (a_placement, a_stmt)) = a;
    let (_, (_, b_stmt)) = b;
    (a_loc.dupe(), (*a_placement, merge_imports(a_stmt, b_stmt)))
}

fn merge_consecutive_duplicates(
    changes: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
    fn loop_helper(
        mut prev: (Loc, (Placement, statement::Statement<Loc, Loc>)),
        mut acc: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
        rest: &[(Loc, (Placement, statement::Statement<Loc, Loc>))],
    ) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
        if rest.is_empty() {
            acc.push(prev);
            return acc;
        }
        let hd = &rest[0];
        let tl = &rest[1..];
        if compare_changes(hd, &prev) == Ordering::Equal {
            prev = merge_changes(hd, &prev);
            loop_helper(prev, acc, tl)
        } else {
            acc.push(prev);
            loop_helper(hd.clone(), acc, tl)
        }
    }
    if changes.is_empty() {
        return Vec::new();
    }
    let mut iter = changes.into_iter();
    let hd = iter.next().unwrap();
    let tl: Vec<_> = iter.collect();
    loop_helper(hd, Vec::new(), &tl)
}

fn sort_and_dedup_changes(
    mut changes: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
    changes.sort_by(compare_changes);
    merge_consecutive_duplicates(changes)
}

struct LocStripper;

impl LocMapper<Loc, Loc, Loc, Loc> for LocStripper {
    fn on_loc_annot(&mut self, _loc: &Loc) -> Result<Loc, !> {
        Ok(LOC_NONE)
    }
    fn on_type_annot(&mut self, _annot: &Loc) -> Result<Loc, !> {
        Ok(LOC_NONE)
    }
}

pub fn organize_imports(
    options: &js_layout_generator::Opts,
    ast: &ast::Program<Loc, Loc>,
) -> Vec<(Loc, String)> {
    let partition = flow_ast_differ::partition_imports(&ast.statements);
    let imports = &partition.imports;
    let mut no_locs = LocStripper;
    match imports.as_slice() {
        [] => Vec::new(),
        [first_import, tl @ ..] => {
            let loc = {
                let start_loc = loc_with_comments(first_import);
                match tl.last() {
                    Some(last_import) => {
                        let end_loc = loc_with_comments(last_import);
                        Loc::between(&start_loc, &end_loc)
                    }
                    None => start_loc,
                }
            };
            let edit = {
                let changes: Vec<_> = imports
                    .iter()
                    .map(|stmt| {
                        let Ok(mapped_stmt) = polymorphic_ast_mapper::statement(&mut no_locs, stmt);
                        (
                            loc.dupe(),
                            (Placement::Above { skip_line: true }, mapped_stmt),
                        )
                    })
                    .collect();
                let changes = sort_and_dedup_changes(changes);
                let changes = adjust_placements(changes);
                let joined: String = changes
                    .iter()
                    .map(|change| string_of_change(options, change).1)
                    .collect();
                joined
                    .strip_suffix("\n\n")
                    .expect("expected trailing \\n\\n")
                    .to_string()
            };
            vec![(loc, edit)]
        }
    }
}

mod identifier_finder {
    use flow_parser::ast_visitor::AstVisitor;
    use flow_parser::ast_visitor::map_expression_default;
    use flow_parser::ast_visitor::map_identifier_default;
    use flow_parser::ast_visitor::map_program_default;
    use flow_parser::ast_visitor::map_statement_default;

    use super::*;

    pub enum Kind {
        TypeIdentifier,
        ValueIdentifier,
    }

    pub struct Mapper {
        contains: ContainsMapper,
        pub found: Option<Kind>,
    }

    impl Mapper {
        pub fn new(target: Loc) -> Self {
            Self {
                contains: ContainsMapper::new(target),
                found: None,
            }
        }
    }

    impl AstVisitor<'_, Loc> for Mapper {
        fn normalize_loc(loc: &Loc) -> &Loc {
            loc
        }

        fn normalize_type(type_: &Loc) -> &Loc {
            type_
        }

        fn map_identifier(&mut self, id: &ast::Identifier<Loc, Loc>) -> ast::Identifier<Loc, Loc> {
            if self.found.is_none() && self.contains.target_contained_by(&id.loc) {
                self.found = Some(Kind::ValueIdentifier);
            }
            map_identifier_default(self, id)
        }

        fn map_type_identifier(
            &mut self,
            id: &ast::Identifier<Loc, Loc>,
        ) -> ast::Identifier<Loc, Loc> {
            if self.found.is_none() && self.contains.target_contained_by(&id.loc) {
                self.found = Some(Kind::TypeIdentifier);
            }
            map_identifier_default(self, id)
        }

        fn map_program(&mut self, program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
            if self.contains.should_map_program(program) {
                map_program_default(self, program)
            } else {
                program.clone()
            }
        }

        fn map_statement(
            &mut self,
            stmt: &ast::statement::Statement<Loc, Loc>,
        ) -> ast::statement::Statement<Loc, Loc> {
            if self.contains.should_map_statement(stmt) {
                map_statement_default(self, stmt)
            } else {
                stmt.dupe()
            }
        }

        fn map_expression(
            &mut self,
            expr: &ast::expression::Expression<Loc, Loc>,
        ) -> ast::expression::Expression<Loc, Loc> {
            if self.contains.should_map_expression(expr) {
                map_expression_default(self, expr)
            } else {
                expr.dupe()
            }
        }
    }
}

pub fn loc_is_type(ast: &ast::Program<Loc, Loc>, loc: Loc) -> bool {
    let mut mapper = identifier_finder::Mapper::new(loc);
    let _ast = mapper.map_program(ast);
    match mapper.found {
        Some(identifier_finder::Kind::ValueIdentifier) => false,
        Some(identifier_finder::Kind::TypeIdentifier) => true,
        None => false,
    }
}
