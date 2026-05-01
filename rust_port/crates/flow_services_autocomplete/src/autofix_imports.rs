/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;

use flow_parser::ast;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::flow_ast_differ;
use flow_parser_utils_output::js_layout_generator;
use flow_parser_utils_output::pretty_printer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedBinding {
    pub remote_name: String,
    pub local_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placement {
    Above { skip_line: bool },
    Below { skip_line: bool },
    Replace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Section {
    ImportType,
    ImportValueFromRelative,
    ImportValueFromModule,
    Require,
}

fn is_sorted<T>(items: &[T], cmp: impl Fn(&T, &T) -> Ordering) -> bool {
    items
        .windows(2)
        .all(|pair| cmp(&pair[0], &pair[1]) != Ordering::Greater)
}

fn compare_import_source(a: &Option<String>, b: &Option<String>) -> Ordering {
    // TODO: sort global modules above ../ above ./
    match (a, b) {
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Less,
        (Some(_), None) => Ordering::Greater,
        (Some(a), Some(b)) => a.cmp(b),
    }
}

fn import_source_of_statement(stmt: &statement::Statement<Loc, Loc>) -> Option<String> {
    match &**stmt {
        StatementInner::ImportDeclaration { inner, .. } => Some(inner.source.1.value.to_string()),
        _ => {
            // TODO: handle requires
            None
        }
    }
}

fn import_source_is_lower(source: &str) -> bool {
    source.len() > 1
        && source
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_lowercase())
}

fn section_of_statement(stmt: &statement::Statement<Loc, Loc>) -> Section {
    match &**stmt {
        StatementInner::ImportDeclaration { inner, .. } => match inner.import_kind {
            statement::ImportKind::ImportType | statement::ImportKind::ImportTypeof => {
                Section::ImportType
            }
            statement::ImportKind::ImportValue => {
                if import_source_is_lower(inner.source.1.value.as_ref()) {
                    Section::ImportValueFromModule
                } else {
                    Section::ImportValueFromRelative
                }
            }
        },
        _ => Section::Require,
    }
}

fn loc_with_comments(stmt: &statement::Statement<Loc, Loc>) -> Loc {
    flow_ast_differ::expand_statement_comment_bounds(stmt)
}

fn mk_default_import(
    loc: Option<Loc>,
    comments: Option<ast::Syntax<Loc, ()>>,
    import_kind: statement::ImportKind,
    from: &str,
    name: &str,
) -> statement::Statement<Loc, Loc> {
    let source = (Loc::none(), ast_builder::string_literal(None, from));
    let default = Some(statement::import_declaration::DefaultIdentifier {
        identifier: ast_builder::identifiers::identifier(None, name),
        remote_default_name_def_loc: None,
    });
    ast_builder::statements::import_declaration(
        loc,
        comments,
        None,
        import_kind,
        source,
        default,
        None,
    )
}

fn mk_named_import(
    loc: Option<Loc>,
    comments: Option<ast::Syntax<Loc, ()>>,
    import_kind: statement::ImportKind,
    from: &str,
    names: &[NamedBinding],
) -> statement::Statement<Loc, Loc> {
    let source = (Loc::none(), ast_builder::string_literal(None, from));
    let specifiers = names
        .iter()
        .map(|binding| {
            let remote = ast_builder::identifiers::identifier(None, &binding.remote_name);
            let local = binding
                .local_name
                .as_ref()
                .map(|name| ast_builder::identifiers::identifier(None, name));
            ast_builder::statements::named_import_specifier(None, None, local, remote)
        })
        .collect();
    ast_builder::statements::named_import_declaration(
        loc,
        comments,
        import_kind,
        source,
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
    let source = (Loc::none(), ast_builder::string_literal(None, from));
    let specifiers = Some(
        statement::import_declaration::Specifier::ImportNamespaceSpecifier((
            Loc::none(),
            ast_builder::identifiers::identifier(None, name),
        )),
    );
    ast_builder::statements::import_declaration(
        loc,
        comments,
        None,
        import_kind,
        source,
        None,
        specifiers,
    )
}

fn mk_import(bindings: &Bindings, from: &str) -> statement::Statement<Loc, Loc> {
    match bindings {
        Bindings::DefaultType(name) => {
            mk_default_import(None, None, statement::ImportKind::ImportType, from, name)
        }
        Bindings::DefaultTypeof(name) => {
            mk_default_import(None, None, statement::ImportKind::ImportTypeof, from, name)
        }
        Bindings::Default(name) => {
            mk_default_import(None, None, statement::ImportKind::ImportValue, from, name)
        }
        Bindings::Named(names) => {
            mk_named_import(None, None, statement::ImportKind::ImportValue, from, names)
        }
        Bindings::NamedType(names) => {
            mk_named_import(None, None, statement::ImportKind::ImportType, from, names)
        }
        Bindings::NamedTypeof(names) => {
            mk_named_import(None, None, statement::ImportKind::ImportTypeof, from, names)
        }
        Bindings::Namespace(name) => {
            mk_namespace_import(None, None, statement::ImportKind::ImportValue, from, name)
        }
        Bindings::TypeofNamespace(name) => {
            mk_namespace_import(None, None, statement::ImportKind::ImportTypeof, from, name)
        }
    }
}

fn string_of_statement(
    options: &js_layout_generator::Opts,
    stmt: &statement::Statement<Loc, Loc>,
) -> String {
    let layout = js_layout_generator::statement(options, false, stmt);
    pretty_printer::print(true, &layout).contents()
}

fn sorted_insert<T: Clone>(cmp: impl Fn(&T, &T) -> Ordering, item: T, items: &[T]) -> Vec<T> {
    let mut rev = Vec::new();
    let mut inserted = false;
    for next in items {
        if !inserted && cmp(&item, next) != Ordering::Greater {
            rev.push(item.clone());
            rev.push(next.clone());
            inserted = true;
        } else {
            rev.push(next.clone());
        }
    }
    if !inserted {
        rev.push(item);
    }
    rev
}

fn compare_specifiers(
    a: &statement::import_declaration::NamedSpecifier<Loc, Loc>,
    b: &statement::import_declaration::NamedSpecifier<Loc, Loc>,
) -> Ordering {
    a.remote.name.cmp(&b.remote.name)
}

fn update_import(
    bindings: &Bindings,
    stmt: &statement::Statement<Loc, Loc>,
) -> (Loc, (Placement, statement::Statement<Loc, Loc>)) {
    let loc = loc_with_comments(stmt);
    match &**stmt {
        StatementInner::ImportDeclaration { inner, .. } => {
            let import_kind = inner.import_kind;
            let source = inner.source.clone();
            let from = source.1.value.to_string();
            let default = inner.default.clone();
            let specifiers = inner.specifiers.clone();
            let comments = inner.comments.clone();
            let edit = match (bindings, &default, &specifiers) {
                (
                    Bindings::DefaultType(bound_name)
                    | Bindings::DefaultTypeof(bound_name)
                    | Bindings::Default(bound_name),
                    Some(default),
                    _,
                ) => {
                    let name = default.identifier.name.to_string();
                    if bound_name == &name {
                        // this should never happen, the name is already in scope
                        (Placement::Replace, stmt.clone())
                    } else {
                        // an `import Baz from 'foo'` already exists. weird, but insert
                        // an `import Foo from 'foo'` anyway. (and similar for `import * as Baz ...`)
                        let placement = if bound_name < &name {
                            Placement::Above { skip_line: false }
                        } else {
                            Placement::Below { skip_line: false }
                        };
                        (placement, mk_import(bindings, &from))
                    }
                }
                (
                    Bindings::Namespace(bound_name) | Bindings::TypeofNamespace(bound_name),
                    None,
                    Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier((
                        _,
                        id,
                    ))),
                ) => {
                    let name = id.name.to_string();
                    if bound_name == &name {
                        // this should never happen, the name is already in scope
                        (Placement::Replace, stmt.clone())
                    } else {
                        let placement = if bound_name < &name {
                            Placement::Above { skip_line: false }
                        } else {
                            Placement::Below { skip_line: false }
                        };
                        (placement, mk_import(bindings, &from))
                    }
                }
                (
                    Bindings::DefaultType(_) | Bindings::DefaultTypeof(_) | Bindings::Default(_),
                    None,
                    Some(_),
                ) => {
                    // a `import {bar} from 'foo'` or `import * as Foo from 'foo'` already exists.
                    // rather than change it to `import Foo, {bar} from 'foo'`, we choose to insert
                    // a separate import. TODO: maybe make this a config option?
                    (
                        Placement::Above { skip_line: false },
                        mk_import(bindings, &from),
                    )
                }
                (
                    Bindings::Named(bound_names)
                    | Bindings::NamedType(bound_names)
                    | Bindings::NamedTypeof(bound_names),
                    _,
                    Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(
                        specifiers,
                    )),
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
                    let extra_specifiers: Vec<_> = bound_names
                        .iter()
                        .map(|binding| {
                            let local = binding
                                .local_name
                                .as_ref()
                                .map(|name| ast_builder::identifiers::identifier(None, name));
                            let remote =
                                ast_builder::identifiers::identifier(None, &binding.remote_name);
                            statement::import_declaration::NamedSpecifier {
                                kind,
                                kind_loc: None,
                                local,
                                remote,
                                remote_name_def_loc: None,
                            }
                        })
                        .collect();
                    let merged_specifiers = if is_sorted(specifiers, compare_specifiers) {
                        extra_specifiers
                            .into_iter()
                            .fold(specifiers.clone(), |acc, spec| {
                                sorted_insert(compare_specifiers, spec, &acc)
                            })
                    } else {
                        let mut merged = specifiers.clone();
                        merged.extend(extra_specifiers);
                        merged
                    };
                    let stmt = statement::Statement::new(StatementInner::ImportDeclaration {
                        loc: loc.clone(),
                        inner: std::sync::Arc::new(statement::ImportDeclaration {
                            import_kind,
                            source,
                            default,
                            specifiers: Some(
                                statement::import_declaration::Specifier::ImportNamedSpecifiers(
                                    merged_specifiers,
                                ),
                            ),
                            attributes: None,
                            comments,
                        }),
                    });
                    (Placement::Replace, stmt)
                }
                (
                    Bindings::Named(_) | Bindings::NamedType(_) | Bindings::NamedTypeof(_),
                    Some(_),
                    _,
                )
                | (
                    Bindings::Named(_) | Bindings::NamedType(_) | Bindings::NamedTypeof(_),
                    None,
                    Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                ) => {
                    // trying to insert a named specifier, but a default or namespace import already
                    // exists. rather than change it to `import Foo, {bar} from 'foo'`, we choose to
                    // insert a separate import `import {bar} from 'foo'` below the namespace/default
                    // import. TODO: maybe make this a config option?
                    (
                        Placement::Below { skip_line: false },
                        mk_import(bindings, &from),
                    )
                }
                (Bindings::Namespace(_) | Bindings::TypeofNamespace(_), Some(_), _) => {
                    // trying to insert a namespace specifier, but a default import already exists.
                    // rather than change it to `import * as Foo, Bar from 'foo'`, we choose to
                    // insert a separate import `import * as Foo from 'foo'` below the default.
                    // TODO: maybe make this a config option?
                    (
                        Placement::Below { skip_line: false },
                        mk_import(bindings, &from),
                    )
                }
                (
                    Bindings::Namespace(_) | Bindings::TypeofNamespace(_),
                    None,
                    Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                ) => {
                    // trying to insert a namespace specifier, but a named import already exists.
                    // rather than change it to `import * as Foo, {bar} from 'foo'`, we choose to
                    // insert a separate import `import * as Foo from 'foo'` above the named import.
                    // TODO: maybe make this a config option?
                    (
                        Placement::Above { skip_line: false },
                        mk_import(bindings, &from),
                    )
                }
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
    match (&**a, &**b) {
        (
            StatementInner::ImportDeclaration { inner: a_inner, .. },
            StatementInner::ImportDeclaration { inner: b_inner, .. },
        ) => {
            let compare_identifier =
                |a: &ast::Identifier<Loc, Loc>, b: &ast::Identifier<Loc, Loc>| a.name.cmp(&b.name);
            match (&a_inner.default, &b_inner.default) {
                (Some(a_default), Some(b_default)) => {
                    compare_identifier(&a_default.identifier, &b_default.identifier)
                }
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => match (&a_inner.specifiers, &b_inner.specifiers) {
                    (
                        Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier((
                            _,
                            a_id,
                        ))),
                        Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier((
                            _,
                            b_id,
                        ))),
                    ) => compare_identifier(a_id, b_id),
                    (
                        Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                    ) => Ordering::Less,
                    (
                        Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                        None,
                    ) => Ordering::Less,
                    (
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                    ) => Ordering::Equal,
                    (
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                        Some(statement::import_declaration::Specifier::ImportNamespaceSpecifier(_)),
                    ) => Ordering::Greater,
                    (
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_)),
                        None,
                    ) => Ordering::Less,
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
    let section_cmp = section_of_statement(a).cmp(&section_of_statement(b));
    if section_cmp != Ordering::Equal {
        return section_cmp;
    }
    let source_cmp = compare_import_source(
        &import_source_of_statement(a),
        &import_source_of_statement(b),
    );
    if source_cmp != Ordering::Equal {
        return source_cmp;
    }
    compare_kind_of_import(a, b)
}

fn sorted_insertion_point(
    import: &statement::Statement<Loc, Loc>,
    imports: &[statement::Statement<Loc, Loc>],
) -> Option<(Loc, Placement)> {
    fn relative_placement(
        import: &statement::Statement<Loc, Loc>,
        current: &statement::Statement<Loc, Loc>,
    ) -> Placement {
        let section_cmp = section_of_statement(import).cmp(&section_of_statement(current));
        if section_cmp == Ordering::Equal {
            let source_cmp = compare_import_source(
                &import_source_of_statement(import),
                &import_source_of_statement(current),
            );
            if source_cmp == Ordering::Less {
                Placement::Above { skip_line: false }
            } else {
                Placement::Below { skip_line: false }
            }
        } else if section_cmp == Ordering::Less {
            Placement::Above { skip_line: true }
        } else {
            Placement::Below { skip_line: true }
        }
    }

    fn helper(
        best: Option<(Loc, Placement)>,
        import: &statement::Statement<Loc, Loc>,
        imports: &[statement::Statement<Loc, Loc>],
    ) -> Option<(Loc, Placement)> {
        match imports.split_first() {
            None => best,
            Some((current, rest)) => {
                let placement = relative_placement(import, current);
                match placement {
                    Placement::Above { skip_line: true } => {
                        best.or_else(|| Some((current.loc().clone(), placement)))
                    }
                    Placement::Above { skip_line: false } | Placement::Replace => {
                        Some((current.loc().clone(), placement))
                    }
                    Placement::Below { .. } => {
                        let best = Some((current.loc().clone(), placement));
                        helper(best, import, rest)
                    }
                }
            }
        }
    }

    if is_sorted(imports, compare_imports) {
        helper(None, import, imports)
    } else {
        None
    }
}

fn last_loc(imports: &[statement::Statement<Loc, Loc>]) -> Loc {
    imports.last().expect("empty list").loc().clone()
}

fn insertion_point(
    program_loc: &Loc,
    imports: &[statement::Statement<Loc, Loc>],
    body: &[statement::Statement<Loc, Loc>],
    import: &statement::Statement<Loc, Loc>,
) -> (Loc, Placement) {
    match sorted_insertion_point(import, imports) {
        Some(edit) => edit,
        None => match (imports.is_empty(), body.first()) {
            (true, None) => (program_loc.clone(), Placement::Below { skip_line: false }),
            (false, _) => (last_loc(imports), Placement::Below { skip_line: false }),
            (true, Some(stmt)) => (stmt.loc().clone(), Placement::Above { skip_line: true }),
        },
    }
}

fn section_matches_bindings(bindings: &Bindings, section: Section) -> bool {
    match (section, bindings) {
        (
            Section::ImportType,
            Bindings::DefaultType(_)
            | Bindings::DefaultTypeof(_)
            | Bindings::NamedType(_)
            | Bindings::NamedTypeof(_)
            | Bindings::TypeofNamespace(_),
        ) => true,
        (
            Section::ImportType,
            Bindings::Default(_) | Bindings::Named(_) | Bindings::Namespace(_),
        ) => false,
        (Section::Require, Bindings::Default(_)) => true,
        (
            Section::Require,
            Bindings::DefaultType(_)
            | Bindings::DefaultTypeof(_)
            | Bindings::Named(_)
            | Bindings::NamedType(_)
            | Bindings::NamedTypeof(_)
            | Bindings::Namespace(_)
            | Bindings::TypeofNamespace(_),
        ) => false,
        (
            Section::ImportValueFromRelative | Section::ImportValueFromModule,
            Bindings::DefaultType(_)
            | Bindings::DefaultTypeof(_)
            | Bindings::NamedType(_)
            | Bindings::NamedTypeof(_)
            | Bindings::TypeofNamespace(_),
        ) => false,
        (
            Section::ImportValueFromRelative | Section::ImportValueFromModule,
            Bindings::Default(_) | Bindings::Named(_) | Bindings::Namespace(_),
        ) => true,
    }
}

fn existing_import(
    bindings: &Bindings,
    from: &str,
    imports: &[statement::Statement<Loc, Loc>],
) -> Option<statement::Statement<Loc, Loc>> {
    let potentials: Vec<_> = imports
        .iter()
        .filter(|stmt| {
            section_matches_bindings(bindings, section_of_statement(stmt))
                && import_source_of_statement(stmt).as_deref() == Some(from)
        })
        .cloned()
        .collect();
    let bindings_type_matches =
        |default: &Option<statement::import_declaration::DefaultIdentifier<Loc, Loc>>,
         specifiers: &Option<statement::import_declaration::Specifier<Loc, Loc>>| {
            matches!(
                (bindings, default, specifiers),
                (Bindings::Default(_), Some(_), _)
                    | (
                        Bindings::Named(_),
                        _,
                        Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(_))
                    )
            )
        };
    let mut iter = potentials.into_iter();
    while let Some(stmt) = iter.next() {
        match &*stmt {
            StatementInner::ImportDeclaration { inner, .. }
                if bindings_type_matches(&inner.default, &inner.specifiers) =>
            {
                return Some(stmt);
            }
            _ if iter.len() == 0 => return Some(stmt),
            _ => {}
        }
    }
    None
}

fn get_change(
    program_loc: &Loc,
    imports: &[statement::Statement<Loc, Loc>],
    body: &[statement::Statement<Loc, Loc>],
    added_import: (&str, &Bindings),
) -> (Loc, (Placement, statement::Statement<Loc, Loc>)) {
    let (from, bindings) = added_import;
    let (loc, placement, stmt) = match existing_import(bindings, from, imports) {
        Some(stmt) => {
            let (loc, (placement, stmt)) = update_import(bindings, &stmt);
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
    change: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
) -> (Loc, String) {
    let (loc, (placement, stmt)) = change;
    let str = string_of_statement(options, stmt);
    let edit = match placement {
        Placement::Above { skip_line } => {
            if *skip_line {
                format!("{str}\n\n")
            } else {
                format!("{str}\n")
            }
        }
        Placement::Below { skip_line } => {
            if *skip_line {
                format!("\n\n{str}")
            } else {
                format!("\n{str}")
            }
        }
        Placement::Replace => str,
    };
    (loc.clone(), edit)
}

fn compare_placement(a: Placement, b: Placement) -> Ordering {
    match (a, b) {
        (Placement::Above { skip_line: a_skip }, Placement::Above { skip_line: b_skip }) => {
            b_skip.cmp(&a_skip)
        }
        (Placement::Above { .. }, Placement::Below { .. } | Placement::Replace) => Ordering::Less,
        (Placement::Below { skip_line: a_skip }, Placement::Below { skip_line: b_skip }) => {
            a_skip.cmp(&b_skip)
        }
        (Placement::Below { .. }, Placement::Above { .. } | Placement::Replace) => {
            Ordering::Greater
        }
        (Placement::Replace, Placement::Replace) => Ordering::Equal,
        (Placement::Replace, Placement::Above { .. }) => Ordering::Greater,
        (Placement::Replace, Placement::Below { .. }) => Ordering::Less,
    }
}

fn compare_changes(
    a: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
    b: &(Loc, (Placement, statement::Statement<Loc, Loc>)),
) -> Ordering {
    let loc_cmp = a.0.cmp(&b.0);
    if loc_cmp != Ordering::Equal {
        return loc_cmp;
    }
    let placement_cmp = compare_placement((a.1).0, (b.1).0);
    if placement_cmp != Ordering::Equal {
        return placement_cmp;
    }
    compare_imports(&(a.1).1, &(b.1).1)
}

fn adjust_placements(
    changes: Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))>,
) -> Vec<(Loc, (Placement, statement::Statement<Loc, Loc>))> {
    let mut iter = changes.into_iter();
    let Some(mut prev) = iter.next() else {
        return Vec::new();
    };
    let mut acc = Vec::new();
    for mut next in iter {
        let same_loc = prev.0 == next.0;
        let same_placement = compare_placement((prev.1).0, (next.1).0) == Ordering::Equal;
        let same_section = section_of_statement(&(prev.1).1) == section_of_statement(&(next.1).1);
        if same_loc && same_placement && same_section {
            match (prev.1).0 {
                Placement::Above { .. } => {
                    (prev.1).0 = Placement::Above { skip_line: false };
                }
                Placement::Below { .. } => {
                    (next.1).0 = Placement::Below { skip_line: false };
                }
                Placement::Replace => {}
            }
        }
        acc.push(prev);
        prev = next;
    }
    acc.push(prev);
    acc
}

pub fn add_imports(
    options: &js_layout_generator::Opts,
    added_imports: &[(&str, &Bindings)],
    ast: &ast::Program<Loc, Loc>,
) -> Vec<(Loc, String)> {
    let program_loc = ast.loc.clone();
    let partitioned = flow_ast_differ::partition_imports(&ast.statements);
    let changes = added_imports
        .iter()
        .map(|added_import| {
            get_change(
                &program_loc,
                &partitioned.imports,
                &partitioned.body,
                *added_import,
            )
        })
        .collect::<Vec<_>>();
    let mut changes = changes;
    changes.sort_by(compare_changes);
    adjust_placements(changes)
        .iter()
        .map(|change| string_of_change(options, change))
        .collect()
}

pub fn add_import(
    options: &js_layout_generator::Opts,
    bindings: &Bindings,
    from: &str,
    ast: &ast::Program<Loc, Loc>,
) -> Vec<(Loc, String)> {
    add_imports(options, &[(from, bindings)], ast)
}
