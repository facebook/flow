/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_analysis::bindings::Kind;
use flow_analysis::scope_builder;
use flow_parser::ast;
use flow_parser::ast::statement::ImportKind;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast::statement::import_declaration;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

enum ActionOnSpecifiers {
    DoNothing,
    RemoveEmptyNamedSpecifiers {
        local: Option<ast::Identifier<Loc, Loc>>,
        remote: ast::Identifier<Loc, Loc>,
    },
    DropOneNamedSpecifier {
        rest_specifiers: Vec<import_declaration::NamedSpecifier<Loc, Loc>>,
        local: Option<ast::Identifier<Loc, Loc>>,
        remote: ast::Identifier<Loc, Loc>,
    },
}

struct Mapper {
    contains: ContainsMapper,
    found: bool,
}

impl AstVisitor<'_, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_program(&mut self, program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        if self.contains.should_map_program(program) {
            // We override map_toplevel_statement_list to handle statement_fork_point
            let ast::Program {
                loc,
                statements,
                interpreter,
                comments,
                all_comments,
            } = program;
            // Apply statement_fork_point to each top-level statement
            let statements_ = self.statement_fork_point_list(statements);
            let comments_ = self.map_syntax_opt(comments.as_ref());
            let all_comments_ = Arc::from(
                all_comments
                    .iter()
                    .map(|c| self.map_comment(c))
                    .collect::<Vec<_>>(),
            );
            ast::Program {
                loc: loc.dupe(),
                statements: statements_,
                interpreter: interpreter.clone(),
                comments: comments_,
                all_comments: all_comments_,
            }
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

impl Mapper {
    // Apply statement_fork_point_list across the top-level statements.
    // This is the Rust equivalent of the OCaml statement_fork_point, which
    // can expand a single statement into multiple statements.
    fn statement_fork_point_list(
        &mut self,
        stmts: &Arc<[ast::statement::Statement<Loc, Loc>]>,
    ) -> Arc<[ast::statement::Statement<Loc, Loc>]> {
        let mut result: Vec<ast::statement::Statement<Loc, Loc>> = Vec::new();
        for stmt in stmts.iter() {
            let expanded = self.statement_fork_point(stmt);
            result.extend(expanded);
        }
        Arc::from(result)
    }

    fn statement_fork_point(
        &mut self,
        stmt: &ast::statement::Statement<Loc, Loc>,
    ) -> Vec<ast::statement::Statement<Loc, Loc>> {
        match &**stmt {
            StatementInner::ImportDeclaration { loc, inner: decl } => {
                let ast::statement::ImportDeclaration {
                    import_kind,
                    source,
                    specifiers,
                    default,
                    attributes: _,
                    comments: _,
                } = &**decl;
                if !loc.contains(self.contains.target_ref()) {
                    vec![stmt.dupe()]
                } else if *import_kind == ImportKind::ImportType {
                    let action = match specifiers {
                        None => ActionOnSpecifiers::DoNothing,
                        Some(specifiers) => match specifiers {
                            import_declaration::Specifier::ImportNamespaceSpecifier(_) => {
                                ActionOnSpecifiers::DoNothing
                            }
                            import_declaration::Specifier::ImportNamedSpecifiers(
                                named_specifiers,
                            ) => {
                                let mut relevant_local_remote_pair: Option<(
                                    Option<ast::Identifier<Loc, Loc>>,
                                    ast::Identifier<Loc, Loc>,
                                )> = None;
                                let named_specifiers_prime: Vec<_> = named_specifiers
                                    .iter()
                                    .filter(
                                        |import_declaration::NamedSpecifier {
                                             kind,
                                             kind_loc: _,
                                             local,
                                             remote,
                                             remote_name_def_loc: _,
                                         }| {
                                            match kind {
                                                Some(ImportKind::ImportValue)
                                                | Some(ImportKind::ImportTypeof) => true,
                                                None | Some(ImportKind::ImportType) => {
                                                    let id_loc =
                                                        &local.as_ref().unwrap_or(remote).loc;
                                                    if *id_loc == *self.contains.target_ref() {
                                                        relevant_local_remote_pair =
                                                            Some((local.clone(), remote.clone()));
                                                        false
                                                    } else {
                                                        true
                                                    }
                                                }
                                            }
                                        },
                                    )
                                    .cloned()
                                    .collect();
                                match relevant_local_remote_pair {
                                    None => ActionOnSpecifiers::DoNothing,
                                    Some((local, remote)) => {
                                        if named_specifiers_prime.is_empty() {
                                            ActionOnSpecifiers::RemoveEmptyNamedSpecifiers {
                                                local,
                                                remote,
                                            }
                                        } else {
                                            ActionOnSpecifiers::DropOneNamedSpecifier {
                                                rest_specifiers: named_specifiers_prime,
                                                local,
                                                remote,
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    };
                    match action {
                        ActionOnSpecifiers::RemoveEmptyNamedSpecifiers { local, remote } => {
                            let new_stmt =
                                ast::statement::Statement::new(StatementInner::ImportDeclaration {
                                    loc: loc.dupe(),
                                    inner: Arc::new(ast::statement::ImportDeclaration {
                                        import_kind: ImportKind::ImportValue,
                                        default: None,
                                        source: source.clone(),
                                        specifiers: Some(
                                            import_declaration::Specifier::ImportNamedSpecifiers(
                                                vec![import_declaration::NamedSpecifier {
                                                    kind: None,
                                                    kind_loc: None,
                                                    local,
                                                    remote,
                                                    remote_name_def_loc: None,
                                                }],
                                            ),
                                        ),
                                        attributes: None,
                                        comments: None,
                                    }),
                                });
                            self.found = true;
                            if default.is_none() {
                                vec![new_stmt]
                            } else {
                                vec![
                                    ast::statement::Statement::new(
                                        StatementInner::ImportDeclaration {
                                            loc: loc.dupe(),
                                            inner: Arc::new(ast::statement::ImportDeclaration {
                                                specifiers: None,
                                                ..(**decl).clone()
                                            }),
                                        },
                                    ),
                                    new_stmt,
                                ]
                            }
                        }
                        ActionOnSpecifiers::DropOneNamedSpecifier {
                            rest_specifiers,
                            local,
                            remote,
                        } => {
                            self.found = true;
                            vec![
                                ast::statement::Statement::new(StatementInner::ImportDeclaration {
                                    loc: loc.dupe(),
                                    inner: Arc::new(ast::statement::ImportDeclaration {
                                        specifiers: Some(
                                            import_declaration::Specifier::ImportNamedSpecifiers(
                                                rest_specifiers,
                                            ),
                                        ),
                                        ..(**decl).clone()
                                    }),
                                }),
                                ast::statement::Statement::new(StatementInner::ImportDeclaration {
                                    loc: loc.dupe(),
                                    inner: Arc::new(ast::statement::ImportDeclaration {
                                        import_kind: ImportKind::ImportValue,
                                        default: None,
                                        source: source.clone(),
                                        specifiers: Some(
                                            import_declaration::Specifier::ImportNamedSpecifiers(
                                                vec![import_declaration::NamedSpecifier {
                                                    kind: None,
                                                    kind_loc: None,
                                                    local,
                                                    remote,
                                                    remote_name_def_loc: None,
                                                }],
                                            ),
                                        ),
                                        attributes: None,
                                        comments: None,
                                    }),
                                }),
                            ]
                        }
                        ActionOnSpecifiers::DoNothing => match default {
                            Some(import_declaration::DefaultIdentifier {
                                identifier,
                                remote_default_name_def_loc: _,
                            }) => {
                                if identifier.loc == *self.contains.target_ref() {
                                    if specifiers.is_none() {
                                        self.found = true;
                                        vec![ast::statement::Statement::new(
                                            StatementInner::ImportDeclaration {
                                                loc: loc.dupe(),
                                                inner: Arc::new(
                                                    ast::statement::ImportDeclaration {
                                                        import_kind: ImportKind::ImportValue,
                                                        ..(**decl).clone()
                                                    },
                                                ),
                                            },
                                        )]
                                    } else {
                                        self.found = true;
                                        vec![
                                            ast::statement::Statement::new(
                                                StatementInner::ImportDeclaration {
                                                    loc: loc.dupe(),
                                                    inner: Arc::new(
                                                        ast::statement::ImportDeclaration {
                                                            default: None,
                                                            ..(**decl).clone()
                                                        },
                                                    ),
                                                },
                                            ),
                                            ast::statement::Statement::new(
                                                StatementInner::ImportDeclaration {
                                                    loc: loc.dupe(),
                                                    inner: Arc::new(
                                                        ast::statement::ImportDeclaration {
                                                            import_kind: ImportKind::ImportValue,
                                                            specifiers: None,
                                                            ..(**decl).clone()
                                                        },
                                                    ),
                                                },
                                            ),
                                        ]
                                    }
                                } else {
                                    vec![stmt.dupe()]
                                }
                            }
                            None => vec![stmt.dupe()],
                        },
                    }
                } else if *import_kind == ImportKind::ImportValue {
                    let specifiers_prime = match specifiers {
                        None => None,
                        Some(specifiers) => match specifiers {
                            import_declaration::Specifier::ImportNamespaceSpecifier(_) => {
                                Some(specifiers.clone())
                            }
                            import_declaration::Specifier::ImportNamedSpecifiers(
                                named_specifiers,
                            ) => Some(import_declaration::Specifier::ImportNamedSpecifiers(
                                named_specifiers
                                    .iter()
                                    .map(|named_specifier| {
                                        let import_declaration::NamedSpecifier {
                                            kind,
                                            kind_loc: _,
                                            local,
                                            remote,
                                            remote_name_def_loc: _,
                                        } = named_specifier;
                                        match kind {
                                            Some(ImportKind::ImportValue)
                                            | Some(ImportKind::ImportTypeof) => {
                                                named_specifier.clone()
                                            }
                                            Some(ImportKind::ImportType) | None => {
                                                let id_loc = &local.as_ref().unwrap_or(remote).loc;
                                                if *id_loc == *self.contains.target_ref() {
                                                    import_declaration::NamedSpecifier {
                                                        kind: None,
                                                        ..named_specifier.clone()
                                                    }
                                                } else {
                                                    named_specifier.clone()
                                                }
                                            }
                                        }
                                    })
                                    .collect(),
                            )),
                        },
                    };
                    if specifiers_prime != *specifiers {
                        self.found = true;
                        vec![ast::statement::Statement::new(
                            StatementInner::ImportDeclaration {
                                loc: loc.dupe(),
                                inner: Arc::new(ast::statement::ImportDeclaration {
                                    specifiers: specifiers_prime,
                                    ..(**decl).clone()
                                }),
                            },
                        )]
                    } else {
                        vec![stmt.dupe()]
                    }
                } else {
                    vec![stmt.dupe()]
                }
            }
            _ => vec![stmt.dupe()],
        }
    }
}

pub fn convert_type_to_value_import(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> Option<ast::Program<Loc, Loc>> {
    let scope_info = scope_builder::program(true, true, ast);
    if scope_info.all_uses().contains(&loc) {
        match scope_info.def_of_use_opt(&loc) {
            Some(def)
                if matches!(
                    def.kind,
                    Kind::Type {
                        imported: true,
                        type_only_namespace: false
                    }
                ) && def.locs.len() == 1 =>
            {
                let def_loc = def.locs.first().dupe();
                let mut mapper = Mapper {
                    contains: ContainsMapper::new(def_loc),
                    found: false,
                };
                let ast_prime = mapper.map_program(ast);
                if !mapper.found { None } else { Some(ast_prime) }
            }
            _ => None,
        }
    } else {
        None
    }
}
