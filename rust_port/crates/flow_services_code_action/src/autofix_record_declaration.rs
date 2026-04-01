/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::statement::record_declaration;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_record_declaration_default;
use flow_parser::ast_visitor::map_record_declaration_property_default;
use flow_parser::ast_visitor::map_record_declaration_static_property_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

fn definitely_includes_void(type_: &ast::types::Type<Loc, Loc>) -> bool {
    match &**type_ {
        TypeInner::Nullable { .. }
        | TypeInner::Mixed { .. }
        | TypeInner::Unknown { .. }
        | TypeInner::Void { .. }
        | TypeInner::Any { .. } => true,
        TypeInner::Union { inner, .. } => {
            let (t1, t2, rest) = &inner.types;
            definitely_includes_void(t1)
                || definitely_includes_void(t2)
                || rest.iter().any(definitely_includes_void)
        }
        TypeInner::Intersection { inner, .. } => {
            let (t1, t2, rest) = &inner.types;
            definitely_includes_void(t1)
                || definitely_includes_void(t2)
                || rest.iter().any(definitely_includes_void)
        }
        _ => false,
    }
}

fn add_void_union_to_annotation(
    annot: &ast::types::Annotation<Loc, Loc>,
) -> ast::types::Annotation<Loc, Loc> {
    let annot_loc = &annot.loc;
    let type_ = &annot.annotation;
    let void_type = ast::types::Type::new(TypeInner::Void {
        loc: LOC_NONE,
        comments: None,
    });
    let new_type = match &**type_ {
        _ if definitely_includes_void(type_) => type_.dupe(),
        TypeInner::Union {
            loc: type_loc,
            inner,
        } => {
            let (t1, t2, rest) = &inner.types;
            let mut new_rest = vec![t2.dupe()];
            new_rest.extend(rest.iter().duped());
            ast::types::Type::new(TypeInner::Union {
                loc: type_loc.dupe(),
                inner: Arc::new(ast::types::Union {
                    types: (void_type, t1.dupe(), new_rest),
                    comments: inner.comments.clone(),
                }),
            })
        }
        _ => {
            let type_loc = type_.loc();
            ast::types::Type::new(TypeInner::Union {
                loc: type_loc.dupe(),
                inner: Arc::new(ast::types::Union {
                    types: (type_.dupe(), void_type, vec![]),
                    comments: None,
                }),
            })
        }
    };
    ast::types::Annotation {
        loc: annot_loc.dupe(),
        annotation: new_type,
    }
}

struct Mapper {
    contains: ContainsMapper,
}

impl Mapper {
    fn record_property_internal(
        &self,
        prop: &record_declaration::Property<Loc, Loc>,
    ) -> record_declaration::Property<Loc, Loc> {
        let record_declaration::Property {
            loc,
            key,
            annot,
            default_value,
            comments,
            invalid_syntax,
        } = prop;
        if invalid_syntax.is_none() {
            prop.clone()
        } else {
            let (annot, default_value) = match invalid_syntax {
                Some(record_declaration::InvalidPropertySyntax {
                    invalid_optional: Some(_),
                    ..
                }) => {
                    // Fix `foo?: number,` as `foo: void | number = undefined,`
                    let new_annot = add_void_union_to_annotation(annot);
                    let new_default_value = match default_value {
                        None => Some(ast::expression::Expression::new(
                            ExpressionInner::Identifier {
                                loc: LOC_NONE,
                                inner: ast::Identifier::new(IdentifierInner {
                                    loc: LOC_NONE,
                                    name: FlowSmolStr::new("undefined"),
                                    comments: None,
                                }),
                            },
                        )),
                        Some(_) => default_value.clone(),
                    };
                    (new_annot, new_default_value)
                }
                _ => (annot.clone(), default_value.clone()),
            };
            record_declaration::Property {
                loc: loc.dupe(),
                key: key.clone(),
                annot,
                default_value,
                comments: comments.clone(),
                invalid_syntax: None,
            }
        }
    }

    fn record_static_property_internal(
        &self,
        prop: &record_declaration::StaticProperty<Loc, Loc>,
    ) -> record_declaration::StaticProperty<Loc, Loc> {
        let record_declaration::StaticProperty {
            loc,
            key,
            annot,
            value,
            comments,
            invalid_syntax,
        } = prop;
        if invalid_syntax.is_none() {
            prop.clone()
        } else {
            let annot = match invalid_syntax {
                Some(record_declaration::InvalidPropertySyntax {
                    invalid_optional: Some(_),
                    ..
                }) => add_void_union_to_annotation(annot),
                _ => annot.clone(),
            };
            record_declaration::StaticProperty {
                loc: loc.dupe(),
                key: key.clone(),
                annot,
                value: value.clone(),
                comments: comments.clone(),
                invalid_syntax: None,
            }
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

    fn map_record_declaration(
        &mut self,
        loc: &Loc,
        decl: &ast::statement::RecordDeclaration<Loc, Loc>,
    ) -> ast::statement::RecordDeclaration<Loc, Loc> {
        let ast::statement::RecordDeclaration {
            id,
            tparams,
            implements,
            body,
            comments,
            invalid_syntax,
        } = decl;
        let id_loc = &id.loc;
        if self.contains.is_target(id_loc)
            || (self.contains.target_contained_by(loc) && invalid_syntax.is_some())
        {
            let body_loc = &body.loc;
            let elements = &body.body;
            let body_comments = &body.comments;
            let elements: Arc<[_]> = elements
                .iter()
                .map(|element| match element {
                    record_declaration::BodyElement::Property(prop) => {
                        let prop_ = self.record_property_internal(prop);
                        record_declaration::BodyElement::Property(prop_)
                    }
                    record_declaration::BodyElement::StaticProperty(prop) => {
                        let prop_ = self.record_static_property_internal(prop);
                        record_declaration::BodyElement::StaticProperty(prop_)
                    }
                    record_declaration::BodyElement::Method(_) => element.clone(),
                })
                .collect();
            let body = record_declaration::Body {
                loc: body_loc.dupe(),
                body: elements,
                comments: body_comments.clone(),
            };
            ast::statement::RecordDeclaration {
                id: id.clone(),
                tparams: tparams.clone(),
                implements: implements.clone(),
                body,
                comments: comments.clone(),
                invalid_syntax: None,
            }
        } else {
            map_record_declaration_default(self, loc, decl)
        }
    }

    fn map_record_declaration_property(
        &mut self,
        prop: &record_declaration::Property<Loc, Loc>,
    ) -> record_declaration::Property<Loc, Loc> {
        let record_declaration::Property {
            invalid_syntax,
            loc,
            ..
        } = prop;
        if self.contains.target_contained_by(loc) && invalid_syntax.is_some() {
            self.record_property_internal(prop)
        } else {
            map_record_declaration_property_default(self, prop)
        }
    }

    fn map_record_declaration_static_property(
        &mut self,
        prop: &record_declaration::StaticProperty<Loc, Loc>,
    ) -> record_declaration::StaticProperty<Loc, Loc> {
        let record_declaration::StaticProperty {
            invalid_syntax,
            loc,
            ..
        } = prop;
        if self.contains.target_contained_by(loc) && invalid_syntax.is_some() {
            self.record_static_property_internal(prop)
        } else {
            map_record_declaration_static_property_default(self, prop)
        }
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
}

pub fn fix_invalid_syntax(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
    };
    mapper.map_program(ast)
}
