/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::expression::object;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast::types::generic;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_generic_type_default;
use flow_parser::ast_visitor::map_type_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

fn prop_name_contains_target_loc(
    target_loc: &Loc,
    prop: &ast::types::object::Property<Loc, Loc>,
) -> bool {
    use ast::types::object::Property;
    match prop {
        Property::NormalProperty(p) => {
            let key_loc = match &p.key {
                object::Key::StringLiteral((loc, _)) => loc,
                object::Key::NumberLiteral((loc, _)) => loc,
                object::Key::BigIntLiteral((loc, _)) => loc,
                object::Key::Identifier(id) => &id.loc,
                object::Key::PrivateName(pn) => &pn.loc,
                object::Key::Computed(ck) => &ck.loc,
            };
            key_loc.contains(target_loc)
        }
        Property::SpreadProperty(_) => false,
        Property::Indexer(_) => false,
        Property::CallProperty(_) => false,
        Property::InternalSlot(_) => false,
        Property::MappedType(_) => false,
        Property::PrivateField(_) => false,
    }
}

#[derive(Clone, Copy)]
pub enum ConversionKind {
    ConversionToReadOnlyArray,
    ConversionToReadOnlyObject,
    ConversionToReadOnlyMap,
    ConversionToReadOnlySet,
}

struct Mapper {
    target_loc: Loc,
    readonly_array_name: FlowSmolStr,
    readonly_object_name: FlowSmolStr,
    readonly_map_name: FlowSmolStr,
    readonly_set_name: FlowSmolStr,
    conversion_kind: Option<ConversionKind>,
}

impl Mapper {
    fn new(ts_readonly_name: bool, target_loc: Loc) -> Self {
        let readonly_array_name = FlowSmolStr::new(if ts_readonly_name {
            "ReadonlyArray"
        } else {
            "$ReadOnlyArray"
        });
        let readonly_object_name = FlowSmolStr::new(if ts_readonly_name {
            "Readonly"
        } else {
            "$ReadOnly"
        });
        let readonly_map_name = FlowSmolStr::new(if ts_readonly_name {
            "ReadonlyMap"
        } else {
            "$ReadOnlyMap"
        });
        let readonly_set_name = FlowSmolStr::new(if ts_readonly_name {
            "ReadonlySet"
        } else {
            "$ReadOnlySet"
        });
        Mapper {
            target_loc,
            readonly_array_name,
            readonly_object_name,
            readonly_map_name,
            readonly_set_name,
            conversion_kind: None,
        }
    }

    fn get_conversion_kind(&self) -> Option<ConversionKind> {
        self.conversion_kind
    }

    fn handle_generic_type(
        &mut self,
        loc: &Loc,
        type_: &ast::types::Generic<Loc, Loc>,
    ) -> ast::types::Type<Loc, Loc> {
        let ast::types::Generic {
            id,
            targs,
            comments,
        } = type_;
        match (id, targs) {
            // Skip already readonly objects.
            (generic::Identifier::Unqualified(id_ident), Some(targs_val))
                if (id_ident.name.as_str() == "$ReadOnly"
                    || id_ident.name.as_str() == "Readonly")
                    && targs_val.arguments.len() == 1
                    && matches!(&*targs_val.arguments[0], TypeInner::Object { .. }) =>
            {
                let targ = &targs_val.arguments[0];
                let targ_ = map_type_default(self, targ);
                // We cannot just do super#generic_type, since we don't want to suggest converting to readonly
                // at foo for `$ReadOnly<{foo: string}>`
                if *targ == targ_ {
                    ast::types::Type::new(TypeInner::Generic {
                        loc: loc.dupe(),
                        inner: Arc::new(type_.clone()),
                    })
                } else {
                    let new_targs = Some(ast::types::TypeArgs {
                        loc: targs_val.loc.dupe(),
                        arguments: Arc::from([targ_]),
                        comments: targs_val.comments.clone(),
                    });
                    ast::types::Type::new(TypeInner::Generic {
                        loc: loc.dupe(),
                        inner: Arc::new(ast::types::Generic {
                            id: id.clone(),
                            targs: new_targs,
                            comments: comments.clone(),
                        }),
                    })
                }
            }
            // Array<T> ~> ReadonlyArray<T>
            (generic::Identifier::Unqualified(id_ident), Some(targs_val))
                if id_ident.name.as_str() == "Array"
                    && targs_val.arguments.len() == 1
                    && (id_ident.loc.contains(&self.target_loc) || loc == &self.target_loc) =>
            {
                self.conversion_kind = Some(ConversionKind::ConversionToReadOnlyArray);
                ast::types::Type::new(TypeInner::Generic {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Generic {
                        id: generic::Identifier::Unqualified(ast::Identifier::new(
                            IdentifierInner {
                                loc: loc.dupe(),
                                name: self.readonly_array_name.dupe(),
                                comments: id_ident.comments.clone(),
                            },
                        )),
                        targs: targs.clone(),
                        comments: comments.clone(),
                    }),
                })
            }
            // Map<T> ~> ReadonlyMap<T>
            (generic::Identifier::Unqualified(id_ident), Some(targs_val))
                if id_ident.name.as_str() == "Map"
                    && targs_val.arguments.len() == 2
                    && (id_ident.loc.contains(&self.target_loc) || loc == &self.target_loc) =>
            {
                self.conversion_kind = Some(ConversionKind::ConversionToReadOnlyMap);
                ast::types::Type::new(TypeInner::Generic {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Generic {
                        id: generic::Identifier::Unqualified(ast::Identifier::new(
                            IdentifierInner {
                                loc: loc.dupe(),
                                name: self.readonly_map_name.dupe(),
                                comments: id_ident.comments.clone(),
                            },
                        )),
                        targs: targs.clone(),
                        comments: comments.clone(),
                    }),
                })
            }
            // Map<T> ~> ReadonlySet<T>
            (generic::Identifier::Unqualified(id_ident), Some(targs_val))
                if id_ident.name.as_str() == "Set"
                    && targs_val.arguments.len() == 1
                    && (id_ident.loc.contains(&self.target_loc) || loc == &self.target_loc) =>
            {
                self.conversion_kind = Some(ConversionKind::ConversionToReadOnlySet);
                ast::types::Type::new(TypeInner::Generic {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Generic {
                        id: generic::Identifier::Unqualified(ast::Identifier::new(
                            IdentifierInner {
                                loc: loc.dupe(),
                                name: self.readonly_set_name.dupe(),
                                comments: id_ident.comments.clone(),
                            },
                        )),
                        targs: targs.clone(),
                        comments: comments.clone(),
                    }),
                })
            }
            _ => {
                let type__ = map_generic_type_default(self, loc, type_);
                if *type_ == type__ {
                    ast::types::Type::new(TypeInner::Generic {
                        loc: loc.dupe(),
                        inner: Arc::new(type_.clone()),
                    })
                } else {
                    ast::types::Type::new(TypeInner::Generic {
                        loc: loc.dupe(),
                        inner: Arc::new(type__),
                    })
                }
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

    fn map_type_(&mut self, t: &ast::types::Type<Loc, Loc>) -> ast::types::Type<Loc, Loc> {
        match &**t {
            TypeInner::Generic { loc, inner: gt } => self.handle_generic_type(loc, gt),
            TypeInner::Array { loc, inner } if loc == &self.target_loc => {
                self.conversion_kind = Some(ConversionKind::ConversionToReadOnlyArray);
                ast::types::Type::new(TypeInner::Generic {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Generic {
                        id: generic::Identifier::Unqualified(ast::Identifier::new(
                            IdentifierInner {
                                loc: loc.dupe(),
                                name: self.readonly_array_name.dupe(),
                                comments: None,
                            },
                        )),
                        targs: Some(ast::types::TypeArgs {
                            loc: LOC_NONE,
                            arguments: Arc::from([inner.argument.dupe()]),
                            comments: None,
                        }),
                        comments: inner.comments.clone(),
                    }),
                })
            }
            TypeInner::Object { loc, inner: obj }
                if obj
                    .properties
                    .iter()
                    .any(|p| prop_name_contains_target_loc(&self.target_loc, p))
                    || loc == &self.target_loc =>
            {
                self.conversion_kind = Some(ConversionKind::ConversionToReadOnlyObject);
                ast::types::Type::new(TypeInner::Generic {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Generic {
                        id: generic::Identifier::Unqualified(ast::Identifier::new(
                            IdentifierInner {
                                loc: loc.dupe(),
                                name: self.readonly_object_name.dupe(),
                                comments: None,
                            },
                        )),
                        targs: Some(ast::types::TypeArgs {
                            loc: LOC_NONE,
                            arguments: Arc::from([ast::types::Type::new(TypeInner::Object {
                                loc: loc.dupe(),
                                inner: obj.clone(),
                            })]),
                            comments: None,
                        }),
                        comments: None,
                    }),
                })
            }
            _ => map_type_default(self, t),
        }
    }
}

pub fn convert(
    ts_readonly_name: bool,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> Option<(ast::Program<Loc, Loc>, ConversionKind)> {
    let mut mapper = Mapper::new(ts_readonly_name, loc);
    let ast_ = mapper.map_program(ast);
    match mapper.get_conversion_kind() {
        None => None,
        Some(conversion_kind) => {
            if *ast == ast_ {
                None
            } else {
                Some((ast_, conversion_kind))
            }
        }
    }
}
