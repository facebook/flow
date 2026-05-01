/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::types::Renders;
use flow_parser::ast::types::RendersVariant;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast::types::generic;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_render_type_default;
use flow_parser::ast_visitor::map_type_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

struct NormalizeTypeLocAndCommentMapper;

impl NormalizeTypeLocAndCommentMapper {
    fn with_none_loc(inner: &TypeInner<Loc, Loc>) -> TypeInner<Loc, Loc> {
        match inner {
            TypeInner::Any { comments, .. } => TypeInner::Any {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Mixed { comments, .. } => TypeInner::Mixed {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Empty { comments, .. } => TypeInner::Empty {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Void { comments, .. } => TypeInner::Void {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Null { comments, .. } => TypeInner::Null {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Number { comments, .. } => TypeInner::Number {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::BigInt { comments, .. } => TypeInner::BigInt {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::String { comments, .. } => TypeInner::String {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Boolean { raw, comments, .. } => TypeInner::Boolean {
                loc: LOC_NONE,
                raw: raw.clone(),
                comments: comments.clone(),
            },
            TypeInner::Symbol { comments, .. } => TypeInner::Symbol {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Exists { comments, .. } => TypeInner::Exists {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Nullable { inner, .. } => TypeInner::Nullable {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Function { inner, .. } => TypeInner::Function {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::ConstructorType {
                abstract_, inner, ..
            } => TypeInner::ConstructorType {
                loc: LOC_NONE,
                abstract_: *abstract_,
                inner: inner.dupe(),
            },
            TypeInner::Component { inner, .. } => TypeInner::Component {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Object { inner, .. } => TypeInner::Object {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Interface { inner, .. } => TypeInner::Interface {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Array { inner, .. } => TypeInner::Array {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Conditional { inner, .. } => TypeInner::Conditional {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Infer { inner, .. } => TypeInner::Infer {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Generic { inner, .. } => TypeInner::Generic {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::IndexedAccess { inner, .. } => TypeInner::IndexedAccess {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::OptionalIndexedAccess { inner, .. } => TypeInner::OptionalIndexedAccess {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Union { inner, .. } => TypeInner::Union {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Intersection { inner, .. } => TypeInner::Intersection {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Typeof { inner, .. } => TypeInner::Typeof {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Keyof { inner, .. } => TypeInner::Keyof {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Renders { inner, .. } => TypeInner::Renders {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::ReadOnly { inner, .. } => TypeInner::ReadOnly {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Tuple { inner, .. } => TypeInner::Tuple {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::StringLiteral { literal, .. } => TypeInner::StringLiteral {
                loc: LOC_NONE,
                literal: literal.clone(),
            },
            TypeInner::NumberLiteral { literal, .. } => TypeInner::NumberLiteral {
                loc: LOC_NONE,
                literal: literal.clone(),
            },
            TypeInner::BigIntLiteral { literal, .. } => TypeInner::BigIntLiteral {
                loc: LOC_NONE,
                literal: literal.clone(),
            },
            TypeInner::BooleanLiteral { literal, .. } => TypeInner::BooleanLiteral {
                loc: LOC_NONE,
                literal: literal.clone(),
            },
            TypeInner::TemplateLiteral { inner, .. } => TypeInner::TemplateLiteral {
                loc: LOC_NONE,
                inner: inner.dupe(),
            },
            TypeInner::Unknown { comments, .. } => TypeInner::Unknown {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Never { comments, .. } => TypeInner::Never {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::Undefined { comments, .. } => TypeInner::Undefined {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
            TypeInner::UniqueSymbol { comments, .. } => TypeInner::UniqueSymbol {
                loc: LOC_NONE,
                comments: comments.clone(),
            },
        }
    }
}

impl AstVisitor<'_, Loc> for NormalizeTypeLocAndCommentMapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_syntax_opt<Internal: Dupe>(
        &mut self,
        _syntax_opt: Option<&ast::Syntax<Loc, Internal>>,
    ) -> Option<ast::Syntax<Loc, Internal>> {
        None
    }

    fn map_identifier(&mut self, id: &ast::Identifier<Loc, Loc>) -> ast::Identifier<Loc, Loc> {
        ast::Identifier::new(ast::IdentifierInner {
            loc: LOC_NONE,
            name: id.name.dupe(),
            comments: id.comments.clone(),
        })
    }

    fn map_type_(&mut self, t: &ast::types::Type<Loc, Loc>) -> ast::types::Type<Loc, Loc> {
        let with_none_loc = ast::types::Type::new(Self::with_none_loc(t));
        map_type_default(self, &with_none_loc)
    }
}

type AstTypeSet = BTreeSet<ast::types::Type<Loc, Loc>>;

fn mod_t(
    unwrap_iterable: bool,
    t: &ast::types::Type<Loc, Loc>,
) -> Result<Option<ast::types::Type<Loc, Loc>>, ()> {
    match &**t {
        TypeInner::Nullable { inner, .. } => Ok(Some(inner.argument.dupe())),
        TypeInner::BooleanLiteral { literal, .. } if !literal.value => Ok(None),
        TypeInner::Null { .. } => Ok(None),
        TypeInner::Void { .. } => Ok(None),
        TypeInner::Array { inner, .. } if unwrap_iterable => Ok(Some(inner.argument.dupe())),
        TypeInner::Generic { inner, .. }
            if matches!(
                &inner.id,
                generic::Identifier::Unqualified(id)
                    if matches!(id.name.as_str(), "$ReadOnlyArray" | "Array" | "Iterable" | "Set")
            ) && inner
                .targs
                .as_ref()
                .is_some_and(|ta| ta.arguments.len() == 1) =>
        {
            let targs = inner.targs.as_ref().unwrap();
            Ok(Some(targs.arguments[0].dupe()))
        }
        _ => Err(()),
    }
}

fn mod_ts(
    unwrap_iterable: bool,
    ts: &[ast::types::Type<Loc, Loc>],
) -> Option<Vec<ast::types::Type<Loc, Loc>>> {
    let (ts_rev, transformed) = ts.iter().fold(
        (Vec::new(), false),
        |(mut collector, transformed), t| match mod_t(unwrap_iterable, t) {
            Ok(Some(t)) => {
                collector.push(t);
                (collector, true)
            }
            Ok(None) => (collector, true),
            Err(()) => {
                collector.push(t.dupe());
                (collector, transformed)
            }
        },
    );
    if ts_rev.is_empty() || !transformed {
        return None;
    }
    let mut normalize = NormalizeTypeLocAndCommentMapper;
    let last = ts_rev.last().unwrap().dupe();
    let ts_rev_rest = &ts_rev[..ts_rev.len() - 1];
    let init_normalized = normalize.map_type_(&last);
    let init_seen: AstTypeSet = {
        let mut s = BTreeSet::new();
        s.insert(init_normalized);
        s
    };
    let init_acc: Vec<ast::types::Type<Loc, Loc>> = vec![last];
    let (_, mut acc) =
        ts_rev_rest
            .iter()
            .rev()
            .fold((init_seen, init_acc), |(mut seen, mut acc), t| {
                let normalized = normalize.map_type_(t);
                if seen.contains(&normalized) {
                    (seen, acc)
                } else {
                    seen.insert(normalized);
                    acc.push(t.dupe());
                    (seen, acc)
                }
            });
    acc.reverse();
    Some(acc)
}

fn mod_renders_arg(
    unwrap_iterable: bool,
    t: &ast::types::Type<Loc, Loc>,
) -> Option<ast::types::Type<Loc, Loc>> {
    match &**t {
        TypeInner::Union { loc, inner } => {
            let (t0, t1, ts) = &inner.types;
            let mut all_ts = vec![t0.dupe(), t1.dupe()];
            all_ts.extend(ts.iter().map(|t| t.dupe()));
            match mod_ts(unwrap_iterable, &all_ts) {
                None => None,
                Some(deduped) if deduped.len() == 1 => Some(deduped.into_iter().next().unwrap()),
                Some(mut deduped) => {
                    let new_t0 = deduped.remove(0);
                    let new_t1 = deduped.remove(0);
                    let new_ts = deduped;
                    Some(ast::types::Type::new(TypeInner::Union {
                        loc: loc.dupe(),
                        inner: Arc::new(ast::types::Union {
                            types: (new_t0, new_t1, new_ts),
                            comments: inner.comments.clone(),
                        }),
                    }))
                }
            }
        }
        _ => match mod_t(unwrap_iterable, t) {
            Ok(Some(t)) => Some(t),
            Ok(None) | Err(()) => None,
        },
    }
}

struct ChangeToRendersMaybeMapper {
    render_arg_loc: Loc,
}

impl AstVisitor<'_, Loc> for ChangeToRendersMaybeMapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_render_type(&mut self, loc: &Loc, renders: &Renders<Loc, Loc>) -> Renders<Loc, Loc> {
        let Renders { argument, .. } = renders;
        if self.render_arg_loc == *argument.loc() {
            match mod_renders_arg(false, argument) {
                None => map_render_type_default(self, loc, renders),
                Some(argument_) => Renders {
                    operator_loc: renders.operator_loc.dupe(),
                    variant: RendersVariant::Maybe,
                    argument: argument_,
                    comments: renders.comments.clone(),
                },
            }
        } else {
            map_render_type_default(self, loc, renders)
        }
    }
}

struct ChangeToRendersStarMapper {
    render_arg_loc: Loc,
}

impl AstVisitor<'_, Loc> for ChangeToRendersStarMapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_render_type(&mut self, loc: &Loc, renders: &Renders<Loc, Loc>) -> Renders<Loc, Loc> {
        let Renders { argument, .. } = renders;
        if self.render_arg_loc == *argument.loc() {
            match mod_renders_arg(true, argument) {
                None => map_render_type_default(self, loc, renders),
                Some(argument_) => Renders {
                    operator_loc: renders.operator_loc.dupe(),
                    variant: RendersVariant::Star,
                    argument: argument_,
                    comments: renders.comments.clone(),
                },
            }
        } else {
            map_render_type_default(self, loc, renders)
        }
    }
}

pub fn to_renders_maybe_with_best_effort_fixes(
    ast: &ast::Program<Loc, Loc>,
    render_arg_loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = ChangeToRendersMaybeMapper { render_arg_loc };
    mapper.map_program(ast)
}

pub fn to_renders_star_with_best_effort_fixes(
    ast: &ast::Program<Loc, Loc>,
    render_arg_loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = ChangeToRendersStarMapper { render_arg_loc };
    mapper.map_program(ast)
}
