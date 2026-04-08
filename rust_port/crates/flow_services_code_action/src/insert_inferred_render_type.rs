/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/insert_inferred_render_type.ml`

use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocFuzzy;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::VirtualReasonDesc;
use flow_parser::ast;
use flow_parser::ast::types::ComponentRendersAnnotation;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_js::renders_kit;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_util;

use crate::insert_type;
use crate::insert_type_imports;

struct Found {
    missing_renders_loc: Loc,
    body_loc: Loc,
}

struct Mapper {
    target: Loc,
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Found> for Mapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn component_declaration(
        &mut self,
        _loc: &'ast Loc,
        component: &'ast ast::statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), Found> {
        let ast::statement::ComponentDeclaration {
            id, body, renders, ..
        } = component;
        let id_loc = &id.loc;
        match (body, renders) {
            (
                Some((body_loc, _)),
                ComponentRendersAnnotation::MissingRenders(missing_renders_loc),
            ) => {
                if id_loc.contains(&self.target) {
                    return Err(Found {
                        missing_renders_loc: missing_renders_loc.dupe(),
                        body_loc: body_loc.dupe(),
                    });
                }
            }
            _ => {}
        }
        ast_visitor::component_declaration_default(self, _loc, component)
    }

    fn statement(&mut self, stmt: &'ast ast::statement::Statement<Loc, Loc>) -> Result<(), Found> {
        ast_visitor::statement_default(self, stmt)
    }

    fn expression(
        &mut self,
        expr: &'ast ast::expression::Expression<Loc, Loc>,
    ) -> Result<(), Found> {
        ast_visitor::expression_default(self, expr)
    }

    fn program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> Result<(), Found> {
        ast_visitor::program_default(self, program)
    }
}

pub fn insert_render_type_at_loc<'a, 'cx>(
    remote_converter: Option<&mut insert_type_imports::imports_helper::RemoteConverter<'a>>,
    cx: &Context<'cx>,
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    get_ast_from_shared_mem: &'a dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    get_haste_module_info: &'a dyn Fn(&FileKey) -> Option<flow_common_modulename::HasteModuleInfo>,
    get_type_sig: &'a dyn Fn(
        &FileKey,
    ) -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    >,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    ast: &ast::Program<Loc, Loc>,
    id_loc: Loc,
) -> Result<Option<insert_type::InsertTypeResult<ast::Program<Loc, Loc>>>, FlowJsException> {
    let mut m = Mapper { target: id_loc };
    let Found {
        missing_renders_loc,
        body_loc,
    } = match m.program(ast) {
        Ok(()) => return Ok(None),
        Err(found) => found,
    };
    let inferred = cx.inferred_component_return();
    let body_aloc_fuzzy = ALocFuzzy::new(ALoc::of_loc(body_loc));
    match inferred.get(&body_aloc_fuzzy) {
        None => Ok(None),
        Some(ts) => {
            let reason = reason::mk_reason(
                VirtualReasonDesc::RRenderType(Arc::new(VirtualReasonDesc::RType(Name::new(
                    "React.Node",
                )))),
                ALoc::of_loc(missing_renders_loc.dupe()),
            );
            let t =
                type_util::union_of_ts(reason.dupe(), ts.iter().map(|t| t.dupe()).collect(), None);
            match renders_kit::try_synthesize_render_type(cx, true, &t)? {
                None => Ok(None),
                Some((renders_variant, ts)) => {
                    let t = Type::new(TypeInner::DefT(
                        reason.dupe(),
                        DefT::new(DefTInner::RendersT(Rc::new(
                            CanonicalRendersForm::StructuralRenders {
                                renders_variant,
                                renders_structural_type: type_util::union_of_ts(reason, ts, None),
                            },
                        ))),
                    ));
                    let ast_result = insert_type::insert_type_t(
                        cx,
                        loc_of_aloc,
                        get_ast_from_shared_mem,
                        get_haste_module_info,
                        get_type_sig,
                        file_sig,
                        typed_ast,
                        false, // ~omit_targ_defaults:false
                        false, // ~strict:false
                        remote_converter,
                        ast,
                        missing_renders_loc,
                        t,
                    );
                    Ok(Some(ast_result))
                }
            }
        }
    }
}
