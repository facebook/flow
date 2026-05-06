/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::options::CastingSyntax;
use flow_common_ty::ty;
use flow_common_ty::ty::ALocTy;
use flow_common_ty::ty_serializer;
use flow_common_ty::ty_utils;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_lint_settings::lint_settings::LintSettings;
use flow_lint_settings::severity::Severity;
use flow_parser::ast;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_services_code_action::insert_type_utils::debug;
use flow_services_code_action::insert_type_utils::error;
use flow_services_code_action::insert_type_utils::info;
use flow_services_code_action::insert_type_utils::validator;

use super::codemod_context;

fn validator_validate_type(
    max_type_size: usize,
    loc_of_aloc: &dyn Fn(&flow_aloc::ALoc) -> Loc,
    ty: ALocTy,
) -> (ALocTy, Vec<error::ValidationError>) {
    validator::validate_type(max_type_size, loc_of_aloc, ty)
}

fn hardcoded_ty_fixes_run<Extra: BaseStats>(
    cctx: &codemod_context::typed::TypedCodemodContext<'_>,
    lint_severities: &LintSettings<Severity>,
    generalize_maybe: bool,
    generalize_react_mixed_element: bool,
    merge_arrays: bool,
    acc: &Acc<Extra>,
    loc: &Loc,
    ty: &ALocTy,
) -> (Acc<Extra>, ALocTy) {
    let reader = &cctx.reader;
    let loc_of_aloc = |aloc: &flow_aloc::ALoc| reader.loc_of_aloc(aloc);
    let get_ast_from_shared_mem = |file: &FileKey| -> Option<ast::Program<Loc, Loc>> {
        reader.get_ast(file).map(|arc| (*arc).clone())
    };
    use flow_services_code_action::insert_type_utils::warning;
    let acc_ref = std::cell::RefCell::new(acc.clone());
    let add_warning = |loc_w: Loc, w: warning::Kind| {
        acc_ref.borrow_mut().warn(&loc_w, &w);
    };
    let mut mapper =
        flow_services_code_action::insert_type_utils::TypeNormalizationHardcodedFixesMapper::new(
            &cctx.cx,
            &loc_of_aloc,
            &get_ast_from_shared_mem,
            &cctx.file_sig,
            &cctx.typed_ast,
            lint_severities,
            true, // allow_dollar_flowfixme
            generalize_maybe,
            generalize_react_mixed_element,
            &add_warning,
        );
    let t_prime = mapper.on_t(loc, ty.dupe());
    let t_prime = if merge_arrays {
        use flow_common_ty::ty;
        let members = ty::bk_union(&t_prime);
        let mut arr_members: Vec<ALocTy> = Vec::new();
        let mut other_members: Vec<ALocTy> = Vec::new();
        for m in members.iter() {
            match m.as_ref() {
                ty::Ty::Arr(arr) => arr_members.push(arr.arr_elt_t.dupe()),
                _ => other_members.push(m.dupe()),
            }
        }
        match arr_members.len() {
            0 | 1 => t_prime,
            _ => {
                let arr_union = ty::mk_union(true, arr_members).unwrap_or_else(ty::explicit_any);
                let arr = std::sync::Arc::new(ty::Ty::Arr(ty::ArrT {
                    arr_elt_t: arr_union,
                    arr_readonly: true,
                }));
                let mut all = vec![arr];
                all.extend(other_members);
                ty::mk_union(true, all).unwrap_or_else(ty::explicit_any)
            }
        }
    } else {
        t_prime
    };
    let t_double_prime = if std::sync::Arc::ptr_eq(ty, &t_prime) {
        ty.dupe()
    } else {
        ty_utils::simplify_type(false, None, t_prime)
    };
    (acc_ref.into_inner(), t_double_prime)
}

const TYPE_SIZE_WARNING_THRESHOLD: usize = 30;

pub mod queries {
    use flow_parser::ast_visitor;
    use flow_parser::ast_visitor::AstVisitor;

    use super::*;

    struct IdentVisitor {
        idents: BTreeSet<FlowSmolStr>,
    }

    impl<'ast> AstVisitor<'ast, Loc> for IdentVisitor {
        fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
            loc
        }
        fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
            type_
        }

        fn object_key_identifier(&mut self, _id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
            Ok(())
        }
        fn member_property_identifier(
            &mut self,
            _id: &'ast ast::Identifier<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
        fn member_type_identifier(
            &mut self,
            _id: &'ast ast::Identifier<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
        fn typeof_member_identifier(
            &mut self,
            _id: &'ast ast::Identifier<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
        fn enum_member_identifier(
            &mut self,
            _id: &'ast ast::Identifier<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
            self.idents.insert(id.name.clone());
            ast_visitor::identifier_default(self, id)
        }

        fn jsx_identifier(&mut self, id: &'ast ast::jsx::Identifier<Loc, Loc>) -> Result<(), !> {
            self.idents.insert(id.name.clone());
            ast_visitor::jsx_identifier_default(self, id)
        }
    }

    pub fn used_names(prog: &ast::Program<Loc, Loc>) -> BTreeSet<FlowSmolStr> {
        let mut visitor = IdentVisitor {
            idents: BTreeSet::new(),
        };
        let Ok(()) = visitor.program(prog);
        visitor.idents
    }
}

pub(crate) enum TyOrTypeAst {
    Ty_(ALocTy),
}

#[allow(unreachable_code)]
pub fn validate_ty(
    cctx: &codemod_context::typed::TypedCodemodContext<'_>,
    _max_type_size: i32,
    _ty: ALocTy,
) -> Result<ALocTy, Vec<error::Kind>> {
    let reader = &cctx.reader;
    let loc_of_aloc = |aloc: &flow_aloc::ALoc| reader.loc_of_aloc(aloc);
    let ty: ALocTy = ty_utils::simplify_type(false, None, _ty);
    let (ty, errs) = validator_validate_type(_max_type_size as usize, &loc_of_aloc, ty);
    match &errs[..] {
        [] => Ok(ty),
        _ => Err(errs.into_iter().map(error::Kind::ValidationError).collect()),
    }
}

#[allow(unreachable_code)]
pub fn get_ty<'a, 'cx>(
    cctx: &'a codemod_context::typed::TypedCodemodContext<'cx>,
    loc: Loc,
) -> Result<ALocTy, Vec<error::Kind>> {
    let norm_opts = flow_typing_ty_normalizer::env::Options::default_for_codemod();
    match codemod_context::typed::ty_at_loc(norm_opts, cctx, loc.clone()) {
        Ok(elt) => {
            let typified: Option<ALocTy> = ty_utils::typify_elt(elt);
            typified.ok_or_else(|| vec![error::Kind::MissingAnnotationOrNormalizerError])
        }
        Err(_) => Err(vec![error::Kind::MissingAnnotationOrNormalizerError]),
    }
}

pub fn get_validated_ty<'a, 'cx>(
    cctx: &'a codemod_context::typed::TypedCodemodContext<'cx>,
    max_type_size: i32,
    loc: Loc,
) -> Result<ALocTy, Vec<error::Kind>> {
    let ty = get_ty(cctx, loc);
    ty.and_then(|t| validate_ty(cctx, max_type_size, t))
}

use flow_services_code_action::insert_type_utils::Acc;
use flow_services_code_action::insert_type_utils::BaseStats;

pub struct Make<Extra: BaseStats> {
    _phantom: std::marker::PhantomData<Extra>,
}

impl<Extra: BaseStats> Make<Extra> {}

pub struct Mapper<'a, 'cx, Extra: BaseStats> {
    pub added_annotations_locmap: BTreeMap<Loc, Option<usize>>,
    pub wont_annotate_locs: BTreeSet<Loc>,
    pub codemod_error_locs: BTreeSet<Loc>,
    pub remote_converter: Option<
        flow_services_code_action::insert_type_imports::imports_helper::RemoteConverter<'cx>,
    >,

    pub cctx: &'a codemod_context::typed::TypedCodemodContext<'cx>,
    pub default_any: bool,
    pub generalize_maybe: bool,
    pub generalize_react_mixed_element: bool,
    pub lint_severities: LintSettings<Severity>,
    pub max_type_size: usize,
    pub merge_arrays: bool,
    pub exact_by_default: bool,
    pub casting_syntax: CastingSyntax,

    pub acc: Acc<Extra>,

    pub _phantom: std::marker::PhantomData<Extra>,
}

impl<'a, 'cx, Extra: BaseStats> Mapper<'a, 'cx, Extra> {
    fn get_remote_converter(
        &self,
    ) -> &flow_services_code_action::insert_type_imports::imports_helper::RemoteConverter<'cx> {
        self.remote_converter
            .as_ref()
            .expect("remote_converter not initialized: program() must be called first")
    }

    fn serialize(&self, t: ALocTy) -> ast::types::Type<Loc, Loc> {
        let options = ty_serializer::SerializerOptions {
            exact_by_default: self.exact_by_default,
        };
        ty_serializer::type_(&options, &t)
    }

    #[allow(unreachable_code)]
    #[allow(dead_code)]
    fn replace_type_node_with_ty(
        &mut self,
        loc: Loc,
        ty: ALocTy,
    ) -> Result<ast::types::Type<Loc, Loc>, error::Kind> {
        let (acc_, ty) = hardcoded_ty_fixes_run(
            self.cctx,
            &self.lint_severities,
            self.generalize_maybe,
            self.generalize_react_mixed_element,
            self.merge_arrays,
            &self.acc,
            &loc,
            &ty,
        );
        self.acc = acc_;
        let size = ty_utils::size_of_type(Some(self.max_type_size), &ty);
        let run_result: Result<ast::types::Type<Loc, Loc>, error::Kind> = self
            .get_remote_converter()
            .type_(ty)
            .map(|ty| self.serialize(ty));
        match run_result {
            Ok(t_ast) => {
                let t_ast = flow_services_code_action::insert_type_utils::patch_up_type_ast(&t_ast);
                self.added_annotations_locmap.insert(loc, size);
                Ok(t_ast)
            }
            Err(e) => {
                self.acc.error(&loc, &e);
                self.codemod_error_locs.insert(loc);
                Err(e)
            }
        }
    }

    #[allow(unreachable_code)]
    pub(crate) fn annotate_node<A>(
        &mut self,
        loc: Loc,
        ty_or_type_ast: TyOrTypeAst,
        f: impl Fn(ast::types::Annotation<Loc, Loc>) -> A,
    ) -> Result<A, error::Kind> {
        match ty_or_type_ast {
            TyOrTypeAst::Ty_(ref ty) => {
                let (acc_, ty_fixed) = hardcoded_ty_fixes_run(
                    self.cctx,
                    &self.lint_severities,
                    self.generalize_maybe,
                    self.generalize_react_mixed_element,
                    self.merge_arrays,
                    &self.acc,
                    &loc,
                    ty,
                );
                self.acc = acc_;
                let converted = self.get_remote_converter().type_(ty_fixed.dupe());
                let run_result: Result<ast::types::Type<Loc, Loc>, error::Kind> =
                    converted.map(|ty| self.serialize(ty));
                match run_result {
                    Ok(t_ast) => {
                        let size = ty_utils::size_of_type(Some(self.max_type_size), ty);
                        let t_ast =
                            flow_services_code_action::insert_type_utils::patch_up_type_ast(&t_ast);
                        self.added_annotations_locmap.insert(loc, size);
                        Ok(f(ast::types::Annotation {
                            loc: Loc::none(),
                            annotation: t_ast,
                        }))
                    }
                    Err(e) => {
                        self.acc.error(&loc, &e);
                        self.codemod_error_locs.insert(loc);
                        Err(e)
                    }
                }
            }
        }
    }

    #[allow(dead_code)]
    fn opt_annotate_inferred_type<A>(
        &mut self,
        f: impl Fn(Loc, &A, &TyOrTypeAst) -> Result<A, error::Kind>,
        error_fn: impl Fn(&A) -> A,
        loc: Loc,
        ty: &TyOrTypeAst,
        x: A,
    ) -> A {
        match f(loc.clone(), &x, ty) {
            Ok(y) => {
                Acc::<Extra>::debug(&loc, &debug::T::AddAnnotation(debug::NodeKind::Prop));
                y
            }
            Err(e) if self.default_any => {
                self.acc.error(&loc, &e);
                self.codemod_error_locs.insert(loc.clone());
                let _desc = error::serialize(&e);
                Acc::<Extra>::info(&loc, &info::T::DefaultAny);
                error_fn(&x)
            }
            Err(_) => x,
        }
    }

    #[allow(dead_code)]
    fn opt_annotate<A>(
        &mut self,
        f: impl Fn(Loc, &A, &TyOrTypeAst) -> Result<A, error::Kind> + Copy,
        error_fn: impl Fn(&A) -> A + Copy,
        _expr: Option<ast::expression::Expression<Loc, Loc>>,
        loc: Loc,
        ty_entry: Result<ALocTy, Vec<error::Kind>>,
        x: A,
    ) -> A {
        match ty_entry {
            Err(errs) => {
                for err in &errs {
                    self.acc.error(&loc, err);
                }
                self.codemod_error_locs.insert(loc.clone());
                if self.default_any {
                    Acc::<Extra>::info(&loc, &info::T::DefaultAny);
                    let explicit_any = ty::explicit_any();
                    let ty = TyOrTypeAst::Ty_(explicit_any);
                    self.opt_annotate_inferred_type(f, error_fn, loc, &ty, x)
                } else {
                    x
                }
            }
            Ok(ty) => {
                let ty = TyOrTypeAst::Ty_(ty);
                self.opt_annotate_inferred_type(f, error_fn, loc, &ty, x)
            }
        }
    }

    pub(crate) fn annotate_expr(
        &mut self,
        loc: Loc,
        expression: ast::expression::Expression<Loc, Loc>,
        ty: TyOrTypeAst,
    ) -> Result<ast::expression::Expression<Loc, Loc>, error::Kind> {
        match &*expression {
            ast::expression::ExpressionInner::ArrowFunction { .. } => {
                let arrow_loc = expression.loc().clone();
                self.acc.warn(
                    &arrow_loc,
                    &flow_services_code_action::insert_type_utils::warning::Kind::SkippingArrowFunction,
                );
                Ok(expression)
            }
            _ => {
                let expr_loc = expression.loc().clone();
                Acc::<Extra>::debug(&expr_loc, &debug::T::AddAnnotation(debug::NodeKind::Expr));
                let casting_syntax = self.casting_syntax;
                self.annotate_node(loc, ty, move |annot| match casting_syntax {
                    CastingSyntax::As | CastingSyntax::Both => ast::expression::Expression::new(
                        ast::expression::ExpressionInner::AsExpression {
                            loc: expr_loc.clone(),
                            inner: std::sync::Arc::new(ast::expression::AsExpression {
                                expression: expression.clone(),
                                annot,
                                comments: None,
                            }),
                        },
                    ),
                })
            }
        }
    }

    pub fn add_unannotated_loc_warnings(&mut self, lmap: &BTreeMap<Loc, ()>) {
        let not_annotated_locs: Vec<Loc> = lmap
            .keys()
            .filter(|loc| {
                if self.added_annotations_locmap.contains_key(loc) {
                    return false;
                }
                if self.wont_annotate_locs.contains(loc) {
                    return false;
                }
                if self.codemod_error_locs.contains(loc) {
                    return false;
                }
                true
            })
            .cloned()
            .collect();

        for loc in not_annotated_locs {
            self.acc.warn(
                &loc,
                &flow_services_code_action::insert_type_utils::warning::Kind::LocationUnhandled,
            );
        }
    }

    #[allow(unreachable_code)]
    pub fn initialize_program_state(&mut self, prog: &ast::Program<Loc, Loc>) {
        let reserved_names = queries::used_names(prog);
        let file = self.cctx.file.clone();
        let reader = &self.cctx.reader;
        let reader_haste = reader.clone();
        let get_haste_module_info =
            move |f: &FileKey| -> Option<flow_common_modulename::HasteModuleInfo> {
                reader_haste.get_haste_module_info(f)
            };

        let reader_loc = reader.clone();
        let reader_ts = reader.clone();
        self.remote_converter = Some(
            flow_services_code_action::insert_type_imports::imports_helper::RemoteConverter::new(
                Box::new(move |aloc| reader_loc.loc_of_aloc(aloc)),
                self.cctx.options.file_options.clone(),
                Box::new(get_haste_module_info),
                Box::new(move |fk| {
                    reader_ts.get_type_sig(fk).map(|arc| {
                        let bytes = bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                            .expect("get_type_sig: serialize");
                        bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                            .expect("get_type_sig: deserialize")
                            .0
                    })
                }),
                self.cctx.iteration as usize,
                file.clone(),
                reserved_names.iter().map(|s| s.to_string()).collect(),
            ),
        );
    }

    pub fn finalize_program(
        &mut self,
        prog: &ast::Program<Loc, Loc>,
        prog_: ast::Program<Loc, Loc>,
        extra: Extra,
    ) -> ast::Program<Loc, Loc> {
        let file = self.cctx.file.clone();
        if *prog != prog_ {
            self.acc.changed_set.insert(file.clone());
        }

        let ast::Program {
            loc,
            statements: stmts,
            interpreter,
            comments,
            all_comments,
        } = prog_;

        let total_size: usize = self
            .added_annotations_locmap
            .iter()
            .map(|(loc, size)| {
                let size = match size {
                    Some(x) => *x,
                    None => self.max_type_size,
                };
                if size > TYPE_SIZE_WARNING_THRESHOLD {
                    self.acc.warn(
                        loc,
                        &flow_services_code_action::insert_type_utils::warning::Kind::LargeTypeAdded(
                            size as i64,
                        ),
                    );
                }
                size
            })
            .sum();

        let stats = flow_services_code_action::insert_type_utils::Stats {
            number_of_annotations_added: self.added_annotations_locmap.len() as i64,
            total_size_of_annotations: total_size as i64,
            extra,
        };
        flow_hh_logger::info!("{} file stats: {}", file.as_str(), stats.serialize());
        self.acc.stats = stats;
        let generated_imports = self.get_remote_converter().to_import_stmts();
        let stmts: Vec<ast::statement::Statement<Loc, Loc>> =
            flow_services_code_action::insert_type::add_statement_after_directive_and_type_imports(
                &stmts,
                generated_imports,
            );
        ast::Program {
            loc,
            statements: stmts.into(),
            interpreter,
            comments,
            all_comments,
        }
    }

    pub fn program(
        &mut self,
        prog: ast::Program<Loc, Loc>,
        post_run: impl FnOnce(&mut Self) -> Extra,
    ) -> ast::Program<Loc, Loc> {
        self.initialize_program_state(&prog);
        let prog_: ast::Program<Loc, Loc> = self.map_program(&prog);
        let extra = post_run(self);
        self.finalize_program(&prog, prog_, extra)
    }
}

impl<'ast, 'a, 'cx, Extra: BaseStats> AstVisitor<'ast, Loc> for Mapper<'a, 'cx, Extra> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
}
