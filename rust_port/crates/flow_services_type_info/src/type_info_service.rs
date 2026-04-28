/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
use std::sync::LazyLock;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::refinement_invalidation;
use flow_common::refinement_invalidation::RefinementInvalidation;
use flow_common_ty::ty::TypeAtPosResult;
use flow_common_ty::ty_printer;
use flow_common_ty::ty_printer::PrinterOptions;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_parsing::docblock_parser;
use flow_services_coverage;
use flow_typing::query_types;
use flow_typing::query_types::QueryResult;
use flow_typing_context::Context;
use flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::string_of_ctor;
use regex::Regex;

fn json_data_of_result(
    s: &str,
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    acc.push((
        "result".to_string(),
        serde_json::Value::String(s.to_string()),
    ));
    acc
}

fn json_data_of_error(
    s: &str,
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    acc.push((
        "error".to_string(),
        serde_json::Value::String(s.to_string()),
    ));
    acc
}

fn json_data_of_loc(
    loc: &Loc,
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    acc.push((
        "loc".to_string(),
        flow_common::reason::json_of_loc(None, false, None, loc),
    ));
    acc
}

fn json_data_of_type(
    key: &str,
    s: &str,
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    acc.push((key.to_string(), serde_json::Value::String(s.to_string())));
    acc
}

fn json_data_of_type_opt(
    key: &str,
    str_opt: &Option<String>,
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    let value = match str_opt {
        Some(s) => serde_json::Value::String(s.clone()),
        None => serde_json::Value::Null,
    };
    acc.push((key.to_string(), value));
    acc
}

fn json_data_of_locs_opt(
    key: &str,
    locs: &[Loc],
    mut acc: Vec<(String, serde_json::Value)>,
) -> Vec<(String, serde_json::Value)> {
    let value = match locs {
        [] => serde_json::Value::Null,
        _ => serde_json::Value::Array(
            locs.iter()
                .map(|loc| flow_common::reason::json_of_loc(None, false, None, loc))
                .collect(),
        ),
    };
    acc.push((key.to_string(), value));
    acc
}

pub fn type_at_pos<'a>(
    cx: &Context<'a>,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    omit_targ_defaults: bool,
    max_depth: u32,
    verbose_normalizer: bool,
    no_typed_ast_for_imports: bool,
    include_refs: Option<&dyn Fn(&ALoc) -> Loc>,
    include_refinement_info: Option<&dyn Fn(&ALoc) -> Loc>,
    file: FileKey,
    line: i32,
    col: i32,
) -> Result<
    (
        (
            Loc,
            Option<TypeAtPosResult>,
            Vec<Loc>,
            Vec<(Loc, refinement_invalidation::Reason)>,
        ),
        Vec<(String, serde_json::Value)>,
    ),
    flow_utils_concurrency::job_error::JobError,
> {
    let loc = Loc::cursor(Some(file), line, col);
    let (refining_locs, refinement_invalidated) = match include_refinement_info {
        None => (vec![], vec![]),
        Some(loc_of_aloc) => {
            let contains_cursor = |aloc: &ALoc| loc_of_aloc(aloc).contains(&loc);
            let refined = cx.refined_locations();
            let mut merged_set = flow_aloc::ALocSet::new();
            for (aloc, aloc_set) in refined.iter() {
                if contains_cursor(aloc) {
                    for a in aloc_set.iter() {
                        merged_set.insert(a.dupe());
                    }
                }
            }
            let refining_locs: Vec<Loc> = merged_set.iter().map(loc_of_aloc).collect();
            let invalidated = cx.aggressively_invalidated_locations();
            let mut merged_invalidation = RefinementInvalidation::new();
            for (aloc, invalidation) in invalidated.iter() {
                if contains_cursor(aloc) {
                    merged_invalidation =
                        refinement_invalidation::union(merged_invalidation, invalidation.dupe());
                }
            }
            let refinement_invalidated: Vec<(Loc, refinement_invalidation::Reason)> =
                merged_invalidation
                    .iter()
                    .map(|(aloc, reason)| (loc_of_aloc(aloc), *reason))
                    .collect();
            (refining_locs, refinement_invalidated)
        }
    };
    let result = query_types::type_at_pos_type(
        cx,
        file_sig,
        omit_targ_defaults,
        verbose_normalizer,
        max_depth,
        typed_ast,
        no_typed_ast_for_imports,
        include_refs,
        loc,
    )?;
    let (json_data, loc, ty) = match result {
        QueryResult::FailureNoMatch => (
            json_data_of_result("FAILURE_NO_MATCH", vec![]),
            LOC_NONE,
            None,
        ),
        QueryResult::FailureUnparseable(loc, gt, msg) => {
            let json_data = vec![];
            let json_data = json_data_of_result("FAILURE_UNPARSEABLE", json_data);
            let json_data = json_data_of_error(&msg, json_data);
            let json_data = json_data_of_loc(&loc, json_data);
            let json_data = json_data_of_type("type", string_of_ctor(&gt), json_data);
            (json_data, loc, None)
        }
        QueryResult::Success(loc, tys) => {
            let exact_by_default = cx.exact_by_default();
            let ts_syntax = cx.ts_syntax();
            let opts = PrinterOptions {
                exact_by_default,
                ts_syntax,
                ..Default::default()
            };
            let json_data = vec![];
            let json_data = json_data_of_result("SUCCESS", json_data);
            let json_data = json_data_of_loc(&loc, json_data);
            let json_data = json_data_of_type(
                "type",
                &ty_printer::string_of_elt(&tys.unevaluated, &opts),
                json_data,
            );
            let evaluated_str = tys
                .evaluated
                .as_ref()
                .map(|e| ty_printer::string_of_elt(e, &opts));
            let json_data = json_data_of_type_opt("type_evaluated", &evaluated_str, json_data);
            let json_data = json_data_of_locs_opt("refining_locs", &refining_locs, json_data);
            let invalidated_locs: Vec<Loc> = refinement_invalidated
                .iter()
                .map(|(l, _)| l.dupe())
                .collect();
            let json_data =
                json_data_of_locs_opt("refinement_invalidated", &invalidated_locs, json_data);
            (json_data, loc, Some(tys))
        }
    };
    Ok(((loc, ty, refining_locs, refinement_invalidated), json_data))
}

static BATCHED_TYPE_AT_POS_SPECIAL_COMMENT_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ *\^\?$").unwrap());

pub fn batched_type_at_pos_from_special_comments<'a>(
    cx: &Context<'a>,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    omit_targ_defaults: bool,
    max_depth: u32,
    verbose_normalizer: bool,
    no_typed_ast_for_imports: bool,
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    file: FileKey,
) -> Result<
    (
        Vec<(
            Loc,
            Loc,
            Option<TypeAtPosResult>,
            Vec<Loc>,
            Vec<(Loc, refinement_invalidation::Reason)>,
        )>,
        serde_json::Value,
    ),
    flow_utils_concurrency::job_error::JobError,
> {
    let all_comments = &typed_ast.all_comments;
    let handle_comment = |comment: &ast::Comment<ALoc>| -> Result<
        Option<(
            (
                Loc,
                Loc,
                Option<TypeAtPosResult>,
                Vec<Loc>,
                Vec<(Loc, refinement_invalidation::Reason)>,
            ),
            serde_json::Value,
        )>,
        flow_utils_concurrency::job_error::JobError,
    > {
        let ast::Comment {
            loc: comment_loc,
            kind,
            text,
            ..
        } = comment;
        if *kind == ast::CommentKind::Line
            && BATCHED_TYPE_AT_POS_SPECIAL_COMMENT_REGEX.is_match(text)
        {
            let comment_loc_resolved = loc_of_aloc(comment_loc);
            let line = comment_loc_resolved.start.line;
            let column = comment_loc_resolved.start.column;
            let line = line - 1;
            let column = column + text.len() as i32;
            let ((ty_loc, tys, refining_locs, invalidation_info), json_data) = type_at_pos(
                cx,
                file_sig.dupe(),
                typed_ast,
                omit_targ_defaults,
                max_depth,
                verbose_normalizer,
                no_typed_ast_for_imports,
                Some(loc_of_aloc),
                Some(loc_of_aloc),
                file.dupe(),
                line,
                column,
            )?;
            let cursor_loc = Loc::cursor(Some(file.dupe()), line, column);
            Ok(Some((
                (cursor_loc, ty_loc, tys, refining_locs, invalidation_info),
                serde_json::Value::Object(json_data.into_iter().collect()),
            )))
        } else {
            Ok(None)
        }
    };
    let mut results: Vec<_> = Vec::new();
    for comment in all_comments.iter() {
        if let Some(res) = handle_comment(comment)? {
            results.push(res);
        }
    }
    let (friendly_results, json_data_list): (Vec<_>, Vec<_>) = results.into_iter().unzip();
    Ok((friendly_results, serde_json::Value::Array(json_data_list)))
}

pub fn dump_types<'a>(
    evaluate_type_destructors: EvaluateTypeDestructorsMode,
    for_tool: Option<i32>,
    cx: &Context<'a>,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
) -> Vec<(Loc, String)> {
    // Print type using Flow type syntax
    match for_tool {
        Some(depth) => query_types::dump_types_for_tool(cx, typed_ast, depth),
        None => {
            let exact_by_default = cx.exact_by_default();
            let ts_syntax = cx.ts_syntax();
            let opts = PrinterOptions {
                exact_by_default,
                ts_syntax,
                ..Default::default()
            };
            let printer = |elt: &flow_common_ty::ty::ALocElt| -> String {
                ty_printer::string_of_elt_single_line(elt, &opts)
            };
            query_types::dump_types(&printer, evaluate_type_destructors, cx, file_sig, typed_ast)
        }
    }
}

pub fn coverage(
    cx: &Context,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    force: bool,
    file: &FileKey,
    content: &str,
) -> Vec<(Loc, flow_services_coverage::Kind)> {
    let should_check = if force {
        true
    } else {
        // We can't just use the docblock that parse_contents returns because parse_contents modifies
        // it and we want the original docblock. Fortunately this is a pure function, and pretty fast,
        // so recomputing it isn't a problem.
        let (_, docblock) = docblock_parser::parse_docblock(
            docblock_parser::DOCBLOCK_MAX_TOKENS,
            &cx.file_options(),
            file,
            content,
        );
        docblock.is_flow()
    };
    flow_services_coverage::covered_types(should_check, cx, typed_ast)
}
